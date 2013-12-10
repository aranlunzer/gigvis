/*jshint forin:true, noarg:true, noempty:true, eqeqeq:true, bitwise:true,
    strict:false, undef:true, unused:true, browser:true, jquery:true, maxerr:50,
    curly:false, multistr:true */
/*global Shiny, vg*/
// oneTimeInitShinyGgvis should only be called once for a page, given that initShiny()
// in shiny.js retains the Shiny object if it's already defined.
oneTimeInitShinyGgvis = function() {
  window.shinyGgvisInitialized = true;

  var debug = true;

  var livelyPendingData = {};
  var livelyPendingCharts = {};
  var livelyRenderedCharts = {};

  var ggvisOutputBinding = new Shiny.OutputBinding();
  $.extend(ggvisOutputBinding, {
    find: function(scope) {
      return $(scope).find('.shiny-ggvis-output');
    },
    onValueError: function(el, err) {
      Shiny.unbindAll(el);
      this.renderError(el, err);
    },
    renderValue: function(el, data) {
      vg.parse.spec(data.spec, function(chart) {
        chart({el: el}).update({duration: 250});
      });
    }
  });
  Shiny.outputBindings.register(ggvisOutputBinding, 'shiny.ggvisOutput');

  // A customized version of Shiny's htmlOutputBinding which can call a plot's
  // onControlOutput function when outputs are updated drawn.
  var ggvisControlOutputBinding = new Shiny.OutputBinding();
  $.extend(ggvisControlOutputBinding, {
    find: function(scope) {
      return $(scope).find('.ggvis-control-output');
    },
    onValueError: function(el, err) {
      Shiny.unbindAll(el);
      this.renderError(el, err);
    },
    renderValue: function(el, data) {
      var $el = $(el);

      Shiny.unbindAll(el);
      $el.html(data);
      Shiny.initializeInputs(el);
      Shiny.bindAll(el);

      // Run onControlOutput for each plot listed in data-plot-id
      var plotId = $el.data('plot-id');
      if (plotId !== undefined) {
        var ids = plotId.split(/ +/);

        for (var i = 0; i < ids.length; i++) {
          var plot = ggvis.plots[ids[i]];
          if (plot && plot.onControlOutput) {
            plot.onControlOutput();
          }
        }
      }
    }
  });
  Shiny.outputBindings.register(ggvisControlOutputBinding, 'shiny.ggvisControlOutput');


  // Receive data object and dispatch to appropriate vega object
  Shiny.addCustomMessageHandler("ggvis_data", function(message) {
    var plotId = message.plotId;
    var name = message.name;
    var data = message.value[0].values;
    var format = message.value[0].format;

    var plot = ggvis.getPlot(plotId);

    if (plot.chart) {
      // If the plot exists already, feed it the data
      var dataset = {};

      dataset[name] = vg.data.read(data, format);
      plot.chart.data(dataset);

      // If all data objects have been received, update
      if (plot.dataReady()) {
        if (!plot.initialized) {
          plot.initialUpdate();
        } else {
          plot.chart.update({ duration: plot.opts.duration });
        }
      }

    } else {
      // The plot doesn't exist, save the data for when the plot arrives
      if (!plot.pendingData) plot.pendingData = {};

      plot.pendingData[name] = data;
    }
  });

  // Receive a vega spec and parse it
  Shiny.addCustomMessageHandler("ggvis_vega_spec", function(message) {
    var plotId = message.plotId;
    var spec = message.spec;
    var plot = ggvis.getPlot(plotId);

    plot.parseSpec(spec,
      { mouseover: createMouseOverHandler(plotId),
        mouseout: createMouseOutHandler(plotId) }
    );
  });

  // a bunch of functions added by ael.
  // a lot of replication of the above, but that should make it easier
  // to track code changes.

  // Receive a vega spec and parse it
  Shiny.addCustomMessageHandler("ggvis_lively_vega_spec", function(message) {
    var chartId = message.chartId;
    var version = message.version;
    var spec = message.spec;
    var renderer = message.renderer;
    var dataSeparate = message.dataSeparate;    // whether data are sent separately
    console.log("ggvis_lively_vega_spec", chartId, version, spec); // { dataSeparate: dataSeparate }); // , message.timings);

    // If data are included, we can build and render immediately.
    if (!dataSeparate) return buildLivelyChart(spec, chartId, version, renderer, false);
    
    // But if data are being sent separately, add the details for this chart to
    // the pending-charts list, along with an extracted list of all the data
    // sources (with the right version number) the chart will need.
    var dataNames = spec.data.map(function(dSpec) { return dSpec.name });
    livelyPendingCharts[chartId] = { version: version, dataNames: dataNames, spec: spec, renderer: renderer };

    checkPendingChartsAndData();
  });

  // receive a data set for a chart whose spec may or may not have been sent yet
  Shiny.addCustomMessageHandler("ggvis_lively_data", function(message) {
    var chartId = message.chartId;
    var version = message.version;
    var name = message.name;
    // before we started dealing with dynamic split dfs, message.value would always be a 
    // one-element array holding a { name: n, values: valueArray } object.
    // now that object could also be...
    //   { name: format: values: {children: } }  for a xxx_tree dataset
    //   { name: source: transform: }    for using the result of parsing a xxx_tree
    var dataSpec = message.value[0];
    var values = dataSpec.values;   // may be undefined
if (debug) console.log("data", chartId, version, name);
//if (debug && name=="r_scenTrial3") console.log("data", chartId, version, name);

    // If the data spec belongs to a chart that's already rendered, and contains a
    // values array, send it in.
    var existingChartDetails = livelyRenderedCharts[chartId];
    if (existingChartDetails && existingChartDetails.version == version) {
      if (values) {
        var chart = existingChartDetails.chart;
        // HUMONGOUS HAIRY HACK
        // if there isn't a cleaner way of doing this, we should add one
        if (values.children) {
          var treeDef = chart.model().defs().data.load[name];
          treeDef[0].children = values.children;    // mega-hack
          var subDataSpec = {};
          subDataSpec[name] = treeDef;
          chart.data(subDataSpec);
        } else {
          var format = dataSpec.format;
          var subData = {};
          subData[name] = vg.data.read(values, format);
          chart.data(subData);
        }
        chart.update();
        chart.highlightDragItems();
      }
      // chart.update({props: "update", duration: 500});
    } else {
      // put the dataSpec in the pendingData collection, for use by the chart when all
      // data are assembled.
      var dataRecord = livelyPendingData[chartId] || (livelyPendingData[chartId] = {});
      if (dataRecord.version != version) {   // wrong version, or none
        dataRecord.version = version;
        dataRecord.datasets = {};
      }
      dataRecord.datasets[name] = dataSpec;

      checkPendingChartsAndData();
    }
  });
  
  function checkPendingChartsAndData() {
    // For each chart in the pendingCharts list, see if all the datasets with
    // the right version number have turned up yet.  If so, build the chart and
    // supply the data.
if (debug) console.log("pending:", livelyPendingCharts, livelyPendingData);
    for (var chartId in livelyPendingCharts) {
      var pendingChartRecord = livelyPendingCharts[chartId];
      var version = pendingChartRecord.version;
      var pendingDataRecord = livelyPendingData[chartId];
      if (pendingDataRecord && (pendingDataRecord.version == version)) {
        var dataNames = pendingChartRecord.dataNames;  // what the plot wants
        if (dataNames.every(function (name) { return pendingDataRecord.datasets[name] != undefined })) {
          buildLivelyChart(pendingChartRecord.spec, chartId, version, pendingChartRecord.renderer, true);  // true because data is separate; look in pendingData.
        }
      }
    }
  }

  function buildLivelyChart(spec, chartId, version, renderer, dataSeparate) {
    // NB: still using the old way of building charts.  ggvis has some new stuff.
    // NB: this is asynchronous.  On return, the chart won't yet have been built.

    vg.parse.spec(spec, function (chartBuilder) {
      // This callback is called from within vg.parse.data, once the data in the spec
      // have been procured.
      // chart is a viewConstructor, already initialised with width, height,
      // viewport, padding, marks, data - all derived from the spec.
      // The standard viewConstructor is that returned by vg.ViewFactory().
      // If called with an el: elem, view.initialize(elem) is invoked and does
      // the work of setting up a div.vega.  The view is given ._renderer, ._handler etc
      // and the handler (e.g., vg.svg.Handler) records the view (as ._obj).
      // The view's .on method adds to view._handlers a structure { type:, handler:, svg: }
      // and adds the svg part (an svgHandler) to the dom as a listener.
      var selector = ".ggvis-output#" + chartId;
      var $el = $(selector);
      // disable the default hover behaviour
      var chart = chartBuilder({ el: selector, hover: false, renderer: renderer });
      $el.data("ggvis-chart", chart);   // a convenient way to access the View object
      livelyRenderedCharts[chartId] = { version: version, chart: chart };

      if (!dataSeparate) {
        chart.update();   // need to render once anyway
        if (movingItems(chart).length) {
          chart.update({ props: "enter" });
          setTimeout(function(){$world.setHandStyle(null)}, 250);
          chart.update({ props: "update", /*items: movingItems(chart),*/ duration: 750 });
        }
      } else {
        // this function is only called once the pendingData are complete
/*  ***** currently not used *****
Code to load separately sent data (including tree-structured data) into a newly created chart.  Starting point: try using vg.parse.data here to get the model to pick up the data that need some processing such as tree expansion and flattening.  in vg.parse.spec, vg.parse.data is called with function() { callback(viewConstructor) } that gets called when all datasets have successfully loaded.  before that vg.parse.data returns a model with elements defs, load, flow, source... representing all loaded data, with flow and source set up when called for.  in vg.parse.spec this goes into the data element.

only Model.data() attends to the data.flow element.  Model.data() is called from View.data().  the former will only try to ingest datasets whose names are already registered in _defs.data.defs.  ingest will apply transforms if they are already registered in  _defs.data.flow (which is what we'd need for flattening the tree data to its non-tree analogue).

datasets with a "source" property are handled as part of the Model.ingest() processing.  for example, a split df results in a dataset foo that has a source foo_tree.  this sets up an entry _defs.data.source["foo_tree"] = ["foo"]  (see vg.parse.data).  then in vg.model.dependencies() - called from model.ingest(), after model._data["foo_tree"] has been set up, following transformation - all datasets that depend on foo_tree are ingested in turn.

so it looks like...  if we can set up the defs, flow, load, source elements in the model._data then call View.data() we might be laughing.  or in fact if the parsing has gone ok we don't have to worry which ones need parsing; they'll have been put into the defs element, with the things they depend on in source etc.

...note that this is only really needed in the case where we have more than one dataset, with dependency relationships.

so then the question is how to send an update to a dataset on which another depends - the example for now being tree-structured data.  in theory there should be a way to do it by just replacing the foo_tree definition - the { name: format: values: } object in data.defs - then saying "model.ingest("foo_tree")", which would do the tree parsing then tell "foo" to update itself too.  how to do this through the View...
*/

//  simple version:      chart.data(livelyPendingData[chartId].datasets);

        // reverse engineering the Vega code suggested that the following might work.
        // first prepare a dataDefs structure as a simple array with elements
        //    { name: ... }   (see the comment in ggvis_lively_data method above)
        var pendingDataRecord = livelyPendingData[chartId];
        var dataDefs = [];
        for (var dataId in pendingDataRecord.datasets) {
          dataDefs.push(pendingDataRecord.datasets[dataId])
        }
        // then tell Vega to parse that structure, and load the result.
        var dataSpec = vg.parse.data(dataDefs, function() {
          chart.model().defs().data = dataSpec;   // shove the filled-in spec into the model where expected
          chart.data(dataSpec.load);  // then load all datasets listed in the load element
          chart.update();
          delete livelyPendingCharts[chartId];
          delete livelyPendingData[chartId];
        })
      }

      function allCharts() {
        var charts = [];
        $(".ggvis-output").each(function(i, el) {
          var ch = $(el).data("ggvis-chart");
          if (ch) charts.push(ch);
        });
        return charts;
      }

      function allMarkableItems(chart) {
        // ael: return a collection of all vega-level items that can be marked, within a given chart
        var matches = [];
        d3.select(chart._el).selectAll("svg.marks rect, svg.marks path, svg.marks tr").filter(function(d) { return d && d.datarows })[0].each(function(el) { matches.push(el.__data__) });
        return matches
      }
      
      function relatedItems(chart, item) {
        // ael: return a collection of vega-level items
        //debugger;
        
        var rownumbers = JSON.parse(item.datarows);
        var data_id = item.mark.def.description.datasource;
        var p = data_id.indexOf(":");
        if (p != -1) data_id = data_id.substr(0, p);
        
        var matches = [];
        d3.select(chart._el).selectAll("svg.marks rect, svg.marks path, svg.marks tr").filter(function(d) { return d && d.datarows && d.mark.def.description.datasource.indexOf(data_id) == 0; })[0].each(function(el) {
          var item = el.__data__;
          (JSON.parse(item.datarows)).some(function(thisID) {
            if (rownumbers.indexOf(thisID) >= 0) {
              matches.push(item);
              return true;
            } else { return false; }
          })
        })
        // console.log(matches);
        return matches;
      }

      function movingItems(chart) {
        // ael: return a collection of vega-level items that have initialx, initialy props
        return d3.select(chart._el).selectAll("svg.marks path").filter(function(d) { return d && d.datum && d.datum.data && d.datum.data.initialx })[0].map(function(el) { return el.__data__ });
      }

      function findAndSetScenarioMarks(chart) {
          var scenarioMarks = [];
          var indices = [];
          d3.select(chart._el).selectAll("g").each(function(f) {
            var el = $(this)[0];
            var s;
            try { s = el.firstChild.__data__.mark.def.description.scenario } catch(e) { s = undefined };
            if (s) {
              scenarioMarks.push(el);
              indices.push(s);
              }
          });
          // associate the found marks with the scenario indices
          d3.selectAll(scenarioMarks).data(indices);
          return scenarioMarks;
      }
    
      function highlightScenarioMarks(chart, marks, touchedScenario) {
        var highlightItems = [];
        var otherItems = [];
        marks.forEach(function(el) {
          var s = d3.select(el).data();
          var nodes = el.childNodes;
          for ( i=0; i<nodes.length; i++ ) {
            var item = nodes[i].__data__;
            if (s == touchedScenario) { highlightItems.push( item );
            } else { otherItems.push( item ) }
          }
        })
        chart.update({ props: "update", items: otherItems });
        chart.update({ props: "scenarioHighlight", items: highlightItems });
      }
      
      chart.dragItem = null;
      chart.on("mouseover", (function(event, item) {
        if (this.dragItem) return;

        var scenarioMarks = findAndSetScenarioMarks(this);
        if (item.mark && item.mark.def.description && item.mark.def.description.scenario) {
          var touchedScenario = item.mark.def.description.scenario
          highlightScenarioMarks(this, scenarioMarks, touchedScenario)
          // DISABLED: code to sort all scenario marks so the touched one is guaranteed to be on top.  Using this function messed something up, preventing cleanup of the marks.  Figure it out later.
          //d3.selectAll(scenarioMarks).sort(function(a,b) { if (a==touchedScenario) { return 1 } else if (b == touchedScenario) { return -1 } else { return 0 } });
        } else if (item.datarows) {
          // hack that used to let us disable brushing on main-scenario histogram when scenarios are being shown:
          //   && !(item.mark.marktype == "rect" && scenarioMarks.length > 0)
          allCharts().forEach(function(ch) {
            ch.update({ props: "highlight", items: relatedItems(ch, item) })
          });
        }
      }).bind(chart));
      function restoreAllMarks() {
          allCharts().forEach(function(ch) {
            ch.update({ props: "update", items: allMarkableItems(ch) });
          });
      }
      chart.highlightDragItems = (function () {
          if (this.dragItem) {
            var dragItem = this.dragItem;
            allCharts().forEach(function(ch) {
              ch.update({ props: "highlight", items: relatedItems(ch, dragItem) })
            });
          }
      }).bind(chart);
      chart.endDrag = (function () {
        this.dragItem = null;
        restoreAllMarks();
      }).bind(chart);
      chart.on("mouseout", (function(event, item) {
        if (this.dragItem) return;
        if (item.datarows) restoreAllMarks();
      }).bind(chart));

      chart.on("mousedown", (function (evt, item) {
        // dragx and dragy are comma-separated strings of the form
        //    scaleName,datasetName,columnName  (e.g. x,workingData,wt)
        // we signal to R with structured messages (JSON encoded) such as
        //    message: set
        //    args:
        //      0 -> { dataset: "workingData", column: "wt", row: 6, value: 3.5 }
        //      1 -> { dataset: "workingData", column: "mpg", row: 6, value: 15 }
        if (item.dragx || item.dragy) {        // this is an item the user can drag
          var handleSize = 6;
          var handle = lively.morphic.Morph.makePolygon(
                [pt(-handleSize, 0), pt(handleSize, 0), pt(0, 0), pt(0, -handleSize), pt(0, handleSize)], 2, Color.black, Color.black);

          this.dragItem = item;
          handle.lastDragPos = evt.getPosition();
          handle.itemRow = JSON.parse(item.datarows)[0];
          handle.chart = this;
          
          function toList(dragSpec) {
            s = dragSpec.split(",");
            return { scale: s[0], dataset: s[1], column: s[2] }
          }
          if (item.dragx) handle.xSpec = toList(item.dragx);
          if (item.dragy) handle.ySpec = toList(item.dragy);
          var dataset = (handle.xSpec && handle.xSpec.dataset) || (handle.ySpec && handle.ySpec.dataset);
          if (dataset == "trialLine") {
            // HACK
            window.Shiny.shinyapp.sendInput({"showPopup": "TRUE"});
            $world.get("ShinyGigvisMorph1").popupChartMorph(3);
            };
          var msg = { message: "startEdit", args: [ dataset ] };
          window.Shiny.onInputChange("trigger", JSON.stringify(msg));

          // handle.onBlur = function() { debugger; }

          handle.addScript(function toChartCoords(evtPt) {
            var targetChart = this.chart;
            var chartRect = $(targetChart._el).bounds();
            var padding = targetChart.padding();
            var chartTop = chartRect.top + padding.top;
            var chartLeft = chartRect.left + padding.left;
            var chartGroup = targetChart.model().scene().items[0];
            var xVal, yVal;
            if (this.xSpec) {
              xVal = chartGroup.scales[this.xSpec.scale].invert(evtPt.x - chartLeft);
            }
            if (this.ySpec) {
              yVal = chartGroup.scales[this.ySpec.scale].invert(evtPt.y - chartTop);
            }
            return pt(xVal, yVal);
          }).bind(handle);

          handle.throttledDragHandler = Functions.throttle((function(evt) {
            var evtPos = evt.getPosition();
            this.lastDragPos = evtPos;
            if (this.rangeLine) {
              var vs = this.rangeLine.vertices();
              vs[1] = vs[0].addPt(evtPos.subPt(this.rangeStartPos));
              this.rangeLine.setVertices(vs);
            }

            var args = [];
            var msg = { message: "editData", args: args };
            var chartPt = this.toChartCoords(evtPos);
            if (chartPt.x) {
              args.push( { dataset: this.xSpec.dataset, column: this.xSpec.column, row: this.itemRow, value: chartPt.x.toFixed(2), xy: "x" } );
            }
            if (chartPt.y) {
              args.push( { dataset: this.ySpec.dataset, column: this.ySpec.column, row: this.itemRow, value: chartPt.y.toFixed(2), xy: "y" } );
            }
            window.Shiny.onInputChange("trigger", JSON.stringify(msg));
          }).bind(handle), 200);

          handle.addScript(function onDrag(evt) {
            this.throttledDragHandler(evt);
          }).bind(handle);

          handle.addScript(function onKeyDown(evt) {
            if (evt.getKeyCode() === Event.KEY_SHIFT) {
              this.rangeStartPos = this.lastDragPos;
              var line = lively.morphic.Morph.makeLine([this.rangeStartPos, this.rangeStartPos]).applyStyle({borderWidth: 2, borderColor: Color.red});
              this.rangeLine = $world.addMorph(line);
              this.focus();
            } else if (evt.getKeyCode() === Event.KEY_TAB) {
              // couldn't figure out whether there's a combination of stopPropagation etc
              // to allow the tab press to appear to onKeyPress.  So we deal with it here.
              console.log("tab down");
              console.log(evt.isShiftDown() ? " shifted" : " unshifted");
              evt.stop();
              return false;
            }
          }).bind(handle);

          handle.addScript(function onKeyUp(evt) {
            if (evt.getKeyCode() === Event.KEY_SHIFT) {
              if (!this.rangeStartPos) return;
              if (this.rangeLine) { this.rangeLine.remove(); delete this.rangeLine };
              var minDiff = 8;          // pixels, in either direction
              var start = this.rangeStartPos;
              var end = this.lastDragPos;
              var xDiff = start.x ? end.x - start.x : 0;
              var yDiff = start.y ? end.y - start.y : 0;
              var nSteps = Math.floor(Math.min(Math.max(Math.abs(xDiff), Math.abs(yDiff)) / minDiff, 9));
              if (nSteps == 0) return;
              var xStep = xDiff / nSteps;
              var yStep = yDiff / nSteps;
              var steps = [];
              for (var s = 0; s <= nSteps; s++) {
                var interPos = pt(start.x + (s*xStep), start.y + (s*yStep));
                var chartPt = this.toChartCoords(interPos); 
                steps.push(chartPt);
              }
              //console.log(steps);
              if (this.whichDataset() == "trialLine") {
                var isLeft = this.itemRow == 1;
                var parm = isLeft ? "trialLeft" : "trialRight";
                var values = steps.map(function (s) { return Number(s.y.toFixed(2)) });
                var msg = { message: "setScenarios", args: [ parm, values ] };
                window.Shiny.onInputChange("trigger", JSON.stringify(msg));
              }
            }
          }).bind(handle);

          handle.addScript(function onKeyPress(evt) {
            // console.log(evt.charCode);
          }).bind(handle);

          handle.addScript(function whichDataset() {
            return (this.xSpec && this.xSpec.dataset) || (this.ySpec && this.ySpec.dataset);
          }).bind(handle);
          
          handle.addScript(function onDropOn(evt) {
            var dataset = this.whichDataset();
            if (dataset == "trialLine") {
              // HACK
              window.Shiny.shinyapp.sendInput({"showPopup": "FALSE"});
              $world.get("GigvisPopup3").remove();
            };
            var msg = { message: "endEdit", args: [ dataset ] };
            window.Shiny.onInputChange("trigger", JSON.stringify(msg));
            if (this.rangeLine) this.rangeLine.remove();
            this.remove();
            this.chart.endDrag();
          }).bind(handle);
          handle.getGrabShadow = function() { return null };
          
          //evt.hand.getPosition()
          evt.hand.grabMorph(handle, evt);
          handle.setPosition(pt(-4,-4));
          $world.draggedMorph = handle;
          handle.focus();
        }
      }).bind(chart));
    });
  };

// utility functions

  // Returns a mouseover handler with plotId
  var createMouseOverHandler = function(plotId) {
    return function(event, item) {
      Shiny.onInputChange("ggvis_hover",
        {
          plot_id: plotId,
          data: item.datum.data,
          pagex: event.pageX,
          pagey: event.pageY
        }
      );
    };
  };

  // Returns a mouseout handler with plotId
  var createMouseOutHandler = function(plotId) {
    return function(event, item) {
      /* jshint unused: false */
      Shiny.onInputChange("ggvis_hover",
        {
          plot_id: plotId,
          data: null,
          pagex: null,
          pagey: null
        }
      );
    };
  };

  // Tooltip message handler
  Shiny.addCustomMessageHandler('ggvis_tooltip', function(data) {
    if (data.visible) {
      // Remove any existing tooltips
      $('.ggvis-tooltip').remove();

      // Add the tooltip div
      var $el = $('<div id="ggvis-tooltip" class="ggvis-tooltip"></div>')
        .appendTo('body');

      $el.html(data.html);
      $el.css({
        left:  data.pagex,
        top:   data.pagey,
        display: "block"
      });
    } else {
      $('.ggvis-tooltip').remove();
    }
  });

  refreshShinyGgvis = function() {
    // The user is throwing away the existing chart(s) and building anew. 
    livelyPendingData = {};
    livelyPendingCharts = {};
    livelyRenderedCharts = {};
  }
};

initShinyGgvis = function() {
  if (!window.shinyGgvisInitialized) oneTimeInitShinyGgvis();
  refreshShinyGgvis();
}

$(function(){ //DOM Ready
    if (!window.deferredShinyInit) initShinyGgvis();
  });
