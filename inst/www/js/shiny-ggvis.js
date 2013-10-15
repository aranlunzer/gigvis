/*jshint forin:true, noarg:true, noempty:true, eqeqeq:true, bitwise:true,
    strict:false, undef:true, unused:true, browser:true, jquery:true, maxerr:50,
    curly:false, multistr:true */
/*global Shiny, vg*/
// oneTimeInitShinyGgvis should only be called once for a page, given that initShiny()
// in shiny.js retains the Shiny object if it's already defined.
oneTimeInitShinyGgvis = function() {
  window.shinyGgvisInitialized = true;

  var debug = true;

  var pendingData = {};
  var allPlots = {};
  
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
        chart({el: el}).update();
      });
    }
  });
  Shiny.outputBindings.register(ggvisOutputBinding, 'shiny.ggvisOutput');

  Shiny.addCustomMessageHandler("ggvis_data", function(message) {
    var plotId = message.plotId;
    var name = message.name;
    var value = message.value[0].values;

    if (allPlots[plotId]) {
      // If the plot exists already, feed it the data
      var dataset = {};
      dataset[name] = value;
      allPlots[plotId].data(dataset);
      allPlots[plotId].update();

      updateGgvisDivSize(plotId);
    } else {
      // The plot doesn't exist, save it for when the plot arrives
      if (!pendingData[plotId])
        pendingData[plotId] = {};
      pendingData[plotId][name] = value;
    }
  });

  // Receive a vega spec and parse it
  Shiny.addCustomMessageHandler("ggvis_vega_spec", function(message) {
    var plotId = message.plotId;
    var spec = message.spec;
    var renderer = message.renderer;
if (debug) console.log("gigvis_vega_spec", spec);
    vg.parse.spec(spec, function(chart) {
      var selector = ".ggvis-output#" + plotId;
      var $el = $(selector);
      chart = chart({ el: selector, renderer: renderer });
      $el.data("ggvis-chart", chart);
      ggvisInit(plotId);

      // When done resizing, update with new width and height
      if ($el.resizable) {            // ael- might not be available
        $el.resizable({
          helper: "ui-resizable-helper",
          grid: [10, 10],
          stop: function() {
            var padding = chart.padding();
            chart.width($el.width() - padding.left - padding.right);
            chart.height($el.height() - padding.top - padding.bottom);
            chart.update();
          }
        });
      }
    });
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
    // before we started dealing with dynamic split dfs, message.value would be a 
    // one-element array holding a { name: n, values: valueArray } object.
    // now that object could be...
    //   { name: format: values: {children: } }  for a xxx_tree dataset
    //   { name: source: transform: }    for using the result of parsing a xxx_tree
    var dataSpec = message.value[0];
    var values = dataSpec.values;   // may be undefined
if (debug) console.log("data", chartId, version, name);

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
          var subData = {};
          subData[name] = values;
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
if (debug) console.log(livelyPendingCharts, livelyPendingData);
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
          chart.update({ props: "initial" });
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

      function draggableItems(chart) {
        // ael: return a collection of vega-level items that have secondx, secondy props
        return d3.select(chart._el).selectAll("svg.marks path").filter(function(d) { return d && d.dragx })[0].map(function(el) { return el.__data__ });
      }

      function movingItems(chart) {
        // ael: return a collection of vega-level items that have initialx, initialy props
        return d3.select(chart._el).selectAll("svg.marks path").filter(function(d) { return d && d.initialx })[0].map(function(el) { return el.__data__ });
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
          d3.selectAll(scenarioMarks).sort(function(a,b) { if (a==touchedScenario) { return 1 } else if (b == touchedScenario) { return -1 } else { return 0 } });
        } else if (item.datarows && !(item.mark.marktype == "rect" && scenarioMarks.length > 0)) {  // hack to disable brushing on main-scenario histogram when scenarios are being shown
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
        //    scaleName,datasetName,columnName  (e.g. x,mtcars,wt)
        // we signal to R with structured messages (JSON encoded) such as
        //    message: set
        //    args:
        //      0 -> { dataset: "mtcars", column: "wt", row: 6, value: 3.5 }
        //      1 -> { dataset: "mtcars", column: "mpg", row: 6, value: 15 }
        if (item.dragx || item.dragy) {        // this is an item the user can drag
          // var handle = lively.morphic.Morph.makeCircle(pt(0,0), 8, 2, Color.gray, Color.black);
          var handleSize = 6;
          var handle = lively.morphic.Morph.makePolygon(
                [pt(-handleSize, 0), pt(handleSize, 0), pt(0, 0), pt(0, -handleSize), pt(0, handleSize)], 2, Color.black, Color.black);

          this.dragItem = item;
          handle.itemRow = JSON.parse(item.datarows)[0];
          handle.chart = this;
          
          function toList(dragSpec) {
            s = dragSpec.split(",");
            return { scale: s[0], dataset: s[1], column: s[2] }
          }
          if (item.dragx) handle.xSpec = toList(item.dragx)
          if (item.dragy) handle.ySpec = toList(item.dragy)

          handle.throttledDragHandler = Functions.throttle((function(evt) {
            var targetChart = this.chart;
            var chartRect = $(targetChart._el).bounds();
            // var rect = targetChart._el.getBoundingClientRect();
            var evtPos = evt.getPosition() // this.globalBounds().center();
            var padding = targetChart.padding();
            var chartTop = chartRect.top + padding.top;
            var chartLeft = chartRect.left + padding.left;
/*
$world.cachedBounds = null;
var worldRect = $world.getBounds()
var chartTop = rect.top + padding.top - worldRect.top();
var chartLeft = rect.left + padding.left - worldRect.left();
*/
            var chartGroup = targetChart.model().scene().items[0];
            var args = [];
            var msg = { message: "editData", args: args };
            if (this.xSpec) {
              var xVal = chartGroup.scales[this.xSpec.scale].invert(evtPos.x - chartLeft).toFixed(2);
              args.push( { dataset: this.xSpec.dataset, column: this.xSpec.column, row: this.itemRow, value: xVal } );
            }
            if (this.ySpec) {
              var yVal = chartGroup.scales[this.ySpec.scale].invert(evtPos.y - chartTop).toFixed(2);
              args.push( { dataset: this.ySpec.dataset, column: this.ySpec.column, row: this.itemRow, value: yVal } );
            }
            window.Shiny.onInputChange("trigger", JSON.stringify(msg));
          }).bind(handle), 200);

          handle.addScript(function onDrag(evt) {
            this.throttledDragHandler(evt);
          }).bind(handle);
          
          handle.addScript(function onDropOn(evt) { this.remove(); this.chart.endDrag() }).bind(handle);
          handle.getGrabShadow = function() { return null };
          
          //evt.hand.getPosition()
          evt.hand.grabMorph(handle, evt);
          handle.setPosition(pt(-4,-4));
          $world.draggedMorph = handle;
        }
      }).bind(chart));
    });
  };

// utility functions

  // Sets height and width of wrapper div to contain the plot area.
  // This is so that the resize handle will be put in the right spot.
  function updateGgvisDivSize(plotId) {
    var $el = $(".ggvis-output#" + plotId);
    var $plotarea = $el.find("div.vega > .marks");

    $el.width($plotarea.width());
    $el.height($plotarea.height());
  }

  function ggvisInit(plotId) {
    var chart = $(".ggvis-output#" + plotId).data("ggvis-chart");
    allPlots[plotId] = chart;

    if (pendingData[plotId]) {
      // The data arrived earlier; use it.
      chart.data(pendingData[plotId]);
      chart.update();
      delete pendingData[plotId];

      updateGgvisDivSize(plotId);
    }
  }

  refreshShinyGgvis = function() {
    // The user is throwing away the existing chart(s) and building anew. 
    pendingData = {};
    allPlots = {};
    
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
