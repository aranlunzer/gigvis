/*jshint forin:true, noarg:true, noempty:true, eqeqeq:true, bitwise:true,
    strict:false, undef:true, unused:true, browser:true, jquery:true, maxerr:50,
    curly:false, multistr:true */
/*global Shiny, vg*/
// oneTimeInitShinyGgvis should only be called once for a page, given that initShiny()
// in shiny.js retains the Shiny object if it's already defined.
oneTimeInitShinyGgvis = function() {
  window.shinyGgvisInitialized = true;

  var debug = false;

  var livelyPendingData = {};
  var livelyPendingCharts = {};
  var livelyRenderedCharts = {};
  var livelyDataTriggers = {};
  var livelyEditRange = null;      // last used edit range, suitable for creating a sweep

  var _ = window.lodash;

  // ael: a wrapper onto onInputChange, that adds a millisecond-level time field to
  // ensure that each value (e.g., message) is different from the previous one.
  // NB: assumes message has NOT yet been JSONised.
  Shiny.timestampedOnInputChange = function(name, messageObj) {
    Shiny.onInputChange(name, JSON.stringify($.extend(messageObj, { timestamp: new Date().getTime() })));
  }
  Shiny.chartNamed = function(chartName) { return $(".ggvis-output#"+chartName).data("ggvisChart") }
  Shiny.startJogOnChartNamed = function(chartName) {
    Shiny.startJog(Shiny.chartNamed(chartName));
  }
  Shiny.setUpSweepOnChartNamed = function(chartName) {
    Shiny.setUpSweep(Shiny.chartNamed(chartName));
  }
  Shiny.setUpParameterRangeOnChartNamed = function(chartName, parameter, values, triggerName, trackerFn) {
    Shiny.setUpParameterRange(Shiny.chartNamed(chartName), parameter, values, triggerName, trackerFn)
  }
  Shiny.resetSweep = function() {
    Shiny.numScenarios = Shiny.visitScenario = 0;
    Shiny.timestampedOnInputChange("trigger", { message: "resetSweep" });
  }
  
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

    plot.parseSpec(spec, {
      handlers: {
        mouseover: _.throttle(createMouseOverHandler(plotId), 100),
        mouseout: _.throttle(createMouseOutHandler(plotId), 100)
      }
    });

    // Add brush handler
    var policy = spec.ggvis_opts.brush_policy || "debounce";
    var brushHandler;
    if (policy === "throttle") {
      brushHandler = _.throttle(createBrushHandler(plotId), spec.ggvis_opts.brush_delay);
    } else if (policy === "debounce") {
      brushHandler = _.debounce(createBrushHandler(plotId), spec.ggvis_opts.brush_delay);
    }
    plot.brush.on("updateItems", brushHandler);
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

    // If the data spec belongs to a chart that's already rendered, and contains a
    // values array, send it in.
    var existingChartDetails = livelyRenderedCharts[chartId];
    if (existingChartDetails && existingChartDetails.version == version) {
      if (values) {  // NB: for a tree dataset sent as r_foo and r_foo_tree, r_foo has no values 
        var chart = existingChartDetails.chart;
        var needsSort = false;
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
          // if (name=="r_sweep_scatter") console.log(subData[name]);  // often useful  :-)
          needsSort = subData[name].length>0 && (subData[name][0].scenario!==undefined);
        }
        chart.update();
        if (needsSort) chart.sortScenarioItems();
        chart.highlightDragItems();
      }
      var trigger = livelyDataTriggers[name];
      if (trigger) { delete livelyDataTriggers[name]; setTimeout(trigger, 1) }; 
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
      // chartBuilder is a viewConstructor, already initialised with width, height,
      // viewport, padding, marks, data - all derived from the spec.
      // The standard viewConstructor is that returned by vg.ViewFactory().
      // If called with an el: elem, view.initialize(elem) is invoked and sets up a child
      // div.vega element, stored as view._el.  The view is given ._renderer, ._handler etc
      // and the handler (e.g., vg.svg.Handler) records the view (as ._obj).
      // The view's .on method adds to view._handlers a structure { type:, handler:, svg: }
      // and adds the svg part (an svgHandler) to the dom as a listener.
      var selector = ".ggvis-output#" + chartId;
      var viewDivObj = $(selector);
      viewDivObj.off();   // this div is long-lived; remove any event listeners left over from last time
      // disable the default hover behaviour
      var chart = chartBuilder({ el: selector, hover: false, renderer: renderer });
      viewDivObj.data("ggvis-chart", chart);   // a convenient way to access the View object
      viewDivObj.attr("tabindex",-1);       // allow the element to get focus
      chart.viewDivObj = viewDivObj;        // a convenient way to get back to the JS element
      livelyRenderedCharts[chartId] = { version: version, chart: chart };

      if (!dataSeparate) {
        chart.update();   // need to render once anyway
        if (movingItems(chart).length) {
          chart.update({ props: "enter" });
          setTimeout(function(){$world.setHandStyle(null)}, 250);
          chart.update({ props: "update", /*items: movingItems(chart),*/ duration: 400 });
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

      function allMarkableElements(chart) {
        // ael: return a d3 selection with all svg elements that can be marked, within a given chart
        return d3.select(chart._el).selectAll("svg.marks rect, svg.marks path, svg.marks tr").filter(function(d) { return d && d.datarows });
      }

      function allMarkableItems(chart) {
        // ael: return a collection of all vega-level items that can be marked, within a given chart
        var matches = [];
        allMarkableElements(chart)[0].each(function(el) { matches.push(el.__data__) });
        // d3.select(chart._el).selectAll("svg.marks rect, svg.marks path, svg.marks tr").filter(function(d) { return d && d.datarows })[0].each(function(el) { matches.push(el.__data__) });
        // console.log("allMarkableItems: ", matches.length);
        return matches
      }
      
      function relatedItems(chart, item) {
        // ael: return a collection of vega-level items
        //debugger;
        
        var rownumbers = JSON.parse(item.datarows);
        // console.log(rownumbers[0], item);
        var data_id = item.mark.def.description.datasource;
        var p = data_id.indexOf(":");
        if (p != -1) data_id = data_id.substr(0, p);
        
        var matches = [];
        // used to be: d3.select(chart._el).selectAll("svg.marks rect, svg.marks path, svg.marks tr").filter(function(d) { return d && d.datarows && d.mark.def.description.datasource.indexOf(data_id) == 0; })[0].each(function(el) {
        allMarkableElements(chart).filter(function(d) { return d.mark.def.description.datasource.indexOf(data_id) == 0; })[0].each(function(el) {
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

      function scenarioItems(chart, touchedScenario) {
        var matches = [];
        allMarkableItems(chart).forEach(function(item) {
          if (item.scenario == touchedScenario) matches.push( item );
        });
        return matches;
      }
      
      // currently called only from unused code
      function movingItems(chart) {
        // ael: return a collection of vega-level items that have initialx, initialy props
        return d3.select(chart._el).selectAll("svg.marks path").filter(function(d) { return d && d.datum && d.datum.data && d.datum.data.initialx })[0].map(function(el) { return el.__data__ });
      }

      function parseDragSpec(dragSpec) {
        // a dragSpec has comma-separated components  scale, dataset, column[, triggerName]
        if (!dragSpec) return null;

        s = dragSpec.split(",");
        var spec = { scale: s[0], dataset: s[1], column: s[2] };
        if (s.length>3) spec.triggerName = s[3];
        return spec;
      }
      
      function initJogSpec(jogSpec, xSpec, ySpec) {
        jogSpec.xScale = xSpec && xSpec.scale;
        jogSpec.yScale = ySpec && ySpec.scale;
        jogSpec.xColumn = xSpec ? xSpec.column : "-";
        jogSpec.yColumn = ySpec ? ySpec.column : "-";
        jogSpec.dataset = (xSpec && xSpec.dataset) || (ySpec && ySpec.dataset);
        jogSpec.triggerName = (xSpec && xSpec.triggerName) || (ySpec && ySpec.triggerName);
      }

      function toChartCoords(evtPt, absolute, targetChart, xScale, yScale) {
        // based on the x and y drag specs of the specified element, turn the 
        // event point into an abstract point with coords in x and/or y.
        // if absolute is true, assume we need to subtract the chart origin.
        var chartTop = 0, chartLeft = 0;
        if (absolute) {
          var chartRect = $(targetChart._el).bounds();
          var padding = targetChart.padding();
          chartTop = chartRect.top + padding.top;
          chartLeft = chartRect.left + padding.left;
        }
        var chartGroup = targetChart.model().scene().items[0];
        var xVal, yVal;
        if (xScale) xVal = chartGroup.scales[xScale].invert(evtPt.x - chartLeft);
        if (yScale) yVal = chartGroup.scales[yScale].invert(evtPt.y - chartTop);
        return pt(xVal, yVal);
      }
      
      function setUpParameterRange(chart, parameter, unfilteredValues, triggerName, trackerFn) {
        // sent by a slider, at the end of a drag
        if (unfilteredValues.length < 2) return;

        var start = Number(unfilteredValues.first());
        var end = Number(unfilteredValues.last());
        var low = Math.min(start, end);
        var high = Math.max(start, end);
        var seen = [];
        var validPositions = unfilteredValues.filter(function(v) {
          if (seen.indexOf(v) !== -1) return false;
          seen.push(v);
          return Number(v) >= low && Number(v) <= high;
        });
        var values = validPositions;
        var jogSpec = chart.nextJogSpec = {};
        jogSpec.parameter = parameter;
        jogSpec.triggerName = triggerName;
        jogSpec.restoreValue = values.last();
        jogSpec.valueRange = [];
        jogSpec.scenarios = [];
        var numPositions = values.length;
        for (var i=0; i<numPositions; i++) {
          jogSpec.valueRange.push(values[i]);
          jogSpec.scenarios.push(i+1);
        }
        jogSpec.direction = -1;
        jogSpec.maxIndex = numPositions - 1;
        jogSpec.index = jogSpec.maxIndex - 1;  // first below the starting point
        jogSpec.bounce = true;
        
        jogSpec.trackerFn = trackerFn;
      }
      Shiny.setUpParameterRange = setUpParameterRange;
      
      function setUpEditRange(chart, item) {
        // set up a suitable range for a point that has been clicked, rather than dragged.
        // will be invoked even if the item isn't one for which an edit is valid
        if (!(item.dragx || item.dragy)) return;  // don't even delete old edit range

        var jogSpec = chart.nextJogSpec = {};

        // figure out x and/or y of the item, and hence the range through which it should be varied
        var xSpec = parseDragSpec(item.dragx);   // may be null
        var ySpec = parseDragSpec(item.dragy);   // ditto
        initJogSpec(jogSpec, xSpec, ySpec);
        
        var dataset = jogSpec.dataset;
        var itemPos = pt(item.x, item.y);        // NB: pixels, relative to chart.
        jogSpec.restorePoint = toChartCoords(itemPos, false, chart, jogSpec.xScale, jogSpec.yScale);
        jogSpec.pointRange = [];
        jogSpec.scenarios = [];
        if (xSpec && ySpec) {
          // visit 8 compass directions, in a circle of diameter 0.1*scale in each dimension.
          var numPositions = 8;  // numbered 0 to 7
          var chartRect = $(chart._el).bounds();
          var ratio = 0.15;
          var xRadius = chartRect.width() * ratio / 2;
          var yRadius = chartRect.height() * ratio / 2;
          for (var segment=0; segment<8; segment++) {
            var angle = segment * Math.PI / 4;
            var jogPoint = pt(itemPos.x + xRadius*Math.sin(angle), itemPos.y - yRadius*Math.cos(angle)); 
            var jogChartPoint = toChartCoords(jogPoint, false, chart, jogSpec.xScale, jogSpec.yScale);
            jogSpec.pointRange.push(jogChartPoint);
            jogSpec.scenarios.push(segment+1);
          }
          jogSpec.index = 0;
          jogSpec.direction = 1;
          jogSpec.maxIndex = numPositions - 1;
          jogSpec.bounce = false;
        } else {
          var numPositions = 10;
          var startPoint = jogSpec.restorePoint;
          var isXRange = !!startPoint.x;
          var startVal = 0;
          var stopVal = isXRange ? startPoint.x : startPoint.y;
          jogSpec.pointRange = [];
          jogSpec.scenarios = [];
          for (var step=0; step<numPositions; step++) {
            var val = startVal+step*(stopVal-startVal)/(numPositions-1);
            jogSpec.pointRange.push(isXRange ? pt(val, -1000) : pt(-1000, val));
            jogSpec.scenarios.push(step+1);
          }
          jogSpec.direction = -1;
          jogSpec.maxIndex = numPositions - 1;
          jogSpec.index = jogSpec.maxIndex - 1;  // first below the starting point
          jogSpec.bounce = true;
        }
        jogSpec.xSpec = xSpec;
        jogSpec.ySpec = ySpec;
        jogSpec.row = JSON.parse(item.datarows)[0];
      }

      function startJog(chart) {
        if (!chart.nextJogSpec) return;
        if (debug) console.log("start jog");
        chart.jogSpec = chart.nextJogSpec;
        jogStep(chart);
        // make sure the view has keyboard focus, to allow Escape to stop the jog
        chart.viewDivObj.focus();
      }
      Shiny.startJog = startJog;

      function jogStep(chart) {
        if (!chart.jogSpec) return;
        // console.log("jogStep");
        var jogSpec = chart.jogSpec;
        if (jogSpec.dataset) {
          var jogPoint = jogSpec.pointRange[jogSpec.index];
          sendEditMessage(jogSpec.dataset, [jogSpec.xColumn, jogSpec.yColumn], jogSpec.row, [ 0 ], [ jogPoint ], "trigger2");
        } else {
          var jogValue = jogSpec.valueRange[jogSpec.index];
          sendParameterMessage(jogSpec.parameter, [ 0 ], [ jogValue ], "trigger2");
        }
        // step to next jog position, taking account of the "bounce" setting
        var dir = jogSpec.direction;
        var next = jogSpec.index + dir;  // tentative
        if (jogSpec.bounce) {
          if (next < 0) {     // bounce off bottom limit
            next = 1;
            dir = 1;
          } else if (next > jogSpec.maxIndex) {
            next = jogSpec.maxIndex - 1;  // bounce off top limit
            dir = -1;
          }
        } else {
          next = next % (jogSpec.maxIndex+1);
        }
        jogSpec.index = next;
        jogSpec.direction = dir;
        livelyDataTriggers[jogSpec.triggerName] = function() { jogStep(chart) };
      }

      function endJog(chart) {
        if (debug) console.log("ending jog");
        var jogSpec = chart.jogSpec;
        if (jogSpec) {
          delete livelyDataTriggers[jogSpec.triggerName];
          if (jogSpec.dataset) {    // it's from a chart-point drag, not a slider
            sendEditMessage(jogSpec.dataset, [jogSpec.xColumn, jogSpec.yColumn], jogSpec.row, [ 0 ], [ jogSpec.restorePoint ], "trigger2");
          } else {
            sendParameterMessage(jogSpec.parameter, [ 0 ], jogSpec.restoreValue, "trigger2");
          }
          delete chart.jogSpec;
        }
      }

      function setUpSweep(chart) {
        if (!chart.nextJogSpec) return;

        if (debug) console.log("set up sweep");
        var range = chart.nextJogSpec;
        // the spec is either for edits on a draggable variable, or 
        if (range.dataset) {
          sendEditMessage(range.dataset, [range.xColumn, range.yColumn], range.row, range.scenarios, range.pointRange);
          Shiny.numScenarios = range.pointRange.length;
          Shiny.sweepTracker = null;
        } else {
          Shiny.sweepValueRange = range.valueRange;
          Shiny.sweepTracker = range.trackerFn;
          Shiny.numScenarios = range.valueRange.length;
          sendParameterMessage(range.parameter, range.scenarios, range.valueRange);
        }
        Shiny.visitScenario = 0;
      }
      Shiny.setUpSweep = setUpSweep;

      function visitNextScenario(dir) {
        if (Shiny.numScenarios> 0) {
          var vs = Shiny.visitScenario % Shiny.numScenarios + dir;
          if (vs < 1) vs += Shiny.numScenarios;
          Shiny.visitScenario = vs;
          // console.log("new scenario: ", vs);
          if (Shiny.sweepTracker) Shiny.sweepTracker(Shiny.sweepValueRange[vs-1]);
          window.Shiny.timestampedOnInputChange("trigger", { message: "visitScenario", args: vs });
        }
      }
      Shiny.visitNextScenario = visitNextScenario;

      function sendEditMessage(dataset, xycolumns, row, scenarios, chartPoints, channel) {
        var args = {};
        args.type = "data";
        args.target = { dataset: dataset, row: row, xycolumns: xycolumns };
        args.scenarios = scenarios;
        args.values = chartPoints.map(function(cp) {
          var xy = ["-1000", "-1000"];
          if (cp.x) xy[[0]] = cp.x.toFixed(2);
          if (cp.y) xy[[1]] = cp.y.toFixed(2);
          return xy;
        });
        var ch = channel || "trigger";  // allow override of default channel
        window.Shiny.timestampedOnInputChange(ch, { message: "edit", args: args });
      }

      function sendParameterMessage(parameter, scenarios, values, channel) {
        var args = { type: "parameter", target: parameter, scenarios: scenarios, values: values };
        var ch = channel || "trigger";  // allow override of default channel
        window.Shiny.timestampedOnInputChange(ch, { message: "edit", args: args });
      }

      //viewDivObj.on("blur", function() { if (debug) console.log("div blur") });
      //viewDivObj.on("focus", function() { if (debug) console.log("div focus") });
      viewDivObj.on("mouseover", function() {
        // grab focus if this is a chart that wants it, and there isn't currently a drag
        if (viewDivObj.attr("livelyautofocus")=="true" && !viewDivObj.data("ggvisChart").dragItem) viewDivObj.focus();
      });
      
      viewDivObj.on("keydown", (function(evt) {
        // console.log(evt.keyCode);
        if (evt.keyCode === Event.KEY_ESC) {
          if (debug) console.log("ESCAPE");
          endJog(this.data("ggvis-chart"));
          return false;
        }
        if (evt.keyCode === Event.KEY_TAB) {
          if (debug) console.log("TAB");
          setTimeout(function() { visitNextScenario(evt.shiftKey ? -1 : 1)}, 1);
          return false;
        }
      }).bind(viewDivObj));
      
      //viewDivObj.on("keyup", (function(evt) {
        // console.log("up", evt);
      //}).bind(viewDivObj));

      //viewDivObj.on("keypress", (function(evt) {
      //}).bind(viewDivObj));

      chart.dragItem = null;
      chart.on("mouseover", (function(evt, item) {
        // NB: this is not a morphic event - so to look at the position, use pageX & pageY
        if (this.dragItem) return;
        
        // look for some scenario highlighting to do
        var touchedScenario = item.scenario;
        if (touchedScenario) {  // won't trigger on scenario 0, which is probably fine...?
          allCharts().forEach(function(ch) {
            ch.update({ props: "update", items: allMarkableItems(ch) });
            ch.update({ props: "scenarioHighlight", items: scenarioItems(ch, touchedScenario) });
          });
          // DISABLED: code to sort all scenario marks so the touched one is guaranteed to be on top.  Using this function messed something up, preventing cleanup of the marks.  Might now be fixed for marks that use non-index keys for their elements.
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
      
      chart.endDrag = (function (xSpec, ySpec, row, dragPositions, dragType) {
        // derive a range of positions from a completed drag.
        // if dragType is "jog" or "sweep", set that up.
        this.dragItem = null;
        restoreAllMarks();
        this.defineSweepFromDrag(xSpec, ySpec, row, dragPositions, dragType);
      }).bind(chart);

      chart.defineSweepFromDrag = (function (xSpec, ySpec, row, dragPositions, dragType) {
        if (!this.lastMouseWasDrag) return;  // was only a click, not a drag

        this.nextJogSpec = null;             // it was a drag, so cancel any previous one
        if (dragPositions.length<2) return;  // but not a drag that we can use

        var linearise = false;    // config: whether to draw a straight line from start to end 
        var minDiff = 8;          // pixels, in either direction
        var chartPoints = [];
        var scenarios = [];
        var jogSpec = {};
        initJogSpec(jogSpec, xSpec, ySpec);  // extract dataset, scales etc
        if (linearise) {
          var start = dragPositions.first();
          var end = dragPositions.last();
          var xDiff = start.x ? end.x - start.x : 0;
          var yDiff = start.y ? end.y - start.y : 0;
          var nSteps = Math.floor(Math.min(Math.max(Math.abs(xDiff), Math.abs(yDiff)) / minDiff, 9));
          if (nSteps == 0) return;
          var xStep = xDiff / nSteps;
          var yStep = yDiff / nSteps;
          for (var s = 0; s <= nSteps; s++) {
            var evtPt = pt(start.x + (s*xStep), start.y + (s*yStep));
            chartPoints.push(toChartCoords(evtPt, true, this, jogSpec.xScale, jogSpec.yScale));
            scenarios.push(s+1);
          }
        } else {
          // evenly space along the user's drag path, for up to 10 scenarios
          calcDist = function(a, b) { var dx=a.x-b.x, dy=a.y-b.y; return Math.sqrt(dx*dx + dy*dy) };
          var aggDists = [0];           // aggregate distances from start
          var prev = dragPositions[0];
          var dist = 0;
          for (var i=1; i<dragPositions.length; i++) {
            var next = dragPositions[i];
            dist += calcDist(prev, next);
            aggDists.push(dist);
            prev = next;
          }
          var nSteps = Math.floor(Math.min(dist / minDiff, 9));
          if (nSteps == 0) return;
          
          var distStep = dist / nSteps;
          var aggInd = 0;
          for (var s=0; s<=nSteps; s++) {
            var nextDist = s*distStep;
            // find the first aggregate distance that is at least nextDist along the path
            for (; aggInd<aggDists.length && aggDists[aggInd]<nextDist; aggInd++) {}
            aggInd = Math.min(aggInd, aggDists.length-1);    // allow for precision error
            var nextPoint;
            if (Math.abs(aggDists[aggInd] - nextDist)<0.5) nextPoint = dragPositions[aggInd]; // a hit, to sub-pixel accuracy
            else {
              var ratio = (nextDist-aggDists[aggInd-1])/(aggDists[aggInd]-aggDists[aggInd-1]);
              var before = dragPositions[aggInd-1];
              var after = dragPositions[aggInd];
              nextPoint = pt(before.x + ratio*(after.x-before.x), before.y + ratio*(after.y-before.y))
            }
            chartPoints.push(toChartCoords(nextPoint, true, this, jogSpec.xScale, jogSpec.yScale));
            scenarios.push(s+1);
          }
        }
        jogSpec.row = row;
        jogSpec.pointRange = chartPoints;
        jogSpec.scenarios = scenarios;
        jogSpec.restorePoint = chartPoints.last();
        jogSpec.direction = -1;
        jogSpec.maxIndex = nSteps;
        jogSpec.index = jogSpec.maxIndex - 1;  // first below the starting point
        jogSpec.bounce = true;
        this.nextJogSpec = jogSpec;

        if (dragType) {         // a raw drag does nothing else at this point
          var self = this;
          var setUp = function() {
            if (dragType=="jog") startJog(self);
            else setUpSweep(self);
            }
          if (this.jogSpec) {
            endJog(this);        // clear any existing jog
            setTimeout(setUp, 400);  // and give it a chance to settle
          } else setUp();
        }
      }).bind(chart);
      
      chart.sortScenarioItems = (function () {
        // sort such that scenario 0's marks come at the end
        //if (debug) console.log("sorting");
        // NB: we exclude items that appear to have been assigned index-based keys by vega, because changing their order causes Vega to lose track of them.  We could probably do a much better job by adding the logic to sort within more constrained ranges - notably, ensuring we only sort against each other the elements for a single mark. 
        d3.select(this._el).selectAll("svg.marks rect, svg.marks path").filter(function(d) { return d && (d.scenario!==undefined) && !(d.key.toFixed)}).sort(function(a,b) { return b.scenario-a.scenario } );
      }).bind(chart);
      
      chart.on("mouseout", (function(evt, item) {
        if (this.dragItem) return;
        if (item.datarows) restoreAllMarks();
      }).bind(chart));

      chart.on("click", (function(evt, item) {  // NB: non-morphic event
        if (this.lastMouseWasDrag) return;      // this is just the mouse-up after a drag

        var clickType;
        if (evt.shiftKey) clickType = "jog";
        else if (evt.altKey) clickType = "sweep";
        if (clickType) {         // a raw click does nothing
          var self = this;
          var clickResponse = function() {
            setUpEditRange(self, item);
            if (clickType=="jog") startJog(self);
            else setUpSweep(self);
            }
          if (this.jogSpec) {
            endJog(this);        // clear any existing jog
            setTimeout(clickResponse, 400);  // and give it a chance to settle
          } else clickResponse();
        }
      }).bind(chart));
      
      chart.on("dblclick", (function(evt, item) {
        // console.log("double click on", item);
        if (item.dragx || item.dragy) {        // this is an item the user can drag
          var xSpec = parseDragSpec(item.dragx);   // may be null
          var ySpec = parseDragSpec(item.dragy);   // ditto
          var dataset = (xSpec && xSpec.dataset) || (ySpec && ySpec.dataset);
          if (dataset=="workingDataRanges") {
            var column = (xSpec && xSpec.column) || (ySpec && ySpec.column);
            var itemRow = JSON.parse(item.datarows)[0];
            window.Shiny.timestampedOnInputChange("trigger", { message: "clearControl", args: { dataset: dataset, row: itemRow, column: column } });
          }
        }
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
          this.lastMouseWasDrag = false;
          handle.dragStartPos = handle.lastDragPos = evt.getPosition();
          handle.dragPositions = [handle.dragStartPos];
          handle.itemRow = JSON.parse(item.datarows)[0];
          handle.chart = this;
          
          handle.xSpec = parseDragSpec(item.dragx);   // may be null
          handle.ySpec = parseDragSpec(item.dragy);   // ditto
          var dataset = handle.dataset = (handle.xSpec && handle.xSpec.dataset) || (handle.ySpec && handle.ySpec.dataset);
          if (dataset == "guessMList") {
            // HACK
            window.Shiny.shinyapp.sendInput({"showPopup": "TRUE"});
            $world.get("ShinyGigvisMorph1").popupChartMorph(3);
            };
          var msg = { message: "startEdit", args: [ dataset ] };
          window.Shiny.onInputChange("trigger", JSON.stringify(msg));

          // handle.onFocus = function() { console.log("handle focus") }
          // handle.onBlur = function() { console.log("handle blur") }
          
          handle.throttledDragHandler = Functions.throttle((function(evt) {
            var evtPos = evt.getPosition();
            this.lastDragPos = evtPos;
            this.dragPositions.push(evtPos);
            this.chart.lastMouseWasDrag = true;
            if (this.rangeLine) {
              var vs = this.rangeLine.vertices();
              vs[1] = vs[0].addPt(evtPos.subPt(this.rangeStartPos));
              this.rangeLine.setVertices(vs);
            }
            var xScale = this.xSpec && this.xSpec.scale;
            var yScale = this.ySpec && this.ySpec.scale;
            var xColumn = this.xSpec ? this.xSpec.column : "-";
            var yColumn = this.ySpec ? this.ySpec.column : "-";
            sendEditMessage(this.dataset, [xColumn, yColumn], this.itemRow, [0], [toChartCoords(evtPos, true, this.chart, xScale, yScale)]);
          }).bind(handle), 200);

          handle.addScript(function onDrag(evt) {
            this.focus();  // just to make sure
            this.throttledDragHandler(evt);
          }).bind(handle);

          handle.addScript(function onKeyDown(evt) {
            var keyCode = evt.getKeyCode()
            if (keyCode === Event.KEY_TAB) {
              // couldn't figure out whether there's a combination of stopPropagation etc
              // to allow the tab press to appear to onKeyPress.  So we deal with it here.
              // console.log("handle tab");
              Shiny.visitNextScenario(evt.isShiftDown() ? -1 : 1);
              evt.stop();
              return false;
            } else if (keyCode === Event.KEY_ALT || keyCode === Event.KEY_SHIFT) {
              // console.log("handle alt...");
              // Pressing alt or shift during a drag deletes all but the last recorded drag position.  But later releasing shift does nothing, whereas releasing alt immediately uses the positions up to then as a sweep.
              this.dragPositions = [this.dragPositions.last()];
              evt.stop();
              return false;
            }
          }).bind(handle);

          handle.onKeyUp = (function onKeyUp(evt) {
            if (evt.getKeyCode() === Event.KEY_ALT) {
              this.chart.defineSweepFromDrag(this.xSpec, this.ySpec, this.itemRow, this.dragPositions, "sweep");
              evt.stop();
              return false;
            }
          }).bind(handle);

          handle.addScript(function onKeyPress(evt) {
            // console.log(evt.charCode);
          }).bind(handle);

          handle.addScript(function onDropOn(morph) {
            if (this.dataset == "guessMList") {
              // HACK
              window.Shiny.shinyapp.sendInput({"showPopup": "FALSE"});
              $world.get("GigvisPopup3").remove();
            };
            var msg = { message: "endEdit", args: [ this.dataset ] };
            window.Shiny.onInputChange("trigger", JSON.stringify(msg));
            
            this.remove();
            var evt = Global.event;          // hack?
            var dragType = null;
            if (evt.isShiftDown()) dragType = "jog";
            else if (evt.isAltDown()) dragType = "sweep";
            this.chart.endDrag(this.xSpec, this.ySpec, this.itemRow, this.dragPositions, dragType);
          }).bind(handle);
          
          handle.getGrabShadow = function() { return new lively.morphic.Morph() };

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
  function createMouseOverHandler(plotId) {
    return function(event, item) {
      Shiny.onInputChange("ggvis_" + plotId + "_hover",
        {
          plot_id: plotId,
          data: item.datum.data,
          pagex: event.pageX,
          pagey: event.pageY
        }
      );
    };
  }

  // Returns a mouseout handler with plotId
  function createMouseOutHandler(plotId) {
    return function(event, item) {
      /* jshint unused: false */
      Shiny.onInputChange("ggvis_" + plotId + "_hover",
        {
          plot_id: plotId,
          data: null,
          pagex: null,
          pagey: null
        }
      );
    };
  }

  // Send information about the current brush
  function createBrushHandler(plotId) {
    return function(info) {
      info.items = info.items.map(function(item) {
        var newitem = $.extend({}, item.datum.data);
        newitem.key__ = item.key;
        return newitem;
      });
      Shiny.onInputChange("ggvis_" + plotId + "_brush", info);
    };
  }

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
    
    livelyDataTriggers = {};
    livelyEditRange = null;
    
    Shiny.propLatest = { x: null, y: null };
    Shiny.propHistory = { x: null, y: null };
    delete Shiny.lastColumnHighlight;
    Shiny.numScenarios = Shiny.visitScenario = 0;
  }
};

initShinyGgvis = function() {
  if (!window.shinyGgvisInitialized) oneTimeInitShinyGgvis();
  refreshShinyGgvis();
}

$(function(){ //DOM Ready
    if (!window.deferredShinyInit) initShinyGgvis();
  });
