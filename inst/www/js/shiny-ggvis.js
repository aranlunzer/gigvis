/*jshint forin:true, noarg:true, noempty:true, eqeqeq:true, bitwise:true,
    strict:false, undef:true, unused:true, browser:true, jquery:true, maxerr:50,
    curly:false, multistr:true */
/*global Shiny, vg*/
// oneTimeInitShinyGgvis should only be called once for a page, given that initShiny()
// in shiny.js retains the Shiny object if it's already defined.
oneTimeInitShinyGgvis = function() {
  window.shinyGgvisInitialized = true;

  var debug = true;

  var _ = window.lodash;

  // ael: a wrapper onto onInputChange, that adds a millisecond-level time field to
  // ensure that each value (e.g., message) is different from the previous one.
  // NB: assumes message has NOT yet been JSONised.
  Shiny.timestampedOnInputChange = function(name, messageObj) {
    //console.log(JSON.stringify($.extend(messageObj, { timestamp: new Date().getTime() })));
    Shiny.onInputChange(name, JSON.stringify($.extend(messageObj, { timestamp: new Date().getTime() })));
  }
  Shiny.chartNamed = function(chartName) {
    return $(".ggvis-output#"+chartName).data("ggvisChart")
  }
  Shiny.historyManager = function() {
    return lively.morphic.World.current().get("LivelyRHistory")
  }
  Shiny.livelyMessageQueue = [];
  Shiny.initialPlotState = null;
  Shiny.xMin = Shiny.xMax = Shiny.yMin = Shiny.yMax = 0;

  Shiny.addCustomMessageHandler("ggvis_lively_quiescent", function(message) {
    if (!Shiny.initialPlotState) Shiny.initialPlotState = message.plotState;  // first time since chart init
    Shiny.latestPlotState = message.plotState;
    var queue = Shiny.livelyMessageQueue;
    if (!queue.length) { console.log("Unexpected quiescence message"); return }
    
    var completionFn = queue.shift().completionFn;
    if (completionFn) {
      //console.log("found completion function");
      completionFn(message);
    }

    if (queue.length) queue[0].messageFn();
  });
  
  Shiny.sendMessage = function(message, forceUpdate, completionFn, channel) {
    // if forceUpdate is true we add a timestamp so the message is guaranteed to be noticed
    var ch = channel || "trigger";  // allow override of default channel
    var messageFn = forceUpdate ? Shiny.timestampedOnInputChange.curry(ch, message) : Shiny.onInputChange.curry(ch, message);

    //if (completionFn && typeof completionFn != "function") debugger;

    var queue = Shiny.livelyMessageQueue;

    queue.push({ messageFn: messageFn, completionFn: completionFn });
    if (queue.length == 1) queue[0].messageFn();
  }
  
  Shiny.sendReactiveValue = function(varName, value) {
    // varName becomes the channel, value the message
    Shiny.sendMessage(value, false, null, varName);  // false means we won't add a timestamp
  }
  
  Shiny.buildAndSendMessage = function(type, args, completionFn, channel) {
    // type is "switch", "data", "command", "parameter"
    var message = Shiny.buildMessage(type, args);
    Shiny.sendMessage(message, true, completionFn, channel)
  }

  Shiny.buildMessage = function(type, args, annotations) {
    var funcTable = { switch: Shiny.buildSwitchMessage, data: Shiny.buildDataMessage, command: Shiny.buildCommandMessage, parameter: Shiny.buildParameterMessage };
    var message = funcTable[type].apply(null, args);
    if (annotations) message.annotations = annotations;
    return message;
  }

  Shiny.buildSwitchMessage = function(parameter, value) {
    return { message: "setSwitch", args: { parameter: parameter, value: value } };
  }

  Shiny.buildCommandMessage = function(command, args) {
    return { message: command, args: args };
  }

  Shiny.buildDataMessage = function(dataset, xycolumns, row, scenarios, chartPoints) {
    // edits that arise from dragging or perturbing chart points
    var args = {};
    args.type = "data";
    args.target = { dataset: dataset, row: row, xycolumns: xycolumns };
    args.scenarios = scenarios;
    args.values = chartPoints.map(function(cp) {
      var xy = ["-1000", "-1000"];
      if (cp.x) xy[[0]] = cp.x.toPrecision(4);  // cf toFixed(3)
      if (cp.y) xy[[1]] = cp.y.toPrecision(4);
      return xy;
    });
    return { message: "edit", args: args };
  }

  Shiny.buildParameterMessage = function(parameter, scenarios, values) {
    var args = { type: "parameter", target: parameter, scenarios: scenarios, values: values };
    return { message: "edit", args: args };
  }

  Shiny.suppressEdits = function(bool) {
    Shiny.buildAndSendMessage("command", [ "suppressEdits", { bool: bool } ]);
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

    plot.parseSpec(spec);
  });

  // a bunch of functions added by ael.
  // a lot of replication of the above, but that should make it easier
  // to track code changes.

  // Receive a vega spec and parse it
  Shiny.addCustomMessageHandler("ggvis_lively_vega_spec", function(message) {
    var chartId = message.chartId;
    // var version = message.version;
    var spec = message.spec;
    var renderer = message.renderer;
    console.log("ggvis_lively_vega_spec", chartId, spec);

    buildLivelyChart(spec, chartId, renderer);
    if (chartId=="plot1") Shiny.buildAndSendMessage("command", [ "finishedBuild", {} ]);

    $world.setHandStyle(null);
  });

  // receive a data set for a chart whose spec we (these days) assume has already been sent
  Shiny.addCustomMessageHandler("ggvis_lively_data", function(message) {
    // new regime (post-v0.2): message elements are chartId and valueList.
    // we've also introduced "r_default_axisSpec", a pseudo-dataset used to set axis properties.
    var chartId = message.chartId;
    // Check that the data spec belongs to a chart that's already rendered.
    var chart = Shiny.chartNamed(chartId);
    if (chart) {
      var valueList = message.valueList;   // a list (i.e., object) mapping dataName -> value
      // each value associated with a dataName is expected to be a one-element array containing
      //   { name: n, values: valueArray }  (see as.vega.data.frame), or
      //   { name: format: values: {children: } }  for a xxx_tree dataset
      var axesChanged = false;
      var needsSort = false;

      var dataNames = Object.keys(valueList);
      if (debug) console.log("data", chartId, dataNames.join());
      dataNames.forEach(function(name) {
        var dataSpec = valueList[name][0];
        var values = dataSpec.values;
        // HUMONGOUS HAIRY HACK for dealing with tree data
        // if there really isn't a cleaner way of doing this, we should add one
        if (values.children) {
          var treeDef = chart.model().defs().data.load[name];
          treeDef[0].children = values.children;    // hackadelica
          var subDataSpec = {};
          subDataSpec[name] = treeDef;
          chart.data(subDataSpec);
        } else {
          var formattedData = vg.data.read(values, dataSpec.format);
          if (name=="r_default_axisSpec") {
            // special dataset for (re)defining the axes and scales
            updateAxes(chart, formattedData);
            axesChanged = true;      // NB: in fact we might just be redefining dataMin and dataMax
          } else {
            var subData = {};
            subData[name] = formattedData;
            chart.data(subData);
            // if (name=="r_sweep_scatter") console.log(subData[name]);  // often useful  :-)
            needsSort = subData[name].length>0 && (subData[name][0].scenario!==undefined);
          }
        }
      });  // end of forEach(name)

      $world.setHandStyle(null);

      if (axesChanged) {
        chart.model()._reset.axes = true;
        chart.model().reset();
      }
      chart.update({duration: message.duration});

      if (needsSort) chart.sortScenarioItems();
      chart.highlightDragItems();
    }
  });
  
  function updateAxes(chart, axisData) {
    // a pretty hacky delve into Vega's encoding for axes
    for (var i=0; i<axisData.length; i++) {
      var axisSpec = axisData[i];
      var scaleName = axisSpec.scale;
      var axisDef = $.grep(chart.model().defs().marks.axes, function(d) { return d.scale==scaleName })[0];
      var newTitle = axisSpec.title;
      var suppress = (newTitle == "");
      axisDef.title = newTitle;
      var properties = axisDef.properties || {};
      var lineColour = (scaleName=="yhist" ? "gray" : "black");
      var labelColour = (scaleName=="yhist" ? "gray" : "black");
      if (suppress) lineColour = labelColour = null;
      properties.axis = { stroke: { value: lineColour } };
      properties.ticks = { stroke: { value: lineColour }};
      properties.labels = { fill: { value: labelColour }};
      axisDef.properties = properties;
      if (!suppress) {
        var scaleDef = $.grep(chart.model().defs().marks.scales, function(d) { return d.name==scaleName })[0];
        var newMax = axisSpec.max;
        scaleDef.domain[1] = axisSpec.max;
      }
      if (axisSpec.dataMin) Shiny[[axisSpec.scale+"Min"]] = axisSpec.dataMin;
      if (axisSpec.dataMax) Shiny[[axisSpec.scale+"Max"]] = axisSpec.dataMax;
    }
  // console.log("updated axes: ", Shiny.xMin, Shiny.xMax, Shiny.yMin, Shiny.yMax);
  }

  function buildLivelyChart(spec, chartId, renderer) {
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

      chart.update();
      
      function allCharts() {
        var charts = [];
        $(".ggvis-output").each(function(i, el) {
          var ch = $(el).data("ggvis-chart");
          if (ch) charts.push(ch);
        });
        return charts;
      }

      function firstDataItem(d) { return (d instanceof Array && d[0]) || d }

      function allMarkableElements(chart) {
        // ael: return a d3 selection with all svg elements that can be marked, within a given chart
        return d3.select(chart._el).selectAll("svg.marks rect, svg.marks path, svg.marks tr").filter(function(d) { return d && firstDataItem(d).hasOwnProperty("datarows") });
      }

      function allMarkableItems(chart) {
        // ael: return a collection of all vega-level items that can be marked, within a given chart
        var matches = [];
        allMarkableElements(chart)[0].each(function(el) { matches.push(firstDataItem(el.__data__)) });
        // d3.select(chart._el).selectAll("svg.marks rect, svg.marks path, svg.marks tr").filter(function(d) { return d && d.datarows })[0].each(function(el) { matches.push(el.__data__) });
        // console.log("allMarkableItems: ", matches.length);
        return matches
      }
      
      function parsedDataRows(item) {
        // assume there is a datarows element, and that it's a JSON-encoded collection.
        // iff the item is part of a symbol mark, cache the result (because once assigned it can't change).
        var rows = item.cachedDataRows;  // might not be there
        if (!rows) {
          rows = JSON.parse(item.datarows);
          if (item.mark.marktype == "symbol") item.cachedDataRows = rows;
        }
        return rows;
      }
      
      function dataRowOrRole(item) {
        // if the item has a datarows element, return the first entry in that collection.
        if (item.datarows) return parsedDataRows(item)[0];
        
        return item.datarole;  // better have one...
      }
      
      function relatedItems(chart, item) {
        // ael: return a collection of vega-level items
        //debugger;
        
        var rownumbers = parsedDataRows(item);
        // console.log(rownumbers[0], item);
        var data_id = item.mark.def.description.datasource;
        var p = data_id.indexOf(":");
        if (p != -1) data_id = data_id.substr(0, p);
        
        var matches = [];
        // allMarkableItems returns just items that have a datarows member.  for some marks that don't specifically correspond to a subset of the data rows we attach an empty datarows array.
        allMarkableItems(chart).each(function(item) {
          if (item.mark.def.description.datasource.indexOf(data_id) == 0) {
            parsedDataRows(item).some(function(thisID) {
              if (rownumbers.indexOf(thisID) >= 0) {
                matches.push(item);
                return true;
              } else { return false; }
            })
          }
        })
        // console.log(matches);
        if (matches.length==0) matches.push(item);
        return matches;
      }

      function scenarioItems(chart, touchedScenario) {
        var matches = [];
        allMarkableItems(chart).forEach(function(item) {
          if (item.scenario == touchedScenario) matches.push( item );
        });
        // console.log(matches);
        return matches;
      }

      function parseDragSpec(dragSpec) {
        // a dragSpec has comma-separated components  scale (which can be "tablecell" for a cell drag, or have a suffix, eg. "x:percent"), dataset, column
        // apr 2014: we now ignore the optional fourth component that used to signify a dataset to watch for
        if (!dragSpec) return null;

        var s = dragSpec.split(",");
        var spec = { scale: s[0], dataset: s[1], column: s[2] };
        //if (s.length>3) spec.triggerName = s[3];
        return spec;
      }
      Shiny.parseDragSpec = parseDragSpec;

      //viewDivObj.on("blur", function() { if (debug) console.log("div blur") });
      //viewDivObj.on("focus", function() { if (debug) console.log("div focus") });
      viewDivObj.on("mouseover", function() {
        // grab focus if this is a chart that wants it, and there isn't currently a drag
        if (viewDivObj.attr("livelyautofocus")=="true" && !viewDivObj.data("ggvisChart").dragItem) viewDivObj.focus();
      });
      
      viewDivObj.on("keydown", (function(evt) {
        // console.log(evt.keyCode);
        if (evt.keyCode === Event.KEY_ESC) {
          //if (debug) console.log("ESCAPE");
          Shiny.historyManager().resetJogAndSweep();
          return false;
        }
        if (evt.keyCode === Event.KEY_TAB) {
          //if (debug) console.log("TAB");
          setTimeout(function() { Shiny.historyManager().visitNextScenario(evt.shiftKey ? -1 : 1)}, 1);
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
        if (this.dragItem) return;   // (dragItem is also set when dragging in a cell)

        // look for some scenario highlighting to do
        var touchedScenario = item.scenario;
        if (touchedScenario) {  // won't trigger on scenario 0, which is probably fine...?
          allCharts().forEach(function(ch) {
            ch.update({ props: "update", items: allMarkableItems(ch) });
            ch.update({ props: "scenarioHighlight", items: scenarioItems(ch, touchedScenario) });
          });
          Shiny.historyManager().highlightHistoryPseudoIndex(touchedScenario);

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
          Shiny.historyManager().highlightHistoryPseudoIndex(-1);
      }
      
      chart.highlightDragItems = (function () {
          if (this.dragItem && this.dragItem.datarows) {
            var dragItem = this.dragItem;
            allCharts().forEach(function(ch) {
              ch.update({ props: "highlight", items: relatedItems(ch, dragItem) })
            });
          }
          if (this.dragCell) {
            var origC = Global.apps.ColorParser.getColorFromString("orange");
            var newCString = origC.withA(0.5).toRGBAString();
            this.dragCell.style.backgroundColor = newCString;
            // this.dragCell.style.setProperty("background-color", newCString, null);
          }
      }).bind(chart);
      
      chart.endDrag = (function (xSpec, ySpec, row, dragPositions, pointConverter, dragType) {
        // derive a range of positions from a completed drag.
        // if dragType is "jog" or "sweep", set that up.
        if (this.dragCell) this.dragCell.style.backgroundColor = null;
        this.dragItem = this.dragCell = null;
        restoreAllMarks();
        
        if (this.lastMouseWasDrag) { // ensure it was a drag, not just a click
          Shiny.historyManager().addValueDragItem(this, xSpec, ySpec, row, dragPositions, pointConverter);

          if (dragType == "jog") {
            Shiny.historyManager().startJog()
            // make sure the view has keyboard focus, to allow Escape to stop the jog
            chart.viewDivObj.focus();
          } else if (dragType == "sweep") Shiny.historyManager().selectItemForSweep();
        }  
      }).bind(chart);

      chart.localPointToGlobal = (function (localPt) {
        var chartRect = $(this._el).bounds();
        var padding = this.padding();
        chartTop = chartRect.top + padding.top;
        chartLeft = chartRect.left + padding.left;
        return pt(localPt.x+chartLeft, localPt.y+chartTop);
      }).bind(chart);

      chart.toChartCoords = (function (evtPt, absolute, xScale, yScale) {
        // based on the x and y drag specs of the specified element, turn the 
        // event point into an abstract point with coords in x and/or y.
        // if absolute is true, assume we need to subtract the chart origin.
        var chartTop = 0, chartLeft = 0;
        if (absolute) {
          var chartRect = $(this._el).bounds();
          var padding = this.padding();
          chartTop = chartRect.top + padding.top;
          chartLeft = chartRect.left + padding.left;
        }
        var chartGroup = this.model().scene().items[0];
        var xyScales = [xScale, yScale], xyEvtCoords = [evtPt.x, evtPt.y], xyOffsets = [chartLeft, chartTop], xyMins = [Shiny.xMin, Shiny.yMin], xyMaxs = [Shiny.xMax, Shiny.yMax], xyConverted = [null, null];
        for (var xy=0; xy<2; xy++){
          if (xyScales[xy]) {
            var scaleSplit = xyScales[xy].split(":");
            xyConverted[xy] = chartGroup.scales[scaleSplit[0]].invert(xyEvtCoords[xy] - xyOffsets[xy]);
            if (scaleSplit.length>1 && scaleSplit[1]=="percent") {  // range controls are x:percent, y:percent
              xyConverted[xy] = Math.round((xyConverted[xy]-xyMins[xy])/(xyMaxs[xy]-xyMins[xy])*100);              
            }
          }
        }
        return pt(xyConverted[0], xyConverted[1]);
      }).bind(chart);
      
      chart.sortScenarioItems = (function () {
        // sort such that scenario 0's marks come at the end
        //if (debug) console.log("sorting");
        // NB: we exclude items that appear to have been assigned index-based keys by vega, because changing their order causes Vega to lose track of them.  We could probably avoid this hassle by adding the logic to sort within more constrained ranges - notably, ensuring we only sort against each other the elements for a single mark. 
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
          var manager = Shiny.historyManager();
          manager.addPerturbationItem(self, item, dataRowOrRole(item));

          if (clickType=="jog") {
            manager.startJog();      // on the newly added item; includes ending any previous jog
            // make sure the view has keyboard focus, to allow Escape to stop the jog
            chart.viewDivObj.focus();
          } else manager.endJogThenDo(manager.selectItemForSweep.bind(manager));
        }
      }).bind(chart));
      
      chart.on("dblclick", (function(evt, item) {
        // console.log("double click on", item);
        if (item.dragx || item.dragy) {        // this is an item the user can drag
          var xSpec = parseDragSpec(item.dragx);   // may be null
          var ySpec = parseDragSpec(item.dragy);   // ditto
          var dataset = (xSpec && xSpec.dataset) || (ySpec && ySpec.dataset);
          if (dataset.indexOf("workingDataRanges")==0) {  // table or chart (raw) instance
            var column = (xSpec && xSpec.column) || (ySpec && ySpec.column);
            var itemRow = dataRowOrRole(item);
            Shiny.buildAndSendMessage("command", [ "clearControl", { dataset: "workingDataRanges", row: itemRow, column: column } ]);
            var returnValue = itemRow & 1 ? 0 : 100;
            var pseudoPosition = pt(xSpec ? returnValue : -1000, ySpec ? returnValue : -1000);
            var pseudoConverter = function(pt) { return pt }
            Shiny.historyManager().addValueDragItem(this, xSpec, ySpec, itemRow, [pseudoPosition], pseudoConverter);
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

        //console.log(item);
        // DEMO HACK
        var doGather = false;
        try { doGather = (item.mark.def.description.datasource == "r_default_lmLine") } catch(e) {};
        if (doGather) Shiny.historyManager().gatherValueOverEditSequence();

        this.lastMouseWasDrag = false;
        this.lastDragEvent = null;

        if (item.dragx || item.dragy) {        // this is an item the user can drag
          var handleSize = 8;
          var handle = lively.morphic.Morph.makePolygon(
                [pt(-handleSize, 0), pt(handleSize, 0), pt(0, 0), pt(0, -handleSize), pt(0, handleSize), pt(0,0)], 3, Color.black, Color.black);

          this.dragItem = item;
          handle.itemRow = dataRowOrRole(item);
          handle.chart = this;
          
          handle.xSpec = parseDragSpec(item.dragx);   // may be null
          handle.ySpec = parseDragSpec(item.dragy);   // ditto
          var dataset = handle.dataset = (handle.xSpec && handle.xSpec.dataset) || (handle.ySpec && handle.ySpec.dataset);
          Shiny.buildAndSendMessage("command", [ "startEdit", { dataset: dataset, row: handle.itemRow } ]);

          if (dataset == "guessMList") {
            // HACK
            setTimeout(function() {
              window.Shiny.shinyapp.sendInput({"showPopup": "TRUE"});
              $world.get("ShinyGigvisMorph1").popupChartMorph(3);
            }, 100);    // leave a little time for startEdit
          };

          // set up the function for interpreting mouse points during this manipulation
          var xScale = handle.xSpec && handle.xSpec.scale;
          var yScale = handle.ySpec && handle.ySpec.scale;
          var xColumn = handle.xSpec ? handle.xSpec.column : "-";
          var yColumn = handle.ySpec ? handle.ySpec.column : "-";
          var itemStartPosition = evt.getPosition();  // though this will be refined for on-chart items
          // dragging in cells is indicated by "tablecell" scale
          if (xScale == "tablecell") {
            this.dragCell = item._element;
            handle.dragStartValue = item.value;
            var chartWidth = 600;  // ought to look it up
            handle.convertEvtPoint = function(evtPos) { return pt(Math.min(100, Math.max(0, Math.round(handle.dragStartValue + (evtPos.x - handle.dragStartPos.x)*100/(0.33*chartWidth)))), -1000) };
          } else {
            // if we use evt.getPosition() as the start of the drag on a chart item, it will be 
            // offset from the coord of the item being dragged.
            itemStartPosition = this.localPointToGlobal(pt(item.x, item.y));
            handle.convertEvtPoint = function(evtPos) { return handle.chart.toChartCoords(evtPos, true, xScale, yScale) };
            console.log("axis defs at drag start: ", Shiny.xMin, Shiny.xMax, Shiny.yMin, Shiny.yMax);
          }
          handle.dragStartPos = handle.lastDragPos = itemStartPosition;
          handle.dragPositions = [handle.dragStartPos];
          handle.dragToPoint = function(evtPos) { 
            Shiny.buildAndSendMessage("data", [ handle.dataset, [xColumn, yColumn], handle.itemRow, [0], [handle.convertEvtPoint(evtPos)] ])
          };

          // handle.onFocus = function() { console.log("handle focus") }
          // handle.onBlur = function() { console.log("handle blur") }
          
          handle.throttledDragHandler = Functions.throttle((function(evt) {
            this.lastDragEvent = evt;
            var evtPos = evt.getPosition();
            this.lastDragPos = evtPos;
            this.dragPositions.push(evtPos);
            this.chart.lastMouseWasDrag = true;
            if (this.rangeLine) {
              var vs = this.rangeLine.vertices();
              vs[1] = vs[0].addPt(evtPos.subPt(this.rangeStartPos));
              this.rangeLine.setVertices(vs);
            }
            this.dragToPoint(evtPos);
          }).bind(handle), 300);       // @@ not sure what makes for a good time value here

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
              Shiny.historyManager().visitNextScenario(evt.isShiftDown() ? -1 : 1);
              evt.stop();
              return false;
            } else if (keyCode === Event.KEY_ALT || keyCode === Event.KEY_SHIFT) {
              // console.log("handle alt...");
              // Pressing alt or shift during a drag deletes all but the last recorded drag position.  But later releasing shift does nothing, whereas releasing alt immediately uses the positions up to then as a sweep.
              if (keyCode === Event.KEY_ALT) this.setBorderColor(Color.red);
              this.dragPositions = [this.dragPositions.last()];
              evt.stop();
              return false;
            }
          }).bind(handle);

          handle.onKeyUp = (function onKeyUp(evt) {
            // *** early setup of sweep is currently disabled *** 
            if (false && evt.getKeyCode() === Event.KEY_ALT) {
              this.setBorderColor(Color.black);
              Shiny.historyManager().addValueDragItem(this.chart, this.xSpec, this.ySpec, this.itemRow, this.dragPositions, this.convertEvtPoint);
              Shiny.historyManager().selectItemForSweep();
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
            Shiny.buildAndSendMessage("command", [ "endEdit", { dataset: this.dataset } ]);
            
            this.remove();
            var dragType = null;
            var evt = this.lastDragEvent;
            if (evt) {
              if (evt.isShiftDown()) dragType = "jog";
              else if (evt.isAltDown()) dragType = "sweep";
            }
            this.chart.endDrag(this.xSpec, this.ySpec, this.itemRow, this.dragPositions, this.convertEvtPoint, dragType);
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

  // ---------------------------------------------------------------------------
  // Interaction event handlers
  // These are defined here instead of ggvis.js because at present all of the
  // event handlers use shiny.
  // ---------------------------------------------------------------------------
  // Keyboard handler
  // Sends ggvis_xxxx_key_press events
  ggvis.handlers.keyboard = (function() {
    var keyboard = function(plot, h_spec) {
      this.plot = plot;
      this.h_spec = h_spec;

      // jQuery event ID for naming event handlers and removing later
      this._eventId = "ggvis_" + h_spec.id;
      // The prefix to the shiny input name
      this._inputId = "ggvis_" + h_spec.id + "_key_press";
      // Used for keeping track of number of key events. Needed so that Shiny
      // will send info when same key is pressed multiple times in a row.
      this._counter = 0;

      var self = this;

      // keypress handler works for regular character keys
      $(document).on("keypress." + this._eventId, function(e) {
        var str = String.fromCharCode(e.which);
        self._sendValue(str);
      });

      // keydown handler for special keys that aren't caught by keypress,
      // like arrows
      $(document).on("keydown." + this._eventId, function(e) {
        var str = keycodes[e.which];
        if (str) {
          self._sendValue(str);
        }
      });
    };

    // Mappings for keycodes of special characters
    var keycodes = {
      8: "backspace",
      9: "tab",
      27: "esc",
      37: "left",
      38: "up",
      39: "right",
      40: "down",
      46: "delete"
    };

    var prototype = keyboard.prototype;

    prototype.remove = function() {
      $(document).off("keypress." + this._eventId);
      $(document).off("keydown." + this._eventId);
    };

    prototype._sendValue = function(str) {
      this._counter++;
      Shiny.onInputChange(this._inputId, {
        value: str,
        _nonce: this._counter
      });
    };

    return keyboard;
  })(); // ggvis.handlers.keyboard


  // ---------------------------------------------------------------------------
  // Hover handler
  // Sends ggvis_xxxx_mouse_over and ggvis_xxxx_mouse_out events
  ggvis.handlers.hover = (function() {
    var hover = function(plot, h_spec) {
      this.plot = plot;
      this.h_spec = h_spec;

      // Event ID for naming event handlers and removing later
      this._eventId = "ggvis_" + h_spec.id;
      // The prefix to the shiny input name
      this._inputIdPrefix = "ggvis_" + h_spec.id;
      // Used for keeping track of number of events. Needed so that Shiny
      // will send info when mouse_out event happens multiple times.
      this._nonce_counter = 0;

      plot.chart.on("mouseover." + this._eventId, this._createMouseOverHandler());
      plot.chart.on("mouseout."  + this._eventId, this._createMouseOutHandler());
    };

    var prototype = hover.prototype;

    prototype.remove = function() {
      this.plot.chart.off("mouseover." + this._eventId);
      this.plot.chart.off("mouseout."  + this._eventId);
    };

    prototype._createMouseOverHandler = function() {
      var self = this;
      return function(event, item) {
        Shiny.onInputChange(self._inputIdPrefix + "_mouse_over",
          {
            plot_id: self.plot.plotId,
            data: item.datum.data,
            pagex: event.pageX,
            pagey: event.pageY,
            _nonce: self._nonce_counter
          }
        );
        self._nonce_counter++;
      };
    };

    prototype._createMouseOutHandler = function() {
      var self = this;
      return function(event, item) {
        /* jshint unused: false */
        Shiny.onInputChange(self._inputIdPrefix + "_mouse_out",
          {
            plot_id: self.plot.plotId,
            _nonce: self._nonce_counter
          }
        );
        self._nonce_counter++;
      };
    };

    return hover;
  })(); // ggvis.handlers.hover

  // ---------------------------------------------------------------------------
  // Click handler
  // Sends ggvis_xxxx_mouse_click
  ggvis.handlers.click = (function() {
    var click = function(plot, h_spec) {
      this.plot = plot;
      this.h_spec = h_spec;

      // Event ID for naming event handlers and removing later
      this._eventId = "ggvis_" + h_spec.id;
      // The prefix to the shiny input name
      this._inputIdPrefix = "ggvis_" + h_spec.id;
      // Used for keeping track of number of events. Needed so that Shiny
      // will send info when mouse_out event happens multiple times.
      this._nonce_counter = 0;

      plot.chart.on("click." + this._eventId, this._createMouseClickHandler());
    };

    var prototype = click.prototype;

    prototype.remove = function() {
      this.plot.chart.off("click."  + this._eventId);
    };

    prototype._createMouseClickHandler = function() {
      var self = this;
      return function(event, item) {
        Shiny.onInputChange(self._inputIdPrefix + "_mouse_click",
          {
            plot_id: self.plot.plotId,
            data: item.datum.data,
            pagex: event.pageX,
            pagey: event.pageY,
            _nonce: self._nonce_counter
          }
        );
        self._nonce_counter++;
      };
    };

    return click;
  })(); // ggvis.handlers.click


  // ---------------------------------------------------------------------------
  // Brush handler
  // Sends ggvis_xxxx_mouse_brush
  ggvis.handlers.brush = (function() {
    var brush = function(plot, h_spec) {
      this.plot = plot;
      this.h_spec = h_spec;

      // Event ID for naming event handlers and removing later
      this._eventId = "ggvis_" + h_spec.id;
      // The prefix to the shiny input name
      this._inputIdPrefix = "ggvis_" + h_spec.id;

      var policy = h_spec.policy || "debounce";
      var policy_fun = _[policy];
      var delay = h_spec.delay || 100;

      var brushHandler = policy_fun(this._createBrushHandler(), delay);

      plot.brush.on("updateItems." + this._eventId, brushHandler);
    };

    var prototype = brush.prototype;

    prototype.remove = function() {
      this.plot.brush.off("updateItems." + this._eventId);
    };

    // Send information about the current brush
    prototype._createBrushHandler = function() {
      var self = this;
      return function(info) {
        info.items = info.items.map(function(item) {
          var newitem = $.extend({}, item.datum.data);
          newitem.key__ = item.key;
          return newitem;
        });

        // Get x and y coordinates relative to the page
        var offset = self.plot.getVegaDiv().offset();
        var padding = self.plot.chart.padding();
        info.pagex1 = info.x1 + offset.left + padding.left;
        info.pagex2 = info.x2 + offset.left + padding.left;
        info.pagey1 = info.y1 + offset.top  + padding.top;
        info.pagey2 = info.y2 + offset.top  + padding.top;

        Shiny.onInputChange(self._inputIdPrefix + "_brush_move", info);
      };
    };

    return brush;
  })(); // ggvis.handlers.brush


  // ---------------------------------------------------------------------------
  // Resize handler
  // Sends ggvis_xxxx_resize
  ggvis.handlers.resize = (function() {
    var resize = function(plot, h_spec) {
      this.plot = plot;
      this.h_spec = h_spec;

      // Event ID for naming event handlers and removing later
      this._eventId = "ggvis_" + h_spec.id;
      // The prefix to the shiny input name
      this._inputIdPrefix = "ggvis_" + h_spec.id;

      this.plot.on("resize." + this._eventId, this._createResizeHandler());
    };

    var prototype = resize.prototype;

    prototype.remove = function() {
      this.plot.off("resize." + this._eventId);
    };

    // Returns a function which takes an event
    prototype._createResizeHandler = function() {
      var self = this;
      return function(event) {
        Shiny.onInputChange(self._inputIdPrefix + "_resize",
          {
            plot_id: self.plot.plotId,
            width: event.width,
            height: event.height,
            padding: event.padding
          }
        );
      };
    };

    return resize;
  })(); // ggvis.handlers.resize


  // ---------------------------------------------------------------------------
  // Handlers for messages sent from Shiny server to client
  ggvis.messages = (function() {
    var messages = {
      _handlers: {}   // Registry of ggvis message handlers
    };

    // Register a handler function for messages of a given type
    // handler should have signature function(data, id)
    messages.addHandler = function(type, handler) {
      messages._handlers[type] = handler;
    };

    // Remove handler function for messages of a given type
    messages.removeHandler = function(type) {
      delete messages._handlers[type];
    };

    // Handle custom messages with this format:
    // {
    //   "custom": {
    //     "ggvis_message": {
    //       "type": "show_tooltip",
    //       "id": null,
    //       "data": {
    //         "pagex":    -63,
    //         "pagey":    196,
    //         "html": "text here"
    //       }
    //     }
    //   }
    // }
    Shiny.addCustomMessageHandler('ggvis_message', function(msg) {
      var type = msg.type;
      if (!type) return;

      // Grab the appropriate handler function for this type of message
      var handler = messages._handlers[type];
      if (!handler) return;

      handler(msg.data, msg.id);
    });

    return messages;
  })(); // ggvis.messages


  // ---------------------------------------------------------------------------
  // Message handlers

  // Tooltip message handlers
  ggvis.messages.addHandler("show_tooltip", function(data, id) {
    /* jshint unused: false */
    // Remove any existing tooltips
    $('.ggvis-tooltip').remove();

    // Add the tooltip div
    var $el = $('<div id="ggvis-tooltip" class="ggvis-tooltip"></div>')
      .appendTo('body');

    $el.html(data.html);
    $el.css({
      left:  data.pagex,
      top:   data.pagey
    });
  });

  ggvis.messages.addHandler("hide_tooltip", function(data, id) {
    /* jshint unused: false */
    $('.ggvis-tooltip').remove();
  });

  refreshShinyGgvis = function() {
    // The user is throwing away the existing chart(s) and building anew. 
    Shiny.livelyMessageQueue = [];
    Shiny.initialPlotState = null;
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
