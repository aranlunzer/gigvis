/*jshint forin:true, noarg:true, noempty:true, eqeqeq:true, bitwise:true,
    strict:false, undef:true, unused:true, browser:true, jquery:true, maxerr:50,
    curly:false, multistr:true */
/*global Shiny, vg*/
// oneTimeInitShinyGgvis should only be called once for a page, given that initShiny()
// in shiny.js retains the Shiny object if it's already defined.
oneTimeInitShinyGgvis = function() {
  shinyGgvisInitialized = true;

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

  var pendingData = {};
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

    vg.parse.spec(spec, function(chart) {
      var selector = ".ggvis-output#" + plotId;
      var $el = $(selector);
      chart = chart({ el: selector, renderer: renderer });
      $el.data("ggvis-chart", chart);
      ggvisInit(plotId);

      // When done resizing, update with new width and height
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
    });
  });


  // ael added.  a lot of replication of the above, but that should make it easier
  // to track code changes.
  // Receive a vega spec, complete with data, and parse it
  Shiny.addCustomMessageHandler("gigvis_vega_spec_with_data", function(message) {
    var plotId = message.plotId;
    var spec = message.spec;
    var renderer = message.renderer || "canvas";  // ael

    vg.parse.spec(spec, function(chart) {
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
      console.log(spec);  // harmless and useful
      var selector = ".ggvis-output#" + plotId;
      var $el = $(selector);
      var chart = chart({ el: selector, renderer: renderer });
      $el.data("ggvis-chart", chart);   // a convenient way to access the View object

      // instead of ggvisInit(plotId);
      chart.update();

    // ael: abandoned attempt to use d3 drag behaviour
/*
    var mydrag = d3.behavior.drag()
        .on("drag", function(item) {
          var rowString = item.provenance;
          var scaledX = item.mark.group.scales["x"].invert(d3.event.x).toFixed(2);
          var scaledY = item.mark.group.scales["y"].invert(d3.event.y).toFixed(2);
          window.Shiny.onInputChange("trigger", "drag:"+rowString+":"+String(scaledX)+":"+String(scaledY)+":"+String(d3.event.x)+":"+String(d3.event.y) )
          })
    
    chart.model().scene().items[0].items.forEach(function(series) {
      series.items.forEach(function(item) {
        if (item.drag) {
          d3.select(item._svg).call(mydrag);
        }
      })
    })
*/

      relatedItems = function(itemIDs) {
        // ael: return a collection of vega-level items
        //debugger;
        var matches = []
        // a clumsy way of finding histogram rectangles
        d3.select(selector).select("svg.marks").selectAll("rect")[0].forEach(function(domElem) {
          var item = domElem.__data__
          if (item && item.provenance) {
            (item.provenance.split(",")).some(function(thisID) {
              if (itemIDs.indexOf(thisID) >= 0) {
                matches.push(item);
                return true;
              } else { return false; }
            })
          }
        })
        // and a clumsy way of finding dots
        d3.select(selector).select("svg.marks").selectAll("path")[0].forEach(function(domElem) {
          var item = domElem.__data__
          if (item && item.provenance) {
            if (itemIDs.indexOf(item.provenance) >= 0) {
                matches.push(item);
            }
          }
        })
      
        return matches;
      }

      findScenarioMarks = function() {
          var scenarioMarks = [];
          var indices = [];
          d3.selectAll("g").each(function(f) {
            var el = $(this)[0];
            var s;
            try { s = el.firstChild.__data__.mark.def.description.scenario } catch(e) { s = undefined };
            if (s) {
              scenarioMarks.push(el);
              indices.push(s);
              }
          });
          d3.selectAll(scenarioMarks).data(indices);
          return scenarioMarks;
      }
    
      highlightScenarioMarks = function(marks, touchedScenario) {
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

      chart.on("mouseover", function(event, item) {
        if (item.mark && item.mark.def.description && item.mark.def.description.scenario) {
          var touchedScenario = item.mark.def.description.scenario
          var scenarioMarks = findScenarioMarks()
          highlightScenarioMarks(scenarioMarks, touchedScenario)
          d3.selectAll(scenarioMarks).sort(function(a,b) { if (a==touchedScenario) { return 1 } else if (b == touchedScenario) { return -1 } else { return 0 } });
        }
        if (item.provenance) {
          // chart.update({ props: "highlight", items: relatedItems(item.provenance.split(",")) })
        }
      })
      chart.on("mouseout", function(event, item) {
        // chart.update({ props: "update", duration: 1000, ease: "linear" })
        highlightScenarioMarks( findScenarioMarks(), 0 )
      })
      chart.on("mousedown", function (evt, item) {
        // dragx and dragy are comma-separated strings of the form
        //    scaleName,datasetName,columnName  (e.g. x,mtcars,wt)
        // we signal to R with structured messages (JSON encoded) such as
        //    message: set
        //    args:
        //      0 -> { dataset: "mtcars", column: "wt", row: 6, value: 3.5 }
        //      1 -> { dataset: "mtcars", column: "mpg", row: 6, value: 15 }
        if (item.dragx || item.dragy) {        // this is an item the user can drag
          var handle = lively.morphic.Morph.makeCircle(pt(0,0), 8, 2, Color.gray, Color.black);
          handle.itemRow = Number(item.provenance.substr(0,item.provenance.indexOf("/")));
          toList = function(dragSpec) {
            s = dragSpec.split(",");
            return { scale: s[0], dataset: s[1], column: s[2] }
          }
          
          if (item.dragx) handle.xSpec = toList(item.dragx)
          if (item.dragy) handle.ySpec = toList(item.dragy)

          handle.addScript(function onDrag(evt) {
            var now = new Date().getTime();
            if (this.lastTime && now - this.lastTime < 200) return;
            this.lastTime = now;

            var targetChart = $(".ggvis-output#plot1").data("ggvisChart");
            var rect = $(".ggvis-output#plot1")[0].getBoundingClientRect();
            var evtPos = evt.getPosition() // this.globalBounds().center();
            var padding = targetChart.padding();
            $world.cachedBounds = null;
            var worldRect = $world.getBounds()
            var chartTop = rect.top + padding.top - worldRect.top();
            var chartLeft = rect.left + padding.left - worldRect.left();
            var chartGroup = targetChart.model().scene().items[0];
            var args = [];
            var msg = { message: "set", args: args };
            if (this.xSpec) {
              var xVal = chartGroup.scales[this.xSpec.scale].invert(evtPos.x - chartLeft).toFixed(2);
              args.push( { dataset: this.xSpec.dataset, column: this.xSpec.column, row: this.itemRow, value: xVal } );
            }
            if (this.ySpec) {
              var yVal = chartGroup.scales[this.ySpec.scale].invert(evtPos.y - chartTop).toFixed(2);
              args.push( { dataset: this.ySpec.dataset, column: this.ySpec.column, row: this.itemRow, value: yVal } );
            }
            window.Shiny.onInputChange("trigger", JSON.stringify(msg))
            //console.log(msg)
          }).bind(handle);
          handle.addScript(function onDropOn(evt) { this.remove() }).bind(handle)
          //evt.hand.getPosition()
          evt.hand.grabMorph(handle, evt);
          handle.setPosition(pt(-4,-4));
          $world.draggedMorph = handle;
        }
      }.bind(chart))
    });
  });

  // Sets height and width of wrapper div to contain the plot area.
  // This is so that the resize handle will be put in the right spot.
  function updateGgvisDivSize(plotId) {
    var $el = $(".ggvis-output#" + plotId);
    var $plotarea = $el.find("div.vega > .marks");

    $el.width($plotarea.width());
    $el.height($plotarea.height());
  }

  var allPlots = {};
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
    // Doesn't seem to be much we can/should do here.
    pendingData = {};
    allPlots = {};
  }
};

initShinyGgvis = function() {
  if (!Global.shinyGgvisInitialized) oneTimeInitShinyGgvis();
  refreshShinyGgvis();
}

$(function(){ //DOM Ready
    if (!window.deferredShinyInit) initShinyGgvis();
  });
