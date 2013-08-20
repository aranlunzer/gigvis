$(function(){ //DOM Ready

  var gigvisOutputBinding = new Shiny.OutputBinding();
  $.extend(gigvisOutputBinding, {
    find: function(scope) {
      return $(scope).find('.shiny-gigvis-output');
    },
    onValueError: function(el, err) {
      Shiny.unbindAll(el);
      this.renderError(el, err);
    },
    // ael - this code is installed, but i'm not sure when it would be called
    renderValue: function(el, data) {
      vg.parse.spec(data.spec, function(chart) {
        chart({el: el}).update();
      });
    }
  });
  Shiny.outputBindings.register(gigvisOutputBinding, 'shiny.gigvisOutput');

});

// ael added
Shiny.addCustomMessageHandler("gigvis_forget_plot", function(message) {
  var plot = message.plot;
  if (allPlots[plot]) {
    allPlots[plot].data(null);
    delete allPlots[plot];
  }
  delete pendingData[plot];
});

// ael added
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
    var domPlotId = "#" + plotId;
    var chart = chart({ el: domPlotId, renderer: renderer });  // ael add renderer
    
    relatedItems = function(itemNums) {
      // ael: return a collection of vega-level items
      //debugger;
      var matches = []
      // a clumsy way of finding histogram rectangles
      d3.select(domPlotId).select("svg.marks").selectAll("rect")[0].forEach(function(domElem) {
        var item = domElem.__data__
        if (item && item.provenance) {
          splitProvenance(item.provenance).some(function(thisNum) {
            if (itemNums.indexOf(thisNum) >= 0) {
              matches.push(item);
              return true;
            } else { return false; }
          })
        }
      })
      // and a clumsy way of finding dots
      d3.select(domPlotId).select("svg.marks").selectAll("path")[0].forEach(function(domElem) {
        var item = domElem.__data__
        if (item && item.key !== undefined) {
          if (itemNums.indexOf(item.key+1) >= 0) {
              matches.push(item);
          }
        }
      })
      
      return matches;
    }
    
    splitProvenance = function(provString) {
      var nums = [];
      provString.split(",").forEach(function(nstr) { nums.push(Number(nstr)) });
      return nums;
    }

    if (true) { // (message.interactivitySpec) {
      chart.on("mouseover", function(event, item) {
        // console.log(item.provenance || item.key);  // NB item.key is zero-based
        if (item._svg.nodeName == "path") {
          chart.update({ props: "highlight", items: relatedItems([item.key+1]) })
          // window.Shiny.onInputChange("trigger", "in:"+(item.key+1))
        } else if (item._svg.nodeName == "rect") {
          var itemNums = splitProvenance(item.provenance);
          chart.update({ props: "highlight", items: relatedItems(itemNums) })
          // console.log(item.provenance);
        }
        })
      chart.on("mouseout", function(event, item) {
        /*
        if (item._svg.nodeName == "path") {
          window.Shiny.onInputChange("trigger", "out:"+(item.key+1))
        */
        // chart.update({ props: "update", duration: 1000, ease: "linear" })
        chart.update()
      })
    }
    $(domPlotId).data("gigvis-chart", chart);  // provide a way to access the view object
    chart.update();
  });
});

var pendingData = {};
Shiny.addCustomMessageHandler("gigvis_data", function(message) {
  var plot = message.plot;
  var name = message.name;
  var value = message.value[0].values;

  if (allPlots[plot]) {
    // If the plot exists already, feed it the data
    var dataset = {};
    dataset[name] = value;
    allPlots[plot].data(dataset);
    allPlots[plot].update();
  } else {
    // The plot doesn't exist, save it for when the plot arrives
    if (!pendingData[plot])
      pendingData[plot] = {}
    pendingData[plot][name] = value;
  }
});


// Receive a vega spec and parse it
Shiny.addCustomMessageHandler("gigvis_vega_spec", function(message) {
  var plotId = message.plotId;
  var spec = message.spec;
  var renderer = message.renderer || "canvas";  // ael

  vg.parse.spec(spec, function(chart) {
    console.log(spec);  // harmless and useful
    var chart = chart({ el: "#" + plotId, renderer: renderer });  // ael add renderer
    // ael experiment
    if (true) { // }(message.interactivitySpec) {
      chart.on("mouseover", function(event, item) {
        // debugger;
        // console.log(item.key);  // NB zero-based
        if (item._svg.nodeName == "path") {
          window.Shiny.onInputChange("trigger", "in:"+(item.key+1))
        }
      })
      chart.on("mouseout", function(event, item) {
        if (item._svg.nodeName == "path") {
          window.Shiny.onInputChange("trigger", "out:"+(item.key+1))
        }
      })
    }
    $("#" + plotId).data("gigvis-chart", chart);
    gigvisInit(plotId);
  });
});


var allPlots = {};
window.gigvisInit = function(plotId) {
  var chart = $("#" + plotId).data("gigvis-chart");
  allPlots[plotId] = chart;

  if (pendingData[plotId]) {
    // The data arrived earlier; use it.
    chart.data(pendingData[plotId]);
    chart.update();
    delete pendingData[plotId];
  }
};