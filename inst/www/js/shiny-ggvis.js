/*jshint forin:true, noarg:true, noempty:true, eqeqeq:true, bitwise:true,
    strict:false, undef:true, unused:true, browser:true, jquery:true, maxerr:50,
    curly:false, multistr:true */
/*global Shiny, vg*/
$(function(){ //DOM Ready

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

    relatedItems = function(itemNums) {
      // ael: return a collection of vega-level items
      //debugger;
      var matches = []
      // a clumsy way of finding histogram rectangles
      d3.select(selector).select("svg.marks").selectAll("rect")[0].forEach(function(domElem) {
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
      d3.select(selector).select("svg.marks").selectAll("path")[0].forEach(function(domElem) {
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
});
