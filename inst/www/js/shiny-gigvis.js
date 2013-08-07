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
    var chart = chart({ el: "#" + plotId, renderer: renderer });  // ael
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