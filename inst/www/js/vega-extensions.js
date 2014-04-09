// various extensions and hacks to vega
// Aran Lunzer, March 2014

// table function adapted from example at
// http://www.devforrest.com/blog/scrollable-sortable-html-table-with-static-headers-using-d3-js/
function drawTable(rowItems, xProp, yProp, minimaRow, maximaRow, table_element, dimensions) {
    // removed sorting code.  if you want it back, look at gigvis-expt4 or earlier.
    // The supplied table_element is an xhtml body element within a foreignObject, embedded
    // in the svg for a chart.
    // Within this we build a single table (class table.outerTable) that has one inner table
    // for the column headers.  This has two rows: "tr.headerNames" and "tr.headerXY".
    // We also create a "tbody.mainTable" that will hold the main data rows.
    var width = dimensions.width + "px";
    var height = dimensions.height + "px";
    var twidth = (dimensions.width - 20) + "px";    // width for the inner tables - leave some room for scroller
    var dataTHeight = (dimensions.height - 60) + "px";

    var dataColumns = Object.keys(rowItems[0].datum.data);  // start with all columns
    dataColumns.remove("originalrow");
    dataColumns.remove("filtering");
    dataColumns.remove("editedColumns");
    
    var editedColStr = minimaRow.editedColumns;
    var editedMinima = (editedColStr == "") ? [] : editedColStr.split("|");
    editedColStr = maximaRow.editedColumns;
    var editedMaxima = (editedColStr == "") ? [] : editedColStr.split("|");

    var headerItems = [], minimaItems = [], maximaItems = [];
    dataColumns.forEach(
      function(col) {
        var annotation = "";
        if (col==xProp) annotation = "x";
        if (col==yProp) annotation = annotation+"y";
        headerItems.push([{datacolumn: col, annotation: annotation}]);
        minimaItems.push([{datacolumn: col, value: minimaRow[col], dragx: "tablecell,workingDataRanges," + col + ",r_default_rangeControls", datarole: 1}]);
        maximaItems.push([{datacolumn: col, value: maximaRow[col], dragx: "tablecell,workingDataRanges," + col + ",r_default_rangeControls", datarole: 2}]);
      });
    Shiny.propLatest = { x: xProp, y: yProp };
    
    var tableRoot = d3.select(table_element);
    // Iff the table hasn't been built yet, build it now.
    // one outer table for the whole data collection
    if (tableRoot.select("table.outerTable").empty()) {
        var outerTable = d3.select(table_element)
        .append("table").attr("class", "outerTable").attr("width", width).attr("style", "border-spacing:0; padding:0");
        // make a row in the outerTable for the header table
        var headerTable = outerTable
        .append("tr")
        .append("td").attr("style", "padding:0")
        .append("table").attr("class", "headerTable").attr("width", twidth).attr("style", "table-layout:fixed; font-size:14px").data([null]);
        // one row in the header table for the column names
        headerTable
        .append("tr").attr("class", "headerXY").attr("style", "font-size:13px; cursor:default");
        headerTable
        .append("tr").attr("class", "headerNames").attr("style", "cursor:default");
        headerTable
        .append("tr").attr("class", "minima").attr("style", "font-size:12px; cursor:ew-resize").attr("align", "center");
        headerTable
        .append("tr").attr("class", "maxima").attr("style", "font-size:12px; cursor:ew-resize").attr("align", "center");

        // and then another row in the outerTable to hold a scrolling div that will then hold the main data table itself
        outerTable
        .append("tr")
        .append("td").attr("style", "padding:0")
        // the cell holds a scrolling div
        .append("div").attr("class", "scrollingTableDiv").attr("width", width).attr("style", "height:" + dataTHeight + "; overflow:auto;")
        // and the div holds a table
        .append("table").attr("border", 1).attr("width", twidth).attr("height", dataTHeight).attr("style", "table-layout:fixed; border-collapse:collapse; font-size:12px")  // fixed layout because otherwise we can't figure out the widths for the header row (which is also a table)
        .append("tbody").attr("class", "mainTable");
    }  // end of table building
    
    // now load the headerNames row with the column-name items, with some behaviour
    var switchCols = function(newXCol, newYCol) {
        // either argument can be null, meaning "leave as is".
        // check whether values are same as those most recently used
        var latest = Shiny.propLatest;
        var newX = newXCol || latest["x"];
        var newY = newYCol || latest["y"];
        if (newX == latest["x"] && newY == latest["y"]) return;
        
        Shiny.propHistory = latest;
        
        $world.setHandStyle("wait");
        Shiny.historyManager().addParameterRangeItem(Shiny.chartNamed("plot1"), "newXYProps", "axes", [ { x: newX, y: newY } ], null);
        Shiny.buildAndSendMessage("command", [ "newXYProps", { x: newX, y: newY } ]);
    }
    var revertLastChange = function() {
        // revert to the columns stored in history, if any.
        switchCols(Shiny.propHistory["x"], Shiny.propHistory["y"]);
    }
    var swapXY = function() {
        switchCols(Shiny.propLatest["y"], Shiny.propLatest["x"]);
    }
    var highlightHeader = function(elem) {
        var jsTH = $(elem)[0];
        jsTH.focus();
        jsTH.style.backgroundColor="lightgray";
        // or supposedly sthg like d3.select(this).style("backgroundColor", "gray")?
    }
    var headerNames = tableRoot.select("tr.headerNames").selectAll("th").data(headerItems);
    headerNames.enter()
    .append("th")
    .attr("tabindex",-1)       // allow the element to get focus
    //.attr("style", "cursor: auto")
    .on("mouseenter", function(d) {
        var jsTH = $(this)[0];
        var col = d[0].datacolumn;
        if (d3.event.altKey) switchCols(col, null);         // ALT switches x
        else if (d3.event.shiftKey) switchCols(null, col);  // SHIFT switches y
        })
    .on("mouseover", function(d) {
        highlightHeader(this);
        })
    .on("mouseout", function(d) {
        $(this)[0].style.backgroundColor=null;
        })
    .on("dblclick", swapXY)
    .on("keydown", function(d) {
        // console.log(d3.event.getKeyChar());
        if (d3.event.getKeyChar() == "X") switchCols(d[0].datacolumn, null);
        else if (d3.event.getKeyChar() == "Y") switchCols(null, d[0].datacolumn);
        else if (d3.event.getKeyCode() == Event.KEY_SPACEBAR) revertLastChange();
        d3.event.stopPropagation();
        d3.event.preventDefault();
        });
    
    headerNames
    .text(function (d) { return d[0].datacolumn; })
    
    // and the annotations row with x and y indicators
    var annotationCells = tableRoot.select("tr.headerXY").selectAll("th").data(headerItems);
    annotationCells.enter().append("th");
    annotationCells.text(function (d) { return d[0].annotation });
    
    // minima and maxima rows
    var minimaCells = tableRoot.select("tr.minima").selectAll("td").data(minimaItems);
    minimaCells.enter().append("td");
    minimaCells.text(function (d) { return d[0].value })
    .attr("style", function(d) {
          return "color: " + (editedMinima.indexOf(d[0].datacolumn) == -1 || d[0].value == "0" ? "black" : "red") 
    });
    
    var maximaCells = tableRoot.select("tr.maxima").selectAll("td").data(maximaItems);
    maximaCells.enter().append("td");
    maximaCells.text(function (d) { return d[0].value })
    .attr("style", function(d) {
          return "color: " + (editedMaxima.indexOf(d[0].datacolumn) == -1 || d[0].value == "100"  ? "black" : "red") 
    });
    
    // then an inner-table row for each element of the data
    var rows = tableRoot.select("tbody.mainTable").selectAll("tr").data(rowItems);
    rows.enter()
    .append("tr"); //.sort(sortValueDescending);
    
    rows
    .each(function(d) { d._svg = this });
    
    // a cell for each column of the row
    // NB: we assume each row is an item with a field corresponding to each column name
    // We also take advantage of the fact that Vega's svgHandler will look at a DOM element's
    // __data__ property and expect it to be either an item (with a mark property) or an
    // array (supposedly of items) of which it returns the first entry.

    //dataColumns.splice( $.inArray("tableTextColour", dataColumns), 1 );
    var cells = rows.selectAll("td")
        .data(function (d, i, j) {
            // d is the data object (with items mpg, wt etc) for a single row.
            // We return a collection of objects, one to be stored in the __data__ of each td.
            return dataColumns.map(function (column) { return [d, column] });  // we'll look up the value later
            });
    
    cells.enter()
    .append("td");
    
    /*  Any benefit to giving the cells values, rather than just deriving them every time?
     cells
     .datum(function(d, i, j) {
     var column = d[1];
     var rowItem = rowItems[j];
     return [rowItem, rowItem.datum.data[column], j, column]
     });
     */
    // show all the values.  This is run for all cells every time any row is updated.  Could we
    // restrict to just the changing rows?
    cells
    .text(function (d, i, j) {
          var column = d[1];
          var rowItem = rowItems[j];
          return String(rowItem.datum.data[column]);
          })
/*
    .attr("style", function(d, i, j) {
          var editedColStr = rowItems[j].datum.data["editedColumns"];
          var color = "black";
          if (editedColStr != "" && editedColStr.split("|").indexOf(d[1]) != -1) color = "red";
          var filtered = rowItems[j].datum.data["filtering"] != 0;
          return "opacity: "+ (filtered ? "0.3" : "1") + "; color: " + color;
          })
*/

}   // end of drawTable

(function() {
    // hijack bounds.mark() and bounds.item()
    vg.scene.bounds.oldMark = vg.scene.bounds.mark;
    vg.scene.bounds.mark = function(mark, bounds, opt) {
        if (mark.marktype == "lively_table") {
            var b = new vg.Bounds();
            b.set(0, 0, mark.group.width, mark.group.height);
            mark.bounds = b;
        } else { vg.scene.bounds.oldMark(mark, bounds, opt) }
    }
    vg.scene.bounds.oldItem = vg.scene.bounds.item;
    vg.scene.bounds.item = function(item, func, opt) {
        if (item.mark.marktype != "lively_table") {     // just ignore for l_t items
            vg.scene.bounds.oldItem(item, func, opt);
        }
    }
    
    // vg.svg.marks.nested["lively_table"] = true;  only used in vg.svg.Renderer.prototype.renderItems
    
    vg.svg.marks.update["lively_table"] = function(o) {
        // a single item within a lively_table mark is being (re)rendered
        //debugger;
        // not quite sure how changes to the style are applied.  seems to work.
        var bg = o.backgroundColor;
        if (bg && bg != "none") {
            var origC = Global.apps.ColorParser.getColorFromString(bg);
            var newCString = origC.withA(0.5).toRGBAString();
            this.style.setProperty("background-color", newCString, null);
        } else {
            this.style.removeProperty("background-color");
        }
    };
    
    vg.svg.marks.draw["lively_table"] =
        vg.svg.marks.draw["draw"]("g",
            function(items) {
              // we use a g as the container because (a) using foreignObject seems to cause the
              // search (for  g > foreignObject) to fail, leading to repeated appends of the
              // same element type; and (b) it's innocuous.
              // it seems that Vega doesn't normally create DOM elements in draw functions.
              // this is why we take care to reuse any elements we do create - in this case, a
              // container for an html table.
              
              // debugger;
              
              var mark = items[0].mark;
              var w = mark.bounds.width();
              var h = mark.bounds.height();
              if ($(this).find(".tablecontainer").length == 0) {
                // console.log("appending");
                var foreign = d3.select(this).append("foreignObject").attr("x",40).attr("y",30).attr("width", w).attr("height", h);
                foreign.append("xhtml:body").attr("class","tablecontainer").attr("style", "margin:0; padding:0");
              }
              // console.log("table:", mark);
              
              // extract any table metadata that the sender has encoded in items with special
              // values for "originalrow":
              //   "meta:xy" encodes the x and y column annotations (1=x, 2=y, 3=both)
              //   "meta:minima" and "meta:maxima" contain the min and max for the columns; edited range settings are encoded in the "editedColumns" column.
              // we assume metadata rows will be at the start of the data
              var row = 0;
              var xProp, yProp, minimaRow, maximaRow;
              var rowData;
              while ((rowData = items[row].datum.data).originalrow.indexOf("meta:") != -1) {
                if (rowData.originalrow == "meta:xy") {
                  Object.keys(rowData).forEach(function(col) {
                    if (col != "originalrow" && col != "tableTextColour") {
                      if (rowData[col] & 1) xProp = col;
                      if (rowData[col] & 2) yProp = col;
                    }
                  });
                } else if (rowData.originalrow == "meta:minima") {
                  minimaRow = rowData
                } else if (rowData.originalrow == "meta:maxima") {
                  maximaRow = rowData
                }
                row++;
              }
              
              // for the true table data rows, use values from the "originalrow" column for a "datarows" annotation in the format expected by the brushing code in shiny-ggvis
              var rowItems = items.slice(row);      
              for (var i=0; i<rowItems.length; i++) { rowItems[i]["datarows"] = "[" + rowItems[i].datum.data.originalrow + "]" };
    
              var container = $(this).find(".tablecontainer")[0];
              drawTable(rowItems, xProp, yProp, minimaRow, maximaRow, container, { width: w-40, height: h-40 })
              },
              true);  // nest the data items

    // custom symbol shapes for the axis range and limit indicators
    var sqrt3 = Math.sqrt(3);
    vg.svg.marks.customSymbols =
        d3.map({
            'range-pointer-right': function(size) {
            var ry = Math.sqrt(size / sqrt3), rx = ry * sqrt3;
            return "M0,0" + "L" + -rx + "," + ry + " " + -rx + "," + -ry + "Z";
            },
            'limit-pointer-right': function(size) {
            var ry = Math.sqrt(size / sqrt3), rx = ry * sqrt3, halfT = 1;
            return "M0,0" + "L" + -rx + "," + ry + " " + -rx + "," + -ry + "Z" + "M" + halfT + "," + ry + "L" + halfT + "," + -ry + " " + -halfT + "," + -ry + " " + -halfT + "," + ry + "Z";
            },
            'range-pointer-left': function(size) {
            var ry = Math.sqrt(size / sqrt3), rx = ry * sqrt3;
            return "M0,0" + "L" + rx + "," + ry + " " + rx + "," + -ry + "Z";
            },
            'limit-pointer-left': function(size) {
            var ry = Math.sqrt(size / sqrt3), rx = ry * sqrt3, halfT = 1;
            return "M0,0" + "L" + rx + "," + ry + " " + rx + "," + -ry + "Z" + "M" + halfT + "," + ry + "L" + halfT + "," + -ry + " " + -halfT + "," + -ry + " " + -halfT + "," + ry + "Z";
            },
            'range-pointer-up': function(size) {
            var rx = Math.sqrt(size / sqrt3), ry = rx * sqrt3;
            return "M0,0" + "L" + -rx + "," + ry + " " + rx + "," + ry + "Z";
            },
            'limit-pointer-up': function(size) {
            var rx = Math.sqrt(size / sqrt3), ry = rx * sqrt3, halfT = 1;
            return "M0,0" + "L" + -rx + "," + ry + " " + rx + "," + ry + "Z" + "M" + rx + "," + halfT + "L" + -rx + "," + halfT + " " + -rx + "," + -halfT + " " + rx + "," + -halfT + "Z";
            },
            'range-pointer-down': function(size) {
            var rx = Math.sqrt(size / sqrt3), ry = -rx * sqrt3;
            return "M0,0" + "L" + -rx + "," + ry + " " + rx + "," + ry + "Z";
            },
            'limit-pointer-down': function(size) {
            var rx = Math.sqrt(size / sqrt3), ry = -rx * sqrt3, halfT = 1;
            return "M0,0" + "L" + -rx + "," + ry + " " + rx + "," + ry + "Z" + "M" + rx + "," + halfT + "L" + -rx + "," + halfT + " " + -rx + "," + -halfT + " " + rx + "," + -halfT + "Z";
            }
            });
    vg.svg.marks.customSymbolTypes = vg.svg.marks.customSymbols.keys();
    
    // replicate d3.svg.symbol, so our custom symbols behave just like the d3 ones
    vg.svg.marks.customSymbol = function() {
        function d3_functor(v) {
            return typeof v === "function" ? v : function() {
                return v;
            };
        }
        var type = function() { return "" }, size = function() { return 64 };  // note: Vega sets own default size of 100 
        function symbol(d, i) {
            return (vg.svg.marks.customSymbols.get(type.call(this, d, i)))(size.call(this, d, i));
        }
        symbol.type = function(x) {
            if (!arguments.length) return type;
            type = d3_functor(x);
            return symbol;
        };
        symbol.size = function(x) {
            if (!arguments.length) return size;
            size = d3_functor(x);
            return symbol;
        };
        return symbol;
    }
})();

/* some left-over bits
 this.setAttribute('style','fill:green'); // see http://stackoverflow.com/questions/5069006/change-attributes-defined-in-defs-on-use-element
 this.style.setProperty("font", vg.config.render.fontSize+"px " + vg.config.render.font);
 */
