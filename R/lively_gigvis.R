# A collection of methods specific to our use of ggvis from Lively

# a ggvis(...) is basically a ggvis_node(...) - a structure with classes "ggvis", "ggvis_node"

# this is the equivalent of mark_xxxx
lively_table <- function(data, xProp, yProp) {
  structure(
    list(
      type = "lively_table",         # goes all the way through to the vega spec
      data = data,
      dataname = substitute(data),
      xProp = xProp,
      yProp = yProp
    ),
    class = c("lively_table_mark")    # to determine which as.vega gets used
  )
}

# a lively_table component behaves as one of the children...
# nope - a lively_table component will no longer get asked.
# component_type.lively_table_mark <- function(x) "children"

# as a replacement for ggvis(), a degenerate form of ggvis_node (no props, no scales)
# - eventually define S3 methods for ggvis, where lively_table is one of the types
ggvis_table <- function(tableDef) {
  # expecting the argument to be a lively_table_mark structure.
  # turn it into a structure suitable for as.vega.
  structure(tableDef, class = c("ggvis_table", "ggvis_node"))
}

as.vega.ggvis_table <- function(x, width = 640, height = 420, padding = NULL,
                          session = NULL, dynamic = FALSE, ...) {
  if (is.null(padding)) padding <- padding() # top, right, bottom, left
  # expecting the x argument to be a ggvis_table structure with a single
  # element, which is a lively_table_mark structure.
  data_id <- paste0(x$dataname, "_", digest(x$data))
  datasets <- as.vega(x$data, data_id)
  
  # inline version of a degenerate as.vega.mark
  markprops <- list()
  markprops$update <- as.vega(props(backgroundColor="none"))
  markprops$highlight <- as.vega(props(backgroundColor="red"))
  #markprops$scenarioHighlight <- as.vega(props(fill="green", fillOpacity=0.3, stroke="black"))
  
  froms <- list()
  froms$data <- data_id
  description <- list()
  #   if (!is.null(vegaprops$sharedProvenance)) {
  #     description <- fromJSON(vegaprops$sharedProvenance$value)
  #   }
  description$datasource <- data_id  # so it's accessible from the chart
  description$xProp <- x$xProp
  description$yProp <- x$yProp
  
  mark_vega <- list(
    type = x$type,
    description = description,
    properties = markprops,
    from = froms
  )
  
  spec <- list(
    data = datasets,
    marks = list(mark_vega),
    width = width,
    height = height,
    padding = as.vega(padding)
  )
  
  structure(spec, data_table = NULL)
}

view_lively <- function(r_gvSpecs, customObserver = NULL, controls = NULL, renderer = "svg") {
  # build an html page - which Lively will never look at, but a browser can view to help debugging
  uiList <- list()
  uiList[[1]] <- textOutput("measures1")
  for (s in 1:length(r_gvSpecs)) {
    # ggvis_output builds a list of tags pulling in all ggvis-related scripts plus a div for the id
    uiList[[length(uiList)+1]] <- ggvis_output(paste0("plot", toString(s)))
  }
  ui <- do.call("mainPanel", uiList)

  server <- function(input, output, session) {
    options(warn = 1)

    # create an observer for processing and updating each of the gv specs
    for (s in 1:length(r_gvSpecs)) {
      plot_id <- paste0("plot", toString(s))
      observe_ggvis_lively(r_gvSpecs[[s]], plot_id, session, renderer)
    }
    
    # (ael) allow supply of custom observer of changes in input and output
    if (!is.null(customObserver)) customObserver(input, output)

    output$measures1 <- renderText({gvReactives$measures})
    
    triggerWatcher <- observe({
      if (!is.null(input$trigger)) {
        msg <- fromJSON(input$trigger);
        #print(msg)
        if (msg[["message"]] == "editData") {
          cmds <- msg[["args"]]
          for (c in cmds) {
            # dataset,column,row,value
            # becomes   dataset[row,"column"]<-value
            cmd <- paste0(c[["dataset"]],"[",c[["row"]],",'",c[["column"]],"']<-",c[["value"]])
            # write(cmd, file="gvActionLog", append=TRUE)
            eval(parse(text=cmd),envir=globalenv())
          }
          # hack
          if (cmds[[1]][["dataset"]] == "trialLine") { triggerRefresh("Chart")
          } else { triggerRefresh("Views") }      # force refresh of all views
        }
      }
    })

    session$onSessionEnded(function() {
      triggerWatcher$suspend()
    })
    
    # Stop the app when the quit button is clicked (or some code pretends by tweaking input$quit)
    observe({
      if (is.null(input$quit)) return()
      if (input$quit > 0) stopApp()
    })
  }

  # try 3 times to find an available port, somewhere in the range 8120 to 8149
  for (try in 1:3) {
    port = 8119 + sample(30,1)
    portTest = paste0("lsof -ta -i tcp:", toString(port))
    testResult = tryCatch(system(portTest, intern=TRUE), warning=function(w) {} )
    # a NULL result means no-one's using the port
    if (is.null(testResult)) break
    message(paste0("failed on port ", toString(port)))
    if (try == 3) stop("Can't find a free port")
  }
  
  runApp(list(ui = ui, server = server), port=port, launch.browser=FALSE)
}

# ael: this needs to be called once per ggvis chart
observe_ggvis_lively <- function(r_gv, id, session, renderer = "svg", ...) {
  if (!is.reactive(r_gv)) {
    stop("observe_ggvis requires a reactive expression that returns a ggvis object",
         call. = FALSE)
  }
  
  force(id)
  
  obs <- observe({
    if (!is.null(r_gv())) {
      spec <- as.vega(r_gv(), session = session, dynamic = FALSE, ...)
    
      session$sendCustomMessage("gigvis_vega_spec_with_data", list(
        plotId = id,
        spec = spec,
        renderer = renderer
      ))
    }
  })
  
  session$onSessionEnded(function() {
    obs$suspend()
  })
}
