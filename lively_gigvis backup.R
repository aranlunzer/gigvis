# A collection of methods specific to our use of ggvis from Lively

# a ggvis(...) is basically a ggvis_node(...) - a structure with classes "ggvis", "ggvis_node"

# this is the equivalent of mark_xxxx
lively_table <- function(data = NULL) {
  structure(
    compact(list(
      type = "lively_table",
      data = as.pipeline(data)
    )),
    class = c("lively_table_mark")    # to determine which as.vega gets used
  )
}

# a lively_table component is one of the children
component_type.lively_table_mark <- function(x) "children"

# a reduced form of ggvis_node (no props, no scales)
ggvis_table <- function(...) {
  args <- unname(list(...))
  types <- vapply(args, component_type, character(1))
  
  components <- split(args, types)
  if (length(components$data) > 0) {
    # Capture names from ...
    names <- dot_names(...)[types == "data"]
    # Convert each component to a pipeline, preserving original names
    pls <- Map(as.pipeline, components$data, name = names)
    # Collapse into single pipeline
    pl <- structure(unlist(pls, recursive = FALSE), class = "pipeline")    
    # Trim any redundant sources
    components$data <- trim_to_source(pl)
  }
    
  structure(components, class = c("ggvis_table", "ggvis_node"))
}

as.vega.ggvis_table <- function(x, width = 600, height = 400, padding = NULL,
                          session = NULL, dynamic = FALSE, ...) {
  if (is.null(padding)) padding <- padding() # though ignored for now

  # nodes <- flatten(x, session = session)
  # inlining of just the useful bits of flatten() - namely, turning the top-level
  # data element into a pipeline, then connecting the children to that pipeline
  x$props <- props(x~x, y~y)
  x$pipeline <- connect(x$data, x$props, NULL, session)
  x$pipeline_id <- pipeline_id(x$data, x$props)

  child <- x$children[[1]]
  child$props <- x$props
  child$pipeline <- x$pipeline
  child$pipeline_id <- x$pipeline_id

  nodes <- list(child)
  datasets <- as.vega(isolate(x$pipeline()), x$pipeline_id)
    
  spec <- list(
    data = datasets,
    marks = lapply(nodes, as.vega),
    width = width,
    height = height,
    padding = 0 # was as.vega(padding), but vega's autopad was causing the mark to be duplicated
  )
  
  structure(spec, data_table = NULL)
}

as.vega.lively_table_mark <- function(def,session=NULL,dynamic=FALSE) {
  markprops <- list()
  markprops$update <- as.vega(props(datarows=prop(quote(singlerowstring), scale=FALSE)))  # might include sharedProvenance
  markprops$highlight <- as.vega(props(fill="red"))
  #markprops$scenarioHighlight <- as.vega(props(fill="green", fillOpacity=0.3, stroke="black"))
  
  froms <- list()
  froms$data <- def$pipeline_id   # standard
  description <- list()
#   if (!is.null(vegaprops$sharedProvenance)) {
#     description <- fromJSON(vegaprops$sharedProvenance$value)
#   }
  description$datasource <- def$pipeline_id  # so it's accessible from the chart
  
  list(
    type = def$type,
    description = description,
    properties = markprops,
    from = froms
  )
}

view_lively <- function(r_gvSpecs, customObserver = NULL, controls = NULL, renderer = "svg") {
  # build an html page - which Lively will never look at, but a browser can view to help debugging
  uiList <- list()
  for (s in 1:length(r_gvSpecs)) {
    # ggvis_output builds a list of tags pulling in all ggvis-related scripts plus a div for the id
    uiList[[s]] <- ggvis_output(paste0("plot", toString(s)))
  }
  uiList[[length(uiList)+1]] <- textOutput("measures1")
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
        if (msg[["message"]] == "set") {
          cmds <- msg[["args"]]
          for (c in cmds) {
            # dataset,column,row,value
            # becomes   dataset[row,"column"]<-value
            cmd <- paste0(c[["dataset"]],"[",c[["row"]],",'",c[["column"]],"']<-",c[["value"]])
            # write(cmd, file="gvActionLog", append=TRUE)
            eval(parse(text=cmd),envir=globalenv())
          }
          gvReactives$refresh <- isolate(gvReactives$refresh)+1
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
    spec <- as.vega(r_gv(), session = session, dynamic = FALSE, ...)
    
    session$sendCustomMessage("gigvis_vega_spec_with_data", list(
      plotId = id,
      spec = spec,
      renderer = renderer
    ))
    
    })
  
  session$onSessionEnded(function() {
    obs$suspend()
  })
}
