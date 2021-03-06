# A collection of methods specific to our use of ggvis from Lively

# a ggvis(...) is basically a ggvis_node(...) - a structure with classes "ggvis", "ggvis_node"

# this is the equivalent of mark_xxxx
lively_table <- function(dataReactive, sourceName) {
  structure(
    list(
      type = "lively_table",         # goes all the way through to the vega spec
      data = dataReactive,
      dataname = deparse2(substitute(dataReactive)),
      sourcename = sourceName
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

as.vega.ggvis_table <- function(x, width = 680, height = 420, padding = NULL,
                          session = NULL, dynamic = FALSE, ...) {
  if (is.null(padding)) padding <- padding() # top, right, bottom, left
  # expecting the x argument to be a ggvis_table structure with a single
  # element, which is a lively_table_mark structure.
#   r_data <- reactive(
#     x$data()[setdiff(names(x$data()), c("chartx", "charty", "originalrow"))]
#     )
  r_data <- x$data
  data_id <- x$dataname
  datasets <- as.vega(isolate(r_data()), data_id)   # the table's initial data
  data_table <- new.env(parent = emptyenv())
  data_table[[data_id]] <- r_data  # a table of reactives for tracking updates to the data

  # inline version of a degenerate as.vega.mark
  markprops <- as.vega(props(backgroundColor:="none"))
  markprops$highlight <- as.vega(props(backgroundColor:="orange"))$update
  #markprops$scenarioHighlight <- as.vega(props(fill="green", fillOpacity=0.3, stroke="black"))
  
  froms <- list()
  froms$data <- data_id
  description <- list()
  #   if (!is.null(vegaprops$sharedProvenance)) {
  #     description <- fromJSON(vegaprops$sharedProvenance$value, asText=TRUE)
  #   }
  description$datasource <- x$sourcename # for figuring out row numbers
  #description$annotations <- x$annotations
  
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
  
  structure(spec, data_table=data_table)
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
    options(warn = 1)       # ensure issued immediately (only applies to non-caught warnings)

    #searchpath <- capture.output(search())
    #debugLog(paste0("search in server: ", paste(searchpath, collapse="")))

    # create an observer for processing and updating each of the gv specs
    for (s in 1:length(r_gvSpecs)) {
      plot_id <- paste0("plot", toString(s))
      gvChartVersions[[plot_id]] <<- 0
      observe_ggvis_lively(r_gvSpecs[[s]], plot_id, session, renderer)
    }
    
    # (ael) allow supply of custom observer of changes in input and deliverer of output
    if (!is.null(customObserver)) customObserver(input, output, session)
    
    # Stop the app when input$quit is signalled
    observe({
      if (!is.null(input$quit)) {
        debugLog("stopping Shiny app")
        stopApp()
      }
    }, label="obs_quit")
  }
  environment(server) <- environment()

  # try 3 times to find an available port, somewhere in the range 8120 to 8149
  if (isTRUE(getOption("shiny.localServer"))) port <- 8141
  else {
    for (try in 1:3) {
      port = 8119 + sample(30,1)
      portTest = paste0("lsof -ta -i tcp:", toString(port))
      testResult = tryCatch(system(portTest, intern=TRUE), warning=function(w) {} )
      # a NULL result means no-one's using the port
      if (is.null(testResult)) break
      message(paste0("failed on port ", toString(port)))
      if (try == 3) stop("Can't find a free port")
    }
  }
  # host=NULL to allow connections from outside
  runApp(list(ui = ui, server = server), port=port, host=NULL, launch.browser=FALSE)
}

# ael: this is called once per ggvis chart.  Its main argument is a reactive
# that is expected to deliver either a NULL value or a ggvis component list
# as produced by ggvis().
observe_ggvis_lively <- function(r_gv, id, session, renderer = "svg", ...) {
  if (!is.reactive(r_gv)) {
    stop("observe_ggvis requires a reactive expression that returns a ggvis object",
         call. = FALSE)
  }
  
  force(id)

  # We set up separate observers for spec and data, thus
  # allowing rapid changes to data without recompiling the spec.  So we
  # supply a reactive spec to the observe_spec and observe_data functions.
  # But we build the spec with all its data, so first-time loading is
  # fast, and suppress the first data-value updates (which would be redundant).

  # If just the data elements (listed in the spec's data_table) are updated,
  # that will be picked up by the observers set up by observe_data.
  # If the spec as a whole is updated, the top-level observers set up
  # by both functions will leap into action, sending browser messages
  # and building new data observers as appropriate.
  
  r_spec <- reactive({
    if (!is.null(r_gv())) {
      gvChartVersions[[id]] <<- gvChartVersions[[id]] + 1
      debugLog(paste0(
        id, " version ", as.character(gvChartVersions[[id]]), " vega spec"))
      spec_struct <- NULL
      # a place to profile the heaviest part of any chart re-generation
      #if (id == "plot1") Rprof("r_profile", memory.profiling=FALSE, interval=0.002)
      #if (id == "plot1") Rprof("r_profile", memory.profiling=FALSE, line.profiling=TRUE, interval=0.002)
      all_rs <- trackReactivesDuring(function() {
        spec_struct <<- as.vega(r_gv(), session = session, dynamic = FALSE)
      })
      #if (id == "plot1") Rprof(NULL)
      #all_rs <- attr(spec_struct, "all_reactives")
      debugLog(paste0(
        "all_chart_reactives for ", id, ": ", as.character(length(all_rs))))
      all_chart_reactives[[id]] <<- all_rs
      
      spec_struct
    }
  }, label="react_spec")
  lively_observe_spec(r_spec, id, session, renderer)
  lively_observe_data(r_spec, id, session)
}

# Create an observer for a reactive vega spec
lively_observe_spec <- function(r_spec, id, session, renderer) {
  debugLog(paste0(id, " setup lively_observe_spec"))
  obs <- observe({
    if (!is.null(spec <- r_spec())) {
      debugLog(paste0(id, " non-null lively_observe_spec"))
      session$sendCustomMessage("ggvis_lively_vega_spec", list(
        chartId = id,
        version = gvChartVersions[[id]],
        spec = spec,
        renderer = renderer
        # timings = timeTracker      disabled
      ))
    }
  }, label="obs_spec")
#   session$onSessionEnded(function() {
#     obs$suspend()
#   })
}

# Create observers for the data objects attached to a reactive vega spec
lively_observe_data <- function(r_spec, id, session) {
  # A list for keeping track of each data observer
  # data_observers <- list()  we no longer have to maintain a list between updates
  debugLog(paste0(id, " setup lively_observe_data"))
          
  obs_all <- observe({
    # Observing the reactive r_spec.
    
    # If data_observers list is nonempty, that means there are old observers
    # which need to be suspended before we create new ones.
#     for (obs in data_observers) {
#       debugLog(paste0(id, " suspending observer"))
#       obs$suspend()
#     }
#     data_observers <<- list()
    data_observers <- list()  # just to gather the observers set up this time through
    
    if (!is.null(spec <- r_spec())) {
      debugLog(paste0(id, " non-null lively_observe_data"))
      data_table <- attr(spec, "data_table")
      data_names <- ls(data_table, all.names = TRUE)
      
      # Create observers for each of the data objects
      for (name in data_names) {
        # The datasets list contains named objects. The names are synthetic IDs
        # that are present in the vega spec. The values can be a variety of things.
        
        local({
          # Have to do everything in a local so that these variables are not shared
          # between the different iterations
          data_name <- name
          data_reactive <- get(data_name, data_table)
#debugLog(paste0("installing observer on: ", data_name, " currently ", attr(data_reactive, "observable")$.label)) 
          firstTime <- TRUE
          #version <- gvChartVersions[[id]]

          obs <- observe({
            # watch for changes in the data reactive
            debugLog(paste0("get: ", data_name, " for ", as.character(id))) 
            ok <- TRUE
            tryCatch({
              data_content <- data_reactive()  # make sure we look, whether first time or not
            }, error = function(e) {
              debugLog("caught error")
              ok <<- FALSE
              })
            if (ok && !is.null(data_content)) {  # ael - in Lively an error will return NULL
              if (firstTime) {
                # skip this once
                firstTime <<- FALSE
                debugLog("first time") 
              } else {
                # don't prepare Vega data if we're just iterating through history
                if (!gvStatics$iterating) {
                # split_df handling is different
                if (!is.split_df(data_content)) {
                  sendOrQueueData(list(
                    chartId = id,
                    version = version,
                    name = data_name,
                    value = as.vega(data_content, data_name)
                  ), session)
                } else {
                  # for now we can only send a replacement for a split_df that's always been split -
                  # and is thus already known on the JS side to be a nested data set.  so we 
                  # only need to send a replacement for the "_tree" structure.
                  sendOrQueueData(list(
                    chartId = id,
                    version = version,
                    name = paste0(data_name, "_tree"),
                    value = list(list(
                      name = paste0(data_name, "_tree"),
                      format = list(type = "treejson"),
                      values = list(children = lapply(data_content, function(x) list(children = df_to_json(x))))
                    ))
                  ), session)
                }
              }}
            }
          }, label="obs_single_data")
#           session$onSessionEnded(function() {
#             obs$suspend()
#           })
          
          # Track this data observer
          data_observers[[length(data_observers) + 1]] <<- obs
        })
      }
    }
    all_chart_observers[[id]] <<- data_observers 
  }, label="obs_all_data")
#   session$onSessionEnded(function() {
#     obs_all$suspend()
#   })
}
