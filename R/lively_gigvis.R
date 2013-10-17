# A collection of methods specific to our use of ggvis from Lively

# a ggvis(...) is basically a ggvis_node(...) - a structure with classes "ggvis", "ggvis_node"

lg_debug = TRUE   # a bunch of logToFile() reports

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
  r_data <- reactive( x$data()[setdiff(names(x$data()), c("initialx","initialy"))] )
  data_id <- paste0(x$dataname, "_table")  # , digest(x$data))
  datasets <- as.vega(isolate(r_data()), data_id)
  data_table <- new.env(parent = emptyenv())
  data_table[[data_id]] <- r_data  # users of data_table expect reactives

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
  description$datasource <- x$dataname #digest(isolate(x$data()))  # for figuring out row numbers
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
  
  structure(spec, data_table = data_table)
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
    options(warn = 1)       # make sure warnings are issued immediately

    # create an observer for processing and updating each of the gv specs
    for (s in 1:length(r_gvSpecs)) {
      plot_id <- paste0("plot", toString(s))
      gvChartVersions[[plot_id]] <<- 0
      observe_ggvis_lively(r_gvSpecs[[s]], plot_id, session, renderer)
    }
    
    # (ael) allow supply of custom observer of changes in input and deliverer of output
    if (!is.null(customObserver)) customObserver(input, output, session)
    
    # Stop the app when the quit button is clicked (or some code pretends by tweaking input$quit)
    observe({
      if (is.null(input$quit)) return()
      if (input$quit > 0) stopApp()
    }, label="obs_quit")
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

# ael: this is called once per ggvis chart.  Its main argument is a reactive
# that is expected to deliver either a NULL value or a ggvis component list
# as produced by ggvis().
observe_ggvis_lively <- function(r_gv, id, session, renderer = "svg", ...) {
  if (!is.reactive(r_gv)) {
    stop("observe_ggvis requires a reactive expression that returns a ggvis object",
         call. = FALSE)
  }
  
  force(id)
  
  lively_separate_data = TRUE     # but see comments below for what this now means
  
  if (!lively_separate_data) {
    # In this case we're sending the browser complete specs with data included.
    # For ease of chart reconfiguration, we allow the reactive gv definition to
    # deliver a NULL value.
    # Every time r_gv changes we look at it.  If it's non-null we turn it into
    # a static Vega spec and send that to the browser.
    obs <- observe({
      if (!is.null(r_gv())) {
        
        spec <- as.vega(r_gv(), session = session, dynamic = FALSE, ...)

        session$sendCustomMessage("ggvis_lively_vega_spec", list(
          chartId = id,
          spec = spec,
          renderer = renderer,
          dataSeparate = FALSE
        ))
      }
    }, label="obs_whole_spec")
    
    session$onSessionEnded(function() {
      obs$suspend()
    })
    
  } else {
    # Sending spec and data separately.
    # In this case we want to set up separate observers for spec and data, thus
    # allowing rapid changes to data without recompiling the spec.  So we
    # supply a reactive spec to the observe_spec and observe_data functions.
    # If just the data elements (listed in the spec's data_table) are updated,
    # that will be picked up by the observers set up by observe_data.
    # If the spec as a whole is updated, the top-level observers set up
    # by both functions will leap into action, sending browser messages
    # and building new data observers as appropriate.
    
    # In fact we now build the spec with all its data, so first-time loading is
    # fast, and suppress the first data-value updates (which would be redundant).
    r_spec <- reactive({
      if (!is.null(r_gv())) {
        gvChartVersions[[id]] <<- gvChartVersions[[id]] + 1
        if (lg_debug) logToFile(paste0(
          id, " version ", as.character(gvChartVersions[[id]]), " vega spec"))
        spec_struct <- as.vega(r_gv(), session = session, dynamic = FALSE)
        all_rs <- attr(spec_struct, "all_reactives")
        if (lg_debug) logToFile(paste0(
          "all_chart_reactives for ", id, ": ", as.character(length(all_rs))))
        all_chart_reactives[[id]] <<- all_rs
        
        spec_struct
      }
    }, label="react_spec")
    lively_observe_spec(r_spec, id, session, renderer)
    lively_observe_data(r_spec, id, session)
  }
}

# Create an observer for a reactive vega spec
lively_observe_spec <- function(r_spec, id, session, renderer) {
  if (lg_debug) logToFile(paste0(id, " setup lively_observe_spec"))
  obs <- observe({
    if (!is.null(r_spec())) {
      if (lg_debug) logToFile(paste0(id, " non-null lively_observe_spec"))
      session$sendCustomMessage("ggvis_lively_vega_spec", list(
        chartId = id,
        version = gvChartVersions[[id]],
        spec = r_spec(),
        renderer = renderer,
        dataSeparate = FALSE
        # timings = timeTracker      disabled
      ))
    }
  }, label="obs_spec")
  session$onSessionEnded(function() {
    obs$suspend()
  })
}

# Create observers for the data objects attached to a reactive vega spec
lively_observe_data <- function(r_spec, id, session) {
  # A list for keeping track of each data observer
  # data_observers <- list()  we no longer have to maintain a list between updates
  if (lg_debug) logToFile(paste0(id, " setup lively_observe_data"))
          
  obs_all <- observe({
    # Observing the reactive r_spec.
    
    # If data_observers list is nonempty, that means there are old observers
    # which need to be suspended before we create new ones.
#     for (obs in data_observers) {
#       if (lg_debug) logToFile(paste0(id, " suspending observer"))
#       obs$suspend()
#     }
#     data_observers <<- list()
    data_observers <- list()  # just to gather the observers set up this time through
    
    if (!is.null(r_spec())) {
      if (lg_debug) logToFile(paste0(id, " non-null lively_observe_data"))
      data_table <- attr(r_spec(), "data_table")
      data_names <- ls(data_table, all.names = TRUE)
      
      # Create observers for each of the data objects
      for (name in data_names) {
        # The datasets list contains named objects. The names are synthetic IDs
        # that are present in the vega spec. The values can be a variety of things.
        
        local({
          # Have to do everything in a local so that these variables are not shared
          # between the different iterations
          data_name <- name
          # data_reactive <- get(data_name, data_table)
#message(paste0("installing observer on: ", data_name, " currently ", attr(data_reactive, "observable")$.label)) 
          firstTime <- TRUE
          version <- gvChartVersions[[id]]

          obs <- observe({
            # watch for changes in the appropriate reactive within the data table
            if (lg_debug) logToFile(paste0("observing data: ", data_name))
            data_reactive <- get(data_name, data_table)
            if (lg_debug) logToFile(paste0("get: ", data_name, " for ", as.character(id), " version ", as.character(version))) 
            ok <- TRUE
            tryCatch({
              data_content <- data_reactive()  # make sure we look, whether first time or not
            }, error = function(e) {
              logToFile("caught error")
              ok <<- FALSE
              })
            if (ok && !is.null(data_content)) {  # ael - in Lively an error will return NULL
              if (firstTime) {
                # skip this once
                firstTime <<- FALSE
                if (lg_debug) logToFile("first time") 
              } else {
                # split_df handling is different
                if (!is.split_df(data_content)) {
                  session$sendCustomMessage("ggvis_lively_data", list(
                    chartId = id,
                    version = version,
                    name = data_name,
                    value = as.vega(data_content, data_name)
                  ))
                } else {
                  # for a split_df we need to send two datasets
                  session$sendCustomMessage("ggvis_lively_data", list(
                    chartId = id,
                    version = version,
                    name = paste0(data_name, "_tree"),
                    value = list(list(
                      name = paste0(data_name, "_tree"),
                      format = list(type = "treejson"),
                      values = list(children = lapply(data_content, function(x) list(children = df_to_json(x))))
                    ))
                  ))
                  session$sendCustomMessage("ggvis_lively_data", list(
                    chartId = id,
                    version = version,
                    name = data_name,
                    value = list(list(
                      name = data_name,
                      source = paste0(data_name, "_tree"),
                      transform = list(list(type = "flatten"))
                    ))
                  ))
                }
              }
            }
          }, label="obs_single_data")
          session$onSessionEnded(function() {
            obs$suspend()
          })
          
          # Track this data observer
          data_observers[[length(data_observers) + 1]] <<- obs
        })
      }
    }
    all_chart_observers[[id]] <<- data_observers 
  }, label="obs_all_data")
  session$onSessionEnded(function() {
    obs_all$suspend()
  })
}
