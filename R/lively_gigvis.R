# Adaptation of view_dynamic

#' @importFrom shiny pageWithSidebar headerPanel sidebarPanel uiOutput
#'   mainPanel tags observe runApp stopApp renderUI
view_lively <- function(r_gvSpecs, customObserver = NULL, controls = NULL, renderer = "svg") {
  # build an html page - which Lively will never look at, but a browser can view to help debugging
  uiList <- list()
  for (s in 1:length(r_gvSpecs)) {
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
    
    observe({
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
