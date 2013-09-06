# Adaptation of view_dynamic

#' @importFrom shiny pageWithSidebar headerPanel sidebarPanel uiOutput
#'   mainPanel tags observe runApp stopApp renderUI
view_lively <- function(r_gv, customObserver = NULL, controls = NULL, renderer = "svg") {
  plot_id <- "plot1"

  ui <- 
    mainPanel(
      ggvis_output(plot_id),
      textOutput("measures1")
    )

  server <- function(input, output, session) {
    observe_ggvis_lively(r_gv, plot_id, session, renderer)

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
            # print(cmd)
            eval(parse(text=cmd),envir=gvTopEnv)
          }
          gvReactives$refresh <- isolate(gvReactives$refresh)+1
        }
      }
    })

    # Stop the app when the quit button is clicked
    observe({
      if (is.null(input$quit)) return()
      if (input$quit > 0) stopApp()
    })
  }

  runApp(list(ui = ui, server = server))
}

# ael: this is only called once
observe_ggvis_lively <- function(r_gv, id, session, renderer = "svg", ...) {
  if (!is.reactive(r_gv)) {
    stop("observe_ggvis requires a reactive expression that returns a ggvis object",
         call. = FALSE)
  }
  
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
