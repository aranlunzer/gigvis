# Adaptation of view_dynamic

#' @importFrom shiny pageWithSidebar headerPanel sidebarPanel uiOutput
#'   mainPanel tags observe runApp stopApp renderUI
view_lively <- function(r_gv, customObserver = NULL, envir = parent.frame(), controls = NULL,
                         renderer = "svg", launch = TRUE) {
  if (!(renderer %in% c("canvas", "svg")))
    stop("renderer must be 'canvas' or 'svg'")

  plot_id <- "plot1"

  # Make our resources available
  ui <- pageWithSidebar(
    headerPanel("Ggvis plot"),
    sidebarPanel(
      uiOutput("ggvis_ui")
    ),
    mainPanel(
      ggvis_output(plot_id),

      # Add an actionButton that quits the app and closes the browser window
      tags$button(id="quit", type="button", class="btn action-button",
        onclick = "window.close()", "Quit")
    )
  )

  server <- function(input, output, session) {
    # Set up observers for the spec and the data
    observe_ggvis_lively(r_gv, plot_id, session, renderer)

    # User interface elements (in the sidebar)
    output$ggvis_ui <- renderControls(r_gv)

    # (ael) allow supply of custom observer of changes in input
    if (!is.null(customObserver)) customObserver(input);

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
    # print("tock")
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
