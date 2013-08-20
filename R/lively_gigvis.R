# Adaptation of view_dynamic

#' @importFrom shiny pageWithSidebar headerPanel sidebarPanel uiOutput
#'   mainPanel tags observe runApp stopApp renderUI
view_lively <- function(gv, customObserver = NULL, envir = parent.frame(), controls = NULL,
                         renderer = "svg", launch = TRUE) {

  if (!(renderer %in% c("canvas", "svg")))
    stop("renderer must be 'canvas' or 'svg'")

  plot_id <- "plot1"

  # Make our resources available
  ui <- pageWithSidebar(
    headerPanel("Gigvis plot"),
    sidebarPanel(
      uiOutput("gigvis_ui")
    ),
    mainPanel(
      gigvisOutput(plot_id),

      # Add an actionButton that quits the app and closes the browser window
      tags$button(id="quit", type="button", class="btn action-button",
        onclick = "window.close()", "Quit")
    )
  )

  server <- function(input, output, session) {
    # Set up observers for the spec and the data
    observeGigvis_lively(gv, plot_id, session, renderer)

    # User interface elements (in the sidebar)
    output$gigvis_ui <- renderControls(gv)

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
# we now expect the whole gv to be reactive
observeGigvis_lively <- function(gv, id, session, renderer = "svg", ...) {
  obs <- observe({
    spec <- as.vega(gv(), session = session, dynamic = FALSE, ...)

#     session$sendCustomMessage("gigvis_forget_plot", list(plot = id))
#     
#     data_table <- attr(spec, "data_table")
#     for (name in ls(data_table, all.names = TRUE)) {
#       # The datasets list contains named objects. The names are synthetic IDs
#       # that are present in the vega spec. The values can be a variety of things,
#       # see the if/else clauses below.
# 
#       data_name <- name
#         
#       data_reactive <- get(data_name, data_table)
#       data <- data_reactive()
# 
#       session$sendCustomMessage("gigvis_data", list(
#             plot = id,
#             name = data_name,
#             value = as.vega(data, data_name)
#         ))
#       }

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

  # old calls
  # observe_spec_lively(spec, id, session, renderer)
  # observe_data_lively(attr(spec, "data_table"), id, session)

# Create an observer for the vega spec
# NOT USED
observe_spec_lively <- function(spec, id, session, renderer) {
  obs <- observe({
    session$sendCustomMessage("gigvis_vega_spec", list(
      plotId = id,
      spec = spec,
      renderer = renderer
    ))
  })
  session$onSessionEnded(function() {
    obs$suspend()
  })
}

# Create observers for the data objects
# NOT USED
observe_data_lively <- function(data_table, id, session) {
  # Send each of the data objects
  for (name in ls(data_table, all.names = TRUE)) {
    # The datasets list contains named objects. The names are synthetic IDs
    # that are present in the vega spec. The values can be a variety of things,
    # see the if/else clauses below.
    local({
      # Have to do everything in a local so that these variables are not shared
      # between the different iterations
      data_name <- name
      
      obs <- observe({
        data_reactive <- get(data_name, data_table)
        data <- data_reactive()
        
        session$sendCustomMessage("gigvis_data", list(
          plot = id,
          name = data_name,
          value = as.vega(data, data_name)
        ))
      })
      session$onSessionEnded(function() {
        obs$suspend()
      })
    })
  }
}

