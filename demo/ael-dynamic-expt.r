library(shiny)
#options(shiny.reactlog=T)

# ael dynamic experiment based on car scatter plot

library(shiny)
#options(shiny.reactlog=T)
values <- reactiveValues(trigger = 0, binwidth=0.2, argList = NULL)
carsWorking <- mtcars
observer = function(input) {
  observe({
    if (is.null(input$trigger)) return()
    trig <- input$trigger
    # print(trig)
    if (grepl("in:", trig)) {  # ignore 'out' for now
      index = as.integer(substr(trig, 4, nchar(trig)))
      oldWt = carsWorking$wt[index]
      carsWorking$wt[index] <<- oldWt - 0.25
      values$trigger <- isolate(values$trigger)+1
    }
  })
  observe({
    if (is.null(input$bw)) return()
    w <- round(as.double(input$bw), digits=1)
    if (w>0) values$binwidth <- w
  })
}
carsReactive <- reactive({ values$trigger; carsWorking }, label="carsReactive")
values$argList <- reactive({
  list(
    carsReactive(),
    props(x ~ wt, fill="green"),
    #, provenance = variable(quote(key), scale=FALSE))
    mark_symbol(props(y~mpg, fill="blue")),
    node(
      branch_histogram(binwidth=values$binwidth, origin=1),
      axis("y", orient="right"),
      scale_quantitative("y", domain = c(0,10))
    ),
    scale_quantitative("x", range = "width", zero = TRUE)
  )
})
gv <- reactive({ do.call("gigvis", values$argList()) })
view_lively(gv, customObserver = observer)