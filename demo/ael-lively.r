library(shiny)
options(shiny.reactlog=T)
values <- reactiveValues(trigger = 0, binwidth=0.2)
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
gv <- reactive({
  gigvis(carsReactive(),
         props(x ~ wt, y ~ mpg),
         mark_symbol(),
         branch_histogram(binwidth=values$binwidth)
         # scales(scale_quantitative("x", range = "width", zero = TRUE))
  )
})
view_lively(gv, customObserver = observer)