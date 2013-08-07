library(gigvis)
library(shiny)

# ael dynamic experiment based on car data

values <- reactiveValues(trigger = "", trigger2 = 0)
carsWorking <- mtcars
observer = function(input) {
  # watch for changes in the client-side variable input$trigger (in theory a text input)
  observe({
    if (is.null(input$trigger)) return()
    trig <- input$trigger
    # print(trig)
    if (grepl("in:", trig)) {  # ignore 'out' for now
      index = as.integer(substr(trig, 4, nchar(trig)))
      oldMpg = carsWorking$mpg[index]
      carsWorking$mpg[index] <<- oldMpg - 1
      values$trigger2 <- isolate(values$trigger2)+1
    }
  })
}
carsReactive <- reactive({ values$trigger2; carsWorking }) # carsWorking[1:nrow(carsWorking),]})
gigvis_extended(
  customObserver = observer,
  renderer = "svg",
  carsReactive,
  props(x ~ wt, y ~ mpg ),
  mark_symbol()
)
         
#
#gigvis(mtc1, props(x ~ wt),
#  branch_histogram(binwidth = 1),
#
#  dynamic = TRUE,
#  renderer = "svg",
#    aelObserver = function(input) {
#      observe({
#        if (is.null(input$trigger)) return()
#        values$trigger <- input$trigger
#      })
#    }
#)
