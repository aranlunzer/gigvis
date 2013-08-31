
# mtcars data, scatter plot + histogram/freqpoly (same scale) + binoffset samples + smooth

library(shiny)
#options(shiny.reactlog=T)

values <- reactiveValues(trigger = 0, binwidth=0.5, binoffset=0, check1="false", check2="false", argList = NULL)
carsWorking <- mtcars
observer = function(input) {
  observe({
    if (!is.null(input$trigger)) {
      trig <- input$trigger
      if (grepl("in:", trig)) {  # ignore 'out' for now
        index = as.integer(substr(trig, 4, nchar(trig)))
        oldWt = carsWorking$wt[index]
        carsWorking$wt[index] <<- oldWt - 0.25
        values$trigger <- isolate(values$trigger)+1
      }
    }
  })
  
  observe({
    if (!is.null(input$check1)) {
      values$check1 <- input$check1
    }
  })
  
  observe({
    if (!is.null(input$check2)) {
      values$check2 <- input$check2
    }
  })
  
  observe({
    if (!is.null(input$bw)) {
      w <- round(as.double(input$bw), digits=1)
      if (w>0) values$binwidth <- w
    }
  })
  
  observe({
    if (!is.null(input$bo)) {
      values$binoffset <- round(as.double(input$bo), digits=1)
    }
  })
  
}
carsReactive <- reactive({ values$trigger; carsWorking }, label="carsReactive")
values$argList <- reactive({
  ls <- list(
    carsReactive(),
    props(x ~ wt, y~mpg),
    mark_symbol(props(y~mpg, fill="blue")),
    scale_quantitative("x", domain = c(0, 7), range = "width"),
    branch_smooth(props(stroke = "red", strokeWidth=2))
  )
  ind <- length(ls)+1
  if (values$check1 == "false") {
    ls[[ind]] = branch_histogram(props(fill="green", fillOpacity=0.4), binwidth=values$binwidth, origin=values$binwidth*values$binoffset)
  } else {
    if (values$check2 == "false") {
      ls[[ind]] = branch_freqpoly(props(stroke="black", strokeWidth=1.5), binwidth=values$binwidth, origin=values$binoffset)
    } else {
      for (step in seq(9, 0, -1)) {
        if (step==0) {
          colour <- "black"
          sw <- 2.5
        } else {
          colour <- rgb(10,10,10, maxColorValue=20)
          sw <- 1.5
        }
        ls[[ind]] = branch_freqpoly(props(stroke=colour, strokeWidth=sw), binwidth=values$binwidth, origin=values$binwidth*step*0.1)
        ind <- ind + 1
      }
    }
  }
  ls
})
gv <- reactive({ do.call("ggvis", values$argList()) })
view_lively(gv, customObserver = observer)
