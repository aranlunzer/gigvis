# diamonds data (sample 1000), scatterplot + histogram/frequency plot

library(shiny)
library(ggplot2)

#options(shiny.reactlog=T)

values <- reactiveValues(trigger = 0, binwidth=0.05, binoffset=0, check1="false", check2="false", argList = NULL)
workingData <- diamonds[sample(nrow(diamonds), 1000), ]
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
      if (w>0) values$binwidth <- w*0.1
    }
  })
  
  observe({
    if (!is.null(input$bo)) {
      values$binoffset <- round(as.double(input$bo), digits=1)
    }
  })
  
}
# dataReactive <- reactive({ values$trigger; workingData }, label="dataReactive")
values$argList <- reactive({
  ls <- list(
    quote(workingData),
    props(x ~ carat),
    mark_symbol(props(y~price, fill="blue", size=8)),
    dscale("x", "numeric", domain = c(0, 3), range = "width"),
    guide_axis("y"),
    dscale("y", "numeric", domain = c(0, 200), range = "height", name="yhist"),
    guide_axis("y", scale="yhist", orient="right", grid=FALSE)
  )
  ind <- length(ls)+1
  if (values$check1 == "false") {
    ls[[ind]] = branch_histogram(props(y = prop(quote(count__), scale="yhist"), fill="green", fillOpacity=0.7, strokeWidth=0), binwidth=values$binwidth, origin=values$binwidth*values$binoffset)
  } else {
    if (values$check2 == "false") {
      ls[[ind]] = branch_freqpoly(props(y = prop(quote(count__), scale="yhist"), stroke="black", strokeWidth=1.5), binwidth=values$binwidth, origin=values$binwidth*values$binoffset)
    } else {
      for (step in seq(9, 0, -1)) {
        if (step==0) {
          colour <- "black"
          so <- 1
          sw <- 2
        } else {
          colour <- rgb(10,10,10, maxColorValue=24)
          so <- 0.4
          sw <- 1
        }
        ls[[ind]] = branch_freqpoly(props(y = prop(quote(count__), scale="yhist"), stroke=colour, strokeWidth=sw, strokeOpacity=so), binwidth=values$binwidth, origin=values$binwidth*step*0.1)
        ind <- ind + 1
      }
    }
  }
  ls
})
gv <- reactive({ do.call("ggvis", values$argList()) })
view_lively(gv, customObserver = observer)
