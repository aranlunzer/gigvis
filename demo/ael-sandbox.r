library(shiny)
library(ggplot2)

#options(shiny.reactlog=T)

values <- reactiveValues(trigger = 0, binwidth=0.5, binoffset=0, check1="false", check2="false", argList = NULL)
workingData <- diamonds[sample(nrow(diamonds), 1000), ]
observer = function(input) {}
dataReactive <- reactive({ values$trigger; workingData }, label="dataReactive")
values$argList <- reactive({
  ls <- list(
    dataReactive(),
    props(x ~ carat),
    mark_symbol(props(y~price, fill="blue", size=8)),
    scale_quantitative("x", domain = c(0, 3), range = "width"),
    axis("y", scale="y2", orient="right", grid=FALSE),
    scale_quantitative("y2", domain=c(0,200), range="height", clamp=TRUE),
    branch_histogram(props(y = variable(quote(count__), scale="y2"), fill="green", fillOpacity=0.7, strokeWidth=0), binwidth=0.1)
  )
  ls
})
gv <- reactive({ do.call("gigvis", values$argList()) })
view_lively(gv, customObserver = observer)