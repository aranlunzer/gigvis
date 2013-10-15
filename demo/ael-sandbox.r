carsWorking <- mtcars
gvReactives <- reactiveValues()
r_cw <- reactive({carsWorking})
gv <- reactive({ggvis(
  r_cw,
  props(x~mpg, y~wt),
  mark_symbol(props(fill="green"))
)
})
view_lively(list(gv))

# mtcars data, scatter plot + histogram/freqpoly (same scale) + binoffset samples + smooth

library(shiny)

ggvis(
  mtcars,
  props(x~wt, y~mpg),
  mark_symbol()
  )

ggvis(
  mtcars,
  props(x~wt, y~mpg),
  mark_symbol(),
  node(
    data = transform_bin(binwidth=0.2),
    mark_line(props(x ~ xmin__, y ~ count__, interpolate="step-after"))
  ))

node(mark_line(props(x ~ xmin__, y ~ count__, interpolate="step-after", 
strokeOpacity=0.4)), pipeline(data=carsWorking, transform_bin(binwidth=gvParms$binwidth, origin=gvParms$binwidth*gvParms$binoffset)))',

library(ggvis)
library(shiny)
ggvis(
  reactive({mtcars}),
  props(x~wt, y~mpg),
  mark_symbol()
  )


gv <- reactive({ do.call("ggvis", values$argList()) })
view_lively(gv, customObserver = observer)
