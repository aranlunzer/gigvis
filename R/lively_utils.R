debugLog <- function(message) { if (lg_debug) write(paste0(print(Sys.time()),": ", message), file="lively_r_log", append=TRUE) }

printReactLog <- function() {
  message(RJSONIO::toJSON(shiny:::.graphEnv$log, pretty=TRUE))
  resetReactLog()
}

resetReactLog <- function() {
  assign("log", list(), envir=shiny:::.graphEnv)
}

# from Hadley
calcmem <- function() { 
  bit <- 8L * .Machine$sizeof.pointer 
  if (bit != 32L && bit != 64L) { 
    stop("Unknown architecture", call. = FALSE) 
  } 
  
  node_size <- if (bit == 32L) 28L else 56L 
  
  usage <- gc() 
  total <- sum(usage[, 1] * c(node_size, 8)) / (1024 ^ 2)
  paste0(round(total),"MB")
}

.ls.objects <- function (pos = 1, pattern, order.by,
                          decreasing=FALSE, head=FALSE, n=5) {
  napply <- function(names, fn) sapply(names, function(x)
    fn(get(x, pos = pos)))
  names <- ls(pos = pos, pattern = pattern)
  obj.class <- napply(names, function(x) as.character(class(x))[1])
  obj.mode <- napply(names, mode)
  obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
  obj.size <- napply(names, object.size)
  obj.dim <- t(napply(names, function(x)
    as.numeric(dim(x))[1:2]))
  vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
  obj.dim[vec, 1] <- napply(names, length)[vec]
  out <- data.frame(obj.type, obj.size, obj.dim)
  names(out) <- c("Type", "Size", "Rows", "Columns")
  if (!missing(order.by))
    out <- out[order(out[[order.by]], decreasing=decreasing), ]
  if (head)
    out <- head(out, n)
  out
}
# shorthand
lsos <- function(..., n=100) {
  .ls.objects(..., order.by="Size", decreasing=TRUE, head=TRUE, n=n)
}

writeMemoryProfile <- function() {
  gcout <- gc(TRUE)
  pro <- memory.profile()
  cat(sapply(names(pro),function(x) paste(x,paste(pro[[x]],collapse=" "))), file="memory_profile", append=TRUE)
  write("\n", file="memory_profile", append=TRUE)
  write.table(gcout, file="memory_profile", append=TRUE)
  write.table(lsos(pos=livelyR_env), file="memory_profile", append=TRUE)
}

#' Pretending we're modeling using our own lines
#' Instead of using a model to make a line, we create our own arbitrarily and then want to know things about it,
#' such as residuals and R^2

predictOnLine <- function(trialdata, realdata, giveme="vert"){
  slope <- (trialdata$y[1] -trialdata$y[2])/(trialdata$x[1] -trialdata$x[2])
  intercept <- trialdata$y[1] - slope*trialdata$x[1]
  if (giveme=="vert" || (giveme=="perp" && slope==0)){
    y <- slope*realdata$x + intercept
    return(data.frame(x=realdata$x, y=y))
  }else if (giveme=="horiz"){
    x <- (realdata$y-intercept)/slope
    return(data.frame(x=x, y=realdata$y))
  }else if (giveme=="perp"){
    slopePerp <- -(1/slope)
    interceptPerp <- realdata$y - slopePerp*realdata$x
    xAns <- (interceptPerp-intercept)/(slope-slopePerp)
    yAns <- slopePerp*xAns + interceptPerp
    return(data.frame(x=xAns, y=yAns))
    #     v <- c(trialdata$y[2]-trialdata$y[1], -(trialdata$x[2]-trialdata$x[1]))
    #     r <- c(trialdata$x[1]-realdata$x, trialdata$y[1]-realdata$y)
  } else {
    print("specify vert, horiz or perp")
  }
}

rsquared <- function(realdata, trialdata){
  ssTot <- sum((realdata$y-mean(realdata$y))^2)
  ssRes <- sum((realdata$y-predictOnLine(trialdata, realdata)$y)^2)
  return(round(1-ssRes/ssTot, digits=3))
}

resLines <- function(trialdata, realdata, giveme="vert"){
  resdf <- NULL
  resdf$x <- NULL
  resdf$y <- NULL
  predictions <- predictOnLine(trialdata, realdata, giveme)
  for(i in 1:dim(realdata)[1]){
    resdf$x <- c(resdf$x, realdata$x[i], predictions$x[i])
    resdf$y <- c(resdf$y, realdata$y[i], predictions$y[i])
  }
  return(data.frame(resdf))
}

# ael
resLinesInChartDomain <- function(trialdata, realdata, giveme="vert") {
  if (is.null(trialdata) || is.null(realdata)) return(NULL);
  tempReal <- data.frame(x=realdata$chartx, y=realdata$charty)
  tempTrial <- data.frame(x=trialdata$chartx, y=trialdata$charty)
  res <- setNames(resLines(tempTrial, tempReal, giveme), c("chartx", "charty"))
  res$item <- gl(nrow(realdata),2)
  sluice(by_group(item), props(), res)    # by_group expects a props 
}

lmLine <- function(realdata, xProp, yProp) {
  # provide a line of data$yProp~data$xProp from x=0 to x=max(x)*2
  lmRes <- lm(realdata[[yProp]] ~ realdata[[xProp]])
  slope <- lmRes$coefficients[[2]]
  intercept <- lmRes$coefficients[[1]]
  maxX <- max(realdata[[xProp]])
  data.frame(chartx=c(0, maxX*2), charty=c(intercept, intercept+(slope*maxX*2)))
}

demingLine <- function(realdata, xProp, yProp) {
  # provide a line from x=0 to x=max(x)*2
  demingRes <- Deming(realdata[[xProp]], realdata[[yProp]])
  slope <- demingRes[["Slope"]]
  intercept <- demingRes[["Intercept"]]
  maxX <- max(realdata[[xProp]])
  data.frame(chartx=c(0, maxX*2), charty=c(intercept, intercept+(slope*maxX*2)))
}

extendTrialLine <- function(trialdata, maxX, maxY) {
  # extrapolate to x=0 and x=max*2
  if (trialdata$chartx[1] == trialdata$chartx[2])     # vertical line
    return(data.frame(chartx=c(trialdata$chartx[1], trialdata$chartx[1]), charty=c(0, maxY*2)))
  slope <- (trialdata$charty[1] -trialdata$charty[2])/(trialdata$chartx[1] -trialdata$chartx[2])
  intercept <- trialdata$charty[1] - slope*trialdata$chartx[1]
  yMax <- intercept + (slope * maxX * 2)
  data.frame(chartx=c(0, maxX*2), charty=c(intercept, yMax))  
}

textResiduals <- function(realdata, trialdata){
  ssRes <- sum((realdata$y-predictOnLine(trialdata, realdata)$y)^2)
  scaled <- ssRes/(mean(realdata$y)^2)/nrow(realdata)
  return(paste0(rep('|', min(round(scaled*200),120)),collapse=""))
}
# experimental hack for testing popup
rsquaredRange <- function(realdata, trialdata) {
  ys <- 0:10
  xs <- vapply(ys, function(step) {
    y <- 10 + (2.5*step)
    trialdata[1,"y"] <- y
    rsquared(realdata, trialdata)
  }, numeric(1))  
  data.frame(x=xs, y=ys)
}

switchedXorY <- function() {
  switched <- 
    (!is.null(gvHistory$xProp) && (gvHistory$xProp != isolate(gvParms$xProp))) ||
    (!is.null(gvHistory$yProp) && (gvHistory$yProp != isolate(gvParms$yProp)))
  debugLog(paste0("checking switched X or Y: ", switched))
  switched
}
transitionDataFrame <- function(data, prevXProp, prevYProp, newXProp, newYProp) {
  # return a data frame with columns "initialx", "initialy" in which values in the 
  # prevXProp column of the supplied dataset are scaled so they will appear at the same
  # relative positions on the newXProp scale; ditto for prevYProp.
  # if prevXProp is NULL (no history), or prev == new for both x and y, don't create
  # the "initial" columns.
  # If newXProp or newYProp is an integer (used to initialise the chart), replace with
  # the corresponding column name.
  df <- data
  if (is.numeric(newXProp)) {
    newXProp <- names(df)[[newXProp]]
    gvParms$xProp <- newXProp
  }
  if (is.numeric(newYProp)) {
    newYProp <- names(df)[[newYProp]]
    gvParms$yProp <- newYProp
  }
  if (switchedXorY()) {
    prevX <- prevXProp
    prevY <- prevYProp
    if (is.null(prevX)) prevX <- newXProp
    if (is.null(prevY)) prevY <- newYProp
    xFactor <- max(data[[newXProp]])/max(data[[prevX]])
    yFactor <- max(data[[newYProp]])/max(data[[prevY]])
    df$initialx <- data[[prevX]]*xFactor
    df$initialy <- data[[prevY]]*yFactor
  } else {
    df$initialx <- rep(-1, nrow(data))
    df$initialy <- df$initialx
  }
  df$chartx <- data[[newXProp]]
  df$charty <- data[[newYProp]]
  df
}

trackReactivesDuring <- function(func) {
  assign("reactives_created", list(), envir=globalenv())
  func()
  rs <- reactives_created
  rm("reactives_created", envir=globalenv())
  rs
}

registerChartObserver <- function(id, obs) {
  all_chart_observers[[id]] <- obs
}

discardChartReactives <- function(id) {
  reactives <- all_chart_reactives[[id]]
  if (!is.null(reactives)) {
    debugLog(paste0("discarding ", as.character(length(reactives)), " reactives for ", id))
    lapply(reactives, function(r) attr(r, "observable")$discard())
    all_chart_reactives[[id]] <<- NULL
  }
}

discardChartObservers <- function(id) {
  observers <- all_chart_observers[[id]]
  if (!is.null(observers)) {
    debugLog(paste0("discarding ", as.character(length(observers)), " observers for ", id))
    lapply(observers, function(o) o$discard())
    all_chart_observers[[id]] <<- NULL
  }
}

# detach the observers and other reactives registered for the specified chart
refreshChart <- function(id) {
  discardChartObservers(id)
  discardChartReactives(id)
  gvStatics[[paste0("refresh", id)]] <<- TRUE
  debugLog(paste0("setting ", paste0("refresh", id)))
}

# this entry point currently not used
refreshCharts <- function() {
  if (length(all_chart_observers) > 0) {
    for (id in ls(all_chart_observers, all.names=TRUE)) refreshChart(id)
    #    writeMemoryProfile()        #debug
  }
}

triggerRefresh <- function(scope) {
  scopeVar <- paste0("refresh", scope)
  prev <- isolate(gvReactives[[scopeVar]])
  debugLog(paste0(scope, " <- ", print(prev+1)))
  gvReactives[[scopeVar]] <- prev + 1
}

