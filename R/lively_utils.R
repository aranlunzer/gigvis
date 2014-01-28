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

provenanceValue <- function(...) {
  parms <- list(...)
  if (exists("gvFOO")) {   ## fix this up
    # put the supplied extras on top of all the gvParm values
    parmList <- isolate(reactiveValuesToList(gvParms))
    for (name in names(parms)) parmList[[name]] <- parms[[name]]
    parms <- parmList
  }
  toJSON(parms)
}

# from aam

#' Pretending we're modeling using our own lines
#' Instead of using a model to make a line, we create our own arbitrarily and then want to know things about it,
#' such as residuals and R^2

predictOnLine <- function(trialdata, realdata, giveme="vert"){
  # expects trialdata and realdata to be DFs with columns x, y.
  # returns a DF with columns x, y - or NA if prediction is impossible
  slope <- (trialdata$y[1] -trialdata$y[2])/(trialdata$x[1] -trialdata$x[2])
  intercept <- trialdata$y[1] - slope*trialdata$x[1]
  if (!is.finite(slope) || !is.finite(intercept)) return(NA);

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

rsquared <- function(realdata, xProp, yProp, guessdata){
  # expects realdata to have columns as specified by xProp, yProp.
  # and guessdata to have columns chartx, charty
  realxy = data.frame(x=realdata[[xProp]], y=realdata[[yProp]])
  guessxy = data.frame(x=guessdata$chartx, y=guessdata$charty)
  ssTot <- sum((realxy$y-mean(realxy$y))^2)
  ssRes <- sum((realxy$y-predictOnLine(guessxy, realxy)$y)^2)
  return(round(1-ssRes/ssTot, digits=3))
}


resLines <- function(trialdata, realdata, giveme="vert"){
  resdf <- NULL
  resdf$x <- NULL
  resdf$y <- NULL
  predictions <- predictOnLine(trialdata, realdata, giveme)
  if (identical(predictions, NA)) return(NA)

  for(i in 1:dim(realdata)[1]){
    resdf$x <- c(resdf$x, realdata$x[i], predictions$x[i])
    resdf$y <- c(resdf$y, realdata$y[i], predictions$y[i])
  }
  return(data.frame(resdf))
}

# one way of applying a bunch of edits to a df
transform_edit <- function(data, rows, columns, replacement_values){
  for (i in 1:length(rows)){     # NB: assumes rows is not empty
    data[rows[i], columns[i]] <- replacement_values[i]
  }
  return(data)
}

# ael
dataBins <- function(data, property, binwidth, binoffset, blended=FALSE) {
  # "blended" just means turning off the prominence - of the default-scenario histogram.
  # if there is a histogram sweep, this means making it disappear.  otherwise just lose
  # the outline.
  if (nrow(data)==0) emptyBins()
  else {
    bins <- compute(transform_bin(binwidth=binwidth, origin=binoffset),
            props(x=prop(as.name(property))),
            data)
    bins$keyField = as.character(1:nrow(bins))
    # NB: strokeWidth and fillOpacity are both ignored on the sweep histograms
    bins$strokeWidth = if (blended) 0 else 1
    bins$fillOpacity = if (blended &&
                            (length(isolate(gvSweep$binwidth))>0 || length(isolate(gvSweep$binoffset))>0))
                          0 else 0.3
    bins
  }
}

resLinesInChartDomain <- function(trialdata, realdata, xProp, yProp, giveme="vert") {
  # NB we expect trialdata to have columns chartx, charty - whereas from realdata we have
  # to extract the columns specified as xProp and yProp.
  # the result is returned with chartx, charty columns.
  if (is.null(trialdata) || is.null(realdata)) return(emptySplit);

  tempReal <- data.frame(x=realdata[[xProp]], y=realdata[[yProp]])
  tempTrial <- data.frame(x=trialdata$chartx, y=trialdata$charty)
  res <- resLines(tempTrial, tempReal, giveme)
  if (identical(res, NA)) return(emptySplit)
  
  res <- setNames(res, c("chartx", "charty"))
  res$item <- rep(1:nrow(realdata), each=2)
  split_df(res, quote(item), env=NULL)
}

lmLine <- function(realdata, xProp, yProp) {
  # provide a line of data$yProp~data$xProp from x=0 to x=max(x)*2
  # semi-hack: for now we force this to be a (one-piece) split df, to ease the transition to a sweep
  if (nrow(realdata)<2) return(emptySplit)

  #lmRes <- lm(realdata[[yProp]] ~ realdata[[xProp]])
  lmRes <- lm(realdata[[yProp]] ~ realdata[[xProp]], qr=FALSE)
  slope <- lmRes$coefficients[[2]]
  intercept <- lmRes$coefficients[[1]]
  if (!is.finite(slope) || !is.finite(intercept)) return(emptySplit)  # e.g. if all x values are same
  
  maxX <- max(realdata[[xProp]])
  df <- data.frame(chartx=c(0, maxX*2), charty=c(intercept, intercept+(slope*maxX*2)), stroke=c("red","red"), grouping=c(1,1))
  split_df(df, quote(grouping), env=NULL)
}

demingLine <- function(realdata, xProp, yProp) {
  # provide a line from x=0 to x=max(x)*2
  if (nrow(realdata)<2) return(emptySplit)
  
  demingRes <- Deming(realdata[[xProp]], realdata[[yProp]])
  slope <- demingRes[["Slope"]]
  intercept <- demingRes[["Intercept"]]
  if (!is.finite(slope) || !is.finite(intercept)) return(emptySplit)

  maxX <- max(realdata[[xProp]])
  df <- data.frame(chartx=c(0, maxX*2), charty=c(intercept, intercept+(slope*maxX*2)), stroke=c("red","red"), grouping=c(1,1))
  split_df(df, quote(grouping), env=NULL)
}

emptySmoothLine <- function() {
  customiseAndSplitDF(list(chartx=c(-1000), charty=c(-1000)), list(y_lower__=0, y_upper__=0))
}

emptyBins <- function() {
  df <- compute(transform_bin(binwidth=0.5), props(x=~chartx), emptyData)
  df$strokeWidth <- 0
  df$fillOpacity <- 0
  df
}

smoothLine <- function(realdata, xProp, yProp, n, se) {
  # provide a line from x=0 to x=max(x)*2
  if (nrow(realdata)<3) return(emptySmoothLine())

  # for some reason this slows dramatically when number of data rows is below about 6
  df <- compute(
    transform_smooth(method="loess", n=n, formula=y~x, se=se),
    props(x=prop(as.name(xProp)), y=prop(as.name(yProp))),
    realdata)
  # debugLog(paste(nrow(realdata), time[["elapsed"]], sep=" : "))
  # caller expects chartx and charty in place of x and y
  names(df)[1] <- "chartx"
  names(df)[2] <- "charty"
  df$grouping <- 1
  split_df(df, quote(grouping), env=NULL)
}

guessMList <- function(data, xProp, yProp) {
  if (nrow(data)==0) return(list())
  xRange <- range(data[[xProp]])
  yMean <- mean(data[[yProp]]); 
  list(`1`=list(chartx=xRange[[1]], charty=yMean),
       `2`=list(chartx=xRange[[2]], charty=yMean))
}

guessLine <- function(guessML) {
  # guessLine is just dots, and we only store one for the default (not for sweep),
  # so no need for it to be a split df
  if (identical(guessML, list())) return(emptyData)
  
  data.frame(chartx=c(guessML[["1"]]$chartx, guessML[["2"]]$chartx),
            charty=c(guessML[["1"]]$charty, guessML[["2"]]$charty))
}

extendGuessLine <- function(gl, maxX, maxY) {
  if (identical(gl, emptyData)) return(emptySplit)

  # extrapolate to x=0 and x=max*2
  leftX <- gl[1, "chartx"]
  leftY <- gl[1, "charty"]
  rightX <- gl[2, "chartx"]
  rightY <- gl[2, "charty"]
  if (leftX == rightX)     # vertical line
    df <- data.frame(chartx=c(leftX, leftX), charty=c(0, maxY*2), grouping=1)
  else {
    slope <- (rightY-leftY)/(rightX-leftX)
    intercept <- leftY - slope*leftX
    yMax <- intercept + (slope * maxX * 2)
    df <- data.frame(chartx=c(0, maxX*2), charty=c(intercept, yMax), grouping=1)
  }
  split_df(df, quote(grouping), env=NULL)
}

# version for extracting from MList instead of DF
# if (length(guessMList)==0) return(emptyData)
# leftX=guessMList[["1"]]$chartx
# rightX=guessMList[["2"]]$chartx
# leftY=guessMList[["1"]]$charty
# rightY=guessMList[["2"]]$charty

chartLineSlope <- function(line) {
  # return slope of a line expressed as a two-row df in chartx, charty coords
  (line$charty[1] -line$charty[2])/(line$chartx[1] -line$chartx[2])
}

update_static <- function(name, value, log=FALSE) {
  gvStatics[[name]] <<- value
  debugLog(paste0("updated static: ", name))
  if (log) debugLog(capture.output(value))
}

clearInvalidSource <- function(sourceName) {
  # we hold off updating the reactive invalid_sources until we know it's empty - because
  # otherwise we'll keep waking up observers for no good reason.
  
  # old version:
  #sources=isolate(gvReactives$invalid_sources)
  #gvReactives$invalid_sources <- sources[sources!=sourceName]

  sources <- gvStatics$invalid_sources
  gvStatics$invalid_sources <<- sourcesNow <- sources[sources!=sourceName]
  if (length(sourcesNow) == 0) gvReactives$waiting_for_sources <- FALSE
  
  # debugLog(paste0("remaining invalid sources: ", length(isolate(gvReactives$invalid_sources))))
}

update_defaultReactive <- function(name, value, log=FALSE) {
  # make an update to a reactive in the gvSweep collection, and
  # remove its name from the invalid_sources list (if it was there).
  gvDefault[[name]] <- value
  debugLog(paste0("updated default reactive: ", name))
  if (log) debugLog(capture.output(value))
  clearInvalidSource(paste0("default_", name))
}

update_sweepReactive <- function(name, value, log=FALSE) {
  # make an update to a reactive in the gvSweep collection, and
  # remove its name from the invalid_sources list (if it was there).
  gvSweep[[name]] <- value
  debugLog(paste0("updated sweep reactive: ", name))
  if (log) debugLog(capture.output(value))
  clearInvalidSource(paste0("sweep_", name))
}

resetSweep <- function() {
  setupNewSweep(0)
  # for now, also reset edits and ranges in default
  for (resettable in gvStatics$resettables) gvDefault[[resettable]] <- list()
}

setupNewSweep <- function(numScenarios) {
  update_static("numScenarios", numScenarios)
  update_static("scenarioColours", colorRampPalette(c("blue", "red"))(numScenarios))
  # clear all sweepables
  for (sweepable in gvStatics$sweepables) gvSweep[[sweepable]] <- list()
}

visitScenario <- function(scen) {
  # debugLog(paste0("visit scenario: ", scen))
  for (sweepable in gvStatics$sweepables) {
    sweep <- isolate(gvSweep[[sweepable]])
    if (length(sweep) > 0) {
      newVal <- if (class(sweep[[1]])=="list")
        # do a merge on a reduced sweep list with just the scenario we want
        mergeManipulationLists(isolate(gvDefault[[sweepable]]), sweep[scen])[[1]]
      else sweep[[scen]]
      update_defaultReactive(sweepable, newVal)
    }
  }
}

customiseDF <- function(df, requiredCols) {
  requiredList <- list()
  for (n in names(requiredCols)) requiredList[[n]] <- c(requiredCols[[n]])
  dfList = c(as.list(df), requiredList, list(scenario=0))
  data.frame(dfList)
}

customiseAndSplitDF <- function(df, requiredCols) {
  # build a customised split df (single part), adding the supplied dummy column/value pairs and splitting into two
  df <- customiseDF(df, requiredCols)
  df$grouping=1
  split_df(df, quote(grouping), env=NULL)
}

bindSweepDFs <- function(dfs, colourProperty, requiredCols=list()) {
  # debugLog(capture.output(print(dfs)))
  if (length(dfs)==0) {
    customiseDF(list(chartx=c(-1000), charty=c(-1000)), requiredCols)
  } else {
    colouredDFs <- lapply(1:length(dfs), function(pi) {
      df <- dfs[[pi]]
      colour <- gvStatics$scenarioColours[[pi]]
      df[[colourProperty]] <- colour  # equiv to rep(colour, nrow(df))
      df$scenario <- c(pi)
      df})
    # debugLog(capture.output(print(colouredDFs)))
    #structure(colouredDFs, class = "split_df", variables = NULL)
    do.call("rbind", colouredDFs)
  }
}

bindSweepSplitDFs <- function(dfs, colourProperty, requiredCols=list()) {
  # debugLog(capture.output(print(dfs)))
  if (length(dfs)==0) {
    customiseAndSplitDF(list(chartx=c(-1000), charty=c(-1000)), requiredCols)
  } else {
    pieces <- do.call("c", dfs)
    colouredPieces <- lapply(1:length(pieces), function(pi) {
                              piece <- pieces[[pi]]
                              colour <- gvStatics$scenarioColours[[pi]]
                              piece[[colourProperty]] <- colour  # equiv to rep(colour, nrow(piece))
                              piece$scenario <- pi
                              piece})
    # debugLog(capture.output(print(colouredPieces)))
    structure(colouredPieces, class = "split_df", variables = NULL)
  }
}

bindSweepBinDFs <- function(dfs, colourProperty, requiredCols=list()) {
  if (length(dfs)==0) {
    customiseAndSplitDF(emptyBins(), requiredCols)
  } else {
    pieces <- dfs
    colouredPieces <- lapply(1:length(pieces), function(pi) {
      piece <- pieces[[pi]]
      colour <- gvStatics$scenarioColours[[pi]]
      piece[[colourProperty]] <- colour
      piece$scenario <- pi
      piece})
    #debugLog(capture.output(print(colouredPieces)))
    structure(colouredPieces, class = "split_df", variables = NULL)
  }
}

# when a chart is cleared for refresh, this is called to record the values used last time
recordLatest <- function() {
  # TODO: generalise this
  for (name in c("xProp", "yProp")) update_static(paste0(name, "Latest"), isolate(gvSwitches[[name]]))
}

mergeManipulationLists <- function(default, sweep) {
  # a manipulation list is our way of storing inputs that are driven by a user's
  # direct manipulation of a chart mark.  for example, for edits to two baseData rows:
  # list(
  #  "5"=list(mpg=10, wt=2)
  #  "16"=list(mpg=15, qsec=20)
  # )
  # here we derive merged lists (default, potentially overridden in a scenario) 
  # for all the supplied sweep scenarios.
  # if there are no edits in the sweep scenarios, sweep will be an empty list.  in that
  # case, provide as many copies of the default as there are scenarios... which may be zero.
  if (length(sweep)==0) {
    numScens <- gvStatics$numScenarios
    if (numScens==0) list()
    else lapply(1:numScens, function(x) default)
  } else {
    lapply(sweep, function(scenML) {
      merged <- default
      for (rowName in names(scenML)) {
        scenEdits <- scenML[[rowName]]
        mergedRow <- merged[[rowName]]
        if (is.null(mergedRow))
          mergedRow <- scenEdits
        else {
          for (colName in names(scenEdits)) {
            mergedRow[[colName]] <- scenEdits[[colName]]
          }
        }
        merged[[rowName]] <- mergedRow
      }
      merged
    })
  }
}

apply_dataEdits <- function(base, editML) {
  # apply edits, supplied as a manipulation list, to the base data
  result <- base
  for (rowName in names(editML)) {
    es <- editML[[rowName]]
    for (colName in names(es)) {
      result[as.integer(rowName), colName] <- es[[colName]]
    }
  }
  result
}

merge_dataRanges <- function(default, sweep) {
  # derive merged range expressions (default, potentially overridden in a scenario)
  # for all the sweep scenarios.
  # if there are no range settings in the sweep scenarios, sweep will be an empty list.
  # a range setting is a list with entries   colName=c(low, high)   where low or high
  # (though not both) can be NA.
  # if the default scenario has a range on mpg, say, it will be inherited by every 
  # sweep scenario that doesn't explicitly set its own range for mpg.
  lapply(sweep, function(scen) {
    merged <- default
    for (colName in names(scen)) {
      merged[[colName]] <- scen[[colName]]
    }
  })
}

apply_dataRanges <- function(editedBase, rangeML) {
  # editedBase is the base data after all relevant edits have been applied.  now apply
  # the ranges (if any) to produce a mask defining which rows of the edited base data
  # are included in the subset from which measures will be calculated.
  # return the mask.  a rangeML is of the form:
  # list(
  #  "1"=list(wt=3)				  # row 1 is minima: mpg doesn't have one
  #  "2"=list(mpg=20, wt=4)	# row 2 is maxima
  # )
  # no doubt there are more elegant ways to do this...
  lows <- rangeML[["1"]]
  highs <- rangeML[["2"]]
  if (length(lows)+length(highs)==0) {
    rep(TRUE, nrow(editedBase)) 
  } else {
    columnNames <- unique(c(names(lows), names(highs)))
    columnMasks <- lapply(columnNames, function(col) {
      low <- lows[[col]]
      if (is.null(low)) low <- NA
      high <- highs[[col]]
      if (is.null(high)) high <- NA
      vals <- editedBase[[col]]
      (is.na(low) | vals>=low) & (is.na(high) | vals<=high)   # NB: NULL/is.null won't work here
      # debugLog(paste0(mask, collapse=" "))
    })
    Reduce("&", columnMasks)
  }
}

range_controls <- function(editedBase, rangeML) {
  # provide a four-row dataset with points for indicating, along the axes of the current chart, the  
  # positions of controls for adjusting min and max.
  rangeAndStatus <- function(property) {
    low <- rangeML[["1"]][[property]]    # or NULL (including if rangeML is empty)
    if (is.null(low)) low <- NA
    high <- rangeML[["2"]][[property]]   # or NULL
    if (is.null(high)) high <- NA
    rangeSetting <- c(low, high)
    defaultRange <- range(editedBase[[property]])  # inefficient if dataset is big
    edited <- !is.na(rangeSetting)
    rangeIndicator <- ifelse(edited, rangeSetting, defaultRange)
    list(range=rangeIndicator, edited=edited)
  }
  xProp <- isolate(gvSwitches$xProp)
  yProp <- isolate(gvSwitches$yProp)
  xDF <- rangeAndStatus(xProp)     # low and high, and whether each was edited
  yDF <- rangeAndStatus(yProp)
  df <- NULL
  df$chartx <- c(xDF$range, 0, 0)
  df$charty <- c(0, 0, yDF$range)
  dragx <- paste0("x,workingDataRanges,", xProp, ",r_default_rangeControls")
  dragy <- paste0("y,workingDataRanges,", yProp, ",r_default_rangeControls")
  df$dragx <- c(dragx, dragx, "", "")
  df$dragy <- c("", "", dragy, dragy)
  df$edited <- c(xDF$edited, yDF$edited)
  df$dotShape <- ifelse(df$edited, "diamond", "circle")
  # debugLog(capture.output(print(data.frame(df))))
  data.frame(df)
}

setXYDataStatics <- function(workingData, xProp, yProp) {
  baseData <- gvStatics$baseData
  update_static("maxX", max(baseData[[xProp]]))
  update_static("standardBin", gvStatics$maxX*0.2)
  update_static("maxY", max(baseData[[yProp]]))
  update_static("popupYRange", gvStatics$maxY*1.2)

  xMean <- xLowSD <- xHighSD <- NA
  if (gvSwitches$showXSDLines && nrow(workingData)>1) {
    xCol <- workingData[[xProp]]
    xSD <- sd(xCol)
    xMean <- mean(xCol)
    xLowSD <- xMean - xSD
    xHighSD <- xMean + xSD
  }
  update_static("xMean", xMean)
  update_static("xLowSD", xLowSD)
  update_static("xHighSD", xHighSD)
}

range_lines <- function(rangeControls) {
  # turn the dataset provided by range_controls into a df for showing lines on the 
  # chart for edited min/max values.  we need up to four lines, corresponding to the
  # rangeControls rows  minX, maxX, minY, maxY
  rowsNeedingLines <- which(rangeControls$edited)
  if (length(rowsNeedingLines)>0) {
    topX <- gvStatics$maxX * 1.1
    topY <- gvStatics$maxY * 1.1
    # lines: (minX, 0 to minX, topY) (maxX, 0 to maxX, topY) (0, minY to topX, minY) (0, maxY to topX, maxY)
    minX <- as.numeric(rangeControls[1, "chartx"])
    maxX <- as.numeric(rangeControls[2, "chartx"])
    minY <- as.numeric(rangeControls[3, "charty"])
    maxY <- as.numeric(rangeControls[4, "charty"])
    df <- NULL
    df$chartx <- c(minX, minX, maxX, maxX, 0, topX, 0, topX)
    df$charty <- c(0, topY, 0, topY, minY, minY, maxY, maxY)
    df$strokeDash <- rep(c("2,8", "8,4"), times=2, each=2)
    df$strokeWidth <- rep(c(2, 0.75), times=2, each=2)
    df$grouping <- rep(1:4, each=2)
    # the rows of this expanded we want are (2r-1, 2r) for every r in rowsNeedingLines
    rowBools <- rep(rangeControls$edited, each=2)
    df <- data.frame(df)
    df <- df[which(rowBools),]
    split_df(df, quote(grouping), env=NULL)
  }
  else customiseAndSplitDF(list(chartx=c(-1000,-1001), charty=c(-1000,-1001)), list(strokeDash="4,4", strokeWidth=1))
}

xSDLines <- function() {
  # produce two lines for 1 SD each side of the x mean
  if (is.na(gvStatics$xMean)) customiseAndSplitDF(list(chartx=c(-1000,-1001), charty=c(-1000,-1001)), list(strokeDash="4,4", strokeWidth=1))
  else {
    topY <- gvStatics$maxY * 1.1
    df <- NULL
    df$chartx <- c(gvStatics$xLowSD, gvStatics$xLowSD, gvStatics$xHighSD, gvStatics$xHighSD)
    df$charty <- c(0, topY, 0, topY)
    df$grouping <- c(1, 1, 2, 2)
    df$strokeDash <- rep("2,8", 4)
    split_df(data.frame(df), quote(grouping), env=NULL)
  }
}

scatterPlotWithSweep <- function() {
  # build a df for a scatter plot, potentially in the presence of a sweep on either
  # the data edits or the ROIs (but not both)
  fullSize <- 80
  reducedSize <- 60
  minimalSize <- 40
  fullOpacity <- 1.0
  reducedOpacity <- 0.4
  zeroOpacity <- 0.0
  fullColour <- "black"
  paleColour <- "#A0A0A0"  # or "gray", which is 808080

  # first, figure out which scenarios have edits, and which have ROI settings.
  # if there is an edit sweep, one or more base-data rows will have additional rows in the
  # final df.
  sweepEdits <- isolate(gvSweep$workingDataEdits)
  sweepROIs <- isolate(gvSweep$workingDataRanges)
  hasEditSweep <- (length(sweepEdits) > 0)
  hasROISweep <- (length(sweepROIs) > 0)
  if (hasEditSweep & hasROISweep) stop("Can't have both edits and ROIs in sweep scenarios")

  #debugLog("defaultEdits")
  #debugLog(capture.output(print(isolate(gvDefault$workingDataEdits))))
  #debugLog("sweepEdits")
  #debugLog(capture.output(print(sweepEdits)))
  
  # for each scenario with edits, figure out which rows are actually different from 
  # the base data in the two dimensions being displayed.  only these get marked as edited.
  # first, the default scenario - for which the edited dataset is included in full.
  scatterDF <- isolate(gvDefault$unfilteredWorkingData)  # with edits applied
  xProp <- isolate(gvSwitches$xProp)
  yProp <- isolate(gvSwitches$yProp)
  baseData <- gvStatics$baseData
  unchangedOnPlot <- (scatterDF[[xProp]] == baseData[[xProp]]) & (scatterDF[[yProp]] == baseData[[yProp]])
  scatterDF$dotShape <- ifelse(unchangedOnPlot, "circle", "diamond")
  defMask <- isolate(gvDefault$workingDataMask)
  if (isolate(gvSwitches$showXSDLines)) {
    if (!is.na(gvStatics$xMean)) {  # if there are enough data rows to define mean & SD
      lowSD <- gvStatics$xLowSD
      highSD <- gvStatics$xHighSD
      scatterXCol <- scatterDF[[xProp]]
      coreMask <- scatterXCol>=lowSD & scatterXCol<=highSD
      scatterDF$dotColour <- ifelse(coreMask, fullColour, paleColour)
    } else scatterDF$dotColour <- paleColour  # there aren't
  } else scatterDF$dotColour <- fullColour
  scatterDF$dotSize <- ifelse(defMask, fullSize, minimalSize)
  scatterDF$dotOpacity <- ifelse(defMask, fullOpacity, zeroOpacity)
  scatterDF$scenario <- 0

  # now for the sweep scenarios.  if there's an edit sweep, prepare and append a partial
  # DF for each scenario.
  if (hasEditSweep) {
    sweepEdits <- isolate(gvSweep$workingDataEdits)
    sweepMasks <- isolate(gvSweep$workingDataMask)
    sweepData <- isolate(gvSweep$unfilteredWorkingData)
    editRows <- as.numeric(names(sweepEdits[[1]]))
    scatterDF[editRows, "dotColour"] <- "green"
    editRowBase <- baseData[editRows,]  # a df with just those rows
    for (s in 1:gvStatics$numScenarios) {
      scenEdits <- sweepEdits[[s]]
      scenPartialDF <- sweepData[[s]][editRows,]
      unchangedOnPlot <- (scenPartialDF[[xProp]] == editRowBase[[xProp]]) & (scenPartialDF[[yProp]] == editRowBase[[yProp]])
      scenPartialDF$dotShape <- ifelse(unchangedOnPlot, "circle", "diamond")
      scenPartialMask <- sweepMasks[[s]][editRows]
      scenPartialDF$dotSize <- ifelse(scenPartialMask, reducedSize, minimalSize)
      scenPartialDF$dotOpacity <- ifelse(scenPartialMask, fullOpacity, zeroOpacity)
      scenPartialDF$dotColour <- gvStatics$scenarioColours[[s]]
      scenPartialDF$scenario <- s
      scatterDF <- rbind(scatterDF, scenPartialDF)   # append sweep-scenario dots
    }
  } else if (hasROISweep) {
    # no additions to the DF from the default scenario, but exclusion status has to be
    # set depending on the ROIs from all scenarios: present in all; present in some;
    # not present in any.
    sweepMasks <- isolate(gvSweep$workingDataMask)
    presentInAll <- defMask & Reduce("&", sweepMasks)
    presentInSome <- defMask | Reduce("|", sweepMasks)
    scatterDF$dotSize <- ifelse(presentInAll, fullSize, reducedSize)
    scatterDF$dotOpacity <- ifelse(presentInAll, fullOpacity,
                                   ifelse(presentInSome, reducedOpacity, zeroOpacity))
  }
  scatterDF$keyField <- as.character(1:nrow(scatterDF))
  scatterDF
}

transitionDataFrame <- function(data) {
  # return a data frame with columns "initialx", "initialy" in which values in the 
  # prevXProp column of the supplied dataset are scaled so they will appear at the same
  # relative positions on the newXProp scale; ditto for prevYProp.
  # movingRows is the number of rows of default-scenario data; now at the end, not start. 
  prevXProp <- gvStatics$xPropLatest
  prevYProp <- gvStatics$yPropLatest
  newXProp <- isolate(gvSwitches$xProp)
  newYProp <- isolate(gvSwitches$yProp)
  rescaled <- 
    (!is.null(prevXProp) && (prevXProp != newXProp)) ||
    (!is.null(prevYProp) && (prevYProp != newYProp))
  # debugLog(paste(prevXProp, prevYProp, newXProp, newYProp, sep=" "))
  debugLog(paste0("rescaled X or Y: ", rescaled))
  if (rescaled) {   # could be first time through
    update_static("xPropHistory", if (is.null(prevXProp)) newXProp else prevXProp)
    update_static("yPropHistory", if (is.null(prevYProp)) newYProp else prevYProp)
    update_static("xPropLatest", newXProp)     # for next time
    update_static("yPropLatest", newYProp)

    movingRows <- totalRows <- nrow(data)   # used to draw a distinction, but no longer needed
    df <- data[1:movingRows,]
    prevX <- prevXProp
    prevY <- prevYProp
    xFactor <- max(df[[newXProp]])/max(df[[prevX]])
    yFactor <- max(df[[newYProp]])/max(df[[prevY]])
    df$initialx <- df[[prevX]]*xFactor
    df$initialy <- df[[prevY]]*yFactor
    if (movingRows < totalRows) {
      appendedData <- data[(movingRows+1):totalRows,]
      appendedData$initialx <- rep(-1, nrow(appendedData))
      appendedData$initialy <- appendedData$initialx
      df <- rbind(df, appendedData)
    }
  } else {
    df <- data
    df$initialx <- rep(-1, nrow(data))
    df$initialy <- df$initialx
  }
  df$chartx <- df[[newXProp]]
  df$charty <- df[[newYProp]]
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

  # add the registered sources for this chart to those we're waiting for
  sources <- gvStatics[[paste0(id, "Sources")]]
  # was: gvReactives$invalid_sources <- c(isolate(gvReactives$invalid_sources), sources)
  gvStatics$invalid_sources <<- c(gvStatics$invalid_sources, sources)
  if (length(gvStatics$invalid_sources)>0) gvReactives$waiting_for_sources <- TRUE

  update_static(paste0("refresh", id), TRUE)
}

handleTriggerMessage <- function(msg) {
  debugLog(paste0("message from browser: ", msg$message))
  if (msg$message=="newXYProps") {
    recordLatest(); refreshChart('plot1'); refreshChart('plot2');
    args <- msg$args
    gvSwitches$xProp <- args$x
    gvSwitches$yProp <- args$y
  } else if (msg$message == "visitScenario") {
    visitScenario(as.numeric(msg$args))
  } else if (msg$message == "resetSweep") {
    resetSweep()
  } else if (msg$message == "clearControl") {
    # clear one of the range-setting controls.
    # args are [ dataset, row, column ]
    args <- msg$args
    dataset <- args$dataset
    row <- args$row
    column <- args$column
    if (dataset=="workingDataRanges" && row>2) row <- row - 2
    editML <- isolate(gvDefault[[dataset]])
    rowIndex <- as.character(row)
    rowList <- editML[[rowIndex]]
    if (!is.null(rowList)) {
      rowList[[column]] <- NULL
      editML[[rowIndex]] <- rowList
      gvDefault[[dataset]] <- editML
    }
  } else if (msg$message == "edit") {
    # now always a single command (rather than potentially x/y commands bundled into one message).
    # the command has a "type" (data/parameter) and a "target"
    #   for data, target is { dataset, row, xycolumns } - one of xycolumns can be NULL
    #   for parameter, target is a stringy parameter name
    # then a scenarios collection: [0] for default, [n] (1 to 10) for an individual 
    #   sweep scen (which must already exist), or 1..n to set up a new sweep
    # and a corresponding values collection.
    # NB: row and scenarios are numeric, but because we use toFixed on values they are strings
    args <- msg$args
    #debugLog(capture.output(print(args)))
    type <- args$type
    target <- args$target            # string or list
    scenarios <- args$scenarios
    numEditScenarios <- length(scenarios)
    values <- args$values            # collection of strings, or of two-place collections
    if (type == "data") {
      dataset <- target$dataset
      row <- target$row
      xycolumns <- target$xycolumns
      if (identical(scenarios, 0)) {
        # only updating the default
        editML <- isolate(gvDefault[[dataset]])
      } else if (numEditScenarios>1) {
        # creating a new sweep.  first (re)initialise all sweep variables.
        setupNewSweep(numEditScenarios)
        # then set up an empty sweep set for this one 
        editMLSweep <- lapply(1:numEditScenarios, function(x) list())
      } else {
        editMLSweep <- isolate(gvSweep[[dataset]])
      }
      # construct mList entry `row`: column=value
      for (si in 1:length(scenarios)) {
        scen <- scenarios[[si]]
        mList <- if (scen==0) editML else editMLSweep[[scen]]
        xyvalues <- as.numeric(values[[si]])
        
        # minor hack: for workingDataRanges, map edits to rows 3 and 4 onto ML rows 1 & 2; also constrain low<=high relationship, if the other one has already been defined.
        if (dataset=="workingDataRanges") {
          if (row>2) row <- row - 2
          otherRow <- as.character(3 - row)
          # no doubt a neater way to do this, but...
          for (i in 1:2) {
            column <- xycolumns[[i]]
            if (!identical(column, "-")) {
              value <- xyvalues[[i]]
              otherLimit <- mList[[otherRow]][[column]]
              if (!is.null(otherLimit)) {
                xyvalues[[i]] <- if (row==1) min(value, otherLimit) else max(value, otherLimit)
              }
            }
          }
        }
        
        rowIndex <- as.character(row)
        rowList <- mList[[rowIndex]]
        if (is.null(rowList)) rowList <- list()
        for (i in 1:2) {
          column <- xycolumns[[i]]
          if (!identical(column, "-")) rowList[[column]] <- xyvalues[[i]]
        }
        mList[[rowIndex]] <- rowList
        if (scen==0) editML <- mList else editMLSweep[[scen]] <- mList
      }
      if (identical(scenarios, 0)) update_defaultReactive(dataset, editML)
      else update_sweepReactive(dataset, editMLSweep)
    } else {           # assume type is "parameter"
      # target is a parameter-name string
      # as for data, scenarios is 0 for default, n (1 to 10) for an individual 
      # sweep scen (which must already exist), or 1..n to set up a new sweep.
      parm <- target
      values <- as.numeric(values)
      if (numEditScenarios>1) {
        # creating a new sweep.  first (re)initialise all sweep variables.
        setupNewSweep(numEditScenarios)
        # then store the values
        update_sweepReactive(parm, as.list(values))
      } else if (identical(scenarios, 0)) {
        # only updating the default
        update_defaultReactive(parm, values)
      } else {
        sweepList <- isolate(gvSweep[[parm]])
        sweepList[[scenarios]] <- values
        update_sweepReactive(parm, sweepList)
      }
    }
  }
}

