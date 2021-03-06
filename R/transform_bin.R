#' Transformation: bin continuous variable.
#'
#' \code{transform_bin} is a data transformation that reduces a one-d vector 
#' of positions to a data frame of binned counts. \code{layer_histogram} 
#' combines \code{transform_bin} with \code{mark_rect} to create a histogram,
#' and \code{layer_freqpoly} combines \code{transform_bin} with 
#' \code{mark_line} to create a frequency polygon.
#' 
#' @section Input:
#' Currently \code{transform_bin} only works with data frames and requires the
#' following properties:
#' 
#' \itemize{
#'   \item \code{x}, numeric, the values to bin and count
#' }
#'
#' @section Ouput:
#'
#' \code{transform_bin} creates a data frame with columns:
#'
#' \itemize{
#'  \item \code{count__}: the number of points
#'  \item \code{x}: mid-point of bin 
#'  \item \code{xmin__}: left boundary of bin 
#'  \item \code{xmax__}: right boundary of bin
#'  \item \code{width__}: width of bin
#' }
#'
#' @param binwidth The width of the bins. The default is \code{guess()}, which
#'   yields 30 bins that cover the range of the data. You should always override
#'   this value, exploring multiple widths to find the best to illustrate the
#'   stories in your data.
#' @param origin The initial position of the left-most bin. If \code{NULL}, the
#'   the default, will use the smallest value in the dataset.
#' @param right Should bins be right-open, left-closed, or
#'   right-closed, left-open
#' @param ... For \code{transform_bin}: ignored, all transforms must use 
#'   named arguments.  For \code{layer_histogram}: named arguments are 
#'   passed on to the transform, unnamed arguments are passed on to the layer.
#' @export
#' @examples
#' # Create histograms and frequency polygons with layers
#' ggvis(mtcars, props(x = ~mpg), layer_histogram())
#' ggvis(mtcars, props(x = ~mpg), layer_histogram(binwidth = 2))
#' ggvis(mtcars, props(x = ~mpg), layer_freqpoly(binwidth = 2))
#'
#' # These are equivalent to combining transform_bin with the corresponding
#' # mark
#' ggvis(mtcars, props(x = ~mpg), transform_bin(binwidth = 2),
#'   mark_rect(props(x = ~xmin__, x2 = ~xmax__, y = ~count__, y2 = 0)),
#'   mark_line(props(x = ~x, y = ~count__, stroke := "red", strokeWidth := 4))
#' )
#'
#' # You can also combine other data transformations like splitting
#' ggvis(mtcars, props(x = ~mpg, stroke = ~cyl, strokeWidth = 4),
#'    by_group(cyl), layer_freqpoly(binwidth = 2))
#' 
#' # You can see the results of a transformation by creating your own pipeline
#' # and flowing data through it
#' sluice(pipeline(mtcars, transform_bin(2)), props(x = ~mpg))
#' sluice(pipeline(mtcars, by_group(cyl), transform_bin(2)), props(x = ~disp))
#' # Or
#' pl <- pipeline(mtcars, transform_bin(10))
#' sluice(pl, props(x = ~disp))

# ael : added datamax 
transform_bin <- function(..., binwidth = guess(), origin = NULL, right = TRUE, datamax=NULL) {
  transform("bin",
    binwidth = binwidth,
    origin = origin,
    right = right,
    datamax = datamax
  )
}

#' @rdname transform_bin
#' @export
layer_histogram <- function(...) {
  layer(
    transform_bin(...),
    layer(
      props(x = ~xmin__, x2 = ~xmax__, y = ~count__, y2 = 0, datarows = prop(quote(indices__), scale = FALSE)),
      mark_rect(),
      ...,
      drop_named = TRUE
    )
  )
}

#' @rdname transform_bin
#' @export
#' @inheritParams layer_histogram
layer_freqpoly <- function(...) {
  layer(
    transform_bin(...),
    layer(
      props(x = ~x, y = ~count__),
      mark_line(),
      ...,
      drop_named = TRUE
    )
  )
}

#' @export
format.transform_bin <- function(x, ...) {
  paste0(" -> bin", param_string(x))
}

#' @export
compute.transform_bin <- function(x, props, data) {
  check_prop(x, props, data, "x.update", c("numeric", "datetime"))

  if (is.guess(x$binwidth)) {
    x$binwidth <- diff(prop_range(data, props$x)) / 30

    if (inherits(x$binwidth, "difftime")) {
      x$binwidth <- as.numeric(x$binwidth, units = "secs")
    }

    message("Guess: transform_bin(binwidth = ", format(x$binwidth, digits = 3),
      ") # range / 30")
  }

  output <- bin(data, x_var = props$x,
    binwidth = x$binwidth, origin = x$origin, right = x$right, datamax = x$datamax)

  # TODO: Check for zero-row output for other data types
  if (is.data.frame(output) && nrow(output) == 0) return(output)

  preserve_constants(data, output)
}

bin <- function(data, ...) UseMethod("bin")

#' @export
bin.split_df <- function(x, x_var, ...) {
  x[] <- lapply(x, bin, x_var = x_var, ...)
  x
}

#' @export
bin.data.frame <- function(x, x_var, ...) {
  x_val <- remove_missing(prop_value(x_var, x))
  bin(x_val, ...)
}

# aam's new version, with indices__ property
#' @export
bin.numeric <- function(x, weight = NULL, binwidth = 1, origin = NULL, right = TRUE, datamax = NULL) {
  stopifnot(is.numeric(binwidth) && length(binwidth) == 1)
  stopifnot(is.null(origin) || (is.numeric(origin) && length(origin) == 1))
  stopifnot(is.flag(right))
  
  if (is.null(datamax)) datamax <- max(x)

  if (length(na.omit(x)) == 0) {
    return(data.frame(
      count__ = numeric(0),
      x = numeric(0),
      xmin__ = numeric(0),
      xmax__ = numeric(0),
      width__ = numeric(0)
    ))
  }

  if (is.null(weight))  weight <- rep(1, length(x))
  weight[is.na(weight)] <- 0
  
  # ael - bins go from origin to the max of the data.  if the origin is greater than
  # the max, ignore it.
  if (is.null(origin) || origin > datamax) {
    breaks <- fullseq(range(x), binwidth, pad = TRUE)
  } else {
    # add origin to end value to ensure we always come up with the same number of bins 
    # (even if last one is guaranteed to be empty).  This might not suit everyone's taste in histograms.
    breaks <- seq(origin, origin + datamax + binwidth, binwidth)  
  }

  # Adapt break fuzziness from base::hist - this protects from floating
  # point rounding errors
  diddle <- 1e-07 * stats::median(diff(breaks))
  if (right) {
    fuzz <- c(-diddle, rep.int(diddle, length(breaks) - 1))
  } else {
    fuzz <- c(rep.int(-diddle, length(breaks) - 1), diddle)
  }
  fuzzybreaks <- sort(breaks) + fuzz
  
  bins <- cut(x, fuzzybreaks, include.lowest = TRUE, right = right)
  left <- breaks[-length(breaks)]
  right <- breaks[-1]
  x <- (left + right)/2
  width <- diff(breaks)
  
  count <- as.numeric(tapply(weight, bins, sum, na.rm=TRUE))
  count[is.na(count)] <- 0

  indices <- 0
  for (i in 1:length(levels(bins))){
    if(length(which(bins==levels(bins)[i])) != 0){
      indices[i] <- toJSON(which(bins==levels(bins)[i]))
    }
    else {
      indices[i] <- "[]"
    }
  }

  results <- data.frame(
    count__ = count,
    x = x,
    xmin__ = x - width/2,
    xmax__ = x + width/2,
    width__ = width,
    indices__ = indices
  )
  
  results
}

#' @export
bin.POSIXt <- function(x, weight = NULL, binwidth = 1, origin = NULL, right = TRUE) {
  # Convert times to raw numbers (seconds since UNIX epoch), and call bin.numeric
  results <- bin(as.numeric(x), weight, binwidth, origin, right)

  # Convert some columns from numeric back to POSIXct objects
  time_cols <- c("x", "xmin__", "xmax__")
  results[time_cols] <- lapply(results[time_cols], function(col) {
    structure(col, class = c("POSIXct", "POSIXt"))
  })

  results
}
