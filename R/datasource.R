#' Create a new data source.
#'
#' A data source implements a pointer (using a environment and a promise)
#' to an existing data frame or other data source in R. Most of the time you
#' should not need to call this function directly - just supply an object
#' to \code{\link{ggvis}} and \code{datasource} will be automatically called.
#' You may want to call this function directly in order to control the 
#' datasource name used when printing and in the vega plot specification.
#'
#' @param data a data frame
#' @param name the name of the data frame (used in error messages etc.)
#' @export
#' @keywords internal
#' @importFrom digest digest
#' @examples
#' datasource(mtcars)
#'
#' # A simple example of a reactive data source
#' library(shiny)
#' v <- reactiveValues(n = 10)
#' p <- pipeline(reactive(mtcars[1:v$n, ]))
#' props <- props(x = ~wt, y = ~mpg)
#'
#' sluice(p, props)
#'
#' v$n <- 5
#' sluice(p, props)
datasource <- function(data, name = deparse2(substitute(data))) {
  if (is.null(data)) return(NULL)

  if (isTRUE(getOption('shiny.withlively'))) {
    hash <- "00"
  } else {
    hash <- digest(data)
  }
    
  structure(list(
    env = environment(),
    name = name,
    hash = hash
  ), class = c(source_class(data), "datasource", "pipe"))
}

source_class <- function(x) UseMethod("source_class")
#' @export
source_class.reactive <- function(x) "datasource_reactive"
#' @export
source_class.default <- function(x) NULL

#' @export
format.datasource <- function(x, ...) {
  paste0("|-> ", x$name, " (", x$hash, ")")
}

#' @export
is_source.datasource <- function(x) TRUE

#' @export
#' ael: if non-NULL props are specified, come up with an id that incorporates them -
#' so that different data subsets are distinguishable by their pipeline ids
pipe_id.datasource <- function(x, props) {
  # paste0(x$name, "_", x$hash)  # original version
  if (is.null(props)) {
    paste0(x$name, "/", x$hash, "/")
  } else {
    # now that props can be expressions such as   ~time + 1   we need to concat their strings.
    # it turns out they can also now be functions (e.g. see prop.R line 63)
    propStr <- paste(
      vapply(
        props,
        function(p)
          if (is.function(p$value)) "(func)"
          else paste(as.character(p$value), collapse=""),
        character(1)
        ),
      collapse="")
    paste0(x$name, "/", x$hash, ":", propStr, "/")
  }
}

# Connect methods --------------------------------------------------------------

#' @export
#' @importFrom shiny reactive
connect.datasource <- function(x, props, source = NULL, session = NULL) {
  reactive(x$env$data)
}
#' @export
connect.datasource_reactive <- function(x, props, source = NULL, session = NULL) {
  x$env$data
}

