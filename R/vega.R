#' Coerce an ggvis object to a vega list.
#'
#' This generic function powers the coercion of ggvis objects to vega
#' compatible data structures.
#'
#' @param x an object to convert to vega
#' @return a list. When converted to JSON, will be the type of structure
#'   that vega expects.
#' @keywords internal
as.vega <- function(x, ...) {
  UseMethod("as.vega", x)
}

#' @method as.vega ggvis
#' @export
#' @rdname as.vega
#' @param width,height width and height of plot, in pixels
#' @param padding padding, as described by \code{\link{padding}}
#' @param session a session object from shiny
#' @param dynamic whether to generate dynamic or static spec
as.vega.ggvis <- function(x, width = 600, height = 430, padding = NULL,
                           session = NULL, dynamic = FALSE, ...) {
  if (is.null(padding)) padding <- padding(10,30,20,30) # top, right, bottom, left
  # if (is.null(padding)) padding <- padding()
  
  nodes <- flatten(x, session = session)
  data_table <- extract_data(nodes)
  
  # keep a record of derived reactives, so we can discard them prior to refreshing the chart
  all_reactives <- list()
  data_table <- active_props(data_table, nodes)
  data_names <- ls(data_table, all.names = TRUE)
  for (name in data_names)
    all_reactives[[length(all_reactives)+1]] <- data_table[[name]]
  
  if (dynamic) {
    # Don't provide data now, just the name.
    # If a data source is split, Vega expects two datasets: foo and foo_tree.
    # This is handled in the non-dynamic branch by as.vega.split_df
    # returning two list entries.  Here we prepare the way for those two datasets.
    datasets <- list()
    for (name in data_names) {
      datasets[[length(datasets)+1]] <- list(name=name)
      if (is.split_df(isolate(data_table[[name]]()))) {
        datasets[[length(datasets)+1]] <- list(name=paste0(name, "_tree"))
      }
    }
    # original version:
    # datasets <- lapply(data_names, function(name) {
    #  list(name = name)
    # })
  } else {
    # not dynamic: include the data in the vega spec
    datasets <- unlist(lapply(data_names, function(name) {
      data <- isolate(data_table[[name]]())
      as.vega(data, name)
    }), recursive = FALSE)
  }
  
  scales <- add_default_scales(x, nodes, data_table)
  axes <- add_default_axes(x$axes, scales)
  legends <- add_default_legends(x$legends, scales)

  spec <- list(
    data = datasets,
    scales = unname(scales),
    marks = lapply(nodes, as.vega),
    width = width,
    height = height,
    legends = lapply(legends, as.vega),
    axes = lapply(axes, as.vega),
    padding = as.vega(padding)
  )
  
  structure(spec, data_table = data_table, all_reactives = all_reactives)
}

# Given a ggvis mark object and set of scales, output a vega mark object
#' @S3method as.vega mark
as.vega.mark <- function(mark) {
  # Keep only the vega-specific fields, then remove the class, drop nulls,
  # and convert to proper format for vega properties.
  defaults <- default_mark_properties(mark)
  props <- merge_props(defaults, mark$props)
  check_mark_props(mark, names(props))

  # ael: fiddle with the properties, adding "highlight" for brushing and "initial"
  # for smooth changes.
  # if there is a sharedProvenance property, don't push it through to
  # the items but store it in the mark's description.
  baseprops <- props
  vegaprops <- as.vega(baseprops)   # might include sharedProvenance
  markprops <- list()
  markprops$update <- vegaprops[names(vegaprops)!="sharedProvenance"]
  if (mark$type == "symbol" || mark$type == "rect") {
    markprops$highlight <- as.vega(props(fill="red"))
    markprops$scenarioHighlight <- as.vega(props(fill="green", fillOpacity=0.75))
  }
  props_list <- as.list(baseprops)
  if (!is.null(props_list$initialx)) {
    markprops$initial <- as.vega(props(x=prop(quote(initialx), scale="x"),
                                       y=prop(quote(initialy), scale="y"),
                                       opacity=0.25))
  } else {
    markprops$initial <- as.vega(props(opacity=0.25))
  }
  
  # HW: It seems less than ideal to have to inspect the data here, but
  # I'm not sure how else we can figure it out.
  split <- is.split_df(isolate(mark$pipeline()))

  if (split) {
    list(
      type = "group",
      from = list(data = paste0(mark$pipeline_id, "_tree")),
      marks = list(
        list(
          type = mark$type,
          properties = markprops
        )
      )
    )
  } else {
    froms <- list(data = mark$pipeline_id)
    description <- list()
    if (!is.null(vegaprops$sharedProvenance)) {
      description <- as.list(fromJSON(vegaprops$sharedProvenance$value))
    }
    if (!is.null(mark$datasource_id)) {
      description$datasource <- mark$datasource_id
    } else {
      description$datasource <- mark$pipeline_id
    }

    list(
      type = mark$type,
      description = description,
      properties = markprops,
      from = froms
    )
  }
}

#' @S3method as.vega ggvis_props
as.vega.ggvis_props <- function(x, default_scales = NULL) {
  if (empty(x)) return(NULL)

  default_scales <- default_scales %||% prop_to_scale(names(x))
  Map(prop_vega, x, default_scales)
}

#' @S3method as.vega vega_axis
as.vega.vega_axis <- function(x) {
  if (empty(x$properties)) {
    x$properties <- NULL
  } else {
    x$properties <- lapply(x$properties, as.vega)
  }

  unclass(x)
}
#' @S3method as.vega vega_legend
as.vega.vega_legend <- as.vega.vega_axis

#' @S3method as.vega data.frame
as.vega.data.frame <- function(x, name, ...) {
  list(list(
    name = name,
    values = df_to_json(x)
  ))
}

#' @S3method as.vega split_df
as.vega.split_df <- function(x, name, ...) {
  data <- lapply(x, function(x) list(children = df_to_json(x)))

  list(
    list(
      name = paste0(name, "_tree"),
      format = list(type = "treejson"),
      values = list(children = data)
    ),
    list(
      name = name,
      source = paste0(name, "_tree"),
      transform = list(list(type = "flatten"))
    )
  )
}
