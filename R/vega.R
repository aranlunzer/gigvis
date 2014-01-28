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
#' @param session a session object from shiny
#' @param dynamic whether to generate dynamic or static spec
as.vega.ggvis <- function(x, session = NULL, dynamic = FALSE, ...) {
  
  nodes <- flatten(x, session = session)
  data_table <- extract_data(nodes)
  
  data_table <- active_props(data_table, nodes)
  data_names <- ls(data_table, all.names = TRUE)
  
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
  axes <- apply_axes_defaults(axes, scales)
  # ael: allow an axis to be suppressed by setting its title to "-"
  axes <- axes[which(vapply(axes, function(a) !identical(a$title,"-"), logical(1)))]
  if (length(axes)==0) { axesVega <- NA }
  else { axesVega <- lapply(axes, as.vega) }

  legends <- add_default_legends(x$legends, scales)
  legends <- apply_legends_defaults(legends, scales)
  opts <- add_default_opts(x$opts[[1]] %||% opts())

  spec <- list(
    data = datasets,
    scales = unname(scales),
    marks = lapply(nodes, as.vega),
    width = opts$width,
    height = opts$height,
    legends = lapply(legends, as.vega),
    axes = axesVega,
    padding = as.vega(opts$padding),
    ggvis_opts = as.vega(opts),
    handlers = lapply(handlers(x), as.vega)
  )
  
  structure(spec, data_table = data_table)
}

# Given a ggvis mark object and set of scales, output a vega mark object
#' @export
as.vega.mark <- function(mark) {
  # Keep only the vega-specific fields, then remove the class, drop nulls,
  # and convert to proper format for vega properties.
  defaults <- default_mark_properties(mark)
  props <- merge_props(defaults, mark$props)

  # Pull out key from props, if present
  key <- props$key
  if (!is.null(key)) {
    props$key <- NULL
  }

  # ael: same for sharedProvenance (which we'll store in the mark's description), 
  # though unlike for key there's no special handling
  # preventing sharedProvenance from being stored with the .update tag
  sharedProvenance <- props$sharedProvenance.update
  if (!is.null(sharedProvenance)) {
    props$sharedProvenance.update <- NULL
  }

  check_mark_props(mark, names(props))

  # ael: fiddle with the properties, adding "highlight" for brushing and "initial"
  # for smooth changes.
  # supplied properties will include some or all of update, enter, exit, hover
  properties <- as.vega(props)
  properties$ggvis <- list()
  if (mark$type == "symbol" || mark$type == "rect") {
    # as.vega.ggvis_props() defaults to attaching properties to "update"
    properties$highlight <- as.vega(props(fill:="orange"))$update
    properties$scenarioHighlight <- as.vega(props(fillOpacity:=0.75))$update
  }
  # ael: provide a default faded "enter" on symbol and line marks
  if (is.null(properties$enter) && (mark$type == "symbol" || mark$type == "line")) {
    properties$enter <- as.vega(props(opacity:=0.25))$update
  }
  # ael: add description, which is now partially replicated by the "from" annotation
  description <- list()
  if (!is.null(sharedProvenance)) {
    description <- as.list(fromJSON(sharedProvenance$value, asText=TRUE))
  }
  if (!is.null(mark$datasource_id)) {
    description$datasource <- mark$datasource_id
  } else {
    description$datasource <- data
  }
    
  # HW: It seems less than ideal to have to inspect the data here, but
  # I'm not sure how else we can figure it out.
  split <- is.split_df(isolate(mark$pipeline()))

  if (split) {
    data <- paste0(mark$pipeline_id, "_tree")
    properties$ggvis$data <- list(value = data)

    m <- list(
      type = "group",
      from = list(data = data),
      marks = list(
        list(
          type = mark$type,
          description = description,
          properties = properties
        )
      )
    )

  } else {
    data <- mark$pipeline_id
    properties$ggvis$data <- list(value = data)

    m <- list(
      type = mark$type,
      description = description,
      properties = properties,
      from = list(data = data)
    )
  }

  if (!is.null(key)) {
    m$key <- paste0("data.", prop_name(key))
  }
  m
}

#' @export
as.vega.ggvis_props <- function(x, default_scales = NULL) {
  x <- prop_sets(x)

  # Given a list of property sets (enter, update, etc.), return appropriate
  # vega property set.
  vega_prop_set <- function(x) {
    if (empty(x)) return(NULL)

    props <- trim_propset(names(x))
    default_scales <- default_scales %||% prop_to_scale(props)
    Map(prop_vega, x, default_scales)
  }

  lapply(x, vega_prop_set)
}

#' @export
as.vega.vega_axis <- function(x) {
  if (empty(x$properties)) {
    x$properties <- NULL
  } else {
    x$properties <- lapply(x$properties, as.vega)
  }

  unclass(x)
}
#' @export
as.vega.vega_legend <- as.vega.vega_axis

#' @export
as.vega.data.frame <- function(x, name, ...) {
  # For CSV output, we need to unescape periods, which were turned into \. by
  # prop_name().
  names(x) <- gsub("\\.", ".", names(x), fixed = TRUE)

  list(list(
    name = name,
    format = list(
      type = "csv",
      # Figure out correct vega parsers for non-string columns
      parse = unlist(lapply(x, vega_data_parser))
    ),
    values = to_csv(x)
  ))
}

#' @export
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
