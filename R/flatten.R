flatten <- function(node, parent = NULL, session = NULL) {
  node$props <- init_inputs(node$props, session)

  # Inherit behaviour from parent
  node$dynamic <- node$dynamic %||% parent$dynamic
  node$props <- merge_props(parent$props, node$props)

  # Create reactive pipeline, connected to parents
  if (empty(node$data)) {
    if (is.mark(node) && empty(parent$pipeline)) {
      stop("Node inherits data from parent, but parent has no data",
           call. = FALSE)
    }

    # Point to parent data
    node$pipeline <- parent$pipeline
    node$pipeline_id <- parent$pipeline_id
    node$datasource_id <- parent$datasource_id
  } else {
    # Create new pipeline connected to parent
    node$pipeline <- connect(node$data, node$props, parent$pipeline, session)
    # ael: if there is a parent id, put a separator into the concatenation
    p_id <- parent$pipeline_id
    n_id <- pipeline_id(node$data, node$props)
    if (is.null(p_id) || p_id == "") node$pipeline_id <- n_id
    else node$pipeline_id <- paste0(p_id, ":", n_id)
    prop_list <- as.list(node$props)
    if (!is.null(prop_list$sharedProvenance)) {
      provenance <- as.list(fromJSON(prop_list$sharedProvenance$value)) 
      if (!is.null(provenance$scenario))
        node$pipeline_id <- paste0(node$pipeline_id, provenance$scenario)
      if (!is.null(provenance$datasource))
        node$datasource_id <- provenance$datasource
    }
    # ael: we want datasource_id to correspond to the first data encountered, so copy down
    # from parent to child.  In addition a node can be given an explicit datasource that overrides
    # that from the data.
    if (is.null(node$datasource_id)) {   # not set from sharedProvenance
      if (is.null(parent$datasource_id)) node$datasource_id <- node$pipeline_id # digest(sluice(node$data, props=props()))
      else node$datasource_id <- parent$datasource_id
    }
  }

  if (is.mark(node)) {
    # Base case, so return self
    list(node)
  } else {
    # Otherwise recurse through children
    children <- lapply(node$children, flatten, parent = node, session = session)
    unlist(children, recursive = FALSE)
  }
}

# return the reactive pipelines mentioned in a flattened spec
extract_data <- function(nodes) {
  data_table <- new.env(parent = emptyenv())
  # if multiple nodes mention the same pipeline id, assume it's the same pipeline
  for (node in nodes) {
    id <- node$pipeline_id
    if (exists(id, data_table)) next

    data_table[[id]] <- node$pipeline
  }

  data_table
}

# Create a new reactive dataset containing only the data actually used
# by properties.
# data is a table of reactive pipelines (prepared by flatten()) that deliver whole
# data sources.  Here we examine all nodes that refer to a given pipeline, deduce
# which properties in the data source they use, and prepare new reactives that
# filter the original pipeline's (possibly changing) data down to that property subset.
active_props <- function(data, nodes) {
  # Collect all props for given data
  pipeline_id <- vapply(nodes, function(x) x$pipeline_id, character(1))
  props <- lapply(nodes, function(x) x$props)
  
  props_by_id <- split(props, pipeline_id)
  props_by_id <- lapply(props_by_id, unlist, recursive = FALSE)
  
  uprops_by_id <- lapply(props_by_id, function(props) {
    names <- vapply(props, prop_name, character(1))
    ok <- !duplicated(names) & names != ""
    
    setNames(props[ok], names[ok])
  })
  
  reactive_prop <- function(props, data) {
    force(props)
    force(data)
    reactive(apply_props(data(), props), label="react_prop")
  }
  
  data_out <- new.env(parent = emptyenv())
  for (data_n in names(uprops_by_id)) {
    data_out[[data_n]] <- reactive_prop(uprops_by_id[[data_n]], data[[data_n]])
  }
  
  data_out
}

# Apply properties to a data object, creating calculated columns and dropping
# unused columns.
apply_props <- function(data, props) {
  UseMethod("apply_props")
}

#' @S3method apply_props data.frame
apply_props.data.frame <- function(data, props) {
  cols <- lapply(props, prop_value, data = data)
  names(cols) <- vapply(props, prop_name, character(1))

  quickdf(compact(cols))
}

#' @S3method apply_props split_df
apply_props.split_df <- function(data, props) {
  data[] <- lapply(data, apply_props, props)
  data
}
