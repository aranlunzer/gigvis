flatten <- function(node, parent = NULL, session = NULL) {
  
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
  } else {  
    # Create new pipeline connected to parent
    node$pipeline <- connect(node$data, node$props, parent$pipeline, session)
    node$pipeline_id <- paste0(
      parent$pipeline_id,
      pipeline_id(node$data, node$props)
    )
  }
  
  if (is.mark(node)) {
    # Base case: so return self
    list(node)
  } else {
    # Otherwise, recurse through children
    children <- lapply(node$children, flatten, parent = node, session = session)
    unlist(children, recursive = FALSE)
  }
}

extract_data <- function(nodes) {
  data_table <- new.env(parent = emptyenv())
  for (node in nodes) {
    id <- node$pipeline_id
    if (exists(id, data_table)) next
    
    data_table[[id]] <- node$pipeline
  }
  
  data_table
}

# ael - separated out the logic for extracting data from a spec, so we don't 
# have to create reactives we don't want.
# There could *very* well be a more direct way of doing this.
# (ael added)
extract_props <- function(nodes) {
  # extract correlated lists of pipeline ids and props
  pipeline_id <- vapply(nodes, function(x) x$pipeline_id, character(1))
  props <- lapply(nodes, function(x) x$props)
  # set up as a list of props keyed by id
  props_by_id <- split(props, pipeline_id)
  # and then flatten the props
  props_by_id <- lapply(props_by_id, unlist, recursive = FALSE)
  # do something involving replacing property keys (such as x, y) with the 
  # properties they refer to (wt, mpg etc)
  uprops_by_id <- lapply(props_by_id, function(props) {
    names <- vapply(props, prop_name, character(1))
    ok <- !duplicated(names) & names != ""
    
    setNames(props[ok], names[ok])
  })
  uprops_by_id
}

# Create a new reactive dataset containing only the data actually used
# by properties.
active_props <- function(data, nodes) {
  uprops_by_id <- extract_props(nodes)

  reactive_prop <- function(props, data) {
    force(props)
    force(data)
    reactive(apply_props(data(), props))
  }
  
  data_out <- new.env(parent = emptyenv())
  for (data_n in names(uprops_by_id)) {
    data_out[[data_n]] <- reactive_prop(uprops_by_id[[data_n]], data[[data_n]])
  }
  
  data_out
}

# (ael added) Return a data table suitable for use in creating a spec with the 
# data embedded.
static_props <- function(data, nodes) {
  uprops_by_id <- extract_props(nodes)
  
  data_out <- new.env(parent = emptyenv())

  static_prop <- function(props, data) {
    force(props)
    force(data)
    apply_props(isolate(data()), props)
  }

  for (data_n in names(uprops_by_id)) {
    data_out[[data_n]] <- static_prop(uprops_by_id[[data_n]], data[[data_n]])
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
  split_df_apply(data, apply_props, props)
}