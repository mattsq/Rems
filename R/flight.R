
#' Default directories to exclude from tree traversal
#'
#' @description
#' These directories are excluded from tree traversal by default,
#' but can be overridden by setting options.
#'
#' @keywords internal
.default_exclude_dirs <- c(
  'Download Information', 'Download Review', 'Processing',
  'Profile 16 Extra Data', 'Flight Review', 'Data Information',
  'Operational Information', 'Operational Information (ODW2)',
  'Weather Information', 'Profiles', 'Profile'
)

#' Get directories to exclude from tree traversal
#'
#' @description
#' Returns the list of directories to exclude from tree traversal.
#' This can be customized by setting the 'flightems.exclude_dirs' option.
#'
#' @return Character vector of directory names to exclude
#'
#' @examples
#' # Get current exclusion list
#' get_exclude_dirs()
#'
#' # Set custom exclusion list
#' options(flightems.exclude_dirs = c("Profiles", "Weather Information"))
#' get_exclude_dirs()
#'
#' # Reset to defaults
#' options(flightems.exclude_dirs = NULL)
#' get_exclude_dirs()
#'
#' @export
get_exclude_dirs <- function() {
  # Get from options if set, otherwise use defaults
  getOption("flightems.exclude_dirs", .default_exclude_dirs)
}

sanitize_sql_input <- function(input) {
  if (is.numeric(input)) {
    return(input)  # Numeric values don't need escaping
  } else if (is.character(input)) {
    # Escape single quotes and other potentially dangerous characters
    return(gsub("'", "''", input))
  } else {
    return(as.character(input))
  }
}

#' Create a Flight Object
#'
#' @description
#' Creates a new Flight object that represents a flight record in the EMS system.
#' This is the main entry point for working with flight data.
#'
#' @param conn A connection object to the EMS API or database
#' @param ems_id Numeric identifier for the EMS record
#' @param data_file Optional path to a local data file. If NULL, data will be fetched from the connection
#'
#' @return A Flight object with the following components:
#'   \item{ems_id}{The EMS identifier for this system}
#'   \item{connection}{The connection to the EMS system}
#'   \item{db_id}{The database identifier (initially NULL)}
#'   \item{metadata}{Metadata for the flight}
#'   \item{trees}{List containing fieldtree, dbtree, and kvmaps data frames}
#'
#' @examples
#' \dontrun{
#' # Create a connection
#' conn <- connect_to_ems("https://ems-api.example.com", "username", "password")
#'
#' # Create a flight object
#' flt <- flight(conn, 12345)
#'
#' # Create a flight object with local data
#' flt <- flight(conn, 12345, "path/to/local/data.rds")
#' }
#'
flight <-
  function(conn, ems_id, data_file = NULL)
  {
    # Validate connection
    if (missing(conn)) {
      stop("Connection object is required")
    }

    if (!is.list(conn) || is.null(conn$uri_root)) {
      stop("Invalid connection object. Please use a proper EMS connection")
    }

    # Validate ems_id
    if (missing(ems_id)) {
      stop("EMS ID is required")
    }

    if (!is.numeric(ems_id) || ems_id <= 0 || ems_id != round(ems_id)) {
      stop("EMS ID must be a positive integer")
    }

    # Validate data_file if provided
    if (!is.null(data_file)) {
      if (!is.character(data_file) || length(data_file) != 1) {
        stop("data_file must be a single character string")
      }

      if (!file.exists(data_file)) {
        stop(sprintf("File not found: %s", data_file))
      }
    }

    # Create flight object
    obj <- list()
    class(obj) <- "Flight"
    obj$ems_id       <- ems_id
    obj$connection   <- conn
    obj$db_id        <- NULL
    obj$metadata     <- NULL
    obj$trees        <- list(fieldtree= data.frame(), dbtree=data.frame(), kvmaps=data.frame())

    # Load tree data
    tryCatch({
      obj <- load_tree(obj, data_file)
    }, error = function(e) {
      stop(sprintf("Failed to load tree data: %s", e$message))
    })

    return(obj)
  }


load_tree <-
  function(flt, file_name)
  {
    if (is.null(flt$metadata)) {
      flt$metadata <- localdata(file_name)
    } else {
      if ((!is.null(file_name)) && (file_loc(flt$metadata) != file.path(file_name))) {
        close.LocalData(flt$metadata)
        flt$metadata <- localdata(file_name)
      }
    }
    flt$trees <- list(fieldtree = get_fieldtree(flt),
                      dbtree    = get_dbtree(flt),
                      kvmaps    = get_kvmaps(flt))
    flt
  }


save_tree <-
  function(flt, file_name = NULL)
  {
    if ( (!is.null(file_name)) && (file_loc(flt$metadata) != file_name) ) {
      file.copy(file_loc(flt$metadata), file_name)
      close.LocalData(flt$metadata)
      flt$metadata <- localdata(file_name)
    }
    save_fieldtree(flt)
    save_dbtree(flt)
    save_kvmaps(flt)
  }

get_fieldtree <-
  function(flt)
  {
    if (is.null(flt$db_id)) {
      cols <- flt$metadata$table_info$fieldtree
      dat  <- data.frame(matrix(NA,nrow=0,ncol=length(cols)), stringsAsFactors = F)
      names(dat) <- cols
      return(dat)
    } else {
      # Sanitize inputs before constructing the query
      safe_ems_id <- sanitize_sql_input(flt$ems_id)
      safe_db_id <- sanitize_sql_input(flt$db_id)
      dat <- get_data(flt$metadata, 'fieldtree',
                      sprintf("ems_id = %d and db_id = '%s'", safe_ems_id, safe_db_id))
      return(dat)
    }
  }


save_fieldtree <-
  function(flt)
  {
    if (nrow(flt$trees$fieldtree) > 0) {
      delete_data(flt$metadata, 'fieldtree', sprintf("ems_id = %d and db_id = '%s'", flt$ems_id, flt$db_id))
      append_data(flt$metadata, 'fieldtree', flt$trees$fieldtree)
    }
  }

get_dbtree <-
  function(flt)
  {
    tr <- get_data(flt$metadata, 'dbtree', paste("ems_id =", flt$ems_id))
    if (nrow(tr) < 1) {
      dbroot <- list(ems_id = flt$ems_id,
                     id     = "[-hub-][entity-type-group][[--][internal-type-group][root]]",
                     name   = "<root>",
                     nodetype = "root",
                     parent_id = "")
      flt$trees$dbtree <- data.frame(dbroot, stringsAsFactors = F)
      flt <- update_children(flt, dbroot, treetype = 'dbtree')
      flt <- update_tree(flt, 'fdw', treetype = 'dbtree', exclude_tree = "APM Events")
      save_dbtree(flt)
      tr <- flt$trees$dbtree
    }
    tr
  }


save_dbtree <-
  function(flt)
  {
    if (nrow(flt$trees$dbtree) > 0) {
      delete_data(flt$metadata, 'dbtree', sprintf("ems_id = %d", flt$ems_id))
      append_data(flt$metadata, 'dbtree', flt$trees$dbtree)
    }
  }

get_kvmaps <-
  function(flt)
  {
    get_data(flt$metadata, 'kvmaps', paste("ems_id =", flt$ems_id))
  }


save_kvmaps <-
  function(flt)
  {
    if (nrow(flt$trees$kvmaps) > 0) {
      delete_data(flt$metadata, 'kvmaps', sprintf("ems_id = %d", flt$ems_id))
      append_data(flt$metadata, 'kvmaps', flt$trees$kvmaps)
    }
  }


#' Set the active database for a Flight object
#'
#' @description
#' Sets the active database for a Flight object by name. This function will
#' match the database name (case-insensitive) and update the flight object
#' accordingly. If multiple matches are found, it will select the one with
#' the shortest name.
#'
#' @param flt A Flight object
#' @param dbname Character string with the name of the database
#' @param legacy Logical, whether to use the legacy behavior (default: FALSE)
#'
#' @return An updated Flight object with the selected database
#'
#' @examples
#' \dontrun{
#' flt <- set_database(flt, "FlightData")
#' }
#'
#' @export
set_database.Flight <-
  function(flt, dbname, legacy = FALSE)
  {
    tr <- flt$trees$dbtree
    db_list <- tr[tr$nodetype=="database" & grepl(escape_pattern(dbname), tr$name, ignore.case=T), c('id', 'name')]

    # Check if we found any matches
    if (nrow(db_list) == 0) {
      stop(sprintf("No database found matching '%s'", dbname))
    }

    # Handle multiple matches
    if (nrow(db_list) > 1) {
      if (legacy) {
        # Legacy behavior just picks the shortest name
        flt$db_id <- db_list[order(nchar(db_list$name))[1], "id"]
      } else {
        # Improved behavior: print matches and let user choose
        cat("Multiple databases match your query:\n")
        for (i in 1:nrow(db_list)) {
          cat(sprintf("%d: %s\n", i, db_list$name[i]))
        }
        cat("Using the first match. Specify a more precise name if this is not what you want.\n")
        flt$db_id <- db_list$id[1]
      }
    } else {
      # Single match
      flt$db_id <- db_list$id
    }

    # Get field tree data
    flt$trees$fieldtree <- get_fieldtree(flt)

    # If the fieldtree is empty, populate it with the root folders for the selected DB
    if (nrow(flt$trees$fieldtree) == 0) {
      flt <- update_children(flt, get_database(flt), treetype= "fieldtree")
    }

    cat(sprintf("Using database '%s'.\n", get_database(flt)$name))
    flt
  }

# Add a compatibility alias for the old function name
#' @rdname set_database.Flight
#' @export
set_database_old.Flight <- function(flt, dbname) {
  .Deprecated("set_database.Flight",
              msg = "set_database_old.Flight is deprecated. Please use set_database.Flight instead.")
  set_database.Flight(flt, dbname, legacy = TRUE)
}

#' Get database information
#'
#' @description
#' Generic function to retrieve database information from different objects.
#'
#' @param x Object containing database information
#' @param ... Additional arguments passed to methods
#'
#' @return Database information (implementation depends on the class of x)
#'
#' @export
get_database <- function(x, ...) {
  UseMethod("get_database")
}

#' @describeIn get_database Get database information from a Flight object
#'
#' @param x A Flight object
#'
#' @return A list containing database information including id, name, and node type
#'
#' @export
get_database.Flight <-
  function(x, ...)
  {
    tr <- x$trees$dbtree
    return(as.list(tr[tr$nodetype=="database" & tr$id==x$db_id, ]))
  }


db_request <-
  function(flt, parent)
  {
    body <- NULL
    if (parent$nodetype=="database_group") {
      body <- list('groupId' = parent$id)
    }
    r    <- request(flt$connection,
                    uri_keys = c('database','group'),
                    uri_args = flt$ems_id,
                    body = body)
    ##  Get the children fields/field groups
    d <- httr::content(r)

    d1 <- list()
    if (length(d$databases) > 0) {
      d1 <- lapply(d$databases, function(x) list(ems_id    = parent$ems_id,
                                                  id        = x$id,
                                                  nodetype  = 'database',
                                                  name      = x$pluralName,
                                                  parent_id = parent$id))
    }
    d2 <- list()
    if (length(d$groups) > 0) {
      d2 <- lapply(d$groups, function(x) list(ems_id    = parent$ems_id,
                                              id        = x$id,
                                              nodetype  = 'database_group',
                                              name      = x$name,
                                              parent_id = parent$id))
    }
    return(list(d1=d1, d2=d2))
  }


#' Request field information from the API
#'
#' @description
#' Requests field information from the API for a given parent node.
#'
#' @param flt A Flight object
#' @param parent A list with field group information
#'
#' @return A list containing field and field group information
#'
#' @importFrom httr http_error status_code content
fl_request <-
  function(flt, parent)
  {
    body <- NULL
    if (parent$nodetype=="field_group") {
      body <- list('groupId' = parent$id)
    }

    # Try to make the API request with error handling
    tryCatch({
      r <- request(flt$connection,
                   uri_keys = c('database','field_group'),
                   uri_args = c(flt$ems_id, flt$db_id),
                   body = body)

      # Check for HTTP errors
      if (httr::http_error(r)) {
        stop(sprintf(
          "API request failed [HTTP %d]: %s",
          httr::status_code(r),
          httr::content(r, as = "text", encoding = "UTF-8")
        ))
      }

      # Parse the response
      d <- httr::content(r)

      # Validate expected structure
      if (is.null(d$fields) && is.null(d$groups)) {
        warning("API response contains neither fields nor groups")
      }

      d1 <- list()
      if (length(d$fields) > 0) {
        d1 <- lapply(d$fields, function(x) list(ems_id    = parent$ems_id,
                                                db_id     = flt$db_id,
                                                id        = x$id,
                                                nodetype  = 'field',
                                                type      = x$type,
                                                name      = x$name,
                                                parent_id = parent$id))
      }
      d2 <- list()
      if (length(d$groups) > 0) {
        d2 <- lapply(d$groups, function(x) list(ems_id    = parent$ems_id,
                                                db_id     = flt$db_id,
                                                id        = x$id,
                                                nodetype  = 'field_group',
                                                type      = '',
                                                name      = x$name,
                                                parent_id = parent$id))
      }
      return(list(d1=d1, d2=d2))

    }, error = function(e) {
      stop(sprintf("Error in field request for %s: %s", parent$name, e$message))
    })
  }


add_subtree <-
  function(flt, parent, exclude_tree = c(), treetype = c('fieldtree', 'dbtree')) {

    cat(sprintf("On %s (%s)...\n", parent$name, parent$nodetype))

    if (treetype == 'dbtree') {
      searchtype <- 'database'
      res <- db_request(flt, parent)
    } else {
      searchtype <- 'field'
      res <- fl_request(flt, parent)
    }

    if (length(res$d1) > 0) {
      flt$trees[[treetype]] <- rbind(flt$trees[[treetype]], lls_to_df(res$d1), stringsAsFactors=F)
      plural <- if (length(res$d1) > 1) "s" else ""
      cat(sprintf("-- Added %d %s%s\n", length(res$d1), searchtype, plural))
    }


    for (x in res$d2) {
      flt$trees[[treetype]] <- rbind(flt$trees[[treetype]], x, stringsAsFactors=F)
      if ( (length(exclude_tree) == 0) || (all(sapply(exclude_tree, function(et) !grepl(et, x$name)))) ) {
        flt <- add_subtree(flt, x, exclude_tree, treetype)
      }
    }
    flt
  }


get_children <-
  function(flt, parent_id, treetype = c('fieldtree','dbtree'))
  {
    tr <- flt$trees[[treetype]]
    return( tr[tr$parent_id %in% parent_id, ])
  }


remove_subtree <-
  function(flt, parent, treetype = c('fieldtree','dbtree'))
  {
    tr <- flt$trees[[treetype]]


    # Update the instance tree by deleting children
    flt$trees[[treetype]] <- tr[tr$parent_id != parent$id, ]

    # Iterate and do recursive removal of children of children
    leaftype <- if (treetype=='fieldtree') 'field' else 'database'
    chld <- tr[(tr$parent_id == parent$id) & (tr$nodetype!=leaftype), ]
    if (nrow(chld) > 0) {
      for (i in 1:nrow(chld)) {
        flt <- remove_subtree(flt, chld[i,], treetype)
      }
    }
    flt
  }


update_children <-
  function(flt, parent, treetype = c('fieldtree', 'dbtree'))
  {

    cat(sprintf("On %s (%s)...\n", parent$name, parent$nodetype))

    if (treetype == 'dbtree') {
      searchtype <- 'database'
      res <- db_request(flt, parent)
    } else {
      searchtype <- 'field'
      res <- fl_request(flt, parent)
    }

    tr <- flt$trees[[treetype]]
    flt$trees[[treetype]] <- subset(tr, !((nodetype==searchtype) & (parent_id == parent$id)))

    if (length(res$d1) > 0) {
      flt$trees[[treetype]] <- rbind(flt$trees[[treetype]], lls_to_df(res$d1), stringsAsFactors=F)
      plural <- if (length(res$d1) > 1) "s" else ""
      cat(sprintf("-- Added %d %s%s\n", length(res$d1), searchtype, plural))
    }
    # If there is an array of groups as children add any that appeared new and remove who does not.
    old_groups <- subset(tr, (nodetype==paste(searchtype, "group", sep="_")) & (parent_id==parent$id))
    old_ones   <- old_groups$id
    new_ones   <- sapply(res$d2, function(x) x$id)

    rm_id <- setdiff(old_ones, new_ones)
    if (length(rm_id) >0) {
      for (x in subset(old_groups, id %in% rm_id)) {
        flt <- remove_subtree(flt, x, treetype)
      }
    }

    add_id <- setdiff(new_ones, old_ones)
    if (length(add_id) > 0) {
      for (x in res$d2) {
        if (x$id %in% add_id) {
          flt$trees[[treetype]] <- rbind(flt$trees[[treetype]], x, stringsAsFactors=F)
        }
      }
    }
    flt
  }


#' Update a tree in the Flight object
#'
#' @description
#' Updates a specific tree (either fieldtree or dbtree) in the Flight object.
#'
#' @param flt A Flight object
#' @param path Path to the node to update
#' @param exclude_tree Optional character vector of directories to exclude
#' @param treetype Type of tree to update ('fieldtree' or 'dbtree')
#'
#' @return Updated Flight object
#'
#' @export
update_tree <-
  function(flt, path, exclude_tree = NULL, treetype=c('fieldtree','dbtree'))
  {
    # Use provided exclude_tree or get from options if NULL and we're in fieldtree mode
    if (is.null(exclude_tree) && treetype == 'fieldtree') {
      exclude_tree <- get_exclude_dirs()
    } else if (is.null(exclude_tree)) {
      exclude_tree <- c()
    }

    searchtype <- if(treetype=="fieldtree") 'field' else 'database'

    path <- tolower(path)
    for ( i in seq_along(tolower(path)) ) {
      p <- escape_pattern(path[i])
      if (i == 1) {
        tr <- flt$trees[[treetype]]
        parent <- tr[grepl(p, tr$name, ignore.case = T), ]
      } else {
        flt     <- update_children(flt, parent, treetype=treetype)
        chld_df <- get_children(flt, parent$id, treetype=treetype)
        child   <- subset(chld_df, grepl(p, name, ignore.case = T))
        parent  <- child
      }
      if (nrow(parent) == 0) {
        stop(sprintf("Search keyword '%s' did not return any %s group.", path[i], searchtype))
      }
      ptype <- paste(searchtype, "group", sep="_")
      parent <- parent[parent$nodetype == ptype, ]
      parent <- get_shortest(parent)
    }
    cat(sprintf("=== Starting to add subtree from '%s' (%s) ===\n", parent$name, parent$nodetype))
    flt <- remove_subtree(flt, parent, treetype=treetype)
    flt <- add_subtree(flt, parent, exclude_tree, treetype = treetype)
    return(flt)
  }


#' Create the default field tree for a Flight object
#'
#' @description
#' Populates the field tree with default fields, excluding specified directories.
#'
#' @param flt A Flight object
#' @param exclude_dirs Optional character vector of directories to exclude.
#'                   If NULL, the package default exclusions will be used.
#'
#' @return Updated Flight object with populated field tree
#'
#' @export
make_default_tree <-
  function(flt, exclude_dirs = NULL)
  {
    # Use provided exclude_dirs or get from options
    if (is.null(exclude_dirs)) {
      exclude_dirs <- get_exclude_dirs()
    }

    dbnode <- get_database(flt)
    flt <- remove_subtree(flt, dbnode, treetype="fieldtree")
    flt <- add_subtree(flt, dbnode, exclude_tree=exclude_dirs, treetype="fieldtree")
    flt
  }

search_fields <-
  function(flt, ..., unique = T)
  {
    flist <- list(...)
    res   <- data.frame()
    for ( f in flist ) {
      if ( length(f) == 1 ) {
        # Single keyword case
        tr <- flt$trees$fieldtree
        fres <- subset(tr, (nodetype=="field") & grepl(escape_pattern(f), name, ignore.case = T))
      } else if ( length(f) > 1 ) {
        # Vector of hierarchical keyword set
        chld <- flt$trees$fieldtree
        for ( i in seq_along(f) ) {
          ff <- escape_pattern(f[i])
          if (i < length(f)) {
            chld      <- chld[chld$nodetype == "field_group", ]
            parent_id <- subset(chld, grepl(ff, name, ignore.case = T))$id
            tr      <- flt$trees$fieldtree
            chld    <- tr[tr$parent_id %in% parent_id, ]
          } else {
            chld    <- subset(chld, (nodetype=='field') & grepl(ff, name, ignore.case = T) )
          }
        }
        fres <- chld
      }
      if (nrow(fres) == 0) {
        # No returned value. Raise an error.
        stop(sprintf("No field found with field keyword %s.", f))
      } else {
        if (unique) {
          fres <- get_shortest(fres)
        }
      }
      res <- rbind(res,fres, stringsAsFactors=F)
    }
    return(lapply(1:nrow(res), function(i) as.list(res[i,])))
  }

#' Update key-value mappings for a field
#'
#' @description
#' Updates the key-value mappings for a specific field in the flight object,
#' fetching from the API if necessary.
#'
#' @param flt A Flight object
#' @param field Character string specifying the field name (optional if field_id is provided)
#' @param field_id Character string specifying the field ID (optional if field is provided)
#'
#' @return An updated Flight object with refreshed key-value mappings
#'
#' @export
update_kvmaps_for_field <-
  function(flt, field = NULL, field_id = NULL)
  {
    if (is.null(field_id)) {
      fld <- search_fields(flt, field)[[1]]
      fld_type <- fld$type
      fld_id <- fld$id
      fld_name <- fld$name
      if (fld_type != "discrete") {
        stop("Queried field should be discrete type to get the list of possible values.")
      }
    } else {
      fld_id <- field_id
      fld_name <- subset(flt$trees$fieldtree, id==fld_id)$name
    }

    flt$trees$kvmaps <- get_kvmaps(flt)
    tr <- flt$trees$kvmaps
    kmap <- subset(tr, (ems_id==flt$ems_id) & (id==fld_id))

    if (nrow(kmap)==0) {
      cat(sprintf("%s: Getting key-value mappings from API. (Caution: Some fields take a very long time)\n", fld_name))
      r <- request(flt$connection,
                   uri_keys = c('database', 'field'),
                   uri_args = c(flt$ems_id, flt$db_id, fld_id))
      km <- httr::content(r)$discreteValues
      kmap <- data.frame(ems_id=flt$ems_id,
                         id=fld_id,
                         key=as.integer(names(km)),
                         value=unlist(km, use.names = F), stringsAsFactors=F)
      flt$trees$kvmaps <- rbind(flt$trees$kvmaps, kmap)
      save_kvmaps(flt)
    }

    # Return the updated flight object
    flt
  }

#' List all values for a discrete field
#'
#' @description
#' Returns all possible values for a discrete field.
#'
#' @param flt A Flight object
#' @param field Character string specifying the field name (optional if field_id is provided)
#' @param field_id Character string specifying the field ID (optional if field is provided)
#' @param in_vec Logical, if TRUE returns a named vector
#' @param in_df Logical, if TRUE returns a data frame with key and value columns
#'
#' @return A vector of values, named vector, or data frame depending on parameters
#'
#' @export
list_allvalues <-
  function(flt, field = NULL, field_id = NULL, in_vec=FALSE, in_df=FALSE)
  {
    # First update the kvmaps if needed and get the updated flight object
    flt <- update_kvmaps_for_field(flt, field, field_id)

    # Now retrieve the field_id (needed if only field name was provided)
    if (is.null(field_id)) {
      fld <- search_fields(flt, field)[[1]]
      fld_id <- fld$id
    } else {
      fld_id <- field_id
    }

    # Get the kvmaps from the updated flight object
    tr <- flt$trees$kvmaps
    kmap <- subset(tr, (ems_id==flt$ems_id) & (id==fld_id))

    # Return the values in the requested format
    if (in_vec) {
      aa <- kmap[,'value']
      names(aa) <- kmap[,'key']
      return(aa)
    }
    if (in_df) {
      return(kmap[, c('key','value')])
    }
    return(kmap$value)
  }

#' @importFrom stringdist stringdist
get_value_id <-
  function(flt, value, field=NULL, field_id=NULL)
  {
    kvmap <- list_allvalues(flt, field = field, field_id = field_id, in_df = T)
    key   <- kvmap[kvmap$value==value, 'key']

    if ( length(key)==0 ) {
      distances <- stringdist::stringdist(kvmap$value, value, method = "osa")
      top_3_distance <- head(sort(distances), 3)
      top_3_closest <- kvmap$value[which(distances %in% top_3_distance)]
      top_3_closest_collapsed <- paste(top_3_closest, collapse = ",  ")
      stop(sprintf("%s could not be found from the list of the field values.\nPerhaps you meant one of the following: %s",
                   value,
                   top_3_closest_collapsed)
           )
    }
    return(as.integer(key))
  }


get_shortest <-
  function(fields)
  {
    if (class(fields)!="data.frame") {
      stop("Input should be a data frame")
    }
    as.list(fields[order(nchar(fields$name))[1], ])
  }


#' Escape special characters in a string for regular expression matching
#'
#' @description
#' Escapes special characters in a string to allow safe use in regular expressions.
#' This is a more robust replacement for the custom treat_spchar function.
#'
#' @param pattern Character string to escape
#'
#' @return Character string with special characters escaped
#'
#' @importFrom utils glob2rx
#' @examples
#' escape_pattern("file.txt")  # escapes the dot
#' escape_pattern("data[2020]") # escapes the brackets
#'
#' @export
escape_pattern <- function(pattern) {
  if (!is.character(pattern)) {
    stop("Pattern must be a character string")
  }

  # First convert to a fixed pattern (escapes all special regex chars)
  escaped <- gsub("([.|()\\^{}+?*$])", "\\\\\\1", pattern)

  # Return the escaped pattern
  return(escaped)
}


lls_to_df <-
  function(lls)
  {
    for (i in 1:length(lls)) {
      if (i==1) {
        dat <- data.frame(lls[[i]], stringsAsFactors = F)
      } else {
        dat <- rbind(dat, lls[[i]])
      }
    }
    dat
  }
