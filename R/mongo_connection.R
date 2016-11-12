#' Configure database
#' @export
db_configure <- function(db_name) {
  if (!require("mongolite")) stop("The package mongolite was not installed")
  .vkr$active_db <- db_name
}


#' The current database name
#' @export
db_getName <- function() {
  return(.vkr$active_db)
}


#' Switch database
#' @export
use_db <- function(db_name) {
  .vkr$active_db <- db_name
}


#' Show databases
#' @export
show_dbs <- function() {
  return(names(.vkr$dbs))
}


#' Show collections
#' @export
show_collections <- function() {
  return(.vkr$dbs[[.vkr$active_db]])
}


#' Get connection
#' @export
get_connection <- function(collection_type, collection_name) {
  db_name <- db_getName()
  if (is.null(.vkr$dbs[[db_name]][[collection_type]])) {
    .vkr$dbs[[db_name]][[collection_type]] <- list()
    .vkr$dbs[[db_name]][[collection_type]] <- collection_name
  } else {
    .vkr$dbs[[db_name]][[collection_type]] <- unique(c(unlist(.vkr$dbs[[db_name]][[collection_type]]), collection_name))
  }
  connection <- mongolite::mongo(collection = collection_name, db = db_name)
  return(connection)
}