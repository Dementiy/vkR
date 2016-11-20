#' Initialize database
#' @param db_name Database name ('temp' by default)
#' @export
db_init <- function(db_name = "temp") {
  if (!require("mongolite")) stop("The package mongolite was not installed")
  
  conn <- mongolite::mongo(db = .vkr$db_name, collection = .vkr$db_meta_name)
  .vkr$db_metadata <- conn
  .vkr$db_active <- db_name
}


#' Get connection
#' @param collection_name Collection name
#' @param collection_type Collection suffix
#' @export
db_get_connection <- function(collection_name, collection_type) {
  meta_conn <- db_metaConnection()
  dbs <- meta_conn$find()
  dbs <- subset(dbs, db == db_getActive() & collection == collection_name & type == collection_type)
  
  if (nrow(dbs) == 0)
    stop("Collection doesn't exist", call. = FALSE)
  if (nrow(dbs) > 1)
    stop("Oops, something has gone wrong. See show_collections()", call. = FALSE)
  
  conn <- mongolite::mongo(db = db_getName(),
                           collection = paste(dbs$db, dbs$collection, dbs$type, sep = '.'))
  return(conn)
}


#' Get collection
#' @param collection_name Collection name
#' @param collection_type Collection suffix
#' @export
db_get_collection <- function(collection_name, collection_type) {
  tryCatch({
    conn <- db_get_connection(collection_name, collection_type)
    collection_data <- conn$find()
    rm(conn)
    return(collection_data)
  }, error = function(e) {
    return(NULL)
  })
}


#' Import collection from db to global env
#' @param collection_name Collection name
#' @param collection_type Collection suffix
#' @export
db_load_collection <- function(collection_name, collection_type) {
  collection <- db_get_collection(collection_name, collection_type)
  if (is.null(collection)) stop("Collection doesn't exist", call. = FALSE)
  assign(paste(db_getActive(), collection_name, collection_type, sep = '.'),
         collection, envir = .GlobalEnv)
}


#' Save object to db
#' @param object Object to save
#' @param db_name Database name
#' @param collection Collection name
#' @param type Collection suffix
#' @export
db_save <- function(object, db_name = db_getActive(), collection, type) {
  if (missing(object)) stop('argument "object" is missing, with no default')
  if (missing(collection)) stop('argument "collection" is missing, with no default')
  if (missing(type)) stop('argument "type" is missing, with no default')
  
  # If collectoin already exists
  conn <- mongolite::mongo(db = db_getName(),
                           collection = paste(db_name, collection, type, sep = '.'))
  if (conn$count() > 0) {
    return(conn)
  }
  rm(conn)
  
  # Add record about new collection to meta
  meta_conn <- db_metaConnection()
  meta_conn$insert(data.frame(db = db_name, collection = collection, type = type))
  
  # Save new collection
  conn <- mongolite::mongo(db = db_getName(),
                           collection = paste(db_name, collection, type, sep = '.'))
  conn$insert(object)
  return(conn)
}


#' Get meta connection
db_metaConnection <- function() {
  return(.vkr$db_metadata)
}


#' The current database name
#' @export
db_getActive <- function() {
  return(.vkr$db_active)
}


#' The current database name
#' @export
db_getName <- function() {
  return(.vkr$db_name)
}


#' Switch database
#' @param db_name Database name
#' @export
use_db <- function(db_name) {
  .vkr$db_active <- db_name
}


#' Show databases
#' @export
show_dbs <- function() {
  meta_conn <- db_metaConnection()
  print(unique(meta_conn$find()$db))
}


#' Show collections
#' @export
show_collections <- function() {
  meta_conn <- db_metaConnection()
  dbs_list <- meta_conn$find()
  dbs_list <- subset(dbs_list, db == db_getActive())
  print(dbs_list)
}


#' Load all collections from db for specified data base
#' @param db_name Database name
#' @export
db_load <- function(db_name = db_getActive()) {
  meta_conn <- db_metaConnection()
  dbs <- meta_conn$find()
  dbs <- subset(dbs, db == db_name)
  collections <- paste(dbs$db, dbs$collection, dbs$type, sep = '.')
  for (collection_name in collections) {
    conn <- mongolite::mongo(db = db_getName(),
                             collection = collection_name)
    assign(collection_name, conn$find(), envir = .GlobalEnv)
    rm(conn)
  }
  gc(verbose = FALSE)
}


#' Drop collection
#' @param collection_name Collection name
#' @param collection_type Collection suffix
#' @export
db_drop_collection <- function(collection_name, collection_type) {
  if (missing(collection_name)) stop('argument "collection_name" is missing, with no default')
  if (missing(collection_type)) stop('argument "collection_type" is missing, with no default')
  
  conn <- db_get_connection(collection_name, collection_type)
  conn$drop()
  rm(conn)
  meta_conn$remove(sprintf('{"db": "%s", "collection": "%s", "type": "%s"}', 
                           dbs$db, dbs$collection, dbs$type))
  gc(verbose = FALSE)
}


#' Drop database
#' @param db_name Database name
#' @export
db_drop <- function(db_name) {
  if (missing(db_name)) stop('argument "db_name" is missing, with no default')
  
  meta_conn <- db_metaConnection()
  dbs <- meta_conn$find()
  dbs <- subset(dbs, db == db_name)
  
  if (nrow(dbs) == 0)
    stop("Database doesn't exist", call. = FALSE)
  
  collections <- unique(paste(dbs$db, dbs$collection, dbs$type, sep = '.'))
  for (collection_name in collections) {
    conn <- mongolite::mongo(db = db_getName(),
                             collection = collection_name)
    conn$drop()
    rm(conn)
  }
  meta_conn$remove(sprintf('{"db": "%s"}', db_name), multiple = TRUE)
  gc(verbose = FALSE)
}