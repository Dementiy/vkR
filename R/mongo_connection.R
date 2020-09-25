#' Initialize database
#' @param db_name Database name ('temp' by default)
#' @param verbose Emit some more output
#' @export
db_init <- function(db_name = "temp", verbose = FALSE) {
  if (!requireNamespace("mongolite", quietly = TRUE))
    stop("The package mongolite was not installed")

  conn <- mongolite::mongo(db = .vkr$db_name, collection = .vkr$db_meta_name, verbose = verbose)
  .vkr$db_metadata <- conn
  .vkr$db_active <- db_name
}


#' Get a mongo connection object
#' @param collection_name Collection name
#' @param collection_suffix Collection suffix
#' @param db_name Database name
#' @export
db_get_connection <- function(collection_name, collection_suffix = '', db_name = db_getActive()) {
  if (missing(collection_name)) stop('argument "collection_name" is missing, with no default')

  meta_conn <- db_metaConnection()
  dbs <- meta_conn$find()

  s <- dbs$db == db_name & dbs$collection == collection_name & dbs$suffix == collection_suffix
  dbs <- subset(dbs, s)

  if (nrow(dbs) == 0)
    stop("Collection doesn't exist", call. = FALSE)
  if (nrow(dbs) > 1)
    stop("Oops, something has gone wrong. See show_collections()", call. = FALSE)

  conn <- mongolite::mongo(db = db_getName(),
                           collection = paste(dbs$db, dbs$collection, dbs$suffix, sep = '.'))
  return(conn)
}


#' Create empty collection
#' @param collection Collection name
#' @param suffix Collection suffix
#' @param db_name Database name
#' @export
create_empty_collection <- function(collection, suffix, db_name = db_getActive()) {
  meta_conn <- db_metaConnection()
  meta_conn$insert(data.frame(db = db_name, collection = collection, suffix = suffix))
}


#' Get collection
#' @param collection_name Collection name
#' @param collection_suffix Collection suffix
#' @param db_name Database name
#' @export
db_get_collection <- function(collection_name, collection_suffix = '', db_name = db_getActive()) {
  tryCatch({
    conn <- db_get_connection(collection_name, collection_suffix, db_name)
    collection_data <- conn$find()
    rm(conn)
    return(collection_data)
  }, error = function(e) {
    return(NULL)
  })
}


#' Check if collection exists
#' @param collection_name Collection name
#' @param collection_suffix Collection suffix
#' @param db_name Database name
#' @export
collection_exists <- function(collection_name, collection_suffix = '', db_name = db_getActive()) {
  tryCatch({
    db_get_connection(collection_name, collection_suffix, db_name)
    return(TRUE)
  }, error = function(e) {
    return(FALSE)
  })
}


#' Load collection from db
#' @param collection_name Collection name
#' @param collection_suffix Collection suffix
#' @param db_name Database name
#' @export
db_load_collection <- function(collection_name, collection_suffix = '', db_name = db_getActive()) {
  collection <- db_get_collection(collection_name, collection_suffix, db_name)
  if (is.null(collection)) stop("Collection doesn't exist", call. = FALSE)
  # assign(paste(db_name, collection_name, collection_suffix, sep = '.'),
  #        collection, envir = .GlobalEnv)
  return(collection)
}


#' Save object to db
#' @param object Object to save
#' @param collection Collection name
#' @param suffix Collection suffix
#' @param db_name Database name
#' @export
db_save <- function(object, collection, suffix = '', db_name = db_getActive()) {
  if (missing(object)) stop('argument "object" is missing, with no default')
  if (missing(collection)) stop('argument "collection" is missing, with no default')

  # If collection already exists
  conn <- mongolite::mongo(db = db_getName(),
                           collection = paste(db_name, collection, suffix, sep = '.'))
  if (conn$count() > 0) {
    return(conn)
  }
  rm(conn)

  # Add record about new collection to meta
  meta_conn <- db_metaConnection()
  meta_conn$insert(data.frame(db = db_name, collection = collection, suffix = suffix))

  # Save new collection
  conn <- mongolite::mongo(db = db_getName(),
                           collection = paste(db_name, collection, suffix, sep = '.'))
  conn$insert(object)
  return(conn)
}


#' Insert object into existing collection
#' @param object Object to insert
#' @param db_name Database name
#' @param collection Collection name
#' @param suffix Collection suffix
#' @export
db_insert <- function(object, collection, suffix, db_name = db_getActive()) {
  if (missing(object)) stop('argument "object" is missing, with no default')
  if (missing(collection)) stop('argument "collection" is missing, with no default')

  conn <- mongolite::mongo(db = db_getName(),
                           collection = paste(db_name, collection, suffix, sep = '.'))
  conn$insert(object)
  rm(conn)
}


#' Update existing records
#' @param object Object to insert
#' @param key Key
#' @param db_name Database name
#' @param collection Collection name
#' @param suffix Collection suffix
#' @param upsert Insert a new document if no matching document exists
#' @export
db_update <- function(object, key, collection, suffix = '', db_name = db_getActive(), upsert = FALSE) {
  if (missing(object)) stop('argument "object" is missing, with no default')
  if (missing(key)) stop('argument "key" is missing, with no default')
  if (missing(collection)) stop('argument "collection" is missing, with no default')

  conn <- mongolite::mongo(db = db_getName(),
                           collection = paste(db_name, collection, suffix, sep = '.'))

  # TODO: Object must be a data frame, key must be an integer value
  for (i in 1:nrow(object)) {
    query <- sprintf('{"%s":%s}', key, object[i, key])
    json <- jsonlite::toJSON(object[i,])
    update <- sprintf('{"$set":%s}', substr(json, 2, nchar(json)-1)) # drop brackets
    conn$update(query = query, update = update, upsert = upsert)
  }

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
  if (nrow(dbs_list) != 0) {
    active <- dbs_list$db == db_getActive()
    dbs_list <- subset(dbs_list, active)
    dbs_list$count <- rep(0, nrow(dbs_list))
  }
  for (i in 1:nrow(dbs_list)) {
    conn <- db_get_connection(collection_name = dbs_list[i,]$collection,
                              collection_suffix = dbs_list[i,]$suffix,
                              db_name = dbs_list[i,]$db)
    dbs_list[i,]$count <- conn$count()
    rm(conn)
  }
  print(dbs_list)
}


#' Load all collections from db for specified data base
#' @param db_name Database name
#' @export
db_load <- function(db_name = db_getActive()) {
  meta_conn <- db_metaConnection()
  dbs <- meta_conn$find()
  s <- dbs$db == db_name
  dbs <- subset(dbs, s)
  collections <- paste(dbs$db, dbs$collection, dbs$suffix, sep = '.')
  collections_list <- list()
  for (collection_name in collections) {
    conn <- mongolite::mongo(db = db_getName(),
                             collection = collection_name)
    # assign(collection_name, conn$find(), envir = .GlobalEnv)
    collections_list[[collection_name]] <- conn$find()
    rm(conn)
  }
  gc(verbose = FALSE)
  return(collections_list)
}


#' Drop collection
#' @param collection_name Collection name
#' @param collection_suffix Collection suffix
#' @param db_name Database name
#' @export
db_drop_collection <- function(collection_name, collection_suffix = '', db_name = db_getActive()) {
  if (missing(collection_name)) stop('argument "collection_name" is missing, with no default')

  conn <- db_get_connection(collection_name, collection_suffix, db_name)
  conn$drop()
  rm(conn)

  meta_conn <- db_metaConnection()
  meta_conn$remove(sprintf('{"db": "%s", "collection": "%s", "suffix": "%s"}',
                           db_name, collection_name, collection_suffix))
  gc(verbose = FALSE)
}


#' Drop database
#' @param db_name Database name
#' @export
db_drop <- function(db_name) {
  if (missing(db_name)) stop('argument "db_name" is missing, with no default')

  meta_conn <- db_metaConnection()
  dbs <- meta_conn$find()
  s <- dbs$db == db_name
  dbs <- subset(dbs, s)

  if (nrow(dbs) == 0)
    stop("Database doesn't exist", call. = FALSE)

  collections <- unique(paste(dbs$db, dbs$collection, dbs$suffix, sep = '.'))
  for (collection_name in collections) {
    conn <- mongolite::mongo(db = db_getName(),
                             collection = collection_name)
    conn$drop()
    rm(conn)
  }
  meta_conn$remove(sprintf('{"db": "%s"}', db_name), multiple = TRUE)
  gc(verbose = FALSE)
}
