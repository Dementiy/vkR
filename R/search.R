#' Allows the programmer to do a quick search for any substring
#' @param q Search query string
#' @param limit Maximum number of results to return
#' @param filters
#' @param search_global
#' @param v Version of API
#' @export
search.getHints <- function(q='', limit='', filters='', search_global='', v=getAPIVersion()) {
  query <- queryBuilder('search.getHints',
                        q = q,
                        limit = limit,
                        filters = filters,
                        search_global = search_global,
                        v = v)
  request_delay()
  response <- jsonlite::fromJSON(URLencode(query))
  response$response
}