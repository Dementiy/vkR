#' Allows the programmer to do a quick search for any substring
#' @param q Search query string
#' @param limit Maximum number of results to return
#' @param filters List of comma-separated words
#' @param search_global Flag, either 1 or 0, default 1
#' @param v Version of API
#' @importFrom utils URLencode
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

  if (has_error(response))
    return(try_handle_error(response))

  response$response
}
