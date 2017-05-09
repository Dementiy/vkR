#' Returns search results by statuses
#'
#' @param q Search query string (e.g., New Year).
#' @param extended 1 — to return additional information about the user or community that placed the post
#' @param count Number of posts to return
#' @param latitude Geographical latitude point (in degrees, -90 to 90) within which to search
#' @param longitude Geographical longitude point (in degrees, -180 to 180) within which to search
#' @param start_time Earliest timestamp (in Unix time) of a news item to return. By default, 24 hours ago
#' @param end_time Latest timestamp (in Unix time) of a news item to return. By default, the current time
#' @param start_from String, accessible for versions from 5.13
#' @param fields Additional fields of profiles and communities to return
#' @param v Version of API
#' @export
newsfeedSearch <- function(q='', extended='', count='', latitude='', longitude='', start_time='', end_time='', start_from='', fields='', v = getAPIVersion()) {
  query <- queryBuilder('newsfeed.search',
                        q = q,
                        extended = extended,
                        count = count,
                        latitude = latitude,
                        longitude = longitude,
                        start_time = start_time,
                        end_time = end_time,
                        start_from = start_from,
                        fields = fields,
                        v = v)
  request_delay()
  response <- jsonlite::fromJSON(URLencode(query))

  if (has_error(response))
    return(try_handle_error(response))

  response$response
}
