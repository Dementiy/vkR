#' Returns a list of IDs of users who added the specified object to their Likes list
#' 
#' @param type Object type
#' @param owner_id ID of the user, community, or application that owns the object
#' @param item_id Object ID
#' @param page_url URL of the page where the Like widget is installed. Used instead of the item_id parameter
#' @param filter Filters to apply: likes — returns information about all users who liked the object (default); copies — returns information only about users who told their friends about the object 
#' @param friends_only Specifies which users are returned: 1 — to return only the current user's friends; 0 — to return all users (default)
#' @param extended 1 — Specifies whether extended information will be returned. 1 — to return extended information about users and communities from the Likes list; 0 — to return no additional information (default) 
#' @param offset Offset needed to select a specific subset of users
#' @param count Number of user IDs to return (maximum 1000)
#' @param v Version of API
#' @export
likesGetList <- function(type='', owner_id='', item_id='', page_url='', filter='', friends_only='0', extended='', offset='', count='100', skip_own=0, v=getAPIVersion()) {
  query <- queryBuilder('likes.getList',
                        type = type,
                        owner_id = owner_id,
                        item_id = item_id,
                        page_url = page_url,
                        filter = filter,
                        friends_only = friends_only,
                        extended = extended,
                        offset = offset,
                        count = count,
                        skip_own = skip_own,
                        v = v)
  request_delay()
  response <- jsonlite::fromJSON(query)
  response$response
}