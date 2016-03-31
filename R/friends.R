#' Returns a list of user IDs or detailed information about a user's friends
#'
#' @param user_id User ID. By default, the current user ID
#' @param order Sort order (name — by name, hints — by rating)
#' @param list_id ID of the friend list returned by the friends.getLists method to be used as the source. This parameter is taken into account only when the uid parameter is set to the current user ID
#' @param count Number of friends to return
#' @param offset Offset needed to return a specific subset of friends
#' @param fields Profile fields to return
#' @param name_case Case for declension of user name and surname
#' @param v Version of API
#' @examples
#' \dontrun{
#' friends_list <- getFriends(user_id='1', order='name', fields='bdate')
#' friends <- friends_list$items
#' }
#' @export
getFriends <- function(user_id='', order='', list_id='', count='', offset='', fields='', name_case='', v=getAPIVersion()) {
  query <- queryBuilder('friends.get', 
                        user_id=user_id, 
                        order=order, 
                        list_id=list_id, 
                        count=count, 
                        offset=offset, 
                        fields=fields, 
                        name_case=name_case, 
                        v=v)
  response <- fromJSON(query)
  response$response
}


#' Returns a list of user IDs of the mutual friends of two users
#' 
#' @param source_id ID of the user whose friends will be checked against the friends of the user specified in target_uid
#' @param target_uid ID of the user whose friends will be checked against the friends of the user specified in source_uid
#' @param target_uids List of target uids
#' @param order Sort order
#' @param count Number of mutual friends to return
#' @param offset Offset needed to return a specific subset of mutual friends
#' @param v Version of API
#' @examples
#' \dontrun{
#' mutual_friends <- getMutual(target_uid='1')
#' }
#' @export
getMutual <- function(source_id='', target_uid='', target_uids='', order='', count='', offset='', v=getAPIVersion()) {
  query <- queryBuilder('friends.getMutual', v=v)
  response <- fromJSON(rawToChar(POST(URLencode(query),
                                      body=list(source_id=source_id,
                                                target_uid=target_uid,
                                                target_uids=target_uids,
                                                order=order,
                                                count=count,
                                                offset=offset))$content))
  response$response
}


#' Checks the friendship status between two users
#' 
#' @param source_id Source user ID
#' @param target_id Target user ID
#' @export
areFriends <- function(source_id, target_id)
{
  if (!is.numeric(source_id)) stop('source_id must be positive integer')
  if (!is.numeric(target_id)) stop('target_id must be positive integer')
  source_id %in% getFriends(target_id)$items | target_id %in% getFriends(source_id)$items
}