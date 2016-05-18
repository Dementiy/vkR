#' Returns a list of user IDs or detailed information about a user's friends
#'
#' @param user_id User ID. By default, the current user ID
#' @param order Sort order (name — by name, hints — by rating)
#' @param list_id ID of the friend list returned by the friends.getLists method to be used as the source. This parameter is taken into account only when the uid parameter is set to the current user ID
#' @param count Number of friends to return
#' @param offset Offset needed to return a specific subset of friends
#' @param fields Profile fields to return
#' @param name_case Case for declension of user name and surname
#' @param flatten Automatically flatten nested data frames into a single non-nested data frame
#' @param v Version of API
#' @examples
#' \dontrun{
#' friends_list <- getFriends(user_id='1', order='name', fields='bdate')
#' friends <- friends_list$items
#' }
#' @export
getFriends <- function(user_id='', order='', list_id='', count='', offset='', fields='', name_case='', flatten=FALSE, v=getAPIVersion()) {
  query <- queryBuilder('friends.get', 
                        user_id = user_id, 
                        order = order, 
                        list_id = list_id, 
                        count = count, 
                        offset = offset, 
                        fields = fields, 
                        name_case = name_case, 
                        v = v)
  request_delay()
  response <- jsonlite::fromJSON(query)
  response <- response$response
  
  if (isTRUE(flatten))
    response$items <- jsonlite::flatten(response$items)
  
  response
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
  body <- list(source_id = source_id, 
               target_uid = target_uid, 
               order = order,
               count = count,
               offset = offset)
  if (length(target_uids) > 1) {
    target_uids <- paste(target_uids, collapse = ",")
    body <- append(body, list(target_uids = target_uids))
    query <- queryBuilder('friends.getMutual', v = v)
  } else {
    query <- queryBuilder('friends.getMutual', target_uids = target_uids, v = v)
  }
  request_delay()
  response <- jsonlite::fromJSON(rawToChar(httr::POST(URLencode(query),
                                                      body = body)$content))
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


#' Returns a list of friends IDs for the specified users
#' 
#' @param user_ids User IDs
#' @param v Version of API
#' @examples
#' \dontrun{
#' friends <- getFriendsFor(sample(x=seq(1:10000000), size=25, replace=FALSE)) %>% getUsersExecute()
#' }
#' @export
#' @export
getFriendsBy25 <- function(user_ids, v=getAPIVersion()) {
  user_ids <- na.omit(user_ids)
  user_ids <- unique(user_ids)
  code <- "var all_friends = {}; var request;"
  for (idx in 1:length(user_ids)) {
    code <- paste(code, "request=API.friends.get({\"user_id\":", user_ids[idx], ", \"v\":", v, "}).items; all_friends.user", user_ids[idx], "=request;", sep = "")
  }
  code <- paste(code, "return all_friends;")
  response <- execute(code)
  if (!is.null(response)) names(response) <- user_ids
  response
  
  class(response) <- c(class(response), "friends.list")
  response
}


#' Returns a list of friends IDs for the specified users
#' 
#' @param users_ids User IDs
#' @param v Version of API
#' @examples
#' \dontrun{
#' friends <- getFriendsFor(sample(x=seq(1:10000000), size=100, replace=FALSE)) %>% getUsersExecute()
#' }
#' @export
getFriendsFor <- function(users_ids, v=getAPIVersion()) {
  users_friends <- list()
  from <- 1
  to <- 25
  repeat {
    users_friends_25 <- getFriendsBy25(users_ids[from:to], v)
    users_friends <- append(users_friends, users_friends_25)
    
    if (to >= length(users_ids))
      break
    
    from <- to + 1
    to <- to + 25
  }
  
  class(users_friends) <- c(class(users_friends), "friends.list")
  users_friends
}