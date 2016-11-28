#' Returns a list of IDs of users who added the specified object to their Likes list
#'
#' @param type Object type
#' @param owner_id ID of the user, community, or application that owns the object
#' @param item_id Object ID
#' @param page_url URL of the page where the Like widget is installed. Used instead of the item_id parameter
#' @param filter Filters to apply: likes - returns information about all users who liked the object (default); copies - returns information only about users who told their friends about the object
#' @param friends_only Specifies which users are returned: 1 - to return only the current user's friends; 0 - to return all users (default)
#' @param skip_own Flag, either 1 or 0
#' @param extended Specifies whether extended information will be returned. 1 - to return extended information about users and communities from the Likes list; 0 - to return no additional information (default)
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

  if (has_error(response))
    return(try_handle_error(response))

  response$response
}


#' Returns a list of IDs of users who added the specified objects to their Likes list
#'
#' @param objects List of objects (objects must contain fields owner_id and id)
#' @param type Object type (post or comment)
#' @param filter Filters to apply: likes - returns information about all users who liked the object (default); copies - returns information only about users who told their friends about the object
#' @param friends_only Specifies which users are returned: 1 - to return only the current user's friends; 0 - to return all users (default)
#' @param extended Specifies whether extended information will be returned. 1 - to return extended information about users and communities from the Likes list; 0 - to return no additional information (default)
#' @param skip_own flag, either 1 or 0
#' @param progress_bar Display progress bar
#' @param v Version of API
#' @examples
#' \dontrun{
#' wall <- getWallExecute(domain = 'privivkanet', count = 10, progress_bar = TRUE)
#' post_likers <- likesGetListForObjects(wall, type = 'post', progress_bar = TRUE)
#' post_likers_extended <- likesGetListForObjects(wall, type = 'post',
#'    extended = 1, progress_bar = TRUE)
#' }
#' @importFrom utils setTxtProgressBar txtProgressBar
#' @export
likesGetListForObjects <- function(objects, type = 'post', filter = 'likes', friends_only = 0, extended = 0, skip_own = 0, progress_bar = FALSE, v = getAPIVersion()) {
  get_likes <- function(objects, type = 'post', filter = 'likes', friends_only = 0, extended = 0, skip_own = 0, v = getAPIVersion()) {
    num_requests <- ceiling(nrow(objects) / 25)
    from <- 1
    to <- 25
    likes <- list()
    for (i in 1:num_requests) {
      code <- 'var likes_per_object = {}; var likes;'
      if (to > nrow(objects))
        to <- nrow(objects)
      for (index in from:to) {
        owner_id <- objects[index, ]$owner_id
        obj_id <- objects[index, ]$id

        code <- paste0(code, 'likes = API.likes.getList({
                       "type":"', type, '",
                       "owner_id":"', owner_id, '",
                       "item_id":"', obj_id, '",
                       "page_url":"', '', '",
                       "filter":"', filter, '",
                       "friends_only":"', friends_only, '",
                       "extended":"', extended, '",
                       "skip_own":"', skip_own, '",
                       "count":"', 1000, '",
                       "v":"', v, '"}).items;
                       likes_per_object.obj', obj_id, "=likes;", sep = "")
      }
      code <- paste0(code, 'return likes_per_object;')
      likes <- append(likes, execute(code))
      from <- from + 25
      to <- to + 25
    }
    names(likes) <- objects$id
    likes
  }

  if ("posts.list" %in% class(objects))
    objects <- objects$posts

  likes <- list()
  from <- 1
  max_count <- nrow(objects)
  to <- ifelse(max_count >= 75, 75, max_count)

  if (progress_bar) {
    pb <- txtProgressBar(min = 0, max = nrow(objects), style = 3)
    setTxtProgressBar(pb, 0)
  }

  repeat {
    likes75 <- get_likes(objects[from:to, ],
                         type = type,
                         filter = filter,
                         friends_only = friends_only,
                         extended = extended,
                         skip_own = skip_own,
                         v = v)
    likes <- append(likes, likes75)

    if (progress_bar)
      setTxtProgressBar(pb, length(likes))

    if (to >= max_count)
      break

    from <- to + 1
    to <- ifelse(to + 75 >= max_count, max_count, to + 75)
  }

  if (progress_bar)
    close(pb)

  class(likes) <- c(class(likes), paste0("vk.likes_per_", type))
  likes
}
