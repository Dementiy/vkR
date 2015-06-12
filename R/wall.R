#' Returns a list of posts on a user wall or community wall
#'
#' @param owner_id ID of the user or community that owns the wall. By default, current user ID. Use a negative value to designate a community ID.
#' @param domain User or community short address.
#' @param offset Offset needed to return a specific subset of posts.
#' @param count Number of posts to return (maximum 100).
#' @param filter Filter to apply:
#' \itemize{
#' \item \strong{owner} — posts by the wall owner;
#' \item \strong{others} — posts by someone else;
#' \item \strong{all} — posts by the wall owner and others (default);
#' \item \strong{postponed} — timed posts (only available for calls with an access_token);
#' \item \strong{suggests} — suggested posts on a community wall.
#' }
#' @param extended 1 — to return wall, profiles, and groups fields, 0 — to return no additional fields (default).
#' getWall()
#' @return Returns a list of post objects.
#' If extended is set to 1, also returns the following:
#' \itemize{
#' \item \strong{wall} — Contains a list of post objects.
#' \item \strong{profiles} — Contains user objects with additional fields photo and online.
#' \item \strong{groups} — Contains community objects.
#' }
#' @export
getWall <- function(owner_id='', domain='', offset='', count='', filter='owner', extended='', v='5.33') {
  query <- queryBuilder('wall.get',
                        owner_id=owner_id,
                        domain=domain,
                        offset=offset,
                        count=count,
                        filter=filter,
                        extended=extended,
                        v=v)
  response <- fromJSON(query)
  response$response
}


#' Allows to search posts on user or community walls
#' 
#' @param owner_id User or community id. Remember that for a community owner_id must be negative.
#' @param domain User or community screen name.
#' @param query Search query string.
#' @param owners_only 1 – returns only page owner's posts.
#' @param count Count of posts to return.
#' @param offset Results offset.
#' @param extended Show extended post info.
#' @return If executed successfully, returns a list of post objects.
#' @export
wallSearch <- function(owner_id='', domain='', query='', owners_only='', count='20', offset='0', extended='') {
  query <- queryBuilder('wall.search',
                        owner_id=owner_id,
                        domain=domain,
                        query=query,
                        owners_only=owners_only,
                        count=count,
                        offset=offset,
                        extended=extended)
  response <- fromJSON(query)
  response$response
}


#' Returns a list of posts from user or community walls by their IDs
#' 
#' @param posts User or community IDs and post IDs, separated by underscores. Use a negative value to designate a community ID.
#' @param extended 1 — to return user and community objects needed to display posts, 0 — no additional fields are returned (default).
#' @param copy_history_depth Sets the number of parent elements to include in the array copy_history that is returned if the post is a repost from another wall.
#' @return Returns a list of post objects. 
#' If extended is set to 1, returns the following:
#' \itemize{
#' \item \strong{wall} — Contains post objects.
#' \item \strong{profiles} — Contains user objects with additional fields sex, photo, photo_medium_rec, and online.
#' \item \strong{groups} — Contains community objects.
#' }
#' If the post is a copy of another post, returns an additional array copy_history with information about original posts.
#' @export
wallGetById <- function(posts='', extended='', copy_history_depth='') {
  query <- queryBuilder('wall.getById',
                        posts=posts,
                        extended=extended,
                        copy_history_depth=copy_history_depth)
  response <- fromJSON(query)
  response$response
}


#' Returns information about reposts of a post on user wall or community wall
#' 
#' @param owner_id User ID or community ID. By default, current user ID. Use a negative value to designate a community ID.
#' @param post_id Post ID.
#' @param offset Offset needed to return a specific subset of reposts.
#' @param count Number of reposts to return.
#' @return
#' Returns an object containing the following fields:
#' \itemize{
#' \item \strong{items} — An array of wall reposts.
#' \item \strong{profiles} — Information about users with additional fields sex, online, photo, photo_medium_rec, and screen_name.
#' \item \strong{groups} — Information about communities.
#' }
#' @export
wallGetReposts <- function(owner_id='', post_id='', offset='', count='20') {
  query <- queryBuilder('wall.getReposts',
                        owner_id=owner_id,
                        post_id=post_id,
                        offset=offset,
                        count=count)
  response <- fromJSON(query)
  response$response
}


#' Returns a list of comments on a post on a user wall or community wall
#' 
#' @param owner_id User ID or community ID. Use a negative value to designate a community ID.
#' @param post_id Post ID.
#' @param need_likes 1 — to return the likes field, 0 — not to return the likes field (default).
#' @param offset Offset needed to return a specific subset of comments.
#' @param count Number of comments to return (maximum 100).
#' @param sort Sort order: asc — chronological, desc — reverse chronological. 
#' @param preview_length Number of characters at which to truncate comments when previewed. By default, 90. Specify 0 if you do not want to truncate comments.
#' @param extended Flag, either 1 or 0.
#' @export
wallGetComments <- function(owner_id='', post_id='', need_likes='', offset='', count='10', sort='', preview_length='0', extended='') {
  query <- queryBuilder('wall.getComments',
                        owner_id=owner_id,
                        post_id=post_id,
                        offset=offset,
                        count=count,
                        sort=sort,
                        preview_length=preview_length,
                        extended=extended)
  response <- fromJSON(query)
  response$response
}


#' Filtering attachments by type
#' 
#' @param attachments List of attachments
#' @param type type field may have the following values:
#' \itemize {
#' \item \strong{photo} - photo from an album;
#' \item \strong{posted_photo} - photo uploaded directly from user's computer;
#' \item \strong{video} - video;
#' \item \strong{audio} - audio;
#' \item \strong{doc} - document;
#' \item \strong{graffiti} - graffiti;
#' \item \strong{url} - web page URL;
#' \item \strong{note} - note;
#' \item \strong{app} - image uploaded with a third party application;
#' \item \strong{poll} - poll;
#' \item \strong{page} - wiki page.
#' }
#' @export
filterAttachments <- function(attachments, type) {
  if (!requireNamespace("plyr", quietly = TRUE)) {
    stop("plyr package needed for this function to work. Please install it.", .call = FALSE)
  }
  if (!is.character(type)) stop('type must be a character')
  
  filtered_attachments <- data.frame()
  for (i in 1:length(attachments)) {
    if (!is.null(attachments[[i]])) {
      for (j in 1:nrow(attachments[[i]])) {
        if (attachments[[i]][j, ]$type == type) {
          filtered_attachments <- rbind.fill(filtered_attachments, attachments[[i]][j, ][[type]])
        }
      }
    }
  }
  filtered_attachments
}


getAllWall <- function(owner_id) {
  all_wall <- c()
  offset_counter <- 0
  repeat {
    wall100 <- getWall(owner_id = owner_id, count = '100', offset = as.character(offset_counter * 100))$items

    all_wall <- c(all_wall, wall100$text)
    if (nrow(wall100) < 100)
      break
    
    offset_counter <- offset_counter + 1
    if (offset_counter %% 3 == 0)
      Sys.sleep(1.0)
  }
  all_wall
}