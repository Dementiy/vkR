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
#' @param fields 
#' @param v Version of API
#' getWall()
#' @return Returns a list of post objects.
#' If extended is set to 1, also returns the following:
#' \itemize{
#' \item \strong{wall} — Contains a list of post objects.
#' \item \strong{profiles} — Contains user objects with additional fields photo and online.
#' \item \strong{groups} — Contains community objects.
#' }
#' @export
getWall <- function(owner_id='', domain='', offset='', count='', filter='owner', extended='', fields='', v=getAPIVersion()) {
  query <- queryBuilder('wall.get',
                        owner_id = owner_id,
                        domain = domain,
                        offset = offset,
                        count = count,
                        filter = filter,
                        extended = extended,
                        fields = fields,
                        v = v)
  request_delay()
  response <- jsonlite::fromJSON(query)
  response$response
}


#' Returns a list of posts on a user wall or community wall
#'
#' @param owner_id ID of the user or community that owns the wall. By default, current user ID. Use a negative value to designate a community ID.
#' @param domain User or community short address.
#' @param offset Offset needed to return a specific subset of posts.
#' @param count Number of posts to return (0 for all posts).
#' @param filter Filter to apply:
#' \itemize{
#' \item \strong{owner} — posts by the wall owner;
#' \item \strong{others} — posts by someone else;
#' \item \strong{all} — posts by the wall owner and others (default);
#' \item \strong{postponed} — timed posts (only available for calls with an access_token);
#' \item \strong{suggests} — suggested posts on a community wall.
#' }
#' @param extended 1 — to return wall, profiles, and groups fields, 0 — to return no additional fields (default).
#' @param fields 
#' @param progress_bar Display progress bar
#' @param v Version of API
#' getWallExecute()
#' @return Returns a list of post objects.
#' If extended is set to 1, also returns the following:
#' \itemize{
#' \item \strong{wall} — Contains a list of post objects.
#' \item \strong{profiles} — Contains user objects with additional fields photo and online.
#' \item \strong{groups} — Contains community objects.
#' }
#' @export
getWallExecute <- function(owner_id='', domain='', offset=0, count=10, filter='owner', extended='', fields='', progress_bar=FALSE, v=getAPIVersion())
{
  get_posts2500 <- function(owner_id='', domain='', offset=0, max_count='', filter='owner', extended='', fields='', v=getAPIVersion())
  {
    if (max_count > 2500) 
      max_count <- 2500
    if (max_count <= 100) {
      execute(paste0('return API.wall.get({"owner_id":"', owner_id, '", 
                     "domain":"', domain, '",
                     "offset":"', offset, '",
                     "count":"', max_count, '",
                     "filter":"', filter, '",
                     "extended":"', extended, '",
                     "v":"', v, '"}).items;'))
    } else {
      code <- 'var wall_records = [];'
      code <- paste0(code, 'wall_records = wall_records + API.wall.get({"owner_id":"', owner_id, '", 
                     "domain":"', domain, '",
                     "offset":"', offset, '",
                     "count":"', 100, '",
                     "filter":"', filter, '",
                     "extended":"', extended, '",
                     "v":"', v, '"}).items;')
      code <- paste0(code, 'var offset = 100 + ', offset, ';
                     var count = 100; var max_offset = offset + ', max_count, ';
                     while (offset < max_offset && wall_records.length <= offset && offset-', offset, '<', max_count, ') {
                       if (', max_count, ' - wall_records.length < 100) {
                        count = ', max_count, ' - wall_records.length;
                       };
                       wall_records = wall_records + API.wall.get({"owner_id":"', owner_id, '", 
                         "domain":"', domain, '",
                         "offset":offset,
                         "count":count,
                         "filter":"', filter, '",
                         "extended":"', extended, '",
                         "v":"', v, '"}).items;
                       offset = offset + 100;
                     };
                     return wall_records;')
      execute(code)
    }
  }
  
  code <- paste0('return API.wall.get({"owner_id":"', owner_id, '", 
                 "domain":"', domain, '",
                 "offset":"', offset, '",
                 "count":"', 1, '",
                 "filter":"', filter, '",
                 "extended":"', extended, '",
                 "v":"', v, '"});')
  response <- execute(code)
  
  posts <- response$items
  max_count <- ifelse((response$count - offset) > count & count != 0, count, response$count - offset)
  
  if (max_count == 0)
    return(list(posts = response$items, 
                count = response$count))
  
  offset_counter <- 0
  
  if (progress_bar) {
    pb <- txtProgressBar(min = 0, max = max_count, style = 3)
    setTxtProgressBar(pb, nrow(posts))
  }
  
  while (nrow(posts) < max_count) {
    posts2500 <- get_posts2500(owner_id = owner_id,
                               domain = domain,
                               filter = filter,
                               extended = extended,
                               fields = fields,
                               max_count = (max_count - nrow(posts)), 
                               offset = (1 + offset + offset_counter * 2500), 
                               v = v)
    posts <- jsonlite::rbind.pages(list(posts, posts2500))
    
    if (progress_bar)
      setTxtProgressBar(pb, nrow(posts))
    
    offset_counter <- offset_counter + 1
  }
  
  if (progress_bar)
    close(pb)
  
  list(posts = posts, 
       count = response$count)
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
#' @param fields
#' @param v Version of API
#' @return If executed successfully, returns a list of post objects.
#' @export
wallSearch <- function(owner_id='', domain='', query='', owners_only='', count='20', offset='0', extended='', fields='', v=getAPIVersion()) {
  query <- queryBuilder('wall.search',
                        owner_id = owner_id,
                        domain = domain,
                        query = query,
                        owners_only = owners_only,
                        count = count,
                        offset = offset,
                        extended = extended,
                        fields = fields,
                        v = v)
  request_delay()
  response <- jsonlite::fromJSON(query)
  response$response
}


#' Returns a list of posts from user or community walls by their IDs
#' 
#' @param posts User or community IDs and post IDs, separated by underscores. Use a negative value to designate a community ID.
#' @param extended 1 — to return user and community objects needed to display posts, 0 — no additional fields are returned (default).
#' @param copy_history_depth Sets the number of parent elements to include in the array copy_history that is returned if the post is a repost from another wall.
#' @param fields
#' @param v Version of API
#' @return Returns a list of post objects. 
#' If extended is set to 1, returns the following:
#' \itemize{
#' \item \strong{wall} — Contains post objects.
#' \item \strong{profiles} — Contains user objects with additional fields sex, photo, photo_medium_rec, and online.
#' \item \strong{groups} — Contains community objects.
#' }
#' If the post is a copy of another post, returns an additional array copy_history with information about original posts.
#' @export
wallGetById <- function(posts='', extended='', copy_history_depth='', fields='', v=getAPIVersion()) {
  query <- queryBuilder('wall.getById',
                        posts = posts,
                        extended = extended,
                        copy_history_depth = copy_history_depth,
                        fields = fields,
                        v = v)
  request_delay()
  response <- jsonlite::fromJSON(query)
  response$response
}


#' Returns information about reposts of a post on user wall or community wall
#' 
#' @param owner_id User ID or community ID. By default, current user ID. Use a negative value to designate a community ID.
#' @param post_id Post ID.
#' @param offset Offset needed to return a specific subset of reposts.
#' @param count Number of reposts to return.
#' @param v Version of API
#' @return
#' Returns an object containing the following fields:
#' \itemize{
#' \item \strong{items} — An array of wall reposts.
#' \item \strong{profiles} — Information about users with additional fields sex, online, photo, photo_medium_rec, and screen_name.
#' \item \strong{groups} — Information about communities.
#' }
#' @export
wallGetReposts <- function(owner_id='', post_id='', offset='', count='20', v=getAPIVersion()) {
  query <- queryBuilder('wall.getReposts',
                        owner_id = owner_id,
                        post_id = post_id,
                        offset = offset,
                        count = count,
                        v = v)
  request_delay()
  response <- jsonlite::fromJSON(query)
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
#' @param v Version of API
#' @export
wallGetComments <- function(owner_id='', post_id='', need_likes='', start_comment_id='', offset='', count='10', sort='', preview_length='0', extended='', v=getAPIVersion()) {
  query <- queryBuilder('wall.getComments',
                        owner_id = owner_id,
                        post_id = post_id,
                        need_likes = need_likes,
                        start_comment_id = start_comment_id,
                        offset = offset,
                        count = count,
                        sort = sort,
                        preview_length = preview_length,
                        extended = extended,
                        v = v)
  request_delay()
  response <- jsonlite::fromJSON(query)
  response$response
}


#' Filtering attachments by type
#' 
#' @param attachments List of attachments
#' @param type type field may have the following values:
#' \itemize{
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
          filtered_attachments <- plyr::rbind.fill(filtered_attachments, attachments[[i]][j, ][[type]])
        }
      }
    }
  }
  filtered_attachments
}