#' Returns a list of posts on a user wall or community wall
#'
#' @param owner_id ID of the user or community that owns the wall. By default, current user ID. Use a negative value to designate a community ID.
#' @param domain User or community short address.
#' @param offset Offset needed to return a specific subset of posts.
#' @param count Number of posts to return (maximum 100).
#' @param filter Filter to apply:
#' \itemize{
#' \item \strong{owner} - posts by the wall owner;
#' \item \strong{others} - posts by someone else;
#' \item \strong{all} - posts by the wall owner and others (default);
#' \item \strong{postponed} - timed posts (only available for calls with an access_token);
#' \item \strong{suggests} - suggested posts on a community wall.
#' }
#' @param extended 1 - to return wall, profiles, and groups fields, 0 - to return no additional fields (default).
#' @param fields List of comma-separated words
#' @param v Version of API
#' @return Returns a list of post objects.
#' If extended is set to 1, also returns the following:
#' \itemize{
#' \item \strong{wall} - Contains a list of post objects.
#' \item \strong{profiles} - Contains user objects with additional fields photo and online.
#' \item \strong{groups} - Contains community objects.
#' }
#' @examples \dontrun{
#' wall <- getWall(domain='spbrug', count=10, progress_bar=TRUE)
#' }
#' @export
getWall <- function(owner_id='', domain='', offset='', count='', filter='owner', extended='', fields='', v=getAPIVersion()) {
  .Deprecated("getWallExecute()")
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

  if (has_error(response))
    return(try_handle_error(response))

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
#' \item \strong{owner} - posts by the wall owner;
#' \item \strong{others} - posts by someone else;
#' \item \strong{all} - posts by the wall owner and others (default);
#' \item \strong{postponed} - timed posts (only available for calls with an access_token);
#' \item \strong{suggests} - suggested posts on a community wall.
#' }
#' @param extended 1 - to return wall, profiles, and groups fields, 0 - to return no additional fields (default).
#' @param fields List of comma-separated words
#' @param progress_bar Display progress bar
#' @param v Version of API
#' @return Returns a list of post objects.
#' If extended is set to 1, also returns the following:
#' \itemize{
#' \item \strong{wall} - Contains a list of post objects.
#' \item \strong{profiles} - Contains user objects with additional fields photo and online.
#' \item \strong{groups} - Contains community objects.
#' }
#' @importFrom utils setTxtProgressBar txtProgressBar
#' @examples \dontrun{
#' # get all posts from wall
#' wall <- getWallExecute(domain='spbrug', count=0, progress_bar=TRUE)
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

  if (progress_bar) {
    pb <- txtProgressBar(min = 0, max = max_count, style = 3)
    setTxtProgressBar(pb, nrow(posts))
  }

  num_records <- max_count - nrow(posts)
  while (nrow(posts) < max_count) {
    tryCatch({ posts2500 <- get_posts2500(owner_id = owner_id,
                               domain = domain,
                               filter = filter,
                               extended = extended,
                               fields = fields,
                               max_count = num_records,
                               offset = offset + nrow(posts),
                               v = v)
      posts <- jsonlite::rbind.pages(list(posts, posts2500))
      num_records <- ifelse((max_count - nrow(posts)) > num_records, num_records, max_count - nrow(posts)) },
    warning = function(w) {
      num_records <<- as.integer(num_records / 2)
      warning(simpleWarning(paste0('Parameter "count" was tuned: ', num_records, ' per request.')))
    })

    if (progress_bar)
      setTxtProgressBar(pb, nrow(posts))
  }

  if (progress_bar)
    close(pb)

  wall <- list(posts = posts,
               count = response$count)
  class(wall) <- c(class(wall), "posts.list")

  return(wall)
}


#' Allows to search posts on user or community walls
#'
#' @param owner_id User or community id. Remember that for a community owner_id must be negative.
#' @param domain User or community screen name.
#' @param query Search query string.
#' @param owners_only 1 - returns only page owner's posts.
#' @param count Count of posts to return.
#' @param offset Results offset.
#' @param extended Show extended post info.
#' @param fields List of comma-separated words
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

  if (has_error(response))
    return(try_handle_error(response))

  response$response
}


#' Returns a list of posts from user or community walls by their IDs
#'
#' @param posts User or community IDs and post IDs, separated by underscores. Use a negative value to designate a community ID.
#' @param extended 1 - to return user and community objects needed to display posts, 0 - no additional fields are returned (default).
#' @param copy_history_depth Sets the number of parent elements to include in the array copy_history that is returned if the post is a repost from another wall.
#' @param fields List of comma-separated words
#' @param v Version of API
#' @return Returns a list of post objects.
#' If extended is set to 1, returns the following:
#' \itemize{
#' \item \strong{wall} - Contains post objects.
#' \item \strong{profiles} - Contains user objects with additional fields sex, photo, photo_medium_rec, and online.
#' \item \strong{groups} - Contains community objects.
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

  if (has_error(response))
    return(try_handle_error(response))

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
#' \item \strong{items} - An array of wall reposts.
#' \item \strong{profiles} - Information about users with additional fields sex, online, photo, photo_medium_rec, and screen_name.
#' \item \strong{groups} - Information about communities.
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

  if (has_error(response))
    return(try_handle_error(response))

  response$response
}


#' Returns a list of comments on a post on a user wall or community wall
#'
#' @param owner_id User ID or community ID. Use a negative value to designate a community ID.
#' @param post_id Post ID.
#' @param need_likes 1 - to return the likes field, 0 - not to return the likes field (default).
#' @param start_comment_id Positive number.
#' @param offset Offset needed to return a specific subset of comments.
#' @param count Number of comments to return (maximum 100).
#' @param sort Sort order: asc - chronological, desc - reverse chronological.
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

  if (has_error(response))
    return(try_handle_error(response))

  response$response
}


#' Returns a list of comments on a post on a user wall or community wall
#'
#' @param owner_id User ID or community ID. Use a negative value to designate a community ID.
#' @param post_id Post ID.
#' @param need_likes 1 - to return the likes field (default), 0 - not to return the likes field.
#' @param start_comment_id Positive number
#' @param offset Offset needed to return a specific subset of comments.
#' @param count Number of comments to return.
#' @param sort Sort order: asc - chronological, desc - reverse chronological.
#' @param preview_length Number of characters at which to truncate comments when previewed. Specify 0 (default) if you do not want to truncate comments.
#' @param extended Flag, either 1 or 0.
#' @param progress_bar Display progress bar
#' @param v Version of API
#' @export
postGetComments <- function(owner_id='', post_id='', need_likes=1, start_comment_id='', offset=0, count=10, sort='', preview_length=0, extended='', progress_bar = FALSE, v=getAPIVersion()) {
  get_comments2500 <- function(owner_id='', post_id='', need_likes=1, start_comment_id='', offset=0, max_count='', sort='', preview_length=0, extended='', v=getAPIVersion())
  {
    if (max_count > 2500)
      max_count <- 2500
    if (max_count <= 100) {
      execute(paste0('return API.wall.getComments({
                     "owner_id":"', owner_id, '",
                     "post_id":"', post_id, '",
                     "count":"', max_count, '",
                     "offset":"', offset, '",
                     "need_likes":"', need_likes, '",
                     "start_comment_id":"', start_comment_id, '",
                     "sort":"', sort, '",
                     "preview_length":"', preview_length, '",
                     "extended":"', extended, '", "v":"', v, '"}).items;'))
    } else {
      code <- 'var comments = [];'
      code <- paste0(code, 'comments = comments + API.wall.getComments({
                     "owner_id":"', owner_id, '",
                     "post_id":"', post_id, '",
                     "count":"', 100, '",
                     "offset":"', offset, '",
                     "need_likes":"', need_likes, '",
                     "start_comment_id":"', start_comment_id, '",
                     "sort":"', sort, '",
                     "preview_length":"', preview_length, '",
                     "extended":"', extended, '", "v":"', v, '"}).items;')
      code <- paste0(code, 'var offset = 100 + ', offset, ';
                     var count = 100; var max_offset = offset + ', max_count, ';
                     while (offset < max_offset && comments.length <= offset && offset-', offset, '<', max_count, ') {
                     if (', max_count, ' - comments.length < 100) {
                     count = ', max_count, ' - comments.length;
                     };
                     comments = comments + API.wall.getComments({
                     "owner_id":"', owner_id, '",
                     "post_id":"', post_id, '",
                     "offset":offset,
                     "count":count,
                     "need_likes":"', need_likes, '",
                     "start_comment_id":"', start_comment_id, '",
                     "sort":"', sort, '",
                     "preview_length":"', preview_length, '",
                     "extended":"', extended, '", "v":"', v, '"}).items;
                     offset = offset + 100;
                     };
                     return comments;')
      execute(code)
    }
  }

  code <- paste0('return API.wall.getComments({
                 "owner_id":"', owner_id, '",
                 "post_id":"', post_id, '",
                 "count":"', 1, '",
                 "offset":"', offset, '",
                 "need_likes":"', need_likes, '",
                 "start_comment_id":"', start_comment_id, '",
                 "sort":"', sort, '",
                 "preview_length":"', preview_length, '",
                 "extended":"', extended, '", "v":"', v, '"});')
  response <- execute(code)
  comments <- response$items
  max_count <- ifelse((response$count - offset) > count & count != 0, count, response$count - offset)

  if (max_count == 0)
    return(list(comments = response$items,
                count = response$count))

  offset_counter <- 0

  if (progress_bar) {
    pb <- txtProgressBar(min = 0, max = max_count, style = 3)
    setTxtProgressBar(pb, nrow(comments))
  }

  while (nrow(comments) < max_count) {
    tryCatch({
      comments2500 <- get_comments2500(owner_id = owner_id,
                                     post_id = post_id,
                                     need_likes = need_likes,
                                     extended = extended,
                                     sort = sort,
                                     preview_length = preview_length,
                                     start_comment_id = start_comment_id,
                                     max_count = (max_count - nrow(comments)),
                                     offset = (1 + offset + offset_counter * 2500),
                                     v = v)
      comments <- jsonlite::rbind.pages(list(comments, comments2500))
      offset_counter <- offset_counter + 1
      }, error = function(e) {
        warning(e)
      })

    if (progress_bar)
      setTxtProgressBar(pb, nrow(comments))

  }

  if (progress_bar)
    close(pb)

  list(comments = comments,
       count = response$count)
}


#' Returns a list of comments on a user wall or community wall
#'
#' @param posts A list of posts or wall object (from getWallExecute())
#' @param progress_bar Display progress bar
#' @param v Version of API
#' @export
wallGetCommentsList <- function(posts, progress_bar = FALSE, v = getAPIVersion()) {
  get_comments <- function(posts, v = getAPIVersion())
  {
    num_requests <- ceiling(nrow(posts) / 25)
    from <- 1
    to <- 25
    comments <- list()
    for (i in 1:num_requests) {
      code <- 'var comments_per_post = {}; var comments;'
      if (to > nrow(posts))
        to <- nrow(posts)
      for (index in from:to) {
        code <- paste0(code, 'comments = API.wall.getComments({
                       "owner_id":"', posts[index, ]$owner_id, '",
                       "post_id":"', posts[index, ]$id, '",
                       "need_likes":"', 1, '",
                       "count":"', 100, '",
                       "v":"', v, '"}).items;
                       comments_per_post.post', posts[index, ]$id, "=comments;", sep = "")
      }
      code <- paste0(code, 'return comments_per_post;')
      comments <- append(comments, execute(code))
      from <- from + 25
      to <- to + 25
    }
    names(comments) <- posts$id
    comments
  }

  if ("posts.list" %in% class(posts))
    posts <- posts$posts

  cmt_groups <- split(posts, posts$comments$count > 100)
  posts_le100 <- cmt_groups[['FALSE']]
  posts_gt100 <- cmt_groups[['TRUE']]

  comments <- list()
  from <- 1
  max_count <- nrow(posts_le100)
  to <- ifelse(max_count >= 75, 75, max_count)

  if (progress_bar) {
    pb <- txtProgressBar(min = 0, max = nrow(posts), style = 3)
    setTxtProgressBar(pb, 0)
  }

  repeat {
    comments75 <- get_comments(posts_le100[from:to, ], v)
    comments <- append(comments, comments75)

    if (progress_bar)
      setTxtProgressBar(pb, length(comments))

    if (to >= max_count)
      break

    from <- to + 1
    to <- ifelse(to + 75 >= max_count, max_count, to + 75)
  }


  if (!is.null(posts_gt100)) {
    for (i in 1:nrow(posts_gt100)) {
      owner_id <- posts_gt100$owner_id[i]
      post_id <- posts_gt100$id[i]
      comments[[paste0(post_id)]] <- postGetComments(owner_id = owner_id,
                                                     post_id = post_id,
                                                     count = 0,
                                                     v = v)$comments
      if (progress_bar)
        setTxtProgressBar(pb, length(comments))
    }
  }

  if (progress_bar)
    close(pb)

  comments_ordered <- list()
  for (i in 1:nrow(posts)) {
    comments_ordered[[paste0(posts$id[i])]] <- comments[[paste0(posts$id[i])]]
  }

  class(comments_ordered) <- c(class(comments_ordered), "vk.comments")
  comments_ordered
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
