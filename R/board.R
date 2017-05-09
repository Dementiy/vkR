#' Returns a list of topics on a community's discussion board
#'
#' @param group_id ID of the community that owns the discussion board.
#' @param topics_ids IDs of topics to be returned (100 maximum). By default, all topics are returned. If this parameter is set, the order, offset, and count parameters are ignored.
#' @param order Sort order:
#' \itemize{
#'  \item 1 - by date updated in reverse chronological order;
#'  \item 2 - by date created in reverse chronological order;
#'  \item -1 - by date updated in chronological order;
#'  \item -2 - by date created in chronological order.
#' }
#' If no sort order is specified, topics are returned in the order specified by the group administrator. Pinned topics are returned first, regardless of the sorting.
#' @param offset Offset needed to return a specific subset of topics.
#' @param count Number of topics to return (default 40, maximum value 100).
#' @param extended 1 — to return information about users who created topics or who posted there last; 0 — to return no additional fields (default).
#' @param preview 1 — to return the first comment in each topic; 2 — to return the last comment in each topic; 0 — to return no comments.
#' @param preview_length Number of characters after which to truncate the previewed comment. To preview the full comment, specify 0.
#' @param v Version of API
#' @export
getTopics <- function(group_id='', topics_ids='', order='', offset=0, count=40, extended=0, preview=0, preview_length=90, v=getAPIVersion()) {
  query <- queryBuilder('board.getTopics',
                        group_id=group_id,
                        topics_ids=topics_ids,
                        order=order,
                        offset=offset,
                        count=count,
                        extended=extended,
                        preview=preview,
                        preview_length=preview_length,
                        v=v)
  request_delay()
  response <- jsonlite::fromJSON(query)

  if (has_error(response))
    return(try_handle_error(response))

  response$response
}


#' Returns a list of topics on a community's discussion board
#'
#' @param group_id ID of the community that owns the discussion board.
#' @param order Sort order:
#' \itemize{
#'  \item 1 - by date updated in reverse chronological order;
#'  \item 2 - by date created in reverse chronological order;
#'  \item -1 - by date updated in chronological order;
#'  \item -2 - by date created in chronological order.
#' }
#' If no sort order is specified, topics are returned in the order specified by the group administrator. Pinned topics are returned first, regardless of the sorting.
#' @param offset Offset needed to return a specific subset of topics.
#' @param count Number of topics to return (default 40, 0 - for all topics).
#' @param preview 1 — to return the first comment in each topic; 2 — to return the last comment in each topic; 0 — to return no comments.
#' @param preview_length Number of characters after which to truncate the previewed comment. To preview the full comment, specify 0.
#' @param use_db Use database
#' @param db_params Collection name and suffix
#' @param progress_bar Display progress bar
#' @param v Version of API
#' @export
getTopicsExecute <- function(group_id='', order='', offset=0, count=40, preview=0, preview_length=90, use_db=FALSE, db_params=list(), progress_bar=FALSE, v=getAPIVersion()) {
  get_topics2500 <- function(group_id='', order='', offset=0, max_count=0, preview=0, preview_length=90, v=getAPIVersion()) {
    if (max_count > 2500)
      max_count <- 2500
    if (max_count <= 100) {
      execute(paste0('return API.board.getTopics({"group_id":"', group_id, '",
                     "order":"', order, '",
                     "offset":"', offset, '",
                     "count":"', max_count, '",
                     "preview":"', preview, '",
                     "preview_length":"', preview_length, '",
                     "v":"', v, '"}).items;'))
    } else {
      code <- 'var topics = [];'
      code <- paste0(code, 'topics = topics + API.board.getTopics({"group_id":"', group_id, '",
                     "order":"', order, '",
                     "offset":"', offset, '",
                     "count":"', 100, '",
                     "preview":"', preview, '",
                     "preview_length":"', preview_length, '",
                     "v":"', v, '"}).items;')
      code <- paste0(code, 'var offset = 100 + ', offset, ';
                     var count = 100; var max_offset = offset + ', max_count, ';
                     while (offset < max_offset && topics.length <= offset && offset-', offset, '<', max_count, ') {
                       if (', max_count, ' - topics.length < 100) {
                        count = ', max_count, ' - topics.length;
                       };
                       topics = topics + API.board.getTopics({"group_id":"', group_id, '",
                         "order":"', order, '",
                         "offset": offset,
                         "count": count,
                         "preview":"', preview, '",
                         "preview_length":"', preview_length, '",
                         "v":"', v, '"}).items;
                       offset = offset + 100;
                     };
                     return topics;')
      execute(code)
    }
  }

  code <- paste0('return API.board.getTopics({"group_id":"', group_id, '",
                  "order":"', order, '",
                  "offset":"', offset, '",
                  "count":"', 1, '",
                  "preview":"', preview, '",
                  "preview_length":"', preview_length, '",
                  "v":"', v, '"});')

  response <- execute(code)

  topics <- response$items
  max_count <- ifelse((response$count - offset) > count & count != 0, count, response$count - offset)

  if (max_count == 0)
    return(list(group_id = group_id,
                topics = response$items,
                count = response$count))

  if (use_db) {
    collection <- or(db_params[['collection']], group_id)
    suffix <- or(db_params[['suffix']], 'board')
    key <- or(db_params[['key']], 'id')

    if (collection_exists(collection, suffix))
      db_update(object = topics, key = key, collection = collection, suffix = suffix, upsert = TRUE)
    else
      db_save(object = topics, collection = collection, suffix = suffix)
  }

  if (progress_bar) {
    pb <- txtProgressBar(min = 0, max = max_count, style = 3)
    setTxtProgressBar(pb, nrow(topics))
  }

  num_records <- max_count - nrow(topics)
  while (nrow(topics) < max_count) {
    tryCatch({ topics2500 <- get_topics2500(group_id = group_id,
                                            order = order,
                                            preview = preview,
                                            preview_length = preview_length,
                                            max_count = num_records,
                                            offset = offset + nrow(topics),
                                            v = v)
    if (use_db)
      db_update(object = topics2500, key = key, collection = collection, suffix = suffix, upsert = TRUE)
    topics <- jsonlite::rbind.pages(list(topics, topics2500))
    num_records <- ifelse((max_count - nrow(topics)) > num_records, num_records, max_count - nrow(topics)) },
    vk_error13 = function(e) {
      num_records <<- as.integer(num_records / 2)
      warning(simpleWarning(paste0('Parameter "count" was tuned: ', num_records, ' per request.')))
    })

    if (progress_bar)
      setTxtProgressBar(pb, nrow(topics))
  }

  if (progress_bar)
    close(pb)

  board <- list(group_id = group_id,
                topics = topics,
                count = response$count)
  class(board) <- c(class(board), "topics.list")

  return(board)
}


#' Returns a list of comments on a topic on a community's discussion board
#'
#' @param group_id ID of the community that owns the discussion board.
#' @param topic_id Topic ID.
#' @param need_likes 1 - to return the likes field, 0 - not to return the likes field (default).
#' @param start_comment_id Positive number.
#' @param offset Offset needed to return a specific subset of comments.
#' @param count Number of comments to return (default 20, maximum 100).
#' @param extended 1 — to return information about users who posted comments; 0 — to return no additional fields (default).
#' @param sort Sort order: asc - chronological, desc - reverse chronological.
#' @param v Version of API
#' @export
boardGetComments <- function(group_id='', topic_id='', need_likes=0, start_comment_id='', offset=0, count=20, sort='', extended=0, v=getAPIVersion()) {
  query <- queryBuilder('board.getComments',
                        group_id = group_id,
                        topic_id = topic_id,
                        need_likes = need_likes,
                        start_comment_id = start_comment_id,
                        offset = offset,
                        count = count,
                        extended = extended,
                        sort = sort,
                        v = v)
  request_delay()
  response <- jsonlite::fromJSON(query)

  if (has_error(response))
    return(try_handle_error(response))

  response$response
}


#' Returns a list of comments on a topic on a community's discussion board
#'
#' @param group_id ID of the community that owns the discussion board.
#' @param topic_id Topic ID.
#' @param need_likes 1 - to return the likes field, 0 - not to return the likes field (default).
#' @param start_comment_id Positive number.
#' @param offset Offset needed to return a specific subset of comments.
#' @param count Number of comments to return (default 20, 0 - for all comments).
#' @param sort Sort order: asc - chronological, desc - reverse chronological.
#' @param progress_bar Display progress bar.
#' @param v Version of API
#' @export
boardGetCommentsExecute <- function(group_id='', topic_id='', need_likes=0, start_comment_id='', offset=0, count=20, sort='', progress_bar=FALSE, v=getAPIVersion()) {
  get_comments2500 <- function(group_id='', topic_id='', need_likes=0, start_comment_id='', offset=0, max_count='', sort='', v=getAPIVersion())
  {
    if (max_count > 2500)
      max_count <- 2500
    if (max_count <= 100) {
      execute(paste0('return API.board.getComments({
                     "group_id":"', group_id, '",
                     "topic_id":"', topic_id, '",
                     "count":"', max_count, '",
                     "offset":"', offset, '",
                     "need_likes":"', need_likes, '",
                     "start_comment_id":"', start_comment_id, '",
                     "sort":"', sort, '",
                     "v":"', v, '"}).items;'))
    } else {
      code <- 'var comments = [];'
      code <- paste0(code, 'comments = comments + API.board.getComments({
                     "group_id":"', group_id, '",
                     "topic_id":"', topic_id, '",
                     "count":"', 100, '",
                     "offset":"', offset, '",
                     "need_likes":"', need_likes, '",
                     "start_comment_id":"', start_comment_id, '",
                     "sort":"', sort, '",
                     "v":"', v, '"}).items;')
      code <- paste0(code, 'var offset = 100 + ', offset, ';
                     var count = 100; var max_offset = offset + ', max_count, ';
                     while (offset < max_offset && comments.length <= offset && offset-', offset, '<', max_count, ') {
                       if (', max_count, ' - comments.length < 100) {
                        count = ', max_count, ' - comments.length;
                       };
                       comments = comments + API.board.getComments({
                         "group_id":"', group_id, '",
                         "topic_id":"', topic_id, '",
                         "offset":offset,
                         "count":count,
                         "need_likes":"', need_likes, '",
                         "start_comment_id":"', start_comment_id, '",
                         "sort":"', sort, '",
                         "v":"', v, '"}).items;
                       offset = offset + 100;
                     };
                     return comments;')
      execute(code)
    }
  }

  code <- paste0('return API.board.getComments({
                 "group_id":"', group_id, '",
                 "topic_id":"', topic_id, '",
                 "count":"', 1, '",
                 "offset":"', offset, '",
                 "need_likes":"', need_likes, '",
                 "start_comment_id":"', start_comment_id, '",
                 "sort":"', sort, '",
                 "v":"', v, '"});')
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
      comments2500 <- get_comments2500(group_id = group_id,
                                       topic_id = topic_id,
                                       need_likes = need_likes,
                                       sort = sort,
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


#' Returns a list of comments on a community's discussion board
#'
#' @param topics A list of topics (from getTopicsExecute())
#' @param progress_bar Display progress bar
#' @param v Version of API
#' @export
boardGetCommentsList <- function(topics, progress_bar = FALSE, v = getAPIVersion()) {
  get_comments <- function(group_id, topics, v = getAPIVersion())
  {
    num_requests <- ceiling(nrow(topics) / 25)
    from <- 1
    to <- 25
    comments <- list()
    for (i in 1:num_requests) {
      code <- 'var comments_per_topic = {}; var comments;'
      if (to > nrow(topics))
        to <- nrow(topics)
      for (index in from:to) {
        code <- paste0(code, 'comments = API.board.getComments({
                       "group_id":"', group_id, '",
                       "topic_id":"', topics[index, ]$id, '",
                       "need_likes":"', 1, '",
                       "count":"', 100, '",
                       "v":"', v, '"}).items;
                       comments_per_topic.topic', topics[index, ]$id, "=comments;", sep = "")
      }
      code <- paste0(code, 'return comments_per_topic;')
      comments <- append(comments, execute(code))
      from <- from + 25
      to <- to + 25
    }
    names(comments) <- topics$id
    comments
  }

  if ("topics.list" %in% class(topics)) {
    group_id <- topics$group_id
    topics <- topics$topics
  }

  cmt_groups <- split(topics, topics$comments > 100)
  topics_le100 <- cmt_groups[['FALSE']]
  topics_gt100 <- cmt_groups[['TRUE']]

  comments <- list()
  from <- 1
  max_count <- nrow(topics_le100)
  to <- ifelse(max_count >= 75, 75, max_count)

  if (progress_bar) {
    pb <- txtProgressBar(min = 0, max = nrow(topics), style = 3)
    setTxtProgressBar(pb, 0)
  }

  repeat {
    comments75 <- get_comments(group_id, topics_le100[from:to, ], v)
    comments <- append(comments, comments75)

    if (progress_bar)
      setTxtProgressBar(pb, length(comments))

    if (to >= max_count)
      break

    from <- to + 1
    to <- ifelse(to + 75 >= max_count, max_count, to + 75)
  }


  if (!is.null(topics_gt100)) {
    for (i in 1:nrow(topics_gt100)) {
      topic_id <- topics_gt100$id[i]
      comments[[paste0(topic_id)]] <- boardGetCommentsExecute(group_id = group_id,
                                                              topic_id = topic_id,
                                                              count = 0,
                                                              v = v)$comments
      if (progress_bar)
        setTxtProgressBar(pb, length(comments))
    }
  }

  if (progress_bar)
    close(pb)

  comments_ordered <- list()
  for (i in 1:nrow(topics)) {
    comments_ordered[[paste0(topics$id[i])]] <- comments[[paste0(topics$id[i])]]
  }

  class(comments_ordered) <- c(class(comments_ordered), "vk.board.comments")
  comments_ordered
}
