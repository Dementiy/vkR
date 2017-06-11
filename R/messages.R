#' Returns message history for the specified user or group chat
#'
#' @param offset Offset needed to return a specific subset of messages
#' @param count Number of messages to return (maximum value 200)
#' @param user_id ID of the user whose message history you want to return
#' @param peer_id Destination ID (user ID, group ID or chat ID)
#' @param start_message_id Starting message ID from which to return history
#' @param rev Sort order: 1 - return messages in chronological order; 0 - return messages in reverse chronological order
#' @param v Version of API
#' @export
messagesGetHistory <- function(offset='', count='', user_id='', peer_id='', start_message_id='', rev='', v=getAPIVersion()) {
  query <- queryBuilder('messages.getHistory',
                        offset = offset,
                        count = count,
                        user_id = user_id,
                        peer_id = peer_id,
                        start_message_id = start_message_id,
                        rev = rev,
                        v = v)
  request_delay()
  response <- jsonlite::fromJSON(query)

  if (has_error(response))
    return(try_handle_error(response))

  response$response
}


#' Returns message history for the specified user or group chat
#'
#' @param offset Offset needed to return a specific subset of messages
#' @param count Number of messages to return (0 for all history)
#' @param user_id ID of the user whose message history you want to return
#' @param peer_id Destination ID (user ID, group ID or chat ID)
#' @param start_message_id Starting message ID from which to return history
#' @param rev Sort order: 1 - return messages in chronological order; 0 - return messages in reverse chronological order
#' @param progress_bar Display progress bar
#' @param v Version of API
#' @importFrom utils setTxtProgressBar txtProgressBar
#' @export
messagesGetHistoryExecute <- function(offset=0, count=0, user_id='', peer_id='', start_message_id='', rev=0, progress_bar=FALSE, v=getAPIVersion())
{
  get_messages <- function(offset='', count='', user_id='', peer_id='', start_message_id='', rev='', v=getAPIVersion())
  {
    code <- 'var messages = [];'
    num_requests <- 0
    while (num_requests != 3 && count != 0)
    {
      current_count <- ifelse((count - 200) >= 0, 200, count)
      code <- paste0(code, 'messages = messages + API.messages.getHistory({"user_id":"', user_id,
                     '", "offset":"', offset,
                     '", "count":"', current_count,
                     '", "peer_id":"', peer_id,
                     ifelse(start_message_id == '', '', paste0('", "start_message_id":"', start_message_id)),
                     '", "rev":"', rev,
                     '", "v":"', v, '"}).items;')
      offset <- offset + 200
      num_requests <- num_requests + 1
      count <- count - current_count
    }
    code <- paste0(code, 'return messages;')
    execute(code)
  }

  code <- paste0('return API.messages.getHistory({"user_id":"', user_id,
                 '", "offset":"', offset,
                 '", "rev":"', rev,
                 '", "count":"', 1,
                 '", "peer_id":"', peer_id,
                 ifelse(start_message_id == '', '', paste0('", "start_message_id":"', start_message_id)),
                 '", "v":"', v, '"});')
  response <- execute(code)
  messages <- response$items
  max_count <- ifelse((response$count - offset) > count & count != 0, count, response$count - offset)

  if (max_count == 0)
    return(list(messages = response$items,
                count = response$count,
                in_read = response$in_read,
                out_read = response$out_read,
                unread = NULL))

  offset_counter <- 0

  if (progress_bar) {
    pb <- txtProgressBar(min = 0, max = max_count, style = 3)
    setTxtProgressBar(pb, nrow(messages))
  }

  while (nrow(messages) < max_count) {
    messages600 <- get_messages(user_id = user_id,
                                peer_id = peer_id,
                                rev = rev,
                                start_message_id = start_message_id,
                                count = (max_count - nrow(messages)),
                                offset = (1 + offset + offset_counter * 600),
                                v = v)
    messages <- jsonlite::rbind_pages(list(messages, messages600))

    if (progress_bar)
      setTxtProgressBar(pb, nrow(messages))

    offset_counter <- offset_counter + 1
  }

  if (progress_bar)
    close(pb)

  list(messages = messages,
       count = response$count,
       in_read = response$in_read,
       out_read = response$out_read,
       unread = response$unread)
}



#' Returns all message history for the specified user or group chat
#'
#' @param user_id ID of the user whose message history you want to return
#' @param peer_id Destination ID (user ID, group ID or chat ID)
#' @param rev Sort order: 1 - return messages in chronological order; 0 - return messages in reverse chronological order
#' @param v Version of API
#' @export
messagesGetHistoryAll <- function(user_id='', peer_id='', rev=0, v=getAPIVersion()) {
  messagesGetHistoryExecute(user_id = user_id, peer_id = peer_id, rev = rev, count = 0, v = v)
}


#' Returns a list of the current user's incoming or outgoing private messages
#'
#' @param out 1 - to return outgoing messages; 0 - to return incoming messages (default)
#' @param offset Offset needed to return a specific subset of messages
#' @param count Number of messages to return
#' @param time_offset Maximum time since a message was sent, in seconds. To return messages without a time limitation, set as 0
#' @param filters Filter to apply: 1 - unread only; 2 - not from the chat; 4 - messages from friends
#' @param preview_length Number of characters after which to truncate a previewed message. To preview the full message, specify 0
#' @param last_message_id ID of the message received before the message that will be returned last
#' @param v Version of API
#' @export
messagesGet <- function(out='', offset='', count='', time_offset='', filters='', preview_length='', last_message_id='', v=getAPIVersion()) {
  query <- queryBuilder('messages.get',
                        out = out,
                        offset = offset,
                        count = count,
                        time_offset = time_offset,
                        filters = filters,
                        preview_length = preview_length,
                        last_message_id = last_message_id,
                        v = v)
  request_delay()
  response <- jsonlite::fromJSON(query)

  if (has_error(response))
    return(try_handle_error(response))

  response$response
}


#' Sends a message.
#' @param user_id User ID (by default — current user).
#' @param random_id Unique identifier to avoid resending the message.
#' @param peer_id Destination ID.
#' @param domain User's short address (for example, illarionov).
#' @param chat_id ID of conversation the message will relate to.
#' @param user_ids IDs of message recipients (if new conversation shall be started).
#' @param message Text of the message (required if attachments is not set).
#' @param lat Geographical latitude of a check-in, in degrees (from -90 to 90).
#' @param long Geographical longitude of a check-in, in degrees (from -180 to 180).
#' @param attachment List of objects attached to the message, separated by commas, in the following format: <type><owner_id>_<media_id>
#'
#' <type> — Type of media attachment:
#' \itemize{
#'  \item photo - photo
#'  \item video - video
#'  \item audio - audio
#'  \item doc - document
#'  \item wall - wall post
#' }
#'
#' <owner_id> — ID of the media attachment owner.
#'
#' <media_id> — media attachment ID.
#' @param forward_messages ID of forwarded messages, separated with a comma. Listed messages of the sender will be shown in the message body at the recipient's.
#' @param sticker_id Sticker id.
#' @param v Version of API.
#' @export
messagesSend <- function(user_id, random_id='', peer_id='', domain='', chat_id='', user_ids='', message='', lat='', long='', attachment='', forward_messages='', sticker_id='', v=getAPIVersion()) {
  query <- queryBuilder('messages.send',
                        user_id = user_id,
                        random_id = random_id,
                        peer_id = peer_id,
                        domain = domain,
                        chat_id = chat_id,
                        user_ids = user_ids,
                        message = message,
                        lat = lat,
                        long = long,
                        attachment = attachment,
                        forward_messages = forward_messages,
                        sticker_id = sticker_id,
                        v = v)
  request_delay()
  response <- jsonlite::fromJSON(URLencode(query))

  if (has_error(response))
    return(try_handle_error(response))

  response$response
}


#' Split messages by days, weeks, months
#'
#' @param messages List of messages from messagesGet()
#' @param format Character string giving a date-time format as used by strptime
#' @export
messagesSplitByDate <- function(messages, format = "%y-%m-%d") {
  days_list <- format(as.POSIXct(messages$date, origin = "1970-01-01"), format = format)
  messages_by_days <- split(messages, as.factor(days_list))
  messages_by_days
}
