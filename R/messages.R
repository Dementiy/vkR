#' Returns message history for the specified user or group chat
#' 
#' @param offset Offset needed to return a specific subset of messages
#' @param count Number of messages to return (maximum value 200)
#' @param user_id ID of the user whose message history you want to return
#' @param peer_id 
#' @param start_message_id Starting message ID from which to return history
#' @param rev Sort order: 1 — return messages in chronological order; 0 — return messages in reverse chronological order
#' @param v Version of API
#' @export
messagesGetHistory <- function(offset='', count='', user_id='', peer_id='', start_message_id='', rev='', v=getAPIVersion()) {
  query <- queryBuilder('messages.getHistory',
                        offset=offset,
                        count=count,
                        user_id=user_id,
                        peer_id=peer_id,
                        start_message_id=start_message_id,
                        rev=rev,
                        v=v)
  response <- fromJSON(query)
  response$response
}


#' Returns all message history for the specified user or group chat
#' 
#' @param user_id ID of the user whose message history you want to return
#' messagesGetAll()
#' @examples
#' \dontrun{
#' friends <- getFriends(fields = 'sex')$items
#' friends_ids <- friends$id
#' messages_for_friend <- data.frame()
#' for (friend_id in friends_ids) {
#'    messages_for_friend <- rbind(messages_for_friend, messagesGetHistoryAll(friend_id))
#' }
#' }
#' @export
messagesGetHistoryAll <- function(user_id) {
  all_messages <- data.frame()
  offset_counter <- 0
  repeat {
    messages200 <- messagesGetHistory(user_id = user_id, count = '200', offset = as.character(offset_counter * 200))$items
    
    # Оставляем только интересные поля
    actual_messages <- data.frame(
      id = messages200$id,
      user_id = messages200$user_id,
      date = messages200$date,
      out = messages200$out,
      body = messages200$body)
    
    all_messages <- rbind(all_messages, actual_messages)
    if (is.data.frame(messages200) == FALSE || nrow(messages200) < 200)
      break
    offset_counter <- offset_counter + 1
    if (offset_counter %% 3 == 0)
      Sys.sleep(1.0)
  }
  all_messages
}


#' Returns a list of the current user's incoming or outgoing private messages
#' 
#' @param out 1 — to return outgoing messages; 0 — to return incoming messages (default)
#' @param offset Offset needed to return a specific subset of messages
#' @param count Number of messages to return
#' @param time_offset Maximum time since a message was sent, in seconds. To return messages without a time limitation, set as 0
#' @param filters Filter to apply:  1 — unread only; 2 — not from the chat; 4 — messages from friends 
#' @param preview_length Number of characters after which to truncate a previewed message. To preview the full message, specify 0
#' @param last_message_id ID of the message received before the message that will be returned last
#' @param v Version of API
#' @export
messagesGet <- function(out='', offset='', count='', time_offset='', filters='', preview_length='', last_message_id='', v=getAPIVersion()) {
  query <- queryBuilder('messages.get',
                        out=out,
                        offset=offset,
                        count=count,
                        time_offset=time_offset,
                        filters=filters,
                        preview_length=preview_length,
                        last_message_id=last_message_id,
                        v=v)
  response <- fromJSON(query)
  response$response
}


#' Split messages by days, weeks, months
#' 
#' @param messages List of messages from messagesGet()
#' @param format Character string giving a date-time format as used by strptime
#' @export
messagesSplitByDate <- function(messages, format = "%y-%m-%d") {
  days_list <- format(as.POSIXct(messages$date, origin="1970-01-01"), format = format)
  messages_by_days <- split(messages, as.factor(days_list))
  messages_by_days
}