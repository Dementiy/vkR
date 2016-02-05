#' Возвращает историю сообщений для указанного пользователя
#' 
#' @param offset Смещение, необходимое для выборки определенного подмножества сообщений
#' @param count Количество сообщений, которое необходимо получить (но не более 200)
#' @param user_id Идентификатор пользователя, историю переписки с которым необходимо вернуть
#' @param chat_id Идентификатор диалога, историю сообщений которого необходимо получить
#' @param start_message_id Если установлен в 1, то сообщения будут возвращены в хронологическом порядке
#' @param rev
#' @export
messagesGetHistory <- function(offset='', count='', user_id='', chat_id='', start_message_id='', rev='', v='5.33') {
  query <- queryBuilder('messages.getHistory',
                        offset=offset,
                        count=count,
                        user_id=user_id,
                        chat_id=chat_id,
                        start_message_id=start_message_id,
                        rev=rev,
                        v=v)
  response <- fromJSON(query)
  response$response
}


#' Возвращает историю всех сообщений для указанного пользователя
#' 
#' @param user_id Идентификатор пользователя, историю переписки с которым необходимо вернуть
#' messagesGetAll()
#' @examples
#' friends <- getFriends(fields = 'sex')$items
#' friends_ids <- friends$id
#' messages_for_friend <- data.frame()
#' for (friend_id in friends_ids) {
#'    messages_for_friend <- rbind(messages_for_friend, messagesGetHistoryAll(friend_id))
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


#' Возвращает список входящих либо исходящих личных сообщений текущего пользователя
#' 
#' @param out Если этот параметр равен 1, сервер вернет исходящие сообщения
#' @param offset Смещение, необходимое для выборки определенного подмножества сообщений
#' @param count Количество сообщений, которое необходимо получить
#' @param time_offset Максимальное время, прошедшее с момента отправки сообщения до текущего момента в секундах. 0, если Вы хотите получить сообщения любой давности
#' @param filters Фильтр возвращаемых сообщений: 8 — важные сообщения
#' @param preview_length Количество символов, по которому нужно обрезать сообщение. Укажите 0, если Вы не хотите обрезать сообщение
#' @param last_message_id Идентификатор сообщения, полученного перед тем, которое нужно вернуть последним
#' @export
messagesGet <- function(out='', offset='', count='', time_offset='', filters='', preview_length='', last_message_id='') {
  query <- queryBuilder('messages.get',
                        out=out,
                        offset=offset,
                        count=count,
                        time_offset=time_offset,
                        filters=filters,
                        preview_length=preview_length,
                        last_message_id=last_message_id)
  response <- fromJSON(query)
  response$response
}