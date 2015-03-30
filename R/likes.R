#' Получает список идентификаторов пользователей, которые добавили заданный объект в свой список "Мне нравится"
#' 
#' @param type Тип объекта
#' @param owner_id Идентификатор владельца Like-объекта: id пользователя, id сообщества (со знаком «минус») или id приложения
#' @param item_id Идентификатор Like-объекта
#' @param page_url url страницы, на которой установлен виджет "Мне нравится"
#' @param filter Указывает, следует ли вернуть всех пользователей, добавивших объект в список "Мне нравится" или только тех, которые рассказали о нем друзьям
#' @param friends_only Указывает, необходимо ли возвращать только пользователей, которые являются друзьями текущего пользователя
#' @param extended 1 — возвращать расширенную информацию о пользователях и сообществах из списка поставивших отметку «Мне нравится» или сделавших репост
#' @param offset Смещение, относительно начала списка, для выборки определенного подмножества
#' @param count Количество возвращаемых идентификаторов пользователей
#' @export
likesGetList <- function(type='', owner_id='', item_id='', page_url='', filter='', friends_only='0', extended='', offset='', count='100') {
  query <- queryBuilder('likes.getList',
                        type=type,
                        owner_id=owner_id,
                        item_id=item_id,
                        page_url=page_url,
                        filter=filter,
                        friends_only=friends_only,
                        extended=extended,
                        offset=offset,
                        count=count)
  response <- fromJSON(query)
  response$response
}