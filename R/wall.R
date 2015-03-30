#' Возвращает список записей со стены пользователя или сообщества
#'
#' @param owner_id Идентификатор пользователя или сообщества
#' @param domain Короткий адрес пользователя или сообщества
#' @param offset Смещение, необходимое для выборки определенного подмножества записей
#' @param count Количество записей, которое необходимо получить (но не более 100)
#' @param filter Определяет, какие типы записей на стене необходимо получить
#' @param extended Если установлен в 1, то будут возвращены три массива wall, profiles и groups
#' getWall()
#' @export
getWall <- function(owner_id='', domain='', offset='', count='', filter='owner', extended='', v='5.28') {
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


#' Метод, позволяющий осуществлять поиск по стенам пользователей
#' 
#' @param owner_id Идентификатор пользователя или сообщества
#' @param domain Короткий адрес пользователя или сообщества
#' @param query Поисковой запрос
#' @param owners_only 1 — возвращать только записи от имени владельца стены
#' @param count Количество записей, которые необходимо вернуть
#' @param offset Смещение
#' @param extended Возвращать ли расширенную информацию о записях
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


#' Возвращает список записей со стен пользователей или сообществ по их идентификаторам
#' 
#' @param posts Перечисленные через запятую идентификаторы, которые представляют собой идущие через знак подчеркивания id владельцев стен и id самих записей на стене
#' @param extended 1 - возвращает объекты пользователей и групп, необходимые для отображения записей
#' @param copy_history_depth Определяет размер массива copy_history, возвращаемого в ответе, если запись является репостом записи с другой стены
#' @export
wallGetById <- function(posts='', extended='', copy_history_depth='') {
  query <- queryBuilder('wall.getById',
                        posts=posts,
                        extended=extended,
                        copy_history_depth=copy_history_depth)
  response <- fromJSON(query)
  response$response
}


#' Позволяет получать список репостов заданной записи
#' 
#' @param owner_id Идентификатор пользователя или сообщества, на стене которого находится запись
#' @param post_id Идентификатор записи на стене
#' @param offset Смещение, необходимое для выборки определенного подмножества записей
#' @param count Количество записей, которое необходимо получить
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


#' Возвращает список комментариев к записи на стене
#' 
#' @param owner_id Идентификатор владельца страницы
#' @param post_id Идентификатор записи на стене
#' @param need_likes 1 — возвращать информацию о лайках
#' @param offset Сдвиг, необходимый для получения конкретной выборки результатов
#' @param count Число комментариев, которые необходимо получить
#' @param sort Порядок сортировки комментариев (asc — от старых к новым, desc - от новых к старым) 
#' @param preview_length Количество символов, по которому нужно обрезать текст комментария
#' @param extended 1 - комментарии в ответе будут возвращены в виде пронумерованных объектов, дополнительно будут возвращены списки объектов profiles, groups
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