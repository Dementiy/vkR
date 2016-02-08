#' Возвращает список сообществ указанного пользователя
#'
#' @param user_id Идентификатор пользователя, информацию о сообществах которого требуется получить
#' @param extended Если указать в качестве этого параметра 1, то будет возвращена полная информация о группах пользователя. По умолчанию 0
#' @param filter Список фильтров сообществ, которые необходимо вернуть, перечисленные через запятую. Доступны значения admin, editor, moder, groups, publics, events
#' @param fields Список дополнительных полей, которые необходимо вернуть
#' @param offset Смещение, необходимое для выборки определённого подмножества сообществ
#' @param count Количество сообществ, информацию о которых нужно вернуть
#' @param v Версия API
#' @examples
#' \dontrun{
#' groups <- getGroups('1', fields='city,country,place,description,wiki_page,members_count,counters,start_date,finish_date,can_post,can_see_all_posts,activity,status,contacts,links,fixed_post,verified,site,can_create_topic')
#' }
#' @export
getGroups <- function(user_id='', extended='', filter='', fields='', offset='', count='', v=getAPIVersion()) {
  query <- queryBuilder('groups.get',
                        user_id=user_id,
                        extended=extended,
                        filter=filter,
                        fields=fields,
                        offset=offset,
                        count=count,
                        v=v)
  response <- fromJSON(query)
  response$response
}

#' Возвращает список участников сообщества
#'
#' @param group_id Идентификатор пользователя, информацию о сообществах которого требуется получить
#' @param sort Если указать в качестве этого параметра 1, то будет возвращена полная информация о группах пользователя. По умолчанию 0
#' @param offset Смещение, необходимое для выборки определённого подмножества сообществ
#' @param count Количество сообществ, информацию о которых нужно вернуть
#' @param fields Список дополнительных полей, которые необходимо вернуть
#' @param filter Список фильтров сообществ, которые необходимо вернуть, перечисленные через запятую. Доступны значения admin, editor, moder, groups, publics, events
#' @param v Версия API
#' @examples
#' \dontrun{
#' groups <- getGroupsMembers('1', fields='sex,bdate,city,country,photo_50,education,interests,music,movies,tv,books,games,about,quotes,personal')
#' }
#' @export
getGroupsMembers <- function(group_id='', sort='', offset='', count='', fields='', filter='', v=getAPIVersion()) {
  query <- queryBuilder('groups.getMembers',
                        group_id=group_id,
                        sort=sort,
                        offset=offset,
                        count=count,
                        fields=fields,
                        filter=filter,
                        v=v)
  response <- fromJSON(query)
  response$response
}
