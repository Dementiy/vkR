#' Возвращает список идентификаторов друзей пользователя или расшиернную информацию о друзьях пользователя
#'
#' @param user_id Идентификатор пользователя, для которого необходимо получить список друзей
#' @param order Порядок, в котором нужно вернуть список друзей
#' @param list_id Идентификатор списка друзей
#' @param count Количество друзей, которое необходимо вернуть
#' @param offset Смещение, необходимое для выборки определенного подмножества друзей
#' @param fields Список дополнительных полей, которые необходимо вернуть
#' @param name_case Падеж для склонения имени и фамилии пользователя
#' @examples
#' friends_list <- getFriends(user_id='1', order='name', fields='bdate')
#' friends <- friends_list$items
#' @export
getFriends <- function(user_id='', order='', list_id='', count='', offset='', fields='', name_case='', v='5.28') {
  query <- queryBuilder('friends.get', 
                        user_id=user_id, 
                        order=order, 
                        list_id=list_id, 
                        count=count, 
                        offset=offset, 
                        fields=fields, 
                        name_case=name_case, 
                        v=v)
  response <- fromJSON(query)
  response$response
}


#' Возвращает список идентификаторов общих друзей между парой пользователей
#' 
#' @param source_id Идентификатор пользователя, чьи друзья пересекаются с друзьями пользователя с идентификатором target_uid
#' @param target_uid Идентификатор пользователя, с которым необходимо искать общих друзей
#' @param target_uids Список идентификаторов пользователей, с которыми необходимо искать общих друзей
#' @param order Порядок, в котором нужно вернуть список общих друзей
#' @param count Количество общих друзей, которое нужно вернуть
#' @param offset Смещение, необходимое для выборки определенного подмножества общих друзей
#' @examples
#' mutual_friends <- getMutual(target_uid='1')
#' @export
getMutual <- function(source_id='', target_uid='', target_uids='', order='', count='', offset='') {
  query <- queryBuilder('friends.getMutual', v='5.29')
  response <- fromJSON(rawToChar(POST(URLencode(query),
                                      body=list(source_id=source_id,
                                                target_uid=target_uid,
                                                target_uids=target_uids,
                                                order=order,
                                                count=count,
                                                offset=offset))$content))
  response$response
}