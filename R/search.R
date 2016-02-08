#' Позволяет получить результаты быстрого поиска по произвольной подстроке
#' @param q Текст запроса, результаты которого нужно получить
#' @param limit Ограничение на количество возвращаемых результатов
#' @param filters Перечисленные через запятую типы данных, которые необходимо вернуть
#' @param search_global По умолчанию к результатам поиска добавляются результаты глобального поиска по всем пользователям и группам, это можно отключить передав 0
#' @param v Версия API
#' @export
search.getHints <- function(q='', limit='', filters='', search_global='', v=getAPIVersion()) {
  query <- queryBuilder('search.getHints',
                        q=q,
                        limit=limit,
                        filters=filters,
                        search_global=search_global,
                        v=v)
  response <- fromJSON(URLencode(query))
  response$response
}