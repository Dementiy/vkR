#' Возвращает расширенную информацию о пользователях
#'
#' @param user_ids Перечисленные через запятую идентификаторы пользователей или их короткие имена
#' @param fields Список дополнительных полей профилей, которые необходимо вернуть
#' @param name_case Падеж для склонения имени и фамилии пользователя
#' @examples
#' user <- getUsers('1', fields='sex,bdate,city,country,photo_50,education,interests,music,movies,tv,books,games,about,quotes,personal')
#' @export
getUsers <- function(user_ids='', fields='', name_case='', v=getAPIVersion()) {
  if (length(user_ids) > 1) user_ids <- paste(user_ids, collapse=",")
  query <- queryBuilder('users.get', v=v)
  response <- fromJSON(rawToChar(POST(URLencode(query),
                                      body=list(user_ids=user_ids,
                                                fields=fields,
                                                name_case=name_case))$content))
  response$response
}


#' Возвращает список пользователей в соответствии с заданным критерием поиска
#' 
#' @param q строка поискового запроса. Например, Вася Бабич
#' @param sort сортировка результатов: 1 - по дате регистрации, 0 - по популярности
#' @param count количество возвращаемых пользователей
#' @param fields список дополнительных полей, которые необходимо вернуть
#' @param city идентификатор города
#' @param country идентификатор страны
#' @param hometown название города строкой
#' @param university_country идентификатор страны, в которой пользователи закончили ВУЗ
#' @param university идентификатор ВУЗа
#' @param university_year год окончания ВУЗа
#' @param university_faculty идентификатор факультета
#' @param university_chair идентификатор кафедры
#' @param sex пол, 1 — женщина, 2 — мужчина, 0 (по умолчанию) — любой
#' @param status семейное положение: 1 — Не женат, 2 — Встречается, 3 — Помолвлен, 4 — Женат, 7 — Влюблён, 5 — Всё сложно, 6 — В активном поиске
#' @param age_from начиная с какого возраста
#' @param age_to до какого возраста
#' @param birth_day день рождения
#' @param birth_month месяц рождения
#' @param birth_year год рождения
#' @param online 1 — только в сети, 0 — все пользователи
#' @param has_photo 1 — только с фотографией, 0 — все пользователи
#' @param school_country идентификатор страны, в которой пользователи закончили школу
#' @param school_city идентификатор города, в котором пользователи закончили школу
#' @param school_class положительное число
#' @param school идентификатор школы, которую закончили пользователи
#' @param school_year год окончания школы
#' @param religion религиозные взгляды
#' @param interests интересы
#' @param company название компании, в которой работают пользователи
#' @param position название должности
#' @param group_id идентификатор группы, среди пользователей которой необходимо проводить поиск
#' @param from_list Разделы среди которых нужно осуществить поиск, перечисленные через запятую: friends – искать среди друзей, subscribes – искать среди друзей и подписок пользователя 
#' @export
usersSearch <- function(q='', sort='', offset='', count='20', fields='', city='', country='', hometown='', 
                        university_country='', university='', university_year='', university_faculty='', university_chair='', 
                        sex='', status='', age_from='', age_to='', birth_day='', birth_month='', birth_year='',
                        online='', has_photo='', school_country='', school_city='', school_class='', school='', school_year='',
                        religion='', interests='', company='', position='', group_id='', v=getAPIVersion()) {
  query <- queryBuilder('users.search',
                        q=q,
                        sort=sort,
                        offset=offset,
                        count=count,
                        fields=fields,
                        city=city,
                        country=country,
                        hometown=hometown,
                        university_country=university_country,
                        university=university,
                        university_year=university_year,
                        university_faculty=university_faculty,
                        university_chair=university_chair,
                        sex=sex,
                        status=status,
                        age_from=age_from,
                        age_to=age_to,
                        birth_day=birth_day, 
                        birth_month=birth_month,
                        birth_year=birth_year,
                        online=online,
                        has_photo=has_photo,
                        school_country=school_country,
                        school_city=school_city,
                        school_class=school_class,
                        school=school,
                        school_year=school_year,
                        religion=religion,
                        interests=interests,
                        company=company,
                        position=position,
                        group_id=group_id,
                        v=v
  )
  response <- fromJSON(query)
  response$response
}


#' Возвращает идентификатор пользователя по его тегу
#' 
#' @param tag Тег пользователя
#' @export
tag2Id <-function(tag) {
  getUsers(tag)$id
}


# ????
getAllUsers <- function(user_ids='', fields='', max_requests_per_second = 3, delay_seconds = 1.0) {
  counter <- 0
  from <- 1
  to <- 50
  users <- data.frame()
  
  repeat {
    getted <- getUsers(paste(user_ids[from:to], collapse = ','), fields)
    users <- rbind(users, getted)
    if (nrow(getted) < 50) break
    
    from <- to + 1
    to <- to + 51
    
    counter <- counter + 1
    if (counter %% max_requests_per_second)
      Sys.sleep(delay_seconds)
  }
  
  users
}