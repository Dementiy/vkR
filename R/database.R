#' Возвращает список стран
#' 
#' @param need_all 1 - вернуть список всех стран. Флаг, может принимать значения 1 или 0
#' @param code Перечисленные через запятую двухбуквенные коды стран в стандарте ISO 3166-1 alpha-2, для которых необходимо выдать информацию
#' @param offset Отступ, необходимый для выбора определенного подмножества стран
#' @param count Количество стран, которое необходимо вернуть
#' @examples
#' \dontrun{
#' databaseGetCountries(count=234)
#' }
#' @export
databaseGetCountries <- function(need_all='1', code='', offset='', count='100') {
  query <- queryBuilder('database.getCountries',
                        need_all=need_all,
                        code=code,
                        offset=offset,
                        count=count)
  response <- fromJSON(query)
  response$response
}


#' Возвращает список регионов
#' 
#' @param country_id Идентификатор страны, полученный в методе database.getCountries
#' @param q Строка поискового запроса
#' @param offset Отступ, необходимый для выбора определенного подмножества регионов
#' @param count Количество регионов, которое необходимо вернуть
#' @examples
#' \dontrun{
#' databaseGetRegions(229)
#' }
#' @export
databaseGetRegions <- function(country_id='', q='', offset='', count='100') {
  query <- queryBuilder('database.getRegions',
                        country_id=country_id,
                        q=q,
                        offset=offset,
                        count=count)
  response <- fromJSON(query)
  response$response
}


#' Возвращает информацию об улицах по их идентификаторам
#' 
#' @param street_ids Идентификаторы улиц
#' @examples
#' \dontrun{
#' databaseGetStreetsById(1)
#' }
#' @export
databaseGetStreetsById <- function(street_ids='') {
  query <- queryBuilder('database.getStreetsById', street_ids=street_ids)
  response <- fromJSON(query)
  response$response
}


#' Возвращает информацию о странах по их идентификаторам
#' 
#' @param country_ids Идентификаторы стран
#' @examples
#' \dontrun{
#' databaseGetCountriesById('1,2,3,4')
#' }
#' @export
databaseGetCountriesById <- function(country_ids) {
  query <- queryBuilder('database.getCountriesById', country_ids=country_ids)
  response <- fromJSON(query)
  response$response
}


#' Возвращает список городов
#' 
#' @param country_id Идентификатор страны, полученный в методе database.getCountries
#' @param region_id Идентификатор региона, города которого необходимо получить
#' @param q Строка поискового запроса
#' @param need_all 1 – возвращать все города. 0 – возвращать только основные города
#' @param offset Отступ, необходимый для получения определенного подмножества городов
#' @param count Количество городов, которые необходимо вернуть
#' @examples
#' \dontrun{
#' databaseGetCities(country_id=1, need_all=0)
#' }
#' @export
databaseGetCities <- function(country_id='', region_id='', q='', need_all='1', offset='', count='100') {
  query <- queryBuilder('database.getCities',
                        country_id=country_id,
                        region_id=region_id,
                        q=q,
                        need_all=need_all,
                        offset=offset,
                        count=count)
  response <- fromJSON(query)
  response$response
}


#' Возвращает информацию о городах по их идентификаторам
#' 
#' @param city_ids Идентификаторы городов
#' @examples
#' \dontrun{
#' databaseGetCitiesById('1,2')
#' }
#' @export
databaseGetCitiesById <- function(city_ids='') {
  query <- queryBuilder('database.getCitiesById', city_ids=city_ids)
  response <- fromJSON(query)
  response$response
}


#' Возвращает список высших учебных заведений
#' 
#' @param q Строка поискового запроса
#' @param country_id Идентификатор страны, учебные заведения которой необходимо вернуть
#' @param city_id Идентификатор города, учебные заведения которого необходимо вернуть
#' @param offset Отступ, необходимый для получения определенного подмножества учебных заведений
#' @param count Количество учебных заведений, которое необходимо вернуть
#' @examples
#' \dontrun{
#' databaseGetUniversities(city_id = '2')
#' }
#' @export
databaseGetUniversities <- function(q='', country_id='', city_id='', offset='', count='100') {
  query <- queryBuilder('database.getUniversities',
                        q=q,
                        country_id=country_id,
                        city_id=city_id,
                        offset=offset,
                        count=count)
  response <- fromJSON(query)
  response$response
}


#' Возвращает список школ
#' 
#' @param q Строка поискового запроса
#' @param city_id Идентификатор города, школы которого необходимо вернуть
#' @param offset Отступ, необходимый для получения определенного подмножества школ
#' @param count Количество школ, которое необходимо вернуть
#' @examples
#' \dontrun{
#' databaseGetSchools(city_id = 2)
#' }
#' @export
databaseGetSchools <- function(q='', city_id='', offset='', count='100') {
  query <- queryBuilder('database.getSchools',
                        q=q,
                        city_id=city_id,
                        offset=offset,
                        count=count)
  response <- fromJSON(query)
  response$response
}


#' Возвращает список классов, характерных для школ определенной страны (!!!Warning!!!)
#' 
#' @param country_id Идентификатор страны, доступные классы в которой необходимо вернуть
#' @examples
#' \dontrun{
#' databaseGetSchoolClasses(1)
#' }
#' @export
databaseGetSchoolClasses <- function(country_id='') {
  query <- queryBuilder('database.getSchoolClasses', country_id=country_id)
  response <- fromJSON(query)
  response$response
}


#' Возвращает список факультетов
#' 
#' @param university_id Идентификатор университета, факультеты которого необходимо получить
#' @param offset Отступ, необходимый для получения определенного подмножества факультетов
#' @param count Количество факультетов которое необходимо получить
#' @examples
#' \dontrun{
#' databaseGetFaculties(53)
#' }
#' @export
databaseGetFaculties <- function(university_id='', offset='', count='100') {
  query <- queryBuilder('database.getFaculties',
                        university_id=university_id,
                        offset=offset,
                        count=count)
  response <- fromJSON(query)
  response$response
}


#' Возвращает список кафедр университета по указанному факультету
#' 
#' @param faculty_id Идентификатор факультета, кафедры которого необходимо получить
#' @param offset Отступ, необходимый для получения определенного подмножества кафедр
#' @param count Количество кафедр которое необходимо получить
#' @examples
#' \dontrun{
#' databaseGetChairs(206)
#' }
#' @export
databaseGetChairs <- function(faculty_id='', offset='', count='100') {
  query <- queryBuilder('database.getChairs',
                        faculty_id=faculty_id,
                        offset=offset,
                        count=count)
  response <- fromJSON(query)
  response$response
}