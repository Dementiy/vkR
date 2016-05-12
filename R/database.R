#' Returns a list of countries
#' 
#' @param need_all 1 - to return a full list of all countries; 0 - to return a list of countries near the current user's country
#' @param code Country codes in ISO 3166-1 alpha-2 standard
#' @param offset Offset needed to return a specific subset of countries
#' @param count Number of countries to return
#' @param v Version of API
#' @examples
#' \dontrun{
#' databaseGetCountries(count=234)
#' }
#' @export
databaseGetCountries <- function(need_all='1', code='', offset='', count='100', v=getAPIVersion()) {
  query <- queryBuilder('database.getCountries',
                        need_all = need_all,
                        code = code,
                        offset = offset,
                        count = count,
                        v = v)
  response <- jsonlite::fromJSON(query)
  response$response
}


#' Returns a list of regions
#' 
#' @param country_id Country ID, received in database.getCountries method
#' @param q Search query
#' @param offset Offset needed to return specific subset of regions
#' @param count Number of regions to return
#' @param v Version of API
#' @examples
#' \dontrun{
#' databaseGetRegions(229)
#' }
#' @export
databaseGetRegions <- function(country_id='', q='', offset='', count='100', v=getAPIVersion()) {
  query <- queryBuilder('database.getRegions',
                        country_id = country_id,
                        q = q,
                        offset = offset,
                        count = count,
                        v = v)
  response <- jsonlite::fromJSON(query)
  response$response
}


#' Returns information about streets by their IDs
#' 
#' @param street_ids Street IDs
#' @param v Version of API
#' @examples
#' \dontrun{
#' databaseGetStreetsById(1)
#' }
#' @export
databaseGetStreetsById <- function(street_ids='', v=getAPIVersion()) {
  query <- queryBuilder('database.getStreetsById', street_ids = street_ids, v = v)
  response <- jsonlite::fromJSON(query)
  response$response
}


#' Returns information about countries by their IDs
#' 
#' @param country_ids Country IDs
#' @param v Version of API
#' @examples
#' \dontrun{
#' databaseGetCountriesById('1,2,3,4')
#' }
#' @export
databaseGetCountriesById <- function(country_ids, v=getAPIVersion()) {
  query <- queryBuilder('database.getCountriesById', country_ids = country_ids, v = v)
  response <- jsonlite::fromJSON(query)
  response$response
}


#' Returns a list of cities
#' 
#' @param country_id Country ID
#' @param region_id Region ID
#' @param q Search query
#' @param need_all 1 – to return all cities in the country; 0 – to return major cities in the country (default)
#' @param offset Offset needed to return a specific subset of cities
#' @param count Number of cities to return
#' @param v Version of API
#' @examples
#' \dontrun{
#' databaseGetCities(country_id=1, need_all=0)
#' }
#' @export
databaseGetCities <- function(country_id='', region_id='', q='', need_all='1', offset='', count='100', v=getAPIVersion()) {
  query <- queryBuilder('database.getCities',
                        country_id = country_id,
                        region_id = region_id,
                        q = q,
                        need_all = need_all,
                        offset = offset,
                        count = count,
                        v = v)
  response <- jsonlite::fromJSON(query)
  response$response
}


#' Returns information about cities by their IDs
#' 
#' @param city_ids City IDs
#' @param v Version of API
#' @examples
#' \dontrun{
#' databaseGetCitiesById('1,2')
#' }
#' @export
databaseGetCitiesById <- function(city_ids='', v=getAPIVersion()) {
  query <- queryBuilder('database.getCitiesById', city_ids = city_ids, v = v)
  response <- jsonlite::fromJSON(query)
  response$response
}


#' Returns a list of higher education institutions
#' 
#' @param q Search query
#' @param country_id Country ID
#' @param city_id City ID
#' @param offset Offset needed to return a specific subset of universities
#' @param count Number of universities to return
#' @param v Version of API
#' @examples
#' \dontrun{
#' databaseGetUniversities(city_id = '2')
#' }
#' @export
databaseGetUniversities <- function(q='', country_id='', city_id='', offset='', count='100', v=getAPIVersion()) {
  query <- queryBuilder('database.getUniversities',
                        q = q,
                        country_id = country_id,
                        city_id = city_id,
                        offset = offset,
                        count = count,
                        v = v)
  response <- jsonlite::fromJSON(query)
  response$response
}


#' Returns a list of schools
#' 
#' @param q Search query
#' @param city_id City ID
#' @param offset Offset needed to return a specific subset of schools
#' @param count Number of schools to return
#' @param v Version of API
#' @examples
#' \dontrun{
#' databaseGetSchools(city_id = 2)
#' }
#' @export
databaseGetSchools <- function(q='', city_id='', offset='', count='100', v=getAPIVersion()) {
  query <- queryBuilder('database.getSchools',
                        q = q,
                        city_id = city_id,
                        offset = offset,
                        count = count,
                        v = v)
  response <- jsonlite::fromJSON(query)
  response$response
}


#' Returns a list of available classes
#' 
#' @param country_id Country ID
#' @param v Version of API
#' @examples
#' \dontrun{
#' databaseGetSchoolClasses(1)
#' }
#' @export
databaseGetSchoolClasses <- function(country_id='', v=getAPIVersion()) {
  query <- queryBuilder('database.getSchoolClasses', country_id = country_id, v = v)
  response <- jsonlite::fromJSON(query)
  response$response
}


#' Returns a list of faculties (i.e., university departments)
#' 
#' @param university_id University ID
#' @param offset Offset needed to return a specific subset of faculties
#' @param count Number of faculties to return
#' @param v Version of API
#' @examples
#' \dontrun{
#' databaseGetFaculties(53)
#' }
#' @export
databaseGetFaculties <- function(university_id='', offset='', count='100', v=getAPIVersion()) {
  query <- queryBuilder('database.getFaculties',
                        university_id = university_id,
                        offset = offset,
                        count = count,
                        v = v)
  response <- jsonlite::fromJSON(query)
  response$response
}


#' Returns list of chairs on a specified faculty
#' 
#' @param faculty_id ID of the faculty to get chairs from 
#' @param offset Offset required to get a certain subset of chairs 
#' @param count Amount of chairs to get
#' @param v Version of API
#' @examples
#' \dontrun{
#' databaseGetChairs(206)
#' }
#' @export
databaseGetChairs <- function(faculty_id='', offset='', count='100', v=getAPIVersion()) {
  query <- queryBuilder('database.getChairs',
                        faculty_id = faculty_id,
                        offset = offset,
                        count = count,
                        v = v)
  response <- jsonlite::fromJSON(query)
  response$response
}


#' Get country ID and title by given city ID
#'  
#' @param city_id City ID
#' @export
getCountryByCityId <- function(city_id)
{
  res <- usersSearch(q = "", fields = "country", city = city_id, count = 1)
  if (res$count == 0)
    stop("No users from this city")
  res <- res$items
  if (length(res) == 0) # Бывает, что возвращается пустой ответ при count=1
    res <- usersSearch(q = "", fields = "country", city = city_id, count = 2)$items
  res$country
}