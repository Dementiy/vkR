#' Returns detailed information on users
#'
#' @param user_ids User IDs or screen names (screen_name). By default, current user ID (the maximum number of elements allowed is 1000)
#' @param fields Profile fields to return
#' @param name_case Case for declension of user name and surname
#' @param v Version of API
#' @examples
#' \dontrun{
#' user <- getUsers('1', fields='sex,bdate,city,country,photo_50,education,interests,music,movies,tv,books,games,about,quotes,personal')
#' }
#' @export
getUsers <- function(user_ids='', fields='', name_case='', v=getAPIVersion()) {
  body <- list(fields=fields, name_case=name_case)
  if (length(user_ids) > 1) {
    user_ids <- paste(user_ids, collapse=",")
    body <- append(body, list(user_ids=user_ids))
    query <- queryBuilder('users.get', v=v)
  } else {
    query <- queryBuilder('users.get', user_ids=user_ids, v=v)
  }
  response <- fromJSON(rawToChar(POST(URLencode(query),
                                      body=body)$content))
  response$response
}


#' Returns detailed information on arbitrary number of users
#'
#' @param user_ids User IDs or screen names (screen_name). By default, current user ID
#' @param fields Profile fields to return
#' @param name_case Case for declension of user name and surname
#' @param v Version of API
#' @examples
#' \dontrun{
#' users <- getUsersExecute(sample(x=seq(1:10000000), size=10000, replace=FALSE), fields='sex,bdate,city')
#' }
#' @export
getUsersExecute <- function(users_ids='', fields='', name_case='', v=getAPIVersion())
{
  get_users <- function(user_ids='', fields='', name_case='', v=getAPIVersion()) {
    code <- 'var users = [];'
    num_requests <- ifelse(length(user_ids) %% 500 == 0, (length(user_ids) %/% 500), (length(user_ids) %/% 500) + 1)
    from <- 1
    to <- ifelse(num_requests >= 2, 500, length(user_ids))
    for (i in 1:num_requests) {
      code <- paste0(code, 'users = users + API.users.get({
                     "user_ids":"', paste0(user_ids[from:to], collapse = ','), '", 
                     "fields":"', fields, '", 
                     "name_case":"', name_case, '", "v":"', v, '"});')
      from <- to + 1
      to <- to + ifelse(length(user_ids) - (to + 500) >= 0, 500, length(user_ids) - to)
    }
    code <- paste0(code, 'return users;')
    if (nchar(code) > 65535) stop("The POST request is limited by 65535 bytes")
    execute(code)
  }
  
  if (is.character(users_ids) && nchar(users_ids) == 0)
    return(getUsers(fields = fields, name_case = name_case, v=v))
  
  all_users <- data.frame()
  counter <- 0
  from <- 1
  to <- 5000
  
  repeat
  {
    if (to >= length(users_ids)) to <- length(users_ids)
    
    users <- get_users(users_ids[from:to], fields = fields, name_case = name_case, v = v)
    all_users <- rbind.pages(list(all_users, users))
    
    if (to >= length(users_ids))
      break
    
    from <- to + 1
    to <- to + 5000
    
    counter <- counter + 1
    if (counter %% 3 == 0)
      Sys.sleep(1.0)
  }
  
  all_users
}


#' Returns a list of users matching the search criteria
#' 
#' @param q Search query string (e.g., Vasya Babich)
#' @param sort Sort order: 1 — by date registered; 0 — by rating 
#' @param offset Offset needed to return a specific subset of users
#' @param count Number of users to return
#' @param fields Profile fields to return
#' @param city City ID
#' @param country Country ID
#' @param hometown City name in a string
#' @param university_country ID of the country where the user graduated
#' @param university ID of the institution of higher education
#' @param university_year Year of graduation from an institution of higher education
#' @param university_faculty Faculty ID
#' @param university_chair Chair ID
#' @param sex 1 — female; 2 — male; 0 — any (default) 
#' @param status Relationship status: 1 — Not married; 2 — In a relationship; 3 — Engaged; 4 — Married; 5 — It's complicated; 6 — Actively searching; 7 — In love 
#' @param age_from Minimum age
#' @param age_to Maximum age
#' @param birth_day Day of birth
#' @param birth_month Month of birth
#' @param birth_year Year of birth
#' @param online 1 — online only; 0 — all users
#' @param has_photo 1 — with photo only; 0 — all users
#' @param school_country ID of the country where users finished school
#' @param school_city ID of the city where users finished school
#' @param school_class
#' @param school ID of the school
#' @param school_year School graduation year
#' @param religion Users' religious affiliation
#' @param interests Users' interests
#' @param company Name of the company where users work
#' @param position Job position
#' @param group_id ID of a community to search in communities
#' @param from_list 
#' @param v Version of API 
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


#' Returns user id by tag
#' 
#' @param tag Tag
#' @export
tag2Id <-function(tag) {
  getUsers(tag)$id
}