#' Returns detailed information on users
#'
#' @param user_ids User IDs or screen names (screen_name). By default, current user ID (the maximum number of elements allowed is 1000)
#' @param fields Profile fields to return (see fetails for more information about fields)
#' @param name_case Case for declension of user name and surname
#' @param flatten Automatically flatten nested data frames into a single non-nested data frame
#' @param v Version of API
#' @details 
#' \href{https://vk.com/dev/fields}{User object} describes a user profile, contains the following fields:
#' \itemize{
#'   \item \strong{uid} User ID
#'   \item \strong{first_name} First name
#'   \item \strong{last_name} Last name
#'   \item \strong{deactivated} Returns if a profile is deleted or blocked. Gets the value deleted or banned. Keep in mind that in this case no additional fields are returned
#'   \item \strong{hidden: 1} Returns while operating without access_token if a user has set the "Who can see my profile on the Internet" -> "Only VK users" privacy setting. Keep in mind that in this case no additional fields are returned
#'   \item \strong{verified} Returns 1 if the profile is verified, 0 if not
#'   \item \strong{blacklisted} Returns 1 if a current user is in the requested user's blacklist
#'   \item \strong{sex} User sex (1 - female; 2 - male; 0 - not specified)
#'   \item \strong{bdate} User's date of birth.  Returned as DD.MM.YYYY or DD.MM (if birth year is hidden). If the whole date is hidden, no field is returned
#'   \item \strong{city} ID of the city specified on user's page in "Contacts" section.  Returns city ID that can be used to get its name using places.getCityById method. If no city is specified or main information on the page is hidden for in privacy settings, then it returns 0
#'   \item \strong{country} ID of the country specified on user's page in "Contacts" section.  Returns country ID that can be used to get its name using places.getCountryById method. If no country is specified or main information on the page is hidden in privacy settings, then it returns 0
#'   \item \strong{home_town} User's home town
#'   \item \strong{photo_50} Returns URL of square photo of the user with 50 pixels in width.  In case user does not have a photo, http://vk.com/images/camera_c.gif is returned
#'   \item \strong{photo_100} Returns URL of square photo of the user with 100 pixels in width.  In case user does not have a photo, http://vk.com/images/camera_b.gif is returned
#'   \item \strong{photo_200_orig} Returns URL of user's photo with 200 pixels in width.  In case user does not have a photo, http://vk.com/images/camera_a.gif is returned
#'   \item \strong{photo_200} Returns URL of square photo of the user with 200 pixels in width.  If the photo was uploaded long time ago, there can be no image of such size and in this case the reply will not include this field
#'   \item \strong{photo_400_orig} Returns URL of user's photo with 400 pixels in width.  If user does not have a photo of such size, reply will not include this field
#'   \item \strong{photo_max} Returns URL of square photo of the user with maximum width. Can be returned a photo both 200 and 100 pixels in width.  In case user does not have a photo, http://vk.com/images/camera_b.gif is returned
#'   \item \strong{photo_max_orig} Returns URL of user's photo of maximum size. Can be returned a photo both 400 and 200 pixels in width.  In case user does not have a photo, http://vk.com/images/camera_a.gif is returned
#'   \item \strong{online} Information whether the user is online.  Returned values: 1 - online, 0 - offline.  If user utilizes a mobile application or site mobile version, it returns online_mobile additional field that includes 1. With that, in case of application, online_app additional field is returned with application ID.
#'   \item \strong{lists} Information about friend lists. Returns IDs of friend lists the user is member of, separated with a comma. The field is available for friends.get method only. To get information about ID and names of friend lists use friends.getLists method. If user is not a member of any friend list, then when accepting data in XML format the respective <user> node does not contain <lists> tag
#'   \item \strong{domain} Page screen name.  Returns a string with a page screen name (only subdomain is returned, like andrew). If not set, "id'+uid is returned, e.g. id35828305
#'   \item \strong{has_mobile} Information whether the user's mobile phone number is available.  Returned values: 1 - available, 0 - not available.  We recommend you to use it prior to call of secure.sendSMSNotification method
#'   \item \strong{contacts} Information about user's phone numbers.  If data are available and not hidden in privacy settings, the following fields are returned (mobile_phone - user's mobile phone number (only for standalone applications); home_phone - user's additional phone number)
#'   \item \strong{site} Returns a website address from a user profile
#'   \item \strong{education} Information about user's higher education institution. The following fields are returned:
#'   \itemize{
#'     \item \strong{university} University ID
#'     \item \strong{university_name} University name
#'     \item \strong{faculty} Faculty ID
#'     \item \strong{faculty_name} Faculty name
#'     \item \strong{graduation} Graduation year
#'   }
#'   \item \strong{universities} List of higher education institutions where user studied.  Returns universities array with university objects with the following fields:
#'   \itemize{
#'     \item \strong{id} University ID
#'     \item \strong{country} ID of the country the university is located in
#'     \item \strong{city} ID of the city the university is located in
#'     \item \strong{name} University name
#'     \item \strong{faculty} Faculty ID
#'     \item \strong{faculty_name} Faculty name
#'     \item \strong{chair} University chair ID
#'     \item \strong{chair_name} Chair name
#'     \item \strong{graduation} Graduation year
#'   }
#'   \item \strong{schools} List of schools where user studied in.  Returns schools array with school objects with the following fields:
#'   \itemize{
#'     \item \strong{id} School ID
#'     \item \strong{country} ID of the country the school is located in
#'     \item \strong{city} ID of the city the school is located in
#'     \item \strong{name} School name
#'     \item \strong{year_from} Year the user started to study
#'     \item \strong{year_to} Year the user finished to study
#'     \item \strong{year_graduated} Graduation year
#'     \item \strong{class} School class letter
#'     \item \strong{speciality} Speciality
#'     \item \strong{type} Type ID
#'     \item \strong{type_str} Type name
#'   }
#'   \item \strong{status} User status.  Returns a string with status text that is in the profile below user's name
#'   \item \strong{last_seen} Last visit date.  Returns last_seen object with the following fields:
#'   \itemize{
#'     \item \strong{time} Last visit date (in Unix time)
#'     \item \strong{platform} Type of the platform that used for the last authorization. See more at \href{https://vk.com/dev/using_longpoll}{Using LongPoll server}
#'   }
#'   \item \strong{followers_count} Number of user's followers 
#'   \item \strong{common_count} Number of common friends with a current user
#'   \item \strong{counters} Number of various objects the user has.  Can be used in users.get method only when requesting information about a user. Returns an object with fields:
#'   \itemize{
#'     \item \strong{albums} Number of photo albums
#'     \item \strong{videos} Number of videos
#'     \item \strong{audios} Number of audios
#'     \item \strong{notes} Number of notes
#'     \item \strong{friends} Number of friends
#'     \item \strong{groups} Number of communities
#'     \item \strong{online_friends} Number of online friends
#'     \item \strong{mutual_friends} Number of mutual friends
#'     \item \strong{user_videos} Number of videos the user is tagged on
#'     \item \strong{followers} Number of followers
#'     \item \strong{user_photos} Number of photos the user is tagged on
#'     \item \strong{subscriptions} Number of subscriptions
#'   }
#'   \item \strong{occupation} Current user's occupation. Returns following fields:
#'   \itemize{
#'     \item \strong{type} Can take the values: work, school, university
#'     \item \strong{id} ID of school, university, company group (the one a user works in)
#'     \item \strong{name} Name of school, university or work place
#'   }
#'   \item \strong{nickname} User nickname
#'   \item \strong{relatives} Current user's relatives list. Returns a list of objects with id and type fields (name instead of id if a relative is not a VK user). type - relationship type. Possible values:
#'   \itemize{
#'     \item \emph{sibling}
#'     \item \emph{parent}
#'     \item \emph{child}
#'     \item \emph{grandparent}
#'     \item \emph{grandchild}
#'   }
#'   \item \strong{relation} User relationship status. Returned values:
#'   \itemize{
#'     \item \strong{1} - Single
#'     \item \strong{2} - In a relationship
#'     \item \strong{3} - Engaged
#'     \item \strong{4} - Married
#'     \item \strong{5} - It's complicated
#'     \item \strong{6} - Actively searching
#'     \item \strong{7} - In love
#'   }
#'   \item \strong{personal} Information from the "Personal views" section
#'   \itemize{
#'     \item \strong{political} Political views:
#'     \itemize{
#'       \item{1} - Communist
#'       \item{2} - Socialist
#'       \item{3} - Moderate
#'       \item{4} - Liberal
#'       \item{5} - Conservative
#'       \item{6} - Monarchist
#'       \item{7} - Ultraconservative
#'       \item{8} - Apathetic
#'       \item{9} - Libertian
#'     }
#'     \item \strong{langs} Languages
#'     \item \strong{religion} World view
#'     \item \strong{inspired_by} Inspired by
#'     \item \strong{people_main} Improtant in others:
#'     \itemize{
#'       \item{1} - Intellect and creativity
#'       \item{2} - Kindness and honesty
#'       \item{3} - Health and beauty
#'       \item{4} - Wealth and power
#'       \item{5} - Courage and persistance
#'       \item{6} - Humor and love for life
#'     }
#'     \item \strong{life_main} Personal priority:
#'     \itemize{
#'       \item{1} - Family and children
#'       \item{2} - Career and money
#'       \item{3} - Entertainment and leisure
#'       \item{4} - Science and research
#'       \item{5} - Improving the world
#'       \item{6} - Personal development
#'       \item{7} - Beauty and art
#'       \item{8} - Fame and influence
#'     }
#'     \item \strong{smoking} Views on smoking (1 - very negative; 2 - negative; 3 - neutral; 4 - compromisable; 5 - positive)
#'     \item \strong{alcohol} Views on alcohol (1 - very negative; 2 - negative; 3 - neutral; 4 - compromisable; 5 - positive)
#'   }
#'   \item \strong{connections} Returns specified services such as: skype, facebook, twitter, livejournal, instagram
#'   \item \strong{exports} External services with export configured (twitter, facebook, livejournal, instagram)
#'   \item \strong{wall_comments} Wall comments allowed(1 - allowed, 0 - not allowed)
#'   \item \strong{activities} Activities
#'   \item \strong{interests} Interests
#'   \item \strong{music} Favorite music
#'   \item \strong{movies} Favorite movies
#'   \item \strong{tv} Favorite TV shows
#'   \item \strong{books} Favorite books
#'   \item \strong{games} Favorite games
#'   \item \strong{about} "About me"
#'   \item \strong{quotes} Favorite quotes
#'   \item \strong{can_post} Can post on the wall: 1 - allowed, 0 - not allowed
#'   \item \strong{can_see_all_posts} Can see other users' posts on the wall: 1 - allowed, 0 - not allowed
#'   \item \strong{can_see_audio} Can see other users' audio on the wall: 1 - allowed, 0 - not allowed
#'   \item \strong{can_write_private_message} Can write private messages to a current user: 1 - allowed, 0 - not allowed
#'   \item \strong{timezone} user time zone. Retuns only while requesting current user info
#'   \item \strong{screen_name} User page's screen name (subdomain)
#' }
#' @examples
#' \dontrun{
#' user <- getUsers('1', fields='sex,bdate,city')
#' }
#' @export
getUsers <- function(user_ids='', fields='', name_case='', flatten=FALSE, v=getAPIVersion()) {
  .Deprecated("getUsersExecute()")
  body <- list(fields = fields, name_case = name_case)
  if (length(user_ids) > 1) {
    user_ids <- paste(user_ids, collapse = ",")
    body <- append(body, list(user_ids = user_ids))
    query <- queryBuilder('users.get', v = v)
  } else {
    query <- queryBuilder('users.get', user_ids = user_ids, v = v)
  }
  request_delay()
  response <- jsonlite::fromJSON(rawToChar(httr::POST(URLencode(query),
                                                      body = body)$content))
  
  if (has_error(response))
    return(try_handle_error(response))
  
  response <- response$response
  
  if (isTRUE(flatten))
    response <- jsonlite::flatten(response)
  
  class(response) <- c(class(response), "vk.users")
  response
}


#' Returns detailed information on arbitrary number of users
#'
#' @param users_ids User IDs or screen names (screen_name). By default, current user ID
#' @param fields Profile fields to return
#' @param name_case Case for declension of user name and surname
#' @param drop Drop deleted or banned users
#' @param flatten Automatically flatten nested data frames into a single non-nested data frame
#' @param progress_bar Display progress bar
#' @param v Version of API
#' @details 
#' \href{https://vk.com/dev/fields}{User object} describes a user profile, contains the following fields:
#' \itemize{
#'   \item \strong{uid} User ID
#'   \item \strong{first_name} First name
#'   \item \strong{last_name} Last name
#'   \item \strong{deactivated} Returns if a profile is deleted or blocked. Gets the value deleted or banned. Keep in mind that in this case no additional fields are returned
#'   \item \strong{hidden: 1} Returns while operating without access_token if a user has set the "Who can see my profile on the Internet" -> "Only VK users" privacy setting. Keep in mind that in this case no additional fields are returned
#'   \item \strong{verified} Returns 1 if the profile is verified, 0 if not
#'   \item \strong{blacklisted} Returns 1 if a current user is in the requested user's blacklist
#'   \item \strong{sex} User sex (1 - female; 2 - male; 0 - not specified)
#'   \item \strong{bdate} User's date of birth.  Returned as DD.MM.YYYY or DD.MM (if birth year is hidden). If the whole date is hidden, no field is returned
#'   \item \strong{city} ID of the city specified on user's page in "Contacts" section.  Returns city ID that can be used to get its name using places.getCityById method. If no city is specified or main information on the page is hidden for in privacy settings, then it returns 0
#'   \item \strong{country} ID of the country specified on user's page in "Contacts" section.  Returns country ID that can be used to get its name using places.getCountryById method. If no country is specified or main information on the page is hidden in privacy settings, then it returns 0
#'   \item \strong{home_town} User's home town
#'   \item \strong{photo_50} Returns URL of square photo of the user with 50 pixels in width.  In case user does not have a photo, http://vk.com/images/camera_c.gif is returned
#'   \item \strong{photo_100} Returns URL of square photo of the user with 100 pixels in width.  In case user does not have a photo, http://vk.com/images/camera_b.gif is returned
#'   \item \strong{photo_200_orig} Returns URL of user's photo with 200 pixels in width.  In case user does not have a photo, http://vk.com/images/camera_a.gif is returned
#'   \item \strong{photo_200} Returns URL of square photo of the user with 200 pixels in width.  If the photo was uploaded long time ago, there can be no image of such size and in this case the reply will not include this field
#'   \item \strong{photo_400_orig} Returns URL of user's photo with 400 pixels in width.  If user does not have a photo of such size, reply will not include this field
#'   \item \strong{photo_max} Returns URL of square photo of the user with maximum width. Can be returned a photo both 200 and 100 pixels in width.  In case user does not have a photo, http://vk.com/images/camera_b.gif is returned
#'   \item \strong{photo_max_orig} Returns URL of user's photo of maximum size. Can be returned a photo both 400 and 200 pixels in width.  In case user does not have a photo, http://vk.com/images/camera_a.gif is returned
#'   \item \strong{online} Information whether the user is online.  Returned values: 1 - online, 0 - offline.  If user utilizes a mobile application or site mobile version, it returns online_mobile additional field that includes 1. With that, in case of application, online_app additional field is returned with application ID.
#'   \item \strong{lists} Information about friend lists. Returns IDs of friend lists the user is member of, separated with a comma. The field is available for friends.get method only. To get information about ID and names of friend lists use friends.getLists method. If user is not a member of any friend list, then when accepting data in XML format the respective <user> node does not contain <lists> tag
#'   \item \strong{domain} Page screen name.  Returns a string with a page screen name (only subdomain is returned, like andrew). If not set, "id'+uid is returned, e.g. id35828305
#'   \item \strong{has_mobile} Information whether the user's mobile phone number is available.  Returned values: 1 - available, 0 - not available.  We recommend you to use it prior to call of secure.sendSMSNotification method
#'   \item \strong{contacts} Information about user's phone numbers.  If data are available and not hidden in privacy settings, the following fields are returned (mobile_phone - user's mobile phone number (only for standalone applications); home_phone - user's additional phone number)
#'   \item \strong{site} Returns a website address from a user profile
#'   \item \strong{education} Information about user's higher education institution. The following fields are returned:
#'   \itemize{
#'     \item \strong{university} University ID
#'     \item \strong{university_name} University name
#'     \item \strong{faculty} Faculty ID
#'     \item \strong{faculty_name} Faculty name
#'     \item \strong{graduation} Graduation year
#'   }
#'   \item \strong{universities} List of higher education institutions where user studied.  Returns universities array with university objects with the following fields:
#'   \itemize{
#'     \item \strong{id} University ID
#'     \item \strong{country} ID of the country the university is located in
#'     \item \strong{city} ID of the city the university is located in
#'     \item \strong{name} University name
#'     \item \strong{faculty} Faculty ID
#'     \item \strong{faculty_name} Faculty name
#'     \item \strong{chair} University chair ID
#'     \item \strong{chair_name} Chair name
#'     \item \strong{graduation} Graduation year
#'   }
#'   \item \strong{schools} List of schools where user studied in.  Returns schools array with school objects with the following fields:
#'   \itemize{
#'     \item \strong{id} School ID
#'     \item \strong{country} ID of the country the school is located in
#'     \item \strong{city} ID of the city the school is located in
#'     \item \strong{name} School name
#'     \item \strong{year_from} Year the user started to study
#'     \item \strong{year_to} Year the user finished to study
#'     \item \strong{year_graduated} Graduation year
#'     \item \strong{class} School class letter
#'     \item \strong{speciality} Speciality
#'     \item \strong{type} Type ID
#'     \item \strong{type_str} Type name
#'   }
#'   \item \strong{status} User status.  Returns a string with status text that is in the profile below user's name
#'   \item \strong{last_seen} Last visit date.  Returns last_seen object with the following fields:
#'   \itemize{
#'     \item \strong{time} Last visit date (in Unix time)
#'     \item \strong{platform} Type of the platform that used for the last authorization. See more at \href{https://vk.com/dev/using_longpoll}{Using LongPoll server}
#'   }
#'   \item \strong{followers_count} Number of user's followers 
#'   \item \strong{common_count} Number of common friends with a current user
#'   \item \strong{counters} Number of various objects the user has.  Can be used in users.get method only when requesting information about a user. Returns an object with fields:
#'   \itemize{
#'     \item \strong{albums} Number of photo albums
#'     \item \strong{videos} Number of videos
#'     \item \strong{audios} Number of audios
#'     \item \strong{notes} Number of notes
#'     \item \strong{friends} Number of friends
#'     \item \strong{groups} Number of communities
#'     \item \strong{online_friends} Number of online friends
#'     \item \strong{mutual_friends} Number of mutual friends
#'     \item \strong{user_videos} Number of videos the user is tagged on
#'     \item \strong{followers} Number of followers
#'     \item \strong{user_photos} Number of photos the user is tagged on
#'     \item \strong{subscriptions} Number of subscriptions
#'   }
#'   \item \strong{occupation} Current user's occupation. Returns following fields:
#'   \itemize{
#'     \item \strong{type} Can take the values: work, school, university
#'     \item \strong{id} ID of school, university, company group (the one a user works in)
#'     \item \strong{name} Name of school, university or work place
#'   }
#'   \item \strong{nickname} User nickname
#'   \item \strong{relatives} Current user's relatives list. Returns a list of objects with id and type fields (name instead of id if a relative is not a VK user). type - relationship type. Possible values:
#'   \itemize{
#'     \item \emph{sibling}
#'     \item \emph{parent}
#'     \item \emph{child}
#'     \item \emph{grandparent}
#'     \item \emph{grandchild}
#'   }
#'   \item \strong{relation} User relationship status. Returned values:
#'   \itemize{
#'     \item \strong{1} - Single
#'     \item \strong{2} - In a relationship
#'     \item \strong{3} - Engaged
#'     \item \strong{4} - Married
#'     \item \strong{5} - It's complicated
#'     \item \strong{6} - Actively searching
#'     \item \strong{7} - In love
#'   }
#'   \item \strong{personal} Information from the "Personal views" section
#'   \itemize{
#'     \item \strong{political} Political views:
#'     \itemize{
#'       \item{1} - Communist
#'       \item{2} - Socialist
#'       \item{3} - Moderate
#'       \item{4} - Liberal
#'       \item{5} - Conservative
#'       \item{6} - Monarchist
#'       \item{7} - Ultraconservative
#'       \item{8} - Apathetic
#'       \item{9} - Libertian
#'     }
#'     \item \strong{langs} Languages
#'     \item \strong{religion} World view
#'     \item \strong{inspired_by} Inspired by
#'     \item \strong{people_main} Improtant in others:
#'     \itemize{
#'       \item{1} - Intellect and creativity
#'       \item{2} - Kindness and honesty
#'       \item{3} - Health and beauty
#'       \item{4} - Wealth and power
#'       \item{5} - Courage and persistance
#'       \item{6} - Humor and love for life
#'     }
#'     \item \strong{life_main} Personal priority:
#'     \itemize{
#'       \item{1} - Family and children
#'       \item{2} - Career and money
#'       \item{3} - Entertainment and leisure
#'       \item{4} - Science and research
#'       \item{5} - Improving the world
#'       \item{6} - Personal development
#'       \item{7} - Beauty and art
#'       \item{8} - Fame and influence
#'     }
#'     \item \strong{smoking} Views on smoking (1 - very negative; 2 - negative; 3 - neutral; 4 - compromisable; 5 - positive)
#'     \item \strong{alcohol} Views on alcohol (1 - very negative; 2 - negative; 3 - neutral; 4 - compromisable; 5 - positive)
#'   }
#'   \item \strong{connections} Returns specified services such as: skype, facebook, twitter, livejournal, instagram
#'   \item \strong{exports} External services with export configured (twitter, facebook, livejournal, instagram)
#'   \item \strong{wall_comments} Wall comments allowed(1 - allowed, 0 - not allowed)
#'   \item \strong{activities} Activities
#'   \item \strong{interests} Interests
#'   \item \strong{music} Favorite music
#'   \item \strong{movies} Favorite movies
#'   \item \strong{tv} Favorite TV shows
#'   \item \strong{books} Favorite books
#'   \item \strong{games} Favorite games
#'   \item \strong{about} "About me"
#'   \item \strong{quotes} Favorite quotes
#'   \item \strong{can_post} Can post on the wall: 1 - allowed, 0 - not allowed
#'   \item \strong{can_see_all_posts} Can see other users' posts on the wall: 1 - allowed, 0 - not allowed
#'   \item \strong{can_see_audio} Can see other users' audio on the wall: 1 - allowed, 0 - not allowed
#'   \item \strong{can_write_private_message} Can write private messages to a current user: 1 - allowed, 0 - not allowed
#'   \item \strong{timezone} user time zone. Retuns only while requesting current user info
#'   \item \strong{screen_name} User page's screen name (subdomain)
#' }
#' @examples
#' \dontrun{
#' random_ids <- sample(x=seq(1:10000000), size=10000, replace=FALSE)
#' users <- getUsersExecute(random_ids, fields='sex,bdate,city')
#' }
#' @export
getUsersExecute <- function(users_ids, fields='', name_case='', drop=FALSE, flatten=FALSE, progress_bar=FALSE, v=getAPIVersion())
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
  
  if (missing(users_ids)) {
    code <- paste0('return API.users.get({"fields":"', fields, '", "name_case":"', name_case, '", "v":"', v, '"});')
    response <- execute(code)
    if (isTRUE(flatten))
      response <- jsonlite::flatten(response)
    return(response)
  }
  
  if ("vk.friends.ids" %in% class(users_ids))
    users_ids <- unique(unlist(users_ids))
  users_ids <- as.integer(users_ids)
  
  all_users <- data.frame()
  from <- 1
  to <- 5000
  
  if (progress_bar) {
    pb <- txtProgressBar(min = 0, max = length(users_ids), style = 3)
    setTxtProgressBar(pb, 0)
  }
  
  repeat
  {
    if (to >= length(users_ids)) to <- length(users_ids)
    
    users <- get_users(users_ids[from:to], fields = fields, name_case = name_case, v = v)
    all_users <- jsonlite::rbind.pages(list(all_users, users))
    
    if (progress_bar)
      setTxtProgressBar(pb, nrow(all_users))
    
    if (to >= length(users_ids))
      break
    
    from <- to + 1
    to <- to + 5000
  }
  
  if (progress_bar)
    close(pb)
  
  if (isTRUE(drop) && "deactivated" %in% colnames(all_users)) {
    all_users <- subset(all_users, is.na(deactivated))
    all_users$deactivated <- NULL
  }
  
  if (isTRUE(flatten))
    all_users <- jsonlite::flatten(all_users)
  
  class(all_users) <- c(class(all_users), "vk.users")
  all_users
}


#' Returns a list of IDs of followers of the user in question, sorted by date added, most recent first
#' 
#' @param user_id User ID
#' @param offset Offset needed to return a specific subset of followers
#' @param count Number of followers to return
#' @param fields Profile fields to return
#' @param name_case Case for declension of user name and surname
#' @param drop Drop deleted or banned followers
#' @param flatten Automatically flatten nested data frames into a single non-nested data frame
#' @param progress_bar Display progress bar
#' @param v Version of API
#' @export
usersGetFollowers <- function(user_id='', offset=0, count=0, fields='', name_case='', drop=FALSE, flatten=FALSE, progress_bar=FALSE, v=getAPIVersion())
{
  get_followers <- function(user_id='', offset=0, count=0, fields='', name_case='', v=getAPIVersion())
  {
    code <- 'var followers = [];'
    num_requests <- 0
    while (num_requests != 25 && count != 0)
    {
      current_count <- ifelse((count - 1000) >= 0, 1000, count)
      code <- paste0(code, 'followers = followers + API.users.getFollowers({"user_id":"', user_id, 
                     '", "offset":"', offset,
                     '", "count":"', current_count,
                     '", "fields":"', fields, 
                     '", "name_case":"', name_case, 
                     '", "v":"', v, '"}).items;')
      offset <- offset + 1000
      num_requests <- num_requests + 1
      count <- count - current_count
    }
    code <- paste0(code, 'return followers;')
    execute(code)
  }
  
  if (isTRUE(drop) && fields == '')
    fields <- 'deactivated'
  
  user_id <- as.integer(user_id)
  code <- paste0('return API.users.getFollowers({"user_id":"', user_id, '", 
                 "offset":"', offset, '",
                 "count":"', 1, '",
                 "fields":"', fields, '",
                 "name_case":"', name_case, '",
                 "v":"', v, '"});')
  
  response <- execute(code)
  followers <- response$items
  max_count <- ifelse((response$count - offset) > count & count != 0, count, response$count - offset)
  
  if (max_count == 0)
    return(list(followers = response$items, 
                count = response$count))
  
  len <- ifelse(is.vector(followers), length, nrow)
  
  offset_counter <- 0
  
  if (progress_bar) {
    pb <- txtProgressBar(min = 0, max = max_count, style = 3)
    setTxtProgressBar(pb, len(followers))
  }
  
  while (len(followers) < max_count) {
    followers3000 <- get_followers(user_id = user_id,
                                  offset = (1 + offset + offset_counter * 3000), 
                                  count = (max_count - len(followers)), 
                                  fields = fields,
                                  name_case = name_case,
                                  v = v)
    if (is.vector(followers))
      followers <- append(followers, followers3000)
    else
      followers <- jsonlite::rbind.pages(list(followers, followers3000))
    
    if (progress_bar)
      setTxtProgressBar(pb, len(followers))
    
    offset_counter <- offset_counter + 1
  }
  
  if (progress_bar)
    close(pb)
  
  if (isTRUE(drop) && "deactivated" %in% colnames(followers)) {
    followers <- subset(followers, is.na(deactivated))
    followers$deactivated <- NULL
    rownames(followers) <- NULL
  }
  
  if (isTRUE(flatten) & !is.vector(followers))
    followers <- jsonlite::flatten(followers)
  
  list(followers = followers, 
       count = response$count)
  
}


#' Returns a list of IDs of users and communities followed by the user
#' 
#' @param user_id User ID
#' @param offset Offset needed to return a specific subset of subscriptions
#' @param count Number of users and communities to return
#' @param fields Profile fields to return
#' @param extended 1 - to return a combined list of users and communities, 0 - to return separate lists of users and communities
#' @param flatten Automatically flatten nested data frames into a single non-nested data frame
#' @param progress_bar Display progress bar
#' @param v Version of API
#' @export
usersGetSubscriptions <- function(user_id='', extended='1', offset=0, count=0, fields='', flatten=FALSE, progress_bar=FALSE, v=getAPIVersion())
{
  get_subscriptions <- function(user_id='', extended='', offset='', count='', fields='', v=getAPIVersion())
  {
    code <- 'var subscriptions = [];'
    num_requests <- 0
    while (num_requests != 25 && count != 0)
    {
      current_count <- ifelse((count - 200) >= 0, 200, count)
      code <- paste0(code, 'subscriptions = subscriptions + API.users.getSubscriptions({"user_id":"', user_id, 
                     '", "offset":"', offset,
                     '", "count":"', current_count,
                     '", "fields":"', fields, 
                     '", "extended":"', extended, 
                     '", "v":"', v, '"}).items;')
      offset <- offset + 200
      num_requests <- num_requests + 1
      count <- count - current_count
    }
    code <- paste0(code, 'return subscriptions;')
    execute(code)
  }
  
  code <- paste0('return API.users.getSubscriptions({"user_id":"', user_id, '", 
                 "extended":"', 1, '",
                 "offset":"', offset, '",
                 "count":"', 1, '",
                 "fields":"', fields, '",
                 "v":"', v, '"});')
  
  response <- execute(code)
  subscriptions <- response$items
  max_count <- ifelse((response$count - offset) > count & count != 0, count, response$count - offset)
  
  if (max_count == 0)
    return(list(subscriptions = subscriptions,
                count = response$count))
  
  offset_counter <- 0
  
  if (progress_bar) {
    pb <- txtProgressBar(min = 0, max = max_count, style = 3)
    setTxtProgressBar(pb, nrow(subscriptions))
  }
  
  while (nrow(subscriptions) < max_count) {
    subscriptions600 <- get_subscriptions(user_id = user_id,
                                          offset = (1 + offset + offset_counter * 600), 
                                          count = (max_count - nrow(subscriptions)), 
                                          fields = fields,
                                          extended = 1,
                                          v = v)
    subscriptions <- jsonlite::rbind.pages(list(subscriptions, subscriptions600))
    
    if (progress_bar)
      setTxtProgressBar(pb, nrow(subscriptions))
    
    offset_counter <- offset_counter + 1
  }
  
  if (progress_bar)
    close(pb)
  
  if (isTRUE(flatten))
    subscriptions <- jsonlite::flatten(subscriptions)
  
  if (as.numeric(extended) == 0) {
    subscriptions_splt <- split(subscriptions, subscriptions$type == 'page')
    groups <- subscriptions_splt$`TRUE`
    users <- subscriptions_splt$`FALSE`
    return(list(groups = groups, users=users))
  }
  
  list(subscriptions = subscriptions, 
       count = response$count)
}


#' Returns a list of users matching the search criteria
#' 
#' @param q Search query string (e.g., Vasya Babich)
#' @param sort Sort order: 1 - by date registered; 0 - by rating 
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
#' @param sex 1 - female; 2 - male; 0 - any (default) 
#' @param status Relationship status: 1 - Not married; 2 - In a relationship; 3 - Engaged; 4 - Married; 5 - It's complicated; 6 - Actively searching; 7 - In love 
#' @param age_from Minimum age
#' @param age_to Maximum age
#' @param birth_day Day of birth
#' @param birth_month Month of birth
#' @param birth_year Year of birth
#' @param online 1 - online only; 0 - all users
#' @param has_photo 1 - with photo only; 0 - all users
#' @param school_country ID of the country where users finished school
#' @param school_city ID of the city where users finished school
#' @param school_class Positive number
#' @param school ID of the school
#' @param school_year School graduation year
#' @param religion Users' religious affiliation
#' @param interests Users' interests
#' @param company Name of the company where users work
#' @param position Job position
#' @param group_id ID of a community to search in communities
#' @param from_list List of comma-separated words
#' @param flatten Automatically flatten nested data frames into a single non-nested data frame
#' @param v Version of API 
#' @export
usersSearch <- function(q='', sort='', offset='', count='20', fields='', city='', country='', hometown='', 
                        university_country='', university='', university_year='', university_faculty='', university_chair='', 
                        sex='', status='', age_from='', age_to='', birth_day='', birth_month='', birth_year='',
                        online='', has_photo='', school_country='', school_city='', school_class='', school='', school_year='',
                        religion='', interests='', company='', position='', group_id='', from_list='', flatten=FALSE, v=getAPIVersion()) {
  query <- queryBuilder('users.search',
                        q = q,
                        sort = sort,
                        offset = offset,
                        count = count,
                        fields = fields,
                        city = city,
                        country = country,
                        hometown = hometown,
                        university_country = university_country,
                        university = university,
                        university_year = university_year,
                        university_faculty = university_faculty,
                        university_chair = university_chair,
                        sex = sex,
                        status = status,
                        age_from = age_from,
                        age_to = age_to,
                        birth_day = birth_day, 
                        birth_month = birth_month,
                        birth_year = birth_year,
                        online = online,
                        has_photo = has_photo,
                        school_country = school_country,
                        school_city = school_city,
                        school_class = school_class,
                        school = school,
                        school_year = school_year,
                        religion = religion,
                        interests = interests,
                        company = company,
                        position = position,
                        group_id = group_id,
                        from_list = from_list,
                        v = v
  )
  request_delay()
  response <- jsonlite::fromJSON(query)
  
  if (has_error(response))
    return(try_handle_error(response))
  
  response <- response$response
  
  if (isTRUE(flatten) && response$count > 0)
    response$items <- jsonlite::flatten(response$items)
  
  response
}


#' Returns user id by tag
#' 
#' @param tag Tag
#' @export
tag2Id <- function(tag) {
  suppressWarnings(getUsers(tag)$id)
}


#' Returns current user ID
#' 
#' @export 
me <- function()
{
  if (.vkr$me == 0)
    .vkr$me <- suppressWarnings(getUsers()$id)
  .vkr$me
}