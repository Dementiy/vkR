#' Clinet authorization
#' 
#' @param client_id Application ID
#' @param scope Requested application access permissions (see below).
#' @param email Email or phone number
#' @param password Password
#' 
#' @details
#' List of Available Settings of \href{https://vk.com/dev/permissions}{Access Permissions}:
#' \itemize{
#'   \item \strong{friends} Access to friends.
#'   \item \strong{photos} Access to photos.
#'   \item \strong{audio} Access to audios.
#'   \item \strong{video} Access to videos.
#'   \item \strong{docs} Access to documents.
#'   \item \strong{notes} Access to user notes.
#'   \item \strong{pages} Access to wiki pages.
#'   \item \strong{status} Access to user status.
#'   \item \strong{wall} Access to standard and advanced methods for the wall.
#'   \item \strong{groups} Access to user groups.
#'   \item \strong{messages} Access to advanced methods for messaging.
#'   \item \strong{notifications} Access to notifications about answers to the user.
#' }
#' @export
vkOAuth <- function(client_id, scope='friends', email, password) {
  if (missing(client_id)) stop('argument "client_id" is missing, with no default')
  if (!is.numeric(client_id) || floor(client_id) != client_id) stop('argument "client_id" must be an integer value')
  if (!is.character(scope)) stop('argument "scope" must be a string')
    
  auth_url <- paste0('https://oauth.vk.com/authorize?client_id=', client_id,
                     '&redirect_uri=https://oauth.vk.com/blank.hmtl&scope=', scope,
                     '&response_type=token&display=page')
  
  if ((missing(email) && missing(password)) || !requireNamespace('XML', quietly = TRUE)) {
    browseURL(auth_url)
  } else {
    if (missing(email)) stop('argument "email" is missing, with no default')
    if (!is.character(email)) stop('argument "email" must be a string')
    if (missing(password)) stop('argument "password" is missing, with no default')
    if (!is.character(password)) stop('argument "password" must be a string')
    
    response <- GET(auth_url)
    
    for (i in 1:2) { # Only God knows why
      authorize_form <- htmlParse(rawToChar(response$content))
      form_attrs <- xpathSApply(authorize_form, "//form/input", xmlGetAttr, "value")
      response <- POST('https://login.vk.com/?act=login&soft=1&utf8=1', 
                       body=list('_origin'=form_attrs[1], 
                                 'ip_h'=form_attrs[2],
                                 'lg_h'=form_attrs[3],
                                 'to'=form_attrs[4],
                                 'email'=email, 
                                 'pass'=password), 
                       encode='form')
    }
    
    if (response$url != "https://oauth.vk.com/blank.html") {
      accept_perms_page <- htmlParse(rawToChar(response$content))
      response <- POST(xmlGetAttr(getNodeSet(accept_perms_page, "//form")[[1]], "action"))
    }
    
    for (i in 1:length(response$all_headers)) {
      location <- response$all_headers[[i]]$headers$location
      if (!is.null(location) & grepl("access_token", location)) {
        access_token <- gsub(".*?access_token=(.*?)&.*", "\\1", location)
        break
      }
    }
    
    setAccessToken(access_token)
  }
}


#' Set access token
#' @param access_token Access token
#' @export
setAccessToken <- function(access_token = '') {
  .vkr$access_token <- access_token
}


#' Get access token
getAccessToken <- function() {
  if (!is.null(.vkr$access_token)) {
    .vkr$access_token
  } else {
    stop("Could not find access token. For more details see ?authorize or ?setAccessToken")
  }
}