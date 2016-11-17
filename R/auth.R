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
    
    response <- httr::GET(auth_url)
    
    for (i in 1:2) {
      authorize_form <- XML::htmlParse(rawToChar(response$content))
      form_attrs <- XML::xpathSApply(authorize_form, "//form/input", XML::xmlGetAttr, "value")
      response <- httr::POST('https://login.vk.com/?act=login&soft=1&utf8=1', 
                       body = list('_origin' = form_attrs[1], 
                                 'ip_h'    = form_attrs[2],
                                 'lg_h'    = form_attrs[3],
                                 'to'      = form_attrs[4],
                                 'email'   = email, 
                                 'pass'    = password), 
                       encode = 'form' , httr::config(followlocation = 0L))
      response <- httr::GET('https://login.vk.com/?act=login&soft=1&utf8=1', 
                      query = list('_origin' = form_attrs[1], 
                                   'ip_h'    = form_attrs[2],
                                   'lg_h'    = form_attrs[3],
                                   'to'      = form_attrs[4],
                                   'email'   = email, 
                                   'pass'    = password), 
                      httr::add_headers('Content-Type' = 'application/x-www-form-urlencoded'))
    }
    
    authorize_form <- XML::htmlParse(rawToChar(response$content))
    action <- XML::xpathSApply(authorize_form, "//form", XML::xmlGetAttr, "action")
    
    if (length(action))
      response <- httr::GET(action, httr::add_headers('Content-Type' = 'application/x-www-form-urlencoded'))
    
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


#' Client authorization (for web application)
#'
#' @param app_name Application name
#' @param client_id Application ID
#' @param client_secret Application secret key
#' @export
vkOAuthWeb <- function(app_name, client_id, client_secret) {
  if (!requireNamespace("httpuv", quietly = TRUE)) stop("The package httpuv was not installed")
  
  if (missing(app_name)) stop('argument "app_name" is missing, with no default')
  if (!is.character(app_name)) stop('argument "app_name" must be a string')
  if (missing(client_id)) stop('argument "client_id" is missing, with no default')
  if (!is.numeric(client_id) || floor(client_id) != client_id) stop('argument "client_id" must be an integer value')
  if (missing(client_secret)) stop('argument "client_secret" is missing, with no default')
  if (!is.character(client_secret)) stop('argument "client_secret" must be a string')
  
  accessURL <- "https://oauth.vk.com/access_token"
  authURL <- "https://oauth.vk.com/authorize"
  vk <- httr::oauth_endpoint(authorize = authURL,
                       access = accessURL)
  vk_app <- httr::oauth_app(app_name, client_id, client_secret)
  ig_oauth <- httr::oauth2.0_token(vk,
                             vk_app,
                             type = 'application/x-www-form-urlencoded',
                             cache = FALSE)
  my_session <-  strsplit(toString(names(ig_oauth$credentials)), '"')
  access_token <- paste0('access_token=', my_session[[1]][4])
  
  setAccessToken(access_token)
}


#' Set access token
#' @param access_token Access token
#' @export
setAccessToken <- function(access_token = '') {
  .vkr$access_token <- access_token
}


#' Get access token
#' @export
getAccessToken <- function() {
  if (!is.null(.vkr$access_token)) {
    .vkr$access_token
  } else {
    stop("Could not find access token. For more details see ?vkOAuth or ?setAccessToken")
  }
}