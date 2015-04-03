#' Авторизация и получение ключа доступа
#' 
#' @param client_id Идентификатор приложения
#' @param scope Битовая маска настроек доступа приложения, которые необходимо проверить при авторизации пользователя и запросить, в случае отсутствия необходимых.
#' @param email Номер телефона или почтовый адрес
#' @param password Пароль
#' 
#' @details
#' Ниже перечислен список некоторых из возможных прав доступа, полный список может быть найден на 
#' странице документации \href{https://vk.com/pages?oid=-1&p=Права_доступа_приложений}{Права доступа приложений}:
#' \itemize{
#'   \item \strong{friends} Доступ к друзьям.
#'   \item \strong{photos} Доступ к фотографиям.
#'   \item \strong{audio} Доступ к аудиозаписям.
#'   \item \strong{video} Доступ к видеозаписям.
#'   \item \strong{docs} Доступ к документам.
#'   \item \strong{notes} Доступ заметкам пользователя.
#'   \item \strong{pages} Доступ к wiki-страницам.
#'   \item \strong{status} Доступ к статусу пользователя.
#'   \item \strong{wall} Доступ к обычным и расширенным методам работы со стеной.
#'   \item \strong{groups} Доступ к группам пользователя.
#'   \item \strong{messages} Доступ к расширенным методам работы с сообщениями.
#'   \item \strong{notifications} Доступ к оповещениям об ответах пользователю.
#' }
#' @export
authorize <- function(client_id, scope='friends', email, password) {
  if (missing(client_id)) stop('argument "client_id" is missing, with no default')
  if (!is.numeric(client_id) || floor(client_id) != client_id) stop('argument "client_id" must be an integer value')
  if (!is.character(scope)) stop('argument "scope" must be a string')
    
  auth_url <- paste0('https://oauth.vk.com/authorize?client_id=', client_id,
                     '&redirect_uri=https://oauth.vk.com/blank.hmtl&scope=', scope,
                     '&response_type=token&display=page')
  
  if (!requireNamespace('XML', quietly = TRUE)) {
    browseURL(auth_url)
  } else {
    if (missing(email)) stop('argument "email" is missing, with no default')
    if (!is.character(email)) stop('argument "email" must be a string')
    if (missing(password)) stop('argument "password" is missing, with no default')
    if (!is.character(password)) stop('argument "password" must be a string')
    
    response <- GET(auth_url)
    authorize_form <- htmlParse(rawToChar(response$content))
    hidden_attrs <- xpathSApply(authorize_form, "//form/input", xmlGetAttr, "value")
    token_page <-  POST('https://login.vk.com/?act=login&soft=1&utf8=1', 
                        body=list('_origin'=hidden_attrs[1], 
                                  'ip_h'=hidden_attrs[2],
                                  'to'=hidden_attrs[3],
                                  'email'=email, 
                                  'pass'=password))
    access_token <- sub(".*?access_token=(.*?)&.*", "\\1", token_page$all_headers[[4]]$headers$location)
    setAccessToken(access_token)
  }
}


#' Установить ключ доступа
#' @param access_token Ключ доступа к API
#' @export
setAccessToken <- function(access_token = '') {
  .vkr$access_token <- access_token
}


#' Возвращает ключ доступа
getAccessToken <- function() {
  if (!is.null(.vkr$access_token)) {
    .vkr$access_token
  } else {
    stop("Could not find access token. For more details see ?authorize or ?setAccessToken")
  }
}