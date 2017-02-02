#' Delaying a request if necessary
#'
#' VK can accept maximum 3 requests to API methods per second from a client.
request_delay <- function()
{
  start_time <- Sys.time()
  taken_time <- start_time - .vkr$last_request_time
  if (taken_time <= 1.0 & .vkr$num_requests >= .vkr$max_requests) {
    Sys.sleep(1.0 - taken_time)
  }
  .vkr$num_requests <- ifelse(.vkr$num_requests < 3, .vkr$num_requests + 1, 1)
  .vkr$last_request_time <- Sys.time()
}


#' Get error code from response
#' @param response httr response object
has_error <- function(response) {
  return(ifelse(!is.null(response$error), response$error$error_code, 0))
}


#' Repeat last function call
#' @param params Query params
#' @param n The number of generations to go back
repeat_last_query <- function(params = list(), n = 1) {
  parent_name <- deparse(sys.calls()[[sys.nframe()-n]])
  parent_args <- as.list(sys.frame(-n))
  for (arg in names(params))
    parent_args[[arg]] <- params[[arg]]
  do.call(what = gsub("\\(.*\\)", "", parent_name), args = parent_args)
}


#' Custom error
#' @param message Error message
#' @param call Call expression
#' @param error_code Error code
vk_stop <- function(message = "", call = sys.call(), error_code = "") {
  cond <- structure(list(message = message, call = call),
                    class = c(paste0("vk_error", error_code), "error", "condition"))
  stop(cond)
}


#' Captcha error handler
#' @param error Error object
#' @importFrom graphics plot rasterImage
#' @importFrom utils download.file
handle_captcha <- function(error) {
  if (!interactive())
    stop("Captcha needed.\nFor handle this error you must to interact with your console", call. = FALSE)
  if (!requireNamespace("jpeg", quietly = TRUE)) stop("The package jpeg was not installed")
  download.file(url = error$captcha_img, destfile = 'captcha.jpg', mode = 'wb')
  captcha_img <- jpeg::readJPEG("captcha.jpg", native = TRUE)
  plot(0:1, 0:1, type = "n", ann = FALSE, axes = FALSE)
  rasterImage(captcha_img, 0, 0, 1, 1)
  captcha_sid <- error$captcha_sid
  captcha_key <- readline("Enter the key from captcha: ")
  return(repeat_last_query(params = list('captcha_key' = captcha_key, 'captcha_sid' = captcha_sid), n = 3))
}


#' Validation error handler
#' @param error Error object
handle_validation <- function(error) {
  if (!interactive())
    stop("Required phone number.\nFor handle this error you must to interact with your console", call. = FALSE)
  response <- httr::GET(error$redirect_uri)
  authorize_form <- XML::htmlParse(rawToChar(response$content))
  action <- XML::xpathSApply(authorize_form, "//form", XML::xmlGetAttr, "action")
  if (length(action) != 0 && grepl("security_check", action)) {
    phone <- XML::xpathSApply(authorize_form, "//*/span", XML::xmlValue)
    print(phone)
    missing_numbers <- readline("Enter the missing numbers in the phone number: ")
    response <- httr::GET(paste0("https://m.vk.com",action),
                          query = list('code' = missing_numbers),
                          httr::add_headers('Content-Type' = 'application/x-www-form-urlencoded'))
    for (i in 1:length(response$all_headers)) {
      location <- response$all_headers[[i]]$headers$location
      if (!is.null(location) & grepl("access_token", location)) {
        access_token <- gsub(".*?access_token=(.*?)&.*", "\\1", location)
        setAccessToken(access_token)
        return(repeat_last_query(n = 3))
      }
    }
  }
}


#' Check response for errors
#' @param response httr response object
try_handle_error <- function(response) {
  tryCatch(
    vk_stop(message = response$error$error_msg,
            error_code = response$error$error_code),
    vk_error14 = function(e) return(handle_captcha(response$error)),
    vk_error17 = function(e) return(handle_validation(response$error))
  )
}


#' Returns a query string
#'
#' @param method_name Method name
#' @param ... Method arguments
queryBuilder <- function(method_name, ...) {
  query <- paste("https://api.vk.com/method/", method_name, "?", sep = "")
  arguments <- sapply(substitute(list(...))[-1], deparse)
  arg_names <- names(arguments)
  for (arg_pos in seq(length(arguments))) {
    if (arg_names[arg_pos] != "") {
      if (is.character(arguments[arg_pos])) {
        #arg_value <- gsub("\"", "", arguments[arg_pos])
        arg_value <- list(...)[arg_names[arg_pos]]
      } else {
        # ???
        arg_value <- arguments[arg_pos]
      }
      query <- paste(query, ifelse(arg_value != "", paste("&", arg_names[arg_pos], "=", arg_value, sep = ""), ""), sep = "")
    }
  }
  query <- paste(query, '&access_token=', getAccessToken(), sep = "")
  query
}


#' A universal method for calling a sequence of other methods while saving and filtering interim results
#' @param code Algorithm code in VKScript
#' @param params Parameters list
#' @export
execute <- function(code, params = list()) {
  request_delay()
  query <- "https://api.vk.com/method/execute"
  body = list('code' = code, 'access_token' = getAccessToken())
  post_res <- httr::POST(url = query, body = append(body, params))

  content <- rawToChar(post_res$content)
  if (startsWith(content, "ERROR"))
    vk_stop(message = sprintf("Response error '%s'", content),
            error_code = post_res$status_code)

  response <- jsonlite::fromJSON(content)

  if (has_error(response)) {
    return(try_handle_error(response))
  }

  response$response
}
