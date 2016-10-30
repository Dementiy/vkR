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


#' Returns a query string
#'
#' @param method_name Method name
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
  query <- "https://api.vk.com/method/execute"
  body = list('code' = code, 'access_token' = getAccessToken())
  post_res <- httr::POST(url = query, body = append(body, params))
  response <- jsonlite::fromJSON(rawToChar(post_res$content))
  
  if (!is.null(response$error)) {
    if (response$error$error_code == 14) {
      # Captcha handling
      if (!require("jpeq")) stop("The package jpeq was not installed")
      download.file(url = response$error$captcha_img, destfile = 'captcha.jpg', mode = 'wb')
      captcha_img <- readJPEG("captcha.jpg", native = TRUE)
      plot(0:1, 0:1, type = "n", ann = FALSE, axes = FALSE)
      rasterImage(captcha_img, 0, 0, 1, 1)
      
      captcha_sid <- response$error$captcha_sid
      captcha_key <- readline("Enter the key from captcha: ")
      response <- execute(code, params = list('captcha_key' = captcha_key, 'captcha_sid' = captcha_sid))
    } else {
      stop(paste0('Server side error: ', response$error$error_msg))
    }
  }
  
  response$response
}