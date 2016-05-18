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
#' @export
execute <- function(code) {
  request_delay()
  query <- "https://api.vk.com/method/execute"
  post_res <- httr::POST(url = query, 
                   body = list('code' = code, 
                               'access_token' = getAccessToken()))
  response <- jsonlite::fromJSON(rawToChar(post_res$content))
  response$response
}