if (!exists(".vkr")) {
  .vkr <- new.env()
  .vkr$aceess_token <- NULL
  .vkr$api_version <- '5.53'
  .vkr$me <- 0
  .vkr$last_request_time <- 0
  .vkr$num_requests <- 0
  .vkr$max_requests <- 3
}


getAPIVersion <- function() {
  .vkr$api_version
}


#' Set API version
#' 
#' @param v API version
#' @export
setAPIVersion <- function(v) {
  .vkr$api_version <- v
}