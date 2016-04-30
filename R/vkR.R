if (!exists(".vkr")) {
  .vkr <- new.env()
  .vkr$aceess_token <- NULL
  .vkr$api_version <- '5.52'
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