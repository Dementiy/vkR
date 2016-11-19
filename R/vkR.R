if (!exists(".vkr")) {
  .vkr <- new.env()
  .vkr$aceess_token <- NULL
  .vkr$api_version <- '5.57'
  .vkr$me <- 0
  .vkr$last_request_time <- 0
  .vkr$num_requests <- 0
  .vkr$max_requests <- 3
  .vkr$db_active <- NULL
  .vkr$db_name <- 'vkR_projects'
  .vkr$db_meta_name <- 'meta_collection'
  .vkr$db_metadata <- NULL
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