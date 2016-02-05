if (!exists(".vkr")) {
  .vkr <- new.env()
  .vkr$aceess_token <- NULL
  .vkr$api_version <- '5.44'
}

getAPIVersion <- function() {
  .vkr$api_version
}

setAPIVersion <- function(new_version) {
  .vkr$api_version <- new_version
}