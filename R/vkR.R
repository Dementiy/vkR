#' Access to VK API via R
#'
#' This package provides a series of functions that allow R users
#' to access VK's API (\url{https://vk.com/dev/methods}) to get
#' information about users, messages, groups, posts and likes.
#'
#' VK (\url{https://vk.com/}) is the largest European online social
#' networking service, based in Russia. It is available in several
#' languages, and is especially popular among Russian-speaking users.
#' VK allows users to message each other publicly or privately, to
#' create groups, public pages and events, share and tag images,
#' audio and video, and to play browser-based games [1].
#'
#' @references [1] \url{https://en.wikipedia.org/wiki/VK_(social_networking)}
#'
#' @seealso \code{\link{vkOAuth}},
#' \code{\link{getUsersExecute}}, \code{\link{getWallExecute}},
#' \code{\link{getFriends}}, \code{\link{getFriendsFor}},
#' \code{\link{getGroupsForUsers}}, \code{\link{getGroupsMembersExecute}},
#' \code{\link{likesGetListForObjects}}, \code{\link{messagesGetHistoryExecute}},
#' \code{\link{getArbitraryNetwork}}, \code{\link{getStatus}}
#'
#' @name vkR
#' @docType package
#' @author Dmitriy Sorokin \email{dementiy@yandex.ru}
NULL


if (!exists(".vkr")) {
  .vkr <- new.env()
  .vkr$access_token <- NULL
  .vkr$api_version <- '5.64'
  .vkr$me <- 0L
  .vkr$last_request_time <- 0
  .vkr$num_requests <- 0
  .vkr$max_requests <- 3

  # Database variables
  .vkr$db_name <- 'vkR_projects'
  .vkr$db_active <- NULL
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
