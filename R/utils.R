# For more details see \url{http://stackoverflow.com/questions/6451152/how-to-catch-integer0}
# @param x Integer value
# @author Richie Cotton
# is.integer0 <- function(x) {
#   is.integer(x) && length(x) == 0L
# }


#' Predict age for the specified user
#' 
#' @param user_id User ID
#' @export
age_predict <- function(user_id='') {
  friends <- getFriends(user_id=user_id, fields='bdate')$items
  friends$bdate <- as.Date.character(friends$bdate, format="%d.%M.%Y")
  friends <- friends[!is.na(friends$bdate), ]
  friends$year_of_birth <- as.numeric(format(friends$bdate, "%Y"))
  data.frame(uid = user_id, year_of_birth = median(friends$year_of_birth), 
             nfriends = length(friends$year_of_birth))
}


#' Extract URLs from messages
#' 
#' @param messages Array of messages
#' @export
getURLs <- function(messages, message_body=FALSE) {
  # http://stackoverflow.com/questions/26496538/extract-urls-with-regex-into-a-new-data-frame-column
  url_pattern <- "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+"
  match <- regexpr(url_pattern, messages)
  
  if (message_body)
    as.character(messages[match != -1])
  else
    regmatches(messages, match)
}


#' Create post object
#' 
#' @param ... List of attributes
#' @export
vkPost <- function(...)
{
  args <- list(...)[[1]]
  post <- list(id           = args[["id"]],
               from_id      = args[["from_id"]],
               owner_id     = args[["owner_id"]],
               date         = args[["date"]],
               post_type    = args[["post_type"]],
               text         = args[["text"]],
               copy_history = args[["copy_history"]],
               post_source  = args[["post_source"]],
               comments     = args[["comments"]],
               likes        = args[["likes"]],
               reposts      = args[["reposts"]],
               attachments  = args[["attachments"]],
               geo          = args[["geo"]])
  class(post) <- "vkPost"
  return (post)
}