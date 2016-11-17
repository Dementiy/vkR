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
  friends <- getFriends(user_id = user_id, fields = 'bdate')$items
  friends$bdate <- as.Date.character(friends$bdate, format = "%d.%M.%Y")
  friends <- friends[!is.na(friends$bdate), ]
  friends$year_of_birth <- as.numeric(format(friends$bdate, "%Y"))
  data.frame(uid = user_id, year_of_birth = median(friends$year_of_birth), 
             nfriends = length(friends$year_of_birth))
}


#' Extract URLs from messages
#' 
#' @param messages Array of messages
#' @param message_body 
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


#' Apply a method over a vector of objects
#' 
#' Returns a data frame of the same number of rows as length of `objs`, each element of which is the 
#' result of applying `method` to the corresponding element of `objs` 
#' @param objs A vector of objects
#' @param method The function to be applied to each element of `objs`
#' @examples
#' \dontrun{
#'  users <- vkApply(c("",1234567), function(user) getUsers(user, fields="sex"))
#'  countries <- vkApply(c(2,5122182,1906578), getCountryByCityId)
#' }
#' @export
vkApply <- function(objs, method)
{
  res <- data.frame()
  for (obj in objs)
    res <- jsonlite::rbind.pages(list(res, method(obj)))
  res
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
  return(post)
}


# Functions for NLP

#' Get stop words list for russian language
#' @param stop_words User defined stop words
#' @export
get_stop_words <- function(stop_words = c()) {
  tm_stop_words <- c()
  if (require("tm"))
      tm_stop_words <- tm::stopwords('russian')
  
  google_stop_words <- c()
  filename <- system.file("extdata", "stop_words_russian.txt", package = 'vkR')
  if (file.exists(filename))
    google_stop_words <- as.vector(read.table(filename)$V1)
  
  stop_words <- unique(c(stop_words, google_stop_words, tm_stop_words))
  stop_words
}


#' Clear text
#' @param lines List of lines
#' @param patterns List of user defined patterns
#' @export
clear_text <- function(lines, patterns = list()) {
  if (!require("stringr")) stop("The package stringr was not installed")
  lines <- stringr::str_replace_all(lines, "[ё]", "е")
  lines <- stringr::str_replace_all(lines, "[[:punct:]]", " ")
  lines <- stringr::str_replace_all(lines, "[[:digit:]]", " ")
  lines <- stringr::str_replace_all(lines, "http\\S+\\s*", " ")
  lines <- stringr::str_replace_all(lines, "[a-zA-Z]", " ")
  
  if (is.list(patterns) & length(patterns)) {
    for (pattern in patterns) {
      if (length(pattern) > 1)
        lines <- stringr::str_replace_all(lines, pattern[1], pattern[2])
      else
        lines <- stringr::str_replace_all(lines, pattern, " ")
    }
  }
  
  lines <- stringr::str_replace_all(lines, "\\s+", " ")
  lines <- tolower(lines)
  lines <- stringr::str_trim(lines, side = "both")
  lines
}