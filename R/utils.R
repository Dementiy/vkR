#' Building a friend graph
#' 
#' @param users_ids User IDs
#' @export
getNetwork <- function(users_ids='') {
  n <- length(users_ids)
  adjacency_matrix <- data.frame(matrix(data = rep(0, n*n), nrow = n, ncol = n), row.names = users_ids)
  colnames(adjacency_matrix) <- users_ids
  
  mutual_friends <- getMutual(target_uids = paste(users_ids, collapse=","))
  for (friend_id in 1:length(users_ids)) {
    friends <- mutual_friends$common_friends[[friend_id]]
    if (length(friends) > 0) {
      share_friends <- intersect(users_ids, friends)
      if (length(share_friends) > 0) {
        for (shared_user_id in 1:length(share_friends)) {
          #if (is.na(share_friends[shared_user_id])) break
          #if (is.null(share_friends[shared_user_id])) break
          adjacency_matrix[as.character(share_friends[shared_user_id]), as.character(users_ids[friend_id])] <- 1
        }
      }
    }
  }
  
  adjacency_matrix
}


#' Получить список друзей для пользователей, указанных в векторе ids
#' @param ids Вектор со списком пользователей, для которых требуется получить список друзей 
getFriendsBy25 <- function(ids) {
  code <- "var all_friends = {}; var request;"
  for (idx in 1:length(ids)) {
    code <- paste(code, "request=API.friends.get({\"user_id\":", ids[idx], "}); all_friends.user", ids[idx], "=request;", sep="")
  }
  code <- paste(code, "return all_friends;")
  response <- execute(code)
  names(response) <- ids
  response
}


# For more details see \url{http://stackoverflow.com/questions/6451152/how-to-catch-integer0}
# @param x Integer value
# @author Richie Cotton
# is.integer0 <- function(x) {
#   is.integer(x) && length(x) == 0L
# }


#' Создание графа друзей для произвольного списка пользователей
#' 
#' @param users_ids Произвольный список пользователей, по которым требуется построить граф друзей
#' @export
getArbitraryNetwork <- function(users_ids) {
  counter <- 0
  from <- 1
  to <- 25
  users_lists <- list()
  
  repeat {
    # Для пользователей из списка users_ids получаем список друзей
    # Обрабатываем по 25 человек за запрос
    ids <- na.omit(users_ids[from:to])
    users_lists <- append(users_lists, getFriendsBy25(ids))
    
    if (to >= length(users_ids)) break
    
    from <- to + 1
    to <- to + 25
    
    counter <- counter + 1
    if (counter %% 3)
      Sys.sleep(1.0)
  }
  
  # Создаем матрицу смежности
  n <- length(users_lists)
  adjacency_matrix <- data.frame(matrix(data = rep(0, n*n), nrow = n, ncol = n), row.names = users_ids)
  colnames(adjacency_matrix) <- users_ids
  
  # Расставляем связи
  for (user_id in 1:(length(users_ids)-1)) {
    for (current_user_id in (user_id + 1):length(users_ids)) {
      if (users_ids[user_id] %in% users_lists[[current_user_id]]) {
        adjacency_matrix[as.character(users_ids[user_id]), as.character(users_ids[current_user_id])] <- 1
        adjacency_matrix[as.character(users_ids[current_user_id]), as.character(users_ids[user_id])] <- 1
      }
    }
  }
  
  adjacency_matrix
}


#' Прогнозирование возраста указанного пользователя
#' 
#' @param user_id Идентификатор пользователя, для которого необходимо определить возраст
#' @export
age_predict <- function(user_id='') {
  friends <- getFriends(user_id=user_id, fields='bdate')$items
  friends$bdate <- as.Date.character(friends$bdate, format="%d.%M.%Y")
  friends <- friends[!is.na(friends$bdate), ]
  friends$year_of_birth <- as.numeric(format(friends$bdate, "%Y"))
  data.frame(uid = user_id, year_of_birth = median(friends$year_of_birth), 
             nfriends = length(friends$year_of_birth))
}


#' Split messages by days, weeks, months
#' 
#' @param messages List of messages from messagesGet()
#' @param format Character string giving a date-time format as used by strptime
#' @export
messagesSplitByDate <- function(messages, format = "%y-%m-%d") {
  days_list <- format(as.POSIXct(messages$date, origin="1970-01-01"), format = format)
  messages_by_days <- split(messages, as.factor(days_list))
  messages_by_days
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