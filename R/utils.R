#' Создание графа друзей
#' 
#' @param users_ids Список пользователей, по которым требуется построить граф друзей
#' @export
getNetwork <- function(users_ids='') {
  is.integer0 <- function(x) {
    is.integer(x) && length(x) == 0L
  }
  
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