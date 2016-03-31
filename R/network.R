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


#' Building a friend graph for an arbitrary list of users
#' 
#' @param users_ids User IDs
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
