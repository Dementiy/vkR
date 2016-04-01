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
#' @param format Either "edgelist" for a list of edges or "adjmatrix" for an adjacency matrix
#' @export
getArbitraryNetwork <- function(users_ids, format='edgelist') {
  if (!require("reshape2")) stop("The package reshape2 was not installed")
  if (!require("dplyr")) stop("The package dplyr was not installed")
  
  users_lists <- getFriendsFor(users_ids)
  edge_list <- melt(users_lists)
  colnames(edge_list) <- c("from", "to")
  edge_list <- edge_list %>% filter(from %in% users_ids & to %in% users_ids)
  edge_list$from <- as.character(edge_list$from)
  edge_list$to <- as.character(edge_list$to)
  
  if (format == 'edgelist') return(edge_list)
  
  n <- length(users_ids)
  adjacency_matrix <- matrix(0, nrow = n, ncol = n)
  rownames(adjacency_matrix) <- colnames(adjacency_matrix) <- users_ids
  adjacency_matrix[as.matrix(edge_list)[,1:2]] <- 1
  adjacency_matrix
}
