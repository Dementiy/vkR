#' Building a friend graph
#'
#' @param users_ids User IDs
#' @export
getEgoNetwork <- function(users_ids='') {
  n <- length(users_ids)
  adjacency_matrix <- data.frame(matrix(data = rep(0, n*n), nrow = n, ncol = n), row.names = users_ids)
  colnames(adjacency_matrix) <- users_ids

  mutual_friends <- getMutualExecute(target_uids = users_ids)
  for (friend_id in 1:length(users_ids)) {
    friends <- mutual_friends$common_friends[[friend_id]]
    if (length(friends) > 0) {
      share_friends <- intersect(users_ids, friends)
      if (length(share_friends) > 0) {
        for (shared_user_id in 1:length(share_friends)) {
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
  if (!requireNamespace("reshape2", quietly = TRUE)) stop("The package reshape2 was not installed")
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("The package dplyr was not installed")

  users_lists <- getFriendsFor(users_ids)
  users_lists <- users_lists[!sapply(users_lists, is.null)]
  users_lists <- users_lists[!!sapply(users_lists, length)]
  edge_list <- reshape2::melt(users_lists)
  colnames(edge_list) <- c("from", "to")
  edge_list <- dplyr::filter(edge_list, edge_list$from %in% users_ids & edge_list$to %in% users_ids)
  edge_list$from <- as.character(edge_list$from)
  edge_list$to <- as.character(edge_list$to)

  if (format == 'edgelist') return(edge_list)

  n <- length(users_ids)
  adjacency_matrix <- matrix(0, nrow = n, ncol = n)
  rownames(adjacency_matrix) <- colnames(adjacency_matrix) <- users_ids
  adjacency_matrix[as.matrix(edge_list)[,1:2]] <- 1
  adjacency_matrix
}


#' Returns a list of paths between two users
#'
#' @param source_id Source ID
#' @param target_id Target ID
#' @param are_friends By default is FALSE
#' @param max_depth Maximum depth
#' @importFrom utils head
#' @export
getPaths <- function(source_id, target_id, are_friends = FALSE, max_depth = 5)
{
  # List of visited users
  visited <- c()

  predictCity <- function(user_id)
  {
    names(sort(table(getFriends(user_id, fields = "city")$items$city$id), decreasing = T)[1])
  }

  findIntersection <- function(friends_sid, friends_tid, depth)
  {
    if (nrow(friends_sid) == 0)
      stop(paste0('Number of friends for ', source_id,' is 0'))
    if (nrow(friends_tid) == 0)
      stop(paste0('Number of friends for ', target_id,' is 0'))
    if (depth > max_depth)
      stop("Sorry, the path is too long")

    print(paste0("Current depth is ", depth))
    # for R CMD check to pass
    city.id <- id <- deactivated <- NULL
    friends_sid$lists <- NULL
    friends_tid$lists <- NULL
    friends_sid <- as.data.frame.list(friends_sid)
    friends_tid <- as.data.frame.list(friends_tid)

    friends_sid_filtered <- subset(friends_sid, city.id == city_tid)
    friends_tid_filtered <- subset(friends_tid, city.id == city_sid)

    if (nrow(friends_sid_filtered) > 0) friends_sid <- friends_sid_filtered
    if (nrow(friends_tid_filtered) > 0) friends_tid <- friends_tid_filtered

    friends_sid <- subset(friends_sid, !(id %in% visited))
    friends_tid <- subset(friends_tid, !(id %in% visited))

    friends_sid <- getFriendsFor(unique(friends_sid$id))
    friends_tid <- getFriendsFor(unique(friends_tid$id))

    friends_lists_sid[[depth]] <<- friends_sid
    friends_lists_tid[[depth]] <<- friends_tid

    shared_friends <- unique(intersect(unlist(friends_sid), unlist(friends_tid)))
    if (length(shared_friends) > 0)
    {
      print(paste0("Number of possible paths is ", length(shared_friends)))
      return(list(sid = friends_lists_sid, tid = friends_lists_tid, shared_friends = shared_friends))
    }

    visited <<- unique(c(visited, unlist(names(friends_sid)), unlist(names(friends_tid))))

    friends_sid <- getUsersExecute(unlist(friends_sid), fields = "city")
    if ("deactivated" %in% names(friends_sid))
      friends_sid <- subset(friends_sid, deactivated != "deleted" | is.na(deactivated))

    friends_tid <- getUsersExecute(unlist(friends_tid), fields = "city")
    if ("deactivated" %in% names(friends_tid))
      friends_tid <- subset(friends_tid, deactivated != "deleted" | is.na(deactivated))

    findIntersection(friends_sid, friends_tid, depth + 1)
  }

  extractPaths <- function(users_lists, shared_friends)
  {
    extractPathsFor <- function(user_id, paths, depth)
    {
      path <- c(user_id)
      if (depth == 1) return(path)
      for (name in names(paths[[depth]]))
        if (user_id %in% paths[[depth]][[name]])
          path <- c(path, extractPathsFor(name, paths, depth - 1))
        path
    }

    splitPaths <- function(ids, depth)
    {
      corePath <- head(ids, depth - 1)
      splittedPaths <- list()
      for (i in 1:(length(ids) - depth + 1)) {
        splittedPaths[[i]] <- c(corePath, ids[i + length(corePath)])
      }
      splittedPaths
    }

    extracted_paths <- list()
    depth <- length(users_lists)
    for (shared_friend in shared_friends)
    {
      path <- extractPathsFor(shared_friend, users_lists, depth)
      if (length(path) > depth) {
        path <- splitPaths(path, depth)
        extracted_paths <- append(extracted_paths, path)
      } else {
        extracted_paths <- append(extracted_paths, list(path))
      }
    }

    extracted_paths
  }

  if ((are_friends == FALSE) & areFriends(target_id, source_id))
    return(c(source_id, target_id))

  source_id_info <- getUsers(user_ids = c(source_id), fields = 'city')
  target_id_info <- getUsers(user_ids = c(target_id), fields = 'city')
  if ("deactivated" %in% names(source_id_info))
    if (source_id_info$deactivated == "banned" | source_id_info$deactivated == "deleted")
      stop(paste0('User ID ', source_id,' is banned or deleted'))
  if ("deactivated" %in% names(target_id_info))
    if (target_id_info$deactivated == "banned" | target_id_info$deactivated == "deleted")
      stop(paste0('User ID ', target_id,' is banned or deleted'))

  city_sid <- source_id_info$city$id
  city_tid <- target_id_info$city$id

  if (is.null(city_sid)) city_sid <- predictCity(source_id)
  if (is.null(city_tid)) city_tid <- predictCity(target_id)

  friends_sid <- getFriends(source_id, fields = "city")$items
  friends_tid <- getFriends(target_id, fields = "city")$items

  if (length(friends_sid) == 0)
    stop(paste0('Number of friends for ', source_id,' is 0'))
  if (length(friends_tid) == 0)
    stop(paste0('Number of friends for ', target_id,' is 0'))

  friends_lists_sid <- list()
  friends_lists_sid[[1]] <- friends_sid$id
  friends_lists_tid <- list()
  friends_lists_tid[[1]] <- friends_tid$id

  shared_friends <- unique(intersect(friends_sid$id, friends_tid$id))
  if (are_friends == TRUE)
    visited <- c(shared_friends)
  else if (length(shared_friends) > 0)
  {
    print(paste0("Number of possible paths is ", length(shared_friends)))
    return(list(sid = friends_lists_sid, tid = friends_lists_tid, shared_friends = shared_friends))
  }

  # for R CMD check to pass
  deactivated <- NULL
  if ("deactivated" %in% names(friends_sid))
    friends_sid <- subset(friends_sid, deactivated != "deleted" | is.na(deactivated))
  if ("deactivated" %in% names(friends_tid))
    friends_tid <- subset(friends_tid, deactivated != "deleted" | is.na(deactivated))

  visited <- c(visited, source_id, target_id)
  paths <- findIntersection(friends_sid, friends_tid, depth = 2)

  shared_friends <- paths$shared_friends[paths$shared_friends != 0]
  source_paths <- extractPaths(paths$sid, shared_friends)
  target_paths <- extractPaths(paths$tid, shared_friends)
  source_paths <- data.frame(do.call(rbind, source_paths))
  target_paths <- data.frame(do.call(rbind, target_paths))
  colnames(source_paths) <- c('mutual_friend', length(paths$sid):2)
  colnames(target_paths) <- c('mutual_friend', length(paths$tid):2)
  source_paths$mutual_friend <- as.character(source_paths$mutual_friend)
  target_paths$mutual_friend <- as.character(target_paths$mutual_friend)
  all_paths <- merge(source_paths, target_paths, by = 'mutual_friend')

  ncols <- ncol(all_paths)
  ordered_paths <- all_paths[, c(ceiling(ncols/2):2,1,ncols:(ncols/2 + 1))]
  ordered_paths$source <- rep(source_id, nrow(all_paths))
  ordered_paths$target <- rep(target_id, nrow(all_paths))
  ordered_paths <- ordered_paths[, c(ncols + 1, 1:ncols, ncols + 2)]

  ordered_paths
}
