#' Returns data required to show the status of a users and/or communities
#' 
#' @param users_ids User IDs
#' @param groups_ids Community IDs
#' @param progress_bar Display progress bar
#' @param v Version of API
#' @examples
#' \dontrun{
#' status.me <- getStatus()
#' status.friends <- getStatus(users_ids = getFriends()$items)
#' status.groups  <- getStatus(groups_ids = getGroups()$items)
#' status.friends_and_groups <- getStatus(users_ids = getFriends()$items, groups_ids = getGroups()$items, progress_bar = T)
#' }
#' @export
getStatus <- function(users_ids=c(), groups_ids=c(), progress_bar=FALSE, v=getAPIVersion()) {
  get_status <- function(users_ids=c(), groups_ids=c(), v=getAPIVersion()) {
    param_name <- 'user_id'
    objects <- users_ids
    if (length(users_ids) <= 0) {
      param_name <- 'group_id'
      objects <- groups_ids
    }
    
    code <- 'var updates = {}; var query;'
    for (i in 1:length(objects)) {
      code <- paste0(code, 'query = API.status.get({"', param_name, '":"', objects[i], '", "v":"', v, '"}).text; updates.id', objects[i], '=query;')
    }
    code <- paste0(code, 'return updates;')
    response <- execute(code)
    if (!is.null(response)) names(response) <- objects
    
    response
  }
  
  if (length(users_ids) <= 0 & length(groups_ids) <= 0)
    return(execute(paste0('return API.status.get({"v":"', v, '"}).text;')))
  
  max_length <- length(users_ids) + length(groups_ids)
  if (progress_bar) {
    pb <- txtProgressBar(min = 0, max = max_length, style = 3)
    setTxtProgressBar(pb, 0)
  }
  
  all_updates <- list()
  
  # By users
  if (length(users_ids) > 0)
  {
    from <- 1
    to <- 25
    repeat
    {
      if (to >= length(users_ids)) to <- length(users_ids)
      
      updates <- get_status(users_ids = users_ids[from:to], v = v)
      all_updates <- append(all_updates, updates)
      
      if (progress_bar)
        setTxtProgressBar(pb, length(all_updates))
      
      if (to >= length(users_ids))
        break
      
      from <- to + 1
      to <- to + 25
    }
  }
  
  # By groups
  if (length(groups_ids) > 0) {
    from <- 1
    to <- 25
    repeat
    {
      if (to >= length(groups_ids)) to <- length(groups_ids)
      
      updates <- get_status(groups_ids = groups_ids[from:to], v = v)
      all_updates <- append(all_updates, updates)
      
      if (progress_bar)
        setTxtProgressBar(pb, length(all_updates))
      
      if (to >= length(groups_ids))
        break
      
      from <- to + 1
      to <- to + 25
    }
  }
  
  if (progress_bar)
    close(pb)
  
  if (!requireNamespace('reshape2', quietly = TRUE)) {
    all_updates <- do.call(rbind.data.frame, all_updates)
    colnames(all_updates) <- c("status")
    all_updates$id <- rownames(all_updates)
    rownames(all_updates) <- NULL
    return(all_updates)
  }
  
  all_updates <- reshape2::melt(all_updates)
  colnames(all_updates) <- c("status", "id")
  all_updates
}