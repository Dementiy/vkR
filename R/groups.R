#' Returns a list of the communities to which a user belongs
#'
#' @param user_id User ID
#' @param extended 1 - to return complete information about a user's communities; 0 - to return a list of community IDs without any additional fields (default)
#' @param filter Types of communities to return: admin, editor, moder, groups, publics, events
#' @param fields List of additional fields to be returned
#' @param offset Offset needed to return a specific subset of communities
#' @param count Number of communities to return (maximum value 1000)
#' @param v Version of API
#' @examples
#' \dontrun{
#' groups <- getGroups(me(), extended = 1, fields = 'city')
#' }
#' @export
getGroups <- function(user_id='', extended='', filter='', fields='', offset='', count='', v=getAPIVersion()) {
  query <- queryBuilder('groups.get',
                        user_id = user_id,
                        extended = extended,
                        filter = filter,
                        fields = fields,
                        offset = offset,
                        count = count,
                        v = v)
  request_delay()
  response <- jsonlite::fromJSON(query)

  if (has_error(response))
    return(try_handle_error(response))

  response$response
}

#' Returns a list of community members
#'
#' @param group_id ID or screen name of the community
#' @param sort Sort order
#' @param offset Offset needed to return a specific subset of community members
#' @param count Number of community members to return (maximum value 1000)
#' @param fields List of additional fields to be returned
#' @param filter friends - only friends in this community will be returned; unsure - only those who pressed 'I may attend' will be returned (if it's an event)
#' @param v Version of API
#' @examples
#' \dontrun{
#' members <- getGroupsMembers(1, fields='sex,bdate,city')
#' }
#' @export
getGroupsMembers <- function(group_id='', sort='', offset='', count='', fields='', filter='', v=getAPIVersion()) {
  query <- queryBuilder('groups.getMembers',
                        group_id = group_id,
                        sort = sort,
                        offset = offset,
                        count = count,
                        fields = fields,
                        filter = filter,
                        v = v)
  request_delay()
  response <- jsonlite::fromJSON(query)

  if (has_error(response))
    return(try_handle_error(response))

  response$response
}


#' Returns a list of community members
#'
#' @param group_id ID or screen name of the community
#' @param sort Sort order. Available values: id_asc, id_desc, time_asc, time_desc. time_asc and time_desc are availavle only if the method is called by the group's moderator
#' @param fields List of additional fields to be returned
#' @param filter friends - only friends in this community will be returned; unsure - only those who pressed 'I may attend' will be returned (if it's an event)
#' @param flatten Automatically flatten nested data frames into a single non-nested data frame
#' @param progress_bar Display progress bar
#' @param v Version of API
#' @importFrom utils setTxtProgressBar txtProgressBar
#' \dontrun{
#' members <- getGroupsMembersExecute(1, fields='sex,bdate,city', progress_bar = TRUE)
#' }
#' @export
getGroupsMembersExecute <- function(group_id='', sort='', fields='', filter='', flatten=FALSE, progress_bar=FALSE, v=getAPIVersion())
{
  getGroupsMembers20 <- function(group_id='', sort='', offset = 0, fields='', filter='', v=getAPIVersion())
  {
    code <- 'var groups_members = [];'
    code <- paste0(code, 'groups_members = groups_members + API.groups.getMembers({"group_id":"', group_id,
                   '", "sort":"', sort,
                   '", "offset":"', offset,
                   '", "fields":"', fields,
                   '", "filter":"', filter,
                   '", "v":"', v, '"}).items;')
    code <- paste0(code, 'var offset = 1000;
                   while (offset < 25000 && groups_members.length >= offset)
                   {
                   groups_members = groups_members + API.groups.getMembers({"group_id":"', group_id,
                   '", "sort":"', sort,
                   '", "fields":"', fields,
                   '", "filter":"', filter,
                   '", "v":"', v,
                   '", "offset":(offset+',offset,')}).items;
                   offset = offset + 1000;
                   };
                   return groups_members;')
    execute(code)
  }

  code <- paste0('return API.groups.getMembers({"group_id":"', group_id, '", "sort":"', sort, '", "fields":"', fields, '", "filter":"', filter, '", "v":"', v, '"});')
  response <- execute(code)

  members <- response$items
  len <- ifelse(is.vector(members), length, nrow)
  count <- response$count

  if (progress_bar) {
    pb <- txtProgressBar(min = 0, max = count, style = 3)
    setTxtProgressBar(pb, ifelse(is.vector(members), len(members), nrow(members)))
  }

  while (len(members) < count)
  {
    members20 <- getGroupsMembers20(group_id = group_id,
                                  sort = sort,
                                  offset = len(members),
                                  fields = fields,
                                  filter = filter,
                                  v = v)
    if (is.vector(members))
      members <- append(members, members20)
    else
      members <- jsonlite::rbind.pages(list(members, members20))

    if (progress_bar)
      setTxtProgressBar(pb, ifelse(is.vector(members), len(members), nrow(members)))
  }

  if (progress_bar)
    close(pb)

  if (isTRUE(flatten) & !is.vector(members))
    members <- jsonlite::flatten(members)

  members
}


#' Returns a list of the communities for the specified users
#'
#' @param users A list of users
#' @param extended 1 - to return complete information about a user's communities; 0 - to return a list of community IDs without any additional fields (default)
#' @param filter Types of communities to return: admin, editor, moder, groups, publics, events
#' @param fields List of additional fields to be returned
#' @param progress_bar Display progress bar
#' @param v Version of API
#' @importFrom utils setTxtProgressBar txtProgressBar
#' \dontrun{
#' members <- getGroupsForUsers(c(me(), 123456), extended = 1, fields='city', progress_bar = TRUE)
#' }
#' @export
getGroupsForUsers <- function(users, extended='', filter='', fields='', progress_bar = FALSE, v = getAPIVersion()) {
  get_groups_for_users <- function(users, extended='', filter='', fields='', offset='', v = getAPIVersion())
  {
    num_requests <- ceiling(length(users) / 25)
    from <- 1
    to <- 25
    groups <- list()
    for (i in 1:num_requests) {
      code <- 'var groups_per_user = {}; var groups;'
      if (to > length(users))
        to <- length(users)
      for (index in from:to) {
        code <- paste0(code, 'groups = API.groups.get({
                       "user_id":"', users[index], '",
                       "extended":"', extended, '",
                       "filter":"', filter, '",
                       "fields":"', fields, '",
                       "offset":"', offset, '",
                       "count":"', 1000, '",
                       "v":"', v, '"}).items;
                       groups_per_user.user', users[index], "=groups;", sep = "")
    }
      code <- paste0(code, 'return groups_per_user;')
      groups <- append(groups, execute(code))
      from <- from + 25
      to <- to + 25
    }
    names(groups) <- users
    groups
  }

  if ("vk.users" %in% class(users))
    users <- users$id

  groups <- list()
  from <- 1
  max_count <- length(users)
  to <- ifelse(max_count >= 75, 75, max_count)

  if (progress_bar) {
    pb <- txtProgressBar(min = 0, max = length(users), style = 3)
    setTxtProgressBar(pb, 0)
  }

  repeat {
    groups75 <- get_groups_for_users(users = users[from:to],
                                     extended = extended,
                                     filter = filter,
                                     fields = fields,
                                     v = v)
    groups <- append(groups, groups75)

    if (progress_bar)
      setTxtProgressBar(pb, length(groups))

    if (to >= max_count)
      break

    from <- to + 1
    to <- ifelse(to + 75 >= max_count, max_count, to + 75)
  }

  if (progress_bar)
    close(pb)

  class(groups) <- c(class(groups), "vk.groups_per_user")
  groups
}
