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
#' @param offset Offset needed to return a specific subset of community members
#' @param count Number of community members to  (0 - get all community members)
#' @param fields List of additional fields to be returned
#' @param filter friends - only friends in this community will be returned; unsure - only those who pressed 'I may attend' will be returned (if it's an event)
#' @param flatten Automatically flatten nested data frames into a single non-nested data frame
#' @param progress_bar Display progress bar
#' @param v Version of API
#' @importFrom utils setTxtProgressBar txtProgressBar
#' @examples \dontrun{
#' members <- getGroupsMembersExecute(1, fields='sex,bdate,city', progress_bar = TRUE)
#' }
#' @export
getGroupsMembersExecute <- function(group_id='', sort='', offset=0, count=0, fields='', filter='', flatten=FALSE, progress_bar=FALSE, v=getAPIVersion())
{
  getGroupsMembers20 <- function(group_id='', sort='', offset=0, count='', fields='', filter='', v=getAPIVersion())
  {
    if (count > 20000)
      count <- 20000
    if (count <= 1000) {
      execute(paste0('return API.groups.getMembers({"group_id":"', group_id, '",
                     "sort":"', sort, '",
                     "offset":"', offset, '",
                     "count":"', count, '",
                     "fields":"', fields, '",
                     "filter":"', filter, '",
                     "v":"', v, '"}).items;'))
    } else {
      code <- 'var groups_members = [];'
      code <- paste0(code, 'groups_members = groups_members + API.groups.getMembers({"group_id":"', group_id,
                   '", "sort":"', sort,
                   '", "offset":"', offset,
                   '", "count":"', 1000,
                   '", "fields":"', fields,
                   '", "filter":"', filter,
                   '", "v":"', v, '"}).items;')
      code <- paste0(code, 'var offset = 1000 + ', offset, ';
                   var count = 1000; var max_offset = offset + ', count, ';
                   while (offset < max_offset && groups_members.length <= offset)
                   {
                     if (', count, ' - groups_members.length < 1000) {
                        count = ', count, ' - groups_members.length;
                     };
                     groups_members = groups_members + API.groups.getMembers({"group_id":"', group_id,
                     '", "sort":"', sort,
                     '", "fields":"', fields,
                     '", "filter":"', filter,
                     '", "v":"', v,
                     '", "count":count,
                     "offset":offset}).items;
                     offset = offset + 1000;
                   };
                   return groups_members;')
      execute(code)
    }
  }

  code <- paste0('return API.groups.getMembers({"group_id":"', group_id, '",
                 "sort":"', sort, '",
                 "offset":"', offset, '",
                 "count":"', 1, '",
                 "fields":"', fields, '",
                 "filter":"', filter, '",
                 "v":"', v, '"});')
  response <- execute(code)
  members <- response$items

  len <- ifelse(is.vector(members), length, nrow)
  max_count <- ifelse((response$count - offset) > count & count != 0, count, response$count - offset)

  if (progress_bar) {
    pb <- txtProgressBar(min = 0, max = max_count, style = 3)
    setTxtProgressBar(pb, len(members))
  }

  num_records <- ifelse(max_count - len(members) > 20000, 20000, max_count - len(members))
  while (len(members) < max_count)
  {
    tryCatch({members20 <- getGroupsMembers20(group_id = group_id,
                                  sort = sort,
                                  offset = offset + len(members),
                                  count = num_records,
                                  fields = fields,
                                  filter = filter,
                                  v = v)
      if (is.vector(members))
        members <- append(members, members20)
      else
        members <- jsonlite::rbind.pages(list(members, members20))

      num_records <- ifelse((max_count - len(members)) > num_records, num_records, max_count - len(members))},
    vk_error500 = function(e) {
      num_records <<- as.integer(num_records / 2)
      warning(simpleWarning(paste0('Parameter "count" was tuned: ', num_records, ' per request.')))
    },
    vk_error404 = function(e) {
      num_records <<- as.integer(num_records / 2)
      warning(simpleWarning(paste0('Parameter "count" was tuned: ', num_records, ' per request.')))
    })

    if (progress_bar)
      setTxtProgressBar(pb, len(members))
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
#' @examples \dontrun{
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


#' Returns information about communities by their IDs
#'
#' @param group_ids IDs or screen names of communities
#' @param group_id ID or screen name of the community
#' @param fields Group fields to return
#' @param v Version of API
#' @export
getGroupsById <- function(group_ids='', group_id='', fields='', v=getAPIVersion()) {
  query <- queryBuilder('groups.getById',
                        group_ids = group_ids,
                        group_id = group_id,
                        fields = fields,
                        v = v)
  request_delay()
  response <- jsonlite::fromJSON(httr::content(httr::POST(query), "text", encoding = "UTF-8"))

  if (has_error(response))
    return(try_handle_error(response))

  response$response
}


#' Returns a list of communities matching the search criteria
#'
#' @param q Search query string
#' @param type Community type. Possible values: group, page, event
#' @param country_id Country ID
#' @param city_id City ID. If this parameter is transmitted, country_id is ignored
#' @param future 1 — to return only upcoming events. Works with the type = event only
#' @param market 1 — to return communities with enabled market only
#' @param sort Sort order. Possible values:
#' \itemize{
#'   \item 0 — default sorting (similar the full version of the site);
#'   \item 1 — by growth speed;
#'   \item 2— by the "day attendance/members number" ratio;
#'   \item 3 — by the "Likes number/members number" ratio;
#'   \item 4 — by the "comments number/members number" ratio;
#'   \item 5 — by the "boards entries number/members number" ratio.
#' }
#' @param offset Offset needed to return a specific subset of results
#' @param count Number of communities to return (default 20, maximum value 1000)
#' @param v Version of API
#' @export
groupsSearch <- function(q='', type='', country_id='', city_id='', future=0, market=0, sort=0, offset=0, count=20, v=getAPIVersion()) {
  query <- queryBuilder('groups.search',
                        q = q,
                        type = type,
                        country_id = country_id,
                        city_id = city_id,
                        future = future,
                        market = market,
                        sort = sort,
                        offset = offset,
                        count = count,
                        v = v)
  request_delay()
  response <- jsonlite::fromJSON(query)

  if (has_error(response))
    return(try_handle_error(response))

  response$response
}
