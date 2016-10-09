#' Returns a list of the communities to which a user belongs
#'
#' @param user_id User ID
#' @param extended 1 — to return complete information about a user's communities; 0 — to return a list of community IDs without any additional fields (default) 
#' @param filter Types of communities to return: admin, editor, moder, groups, publics, events
#' @param fields Profile fields to return
#' @param offset Offset needed to return a specific subset of communities
#' @param count Number of communities to return (maximum value 1000)
#' @param v Version of API
#' @examples
#' \dontrun{
#' groups <- getGroups('1', fields='city,country,place,description,wiki_page,members_count,counters,start_date,finish_date,can_post,can_see_all_posts,activity,status,contacts,links,fixed_post,verified,site,can_create_topic')
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
  response$response
}

#' Returns a list of community members
#'
#' @param group_id ID or screen name of the community
#' @param sort Sort order
#' @param offset Offset needed to return a specific subset of community members
#' @param count Number of community members to return (maximum value 1000)
#' @param fields List of additional fields to be returned
#' @param filter friends – only friends in this community will be returned; unsure – only those who pressed 'I may attend' will be returned (if it's an event)
#' @param v Version of API
#' @examples
#' \dontrun{
#' groups <- getGroupsMembers('1', fields='sex,bdate,city,country,photo_50,education,interests,music,movies,tv,books,games,about,quotes,personal')
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
  response$response
}


#' Returns a list of community members
#' 
#' @param group_id ID or screen name of the community
#' @param fields List of additional fields to be returned
#' @param filter friends – only friends in this community will be returned; unsure – only those who pressed 'I may attend' will be returned (if it's an event)
#' @param flatten Automatically flatten nested data frames into a single non-nested data frame
#' @param progress_bar Display progress bar
#' @param v Version of API
#' @export
getGroupsMembersExecute <- function(group_id='', fields='', filter='', flatten=FALSE, progress_bar=FALSE, v=getAPIVersion())
{
  getGroupsMembers20 <- function(group_id='', offset = 0, fields='', filter='', v=getAPIVersion())
  {
    code <- 'var groups_members = [];'
    code <- paste0(code, 'groups_members = groups_members + API.groups.getMembers({"group_id":"', group_id, 
                   '", "offset":"', offset, 
                   '", "fields":"', fields, 
                   '", "filter":"', filter, 
                   '", "v":"', v, '"}).items;')
    code <- paste0(code, 'var offset = 1000;
                   while (offset < 25000 && groups_members.length >= offset) 
                   {
                   groups_members = groups_members + API.groups.getMembers({"group_id":"', group_id, 
                   '", "fields":"', fields, 
                   '", "filter":"', filter, 
                   '", "v":"', v, 
                   '", "offset":(offset+',offset,')}).items;
                   offset = offset + 1000;
                   };
                   return groups_members;')
    execute(code)
  }
  
  code <- paste0('return API.groups.getMembers({"group_id":"', group_id, '", "fields":"', fields, '", "filter":"', filter, '", "v":"', v, '"});')
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