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
                        user_id=user_id,
                        extended=extended,
                        filter=filter,
                        fields=fields,
                        offset=offset,
                        count=count,
                        v=v)
  response <- fromJSON(query)
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
                        group_id=group_id,
                        sort=sort,
                        offset=offset,
                        count=count,
                        fields=fields,
                        filter=filter,
                        v=v)
  response <- fromJSON(query)
  response$response
}


#' Returns a list of community members
#' 
#' @param group_id ID or screen name of the community
#' @param v Version of API
#' @export
getGroupsMembersExecute <- function(group_id = '', v=getAPIVersion())
{
  getGroupsMembers20 <- function(group_id = '', offset = 0, v=getAPIVersion())
  {
    code <- 'var groups_members = [];'
    code <- paste0(code, 'groups_members = groups_members + API.groups.getMembers({"group_id":"', group_id, 
                   '", "offset":"', offset, 
                   '", "v":"', v, '"}).items;')
    code <- paste0(code, 'var offset = 1000;
                   while (offset < 25000 && groups_members.length >= offset) 
                   {
                   groups_members = groups_members + API.groups.getMembers({"group_id":"', group_id, 
                   '", "v":"', v, 
                   '", "offset":(offset+',offset,')}).items;
                   offset = offset + 1000;
                   };
                   return groups_members;')
    execute(code)
  }
  
  code <- paste0('return API.groups.getMembers({"group_id":"', group_id, '", "v":"', v, '"});')
  response <- execute(code)
  users_ids <- response$items
  count <- response$count
  
  delay_counter <- 0
  while (length(users_ids) < count)
  {
    users_ids <- append(users_ids, getGroupsMembers20(group_id, length(users_ids)))
    delay_counter <- delay_counter + 1
    if (delay_counter %% 3 == 0)
      Sys.sleep(1.0)
  }
  
  users_ids
}
