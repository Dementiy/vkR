##############################################
### Примеры использования метода execute()
##############################################

# Получение сообщений со стены пользователя. За один запрос можно получить не более 2500 сообщений.
# Описание аргументов см. у функции getWall(). Параметр extended не оказывает влияния на результаты.  
get_wall2500 <- function(owner_id='', domain=NULL, offset=0, max_count=100, filter='all', extended='0', v='5.29') {
  if (max_count == 0) max_count <- 2500
  
  owner <- ifelse(!is.null(domain), paste0('"domain":"', domain, '"'), paste0('"owner_id":', owner_id))
  
  if (max_count <= 100) {
    execute(paste0('return API.wall.get({',
                   owner, ', 
                   "offset":', offset, ', 
                   "count":', max_count, ',
                   "filter":', '"', filter, '"', ',
                   "extended":', extended, ',
                   "v":', v, '}).items;'))
  } else {
    code <- 'var wall_records = [];'
    code <- paste0(code, 'wall_records = wall_records + 
                   API.wall.get({',
                     owner, ', 
                     "offset":', offset, ', 
                     "count": 100,
                     "filter":', '"', filter, '"', ',
                     "extended":', extended, ',
                     "v":', v, '}).items;')
    
    code <- paste0(code, 'var offset = 100 + ', offset, ';
      var count = 100; var max_offset = offset + ', max_count, ';
      while (offset < max_offset && wall_records.length <= offset && offset-', offset, '<', max_count, ') {
        if (', max_count, ' - wall_records.length < 100) {
          count = ', max_count, ' - wall_records.length;
        };
        wall_records = wall_records + API.wall.get({',
                   owner, ', 
                     "offset": offset, 
                     "count": count,
                     "filter":', '"', filter, '"', ',
                     "extended":', extended, ',
                     "v":', v, '}).items;
        offset = offset + 100;
      };
      return wall_records;')
    
    execute(code)
  }
}


# Получить список всех постов со стены
get_all_posts <- function(owner_id='', domain=NULL, filter='all', extended='0', v='5.29') {
  offset <- 0
  delay_counter <- 0
  count_per_request <- 2500
  all_posts <- list()
  
  repeat
  {
    messages <- get_wall2500(owner_id = owner_id, 
                             domain = domain, 
                             offset = offset, 
                             max_count = count_per_request,
                             filter = filter,
                             extended = extended,
                             v = v)
    
    if (length(messages) <= 0) break
    
    if (!is.null(messages)) {
      for (i in 1:nrow(messages))
      {
        post <- vkPost(messages[i, ])
        all_posts <- append(all_posts, list(post))
      }
    }
    
    offset <- offset + count_per_request
    delay_counter <- delay_counter + 1
    if (delay_counter %% 3 == 0)
      Sys.sleep(1.0)
  }
  
  all_posts
}
# texts <- sapply(all_posts, function(post) post$text)


# Получить информацию об указанных пользователях. Может быть указано не более ~5-15 тысяч пользователей.
get_users <- function(user_ids='', fields='', name_case='') {
  code <- 'var users = [];'
  num_requests <- ifelse(length(user_ids) %% 500 == 0, (length(user_ids) %/% 500), (length(user_ids) %/% 500) + 1)
  from <- 1
  to <- ifelse(num_requests >= 2, 500, length(user_ids))
  for (i in 1:num_requests) {
    code <- paste0(code, 'users = users + API.users.get({
                   "user_ids":"', paste0(user_ids[from:to], collapse = ','), '", 
                   "fields":"', fields, '", 
                   "name_case":"', name_case, '", "v":"5.50"});')
    from <- to + 1
    to <- to + ifelse(length(user_ids) - (to + 500) >= 0, 500, length(user_ids) - to)
  }
  code <- paste0(code, 'return users;')
  if (nchar(code) > 65535) stop("The POST request is limited by 65535 bytes")
  execute(code)
}
# users <- get_users(sample(x = seq(1:10000000), size=5000, replace = FALSE), fields='sex')