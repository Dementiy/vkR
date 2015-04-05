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
      var count = 100;
      while (offset < 2500 && wall_records.length <= offset && offset-', offset, '<', max_count, ') {
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