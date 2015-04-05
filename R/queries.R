#' Возвращает строку запроса, созданную из переданных аргументов
#'
#' @param method_name Имя метода
queryBuilder <- function(method_name, ...) {
  query <- paste("https://api.vk.com/method/", method_name, "?", sep="")
  arguments <- sapply(substitute(list(...))[-1], deparse)
  arg_names <- names(arguments)
  for (arg_pos in seq(length(arguments))) {
    if (arg_names[arg_pos] != "") {
      if (is.character(arguments[arg_pos])) {
        #arg_value <- gsub("\"", "", arguments[arg_pos])
        arg_value <- list(...)[arg_names[arg_pos]]
      } else {
        # ???
        arg_value <- arguments[arg_pos]
      }
      query <- paste(query, ifelse(arg_value != "", paste("&", arg_names[arg_pos], "=", arg_value, sep=""), ""), sep="")
    }
  }
  query <- paste(query, '&access_token=', getAccessToken(), sep="")
  query
}


#' Универсальный метод, который позволяет запускать последовательность других методов, сохраняя и фильтруя промежуточные результаты
#' @export
execute <- function(code) {
  query <- "https://api.vk.com/method/execute"
  response <- fromJSON(rawToChar(POST(url = query, 
                                      body = list('code' = code, 
                                                  'access_token' = getAccessToken()))$content))
  response$response
}