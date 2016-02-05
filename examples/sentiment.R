##############################################
### Определение тональности сообщений
##############################################

# Сентимент-анализ переданного сообщения
getSentiment <- function(msg, keywords='', key='') {
  response <- POST('https://russiansentimentanalyzer.p.mashape.com/rsa/sentiment/polarity/json/', 
                   add_headers("X-Mashape-Key" = key, "Content-Type" = "application/json"),
                   body = paste("{\"text\":\"", msg, "\", 
                                \"object_keywords\":\"", keywords, "\", 
                                \"output_format\":\"json\"}", sep=''))
  content(response, type='application/json', encoding = 'UTF-8')
}