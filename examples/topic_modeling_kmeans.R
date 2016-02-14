library(stringr)
library(tm)


# Получить список сообщений разбитый по неделям для указанных пользователей
get_messages <- function(users_ids) {
  all_messages <- list()
  for (i in 1:length(users_ids)) {
    messages <- messagesSplitByDate(messagesGetHistoryAll(users_ids[i]), "%W")
    all_messages <- append(all_messages, messages)
  }
  as.vector(unlist(lapply(all_messages, function(messages_per_week)
    { 
      paste(messages_per_week$body, collapse = " ") 
    })))
}


messages <- get_messages(c(
  # PUT USER IDs HERE
));


# Очищаем текст от "мусора"
messages <- str_replace_all(messages, "[ё]", "е")
messages <- str_replace_all(messages, "[[:punct:]]", " ")
messages <- str_replace_all(messages, "[[:digit:]]", " ")
messages <- str_replace_all(messages, "http\\S+\\s*", " ")
messages <- str_replace_all(messages, "[a-zA-Z]", " ")
messages <- str_replace_all(messages, "\\s+", " ")
messages <- tolower(messages)
messages <- str_trim(messages, side = "both")


# Нормализация (стемминг) с помощью python и библиотеки pymorphy2
write(messages, file='messages.txt')

"
import pymorphy2

morph = pymorphy2.MorphAnalyzer()

messages = open('messages.txt')
normailized_messages = open('messages_norm.txt', 'w')

for line in messages:
line_norm = ' '.join(morph.parse(word)[0].normal_form for word in line.split())
normailized_messages.write(line_norm + '\n')

messages.close()
normailized_messages.close()
"

messages_norm <- readLines("messages_norm.txt", encoding="UTF-8")


# Создаем корпус из сообщений
messages.corpus <- Corpus(VectorSource(messages_norm))


# Избавляемся от стоп-слов (https://github.com/imendibo/SEPLN-TASS15/blob/master/DATA/stop-words/stop-words_russian_1_ru.txt)
stop_words <- read.table('stop-words_russian_1_ru.txt')
stop_words <- as.vector(stop_words$V1)
stop_words <- unique(c(stopwords('russian'), stop_words, 
                       "вообще", "например", "вроде", "наверное", 
                       "думаю", "давай", "етот", "поэтому", "кстати"))

messages.corpus <- tm_map(messages.corpus, removeWords, stop_words)


# Удаляем все лишние пробельные символы
messages.corpus <- tm_map(messages.corpus, stripWhitespace)

messages.tdm <- TermDocumentMatrix(messages.corpus)
messages.tdm
messages.tdm.non.sparse <- removeSparseTerms(messages.tdm, 0.90)



# Кластеризация документов
N <- 5
model <- kmeans(messages.tdm.non.sparse, N)

for (cluster in 1:N) {
  cat("cluster ", cluster, ": ", as.vector(names(model$cluster[model$cluster == cluster])), "\n")
}


# Построение облака слов
library(wordcloud)
messages.matrix <- as.matrix(messages.tdm.non.sparse)
words.freq <- sort(rowSums(messages.matrix), decreasing = TRUE)
wordcloud(words = names(words.freq), freq = words.freq, min.freq = 100,
          scale = c(5, 1.0), max.words = 40, random.order = FALSE,
          rot.per = 0.35, use.r.layout = FALSE, colors = brewer.pal(8, "Dark2"))