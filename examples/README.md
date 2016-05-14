Examples
========

Plot frequency of messages
---------------------------

```r
library(data.table)
library(ggplot2)

uid <- # PUT HERE USER ID
messages <- messagesGetHistoryExecute(user_id = uid, progress_bar = TRUE)
messages <- messages$messages
messages_per_week <- messagesSplitByDate(messages, "%y-%m-%W")

df <- do.call(rbind.data.frame, 
              lapply(messages_per_week, function(msgs) { length(msgs$body) }))

df$week <- rownames(df)
df$week_number <- 1:nrow(df)
colnames(df) <- c("freq", "week", "week_number")
rownames(df) <- NULL

ggplot(data = df, aes(week_number, freq)) + 
  xlab("Week") + ylab("Frequency") + 
  geom_line() + geom_smooth(se=FALSE)
```

![alt text](images/freq_plot.png?raw=true "Frequency of text messages")


Topic modeling
---------------
```r
library(stringr)
library(lda)
library(LDAvis)


# Получить список сообщений для указанных пользователей
get_messages <- function(users_ids) {
  all_messages <- c()
  for (i in 1:length(users_ids)) {
    messages <- messagesGetHistoryAll(users_ids[i])
    all_messages <- c(all_messages, as.vector(messages$body))
  }
  all_messages
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


# Нормализация (стемминг)
write(messages, file='messages')
```

Нормализация текста происходит с помощью морфологического анализатора [`pymorphy2`](https://pymorphy2.readthedocs.org/en/latest/). Пример скрипта:
```python
import pymorphy2

morph = pymorphy2.MorphAnalyzer()

messages = open('messages.txt')
normailized_messages = open('messages_norm.txt', 'w')

for line in messages:
    line_norm = ' '.join(morph.parse(word)[0].normal_form for word in line.split())
    normailized_messages.write(line_norm + '\n')

messages.close()
normailized_messages.close()
```

Дальше текст обрабатываем в R:
```r
messages_norm <- readLines("messages_norm.txt", encoding="UTF-8")


# Тематическое моделирование (см. http://cpsievert.github.io/LDAvis/reviews/reviews.html)

# Построение таблицы частот
doc.list <- strsplit(messages_norm, "[[:space:]]+")
term.table <- table(unlist(doc.list))
term.table <- sort(term.table, decreasing = TRUE)


# Избавляемся от стоп-слов (https://github.com/imendibo/SEPLN-TASS15/blob/master/DATA/stop-words/stop-words_russian_1_ru.txt)
stop_words <- read.table('stop-words_russian_1_ru.txt')
stop_words <- as.vector(stop_words$V1)
stop_words <- unique(c(stopwords('russian'), stop_words, 
                "вообще", "например", "вроде", "наверное", 
                "думаю", "давай", "етот", "поэтому", "кстати"))

del <- names(term.table) %in% stop_words | term.table < 5
term.table <- term.table[!del]
vocab <- names(term.table)


# Приводим список терминов к формату понятному lda
get.terms <- function(x) {
  index <- match(x, vocab)
  index <- index[!is.na(index)]
  rbind(as.integer(index - 1), as.integer(rep(1, length(index))))
}
documents <- lapply(doc.list, get.terms)


# Вычисление некоторых статистик
D <- length(documents)
W <- length(vocab)
doc.length <- sapply(documents, function(x) sum(x[2, ])) 
N <- sum(doc.length)
term.frequency <- as.integer(term.table)


# Параметры модели
K <- 5
G <- 1000
alpha <- 0.02
eta <- 0.02


# Построение модели
fit <- lda.collapsed.gibbs.sampler(documents = documents, K = K, vocab = vocab, 
                                   num.iterations = G, alpha = alpha, 
                                   eta = eta, initial = NULL, burnin = 0,
                                   compute.log.likelihood = TRUE)


# Визуализация модели с помощью LDAVis
theta <- t(apply(fit$document_sums + alpha, 2, function(x) x/sum(x)))
phi <- t(apply(t(fit$topics) + eta, 2, function(x) x/sum(x)))

results <- list(phi = phi,
                theta = theta,
                doc.length = doc.length,
                vocab = vocab,
                term.frequency = term.frequency)


json <- createJSON(phi = results$phi, 
                   theta = results$theta, 
                   doc.length = results$doc.length, 
                   vocab = results$vocab, 
                   term.frequency = results$term.frequency)

serVis(json, out.dir = './', open.browser = FALSE)
```