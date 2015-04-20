`vkR` это пакет для взаимодействия с API ВКонтакте с помощью языка R. 

## Установка

``` r
install.packages("devtools")
devtools::install_github("Dementiy/vkR")
```

После чего пакет можно будет загружать с помощью функции `library()`:

``` r
library("vkR")
```

## Авторизация

``` r
authorize(CLIENT_ID, 'SCOPE', 'EMAIL', 'PASSWORD')
```

Где:
* `CLIENT_ID` - идентификатор приложения в ВК. Для получения идентификатора требуется создать новое [Standalone-приложение](https://vk.com/dev/standalone) в ВК (либо воспользоваться уже существующим).
* `FIELDS` - список возможных прав доступа, перечисленных через запятую, например `'friends,groups'` - предоставит доступ к друзьям и группам пользователя. Полный список прав можно найти [здесь](https://vk.com/dev/permissions).
* `EMAIL` и `PASSWORD` - логин и пароль в ВК.

Если аргументы `EMAIL` и `PASSWORD` были опущены, то будет открыто окно браузера. В адресной строке будет показан токен доступа (access token). Его необходимо скопировать и передать в качестве аргумента в функцию `setAccessToken()`:

``` r
setAccessToken(access_token = 'YOUR ACCESS TOKEN')
```

## Пример использования

Построение графа друзей:

``` r
my_friends <- getFriends() # возвращает два значения: count (количество друзей) и items (сами записи)
my_friends <- my_friends$items
network <- getNetwork(my_friends)

library("igraph")
g <- graph.adjacency(as.matrix(network), weighted=T, mode = "undirected")
layout <- layout.fruchterman.reingold(g)
plot(g, layout=layout)
```

Различные примеры работы см. в папке `examples`.
