[![Travis-CI Build Status](https://travis-ci.org/Dementiy/vkR.svg?branch=master)](https://travis-ci.org/Dementiy/vkR)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/vkR)](https://cran.rstudio.com/web/packages/vkR/)

`vkR` is an R package which provides access to the [VKontakte](https://vk.com/) (VK) API.

## Installation

To get the current released version from CRAN:
```r
install.packages("vkR")
```

To get the current development version from github:
``` r
install.packages("devtools")
devtools::install_github("Dementiy/vkR")
library("vkR")
```

## Authorization

Most API requests require the use of an access token. VK has several types of authorization mechanisms. Check out the [documentation](https://vk.com/dev/authentication) for more details.

``` r
vkOAuth(CLIENT_ID, 'SCOPE', 'EMAIL', 'PASSWORD')
```

where:
* `CLIENT_ID` - is an application ID. You have to create new [Standalone-app](https://vk.com/dev/standalone) in VK to get ID (or use the already existing).
* `SCOPE` - the list of comma separated access rights, e.g. `'friends,groups'`- provide the access to user friends and groups. List of all rights can be found [here](https://vk.com/dev/permissions).
* `EMAIL` and `PASSWORD` - username and password.

If the `EMAIL` and `PASSWORD` have been omitted, a browser window will be opened. In the address bar an access token will be shown. Access token must be copied and passed as an argument into the following function:

``` r
setAccessToken(access_token = 'YOUR ACCESS TOKEN')
```

## Example of use

At your own risk you can use mongodb and [mongolite](https://github.com/jeroen/mongolite) package for storing data:

```r
> db_init()
> wall <- getWallExecute(domain="data_mining_in_action", count=0, use_db=TRUE, progress_bar=TRUE)
|======================...======================| 100%
> show_collections()
    db            collection       suffix count
1 temp data_mining_in_action         wall   232
```

If connection was aborted by some reasons we don't lose our data:
```r
> wall <- getWallExecute(domain='privivkanet', count=0, use_db = T, progress_bar = T)
|=================                              |  25%
Show Traceback
 
 Rerun with Debug
 Error in curl::curl_fetch_memory(url, handle = handle) : 
  Operation was aborted by an application callback ...
> show_collections()
    db            collection       suffix count
1 temp data_mining_in_action         wall   232
2 temp           privivkanet         wall   916
> wall <- getWallExecute(domain='privivkanet', count=0, offset=916, use_db = T, progress_bar = T)
|======================...======================| 100%
> show_collections()
    db            collection       suffix count
1 temp data_mining_in_action         wall   232
2 temp           privivkanet         wall  3664
```

You can specify the collection name:
```r
> wall <- getWallExecute(domain="data_mining_in_action", count=0, 
        use_db=TRUE, db_params=list('collection'='dm', 'suffix'='posts'), progress_bar=TRUE)
|======================...======================| 100%
> show_collections()
    db            collection       suffix count
1 temp data_mining_in_action         wall   232
2 temp           privivkanet         wall  3664
3 temp                    dm        posts   232

> friends <- getFriends()
> users <- getUsersExecute(friends$items, use_db = TRUE, db_params=list('collection'='my_friends'), progress_bar = TRUE)
> show_collections()
    db            collection       suffix count
1 temp data_mining_in_action         wall   232
2 temp           privivkanet         wall  3664
3 temp                    dm        posts   232
4 temp            my_friends                141
```

For load collection into a namespace you can use `db_load_collection` function:
```r
> db_load_collection('data_mining_in_action', 'wall')
 Imported 232 records. Simplifying into dataframe...
> ls()
[1] "temp.data_mining_in_action.wall"
> nrow(temp.data_mining_in_action.wall)
[1] 232
```

Building a Friend Graph:

``` r
my_friends <- getFriends(fields = 'sex')
my_friends <- filter(my_friends$items, is.na(deactivated))
network <- getNetwork(my_friends$id)

library("igraph")
g <- graph.adjacency(as.matrix(network), weighted = T, mode = "undirected")
layout <- layout.fruchterman.reingold(g)
plot(g, layout = layout)
```

Analyzing community activity:
``` r
domain <- 'nipponkoku'
wall <- getWallExecute(domain = domain, count = 0, progress_bar = TRUE)
metrics <- jsonlite::flatten(wall$posts[c("date", "likes", "comments", "reposts")])
metrics$date <- as.POSIXct(metrics$date, origin="1970-01-01", tz='Europe/Moscow')

library(dplyr)
df <- metrics %>% 
  mutate(period = as.Date(cut(date, breaks='month'))) %>% 
  group_by(period) %>%
  summarise(likes = sum(likes.count), comments = sum(comments.count), reposts = sum(reposts.count), n = n())

library(ggplot2)
library(tidyr)
ggplot(data=gather(df, 'type', 'count', 2:5), aes(period, count)) + geom_line(aes(colour=type)) +
  labs(x='Date', y='Count')
```

<center><img src="images/community_activity.png" alt="Coomunity activity" style="width: 640px;"/></center>

You can find more examples in `examples` directory.
