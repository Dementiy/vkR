## Examples

Plot frequency of messages:

``` r
uid <- # PUT HERE USER ID
messages <- messagesGetHistoryAll(uid)
messages_per_week <- messagesSplitByDate(messages, "%W") # %W means 'per week'

df <- do.call(rbind.data.frame, 
			  lapply(messages_per_week, function(msgs) { length(msgs$body) }))
df$week <- as.numeric(rownames(df))
colnames(df) <- c("freq", "week")
rownames(df) <- NULL

print(df)

   freq week
1     2    0
2   859    1
3   223    2
4   584    3
5   595    4
6   751    5
7    10    6
8   144   42
9   842   43
10 2444   44
11 2038   45
12 1365   46
13 1551   47
14  963   48
15 2237   49
16 1363   50
17  734   51
18  121   52
```

Weeks from 0 to 6 are in 2016:

```r
df$week <- c(df$week[1:7] + 53, df$week[8:nrow(df)])

library(ggplot2)
ggplot(data = df, aes(week, freq)) + 
	   xlab("Week") + ylab("Frequency") + 
	   geom_line() + geom_smooth(se=FALSE)
```

![alt text](images/freq_plot.png?raw=true "Frequency of text messages")
