##############################################
### Определение возраста через друзей
##############################################

# Формируем список друзей, у которых указан возраст
my_friends <- getFriends(fields='bdate,sex')$items
my_friends$bdate <- as.Date.character(my_friends$bdate, format="%d.%M.%Y")
my_friends <- my_friends[!is.na(my_friends$bdate), ]
my_friends$year_of_birth <- as.numeric(format(my_friends$bdate, "%Y"))
head(my_friends)

# Для каждого друга прогнозируем возраст (желательно использовать execute())
predicted_ages <- data.frame()
counter <- 0
for (idx in 1:nrow(my_friends)) {
  predicted_ages <- rbind(predicted_ages, age_predict(my_friends[idx, ]$id))
  counter <- counter + 1
  if (counter %% 3 == 0)
    Sys.sleep(1.0)
}

# Есть ли явно выбивающиеся?
predicted_ages$actual_ages <- my_friends$year_of_birth
boxplot(predicted_ages$year)

# Строим график
library(ggplot2)
ggplot(predicted_ages, aes(x=actual_ages, y=year_of_birth, color=my_friends$sex)) +
  geom_point(shape=ifelse(abs(predicted_ages$year_of_birth - predicted_ages$actual_ages) == 0.0, 16, 2), 
             size=predicted_ages$nfriends*0.02) +
  ylab("Прогнозируемый год рождения") + xlab("Реальный год рождения") +
  geom_abline()