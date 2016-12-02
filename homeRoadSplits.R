library(dplyr)
library(rvest)
library(data.table)
library(tidyr)
library(ggplot2)

# Get season results data frame
#setwd("C:/Users/jack.werner1/Documents/BB")
setwd("/Users/jackwerner/Documents/My Stuff/Baseball/Team Rating System")

year <- 2001

season.results <- read.csv("gameLogs_1998_2016.csv", stringsAsFactors = F) %>% 
  filter(!playoffs, Year == year)

#############
# Home/Away #
#############

ha.df <- season.results %>% group_by(team, home) %>%
  summarize(W = sum(result == "W"), L = sum(result == "L")) %>% ungroup() %>%
  mutate(record = W/(W + L), HA = ifelse(home, "home", "away")) %>%
  select(team, HA, record) %>%
  spread(key = HA, value = record)

ggplot(data = ha.df, aes(x = home, y = away, label = team)) + geom_text() +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  coord_cartesian(xlim = c(.25, .75), ylim = c(.25, .75))

ha.df <- ha.df %>% mutate(diff = home - away) %>%
  arrange(desc(diff))

home <- season.results %>% filter(home)

length(which(home$result == "W"))/nrow(home)


# Split-half reliabilities
dif.cors <- rep(0, 1000)

for (i in 1:1000) {
  splits <- sample(1:nrow(season.results))
  splits.a <- splits[1:(nrow(season.results)/2)]
  splits.b <- splits[(nrow(season.results)/2 + 1):nrow(season.results)]
  
  ha.df.a <- season.results[splits.a,] %>% group_by(team, home) %>%
    summarize(W = sum(result == "W"), L = sum(result == "L")) %>% ungroup() %>%
    mutate(record = W/(W + L), HA = ifelse(home, "home", "away")) %>%
    select(team, HA, record) %>%
    spread(key = HA, value = record) %>%
    mutate(diff.a = home - away) %>%
    select(team, diff.a)
  
  ha.df.b <- season.results[splits.b,] %>% group_by(team, home) %>%
    summarize(W = sum(result == "W"), L = sum(result == "L")) %>% ungroup() %>%
    mutate(record = W/(W + L), HA = ifelse(home, "home", "away")) %>%
    select(team, HA, record) %>%
    spread(key = HA, value = record) %>%
    mutate(diff.b = home - away) %>%
    select(team, diff.b)
  
  ha.df.ab <- left_join(ha.df.a, ha.df.b, by = "team")
  
  dif.cors[i] <- cor(ha.df.ab$diff.a, ha.df.ab$diff.b)
  
  print(i)
}

hist(dif.cors, breaks = 25)
mean(dif.cors)