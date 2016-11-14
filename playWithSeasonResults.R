library(dplyr)
library(rvest)
library(data.table)
library(tidyr)

# Get season results data frame
setwd("C:/Users/jack.werner1/Documents/BB")

source("getSeasonResults.R")

MLBteams <- "C:/Users/jack.werner1/Documents/BB/MLBTeams.csv"

year <- 2001

season.results <- getLeagueResults(year, MLBteams) %>% filter(!playoffs)

rs2015 <- getLeagueResults(1999, MLBteams) %>% filter(!playoffs)

head(rs2015)

# How does average RA change with starters?
dodgers <- filter(rs2015, team == "LAD")

d.rotation <- filter(dodgers, starter %in% c("B.Anderson", "C.Kershaw", "Z.Greinke", "M.Bolsinger"))

ggplot(data = d.rotation) + facet_grid(starter ~ .) + 
  geom_histogram(aes(x = RA, y = ..density..), binwidth = 1, fill = "grey90", color = "blue")


cubs <- filter(rs2015, team == "CHC")

c.rotation <- filter(cubs, starter %in% c("J.Arrieta", "J.Hammel", "J.Lester", "K.Hendricks"))

ggplot(data = c.rotation) + facet_grid(starter ~ .) + 
  geom_histogram(aes(x = RA, y = ..density..), binwidth = 1, fill = "grey90", color = "blue")


#Which teams and pitchers have lowest RA?
teams <- rs2015 %>% group_by(team) %>%
  summarize(RA = mean(RA)) %>%
  arrange(RA)

View(teams)


players <- rs2015 %>% group_by(starter) %>%
  summarize(starts = n(), RA = mean(RA)) %>%
  filter(starts > 19) %>%
  arrange(RA)

View(players)