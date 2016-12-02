library(dplyr)
library(rvest)
library(data.table)
library(tidyr)

# Get season results data frame
#setwd("C:/Users/jack.werner1/Documents/BB")
setwd("/Users/jackwerner/Documents/My Stuff/Baseball/Team Rating System")

year <- 2015

rs2015 <- read.csv("gameLogs_1998_2016.csv", stringsAsFactors = F) %>% 
  filter(!playoffs, Year == year)

head(rs2015)

# How does average RA change with starters?
dodgers <- filter(rs2015, team == "LAD")

d.rotation <- filter(dodgers, starter %in% c("Anderson", "Kershaw", "Greinke", "Bolsinger"))

ggplot(data = d.rotation) + facet_grid(starter ~ .) + 
  geom_histogram(aes(x = RA, y = ..density..), binwidth = 1, fill = "grey90", color = "blue")


cubs <- filter(rs2015, team == "CHC")

c.rotation <- filter(cubs, starter %in% c("Arrieta", "Hammel", "Lester", "Hendricks"))

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