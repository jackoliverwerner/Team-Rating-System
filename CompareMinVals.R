##########
# Set up #
##########

#setwd("/Users/jackwerner/Documents/My Stuff/Baseball/Team Rating System")
setwd("C:/Users/jack.werner1/Documents/BB/Team-Rating-System")

source("getSeasonResults.R")
source("JWPitchers.R")

MLBteams <- "/Users/jackwerner/Documents/My Stuff/Baseball/Team Rating System/MLBteams.csv"


##############
# Get scores #
##############

year <- 2016

season.results <- read.csv("gameLogs_1998_2016.csv") %>% filter(!playoffs, Year == year)

its <- 1000

# LOOPIFY IT
for (i in seq(0, 25, by = 5)) {
  print(paste0("i = ", i))
  
  jw.results <- runs.array.pitchers(season.results, min.starts = i, pitcherCol = "ID") %>% 
    jw.gradient.pitchers.bt(iterations = its, startVal = 2, print.every.n = 100)
  
  team.scores <- jw.results$team.scores
  pitcher.scores <- jw.results$pitcher.scores

  pitcher.df.name <- paste0("pitcher.scores.", i)
  team.df.name <- paste0("team.scores.", i)
  
  assign(pitcher.df.name, pitcher.scores)
  assign(team.df.name, team.scores)
}

# Look at team scores
team.scores.full <- left_join(team.scores.0, team.scores.5, by = "team") %>%
  rename(score.0 = score.x, score.5 = score.y) %>%
  left_join(team.scores.10, by = "team") %>% rename(score.10 = score) %>%
  left_join(team.scores.15, by = "team") %>% rename(score.15 = score) %>%
  left_join(team.scores.20, by = "team") %>% rename(score.20 = score) %>%
  left_join(team.scores.25, by = "team") %>% rename(score.25 = score)

minValFunc <- function(char) {
  return(as.numeric(strsplit(char, ".", fixed = T)[[1]][2]))
}

team.scores.long <- gather(team.scores.full, key = MinVal, value = Score, 2:7)
team.scores.long$MinVal <- sapply(team.scores.long$MinVal, minValFunc)

ggplot(data = team.scores.long, aes(x = MinVal, y = Score, color = team)) + geom_line() + geom_point()


# Look at pitcher scores
pitcher.scores.full <- left_join(pitcher.scores.0, pitcher.scores.5, by = "name") %>%
  rename(score.0 = score.x, score.5 = score.y) %>%
  left_join(pitcher.scores.10, by = "name") %>% rename(score.10 = score) %>%
  left_join(pitcher.scores.15, by = "name") %>% rename(score.15 = score) %>%
  left_join(pitcher.scores.20, by = "name") %>% rename(score.20 = score) %>%
  left_join(pitcher.scores.25, by = "name") %>% rename(score.25 = score)

nasToGroup.df <- data.frame(nas = c(0, 1, 2, 3, 4, 5),
                            category = factor(c("> 25", "21-25", "16-20", "11-15", "6-10", "< 6"),
                                              levels = c("> 25", "21-25", "16-20", "11-15", "6-10", "< 6"), 
                                              ordered = T))

pitcher.scores.long <- gather(pitcher.scores.full, key = MinVal, value = Score, 2:7) %>%
  group_by(name) %>% mutate(nas = length(which(is.na(Score)))) %>% ungroup() %>%
  left_join(nasToGroup.df, by = "nas") %>% select(-nas)

pitcher.scores.long$MinVal <- sapply(pitcher.scores.long$MinVal, minValFunc)

ggplot(data = pitcher.scores.long, aes(x = MinVal, y = Score, group = name)) + geom_line(alpha = .2)


ggplot(data = filter(pitcher.scores.long, MinVal == 0), aes(x = Score, y = ..density..)) + 
  facet_grid(category ~ .) + geom_histogram(fill = "grey20", color = "grey80", binwidth = .25)











hist.df <- pitcher.scores.full %>% filter(is.na(score.y))
ggplot(data = hist.df, aes(x = score.x)) + geom_histogram(binwidth = .25, fill = "white", color = "black")


