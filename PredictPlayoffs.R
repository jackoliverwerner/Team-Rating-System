##########
# Set up #
##########

library(skellam)

setwd("/Users/jackwerner/Documents/My Stuff/Baseball/Team Rating System")
#setwd("C:/Users/jack.werner1/Documents/BB")

source("getSeasonResults.R")
source("JWPitchers.R")

MLBteams <- paste0(getwd(), "/MLBteams.csv")


####################
# Get game results #
####################

year <- 2016

season.results <- getLeagueResults(year, MLBteams, includePlayoffs = T)

season.results$starter[season.results$starter == "A.Sanchez"] <- 
  ifelse(season.results$team[season.results$starter == "A.Sanchez"] == "TOR", "Aa.Sanchez", "An.Sanchez")

season.results$starter[season.results$starter == "C.Anderson"] <- 
  ifelse(season.results$team[season.results$starter == "C.Anderson"] == "MIL", "Ch.Anderson", "Co.Anderson")

reg.results <- season.results %>% filter(!playoffs)
playoff.results <- season.results %>% filter(playoffs)


# Format playoff results for prediction

playoff.results$series.name <- 
  mapply(FUN = function(x, y){paste(sort(c(x, y)), collapse = "")}, x = playoff.results$team, y = playoff.results$opp)

playoff.results <- playoff.results %>% arrange(series.name, team) %>% group_by(series.name, team) %>%
  mutate(game = 1:n()) %>% ungroup

p.home <- playoff.results %>% filter(home) %>% arrange(series.name, game) %>%
  select(home.team = team, home.starter = starter, home.result = result, home.R = R)

p.away <- playoff.results %>% filter(!home) %>% arrange(series.name, game) %>%
  select(away.team = team, away.starter = starter, away.result = result, away.R = R)

playoffs.df <- cbind(p.home, p.away) %>% mutate(winner = ifelse(home.result == "W", home.team, away.team)) %>%
  select(home.team, home.starter, away.team, away.starter, home.R, away.R, winner) %>%
  as.data.frame()



##########################
# Get model scores -- JW #
##########################

its <- 10000

# Optimize
jw.results <- runs.array.pitchers(reg.results, min.starts = 10) %>% 
  jw.gradient.pitchers(iterations = its, speed = .001, startVal = 2)


# Finalize score dataframes
team.scores <- jw.results$team.scores
pitcher.scores <- jw.results$pitcher.scores

namesFunc <- function(char) {
  spl <- strsplit(char, "-")
  len <- length(spl[[1]])
  return(spl[[1]][len])
}
pitcher.scores$name <- sapply(as.character(pitcher.scores$team), namesFunc)

# Include pitchers with not enough starts (assign them "Other" score)
pitcher.scores.2 <- pitcher.scores %>% group_by(name) %>%
  summarize(score = weighted.mean(score, starts))

pitcher.scores.final.pre <- data.frame(starter = unique(reg.results$starter))

pitcher.scores.final <- pitcher.scores.final.pre %>%
  left_join(pitcher.scores.2, by = c("starter"="name")) %>%
  rename(name = starter)

pitcher.scores.final[is.na(pitcher.scores.final$score),]$score <- pitcher.scores.2$score[pitcher.scores.2$name == "Other"]

###############################
# Prediction data frame -- JW #
###############################

playoff.preds.JW <- playoffs.df %>% 
  left_join(team.scores, by = c("home.team" = "team")) %>% rename(home.offense = score) %>%
  left_join(team.scores, by = c("away.team" = "team")) %>% rename(away.offense = score) %>%
  left_join(pitcher.scores.final, by = c("home.starter" = "name")) %>% rename(home.defense = score) %>%
  left_join(pitcher.scores.final, by = c("away.starter" = "name")) %>% rename(away.defense = score) %>%
  mutate(home.lambda = home.offense*away.defense, away.lambda = away.offense*home.defense,
         home.raw = pskellam(-1, lambda1 = away.lambda, lambda2 = home.lambda),
         away.raw = pskellam(-1, lambda1 = home.lambda, lambda2 = away.lambda),
         home.percent.JW = home.raw/(home.raw + away.raw), 
         away.percent.JW = away.raw/(away.raw + home.raw),
         home.percent.JW.adj = 1.17*home.percent.JW/(2.17 - home.percent.JW),
         away.percent.JW.adj = (away.percent.JW/1.17)/(1/1.17 + 1 - away.percent.JW),
         su = home.percent.JW.adj + away.percent.JW.adj)

playoff.preds <- playoff.preds.JW %>% 
  select(home.team, home.starter, away.team, away.starter, home.R, away.R, winner,
         home.percent.JW)

#################################################
# Simple prediction data frame (with season W%) #
#################################################

teamWPs <- reg.results %>% group_by(team) %>%
  summarize(WP = sum(result == "W")/n())

playoff.preds.WP <- playoffs.df %>%
  left_join(teamWPs, by = c("home.team" = "team")) %>% rename(WP.home = WP) %>%
  left_join(teamWPs, by = c("away.team" = "team")) %>% rename(WP.away = WP) %>%
  mutate(home.percent.WP = WP.home/(WP.home + WP.away),
         away.percent.WP = WP.away/(WP.home + WP.away))








