##########
# Set up #
##########

library(skellam)

#setwd("/Users/jackwerner/Documents/My Stuff/Baseball/Team Rating System")
setwd("C:/Users/jack.werner1/Documents/BB/Team-Rating-System")

source("JWPitchers.R")


####################
# Get game results #
####################

full.results <- read.csv("gameLogs_1998_2016.csv")

year <- 2003

season.results <- full.results %>% filter(Year == year)

reg.results <- season.results %>% filter(!playoffs)
playoff.results <- season.results %>% filter(playoffs) %>%
  arrange(team, gameNum)


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

hfa <- 1.17

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
         home.percent.JW.adj = hfa*home.percent.JW/((hfa - 1)*home.percent.JW + 1),
         away.percent.JW.adj = (away.percent.JW/hfa)/((1/hfa - 1)*away.percent.JW + 1),
         su = home.percent.JW.adj + away.percent.JW.adj)

playoff.preds.pre <- playoff.preds.JW %>% 
  select(home.team, home.starter, away.team, away.starter, home.R, away.R, winner,
         home.percent.JW, home.percent.JW.adj)

#################################################
# Simple prediction data frame (with season W%) #
#################################################

teamWPs <- reg.results %>% group_by(team) %>%
  summarize(WP = sum(result == "W")/n())

playoff.preds.WP <- playoffs.df %>%
  left_join(teamWPs, by = c("home.team" = "team")) %>% rename(WP.home = WP) %>%
  left_join(teamWPs, by = c("away.team" = "team")) %>% rename(WP.away = WP) %>%
  mutate(#home.percent.WP = WP.home/(WP.home + WP.away),
         #away.percent.WP = WP.away/(WP.home + WP.away),
         home.percent.WP = (WP.home - WP.away + 1)/2,
         away.percent.WP = (WP.away - WP.home + 1)/2,
         home.percent.WP.adj = hfa*home.percent.WP/((hfa - 1)*home.percent.WP + 1),
         away.percent.WP.adj = (away.percent.WP/hfa)/((1/hfa - 1)*away.percent.WP + 1))

playoff.preds <- cbind(playoff.preds.pre, select(playoff.preds.WP, home.percent.WP, home.percent.WP.adj)) %>%
  mutate(home.won = winner == home.team)


########################
# Evaluate predictions #
########################

# Confusion matrices
(conf.WP <- table(playoff.preds$home.won, playoff.preds$home.percent.WP > .5))
(conf.WP.adj <- table(playoff.preds$home.won, playoff.preds$home.percent.WP.adj > .5))
(conf.JW <- table(playoff.preds$home.won, playoff.preds$home.percent.JW > .5))
(conf.JW.adj <- table(playoff.preds$home.won, playoff.preds$home.percent.JW.adj > .5))

# Error rates
sum(diag(conf.WP))/sum(conf.WP)
sum(diag(conf.WP.adj))/sum(conf.WP.adj)
sum(diag(conf.JW))/sum(conf.JW)
sum(diag(conf.JW.adj))/sum(conf.JW.adj)

# Brier scores
(brier.WP <- mean((playoff.preds$home.percent.WP - playoff.preds$home.won)^2))
(brier.WP.adj <- mean((playoff.preds$home.percent.WP.adj - playoff.preds$home.won)^2))
(brier.JW <- mean((playoff.preds$home.percent.JW - playoff.preds$home.won)^2))
(brier.JW.adj <- mean((playoff.preds$home.percent.JW.adj - playoff.preds$home.won)^2))

# Distrimination slope
playoff.preds %>% group_by(home.won) %>%
  summarize(home.percent.WP = mean(home.percent.WP), home.percent.WP.adj = mean(home.percent.WP.adj),
            home.percent.JW = mean(home.percent.JW), home.percent.JW.adj = mean(home.percent.JW.adj))

# ROC curve -- Messed Up
truePositiveRate <- function(thresh, pred.vec, actual.vec) {
  tp <- sum(actual.vec[pred.vec > thresh])
  p <- sum(actual.vec)
  
  return(tp/p)
}

falsePositiveRate <- function(thresh, pred.vec, actual.vec) {
  tp <- sum(actual.vec[pred.vec > thresh])
  p <- sum(actual.vec)
  
  fp <- length(actual.vec[pred.vec > thresh]) - tp
  n <- length(actual.vec) - p
  
  return(fp/n)
}

it <- seq(1, 0, by = -.01)

roc <- data.frame(thresh = it,
                  tpr.WP = sapply(it, truePositiveRate, playoff.preds$home.percent.WP, playoff.preds$home.won),
                  fpr.WP = sapply(it, falsePositiveRate, playoff.preds$home.percent.WP, playoff.preds$home.won),
                  tpr.WP.adj = sapply(it, truePositiveRate, playoff.preds$home.percent.WP.adj, playoff.preds$home.won),
                  fpr.WP.adj = sapply(it, falsePositiveRate, playoff.preds$home.percent.WP.adj, playoff.preds$home.won),
                  tpr.JW = sapply(it, truePositiveRate, playoff.preds$home.percent.JW, playoff.preds$home.won),
                  fpr.JW = sapply(it, falsePositiveRate, playoff.preds$home.percent.JW, playoff.preds$home.won),
                  tpr.JW.adj = sapply(it, truePositiveRate, playoff.preds$home.percent.JW.adj, playoff.preds$home.won),
                  fpr.JW.adj = sapply(it, falsePositiveRate, playoff.preds$home.percent.JW.adj, playoff.preds$home.won))

ggplot(data = roc, aes(x = fpr.WP, y = tpr.WP)) + geom_line(color = "blue") + 
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  geom_line(aes(x = fpr.WP, y = tpr.WP.adj), color = "red")

ggplot(data = roc, aes(x = fpr.JW.adj, y = tpr.JW.adj)) + geom_line(color = "blue") + 
  geom_abline(slope = 1, intercept = 0, linetype = 2)


ggplot(data = roc, aes(x = thresh, y = fpr.JW)) + geom_line()

