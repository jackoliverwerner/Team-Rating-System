##########
# Set up #
##########

library(data.table)
library(dplyr)
library(ggplot2)
library(tidyr)
library(skellam)

setwd("/Users/jackwerner/Documents/My Stuff/Baseball/Team Rating System")
#setwd("C:/Users/jack.werner1/Documents/BB/Team-Rating-System")


####################
# Get game results #
####################

full.results <- read.csv("gameLogs_1998_2016.csv", stringsAsFactors = F)
pitcher.scores.full <- read.csv("pitcherScores_1998_2016.csv", stringsAsFactors = F)
team.scores.full <- read.csv("teamScores_1998_2016.csv", stringsAsFactors = F)

playoff.preds.list <- list()
list.ind <- 1


####################################
# Predict playoffs for all seasons #
####################################

for (year in 1998:2016) {
  print(paste0(year, "..."))
  
  playoff.results <- full.results %>% filter(playoffs, Year == year) %>%
    arrange(team, gameNum)
  
  reg.results <- full.results %>% filter(!playoffs, Year == year)
  
  
  ####################################################
  # Format playoff results into home/away data frame #
  ####################################################
  
  playoff.results$series.name <- 
    mapply(FUN = function(x, y){paste(sort(c(x, y)), collapse = "")}, x = playoff.results$team, y = playoff.results$opp)
  
  playoff.results <- playoff.results %>% arrange(series.name, team) %>% group_by(series.name, team) %>%
    mutate(game = 1:n()) %>% ungroup
  
  p.home <- playoff.results %>% filter(home) %>% arrange(series.name, game) %>%
    select(home.team = team, home.name = Name, home.starter = ID, home.result = result, home.R = R)
  
  p.away <- playoff.results %>% filter(!home) %>% arrange(series.name, game) %>%
    select(away.team = team, away.name = Name, away.starter = ID, away.result = result, away.R = R)
  
  playoffs.df <- cbind(p.home, p.away) %>% mutate(winner = ifelse(home.result == "W", home.team, away.team)) %>%
    select(home.team, home.name, home.starter, away.team, away.name, away.starter, home.R, away.R, winner) %>%
    as.data.frame()
  
  
  ##########################
  # Get model scores -- JW #
  ##########################
  
  team.scores <- filter(team.scores.full, Year == year) %>% select(-Year)
  pitcher.scores.final <- filter(pitcher.scores.full, Year == year) %>% select(-Year)
  
  
  ###############################
  # Prediction data frame -- JW #
  ###############################
  
  hfa <- 1.17
  
  playoff.preds.JW <- playoffs.df %>% 
    left_join(team.scores, by = c("home.team" = "team")) %>% rename(home.offense = score) %>%
    left_join(team.scores, by = c("away.team" = "team")) %>% rename(away.offense = score) %>%
    left_join(pitcher.scores.final, by = c("home.starter" = "ID")) %>% rename(home.defense = score) %>%
    left_join(pitcher.scores.final, by = c("away.starter" = "ID")) %>% rename(away.defense = score) %>%
    mutate(home.lambda = home.offense*away.defense, away.lambda = away.offense*home.defense,
           home.raw = pskellam(-1, lambda1 = away.lambda, lambda2 = home.lambda),
           away.raw = pskellam(-1, lambda1 = home.lambda, lambda2 = away.lambda),
           home.percent.JW = home.raw/(home.raw + away.raw), 
           away.percent.JW = away.raw/(away.raw + home.raw),
           home.percent.JW.adj = hfa*home.percent.JW/((hfa - 1)*home.percent.JW + 1),
           away.percent.JW.adj = (away.percent.JW/hfa)/((1/hfa - 1)*away.percent.JW + 1))
  
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
    mutate(home.percent.WP = WP.home/(WP.home + WP.away),
      away.percent.WP = WP.away/(WP.home + WP.away),
      #home.percent.WP = (WP.home - WP.away + 1)/2,
      #away.percent.WP = (WP.away - WP.home + 1)/2,
      home.percent.WP.adj = hfa*home.percent.WP/((hfa - 1)*home.percent.WP + 1),
      away.percent.WP.adj = (away.percent.WP/hfa)/((1/hfa - 1)*away.percent.WP + 1))
  
  
  ##################################
  # Combine prediction data frames #
  ##################################
  
  playoff.preds <- cbind(playoff.preds.pre, select(playoff.preds.WP, home.percent.WP, home.percent.WP.adj)) %>%
    mutate(home.won = winner == home.team)
  
  playoff.preds.list[[list.ind]] <- playoff.preds
  
  list.ind <- list.ind + 1
}

###########################################
# Add year, stack into one big data frame #
###########################################

addYear <- function(x, y) {
  out.df <- x
  out.df$Year <- y
  return(out.df)
}

# Deal with pitchers
playoff.preds.list.final <- mapply(FUN = addYear, x = playoff.preds.list, y = 1998:2016, SIMPLIFY = F)

playoff.preds.full <- rbindlist(playoff.preds.list.final)

########################
# Evaluate predictions #
########################

# Confusion matrices
(conf.WP <- table(playoff.preds.full$home.won, playoff.preds.full$home.percent.WP > .5))
(conf.WP.adj <- table(playoff.preds.full$home.won, playoff.preds.full$home.percent.WP.adj > .5))
(conf.JW <- table(playoff.preds.full$home.won, playoff.preds.full$home.percent.JW > .5))
(conf.JW.adj <- table(playoff.preds.full$home.won, playoff.preds.full$home.percent.JW.adj > .5))

# Error rates
sum(playoff.preds.full$home.won == (playoff.preds.full$home.percent.WP > .5))/nrow(playoff.preds.full)
sum(playoff.preds.full$home.won == (playoff.preds.full$home.percent.WP.adj > .5))/nrow(playoff.preds.full)
sum(playoff.preds.full$home.won == (playoff.preds.full$home.percent.JW > .5))/nrow(playoff.preds.full)
sum(playoff.preds.full$home.won == (playoff.preds.full$home.percent.JW.adj > .5))/nrow(playoff.preds.full)

# Brier scores
(brier.WP <- mean((playoff.preds.full$home.percent.WP - playoff.preds.full$home.won)^2))
(brier.WP.adj <- mean((playoff.preds.full$home.percent.WP.adj - playoff.preds.full$home.won)^2))
(brier.JW <- mean((playoff.preds.full$home.percent.JW - playoff.preds.full$home.won)^2))
(brier.JW.adj <- mean((playoff.preds.full$home.percent.JW.adj - playoff.preds.full$home.won)^2))

# Discrimination slopes
playoff.preds.full %>% group_by(home.won) %>%
  summarize(home.percent.WP = mean(home.percent.WP), home.percent.WP.adj = mean(home.percent.WP.adj),
            home.percent.JW = mean(home.percent.JW), home.percent.JW.adj = mean(home.percent.JW.adj))

# ROC curves
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

it <- seq(1, 0, by = -.001)

# Note that the ROC curves for each model and its home-adjusted counterpart are the same
roc.tpr <- data.frame(thresh = it,
                  WP = sapply(it, truePositiveRate, playoff.preds.full$home.percent.WP, playoff.preds.full$home.won),
                  Poisson.MLE = sapply(it, truePositiveRate, playoff.preds.full$home.percent.JW, playoff.preds.full$home.won)) %>%
  gather(key = Model, value = tpr, -thresh)

roc.fpr <- data.frame(thresh = it,
                  WP = sapply(it, falsePositiveRate, playoff.preds.full$home.percent.WP, playoff.preds.full$home.won),
                  Poisson.MLE = sapply(it, falsePositiveRate, playoff.preds.full$home.percent.JW, playoff.preds.full$home.won)) %>%
  gather(key = Model, value = fpr, -thresh)

roc <- left_join(roc.tpr, roc.fpr, by = c("thresh", "Model"))

ggplot(data = roc, aes(x = fpr, y = tpr, color = Model)) + geom_line() +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  labs(title = "ROC Curves", x = "False Positive Rate", y = "True Positive Rate")

# Distributions of predictions
hist.df <- data.frame(Probability = c(playoff.preds.full$home.percent.JW, playoff.preds.full$home.percent.WP),
                      Model = rep(c("PML", "Simple"), each = nrow(playoff.preds.full)))

ggplot(data = hist.df, aes(x = Probability, fill = Model)) + 
  geom_histogram(alpha = .5, position = "identity") +
  scale_fill_manual(values = c("red", "blue")) +
  coord_cartesian(xlim = c(0, 1)) +
  labs(title = "Distribution of Predictions") +
  ylab("Count")





