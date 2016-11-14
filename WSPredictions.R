##########
# Set up #
##########

#setwd("/Users/jackwerner/Documents/My Stuff/Baseball/Team Rating System")
setwd("C:/Users/jack.werner1/Documents/BB")

source("getSeasonResults.R")
source("JWPitchers.R")

MLBteams <- paste0(getwd(), "/MLBteams.csv")


##############
# Get scores #
##############

year <- 2016

season.results <- getLeagueResults(year, MLBteams) %>% filter(!playoffs)

season.results$starter[season.results$starter == "A.Sanchez"] <- 
  ifelse(season.results$team[season.results$starter == "A.Sanchez"] == "TOR", "Aa.Sanchez", "An.Sanchez")

season.results$starter[season.results$starter == "C.Anderson"] <- 
  ifelse(season.results$team[season.results$starter == "C.Anderson"] == "MIL", "Ch.Anderson", "Co.Anderson")

its <- 10000

jw.results <- runs.array.pitchers(season.results, min.starts = 10) %>% 
  jw.gradient.pitchers(iterations = its, speed = .001, startVal = 2)


team.scores <- jw.results$team.scores
pitcher.scores <- jw.results$pitcher.scores

namesFunc <- function(char) {
  spl <- strsplit(char, "-")
  len <- length(spl[[1]])
  return(spl[[1]][len])
}
pitcher.scores$name <- sapply(as.character(pitcher.scores$team), namesFunc)

pitcher.scores.final <- pitcher.scores %>% group_by(name) %>%
  summarize(score = weighted.mean(score, starts))


#################################
# Make World Series predictions #
#################################

# Cubs/Indians
cubs.rot <- c("J.Lester", "J.Arrieta", "K.Hendricks", "J.Lackey")
inds.rot <- c("C.Kluber", "J.Tomlin", "T.Bauer", "R.Merritt")

ws.offenses <- filter(team.scores, team %in% c("CHC", "CLE"))
ws.pitchers <- filter(pitcher.scores.final, name %in% c(cubs.rot, inds.rot))


library(skellam)

ws.games <- data.frame(game = 1:7, 
                       cubs.pitch = c("J.Lester", "J.Arrieta", "K.Hendricks",
                                      "J.Lackey", "J.Lester", "J.Arrieta", "K.Hendricks"),
                       inds.pitch = c("C.Kluber", "T.Bauer", "J.Tomlin", "C.Kluber",
                                      "T.Bauer", "J.Tomlin", "C.Kluber"),
                       home.field = c("Indians", "Indians", "Cubs", "Cubs", "Cubs",
                                      "Indians", "Indians"),
                       cubs.o.score = ws.offenses$score[2],
                       inds.o.score = ws.offenses$score[1]) %>%
  left_join(ws.pitchers, by = c("cubs.pitch" = "name")) %>%
  rename(cubs.d.score = score) %>%
  left_join(ws.pitchers, by = c("inds.pitch" = "name")) %>%
  rename(inds.d.score = score) %>%
  mutate(cubs.lambda = cubs.o.score*inds.d.score, inds.lambda = inds.o.score*cubs.d.score,
         cubs.raw = pskellam(-1, lambda1 = inds.lambda, lambda2 = cubs.lambda),
         inds.raw = pskellam(-1, lambda1 = cubs.lambda, lambda2 = inds.lambda),
         cubs.percent.pre = cubs.raw/(cubs.raw + inds.raw), 
         inds.percent.pre = inds.raw/(cubs.raw + inds.raw),
         cubs.odds.pre = cubs.percent.pre/(1-cubs.percent.pre),
         cubs.percent = ifelse(home.field == "Cubs",
                               (cubs.odds.pre)*1.128/((cubs.odds.pre)*1.128 + 1),
                               (cubs.odds.pre)/1.128/((cubs.odds.pre)/1.128 + 1)),
         inds.percent = 1 - cubs.percent)

game.probs <- ws.games %>% select(game, cubs.percent)

g7 <- c("Cubs", "Indians")
g6 <- rep(c("Cubs", "Indians"), each = length(g7))
g5 <- rep(c("Cubs", "Indians"), each = length(g6))
g4 <- rep(c("Cubs", "Indians"), each = length(g5))
g3 <- rep(c("Cubs", "Indians"), each = length(g4))
g2 <- rep(c("Cubs", "Indians"), each = length(g3))
g1 <- rep(c("Cubs", "Indians"), each = length(g2))

all.poss <- cbind(g1, g2, g3, g4, g5, g6, g7)

find.length <- function(vec1, vec2) {
  if (max(vec1) > 3) {
    games.1 <- min(which(vec1 == 4))    
  } else {
    games.1 <- 7
  }
  
  if (max(vec2) > 3) {
    games.2 <- min(which(vec2 == 4))    
  } else {
    games.2 <- 7
  }
  
  return(min(games.1, games.2))
}

all.poss.df <- data.frame(scenario = rep(1:128, each = 7), game = 1:7, winner = as.vector(t(all.poss))) %>%
  group_by(scenario) %>% mutate(w.cubs = cumsum(winner == "Cubs"), w.inds = cumsum(winner == "Indians")) %>%
  left_join(game.probs, by = "game") %>% 
  mutate(prob = ifelse(winner == "Cubs", cubs.percent, 1 - cubs.percent)) %>%
  summarize(results = paste(winner, collapse = ""), prob = prod(prob), 
            winner = names(which.max(table(winner))), games = find.length(w.cubs, w.inds)) %>% ungroup() %>%
  group_by(winner, games) %>% summarize(prob = sum(prob)) %>% ungroup()

ws.probs <- all.poss.df %>% group_by(winner) %>% summarize(prob = sum(prob))


# After game 1

all.poss.df <- data.frame(scenario = rep(1:128, each = 7), game = 1:7, winner = as.vector(t(all.poss))) %>%
  group_by(scenario) %>% mutate(w.cubs = cumsum(winner == "Cubs"), w.inds = cumsum(winner == "Indians")) %>%
  left_join(game.probs, by = "game") %>% 
  mutate(prob = ifelse(winner == "Cubs", cubs.percent, 1 - cubs.percent)) %>%
  summarize(results = paste(winner, collapse = ""), prob = prod(prob), 
            winner = names(which.max(table(winner))), games = find.length(w.cubs, w.inds)) %>% ungroup() %>%
  mutate(g1.winner = substr(results, 1, 1)) %>% filter(g1.winner == "I") %>%
  group_by(winner, games) %>% summarize(prob = sum(prob)) %>% ungroup() %>% mutate(prob = prob/sum(prob))

ws.probs <- all.poss.df %>% group_by(winner) %>% summarize(prob = sum(prob))
















