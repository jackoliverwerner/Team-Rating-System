library(dplyr)
library(rvest)
library(data.table)
library(tidyr)

###########################
# Bradley-Terry functions #
###########################

wins.matrix <- function(fullSeason.df, teamCol = "team", opponentCol = "opp", winCol = "W", lossCol = "L") {
  fullSeason.df <- as.data.frame(fullSeason.df)
  
  teamNames <- unique(fullSeason.df[,teamCol])
  
  # Basically a reference of all possible team matchups
  all.matchups <- data.frame(team = rep(teamNames, each = 30), opp = rep(teamNames, 30), 
                             stringsAsFactors = F)
  
  # All possible team matchups, joined with the actual wins and losses
  res.mat <- fullSeason.df %>% group_by_(teamCol, opponentCol) %>%
    summarize(W = sum(result == winCol), L = sum(result == lossCol)) %>%
    right_join(all.matchups, by = c(teamCol, opponentCol))
  
  res.mat[is.na(res.mat)] <- 0
  
  # WINS: Now in more pliable matrix form! A matrix of wins of each team over the others
  wins.mat <- matrix(res.mat$W, nrow = 30, ncol = 30, byrow = T)
  
  rownames(wins.mat) <- unique(res.mat$team); colnames(wins.mat) <- unique(res.mat$team)
  
  return(wins.mat)
}

# Write function to update score
updateScores <- function(p.js, gs, w.tot, p.i) {
  return(sum(w.tot)*sum(gs/(p.js + p.i))^(-1))
}

bradley.terry <- function(wins.mat, iterations = 100)  {
  wins.vec <- apply(wins.mat, 1, sum)
  
  # GAMES: A matrix/list of games between each pair of teams
  games.mat <- wins.mat + t(wins.mat)
  
  games.list <- split(games.mat, f = rep(1:30, each = 30))
  
  # PROBABILITIES: Matrix/list of ps at each iteration: Cols are iterations, rows are teams
  
  mle.scores <- matrix(0, nrow = 30, ncol = iterations)
  rownames(mle.scores) <- rownames(wins.mat)
  
  mle.scores[,1] <- 1
  
  
  
  # MAGIC HAPPENS
  for (i in 2:iterations) {
    new.ps <- mapply(updateScores,
                     gs = games.list,
                     w.tot = wins.vec,
                     p.i = mle.scores[,i - 1],
                     MoreArgs = list(p.js = mle.scores[,i - 1]))
    
    mle.scores[,i] <- new.ps/sum(new.ps)*30
  }
  
  team.scores <- data.frame(team = rownames(wins.mat), score = mle.scores[,iterations]) %>%
    arrange(desc(score))
  
  out.list <- list(scores = team.scores, all.scores = mle.scores)
  
  return(out.list)
}

maxAt <- function(it) {
  return(names(which.max(mle.scores[,it])))
}

orderAt <- function(it) {
  return(row.names(mle.scores)[order(mle.scores[,it])])
}

team.grad <- function(p, pvec, wvec, lvec) {
  out.vec <- sum(wvec/p - (wvec + lvec)/(pvec + p))
  return(out.vec)
}

log.likelihood <- function(wins.mat, ps) {
  ll.out <- 0
  
  for (i in 1:nrow(wins.mat)) {
    for (j in 1:nrow(wins.mat)) {
      if (ps[i] > 0 & ps[j] > 0)
      ll.out <- ll.out + wins.mat[i, j]*log(ps[i]) - wins.mat[i, j]*log(ps[i] + ps[j])
    }
  }
  names(ll.out) <- NULL
  return(ll.out)
}

bt.gradient <- function(wins.mat, iterations = 1000, speed = .001)  {
  wins.list <- split(t(wins.mat), f = rep(1:30, each = 30))
  losses.list <- split(wins.mat, f = rep(1:30, each = 30))
  
  # PROBABILITIES: Matrix/list of ps at each iteration: Cols are iterations, rows are teams
  mle.scores <- matrix(0, nrow = 30, ncol = iterations)
  rownames(mle.scores) <- rownames(wins.mat)
  mle.scores[,1] <- 1
  
  lls <- rep(0, iterations)
  lls[1] <- log.likelihood(wins.mat, mle.scores[,1])
  
  # MAGIC HAPPENS
  for (i in 2:iterations) {
    print(i)
    grad.ps <- mapply(team.grad,
                     p = mle.scores[,i - 1],
                     wvec = wins.list,
                     lvec = losses.list,
                     MoreArgs = list(pvec = mle.scores[,i - 1]))
    
    new.ps <- mle.scores[,i - 1] + speed*grad.ps
    
    mle.scores[,i] <- new.ps/sum(new.ps)*30
    
    lls[i] <- log.likelihood(wins.mat, mle.scores[,i])
  }
  
  team.scores <- data.frame(team = rownames(wins.mat), score = mle.scores[,iterations]) %>%
    arrange(desc(score))
  
  out.list <- list(scores = team.scores, all.scores = mle.scores, lls = lls)
  
  return(out.list)
}

#########################
# Bradley-Terry Results #
#########################

# Get season results data frame
setwd("C:/Users/jack.werner1/Documents/BB/Team-Rating-System")

year <- 2016

season.results <- read.csv("gameLogs_1998_2016.csv", stringsAsFactors = F) %>% filter(!playoffs, Year == year)

# Work some Bradley-Terry magic on that data frame
its <- 1000

bt.results <- wins.matrix(season.results) %>% bt.gradient(iterations = its, speed = 0.001)
#bt.results <- wins.matrix(season.results) %>% bradley.terry(iterations = its)

mle.scores <- bt.results$all.scores
final.scores <- bt.results$scores

# Check out the results
result.df <- data.frame(iteration = 1:its,
                        score = as.vector(t(mle.scores)),
                        team = rep(rownames(mle.scores), each = its))

ggplot(data = result.df, aes(x = iteration, y = score, color = team)) + geom_line()

orderAt(its)

# Plot score by record
team.ref <- read.csv("MLBTeams.csv", stringsAsFactors = F) %>% filter(Year == year)

team.records <- season.results %>% group_by(team) %>%
  summarize(W = sum(result == "W"), L = sum(result == "L")) %>%
  as.data.frame() %>%
  left_join(team.ref, by = c("team" = "Team")) %>%
  left_join(final.scores, by = "team") %>%
  select(-Year)

ggplot(data = team.records, aes(x = W, y = score, color = League)) + geom_point()


ggplot(data = team.records, aes(x = W, y = score, color = Division)) + geom_point() + geom_line()

# Look at division win totals vs. score
div.recs <- team.records %>% group_by(Division) %>%
  summarize(W = sum(W), score = mean(score))

ggplot(data = div.recs, aes(x = W, y = score)) + geom_point()



