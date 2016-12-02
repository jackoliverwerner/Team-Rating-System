library(rvest)
library(data.table)
library(ggplot2)
library(tidyr)
library(plyr)
library(dplyr)


###########################
# Bradley-Terry functions #
###########################

fullSeason.df <- season.results
teamCol <- "team"
opponentCol <- "opp"
runsCol <- "R"
runsAgainstCol <- "RA"

runs.array <- function(fullSeason.df, teamCol = "team", opponentCol = "opp", runsCol = "R", runsAgainstCol = "RA") {
  fullSeason.df <- as.data.frame(fullSeason.df)
  teamNames <- unique(fullSeason.df[,teamCol])
  
  maxRuns <- max(fullSeason.df[,runsCol])
  
  # Basically a reference of all possible team/run matchups
  all.matchups <- data.frame(team = rep(teamNames, each = 30), opp = rep(teamNames, 30), runs = rep(0:maxRuns, each = 30*30),
                             stringsAsFactors = F)
  
  names(all.matchups) <- c(teamCol, opponentCol, runsCol)
  
  res.mat <- fullSeason.df %>% group_by_(teamCol, opponentCol, runsCol) %>%
    summarize(N = n()) %>%
    right_join(all.matchups, by = c(teamCol, opponentCol, runsCol))
  
  res.mat[is.na(res.mat)] <- 0
  
  runs.arr <- array(res.mat$N, dim = c(30, 30, maxRuns + 1), dimnames = list(teamNames, teamNames, NULL)) %>%
    aperm(perm = c(2, 1, 3))
  
  return(runs.arr)
}

team.grad <- function(p, pvec, wvec, lvec) {
  out.vec <- sum(wvec/p - (wvec + lvec)/(pvec + p))
  return(out.vec)
}

score.grad <- function(teamVar, oppVars, maxRuns, freqSlice) {
  runVec <- rep(1:maxRuns, each = 30)
  
  oppVec <- rep(oppVars, maxRuns)
  freqVec <- as.vector(freqSlice)
  
  out.score <- sum(freqVec*(runVec/(oppVars + teamVar) - 1))
  return(out.score)
}

score.grad.2 <- function(teamVar, oppVars, maxRuns, freqSlice) {
  runVec <- rep(1:maxRuns, each = 30)
  
  oppVec <- rep(oppVars, maxRuns)
  freqVec <- as.vector(freqSlice)
  
  out.score <- sum(freqVec*(runVec/teamVar - oppVars))
  return(out.score)
}


runs.arr <- bt.results
iterations <- 1000
speed <- .001

jw.gradient <- function(runs.arr, iterations = 1000, speed = .001) {
  runsList <- alply(runs.arr, 1)
  runsAgainstList <- alply(runs.arr, 2)
  
  R.scores <- matrix(0, nrow = 30, ncol = iterations)
  RA.scores <- matrix(0, nrow = 30, ncol = iterations)
  R.scores[,1] <- 1
  RA.scores[,1] <- 1
  
  maxRuns <- dim(runs.arr)[3]
  
  for (i in 2:iterations) {
    print(i)
    grad.Rs <- mapply(score.grad.2,
                      teamVar = R.scores[,i - 1],
                      freqSlice = runsList,
                      MoreArgs = list(oppVars = RA.scores[,i - 1], maxRuns = maxRuns))
    R.scores[,i] <- R.scores[,i - 1] + speed*grad.Rs
    
    grad.RAs <- mapply(score.grad.2,
                       teamVar = RA.scores[,i - 1],
                       freqSlice = runsAgainstList,
                       MoreArgs = list(oppVars = R.scores[,i - 1], maxRuns = maxRuns))
    
    RA.scores[,i] <- RA.scores[,i - 1] + speed*grad.RAs
    
    team.scores <- data.frame(team = rownames(runs.arr), 
                              offense.score = R.scores[,iterations],
                              defense.score = RA.scores[,iterations])

  }
  
  out.list <- list(scores = team.scores, all.R = R.scores, all.RA = RA.scores)
  
  return(out.list)
  
}



##############
# JW Results #
##############

# Get season results data frame
setwd("/Users/jackwerner/Documents/My Stuff/Baseball/Team Rating System")

year <- 2016

season.results <- read.csv("gameLogs_1998_2016.csv", stringsAsFactors = F) %>% 
  filter(!playoffs, Year == year)

its <- 1000

bt.results <- runs.array(season.results) %>% jw.gradient(iterations = its, speed = .001)

mle.R <- bt.results$all.R
mle.RA <- bt.results$all.RA

final.scores <- bt.results$scores

final.scores %>% arrange(desc(offense.score)) %>% View()
final.scores %>% arrange(defense.score) %>% View()

# Check out the results
result.df <- data.frame(iteration = 1:its,
                        score = as.vector(t(mle.R)),
                        team = rep(final.scores$team, each = its))

ggplot(data = result.df, aes(x = iteration, y = score, color = team)) + geom_line()


# Check out the results
result.df <- data.frame(iteration = 1:its,
                        score = as.vector(t(mle.RA)),
                        team = rep(final.scores$team, each = its))

ggplot(data = result.df, aes(x = iteration, y = score, color = team)) + geom_line()




