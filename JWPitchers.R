library(rvest)
library(data.table)
library(ggplot2)
library(tidyr)
library(plyr)
library(dplyr)


################
# JW functions #
################

runs.array.pitchers <- function(fullSeason.df, teamCol = "team", pitcherCol = "starter", 
                                runsCol = "R", runsAgainstCol = "RA", opponentCol = "opp",
                                min.starts = 20) {
  fullSeason.df <- as.data.frame(fullSeason.df) %>%
    group_by_(pitcherCol) %>%
    mutate(starts = n()) %>%
    ungroup() %>%
    mutate(pitcherID = ifelse(starts > min.starts, paste0(team, "-", starter), "Other"))
  
  teamNames <- fullSeason.df[,teamCol][[1]] %>% unique()

  pitcherNames <- unique(fullSeason.df$pitcherID)
  
  maxRuns <- max(fullSeason.df[,runsCol])
  
  # Basically a reference of all possible team/run matchups
  all.matchups <- data.frame(team = rep(teamNames, each = length(pitcherNames)), 
                             pitcher = rep(pitcherNames, length(teamNames)), 
                             runs = rep(0:maxRuns, each = length(teamNames)*length(pitcherNames)),
                             stringsAsFactors = F)
  
  names(all.matchups) <- c(teamCol, "pitcherID", runsAgainstCol)
  
  res.mat <- fullSeason.df %>% group_by_(opponentCol, "pitcherID", runsAgainstCol) %>%
    summarize(N = n())
  
  names(res.mat) <- c(teamCol, "pitcherID", runsAgainstCol, "N")
  
  res.mat <- right_join(res.mat, all.matchups, by = c(teamCol, "pitcherID", runsAgainstCol))
  
  res.mat[is.na(res.mat)] <- 0
  
  runs.arr <- array(res.mat$N, dim = c(length(pitcherNames), 30, maxRuns + 1), 
                    dimnames = list(pitcherNames, teamNames, as.character(0:maxRuns))) %>%
    aperm(perm = c(2, 1, 3))
  
  return(runs.arr)
}

score.grad.2 <- function(teamVar, oppVars, maxRuns, freqSlice) {
  runVec <- rep(1:maxRuns, each = length(oppVars))
  
  oppVec <- rep(oppVars, maxRuns)
  freqVec <- as.vector(freqSlice)
  
  out.score <- sum(freqVec*(runVec/teamVar - oppVars))
  return(out.score)
}

jw.gradient.pitchers <- function(runs.arr, iterations = 1000, speed = .001, startVal = 1) {
  runsList <- alply(runs.arr, 1)
  runsAgainstList <- alply(runs.arr, 2)
  
  R.scores <- matrix(0, nrow = 30, ncol = iterations)
  RA.scores <- matrix(0, nrow = dim(runs.arr)[2], ncol = iterations)
  R.scores[,1] <- startVal
  RA.scores[,1] <- startVal
  
  maxRuns <- dim(runs.arr)[3]
  
  for (i in 2:iterations) {
    if (i %% 100 == 0) {
      print(i)
    }
    
    diff.means <- mean(R.scores[,i - 1]) - mean(RA.scores[,i - 1])
    
    grad.Rs <- mapply(score.grad.2,
                      teamVar = R.scores[,i - 1],
                      freqSlice = runsList,
                      MoreArgs = list(oppVars = RA.scores[,i - 1], maxRuns = maxRuns)
    ) - (200/nrow(R.scores))*diff.means
    
    
    R.scores[,i] <- R.scores[,i - 1] + speed*grad.Rs
    
    grad.RAs <- mapply(score.grad.2,
                       teamVar = RA.scores[,i - 1],
                       freqSlice = runsAgainstList,
                       MoreArgs = list(oppVars = R.scores[,i - 1], maxRuns = maxRuns)
    ) + (200/nrow(RA.scores))*diff.means
    
    RA.scores[,i] <- RA.scores[,i - 1] + speed*grad.RAs
    
    team.scores <- data.frame(team = rownames(runs.arr), 
                              score = R.scores[,iterations])
    
    pitcher.scores <- data.frame(team = colnames(runs.arr),
                                 score = RA.scores[,iterations],
                                 starts = apply(runs.arr, 2, sum))
    
  }
  
  out.list <- list(team.scores = team.scores, pitcher.scores = pitcher.scores,
                   all.R = R.scores, all.RA = RA.scores)
  
  return(out.list)
  
}


