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
    ungroup()
  
  fullSeason.df$pitcherID <- ifelse(fullSeason.df$starts > min.starts,
                                    as.character(fullSeason.df[[pitcherCol]]),
                                    "Other")
  
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

# Score for one player/team
score.grad.2 <- function(teamVar, oppVars, maxRuns, freqSlice) {
  runVec <- rep(0:(maxRuns-1), each = length(oppVars))
  
  oppVec <- rep(oppVars, maxRuns)
  freqVec <- as.vector(freqSlice)
  
  out.score <- sum(freqVec*(runVec/teamVar - oppVec))
  return(out.score)
}

log.likelihood <- function(runs.arr, R.vec, RA.vec, mean.adj.fac) {
  num.teams <- length(R.vec)
  num.pitchers <- length(RA.vec)
  max.runs <- dim(runs.arr)[3] - 1
  
  names(R.vec) <- NULL
  names(RA.vec) <- NULL
  
  liks.df <- data.frame(runs = rep(0:max.runs, each = num.teams*num.pitchers),
                        pitchers.lambda = rep(RA.vec, each = num.teams),
                        teams.lambda = R.vec,
                        frequency = as.vector(runs.arr)) %>%
    mutate(lambda = teams.lambda*pitchers.lambda, 
           like = frequency*runs*log(lambda) - lambda*frequency - frequency*log(factorial(runs)))
  
  sq.diff.means <- (weighted.mean(liks.df$teams.lambda, liks.df$frequency) - weighted.mean(liks.df$pitchers.lambda, liks.df$frequency))^2
  
  llik <- sum(liks.df$like) - mean.adj.fac*sq.diff.means
  
  return(llik)
}

jw.gradient.pitchers.bt <- function(runs.arr, iterations = 1000, startVal = 1, print.every.n = 100) {
  mean.adj.fac <- 1000
  
  runsList <- alply(runs.arr, 1)
  runsAgainstList <- alply(runs.arr, 2)
  
  R.scores <- matrix(0, nrow = 30, ncol = iterations)
  RA.scores <- matrix(0, nrow = dim(runs.arr)[2], ncol = iterations)
  R.scores[,1] <- startVal
  RA.scores[,1] <- startVal
  
  maxRuns <- dim(runs.arr)[3]
  
  likelihoods <- rep(0, iterations)
  likelihoods[1] <- log.likelihood(runs.arr, R.scores[,1], RA.scores[,1], mean.adj.fac = mean.adj.fac)
  
  alphas <- rep(0, iterations - 1)
  xs <- rep(0, iterations - 1)
  mean.diffs.vec <- rep(0, iterations)
  
  max.counter <- 0
  
  for (i in 2:iterations) {
    if (i %% print.every.n == 0) {
      print(i)
    }
    
    # Calculate gradient
    
    percent.of.all.starts <- apply(runs.arr, 2, sum)/sum(apply(runs.arr, 2, sum))
    percent.of.all.games <- apply(runs.arr, 1, sum)/sum(apply(runs.arr, 1, sum))
    
    diff.means <- weighted.mean(R.scores[,i - 1], percent.of.all.games) - weighted.mean(RA.scores[,i - 1], percent.of.all.starts)
    mean.diffs.vec[i] <- diff.means
    
    grad.Rs <- mapply(score.grad.2,
                      teamVar = R.scores[,i - 1],
                      freqSlice = runsList,
                      MoreArgs = list(oppVars = RA.scores[,i - 1], maxRuns = maxRuns)
    ) - (2*mean.adj.fac*percent.of.all.games)*diff.means
    
    
    grad.RAs <- mapply(score.grad.2,
                       teamVar = RA.scores[,i - 1],
                       freqSlice = runsAgainstList,
                       MoreArgs = list(oppVars = R.scores[,i - 1], maxRuns = maxRuns)
    ) + (2*mean.adj.fac*percent.of.all.starts)*diff.means
    # End calculate gradient

        
    
    # Find step size (if it hasn't been too small for too long)
    if (max.counter < 10) {
      m <- sqrt(sum(grad.RAs^2) + sum(grad.Rs^2))
      c <- .5
      t <- m*c
      tau <- .8
      
      alpha <- .005
      
      
      
      for (x in 1:7) {
        lik.old <- log.likelihood(runs.arr, R.scores[,i-1], RA.scores[,i-1], mean.adj.fac = mean.adj.fac)
        new.Rs <- pmax(R.scores[,i-1] + alpha*grad.Rs, .1)
        new.RAs <- pmax(RA.scores[,i-1] + alpha*grad.RAs, .1)
        lik.new <- log.likelihood(runs.arr, new.Rs, new.RAs, mean.adj.fac = mean.adj.fac)
        increase <- lik.new - lik.old
        
        if (x == 7) {
          max.counter <- max.counter + 1
        }
        if (increase > alpha*t) {
          xs[i-1] <- x
          max.counter <- 0
          break
        } else {
          alpha <- alpha*tau
        }
      }
    } else {
      alpha <- .001
    }
    
    # Adjust by step size
    RA.scores[,i] <- pmax(RA.scores[,i - 1] + alpha*grad.RAs, .1)
    if (min(RA.scores[,i - 1] + alpha*grad.RAs) < 0) {
      print("Warning: Pitcher score below zero before adjustment")
    }
    
    R.scores[,i] <- pmax(R.scores[,i - 1] + alpha*grad.Rs, .1)
    if (min(R.scores[,i - 1] + alpha*grad.Rs) < 0) {
      print("Warning: Team score below zero before adjustment")
    }
    
    team.scores <- data.frame(team = rownames(runs.arr), 
                              score = R.scores[,iterations])
    
    pitcher.scores <- data.frame(team = colnames(runs.arr),
                                 score = RA.scores[,iterations],
                                 starts = apply(runs.arr, 2, sum))
    
    alphas[i-1] <- alpha
    
    if (sum(R.scores[,i] < 0) > 1) {
      print("Teams: ", sum(R.scores[,i] < 0), " negative")
    }
    
    if (sum(RA.scores[,i] < 0) > 1) {
      print("Teams: ", sum(RA.scores[,i] < 0), " negative")
    }
    
    likelihoods[i] <- log.likelihood(runs.arr, R.scores[,i], RA.scores[,i], mean.adj.fac = mean.adj.fac)
    
  }
  
  out.list <- list(team.scores = team.scores, pitcher.scores = pitcher.scores,
                   all.R = R.scores, all.RA = RA.scores, likelihoods = likelihoods, alphas = alphas, xs = xs,
                   mean.diffs.vec = mean.diffs.vec)
  
  return(out.list)
  
}

# Doesn't use backtracking line search
jw.gradient.pitchers.bt.2 <- function(runs.arr, iterations = 1000, speed = .001, startVal = 1, print.every.n = 100) {
  mean.adj.fac <- 1000
  
  runsList <- alply(runs.arr, 1)
  runsAgainstList <- alply(runs.arr, 2)
  
  R.scores <- matrix(0, nrow = 30, ncol = iterations)
  RA.scores <- matrix(0, nrow = dim(runs.arr)[2], ncol = iterations)
  R.scores[,1] <- startVal
  RA.scores[,1] <- startVal
  
  maxRuns <- dim(runs.arr)[3]
  
  likelihoods <- rep(0, iterations)
  likelihoods[1] <- log.likelihood(runs.arr, R.scores[,1], RA.scores[,1], mean.adj.fac = mean.adj.fac)
  
  mean.diffs.vec <- rep(0, iterations)
  
  for (i in 2:iterations) {
    if (i %% print.every.n == 0) {
      print(i)
    }
    
    # Calculate gradient
    
    percent.of.all.starts <- apply(runs.arr, 2, sum)/sum(apply(runs.arr, 2, sum))
    percent.of.all.games <- apply(runs.arr, 1, sum)/sum(apply(runs.arr, 1, sum))
    
    diff.means <- weighted.mean(R.scores[,i - 1], percent.of.all.games) - weighted.mean(RA.scores[,i - 1], percent.of.all.starts)
    mean.diffs.vec[i] <- diff.means
    
    grad.Rs <- mapply(score.grad.2,
                      teamVar = R.scores[,i - 1],
                      freqSlice = runsList,
                      MoreArgs = list(oppVars = RA.scores[,i - 1], maxRuns = maxRuns)
    ) - (2*mean.adj.fac*percent.of.all.games)*diff.means
    
    
    R.scores[,i] <- R.scores[,i - 1] + speed*grad.Rs
    
    grad.RAs <- mapply(score.grad.2,
                       teamVar = RA.scores[,i - 1],
                       freqSlice = runsAgainstList,
                       MoreArgs = list(oppVars = R.scores[,i - 1], maxRuns = maxRuns)
    ) + (2*mean.adj.fac*percent.of.all.starts)*diff.means
    # End calculate gradient
    
    
    RA.scores[,i] <- RA.scores[,i - 1] + speed*grad.RAs
    
    team.scores <- data.frame(team = rownames(runs.arr), 
                              score = R.scores[,iterations])
    
    pitcher.scores <- data.frame(team = colnames(runs.arr),
                                 score = RA.scores[,iterations],
                                 starts = apply(runs.arr, 2, sum))
    
    likelihoods[i] <- log.likelihood(runs.arr, R.scores[,i], RA.scores[,i], mean.adj.fac = mean.adj.fac)
    
  }
  
  out.list <- list(team.scores = team.scores, pitcher.scores = pitcher.scores,
                   all.R = R.scores, all.RA = RA.scores, likelihoods = likelihoods, mean.diffs.vec = mean.diffs.vec)
  
  return(out.list)
  
}




