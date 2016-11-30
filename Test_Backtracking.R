##########
# Set up #
##########

#setwd("/Users/jackwerner/Documents/My Stuff/Baseball/Team Rating System")
setwd("C:/Users/jack.werner1/Documents/BB/Team-Rating-System")

source("getSeasonResults.R")
source("JWPitchers.R")

MLBteams <- paste0(getwd(), "/MLBteams.csv")


##############
# Get scores #
##############

year <- 2015

season.results <- read.csv("gameLogs_1998_2016.csv") %>%
  filter(Year == year, !playoffs)

# Scrape and format season results data frame
#season.results <- getLeagueResults(year, MLBteams) %>% filter(!playoffs)


its <- 1000

# Optimize
ptm <- proc.time()
jw.results.old <- runs.array.pitchers(season.results, min.starts = 15, pitcherCol = "ID") %>% 
  jw.gradient.pitchers.bt.2(iterations = its, speed = .001, startVal = 2, print.every.n = 10)
proc.time() - ptm

plot(1:its, jw.results.old$likelihoods, type = "l")


ptm <- proc.time()
jw.results.new <- runs.array.pitchers(season.results, min.starts = 15, pitcherCol = "ID") %>% 
  jw.gradient.pitchers.bt(iterations = its, speed = .001, startVal = 2, print.every.n = 10)
proc.time() - ptm

plot(1:(its-1), jw.results.new$alphas, type = "l")
table(jw.results.new$xs)

a.df <- data.frame(iteration = 1:its,
                   log.likelihood = c(jw.results.old$likelihoods, jw.results.new$likelihoods),
                   algorithm = rep(c("Without line search", "With line search"), each = its))

ggplot(data = a.df, aes(x = iteration, y = log.likelihood, color = algorithm)) + geom_line()


#######
# OLD #
#######

# Finalize score dataframes
team.scores <- jw.results.old$team.scores
pitcher.scores <- jw.results.old$pitcher.scores

namesFunc <- function(char) {
  spl <- strsplit(char, "-")
  len <- length(spl[[1]])
  return(spl[[1]][len])
}
pitcher.scores$name <- sapply(as.character(pitcher.scores$team), namesFunc)

# Include pitchers with not enough starts (assign them "Other" score)
pitcher.scores.2 <- pitcher.scores %>% group_by(name) %>%
  summarize(score = weighted.mean(score, starts))

pitcher.scores.final.pre <- data.frame(ID = unique(season.results$ID))

pitcher.scores.final <- pitcher.scores.final.pre %>%
  left_join(pitcher.scores.2, by = c("ID"="name")) %>%
  rename(name = ID)

pitcher.scores.final[is.na(pitcher.scores.final$score),]$score <- pitcher.scores.2$score[pitcher.scores.2$name == "Other"]

pitcher.ref <- season.results %>% select(ID, Name) %>% unique()

pitcher.scores.final <- left_join(pitcher.scores.final, pitcher.ref, by = c("name" = "ID")) %>%
  select(ID = name, Name, score)

# Weighted means
runs.arr <- runs.array.pitchers(season.results, min.starts = 15, pitcherCol = "ID")
a <- data.frame(starts = apply(runs.arr, 2, sum), ID = names(apply(runs.arr, 2, sum)))
a$ID <- strsplit(as.character(a$ID), "-") %>% sapply(function(x){x[2]})
row.names(a) <- NULL

a <- season.results %>% group_by(ID) %>% summarize(starts = n()) %>% ungroup()

pscores.means <- pitcher.scores.final %>% left_join(a, by = "ID")

mean(team.scores$score)
weighted.mean(pscores.means$score, pscores.means$starts)

mean(team.scores$score) - weighted.mean(pscores.means$score, pscores.means$starts)
tail(jw.results.old$mean.diffs.vec, 1)

plot(1:its, jw.results.old$mean.diffs.vec, type = "l")

#######
# NEW #
#######

# Finalize score dataframes
team.scores <- jw.results.new$team.scores
pitcher.scores <- jw.results.new$pitcher.scores

namesFunc <- function(char) {
  spl <- strsplit(char, "-")
  len <- length(spl[[1]])
  return(spl[[1]][len])
}
pitcher.scores$name <- sapply(as.character(pitcher.scores$team), namesFunc)

# Include pitchers with not enough starts (assign them "Other" score)
pitcher.scores.2 <- pitcher.scores %>% group_by(name) %>%
  summarize(score = weighted.mean(score, starts))

pitcher.scores.final.pre <- data.frame(ID = unique(season.results$ID))

pitcher.scores.final <- pitcher.scores.final.pre %>%
  left_join(pitcher.scores.2, by = c("ID"="name")) %>%
  rename(name = ID)

pitcher.scores.final[is.na(pitcher.scores.final$score),]$score <- pitcher.scores.2$score[pitcher.scores.2$name == "Other"]

pitcher.ref <- season.results %>% select(ID, Name) %>% unique()

pitcher.scores.final <- left_join(pitcher.scores.final, pitcher.ref, by = c("name" = "ID")) %>%
  select(ID = name, Name, score)

# Weighted means
runs.arr <- runs.array.pitchers(season.results, min.starts = 15, pitcherCol = "ID")
a <- data.frame(starts = apply(runs.arr, 2, sum), ID = names(apply(runs.arr, 2, sum)))
a$ID <- strsplit(as.character(a$ID), "-") %>% sapply(function(x){x[2]})
row.names(a) <- NULL

a <- season.results %>% group_by(ID) %>% summarize(starts = n()) %>% ungroup()

pscores.means <- pitcher.scores.final %>% left_join(a, by = "ID")

mean(team.scores$score)
weighted.mean(pscores.means$score, pscores.means$starts)

mean(team.scores$score) - weighted.mean(pscores.means$score, pscores.means$starts)
tail(jw.results.new$mean.diffs.vec, 1)

plot(1:its, jw.results.new$mean.diffs.vec, type = "l")


#########################
# Plot iterations (old) #
#########################

# Save in-progress matrix
mle.R <- jw.results.old$all.R
mle.RA <- jw.results.old$all.RA

# Plot team score iterations
result.df <- data.frame(iteration = 1:its,
                        score = as.vector(t(mle.R)),
                        team = rep(team.scores$team, each = its))

ggplot(data = result.df, aes(x = iteration, y = score, color = team)) + geom_line()


# Plot pitcher score iterations
result.df <- data.frame(iteration = 1:its,
                        score = as.vector(t(mle.RA)),
                        team = rep(pitcher.scores$team, each = its))

ggplot(data = result.df, aes(x = iteration, y = score, group = team)) + geom_line(alpha = .1) +
  theme(legend.position="none")


#########################
# Plot iterations (new) #
#########################

# Save in-progress matrix
mle.R <- jw.results.new$all.R
mle.RA <- jw.results.new$all.RA

# Plot team score iterations
result.df <- data.frame(iteration = 1:its,
                        score = as.vector(t(mle.R)),
                        team = rep(team.scores$team, each = its))

ggplot(data = result.df, aes(x = iteration, y = score, color = team)) + geom_line()


# Plot pitcher score iterations
result.df <- data.frame(iteration = 1:its,
                        score = as.vector(t(mle.RA)),
                        team = rep(pitcher.scores$team, each = its))

ggplot(data = result.df, aes(x = iteration, y = score, group = team)) + geom_line(alpha = .1) +
  theme(legend.position="none")

