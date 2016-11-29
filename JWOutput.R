##########
# Set up #
##########

setwd("/Users/jackwerner/Documents/My Stuff/Baseball/Team Rating System")
#setwd("C:/Users/jack.werner1/Documents/BB/Team-Rating-System")

source("getSeasonResults.R")
source("JWPitchers.R")

MLBteams <- paste0(getwd(), "/MLBteams.csv")


##############
# Get scores #
##############

year <- 2010

season.results <- read.csv("gameLogs_1998_2016.csv") %>%
  filter(Year == year, !playoffs)

# Scrape and format season results data frame
#season.results <- getLeagueResults(year, MLBteams) %>% filter(!playoffs)


its <- 1000

# Optimize
jw.results <- runs.array.pitchers(season.results, min.starts = 15, pitcherCol = "ID") %>% 
  jw.gradient.pitchers.bt(iterations = its, speed = .001, startVal = 2)

plot(1:its, jw.results$likelihoods, type = "l")
#########################
# Take a look at scores #
#########################

# Save in-progress matrix
mle.R <- jw.results$all.R
mle.RA <- jw.results$all.RA

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

pitcher.scores.final.pre <- data.frame(ID = unique(season.results$ID))

pitcher.scores.final <- pitcher.scores.final.pre %>%
  left_join(pitcher.scores.2, by = c("ID"="name")) %>%
  rename(name = ID)

pitcher.scores.final[is.na(pitcher.scores.final$score),]$score <- pitcher.scores.2$score[pitcher.scores.2$name == "Other"]

pitcher.ref <- season.results %>% select(ID, Name) %>% unique()

pitcher.scores.final <- left_join(pitcher.scores.final, pitcher.ref, by = c("name" = "ID")) %>%
  select(ID = name, Name, score)

# View scores
team.scores %>% arrange(desc(score)) %>% View()
pitcher.scores.final %>% arrange(score) %>% View()

# Means
mean(team.scores$score)
mean(pitcher.scores.final$score)

# Weighted means
runs.arr <- runs.array.pitchers(season.results, min.starts = 15, pitcherCol = "ID")
a <- data.frame(starts = apply(runs.arr, 2, sum), ID = names(apply(runs.arr, 2, sum)))
a$ID <- strsplit(as.character(a$ID), "-") %>% sapply(function(x){x[2]})
row.names(a) <- NULL

a <- season.results %>% group_by(ID) %>% summarize(starts = n()) %>% ungroup()

pscores.means <- pitcher.scores.final %>% left_join(a, by = "ID")

weighted.mean(pscores.means$score, pscores.means$starts)

# Log Likelihood
likelihood <- function(results.df, teams.df, pitchers.df) {
  results.df <- as.data.frame(results.df)
  teams.df <- as.data.frame(teams.df)
  pitchers.df <- as.data.frame(pitchers.df) %>% mutate(name = factor(name))
  
  
  
  full.df <- results.df %>% mutate(starter = ifelse(starter %in% pitchers.df$name, starter, "Other"),
                                   opp = factor(opp), starter = factor(starter)) %>%
    left_join(teams.df, by = c("opp" = "team")) %>%
    rename(offense.score = score) %>%
    left_join(pitchers.df, by = c("starter" = "name")) %>%
    rename(pitcher.score = score) %>%
    mutate(lambda = offense.score*pitcher.score, loglik = RA*log(lambda) - lambda - log(factorial(RA)))
  
  return(sum(full.df$loglik))
}

likelihood(season.results, team.scores, pitcher.scores.final)

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

