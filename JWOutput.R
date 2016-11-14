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

# Scrape and format season results data frame
season.results <- getLeagueResults(year, MLBteams) %>% filter(!playoffs)

# Change duplicate names
season.results$starter[season.results$starter == "A.Sanchez"] <- 
  ifelse(season.results$team[season.results$starter == "A.Sanchez"] == "TOR", "Aa.Sanchez", "An.Sanchez")

season.results$starter[season.results$starter == "C.Anderson"] <- 
  ifelse(season.results$team[season.results$starter == "C.Anderson"] == "MIL", "Ch.Anderson", "Co.Anderson")

its <- 50000

# Optimize
jw.results <- runs.array.pitchers(season.results, min.starts = 10) %>% 
  jw.gradient.pitchers(iterations = its, speed = .001, startVal = 2)


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

pitcher.scores.final <- pitcher.scores %>% group_by(name) %>%
  summarize(score = weighted.mean(score, starts))

# View scores
team.scores %>% arrange(desc(score)) %>% View()
pitcher.scores.final %>% arrange(score) %>% View()

# Means
mean(team.scores$score)
mean(pitcher.scores.final$score)

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

