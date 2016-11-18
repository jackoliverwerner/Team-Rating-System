##########
# Set up #
##########

#setwd("/Users/jackwerner/Documents/My Stuff/Baseball/Team Rating System")
setwd("C:/Users/jack.werner1/Documents/BB/Team-Rating-System")

source("JWPitchers.R")


####################
# Get game results #
####################

full.results <- read.csv("gameLogs_1998_2016.csv")

team.scores.list <- list()
pitcher.scores.list <- list()
list.ind <- 1

for (year in 1998:2016) {
  print(paste0(year, "..."))
  
  season.results <- full.results %>% filter(Year == year)
  
  reg.results <- season.results %>% filter(!playoffs)
  
  
  ##########################
  # Get model scores -- JW #
  ##########################
  
  its <- 10000
  
  # Optimize
  jw.results <- runs.array.pitchers(reg.results, min.starts = 15, pitcherCol = "ID") %>% 
    jw.gradient.pitchers(iterations = its, speed = .001, startVal = 2, print.every.n = 1000)
  
  
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
  
  pitcher.scores.final.pre <- data.frame(ID = unique(reg.results$ID))
  
  pitcher.scores.final <- pitcher.scores.final.pre %>%
    left_join(pitcher.scores.2, by = c("ID"="name")) %>%
    rename(name = ID)
  
  pitcher.scores.final[is.na(pitcher.scores.final$score),]$score <- pitcher.scores.2$score[pitcher.scores.2$name == "Other"]
  
  pitcher.ref <- reg.results %>% select(ID, Name) %>% unique()
  
  pitcher.scores.final <- left_join(pitcher.scores.final, pitcher.ref, by = c("name" = "ID")) %>%
    select(ID = name, Name, score)
  
  team.scores.list[[list.ind]] <- team.scores
  pitcher.scores.list[[list.ind]] <- pitcher.scores.final
  
  list.ind <- list.ind + 1
}

addYear <- function(x, y) {
  out.df <- x
  out.df$Year <- y
  return(out.df)
}

# Deal with pitchers
pitcher.list.final <- mapply(FUN = addYear, x = pitcher.scores.list, y = 1998:2016, SIMPLIFY = F)

pitcher.df <- rbindlist(pitcher.list.final)

write.csv(pitcher.df, file = "pitcherScores_1998_2016.csv", row.names = F)


# Deal with teams
team.list.final <- mapply(FUN = addYear, x = team.scores.list, y = 1998:2016, SIMPLIFY = F)

team.df <- rbindlist(team.list.final)

write.csv(team.df, file = "teamScores_1998_2016.csv", row.names = F)





