library(rvest)
library(data.table)
library(tidyr)
library(plyr)
library(dplyr)

# Scrape dataset

getTeamResults <- function(team, year) {
  
  print(paste0(team, "..."))
  
  # Set up URLs
  resURL <- paste0("http://www.baseball-reference.com/teams/", team, "/", year, "-schedule-scores.shtml")
  pitchURL <- paste0("http://www.baseball-reference.com/teams/tgl.cgi?team=", team,"&t=p&year=", year)
  
  # MAIN TABLE
  # Read in table from URL
  res.df <- read_html(resURL) %>%
    html_nodes(".stats_table") %>%
    html_table() %>%
    .[[1]]
  
  # Tweak some names
  names(res.df)[c(2, 4, 6, 8, 12, 19)] <- c("gameNum", "BS", "HA", "WL", "Record", "DN")
    
  # Reformat

  res.df <- res.df %>% filter(Opp != "Opp", Inn != "Game Preview, Matchups, and Tickets") %>%
    dplyr::mutate(playoffs = nchar(gameNum) > 3, home = (HA == ""), result = substr(WL, 1, 1), 
           R = as.numeric(R), RA = as.numeric(RA), gameNum = 1:n(), team = Tm, opp = Opp) %>% 
    select(team, gameNum, playoffs, home, opp, result, R, RA, Record)

  # STARTERS TABLE
  # Read in table from URL
  pitch.df <- read_html(pitchURL) %>%
    html_nodes(".stats_table") %>%
    html_table() %>%
    .[[1]]
  
  # Tweak some names
  names(pitch.df)[c(2, 4, 34)] <- c("gameNum", "HA", "pitchers")
  
  # Reformat
  pitch.df <- pitch.df %>%
    filter(Opp != "Opp")
  
  pitch.df$starter <- pitch.df$pitchers %>% 
    sapply(function(x) {strsplit(x, " \\(")[[1]][1]}) %>%
    unname()
  
  pitch.df <- pitch.df %>% select(gameNum, starter) %>%
    mutate(gameNum = as.numeric(gameNum))
  
  
  # PUT DATASETS TOGETHER
  
  out.df <- left_join(res.df, pitch.df, by = "gameNum")
  
  return(out.df)
  
}


lineToStarter <- function(txt) {
  start.ind <- regexpr("PITCHERS:", txt)
  end.ind <- regexpr("WP", txt)
  
  pstring <- substr(txt, start.ind + 10, end.ind - 1)
  
  away <- trimws(strsplit(pstring, "\n")[[1]][1])
  away.first <- strsplit(away, ",")[[1]][1]
  away.pitcher <- substr(away.first, 7, nchar(away.first))
  away.team <- substr(away.first, 1, 3)
  
  home <- trimws(strsplit(pstring, "\n")[[1]][2])
  home.first <- strsplit(home, ",")[[1]][1]
  home.pitcher <- substr(home.first, 7, nchar(home.first))
  home.team <- substr(home.first, 1, 3)
  
  out.vec <- c(away.team, away.pitcher, home.team, home.pitcher)
  return(out.vec)
}

getLeagueResults <- function(year, teamFile, includePlayoffs = F) {
  teams.df <- read.csv(teamFile) %>% filter(Year == year)
  teams.vec <- as.character(teams.df$Team)
  
  results.list <- lapply(teams.vec, getTeamResults, year = year)
  
  results.df <- rbindlist(results.list)
  
  if (!includePlayoffs) {
    results.df <- filter(results.df, !playoffs)
  } else {
    print("Playoffs...")
    
    # Scrape playoff line scores to get starting pitchers
    series <- c("ALDS1", "ALDS2", "NLDS1", "NLDS2", "ALCS", "NLCS", "WS")
    if (year > 2011) {
      series <- c("ALWC", "NLWC", series)
    }
    series.urls <- paste0("http://www.baseball-reference.com/postseason/", year, "_", series, ".shtml")
    
    gameLines <- sapply(series.urls, function(x) {read_html(x) %>% html_nodes(xpath = "//pre") %>% html_text()}) %>% unlist()
    names(gameLines) <- NULL
    
    # Turn game lines into starting pitchers, format data
    starters.pre <- t(sapply(gameLines, lineToStarter, USE.NAMES = F))
    starters <- matrix("", ncol = 2, nrow = 2*nrow(starters.pre))
    starters[seq(1, (2*nrow(starters.pre) - 1), by = 2),] <- starters.pre[,1:2]
    starters[seq(2, (2*nrow(starters.pre)), by = 2),] <- starters.pre[,3:4] 
    starters <- as.data.frame(starters) %>% rename(team = V1, starter = V2)
    
    # Match pitchers
    pteams <- as.character(unique(starters$team))
    #reg.pitchers <- lapply(pteams, function(x){unique(filter(results.df, team == x)$starter)})
    
    reg.df <- select(results.df, team, starter) %>% filter(team %in% pteams, !is.na(starter)) %>% unique()
    
    test <- mapply(function(x, y){sum(grepl(x, filter(reg.df, team == y)$starter))}, x = starters$starter, y = starters$team)
    if (mean(test) != 1) {
      print("WARNING: There may have been a double pitcher name match")
    }
    
    starters$full.starter <- 
      mapply(function(x, y){grep(x, filter(reg.df, team == y)$starter, value = T)}, x = starters$starter, y = starters$team)
    
    for (i in pteams) {
      results.df[results.df$playoffs & results.df$team == i,]$starter <- starters[starters$team == i,]$full.starter
    }

  }
  
  return(results.df)
}












