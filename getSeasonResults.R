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
  rosterURL <- paste0("http://www.baseball-reference.com/teams/", team, "/", year, ".shtml")
  
  
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
    select(team, gameNum, playoffs, home, opp, result, R, RA, Record) %>%
    filter(!playoffs)
  
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
  
  # Fix pitcher names
  
  rosterHTML <- read_html(rosterURL)
  
  roster.df <- rosterHTML %>%
    html_nodes(xpath = '//*[@id="team_pitching"]') %>%
    html_table() %>%
    .[[1]]
  
  suppressWarnings(roster.df <- roster.df[!is.na(as.numeric(roster.df$Rk)),])
  
  roster.df$Name <- gsub("\\*", "", roster.df$Name)
  
  roster.df$ID <- rosterHTML %>%
    html_nodes(xpath = '//*[@id="team_pitching"]//a') %>%
    html_attr("href") %>%
    substr(12, 20) %>%
    strsplit("\\.") %>%
    sapply(function(x){x[1]})
  
  roster.df <- roster.df %>% filter(GS > 0) %>% select(Name, ID)
  
  roster.df$starter <- paste0(substr(roster.df$Name, 1, 1), ".",sapply(strsplit(roster.df$Name, " "), function(x){x[length(x)]})) 
  
  if (nrow(roster.df) != length(unique(roster.df$starter))) {
    print("Warning: multiple pitchers with the same abbreviation")
  }
  
  out.df <- out.df %>% left_join(roster.df, "starter")
  
  return(out.df)
  
}

parseLine <- function(txt) {
  start.ind <- regexpr("PITCHERS:", txt)
  end.ind <- regexpr("WP", txt)
  
  pstring <- substr(txt, start.ind + 10, end.ind - 1)
  
  # Get away team, away pitcher
  away <- trimws(strsplit(pstring, "\n")[[1]][1])
  away.first <- strsplit(away, ",")[[1]][1]
  away.pitcher <- substr(away.first, 7, nchar(away.first))
  away.team <- substr(away.first, 1, 3)
  
  # Get home team, home pitcher
  home <- trimws(strsplit(pstring, "\n")[[1]][2])
  home.first <- strsplit(home, ",")[[1]][1]
  home.pitcher <- substr(home.first, 7, nchar(home.first))
  home.team <- substr(home.first, 1, 3)
  
  # Get away runs
  runs <- strsplit(txt, "\n\n")[[1]][2]
  runs.away.pre <- strsplit(runs,"\n")[[1]][3]
  runs.away.pre <- strsplit(runs.away.pre, " +")[[1]]
  away.runs <- as.numeric(runs.away.pre[length(runs.away.pre) - 2])

  # Get home runs (Ha!)
  runs.home.pre <- strsplit(runs,"\n")[[1]][4]
  runs.home.pre <- strsplit(runs.home.pre, " +")[[1]]
  home.runs <- as.numeric(runs.home.pre[length(runs.home.pre) - 2])
  
  # Get game result
  home.result <- ifelse(home.runs > away.runs, "W", "L")
  away.result <- ifelse(home.runs < away.runs, "W", "L")

  # Make return data frame
  out.df <- data.frame(team = c(away.team, home.team), gameNum = 200, playoffs = T, home = c(F, T),
                       opp = c(home.team, away.team), result = c(away.result, home.result),
                       R = c(away.runs, home.runs), RA = c(home.runs, away.runs), Record = "", 
                       starter = c(away.pitcher, home.pitcher))
  
  return(out.df)
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
    
    playoffs <- rbindlist(lapply(gameLines, parseLine)) %>% as.data.frame()
    
    # Match pitchers
    pteams <- as.character(unique(playoffs$team))
    #reg.pitchers <- lapply(pteams, function(x){unique(filter(results.df, team == x)$starter)})
    
    reg.df <- select(results.df, team, starter, Name, ID) %>% filter(team %in% pteams, !is.na(starter)) %>% unique()
    
    # Number of times a pitcher with given last name matches to list of team's starters
    test <- mapply(function(x, y){sum(grepl(x, filter(reg.df, team == y)$starter))}, x = playoffs$starter, y = playoffs$team)
    if (mean(test) != 1) {
      print("WARNING: There may have been a double pitcher name match")
    }
    
    playoffs$starter <- 
      mapply(function(x, y){grep(x, filter(reg.df, team == y)$starter, value = T)}, x = playoffs$starter, y = playoffs$team)
    
    suppressWarnings(playoffs <- left_join(playoffs, reg.df, by = c("starter", "team")))
    
    playoffs$gameNum <- 1:nrow(playoffs) + 200
    playoffs$opp <- as.character(playoffs$opp)
    
    results.df <- rbind(results.df, playoffs)

  }
  
  return(results.df)
}
