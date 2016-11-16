library(rvest)
library(data.table)
library(tidyr)
library(plyr)
library(dplyr)

# Scrape dataset

getTeamResults <- function(team, year) {
  print(paste0(year, " ", team, "..."))
  
  # Set up URLs
  resURL <- paste0("http://www.baseball-reference.com/teams/", team, "/", year, "-schedule-scores.shtml")
  pitchURL <- paste0("http://www.baseball-reference.com/teams/tgl.cgi?team=", team,"&t=p&year=", year)
  rosterURL <- paste0("http://www.baseball-reference.com/teams/", team, "/", year, ".shtml")
  
  
  #####################
  # MAIN SCORES TABLE #
  #####################
  
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
  
  
  ##################
  # STARTERS TABLE #
  ##################
  
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
  
  
  #########################
  # PUT DATASETS TOGETHER #
  #########################
  
  out.df <- left_join(res.df, pitch.df, by = "gameNum")
  
  
  ################
  # ROSTER TABLE #
  ################
  
  # (To get full names, bbref IDs)
  
  # Read in, adjust roster
  rosterHTML <- read_html(rosterURL)
  
  roster.df <- rosterHTML %>%
    html_nodes(xpath = '//*[@id="team_pitching"]') %>%
    html_table() %>%
    .[[1]]
  
  suppressWarnings(roster.df <- roster.df[!is.na(as.numeric(roster.df$Rk)),])
  
  roster.df$Name <- gsub("\\*", "", roster.df$Name)
  
  # Get player IDs from hrefs in link tags
  roster.df$ID <- rosterHTML %>%
    html_nodes(xpath = '//*[@id="team_pitching"]//a') %>%
    html_attr("href") %>%
    substr(12, 20) %>%
    strsplit("\\.") %>%
    sapply(function(x){x[1]})
  
  # Filter only starters, create "starter" category to mimic main table
  roster.df <- roster.df %>% filter(GS > 0) %>% select(Name, ID, GS)
  roster.df$starter <- paste0(substr(roster.df$Name, 1, 1), ".",sapply(strsplit(roster.df$Name, " "), function(x){x[length(x)]})) 
  
  # If there are pitchers with the same first initial and last name, STAND BACK
  if (nrow(roster.df) > length(unique(roster.df$starter))) {
    # Ok so basically... 
    # 1. Ignore all but one pitcher from each set of multiples
    # 2. Go through the matching process as if the other multiples don't exist
    # 3. Look through the game logs of the removed pitchers, find out what games they pitched.
    # 4. Replace data frame entries where removed pitchers pitched with correct data
    
    # Isolate offending names
    dupes <- names(table(roster.df$starter)[table(roster.df$starter) > 1])
    
    # Isolate non-offending names
    singles <- names(table(roster.df$starter)[table(roster.df$starter) == 1])
    
    # Start a vector with the non-offending names
    keep.names <- roster.df$Name[roster.df$starter %in% singles]
    
    # This list will contain all the information about what to replace
    replacements <- list()
    rep.ind <- 1
    
    # Loop through offending names
    for (dname in dupes) {
      print(paste0("Multiple pitchers with the same abbreviation (", dname, "). Resolving..."))
      
      # Data frame with the multiples
      just.dupes <- filter(roster.df, starter == dname) %>%
        mutate(most.common = GS == max(GS))
      
      # Multiple with most starts gets added to "acceptable names" list
      keep.names <- c(keep.names, just.dupes$Name[just.dupes$most.common])
      
      # Other multiples go here
      rarer <- filter(just.dupes, !most.common)
      
      # Loop through other multiples
      for (i in 1:nrow(rarer)) {
        
        # Save all forms of their name
        rID <- rarer$ID[i]
        rName <- rarer$Name[i]
        rStarter <- rarer$starter[i]
        
        # Game log URL
        rURL <- paste0("http://www.baseball-reference.com/players/gl.cgi?id=", rID, "&t=p&year=", year)
        
        # Get, format game log data
        pitcher.df <- read_html(rURL) %>%
          html_nodes(xpath = '//*[@id="pitching_gamelogs"]') %>%
          html_table(fill = T) %>%
          .[[1]]
        
        suppressWarnings(pitcher.df <- pitcher.df[!is.na(as.numeric(pitcher.df$Rk)),-c(6, 49, 50)] %>%
                           filter(Tm == team, grepl("GS", Inngs)))
        
        # Put replacement data, replacement indices for given pitcher in list within "replacements"
        replacements[[rep.ind]] <- list(starts = as.numeric(pitcher.df$Gtm), ID = rID, Name = rName, starter = rStarter)
        rep.ind <- rep.ind + 1
      }
    }
    
    # Merge season data frame with full name data
    out.df <- out.df %>% left_join(filter(roster.df, Name %in% keep.names), "starter") %>%
      select(-GS)
    
    # Loop through and replace the rows with wrong pitchers
    for (i in 1:length(replacements)) {
      out.df$starter[replacements[[i]]$starts] <- replacements[[i]]$starter
      out.df$Name[replacements[[i]]$starts] <- replacements[[i]]$Name
      out.df$ID[replacements[[i]]$starts] <- replacements[[i]]$ID
    }
    
  } else {
    out.df <- out.df %>% left_join(roster.df, "starter") %>% select(-GS)
  }
  
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
  
  # Find out which teams you need to look for
  teams.df <- read.csv(teamFile) %>% filter(Year == year)
  teams.vec <- as.character(teams.df$Team)
  
  # Get regular season results for each team, stack into one data frame
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
    
    # Go through URLs, get just text of line score
    gameLines <- sapply(series.urls, function(x) {read_html(x) %>% html_nodes(xpath = "//pre") %>% html_text()}) %>% unlist()
    names(gameLines) <- NULL
    
    # Function used to count playoff series
    cumunique <- function(var)
      sapply(seq_along(var), function(x) length(unique(head(var, x))))
    
    playoffs <- rbindlist(lapply(gameLines, parseLine)) %>% as.data.frame()
    
    # Determine wild card teams (necessary for determining series)
    if (year > 2011) {
      wc.teams <- (table(playoffs$team, playoffs$opp) == 1) %>%
        apply(1, any) %>% which() %>% names()
    } else {
      wc.teams <- c()
    }
    
    # Series reference table (necessary for determining series)
    series.df <- data.frame(seriesNum = 0:3, series = c("WC", "DS", "CS", "WS"))
    
    # Manipulate playoffs
    playoffs <- playoffs %>% arrange(team) %>% group_by(team) %>% 
      mutate(WC = team %in% wc.teams, seriesNum = cumunique(opp) - WC) %>% 
      ungroup() %>% left_join(series.df, by = "seriesNum") %>%
      group_by(team, series) %>%
      mutate(gameNumPre = 1:n(), gameNum = paste0(series, gameNumPre)) %>% ungroup() %>%
      select(-seriesNum, -gameNumPre, -series, -WC)
    
    
    # Match pitchers with full names
    pteams <- as.character(unique(playoffs$team))

    # All pitchers who started a regular season game for a playoff team
    reg.df <- select(results.df, team, starter, Name, ID) %>% filter(team %in% pteams, !is.na(starter)) %>% unique()
    
    # Number of times a pitcher with given last name matches to list of team's starters
    test <- mapply(function(x, y){sum(grepl(x, filter(reg.df, team == y)$starter))}, x = playoffs$starter, y = playoffs$team)
    
    # If there are playoff starters for the same team with the same last name, STAND BACK
    if (mean(test) != 1) {
      # Get offending player, team
      dupes <- as.character(unique(playoffs$starter[test > 1]))
      teams <- as.character(unique(playoffs$team[test > 1]))
      
      # Manipulate reg.df
      reg.df$last.name <- strsplit(reg.df$Name, " ") %>% sapply(function(x) {x[length(x)]})
      
      reg.df <- reg.df %>% group_by(last.name, team) %>% mutate(first = 1:n() == 1) %>% ungroup()
      
      # A: No more than one player with each last name per team. B: All other pitchers
      reg.df.a <- filter(reg.df, first)
      reg.df.b <- filter(reg.df, !first)
      
      # Will hold replacement information
      replacements <- list()
      rep.ind <- 1
      
      # Loop through all repeated last names
      for (i in 1:length(dupes)) {
        print(paste0("Double pitcher name match (", dupes[i], " - ", teams[i], "). Resolving..."))
        
        # Extras with this last name for given team
        rarer <- reg.df.b %>% filter(team == teams[i], last.name == dupes[i])
        
        # Loop through extras
        for (i in 1:nrow(rarer)) {
          
          # Get name information
          rID <- rarer$ID[i]
          rName <- rarer$Name[i]
          rStarter <- rarer$starter[i]
          
          # Pull playoff game log
          pURL <- paste0("http://www.baseball-reference.com/players/gl.cgi?id=", rID, "&t=p&post=1")
          pHTML <- read_html(pURL)
          
          # Check if they've pitched in the postseason. If not, move on.
          if (grepl("No Data for this season", pHTML)) {
            break
          }
          
          # Get, manipulate playoff game log
          pitcher.df <- pHTML %>%
            html_nodes(xpath = '//*[@id="pitching_gamelogs_post"]') %>%
            html_table(fill = T) %>%
            .[[1]]
          
          suppressWarnings(pitcher.df <- pitcher.df[!is.na(as.numeric(pitcher.df$Rk)),-c(6, 49, 50)] %>%
                             filter(Tm == teams[i], grepl("GS", Inngs), Year == year))
          
          pitcher.df$gameNumber <- substr(pitcher.df$Series, nchar(pitcher.df$Series), nchar(pitcher.df$Series))
          pitcher.df$seriesName <- strsplit(pitcher.df$Series, " ") %>% sapply(function(x){x[1]}) %>% substr(nchar(.) - 1, nchar(.))
          pitcher.df$gameNum <- paste0(pitcher.df$seriesName, pitcher.df$gameNumber)
          
          change.inds <- which(playoffs$team == teams[i] & playoffs$gameNum %in% pitcher.df$gameNum)
          
          replacements[[rep.ind]] <- list(starts = change.inds, ID = rID, Name = rName, starter = rStarter)
          rep.ind <- rep.ind + 1
          
        }
        
        
      }
      
      # Create "starter" column to mimic reg.df
      playoffs$starter <- 
        mapply(function(x, y){grep(x, filter(reg.df.a, team == y)$starter, value = T)}, x = playoffs$starter, y = playoffs$team)
      
      # Join playoffs with full name information
      suppressWarnings(playoffs <- left_join(playoffs, reg.df.a, by = c("starter", "team")) %>%
                         select(-first))
      
      # Replace name info as needed
      if (length(replacements) > 0) {
        for (i in 1:length(replacements)) {
          playoffs$starter[replacements[[i]]$starts] <- replacements[[i]]$starter
          playoffs$Name[replacements[[i]]$starts] <- replacements[[i]]$Name
          playoffs$ID[replacements[[i]]$starts] <- replacements[[i]]$ID
        }
      }
      
    } else {
      playoffs$starter <- 
        mapply(function(x, y){grep(x, filter(reg.df.a, team == y)$starter, value = T)}, x = playoffs$starter, y = playoffs$team)
      
      suppressWarnings(playoffs <- left_join(playoffs, reg.df.b, by = c("starter", "team")))
    }
    
    playoffs$opp <- as.character(playoffs$opp)
    playoffs <- select(playoffs, -last.name)
    
    results.df <- rbind(results.df, playoffs)
    
  }
  
  return(results.df)
}
