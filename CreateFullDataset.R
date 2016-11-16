##########
# Set up #
##########

#setwd("/Users/jackwerner/Documents/My Stuff/Baseball/Team Rating System")
setwd("C:/Users/jack.werner1/Documents/BB")

source("getSeasonResults.R")

MLBteams <- paste0(getwd(), "/MLBteams.csv")

####################
# Get game results #
####################

listOfSeasons <- lapply(1998:2016, getLeagueResults, teamFile = MLBteams, includePlayoffs = T)
names(listOfSeasons) <- paste0("y", 1998:2016)

###################
# List of Errors: #
###################

# PLAYOFFS:
# Multiple Martinezes for BOS 1999
# Multiple Joneses for NYM 2000
# Multiple Hernandezes for NYY 2001

# REGULAR SEASON:
# 2000 NYM: Bobby Jones (27), Bobby Jones (1)
# 2001 STL: Andy Benes (19), Alan Benes (1)
# 2002 SDP: Bobby Jones (18), Bobby Jones (2)
# 2006 LAA: Jered Weaver (19), Jeff Weaver (16)
# 2006 ARI: Enrique Gonzalez (18), Edgar Gonzalez (5)


s2006 <- listOfSeasons$y2006
s2006 %>% filter(team == "LAA") %>% View()


x <- getLeagueResults(2001, teamFile = MLBteams, includePlayoffs = T)




