# Team-Rating-System
#### Poisson maximum likelihood model for MLB ratings and prediction

## Explanation
This project implements and tests a model for rating MLB teams and pitchers in a way that facilitates prediction. Briefly, each team and pitcher is assigned a score. I model the runs a team expects to score in a game against a given starting pitcher with a Poisson distribution with mean equal to the product of the team and pitcher scores. Scores are fit by finding the set of scores which maximize the likelihood on a training dataset. This is done using gradient descent with backtracking line search.


## File details

This project contains core files relevant to the pulling of data and creation of scores, as well as auxiliary scripts along the way for analysis.

#### CORE FILES:

File | Description |
---- | ----------- |
MLBteams.csv | Reference file of every team for every year from 1998 to 2016, along with their abbreviation and division.
getSeasonResults.R | Provides functions to scrape MLB game data from [Baseball Reference](www.baseball-reference.com). Will output a dataset containing every game for a given season (regular and postseason) with the teams, starters, home team, and runs scored from each team (among other things).
CreateFullDataset.R | Uses getSeasonResults.R to pull all game data from 1998 to 2016 and save into one dataset.
gameLogs_1998_2016.csv | Outputted data from CreateFullDataset.R
JWPitchers.R | Provides functions for taking a data frame with season results and scoring each pitcher and team.
CreateScoresDataset.R | Uses JWPitchers.R to score every player and team from 1998 to 2016.
pitcherScores_1998_2016 and teamScores_1998_2016 | Outputted data from CreateScoresDataset.R.
PredictPlayoffs.R | Uses scores to predict the results of every playoff game from 1998 to 2016, and evaluates these predictions against actual results.

#### AUXILIARY FILES:

File | Description
---- | -----------
BTGradient.R | Standalone file used to implement and test out Bradley-Terry model on MLB team data.
CompareMinVals.R | Compares different theshold values for the minimum number of games a starter must appear in to get their own score.
JWGradient.R | Standalone file. Implements and test an earlier version of Poisson MLE model which assigned defensive scores to teams rather than pitchers.
JWOutput.R | Tests use of JWPitchers.R and examines the output.
TestBacktracking.R | Used to test implementation of backtracking line search in gradient boosting algorithm.
WSPredictions | Uses team scores to predict the winner of the 2016 World Series.
homeRoadSplits | Uses pulled season data to estimate home-field advantage in the MLB.
playWithSeasonResults | Examines pulled season data.
