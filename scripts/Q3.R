#################################################
# COMP1013 Assignment
# Question 3
# Name: Vu Ngoc Thien Kim
# Student ID: 22245448
# Purpose:
# This script identifies the top five home team and away teams that most frequently participated
# in FIFA World Cup matches decided by penalty shootouts.
#################################################

###############################################################
# PART A - Load required packages
###############################################################

library(tidyverse)

###############################################################
# PART B - Import and prepare the datasets
###############################################################

# Read the Matches dataset.

matches <-
  read.csv(
    "data/Matches.csv",
    stringsAsFactors = FALSE
  )

# Read the Teams dataset.

teams <- 
  read.csv(
    "data/Teams.csv",
    stringsAsFactors = FALSE
  )


###############################################################
# PART C - Filter penalty shootout matches
###############################################################

# Only keep matches decided by a penalty shootout.

penalty_matches <-
  matches %>%
  filter(
    PenaltyShootout == 1
  )

###############################################################
# Check the filtered dataset
###############################################################

# Display the first six rows.

nrow(penalty_matches)


###############################################################
# PART D - Top 5 Home Teams
###############################################################

# Merge penalty matches with the Teams dataset to obtain the home team name and code.

home_matches <-
  penalty_matches %>%
  left_join(
    teams,
    by = c("HomeTeamID" = "TeamID")
  )

###############################################################
# Check merged dataset
###############################################################

# Display the first six rows.

head(home_matches)


###############################################################
# PART E - Identify the top five home teams
###############################################################

###############################################################
# Count the number of matches for each home team and sort the results in order.
###############################################################

home_summary <-
  home_matches %>%
  group_by(
    TeamName,
    TeamCode
  ) %>%
  summarise(
    NumberOfMatches = n()
  ) %>%
  arrange(
    desc(NumberOfMatches)
  )

###############################################################
# Select the top five home teams
###############################################################

top5_home <-
  head(
    home_summary,
    5
  )

###############################################################
# Display the top five home teams
###############################################################

top5_home


###############################################################
# PART F - Identify the top five away teams
###############################################################

###############################################################
# Merge penalty matches with the Teams dataset
###############################################################

# Merge penalty matches with the Teams dataset to obtain the away team name and code.

away_matches <-
  penalty_matches %>%
  left_join(
    teams,
    by = c("AwayTeamID" = "TeamID")
  )

###############################################################
# Check merged dataset
###############################################################

# Display the first six rows.

head(away_matches)


###############################################################
# Count the number of matches for each away team
###############################################################

# Count the number of matches for each away team and sort the results in order.

away_summary <-
  away_matches %>%
  group_by(
    TeamName,
    TeamCode
  ) %>%
  summarise(
    NumberOfMatches = n()
  ) %>%
  arrange(
    desc(NumberOfMatches)
  )

###############################################################
# Select the top five away teams
###############################################################

top5_away <-
  head(
    away_summary,
    5
  )

###############################################################
# Display the top five away teams
###############################################################

top5_away



###############################################################
# PART G - Validate the results
###############################################################

# Check the structure of the home team summary.

str(home_summary)

# Check the structure of the away team summary.

str(away_summary)

# Display the top five home teams.

top5_home

# Display the top five away teams.

top5_away
