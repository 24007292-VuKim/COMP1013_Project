#################################################
# COMP1013 Assignment
# Question 2
# Name: Vu Ngoc Thien Kim
# Student ID: 22245448
# Purpose:
# This script analyses the distribution of HomeTeamScore across competition stages,
# tournament names and AwayTeamScore groups.
#################################################

###############################################################
# PART A - Load required packages
###############################################################
library(tidyverse)

###############################################################
# PART B - Import and prepare the datasets
###############################################################
# Read the Matches dataset.
# This dataset contains match scores and competition stages.

matches <- 
  read.csv(
    "data/Matches.csv",
    stringsAsFactors = FALSE
  )

# Read the Tournament dataset.
# This dataset contains tournament names.

tournaments <- 
  read.csv(
    "data/Tournaments.csv",
    stringsAsFactors = FALSE
  )

###############################################################
# Merge datasets
###############################################################
# Merge Matches with Tournaments to obtain the tournament name.

matches_full <-
  matches %>%
  left_join(
    tournaments,
    by = "TournamentID"
  )

###############################################################
# Check merged dataset

# Display the first six rows.

head(matches_full)

# Display the data structure.
str(matches_full)

# Convert Stage and TournamentName into factors.
# Stage is a categorical variable.

matches_full$Stage <-
  factor(matches_full$Stage)

# TournamentName is a categorical variable.

matches_full$TournamentName <-
  factor(matches_full$TournamentName)

# Check the updated data types.

str(matches_full)


###############################################################
# PART C - Analyse HomeTeamScore by Stage and TournamentName
###############################################################

# Check Stage values
###############################################################

# Display the frequency of each competition stage.

table(matches_full$Stage)

###############################################################
# Create a histogram of HomeTeamScore by Stage
###############################################################
# A histogram is used because HomeTeamScore is a numerical variable 
# and the chart clearly shows its distribution.

ggplot(
  matches_full,
  aes(
    x = HomeTeamScore
  )
) +
  geom_histogram(
    binwidth = 1,
    colour = "black",
    fill = "steelblue"
  ) +
  facet_wrap(
    ~ Stage
  ) +
  labs(
    title = "Distribution of Home Team Scores by Competition Stage",
    x = "Home Team Score",
    y = "Number of Matches"
  ) +
  theme_minimal()


###############################################################
# Check TournamentName values
###############################################################

# Display the frequency of each tournament.

table(matches_full$TournamentName)

###############################################################
# Create a histogram of HomeTeamScore by TournamentName
###############################################################
# A histogram is used because HomeTeamScore is a numerical variable and the chart
# clearly shows its distribution for each tournament.

ggplot(
  matches_full,
  aes(
    x = HomeTeamScore
  )
) +
  geom_histogram(
    binwidth = 1,
    colour = "black",
    fill = "darkseagreen3"
  ) +
  facet_wrap(
    ~ TournamentName,
    ncol = 4
  ) +
  labs(
    title = "Distribution of Home Team Scores by Tournament",
    x = "Home Team Score",
    y = "Number of Matches"
  ) +
  theme_minimal()
  

###############################################################
# PART D - Create AwayTeamScore groups
###############################################################

# Create a new variable to store AwayTeamScore groups.

matches_full$AwayScoreGroup <- NA

# Group matches where the away team scored 0 or 1 goal.

matches_full$AwayScoreGroup[
  matches_full$AwayTeamScore <= 1
] <- "0-1 goals"

# Group matches where the away team scored 2 or 3 goals.

matches_full$AwayScoreGroup[
  matches_full$AwayTeamScore >= 2 &
    matches_full$AwayTeamScore <= 3
] <- "2-3 goals"

# Group matches where the away team scored 4 or more goals.

matches_full$AwayScoreGroup[
  matches_full$AwayTeamScore >= 4
] <- "4+ goals"

###############################################################
# Check AwayTeamScore groups
###############################################################

# Display the number of matches in each AwayTeamScore group.

table(matches_full$AwayScoreGroup)


###############################################################
# PART E - Create the visualisation
###############################################################

# Create a histogram of HomeTeamScore by AwayTeamScore groups
###############################################################

# A histogram is used because HomeTeamScore is a numerical variable and
# the chart clearly shows the distribution across AwayTeamScore groups.

ggplot(
  matches_full,
  aes(
    x = HomeTeamScore
  )
) +
  geom_histogram(
    binwidth = 1,
    colour = "black",
    fill = "lightblue"
) +
facet_wrap(
  ~ AwayScoreGroup
) +
  labs(
    title = "Distribution of Home Team Scores by Away Team Score Groups",
    x = "Home Team Score",
    y = "Number of Matches"
  ) +
  theme_minimal()

###############################################################
# PART F - Validate the results
###############################################################

# Check the structure of the dataset.

str(matches_full)

# Check summary statistics.

summary(matches_full)

# Check the AwayTeamScore groups.

table(matches_full$AwayScoreGroup)

