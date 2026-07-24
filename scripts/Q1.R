#################################################
# COMP1013 Assignment
# Question 1
# Name:Vu Ngoc Thien Kim
# Student ID: 22245448
# Purpose:
# This script performs data inspection, data cleaning and
# visualisation for the FIFA World Cup dataset.

###############################################################
# PART A - Load required packages
###############################################################

# Load the tidyverse package which contains useful packages
# such as dplyr and ggplot2 for data manipulation and visualisation.
###############################################################

library(tidyverse)

###############################################################
# PART B - Import and Inspect the datasets
###############################################################

# Read the Matches dataset.
# This dataset contains information about every FIFA World Cup
# match.

matches <- 
  read.csv(
    "data/Matches.csv",
           stringsAsFactors = FALSE
           )

# Read the Teams dataset.
# This dataset stores the country name and FIFA team code.

teams <-
  read.csv(
    "data/Teams.csv",
    stringsAsFactors = FALSE
  )
  
# Read the Stadiums dataset.
# This dataset stores stadium locations.

stadiums <-
  read.csv(
    "data/Stadiums.csv",
    stringsAsFactors = FALSE 
  )

# Read the Tournament dataset.
# This dataset contains tournament editions.

tournaments <- 
  read.csv(
    "data/Tournaments.csv",
    stringsAsFactors = FALSE
  )

###############################################################
# Inspect the datasets
###############################################################

# Display the first six rows to understand the structure.

head(matches)

head(teams)

head(stadiums)

head(tournaments)

# Display the structure of each dataset.

str(matches)

str(teams)

str(stadiums)

str(tournaments)

# Display summary statistics and detect possible missing values.

summary(matches)

summary(teams)

summary(stadiums)

summary(tournaments)

###############################################################
# PART C - Clean the data
###############################################################


###############################################################
# Step 1: Check missing values represented by "?"
###############################################################

# Count the number of "?" values in each dataset.

# Count the number of "?" values in the Matches dataset.
sum(matches == "?", na.rm = TRUE)

# Count the number of "?" values in the Stadiums dataset.
sum(stadiums == "?", na.rm = TRUE)

###############################################################
# Step 2: Replace "?" with NA
###############################################################

# Replace all occurrences of "?" with NA.
# NA is the standard representation of missing values in R.
# Replace all "?" values with NA so that R recognises
# them as missing values.

matches[matches == "?"] <- NA

stadiums[stadiums == "?"] <- NA

# Verify that all "?" values have been removed.

sum(matches == "?", na.rm = TRUE)

sum(stadiums == "?", na.rm = TRUE)

###############################################################
# Step 3: Convert categorical variables into factors
###############################################################

# Result contains categorical values.

matches$Result <-
  factor(matches$Result)

# Stage contains competition stages.

matches$Stage <-
  factor(matches$Stage)

# ExtraTime contains 2 categorical values (0 and 1).

matches$ExtraTime <-
  factor(matches$ExtraTime)

# Country is a categorical variable.

stadiums$Country <-
  factor(stadiums$Country)

# Check the updated data types.

str(matches)
str(stadiums)

###############################################################
# Step 4: Convert penalty variables into numeric
###############################################################

# Convert HomePenalty into numeric.

matches$HomePenalty <-
  as.numeric(matches$HomePenalty)

# Convert AwayPenalty into numeric.

matches$AwayPenalty <-
  as.numeric(matches$AwayPenalty)

###############################################################
# Step 5: Replace missing penalty values with zero
###############################################################

# Missing values indicate no penalty goals.

matches$HomePenalty[
  is.na(matches$HomePenalty)
] <- 0

matches$AwayPenalty[
  is.na(matches$AwayPenalty)
] <- 0

###############################################################
# Step 6: Check the cleaned data
###############################################################
str(matches)

summary(matches$HomePenalty)

summary(matches$AwayPenalty)

sum(is.na(matches$HomePenalty))

sum(is.na(matches$AwayPenalty))


###############################################################
# PART D - Prepare the data for visualisation
###############################################################

###############################################################
# Check values of PenaltyShootout
###############################################################

# Display the number of matches with and without
# a penalty shootout.

table(matches$PenaltyShootout)

# The value 1 indicates that the match
# was decided by a penalty shootout.

###############################################################
# Filter matches decided by penalty shootout
###############################################################

# Filter the dataset to include only matches
# decided by a penalty shootout.
# Store the filtered data in a new object
# for plotting.
penalty_matches <-
  matches %>%
  filter(PenaltyShootout == 1)


###############################################################
# PART E - Create the visualisation
###############################################################

# A histogram is used because HomePenalty is a
# numerical variable and the chart clearly shows
# the distribution of penalty goals.

ggplot(
  penalty_matches,
  aes(
    x = HomePenalty
  )
) +
  geom_histogram(
    binwidth = 1,
    colour = "black",
    fill = "steelblue"
  ) +
  labs(
    title = "Distribution of Home Penalty Goals",
    x = "Home Penalty Goals",
    y = "Number of Matches"
  ) +
  theme_minimal()


###############################################################
# PART F - Validate the results
###############################################################

# Check data types

str(matches)

# Check summary statistics

summary(matches)

#Check penalty values

summary(matches$HomePenalty)

summary(matches$AwayPenalty)

# Check filtered data

head(penalty_matches)

getwd()
