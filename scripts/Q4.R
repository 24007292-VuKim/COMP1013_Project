#################################################
# COMP1013 Assignment
# Question 4
# Name: Vu Ngoc Thien Kim
# Student ID: 22245448
# Purpose:
# This script analyses the relationship between match results
# and selected match characteristics in the FIFA World Cup dataset.
#################################################

###############################################################
# PART A - Load required packages
###############################################################

library(tidyverse)


###############################################################
# PART B - Import and prepare the dataset
###############################################################
# Read the Matches dataset.
# This dataset contains match scores and match characteristics.

matches <-
  read.csv(
    "data/Matches.csv",
    stringsAsFactors = FALSE
  )

###############################################################
# Check the dataset
###############################################################

# Display the first six rows.

head(matches)

#Display the structure of the dataset.

str(matches)


###############################################################
# PART C - Prepare the dataset
###############################################################

# Convert Stage into a factor. Stage is a categorical variable.

matches$Stage <-
  factor(
    matches$Stage,
    levels = c(
      "group stage",
      "second group stage",
      "round of 16",
      "quarter-finals",
      "semi-finals",
      "third-place match",
      "final",
      "final round"
    )
  )

# Convert ExtraTime into a factor. ExtraTime contains two categories (0 and 1).

matches$ExtraTime <-
  factor(matches$ExtraTime)

# Convert Result into a factor. Result contains the match outcome categories.

matches$Result <-
  factor(matches$Result)

###############################################################
# Check the updated data types
###############################################################

# Display the structure of the dataset.

str(matches)


###############################################################
# PART D - Explore match results by Stage
###############################################################

# Check Stage values
###############################################################

# Display the number of matches in each competition stage.

table(matches$Stage)


###############################################################
# PART E - Create the first visualisation
###############################################################

# Create a bar chart of match results by competition stage.
# A bar chart is used because Stage and Result are categorical variables.
# The chart clearly compares the number of match results across competition stages.

ggplot(
  matches,
  aes(
    x = Stage,
    fill = Result
  )
) +
  geom_bar(
    position = "dodge"
  ) +
  labs(
    title = "Match Results by Competition Stage",
    x = "Competition Stage",
    y = "Number of Matches",
    fill = "Match Result"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
      
    )
  ) +
  scale_x_discrete(
    labels = c(
    "final" = "Final",
    "final round" = "Final Round",
    "group stage" = "Group Stage",
    "quarter-finals" = "Quarter-finals",
    "round of 16" = "Round of 16",
    "second group stage" = "Second Group Stage",
    "semi-finals" = "Semi-finals",
    "third-place match" = "Third-place Match"
  )
) +
  scale_fill_discrete(
    labels = c(
      "away team win" = "Away Team Win",
      "draw" = "Draw",
      "home team win" = "Home Team Win"
    )
  )


###############################################################
# PART F - Create the second visualisation
###############################################################

# Create a bar chart of match results by extra time.
# A bar chart is used because ExtraTime and Result are categorical variables.
# The chart compares match results between matches with and without extra time.

ggplot(
  matches,
  aes(
    x = ExtraTime,
    fill = Result
  )
) +
  geom_bar(
    position = "dodge"
  ) +
  labs (
    title = "Match Results by Extra Time",
    x = "Extra Time",
    y = "Number of Matches",
    fill = "Match Result"
  ) + 
  theme_minimal() +
  scale_x_discrete(
    labels = c(
      "0" = "No",
      "1" = "Yes"
    )
  ) +
  scale_fill_discrete(
    labels = c(
      "away team win" = "Away Team Win",
      "draw" = "Draw",
      "home team win" = "Home Team Win"
    )
  )


###############################################################
# PART F - Validate the results
###############################################################

# Display the structure of the dataset.

str(matches)

# Display the number of matches in each competition stage.

table(matches$Stage)

# Display the number of matches with and without extra time.

table(matches$ExtraTime)

# Display the number of each match result.

table(matches$Result)
