# ==============================================================
# Title: Data Preparation
# Description: cleaning data and merging two csv files
# Input(s): nba2017-roster.csv and nba2017-stats.csv
# Output(s): nba2017-team.rsv
# Author: Jenny Huang
# Date: 10-09-2017
# ===============================================================

#set working directory
setwd("~/Desktop/stat133/stat133-hws-fall17/hw03")

#packages
library(dplyr)
library(ggplot2)
library(readr)

#importing data
roster <- read_csv("./data/nba2017-roster.csv")
stats <- read_csv("./data/nba2017-stats.csv")
str(stats)

#Adding new variables
stats <- mutate(stats, missed_fg = stats$field_goals_atts - stats$field_goals_made)
stats <- mutate(stats, missed_ft = stats$points1_atts - stats$points1_made)
stats <- mutate(stats, points = 3*stats$points3_made + 2*stats$points2_made + stats$points1_made)
stats <- mutate(stats, rebounds = stats$off_rebounds + stats$def_rebounds)
stats <- mutate(stats, efficiency = (
  stats$points + stats$rebounds + stats$assists + stats$steals + stats$blocks
  - stats$missed_fg - stats$missed_ft -stats$turnovers) / stats$games_played)

#sending output of summary(efficiency) to a text file
sink(file = './output/efficiency-summary.txt')
summary(stats[ ,'efficiency'])
sink()

#Merging Tables
merged_tbl <- merge(roster, stats, by.x = "player", by.y = "player")

# Creating nba2017-teams.csv
teams <- merged_tbl %>%
  group_by(team) %>%
  select(c(experience,salary,points3_made,points2_made,points1_made,points,off_rebounds,
           def_rebounds,assists,steals,blocks,turnovers,fouls,efficiency)) %>%
  summarise(experience = sum(experience), salary = round(sum(salary),2),points3 = sum(points3_made),
            points2 = sum(points2_made),free_throws = sum(points1_made), points = sum(points),
            off_rebounds = sum(off_rebounds),def_rebounds = sum(def_rebounds),
            assists = sum(assists), steals = sum(steals), blocks = sum(blocks), turnovers = sum(turnovers),
            fouls = sum(fouls), efficiency = sum(efficiency))
summary(teams)

#sending output of summary(summary) to a text file
sink(file = './output/team-summary.txt')
summary(teams)
sink()

# exporting teams table to a csv file
write.csv(teams, file = "./data/nba2017-teams.csv", row.names = FALSE)

# star plot of the teams
pdf(file = "./images/teams_star_plot.pdf")
stars(teams[ ,-1], labels = teams$team)
dev.off()

# ggplot: scatterplot of experience and salary
pdf(file = "./images/experience_salary.pdf")
ggplot(teams, aes(x = experience, y = salary)) + geom_point() + 
  geom_text(aes(label = team, hjust = 1, vjust = 1 ))
dev.off()