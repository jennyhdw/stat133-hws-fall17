# ==============================================================
# Title: Cleaning Data
# Description: Data preparation
# Input(s): rawscores.csv
# Output(s): cleaned data
# Author: Jenny Huang
# Date: 11-09-2017
# ===============================================================

#packages
library(readr)        # importing data

#source functions
source('functions.R')

# importing data
rawscores <- read.csv("../data/rawdata/rawscores.csv",
                      colClasses = c(rep("real",16)))


# sinking the structure of the data frame
sink(file = '../output/summary-rawscores.txt')
str(rawscores)
for (i in (1:ncol(rawscores))){
  print (summary_stats(rawscores[,i]))
}
for (i in (1:ncol(rawscores))){
  print (print_stats(rawscores[,i]))
}
sink()

#replacing all missing values with zero
for (i in (1:ncol(rawscores))){
  for (j in (1:nrow(rawscores))){
    if (is.na(rawscores[j,i])){
      rawscores[j,i] <- 0
    } 
  }
}

#rescaling quiz1
for (i in (1:nrow(rawscores))) {
  rawscores[i,11] <- rescale100(rawscores[i,11],0,12)
}

#rescaling quiz2
for (i in (1:nrow(rawscores))) {
  rawscores[i,12] <- rescale100(rawscores[i,12],0,18)
}

#rescaling quiz3
for (i in (1:nrow(rawscores))) {
  rawscores[i,13] <- rescale100(rawscores[i,13],0,20)
}

#rescaling quiz4
for (i in (1:nrow(rawscores))) {
  rawscores[i,14] <- rescale100(rawscores[i,14],0,20)
}

#adding test1
rawscores$Test1 <- rescale100(rawscores$EX1,0,80)

#adding test2
rawscores$Test2 <- rescale100(rawscores$EX2,0,90)

#adding Homework
rawscores$Homework <- c(0)
for (i in (1:nrow(rawscores))){
    rawscores[i,19] <- round(score_homework(as.numeric(rawscores[i,1:9]),
                                            drop = TRUE),digits = 2)
}

#adding quiz
rawscores$Quiz <- c(0)
for (i in (1:nrow(rawscores))){
  rawscores[i,20] <- round(score_quiz(as.numeric(rawscores[i,11:14]),
                                          drop = TRUE),digits = 2)
}

#adding lab score
rawscores$Lab <- c(0)
for (i in (1:nrow(rawscores))) {
  rawscores[i,21] <- score_lab(rawscores[i,10])
}

# adding overall score
rawscores$Overall <- c(0)
for (i in (1:nrow(rawscores))){
  rawscores[i,22] <- 0.1*rawscores[i,21] + 
    0.3*rawscores[i,19] + 0.15*rawscores[i,20] + 0.2*rawscores[i,17] + 
    0.25*rawscores[i,18]
}

#adding Grade
rawscores$Grade <- c("")
for (i in (1:nrow(rawscores))){
  if (rawscores[i,22]<50){rawscores[i,23] <- "F"}
  if (rawscores[i,22]>= 50 & rawscores[i,22]<60){rawscores[i,23] <- "D"}
  if (rawscores[i,22]>= 60 & rawscores[i,22]<70){rawscores[i,23] <- "C-"}
  if (rawscores[i,22]>= 70 & rawscores[i,22]<77.5){rawscores[i,23] <- "C"}
  if (rawscores[i,22]>= 77.5 & rawscores[i,22]<79.5){rawscores[i,23] <- "C+"}
  if (rawscores[i,22]>= 79.5 & rawscores[i,22]<82){rawscores[i,23] <- "B-"}
  if (rawscores[i,22]>= 82 & rawscores[i,22]<86){rawscores[i,23] <- "B"}
  if (rawscores[i,22]>= 86 & rawscores[i,22]<88){rawscores[i,23] <- "B+"}
  if (rawscores[i,22]>= 88 & rawscores[i,22]<90){rawscores[i,23] <- "A-"}
  if (rawscores[i,22]>= 90 & rawscores[i,22]<95){rawscores[i,23] <- "A"}
  if (rawscores[i,22]>= 95 & rawscores[i,22]<=100){rawscores[i,23] <- "A+"}
}
  
  
#sinking text files
for (i in (17:22)){
  filepath <- file.path("../output/",paste0(names(rawscores[i]),"-stats.txt",
                                            collapse =""))
  sink(file = filepath)
  print(summary_stats(rawscores[,i]))
  print(print_stats(rawscores[,i]))
  sink()
}

#sinking data structure
sink(file = '../output/summary-cleanscores.txt')
str(rawscores)
sink()

#sinking clean data frame
write.csv(rawscores,file = "../data/cleandata/cleanscores.csv",
            row.names = FALSE)