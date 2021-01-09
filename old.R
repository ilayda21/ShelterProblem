library(CORElearn)
library(tibble)
library(tidyr)
library(knitr)
library(dplyr)
library(arules)
library(arulesViz)
library(cluster)
library(fpc)
library(DMwR2)
library(rpart)
library(rpart.plot)
library(mlbench)
library(e1071)

# read data
data <- read.csv("C:/Users/user/Desktop/dataminingproject/ShelterProblem/data/train.csv")

dataFrame <- data.frame(data)
dim(data)

# empty string values are converted to NA 
dataFrame[dataFrame==""]<-NA

# Data analyze 

cat("Dimension of data: ", dim(dataFrame))




















# tibbledData = as_tibble(dataFrame)

# tibbledData
# 
# most_common_element <- function (vec) {
#   return (names(which.max(table(vec))))
# }
# 
# 
# most_common_name <- most_common_element(tibbledData["Name"])
# most_common_issue_time <- most_common_element(tibbledData["DateTime"])
# most_common_outcome_type <- most_common_element(tibbledData["OutcomeType"])
# most_common_outcome_subtype <- most_common_element(tibbledData["OutcomeSubtype"])
# most_common_animal_type <- most_common_element(tibbledData["AnimalType"])
# most_common_sex_upon_outcome <- most_common_element(tibbledData["SexuponOutcome"])
# most_common_age_upon_outcome <- most_common_element(tibbledData["AgeuponOutcome"])
# most_common_breed <- most_common_element(tibbledData["Breed"])
# most_common_color <- most_common_element(tibbledData["Color"])
# 
# cat("Most common Name: ", most_common_name)
# cat("Most common issue Time: ", most_common_issue_time)
# cat("Most common Outcome Type: ", most_common_outcome_type)
# cat("Most common Outcome Subtype: ", most_common_outcome_subtype)
# cat("Most common Animal Type: ", most_common_animal_type)
# cat("Most common Sex: ", most_common_sex_upon_outcome)
# cat("Most common Age: ", most_common_age_upon_outcome)
# cat("Most common Breed: ", most_common_breed)
# cat("Most common Color: ", most_common_color)
# 

#since it is a unique data it is deleted
dataFrame$AnimalID <- NULL

# Supervised methods for data
# names(dataFrame)
# attrEval(Name ~ . , dataFrame, estimator = "GainRatio")
# attrEval(Breed ~ . , dataFrame, estimator = "GainRatio")
# attrEval(OutcomeType ~ . , dataFrame, estimator = "GainRatio")
# attrEval(OutcomeSubtype ~ . , dataFrame, estimator = "GainRatio")
# attrEval(AnimalType ~ . , dataFrame, estimator = "GainRatio")
# attrEval(SexuponOutcome ~ . , dataFrame, estimator = "GainRatio")
# attrEval(AgeuponOutcome ~ . , dataFrame, estimator = "GainRatio")
# attrEval(Color ~ . , dataFrame, estimator = "GainRatio")



# dataFrame$OutcomeType <- factor(dataFrame$OutcomeType)
# dataFrame$OutcomeSubtype <- factor(dataFrame$OutcomeSubtype)
# dataFrame$AnimalType <- factor(dataFrame$AnimalType)
# dataFrame$SexuponOutcome <- factor(dataFrame$SexuponOutcome)
# dataFrame$Breed <- factor(dataFrame$Breed)
# dataFrame$Color <- factor(dataFrame$Color)
# summary(dataFrame)
# Create factors

# 
# samp <- sample(1:nrow(dataFrame), 120)
# tr_set <- dataFrame[samp, ]
# tst_set <- dataFrame[-samp, ]
# model <- rpartXse(OutcomeType ~ ., tr_set, se = 0.5)
# predicted <- predict(model, tst_set, type = "class")
# head(predicted)
# 
# table(tst_set$OutcomeType, predicted)
# errorRate <- sum(predicted != tst_set$OutcomeType) / nrow(tst_set)
# errorRate