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

# empty string values are converted to NA 
dataFrame[dataFrame==""]<-NA

# Data analyze 
dataDimension <- dim(dataFrame)
cat("Dimension of data: ", dataDimension)
dataFrame = subset(dataFrame, select = -c(AnimalID))

# Type of each column
print("Column Types: ")
columnTypes <- sapply(dataFrame, class)
print(columnTypes)


# number of NA for each column
print("Number of NAs for each column")
naTable <- colSums(is.na(dataFrame))
print(naTable)

# kable(attrEval(OutcomeType ~ . , dataFrame, estimator = "Gini"))

# since there are too much NA value OutcomeType and OutcomeSubtype will be merged
for (i in 1:dataDimension[1]) {
  secondPart <- dataFrame$OutcomeSubtype[i]
  if (is.na(dataFrame$OutcomeSubtype[i])) {
     secondPart <- ""
  } else {
    secondPart <- paste0("-", secondPart)
  }
  dataFrame$OutcomeType[i] <- paste0(dataFrame$OutcomeType[i], secondPart)
}

#  remove outcomesubtype
dataFrame = subset(dataFrame, select = -c(OutcomeSubtype))

#Because it has lots of NA values
dataFrame = subset(dataFrame, select = -c(Name))

# Remove all NA values because it has very little data that is NA
dataFrame <- dataFrame[complete.cases(dataFrame), ]
dataDimension <- dim(dataFrame)

# Age field is converted to Kitten, Junior, Prime, Mature, senior, Geriatric
for (i in 1:dataDimension[1]) {
  ageParts <- strsplit(dataFrame$AgeuponOutcome[i], "\\s+")[[1]]
  lifeStage <- ""
  agerAsNum <- strtoi(ageParts[1])
  if (ageParts[2] == "year") {
    lifeStage <- "JUNIOR"
  } else if (ageParts[2] == "years") {
    if (agerAsNum < 1) {
      lifeStage <- "BABY"
    } else if (1 <= agerAsNum && agerAsNum < 3) {
      lifeStage <- "JUNIOR"
    } else if (3 <= agerAsNum && agerAsNum < 7) {
      lifeStage <- "PRIME"
    } else if (7 <= agerAsNum && agerAsNum < 11) {
      lifeStage <- "MATURE"
    } else if (11 <= agerAsNum && agerAsNum < 15) {
      lifeStage <- "SENIOR"
    } else if (15 <= agerAsNum) {
      lifeStage <- "GERIATRIC"
    } else {
      print(dataFrame$AgeuponOutcome[i])
      lifeStage <- NA
    }
  } else if (ageParts[2] == "months") {
    if (0 <= agerAsNum && agerAsNum <= 6) {
      lifeStage <- "BABY"
    } else if (6 < agerAsNum && agerAsNum <= 18) {
      lifeStage <- "JUNIOR"
    } else {
      print(dataFrame$AgeuponOutcome[i])
      lifeStage <- NA
    }
  } else if (ageParts[2] == "month" || ageParts[2] == "weeks" || ageParts[2] == "week" 
             || ageParts[2] == "days" || ageParts[2] == "day") {
    lifeStage <- "BABY"
  }
  
  dataFrame$AgeuponOutcome[i] <- lifeStage
}

# kable(attrEval(OutcomeType ~ . , dataFrame, estimator = "Gini"))

# DateTime field is converted Winter, spring, summer, autumn
for (i in 1:dataDimension[1]) {
  dateParts <- strsplit(dataFrame$DateTime[i], "\\s+")[[1]][1]
  month <- strsplit(dateParts, "-")[[1]][2]
  year <- strsplit(dateParts, "-")[[1]][1]
  day <- strsplit(dateParts, "-")[[1]][3]
  datePeriod <- ""
  if (month == "01" || month == "02" || month == "12") {
    datePeriod <- paste0("WINTER-", month, "-", day)
  } else if (month == "03" || month == "04" || month == "05") {
    datePeriod <- paste0("SPRING-", month, "-", day)
  } else if (month == "06" || month == "07" || month == "08") {
    datePeriod <- paste0("SUMMER-", month, "-", day)
  } else if (month == "09" || month == "10" || month == "11") {
    datePeriod <- paste0("AUTUMN-", month, "-", day)
  } 
  
  dataFrame$DateTime[i] <- datePeriod
}

# Convert data to  factors, then convert it to number then detect outliers
# Remove outliers (apply different methods and compare)
# Make some relation analyses apriori
# use some methods for prediction

dbscan.outliers <- function(data, ...) {
  require(fpc, quietly=TRUE)
  cl <- dbscan(data, ...)
  posOuts <- which(cl$cluster == 0)
  list(positions = posOuts,
       outliers = data[posOuts,],
       dbscanResults = cl)
}

outs <- dbscan.outliers(d, eps = 1.07)

print("Outliers are detected.")
