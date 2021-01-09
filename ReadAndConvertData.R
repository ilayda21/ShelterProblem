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
library(adabag)
library(randomForest)
library(stringr)

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
# for (i in 1:dataDimension[1]) {
#   secondPart <- dataFrame$OutcomeSubtype[i]
#   if (is.na(dataFrame$OutcomeSubtype[i])) {
#      secondPart <- ""
#   } else {
#     secondPart <- paste0("-", secondPart)
#   }
#   dataFrame$OutcomeType[i] <- paste0(dataFrame$OutcomeType[i], secondPart)
# }

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
    datePeriod <- paste0("WINTER")
  } else if (month == "03" || month == "04" || month == "05") {
    datePeriod <- paste0("SPRING")
  } else if (month == "06" || month == "07" || month == "08") {
    datePeriod <- paste0("SUMMER")
  } else if (month == "09" || month == "10" || month == "11") {
    datePeriod <- paste0("AUTUMN")
  } 
  
  dataFrame$DateTime[i] <- datePeriod
}

df <- data.frame(DateTime=character(), OutcomeType=character(), AnimalType=character(), SexuponOutcome=character(), 
                 AgeuponOutcome=character(), Breed=character(), Color=character())

trim <- function (x) gsub("^\\s+|\\s+$", "", x)

new_generated_breed <- function(old_breed) {
  result_breed <- old_breed
  if (str_detect(old_breed, "Terrier") || str_detect(old_breed, "Terr") || str_detect(old_breed, "Maltese")
      || str_detect(old_breed, "Yorkshire") || str_detect(old_breed, "Staffordshire")
      || str_detect(old_breed, "Glen Of Imaal")) {
    
    result_breed <- "Terrier"
  } else if (str_detect(old_breed, "Dachshund")) {
    result_breed <- "Dachshund"
  } else if (str_detect(old_breed, "Chihuahua")) {
    result_breed <- "Chihuahua"
  } else if (str_detect(old_breed, "Corgi")) {
    result_breed <- "Corgi"
  } else if (str_detect(old_breed, "Greyhound")) {
    result_breed <- "Greyhound"
  } else if (str_detect(old_breed, "Rex")) {
    result_breed <- "Rex"
  } else if (str_detect(old_breed, "Retriever") || str_detect(old_breed, "Retr")) {
    result_breed <- "Retriever"
  } else if (str_detect(old_breed, "St. Bernard")) {
    result_breed <- "St. Bernard"
  } else if (str_detect(old_breed, "Domestic")) {
    result_breed <- "Domestic"
  } else if (str_detect(old_breed, "Bulldog")) {
    result_breed <- "Bulldog"
  } else if (str_detect(old_breed, "Husky")) {
    result_breed <- "Husky"
  } else if (str_detect(old_breed, "Poodle")) {
    result_breed <- "Poodle"
  } else if (str_detect(old_breed, "Spaniel") || str_detect(old_breed, "Span")) {
    result_breed <- "Spaniel"
  } else if (str_detect(old_breed, "Hound")) {
    result_breed <- "Hound"
  } else if (str_detect(old_breed, "Pointer")) {
    result_breed <- "Pointer"
  } else if (str_detect(old_breed, "Foxhound")) {
    result_breed <- "Foxhound"
  } else if (str_detect(old_breed, "Pinscher")) {
    result_breed <- "Pinscher"
  } else if (str_detect(old_breed, "Foxhound")) {
    result_breed <- "Foxhound"
  } else if (str_detect(old_breed, "Sheepdog")) {
    result_breed <- "Sheepdog"
  } else if (str_detect(old_breed, "Coonhound")) {
    result_breed <- "Coonhound"
  } else if (str_detect(old_breed, "Mastiff")) {
    result_breed <- "Mastiff"
  } else if (str_detect(old_breed, "Cur")) {
    result_breed <- "Cur"
  } else if (str_detect(old_breed, "Schnauzer")) {
    result_breed <- "Schnauzer"
  } else if (str_detect(old_breed, "Shepherd")) {
    result_breed <- "Shepherd"
  } else if (str_detect(old_breed, "Griffon")) {
    result_breed <- "Griffon"
  } else if (str_detect(old_breed, "Setter")) {
    result_breed <- "Setter"
  } else if (str_detect(old_breed, "Water Dog")) {
    result_breed <- "Water Dog"
  } else if (str_detect(old_breed, "Shiba Inu") || str_detect(old_breed, "Akita")) {
    result_breed <- "Inu"
  } else if (str_detect(old_breed, "Collie")) {
    result_breed <- "Collie"
  } else if (str_detect(old_breed, "Mountain Dog")) {
    result_breed <- "Mountain Dog"
  } else if (str_detect(old_breed, "Wolfhound")) {
    result_breed <- "Wolfhound"
  } else if (str_detect(old_breed, "Elkhound")) {
    result_breed <- "Elkhound"
  } else if (str_detect(old_breed, "Mix")) {
    result_breed <- trim(str_remove(old_breed, "Mix"))
  } 
  
  
  return (result_breed)
}

ind <- 1
for(i in 1:dataDimension[1]) {
  breeds <- strsplit(dataFrame$Breed[i], "/")[[1]]
  if (length(breeds) == 1) {
    new_breed <- new_generated_breed(dataFrame$Breed[i])
    
    df[ind,] <- c(dataFrame$DateTime[i], dataFrame$OutcomeType[i], dataFrame$AnimalType[i],
                  dataFrame$SexuponOutcome[i], dataFrame$AgeuponOutcome[i],new_breed , dataFrame$Color[i])
    ind <- ind + 1
  } else if (length(breeds) == 2) {
    new_breed_1 <- new_generated_breed(breeds[1])
    new_breed_2 <- new_generated_breed(breeds[2])
    df[ind,] <- c(dataFrame$DateTime[i], dataFrame$OutcomeType[i], dataFrame$AnimalType[i],
                  dataFrame$SexuponOutcome[i], dataFrame$AgeuponOutcome[i], new_breed_1, dataFrame$Color[i])
    ind <- ind + 1
    
    if (new_breed_1 != new_breed_2) {
      df[ind,] <- c(dataFrame$DateTime[i], dataFrame$OutcomeType[i], dataFrame$AnimalType[i],
                    dataFrame$SexuponOutcome[i], dataFrame$AgeuponOutcome[i], new_breed_2, dataFrame$Color[i])
      ind <- ind + 1  
    }
  } else if (length(breeds) == 3) {
    new_breed_1 <- new_generated_breed(breeds[1])
    new_breed_2 <- new_generated_breed(breeds[2])
    new_breed_3 <- new_generated_breed(breeds[3])
    
    if (new_breed_1 == new_breed_2 && new_breed_2 == new_breed_3) {
      df[ind,] <- c(dataFrame$DateTime[i], dataFrame$OutcomeType[i], dataFrame$AnimalType[i],
                    dataFrame$SexuponOutcome[i], dataFrame$AgeuponOutcome[i], new_breed_1, dataFrame$Color[i])
      ind <- ind + 1  
    } else if (new_breed_1 == new_breed_2) {
      df[ind,] <- c(dataFrame$DateTime[i], dataFrame$OutcomeType[i], dataFrame$AnimalType[i],
                    dataFrame$SexuponOutcome[i], dataFrame$AgeuponOutcome[i], new_breed_1, dataFrame$Color[i])
      ind <- ind + 1
      df[ind,] <- c(dataFrame$DateTime[i], dataFrame$OutcomeType[i], dataFrame$AnimalType[i],
                    dataFrame$SexuponOutcome[i], dataFrame$AgeuponOutcome[i], new_breed_3, dataFrame$Color[i])
      ind <- ind + 1
    } else if (new_breed_2 == new_breed_3) {
      df[ind,] <- c(dataFrame$DateTime[i], dataFrame$OutcomeType[i], dataFrame$AnimalType[i],
                    dataFrame$SexuponOutcome[i], dataFrame$AgeuponOutcome[i], new_breed_1, dataFrame$Color[i])
      ind <- ind + 1 
      df[ind,] <- c(dataFrame$DateTime[i], dataFrame$OutcomeType[i], dataFrame$AnimalType[i],
                    dataFrame$SexuponOutcome[i], dataFrame$AgeuponOutcome[i], new_breed_2, dataFrame$Color[i])
      ind <- ind + 1
    } else if (new_breed_1 == new_breed_3) {
      df[ind,] <- c(dataFrame$DateTime[i], dataFrame$OutcomeType[i], dataFrame$AnimalType[i],
                    dataFrame$SexuponOutcome[i], dataFrame$AgeuponOutcome[i], new_breed_1, dataFrame$Color[i])
      ind <- ind + 1
      df[ind,] <- c(dataFrame$DateTime[i], dataFrame$OutcomeType[i], dataFrame$AnimalType[i],
                    dataFrame$SexuponOutcome[i], dataFrame$AgeuponOutcome[i], new_breed_2, dataFrame$Color[i])
      ind <- ind + 1
    } else {
      df[ind,] <- c(dataFrame$DateTime[i], dataFrame$OutcomeType[i], dataFrame$AnimalType[i],
                    dataFrame$SexuponOutcome[i], dataFrame$AgeuponOutcome[i], new_breed_1, dataFrame$Color[i])
      ind <- ind + 1
      df[ind,] <- c(dataFrame$DateTime[i], dataFrame$OutcomeType[i], dataFrame$AnimalType[i],
                    dataFrame$SexuponOutcome[i], dataFrame$AgeuponOutcome[i], new_breed_2, dataFrame$Color[i])
      ind <- ind + 1
      df[ind,] <- c(dataFrame$DateTime[i], dataFrame$OutcomeType[i], dataFrame$AnimalType[i],
                    dataFrame$SexuponOutcome[i], dataFrame$AgeuponOutcome[i], new_breed_3, dataFrame$Color[i])
      ind <- ind + 1  
    }
    
  } else if (length(breeds) > 3) {
    print(length(breeds))
  }
}


# Convert data to  factors, then convert it to number then detect outliers
df$DateTime <- factor(df$DateTime, labels = c(unique(df$DateTime)))
df$OutcomeType <- factor(df$OutcomeType, labels = c(unique(df$OutcomeType)))
df$AnimalType <- factor(df$AnimalType, labels = c(unique(df$AnimalType)))
df$SexuponOutcome <- factor(df$SexuponOutcome, labels = c(unique(df$SexuponOutcome)))
df$AgeuponOutcome <- factor(df$AgeuponOutcome, labels = c(unique(df$AgeuponOutcome)))
df$Breed <- factor(df$Breed, labels = c(unique(df$Breed)))
df$Color <- factor(df$Color, labels = c(unique(df$Color)))

df <- subset(df, select = -c(Color))

doggos <- data.frame(DateTime=character(), OutcomeType=character(), SexuponOutcome=character(), 
                 AgeuponOutcome=character(), Breed=character())


kitties <- data.frame(DateTime=character(), OutcomeType=character(), SexuponOutcome=character(), 
                     AgeuponOutcome=character(), Breed=character())



# kable(attrEval(OutcomeType ~ . , dataFrame, estimator = "Gini"))

# Remove outliers (apply different methods and compare)
# Make some relation analyses apriori
# use some methods for prediction
# dfAsNumeric <- df
# dfAsNumeric = subset(dfAsNumeric, select = -c(DateTime, Color))
# 
# # dfAsNumeric$DateTime <- as.numeric(dataFrame$DateTime)
# dfAsNumeric$OutcomeType <- as.numeric(df$OutcomeType)
# dfAsNumeric$AnimalType <- as.numeric(df$AnimalType)
# dfAsNumeric$SexuponOutcome <- as.numeric(df$SexuponOutcome)
# dfAsNumeric$AgeuponOutcome <- as.numeric(df$AgeuponOutcome)
# dfAsNumeric$Breed <- as.numeric(df$Breed)
# # dfAsNumeric$Color <- as.numeric(dataFrame$Color)
# 
# 
# dbscan.outliers <- function(data, ...) {
#   require(fpc, quietly=TRUE)
#   cl <- dbscan(data, ...)
#   posOuts <- which(cl$cluster == 0)
#   list(positions = posOuts,
#        outliers = data[posOuts,],
#        dbscanResults = cl)
# }
# 
# 
# outs <- dbscan.outliers(dfAsNumeric, eps = 3.5)
# 
# # outliers are not reasonable to delete
# df[outs$positions,]

####################### TREE#################################
# samp <- sample(1:nrow(df), 23778)
# tr_set <- df[samp, ]
# tst_set <- df[-samp, ]
# model <- rpartXse(OutcomeType ~ ., tr_set, se = 0.5)
# predicted <- predict(model, tst_set, type = "class")
# head(predicted)
# 
# table(tst_set$OutcomeType, predicted)
# errorRate <- sum(predicted != tst_set$OutcomeType) / nrow(tst_set)
# errorRate
# 
# 

###########################################################

###############SVN##############################################
# samp <- sample(1:nrow(df), 23778)
# tr_set <- df[samp, ]
# tst_set <- df[-samp, ]
# 
# s <- svm(OutcomeType ~ ., tr_set, cost=50, kernel="polynomial", degree=20)
# ps <- predict(s, tst_set)
# (cm <- table(ps, tst_set$OutcomeType))
#############################################################

########################RANDM FOREST##########################
samp <- sample(1:nrow(df), 23778)
tr_set <- df[samp, ]
tst_set <- df[-samp, ]
m <- randomForest(OutcomeType ~ ., tr_set, ntree = 3000)
ps <- predict(m,tst_set)
(cm <- table(ps, tst_set$OutcomeType))
##############################################