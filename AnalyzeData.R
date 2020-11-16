
readData <- function() {
  data <- read.csv("C:/Users/user/Desktop/dataminingproject/ShelterProblem/data/train.csv")
  print("Data is read from CSV.")
  dataFrame <- data.frame(data)
  
  print("Data frame is created")
  return(dataFrame)
}

printLists <- function(labelName, listToPrint) {
  formattedString <- ""
  for (listItem in listToPrint) {
    if(formattedString == "") {
      formattedString <- listItem
    } else {
      formattedString <- paste(formattedString, ",", listItem)      
    }
  }
  cat(labelName, formattedString, "\n")
}

analyzeData <- function(dataFrame) {
  dimensions <- dim(dataFrame)
  
  # Print dimensions
  cat("Number of rows: ", dimensions[1], "\n")
  cat("Number of columns: ", dimensions[2], "\n")
  
  # print numeric type columns
  selected_column <- dplyr::select_if(dataFrame, is.numeric)
  numericColumnNames <- names(selected_column)
  printLists("Numeric columns: ", numericColumnNames)
  
  # print character type columns
  selected_column <- dplyr::select_if(dataFrame, is.character)
  printLists("Character columns: ", names(selected_column))
  
  # print logical type columns
  selected_column <- dplyr::select_if(dataFrame, is.logical)
  logicalColumnNames <-names (selected_column)
  printLists("Logical columns: ", logicalColumnNames)
  
  # print factor type columns
  selected_column <- dplyr::select_if(dataFrame, is.factor)
  printLists("Factor columns: ", names(selected_column))
  
  # print number of NA values
  naSum <- sum(is.na(dataFrame))
  cat("Number of NAs: ", naSum, "\n")
  
  # print number of NA values for each column
  naTable <- colSums(is.na(dataFrame))
  naColNames <- names(naTable)
  
  cat("Columns with NAs: ")
  for (i in 1:length(naTable)) {
    if (naTable[i] > 0) {
      formattedNaResult <- paste(naColNames[i], "(", naTable[i], ")")
      if (i == 1) {
        cat(formattedNaResult)  
      } else {
        cat(paste(" ,", formattedNaResult))
      }
    }
  }
  cat("\n")
  
  # print na values' indices in a row
  cat("Indices of NAs in rows: ")
  naIndices <- which(is.na(dataFrame), arr.ind=TRUE)
  df2 <- data.frame(row=naIndices[,1])
  df2 <- df2[order(df2$row),]
  
  cat(unique(df2), "\n")
  
  # print mean and standard deviation for each column
  cat("Mean and standard deviation of numeric columns: \n")
  for(numericColumnName in numericColumnNames) {
    numericValues <- na.omit(dataFrame[[numericColumnName]])
    cat("   ", numericColumnName, 
        ": \n          Mean              : ", mean(numericValues), 
        "\n          Standard Deviation: ", sd(numericValues), "\n")
  }
  
  
  #print the percentages of logical columns
  logicalPercentages <- dataFrame %>% 
    group_by( bool_col1 ) %>% 
    summarise( percent = 100 * n() / nrow( dataFrame ), .groups = 'drop')
  cat("Logical columns percentages:\n")
  for (i in 1:length(logicalPercentages)) {
    logicalPercentage <- logicalPercentages[i,]
    cat("     ", logicalPercentage[[1]], ": ", logicalPercentage[[2]], "\n")
  }
  
  # print categorical stuff
  cat("Categorical column levels: \n")
  for (i in 1:dimensions[2]) {
    dataFrameColumn <- dataFrame[[i]]
    if (is.numeric(dataFrameColumn) == FALSE) {
      factorForDF <- factor(dataFrameColumn)
      tableName <- names(dataFrame[i])
      tableWithCount <- table(dataFrameColumn)
      cat("     ", names(dataFrame[i]), "[", nlevels(factorForDF) ,"] - ")
      
      orderedTableWithCount <- tableWithCount[order(tableWithCount, decreasing = TRUE)]
      
      j <- 1
      
      for (element in orderedTableWithCount) {
        if (j <= 3) {
          if (j != 1) {
            cat(", ")
          }
          cat(names(orderedTableWithCount)[j], "(",  element, ")")
          j <- j + 1
        }
      }
      
      cat("\n")
    }
  }
  
  # print correlations between rows
  cat("Correlation of ")
  for (columnName1 in numericColumnNames) {
    for (columnName2 in numericColumnNames) {
      if (columnName1 != columnName2) {
        cat(columnName1, " and ", columnName2, ": ", cor(dataFrame[columnName1], dataFrame[columnName1]))
      }
      
    }
    cat("\n")
  }
  
}

# dataFrame <- readData()
example_data <- data.frame(site = c("A", "B", NA, "A", "B", "B"),
                             season = c("Winter", "Summer", "Summer", "Spring",
                                        "Fall", "Spring"),
                             numeric_col1 = c(90, 40, 21.1, 10, 30, 1),
                             numeric_col2 = c(1, 40, 20, 300, 2100, 1000),
                             bool_col1 = c(NA, TRUE, FALSE, NA, FALSE, FALSE))
analyzeData(example_data)
