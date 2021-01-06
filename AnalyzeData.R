
# helper function: This function prints the given list
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


# main function: This function prints the statistics of the data
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
  if (length(numericColumnNames) > 0) {
    for(numericColumnName in numericColumnNames) {
      numericValues <- na.omit(dataFrame[[numericColumnName]])
      cat("   ", numericColumnName, 
          ": \n          Mean              : ", mean(numericValues), 
          "\n          Standard Deviation: ", sd(numericValues), "\n")
    }
  }
  
  #print the percentages of logical columns
  cat("Logical columns percentages:\n")
  logical_columns <- dplyr::select_if(dataFrame, is.logical)
  
  for (dataFrameColumnName in names(logical_columns)) {
    dataFrameColumn <- logical_columns[dataFrameColumnName]
    cat("     ", dataFrameColumnName, ": ")
    factorForDF <- factor(dataFrameColumn)
    tableWithCount <- table(dataFrameColumn)
    
    totalElementCountInRow <- sum(tableWithCount)
    j <- 1
    for (element in tableWithCount) {
      if (j != 1) {
        cat(", ")
      }
      cat(names(tableWithCount)[j], "(",  element/totalElementCountInRow, ")")
      j <- j + 1
    }
    cat("\n")
  }
  
  
  # print categorical stuff
  cat("Categorical column levels: \n")
  for (i in 1:dimensions[2]) {
    dataFrameColumn <- dataFrame[[i]]
    if (is.numeric(dataFrameColumn) == FALSE) {
      factorForDF <- factor(dataFrameColumn)
      tableName <- names(dataFrame[i])
      tableWithCount <- table(dataFrameColumn)
      cat("     ", tableName, "[", nlevels(factorForDF) ,"] - ")
      
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
  
  numericColNumber <- length(numericColumnNames)
  # print correlations between rows
  if (numericColNumber > 0) {
    for (i in 1:(numericColNumber - 1)) {
      for (j in (i+1):numericColNumber) {
        cat("Correlation of ", numericColumnNames[i], " and ", numericColumnNames[j], ": ", 
            cor(dataFrame[numericColumnNames[i]], dataFrame[numericColumnNames[j]]))
      }
      cat("\n")
    }
  } else {
    cat("No numerical data exists to calculate correlation.")
  }
  
}

############################# EXAMPLE USAGE ##########################################
# example_data <- data.frame(site = c("A", "B", NA, "A", "B", "B"),                  #
#                              season = c("Winter", "Summer", "Summer", "Spring",    #
#                                         "Fall", "Spring"),                         #
#                              numeric_col1 = c(90, 40, 21.1, 10, 30, 1),            #
#                              numeric_col2 = c(1, 40, 20, 300, 2100, 1000),         #
#                              bool_col1 = c(NA, TRUE, FALSE, NA, FALSE, FALSE),     #
#                            bool_col2 = c(NA, FALSE, FALSE, TRUE, FALSE, FALSE))    #
# analyzeData(example_data)                                                          #
######################################################################################
