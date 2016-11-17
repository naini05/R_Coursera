rankhospital <- function(state, outcome, num = "best")
{
      ## Read outcome data
      outcomeData <- read.csv("outcome-of-care-measures.csv",  na.strings="Not Available" )
      
      validOutcomes <- c("heart attack"= 11, "heart failure" = 17, "pneumonia" = 23)
      
      ## Check that state and outcome are valid
      if(!(state %in% outcomeData$State))
      {
            stop("invalid state")
      }
      
      if(!(outcome %in% names(validOutcomes)))
      {
            stop("invalid outcome")
      }
      
      requiredData <-  outcomeData[, c(2,7,validOutcomes[outcome])]
      requiredData <- requiredData[complete.cases(requiredData),]
      tempData <- split(requiredData, requiredData$State)
      requiredStateData <- tempData[names(tempData) %in% state]
      requiredStateData <- requiredStateData[[1]]
     
      sortedData <- requiredStateData[order(requiredStateData[,3], requiredStateData[,1]), ]
      
      if(num == "best")
            num <- 1
      if(num == "worst")
            num <- nrow(sortedData)
      
      ## Return hospital name in that state with the given rank
      ## 30-day death rate
      
      as.character(sortedData[num,1])
}