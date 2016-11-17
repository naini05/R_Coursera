rankall <- function(outcome, num = "best") 
{
      ## Read outcome data
      outcomeData <- read.csv("outcome-of-care-measures.csv",  na.strings="Not Available" )
      validOutcomes <- c("heart attack"= 11, "heart failure" = 17, "pneumonia" = 23)
      
      ## Check that outcome is valid
      
      if(!(outcome %in% names(validOutcomes)))
      {
            stop("invalid outcome")
      }
      
      requiredData <-  outcomeData[, c(2,7,validOutcomes[outcome])]
      requiredData <- requiredData[complete.cases(requiredData),]
      requiredData <- requiredData[order(requiredData[,2], requiredData[,3], requiredData[,1]),]
      tempData <- split(requiredData, requiredData$State)
     
       ## For each state, find the hospital of the given rank
            
      if(num == "best")
            num <- 1
            
      hospitalName <- function(x)
      {
            if(num == "worst")
             num <- nrow(x)
            x[num,1]
      }

      hospitalRanking <- lapply(tempData, hospitalName)
      stateRanking <- data.frame(hospital = unlist(hospitalRanking), state = names(hospitalRanking), row.names = names(hospitalRanking) )
}