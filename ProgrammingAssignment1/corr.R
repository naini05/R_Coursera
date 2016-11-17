corr <- function(directory, threshold = 0) 
{
      setwd("D:/R_Code")
      setwd(directory)
      data <- list.files()
      correlations <- numeric()
      for(i in seq_along(data))
      {
            temp <- read.csv(data[i])
            completeRows = nrow(temp[complete.cases(temp),])
            if(completeRows > threshold)
            {
                  nitrate <- temp[,"nitrate"]
                  sulfate <- temp[,"sulfate"]
                  correlations = c(correlations, cor(sulfate,nitrate,use="complete.obs"))
            }
      }
      correlations
}
