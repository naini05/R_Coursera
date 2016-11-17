complete <- function(directory, id = 1:332)
{
        setwd(directory)
        data <- list.files()
        completeCases <- data.frame()
        for(i in id)
        {
                temp <- read.csv(data[i])
                completeRows = nrow(temp[complete.cases(temp),])
                completeCases <- rbind(completeCases, c(i,completeRows))
        }
        names(completeCases) <- c("id", "nobs")
        completeCases
}