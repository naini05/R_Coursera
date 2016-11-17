pollutantmean <- function(directory, pollutant, id = 1:332)
{
        setwd(directory)
        data <- list.files()
        dataForMean <- data.frame()
        for(i in id)
        {
                temp <- read.csv(data[i])
                dataForMean <- rbind(dataForMean, temp)
        }
        mean(dataForMean[, pollutant],na.rm = TRUE)
}