# get the data
data <- read.csv("data/Process_Status.csv", stringsAsFactors=F)
str(data)

# convert date time char's to lubridate date times
library(lubridate)
data$SUBMITTED <- mdy_hms(data$SUBMITTED, truncated=3)
data$STARTED <- mdy_hms(data$STARTED, truncated=3)
data$COMPLETED <- mdy_hms(data$COMPLETED, truncated=3)

# add a column representing the quarter to which the period corresponds
fn_setQtr <- function(str) {
    if (((length(grep("jan", str, ignore.case=T)) + 
              length(grep("feb", str, ignore.case=T)) + 
              length(grep("mar", str, ignore.case=T)) + 
              length(grep("qtr-1", str, ignore.case=T)))) > 0)
        retVal <- "QTR-1-2014"
    else if (((length(grep("apr", str, ignore.case=T)) + 
              length(grep("may", str, ignore.case=T)) + 
              length(grep("jun", str, ignore.case=T)) + 
              length(grep("qtr-2", str, ignore.case=T)))) > 0)
        retVal <- "QTR-2-2014"
    else if (((length(grep("jul", str, ignore.case=T)) + 
                   length(grep("aug", str, ignore.case=T)) + 
                   length(grep("sep", str, ignore.case=T)) + 
                   length(grep("qtr-4", str, ignore.case=T)))) > 0)
        retVal <- "QTR-3-2014"
    else if (((length(grep("octr", str, ignore.case=T)) + 
                   length(grep("nov", str, ignore.case=T)) + 
                   length(grep("dec", str, ignore.case=T)) + 
                   length(grep("year", str, ignore.case=T )) + 
                   length(grep("qtr-4", str, ignore.case=T)))) > 0)
        retVal <- "QTR-4-2014"
    else
        retVal <- "n/a"
    return(retVal)
}
suppressWarnings(data$quarter <- as.character(lapply(data$PERIOD, fn_setQtr)))

# calculate the duration in hours and put it in a new column
data$hours <- as.numeric(difftime(data$COMPLETED, data$STARTED, units="hours"))

