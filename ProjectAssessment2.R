library(dplyr)
library(ggplot2)
library(lubridate)

getExp <- function(e) {
  if (e %in% c("h", "H"))
    return(2)
  else if (e %in% c("k", "K"))
    return(3)
  else if (e %in% c("m", "M"))
    return(6)
  else if (e %in% c("b", "B"))
    return(9)
  else if (!is.na(as.numeric(e))) 
    return(as.numeric(e))
  else if (e %in% c("", "-", "?", "+"))
    return(0)
  else {
    stop("Invalid value.")
  }
}


URL <- "http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
destfile <- "stormData.csv.bz2"
download.file(URL, destfile)

df <- tbl_df(read.csv(destfile, header = TRUE, na.strings = "NA" )) 

from <- mdy_hms("01-01-2001 0:00:00")
to <- mdy_hms("12-01-2011 0:00:00")

required.data <- select(df, BGN_DATE, EVTYPE, FATALITIES, INJURIES, PROPDMG, PROPDMGEXP, CROPDMG, CROPDMGEXP) %>%
                  mutate(BGN_DATE = mdy_hms(BGN_DATE)) %>%
                  filter(BGN_DATE > from & BGN_DATE < to)

health.impact <- group_by(required.data, EVTYPE) %>% 
  summarize(fatalities = sum(FATALITIES), injuries = sum(INJURIES)) %>%
  filter(fatalities > 0 | injuries > 0)




qplot(x = injuries, y = EVTYPE, data = health.impact)