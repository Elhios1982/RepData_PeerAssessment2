library(dplyr)
library(ggplot2)
library(lubridate)
library(car)

working.directory <- "C:/coursera/RepData_PeerAssessment2"

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

casualties <- arrange(health.impact, desc(fatalities))


#ggplot(casualties[1:10,], aes(x = factor(injuries), y = EVTYPE)) + geom_bar(stat = "identity")
ggplot(casualties[1:5,], aes(x = factor(EVTYPE), y = fatalities)) + 
  geom_bar(stat = "identity") + 
  guides(fill = FALSE) + xlab("Weather Event") + ylab("Number of Fatalities") +
  ggtitle("Fatalities due to Severe Weather Events")


ggplot(casualties[1:5,], aes(x = factor(EVTYPE), y = injuries)) + 
  geom_bar(stat = "identity") + 
  guides(fill = FALSE) + xlab("Weather Event") + ylab("Number of Injuries") +
  ggtitle("Injuries due to Severe Weather Events")



required.data$PROPDMG.B <- recode(as.character(required.data$PROPDMGEXP), "'K'=1e-6; 'M'=1e-3;'B'=1;''=0")
required.data$PROPDMG.B <- required.data$PROPDMG.B * required.data$PROPDMG
required.data$CROPDMG.B <- recode(as.character(required.data$CROPDMGEXP), "'K'=1e-6; 'M'=1e-3;'B'=1;''=0")
required.data$CROPDMG.B <- required.data$CROPDMG.B * required.data$CROPDMG

event.cost <- group_by(required.data, EVTYPE) %>% 
  summarize(property.dmg = sum(PROPDMG.B), crop.dmg = sum(CROPDMG.B)) 

event.cost.sub <- mutate(event.cost, total = property.dmg + crop.dmg) %>%
  arrange(desc(total))

ggplot(event.cost.sub[1:5,], aes(x = factor(EVTYPE), y = total)) + 
  geom_bar(stat = "identity") + 
  guides(fill = FALSE) + xlab("Weather Event") + ylab("Economic Impact (USD in Bilions)") +
  ggtitle("Economic Cost due to Severe Weather Events")



