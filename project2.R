#loading data.
library(readr)
repdata_data_StormData_csv <- read_csv("repdata_data_StormData.csv.bz2")

#subsetting by date
main_data <- repdata_data_StormData_csv
main_data$BGN_DATE <- strptime(repdata_data_StormData_csv$BGN_DATE, "%m/%d/%Y %H:%M:%S")
main_data <- subset(main_data, BGN_DATE > "1995-12-31")

#subsetting to needed columns
main_data <- subset(main_data, select = c(EVTYPE, FATALITIES, INJURIES, PROPDMG, PROPDMGEXP,CROPDMG, CROPDMGEXP))

#cleaning event types names
main_data$EVTYPE <- toupper(main_data$EVTYPE)

#eliminating zero data
main_data <- main_data[main_data$FATALITIES != 0 | main_data$INJURIES != 0 | main_data$PROPDMG != 0 | main_data$CROPDMG != 0,]

#Population health data processing
Health_data <- aggregate(cbind(FATALITIES, INJURIES) ~ EVTYPE, data = main_data, FUN = sum)
Health_data$PEOPLE_LOSS <- Health_data$FATALITIES + Health_data$INJURIES
Health_data <- Health_data[order(Health_data$PEOPLE_LOSS, decreasing = TRUE),]
Top10_events_people <- Health_data[1:10,]
print(Top10_events_people)

#Economic consequences data processing
main_data$PROPDMGEXP <- gsub("[Hh]", "2", main_data$PROPDMGEXP)
main_data$PROPDMGEXP <- gsub("[Kk]", "3", main_data$PROPDMGEXP)
main_data$PROPDMGEXP <- gsub("[Mm]", "6", main_data$PROPDMGEXP)
main_data$PROPDMGEXP <- gsub("[Bb]", "9", main_data$PROPDMGEXP)
main_data$PROPDMGEXP <- gsub("\\+", "1", main_data$PROPDMGEXP)
main_data$PROPDMGEXP <- gsub("\\?", "0", main_data$PROPDMGEXP)
main_data$PROPDMGEXP <- as.numeric(main_data$PROPDMGEXP)
main_data$CROPDMGEXP <- gsub("[Hh]", "2", main_data$CROPDMGEXP)
main_data$CROPDMGEXP <- gsub("[Kk]", "3", main_data$CROPDMGEXP)
main_data$CROPDMGEXP <- gsub("[Mm]", "6", main_data$CROPDMGEXP)
main_data$CROPDMGEXP <- gsub("[Bb]", "9", main_data$CROPDMGEXP)
main_data$CROPDMGEXP <- gsub("\\+", "1", main_data$CROPDMGEXP)
main_data$CROPDMGEXP <- gsub("\\?", "0", main_data$CROPDMGEXP)
main_data$CROPDMGEXP <- as.numeric(main_data$CROPDMGEXP)
main_data$PROPDMGEXP[is.na(main_data$PROPDMGEXP)] <- 0
main_data$CROPDMGEXP[is.na(main_data$CROPDMGEXP)] <- 0

#Creating total damage values
library(dplyr)
main_data <- mutate(main_data, PROPDMGTOTAL = PROPDMG *(10^PROPDMGEXP), CROPDMGTOTAL = CROPDMG * (10^CROPDMGEXP))

#analyzing
Economic_data <- aggregate(cbind(PROPDMGTOTAL, CROPDMGTOTAL) ~ EVTYPE, data = main_data, FUN = sum)
Economic_data$ECONOMIC_LOSS <- Economic_data$PROPDMGTOTAL+Economic_data$CROPDMGTOTAL
Economic_data <- Economic_data[order(Economic_data$ECONOMIC_LOSS, decreasing = TRUE),]
Top10_events_economy <- Economic_data[1:10,]
print(Top10_events_economy)

#plotting health loss
library(ggplot2)

h <- ggplot(data=Top10_events_people, aes(x=reorder(EVTYPE, PEOPLE_LOSS),y=PEOPLE_LOSS))
h <- h+geom_bar(stat="identity", colour= "black")
h <- h+labs(title = "Total people loss in USA by weather events in 1996-2011")
h <- h+theme(plot.title = element_text(hjust = 0.5))
h <- h+labs(y="Number of fatalities and injuries", x="Event Type")
h <- h+coord_flip()
print(h)


#plotting economic loss

e <- ggplot(data=Top10_events_economy, aes(x=reorder(EVTYPE, ECONOMIC_LOSS),y=ECONOMIC_LOSS))
e <- e+geom_bar(stat="identity", colour= "black")
e <- e+labs(title = "Total economic loss in USA by weather events in 1996-2011")
e <- e+theme(plot.title = element_text(hjust = 0.5))
e <- e+labs(y="Size of property and crop -loss", x="Event Type")
e <- e+coord_flip()
print(e)
