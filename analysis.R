
setwd("C:\\Users\\vvrsk\\Desktop\\Coursera\\Course 5\\Week 4")

#importing the librarires


require(R.utils)
require(ggplot2)
library(gridExtra)

#Sytem info for replication

sessionInfo()

#Unzip the data

bunzip2("data.csv.bz2")

#Reading the data into R
dataip<- read.csv("data.csv",header = TRUE, sep =",",dec = ".",fill=TRUE)

#View The data
View(dataip)

#gettinng the data outline - 902297 Observations    37 Variables
dim(dataip)

str(dataip)

#Getting the data of interest

dataofi <- dataip[, c("EVTYPE", "FATALITIES", "INJURIES", "PROPDMG", "PROPDMGEXP",
                      "CROPDMG", "CROPDMGEXP")]

#Checking the data
str(dataofi)

#Data Pre-processing

# number of unique event types

length(unique(dataofi$EVTYPE))

# translate all letters to lowercase
event_types <- tolower(dataofi$EVTYPE)

# replace all punct. characters with a space

event_types <- gsub("[[:blank:][:punct:]+]", " ", event_types)
length(unique(event_types))


# update the data frame

dataofi$EVTYPE <- event_types


#Getting the effect of the events wrt to the polulations health

library(plyr)

casualties <- ddply(dataofi, .(EVTYPE), summarize,
                    fatalities = sum(FATALITIES),
                    injuries = sum(INJURIES))

# Find events that caused most death and injury

fatal_events <- head(casualties[order(casualties$fatalities, decreasing = T), ], 10)
injury_events <- head(casualties[order(casualties$injuries, decreasing = T), ], 10)


#The events that caused the largest number of deaths are

fatal_events[, c("EVTYPE", "fatalities")]


#The events that caused the largest number of injusries are

injury_events[, c("EVTYPE", "injuries")]


#Getting the effect of the events wrt to the Ecocnomy

#Converting the valuesof the expenses in to Final Monetary Value.
# Inorder to achieve this, we intially convert the exp value to the respective power of 10
# then we multiply with the coefficient to get the total value


exp_transform <- function(e) {
        # h -> hundred, k -> thousand, m -> million, b -> billion
        if (e %in% c('h', 'H'))
                return(2)
        else if (e %in% c('k', 'K'))
                return(3)
        else if (e %in% c('m', 'M'))
                return(6)
        else if (e %in% c('b', 'B'))
                return(9)
        else if (!is.na(as.numeric(e))) # if a digit
                return(as.numeric(e))
        else if (e %in% c('', '-', '?', '+'))
                return(0)
        else {
                stop("Invalid exponent value.")
        }
}

str(dataofi$PROPDMGEXP)

dataofi$PROPDMGEXP <- sapply(dataofi$PROPDMGEXP, FUN=exp_transform)

dataofi$CROPDMGEXP <- sapply(dataofi$CROPDMGEXP, FUN=exp_transform)
 
dataofi$CROPDMGDOLLAR <- dataofi$CROPDMG*dataofi$CROPDMGEXP

dataofi$PROPDMGDOLLAR <- dataofi$PROPDMG*dataofi$PROPDMGEXP 


econ_losses <- ddply(dataofi, .(EVTYPE), summarize,
                    crop_loss = sum(CROPDMGDOLLAR),
                    prop_loss = sum(PROPDMGDOLLAR))


# Find events that caused most crop and property damage

croploss_events <- head(econ_losses[order(econ_losses$crop_loss, decreasing = T), ], 10)
proploss_events <- head(econ_losses[order(econ_losses$prop_loss, decreasing = T), ], 10)


#The events that caused the largest number of deaths are

croploss_events[, c("EVTYPE", "crop_loss")]


#The events that caused the largest number of injusries are

proploss_events[, c("EVTYPE", "prop_loss")]


#Results

#Effect of events on Health

p1 <- ggplot(data=fatal_events,aes(x=reorder(EVTYPE,fatalities),y=fatalities, fill = fatalities)) +
        geom_bar(stat = "identity") +
        coord_flip()+
        ylab("Total number of fatalities") +
        xlab("Event type") + theme(legend.position="none")

p2 <- ggplot(data=injury_events,
             aes(x=reorder(EVTYPE, injuries), y=injuries, fill=injuries)) +
        geom_bar(stat="identity") +
        coord_flip() + 
        ylab("Total number of injuries") +
        xlab("Event type") +
        theme(legend.position="none")

grid.arrange(p1, p2, top="Top deadly weather events in the US (1950-2011)")


#Effect of events on Economy

p3 <- ggplot(data=proploss_events,
             aes(x=reorder(EVTYPE, prop_loss), y=log10(prop_loss), fill=prop_loss )) +
        geom_bar(stat="identity") +
        coord_flip() +
        xlab("Event type") +
        ylab("Property damage in dollars (log-scale)") +
        theme(legend.position="none")

p4 <- ggplot(data=croploss_events,
             aes(x=reorder(EVTYPE, crop_loss), y=crop_loss, fill=crop_loss)) +
        geom_bar(stat="identity") +
        coord_flip() + 
        xlab("Event type") +
        ylab("Crop damage in dollars") + 
        theme(legend.position="none")

grid.arrange(p3, p4, top="Weather costs to the US economy (1950-2011)")


