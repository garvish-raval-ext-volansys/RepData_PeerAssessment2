
## ----cache=TRUE----------------------------------------------------------

## Set Working Directory to This file location
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)

## ------------------------------------------------------------------------
library(plyr)
library(dplyr)
library(ggplot2)
library(gridExtra)

# Data Processing

## First Load the Dataset
storm <- read.csv(bzfile("data/repdata-data-StormData.csv.bz2"))

## Lets First Find Number of unique Strom Events

## translate all letters to lowercase
event_types <- tolower(storm$EVTYPE)
## replace all punct. characters with a space
event_types <- gsub("[[:blank:][:punct:]+]", " ", event_types)
uniqueEvent <- length(unique(event_types))
uniqueEvent

# Top 10 most harmful Events with respect to Population Health
casualties <- storm %>% select(EVTYPE , FATALITIES, INJURIES) %>% 
                group_by(EVTYPE) %>%
                summarise(FATALITIES = sum(FATALITIES), INJURIES = sum(INJURIES)) %>%
                arrange(desc(FATALITIES), desc(INJURIES))

## Find events that caused most death and injury
fatal_events_10 <- casualties %>% select(EVTYPE , FATALITIES) %>%  head(10)
injury_events_10 <- casualties %>% select(EVTYPE , INJURIES) %>%  head(10)


fatal_events_10
injury_events_10

## Top 10 Events have the greatest economic consequences

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

## ----cache=TRUE----------------------------------------------------------
prop_dmg_exp <- sapply(storm$PROPDMGEXP, FUN=exp_transform)
storm$prop_dmg <- storm$PROPDMG * (10 ** prop_dmg_exp)
crop_dmg_exp <- sapply(storm$CROPDMGEXP, FUN=exp_transform)
storm$crop_dmg <- storm$CROPDMG * (10 ** crop_dmg_exp)

## ------------------------------------------------------------------------
# Compute the economic loss by event type

# Top 10 most harmful Events with respect to Economic Loss
economicsLoss <- storm %>% select(EVTYPE , prop_dmg, crop_dmg) %>% 
                  group_by(EVTYPE) %>%
                  summarise(prop_dmg = sum(prop_dmg), crop_dmg = sum(crop_dmg)) %>%
                  arrange(desc(prop_dmg), desc(crop_dmg))

## Find events that caused most death and injury
prop_dmg_10 <- economicsLoss %>% select(EVTYPE , prop_dmg) %>%  head(10)
crop_dmg_10 <- economicsLoss %>% select(EVTYPE , crop_dmg) %>%  head(10)

prop_dmg_10
crop_dmg_10

# Show Result in Graph

## Set the levels in order
p1 <- ggplot(data=fatal_events_10,
             aes(x=reorder(EVTYPE, FATALITIES), y=FATALITIES, fill=FATALITIES)) +
    geom_bar(stat="identity") +
    coord_flip() +
    ylab("Total number of Fatalities") +
    xlab("Event type") +
    theme(legend.position="none")

p2 <- ggplot(data=injury_events_10,
             aes(x=reorder(EVTYPE, INJURIES), y=INJURIES, fill=INJURIES)) +
    geom_bar(stat="identity") +
    coord_flip() + 
    ylab("Total number of Injuries") +
    xlab("Event type") +
    theme(legend.position="none")

grid.arrange(p1, p2, nrow=2, top="Top deadly weather Events in the US (1950-2011)")

## ------------------------------------------------------------------------

# Set the levels in order
p1 <- ggplot(data=prop_dmg_10,
             aes(x=reorder(EVTYPE, prop_dmg), y=log10(prop_dmg), fill=prop_dmg )) +
    geom_bar(stat="identity") +
    coord_flip() +
    xlab("Event type") +
    ylab("Property damage in dollars (log-scale)") +
    theme(legend.position="none")

p2 <- ggplot(data=crop_dmg_10,
             aes(x=reorder(EVTYPE, crop_dmg), y=crop_dmg, fill=crop_dmg)) +
    geom_bar(stat="identity") +
    coord_flip() + 
    xlab("Event type") +
    ylab("Crop damage in dollars") + 
    theme(legend.position="none")

grid.arrange(p1, p2, nrow=2, top="Weather costs to the US economy (1950-2011)")
