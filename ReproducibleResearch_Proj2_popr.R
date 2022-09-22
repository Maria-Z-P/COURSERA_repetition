setwd("C:/Maja/M/R Programming_Coursera")
#setwd("C:/Maja/M/R Programming_Coursera/Weather_events")

#-----
# Loading and processing the raw data

#URL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
#bzFil <- basename(URL)
#if (!file.exists(bzFil)) download.file(URL, bzFil)

# While reading the .z, .gz, .bz2 you don’t need to unzip it. Simply read it 
# directly into R.

#dt_r <- read.csv(bzFil)

dt_r <- read.csv("repdata%2Fdata%2FStormData.csv.bz2")
dim(dt_r)
head(dt_r, 2)
my_dt_r <- dt_r[, c(8,23:28)]
#View(my_dt_r)
head(my_dt_r)
summary(my_dt_r)
my_dt_r$EVTYPE <- as.factor(my_dt_r$EVTYPE)
str(my_dt_r)
#sort(table(my_dt_r$EVTYPE), decreasing = TRUE)
sort(table(my_dt_r$EVTYPE), decreasing = TRUE)[1:12]

# The EVTYPE field contains a variety of similar values. It’s clear that similar
# event types have been written slightly different. For example THUNDERSTORM WIND 
# and TSTM WIND are the same type of event. So we need to clean our data a bit.

library(stringr)

my_dt <- my_dt_r
my_dt$EVTYPE <- str_replace(my_dt$EVTYPE, "THUNDERSTORM WIND", "TSTM WIND")
my_dt$EVTYPE <- str_replace(my_dt$EVTYPE, "TSTM WINDS", "TSTM WIND")
my_dt$EVTYPE <- str_replace(my_dt$EVTYPE, "THUNDERSTORM WINDS", "TSTM WIND")
my_dt$EVTYPE <- str_replace(my_dt$EVTYPE, "HURRICANE/TYPHOON", "HURRICANE")
my_dt$EVTYPE <- str_replace(my_dt$EVTYPE, "HURRICANE OPAL", "HURRICANE")
my_dt$EVTYPE <- str_replace(my_dt$EVTYPE, "HURRICANE OPAL/HIGH WINDS", "HURRICANE")
my_dt$EVTYPE <- str_replace(my_dt$EVTYPE, "MARINE THUNDERSTORM WIND", 
                              "MARINE TSTM WIND")
my_dt$EVTYPE <- str_replace(my_dt$EVTYPE, "TSTM WIND/HAIL", "HAIL")
my_dt$EVTYPE <- str_replace(my_dt$EVTYPE, "RIP CURRENTS", "RIP CURRENT")
my_dt$EVTYPE <- str_replace(my_dt$EVTYPE, "FLASH FLOODING", "FLASH FLOOD")
my_dt$EVTYPE <- str_replace(my_dt$EVTYPE, "FLOOD/FLASH FLOOD", "FLASH FLOOD")
my_dt$EVTYPE <- str_replace(my_dt$EVTYPE, "FLASH FLOODS", "FLASH FLOOD")
my_dt$EVTYPE <- str_replace(my_dt$EVTYPE, "URBAN/SML STREAM FLD", "FLOOD")
my_dt$EVTYPE <- str_replace(my_dt$EVTYPE, "RIVER FLOOD", "FLOOD")
my_dt$EVTYPE <- str_replace(my_dt$EVTYPE, "FLOODING", "FLOOD")
my_dt$EVTYPE <- str_replace(my_dt$EVTYPE, "WILD/FOREST FIRE", "WILDFIRE")
my_dt$EVTYPE <- str_replace(my_dt$EVTYPE, "STORM SURGE/TIDE", "STORM SURGE")
my_dt$EVTYPE <- str_replace(my_dt$EVTYPE, "FROST/FREEZE", "FREEZE")
my_dt$EVTYPE <- str_replace(my_dt$EVTYPE, "FROST/FROST/FREEZE", "FREEZE") 
my_dt$EVTYPE <- str_replace(my_dt$EVTYPE, "HEAVY RAIN/SEVERE WEATHER", "HEAVY RAIN")
my_dt$EVTYPE <- str_replace(my_dt$EVTYPE, "TORNADOES, TSTM WIND, HAIL", "TORNADO")
my_dt$EVTYPE <- as.factor(my_dt$EVTYPE)
sort(table(my_dt$EVTYPE), decreasing = TRUE)[1:15]

# 1. Across the United States, which types of events (as indicated in the EVTYPE
# variable) are most harmful with respect to population health?

library(dplyr)

rob <- my_dt[my_dt$FATALITIES != 0, ]
dt.MH.F <- summarise(group_by(rob, EVTYPE), FATALITIES = sum(FATALITIES))
dim(dt.MH.F)
dt.MH.F10 <- head(arrange(dt.MH.F, desc(FATALITIES)), 10)

rob2 <- my_dt[my_dt$INJURIES != 0, ]
dt.MH.I <- summarise(group_by(rob2, EVTYPE), INJURIES = sum(INJURIES))
dim(dt.MH.I)
dt.MH.I10 <- head(arrange(dt.MH.I, desc(INJURIES)), 10)

op <- par(las = 3, mar = c(8.5, 4, 4, 2) + 0.1)
x1 <- barplot(height=dt.MH.F10$FATALITIES/1000, names.arg=dt.MH.F10$EVTYPE, 
              ylim=c(0, 6.5), col = gray.colors(20), space = 0.8,
              ylab=expression('Number of fatalities in thousands'),
              main=expression('The Most Harmful Types of Events in the U.S.
                              FATALITIES'))
# Add text at top of bars
text(x=x1, y=dt.MH.F10$FATALITIES/1000, 
     label=dt.MH.F10$FATALITIES/1000, pos=3, cex=0.8, col="black")
par(op)

library(RColorBrewer)

cols <- brewer.pal(10, "Paired")

op <- par(las = 3, mar = c(8.5, 4, 4, 2) + 0.1)
x2 <- barplot(height=dt.MH.I10$INJURIES/1000, names.arg=dt.MH.I10$EVTYPE, 
              ylim=c(0, 105), col = cols, space = 0.8,
              ylab=expression('Number of injured in thousands'),
              main=expression('The Most Harmful Types of Events in the U.S.
                              INJURIES'))
text(x=x2, y=dt.MH.I10$INJURIES/1000, 
     label=dt.MH.I10$INJURIES/1000, pos=3, cex=0.8, col="black")
par(op)

# 2. Across the United States, which types of events have the greatest economic 
# consequences?

# Property damage and crop damage estimates are entered as actual dollar amounts.
# Estimates are rounded to three significant digits, followed by an alphabetical
# character signifying the magnitude of the number, i.e., 1.55B for $1,550,000,000.
# Alphabetical characters used to signify magnitude include “K” for thousands,
# “M” for millions, and “B” for billions.

rob3 <- my_dt[my_dt$PROPDMG != 0, ]; rob3 <- rob3[, c(1,4,5)]
#View(rob3)
unique(rob3$PROPDMGEXP)
rob3$PROPDMGEXP <- str_replace(rob3$PROPDMGEXP, "m", "M")
rob3$PROPDMGEXP <- str_replace(rob3$PROPDMGEXP, "h", "H")
dim(rob3)
rob3 <- filter(rob3, PROPDMGEXP=="B" | PROPDMGEXP=="M" | PROPDMGEXP=="K" | 
                 PROPDMGEXP=="H")
rob3$PROPDMGEXP <- str_replace(rob3$PROPDMGEXP, "B", "1000000000")
rob3$PROPDMGEXP <- str_replace(rob3$PROPDMGEXP, "M", "1000000")
rob3$PROPDMGEXP <- str_replace(rob3$PROPDMGEXP, "K", "1000")
rob3$PROPDMGEXP <- str_replace(rob3$PROPDMGEXP, "H", "100")
rob3$PROPDMGEXP <- as.numeric(rob3$PROPDMGEXP)
str(rob3)
rob3$PDMG <- rob3$PROPDMG*rob3$PROPDMGEXP
dt.GEC.P <- summarise(group_by(rob3,EVTYPE), PDMG = sum(PDMG))
dim(dt.GEC.P)

rob4 <- my_dt[my_dt$CROPDMG != 0, ]; rob4 <- rob4[, c(1,6,7)]
unique(rob4$CROPDMGEXP)
rob4$CROPDMGEXP <- str_replace(rob4$CROPDMGEXP, "m", "M")
rob4$CROPDMGEXP <- str_replace(rob4$CROPDMGEXP, "k", "K")
dim(rob4)
rob4 <- filter(rob4, CROPDMGEXP=="B" | CROPDMGEXP=="M" | CROPDMGEXP=="K")
rob4$CROPDMGEXP <- str_replace(rob4$CROPDMGEXP, "B", "1000000000")
rob4$CROPDMGEXP <- str_replace(rob4$CROPDMGEXP, "M", "1000000")
rob4$CROPDMGEXP <- str_replace(rob4$CROPDMGEXP, "K", "1000")
rob4$CROPDMGEXP <- as.numeric(rob4$CROPDMGEXP)
str(rob4)
rob4$CDMG <- rob4$CROPDMG*rob4$CROPDMGEXP
dt.GEC.C <- summarise(group_by(rob4,EVTYPE), CDMG = sum(CDMG))
dim(dt.GEC.C)

dt.GEC.P12 <- head(arrange(dt.GEC.P, desc(PDMG)), 12)
dt.GEC.C12 <- head(arrange(dt.GEC.C, desc(CDMG)), 12)

print(cbind(dt.GEC.P12, dt.GEC.C12))

# Plot_v1

op <- par(mfrow = c(2, 1), mar = c(4, 4, 2, 1), oma = c(0, 0, 2, 0))
x3 <- barplot(height=dt.GEC.P12$PDMG/1000000000, names.arg=1:12, ylim=c(0, 200), 
              col = cols, xlab = expression('Type of event'),
              ylab=expression('Property damages'),
              main=expression('PROPERTY DAMAGES in billions of $'))
# Add text at top of bars
text(x=x3, y=dt.GEC.P12$PDMG/1000000000, 
     label=round(dt.GEC.P12$PDMG/1000000000, 1), pos=3, cex=0.8, col="black")

x4 <- barplot(height=dt.GEC.C12$CDMG/1000000000, names.arg=LETTERS[1:12],  
              col = cols, xlab = expression('Type of event'),
              ylab=expression('Crop damages'), ylim=c(0, 19),
              main=expression('CROP DAMAGES in billions of $'))
text(x=x4, y=dt.GEC.C12$CDMG/1000000000, 
     label=round(dt.GEC.C12$CDMG/1000000000, 1), pos=3, cex=0.8, col="black")

mtext("Types of events in the U.S. with the greatest economic consequences", 
      outer=TRUE)
par(op)

# Plot_v2

dt.GEC.P12 <- head(arrange(dt.GEC.P, desc(PDMG)), 12)
dt.GEC.P12$PDMG <- dt.GEC.P12$PDMG/1000000000
dP <- rename(dt.GEC.P12, Damages = PDMG)
dP$DAM <- rep("Property", 12)
dt.GEC.C4 <- head(arrange(dt.GEC.C, desc(CDMG)), 4)
dt.GEC.C4$CDMG <- dt.GEC.C4$CDMG/1000000000
dC <- rename(dt.GEC.C4, Damages = CDMG)
dC$DAM <- rep("Crop", 4)
dt.GEC <- rbind(dP, dC)
#View(dt.GEC)

library(ggplot2)

g <- ggplot(dt.GEC, aes(Damages, EVTYPE)) + facet_grid(DAM ~ .)
g + geom_col(fill = "blue") + theme_test() +
  xlab("Damage (in billions of dollars)") + ylab("Type of Event") +
  theme(plot.title = element_text(face="bold.italic")) +
  ggtitle("Types of events in the U.S. 
          with the greatest economic consequences")
