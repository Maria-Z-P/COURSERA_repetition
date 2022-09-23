setwd("C:/Maja/M/R Programming_Coursera")

NEI <- readRDS("summarySCC_PM25.rds")
head(NEI)

# 3. Of the four types of sources indicated by the type (point, nonpoint, onroad,
#    nonroad) variable, which of these four sources have seen decreases in 
#    emissions from 1999?2008 for Baltimore City? Which have seen increases in 
#    emissions from 1999?2008? Use the ggplot2 plotting system to make a plot 
#    answer this question.

NEI.BC.p  <- subset(NEI, fips == "24510" & type == "POINT")
NEI.BC.np <- subset(NEI, fips == "24510" & type == "NONPOINT")
NEI.BC.or <- subset(NEI, fips == "24510" & type == "ON-ROAD")
NEI.BC.nr <- subset(NEI, fips == "24510" & type == "NON-ROAD")
rob <- rbind(round(with(NEI.BC.p,  tapply(Emissions, year, sum))),
             round(with(NEI.BC.np, tapply(Emissions, year, sum))),
             round(with(NEI.BC.or, tapply(Emissions, year, sum))),
             round(with(NEI.BC.nr, tapply(Emissions, year, sum))))
rob2 <- c("POINT", "NONPOINT", "ON-ROAD", "NON-ROAD")
s.BC.t <- data.frame(cbind(rob2, rob))
colnames(s.BC.t) <- c("type", "1999", "2002", "2005", "2008")
s.BC.t

library(tidyr)

s.em.BC.t <- gather(s.BC.t, year, emissions, c("1999", "2002", "2005", "2008"))
head(s.em.BC.t)
s.em.BC.t$emissions <- as.numeric(s.em.BC.t$emissions)

library(ggplot2)

#png(file = "proj2_plot3.png")
g <- ggplot(s.em.BC.t, aes(year, emissions))
g + geom_point(size = 3) + facet_grid(. ~ type)
#dev.off() 

# Inaczej (wersja 2)

baltimore_data <- subset(NEI, fips=="24510")
baltimore_type_year <- aggregate(baltimore_data$Emissions, 
                                 by=list(baltimore_data$type, baltimore_data$year),
                                 FUN=sum)
colnames(baltimore_type_year) <- c("Type", "Year", "Emissions")
head(baltimore_type_year)

#library(ggplot2)

qplot(Year, Emissions, data = baltimore_type_year, color=Type, geom="line") + 
  ggtitle("Total Emissions of PM2.5 in Baltimore City by Pollutant Type") + 
  ylab("Total Emissions (tons)") + xlab("Year")

# Inaczej (wersja 3)

library(dplyr)

balcity.emissions.byyear <- summarise(group_by(filter(NEI, fips=="24510"), year,
                                              type), Emissions=sum(Emissions))
head(balcity.emissions.byyear)

ggplot(balcity.emissions.byyear, aes(x=factor(year), y=Emissions, fill=type,
                                     label=round(Emissions, 2))) +
  geom_bar(stat="identity") +
  facet_grid(.~type) + xlab("year") + 
  ylab(expression("total PM"[2.5]*" emission in tones")) +
  ggtitle(expression("PM"[2.5]*paste(" emissions in Baltimore ", 
                                     "City by various source types", sep=""))) + 
  geom_label(aes(fill=type), colour="white", fontface="bold")

#------------------------------------------------------------------
# 4. Across the United States, how have emissions from coal combustion-related 
#    sources changed from 1999?2008?    (combustion - spalanie)

SCC <- readRDS("Source_Classification_Code.rds")
head(SCC, 2)

library(stringr)

SCC.coal <- str_detect(SCC$Short.Name, pattern = fixed("Coal"))
str(SCC.coal)
sum(SCC.coal)

#library(dplyr)

SCC.c  <- filter(SCC)[SCC.coal, ]
SCC.c$SCC <- as.character(SCC.c$SCC)
NEI.c  <- NEI[NEI$SCC %in% SCC.c$SCC, ]
s.c <- with(NEI.c, tapply(Emissions, year, sum))
s.c
barplot(s.c, space = 0.8, col = "gray40", density = 80, 
        main = "Emissions from coal combustion-related sourses")
#dev.copy(png, file = "proj2_plot4.png")
#dev.off() 

# Inaczej (wersja 2)

combustion.coal <- grepl("Fuel Comb.*Coal", SCC$EI.Sector)
combustion.coal.sources <- SCC[combustion.coal, ]

emissions.coal.combustion <- NEI[(NEI$SCC %in% combustion.coal.sources$SCC), ]

#library(dplyr)

emissions.coal.related <- summarise(group_by(emissions.coal.combustion, year),
                                    Emissions=sum(Emissions))
emissions.coal.related

#library(ggplot2)

ggplot(emissions.coal.related, aes(x=factor(year), y=Emissions/1000, fill=year,
                                   label=round(Emissions/1000,2))) +
  geom_bar(stat="identity") +
  xlab("year") + ylab(expression("total PM"[2.5]*" emission in kilotones")) +
  ggtitle("Emissions from coal combustion-related sources in kilotons") + 
  geom_label(aes(fill=year), colour="white", fontface="bold")
