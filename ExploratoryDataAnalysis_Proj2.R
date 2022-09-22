setwd("C:/Maja/M/R Programming_Coursera")

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

NEI$year <- as.factor(NEI$year)
NEI$type <- as.factor(NEI$type)

# The overall goal of this assignment is to explore the National Emissions 
# Inventory database and see what it say about fine particulate matter pollution
# in the United states over the 10-year period 1999–2008. 

# 1. Have total emissions from PM2.5 decreased in the United States from 1999 to
#    2008? Using the base plotting system, make a plot showing the total PM2.5 
#    emission from all sources for each of the years 1999, 2002, 2005, and 2008.

summary(NEI)
head(NEI)
str(NEI)
any(is.na(NEI))
table(NEI$Pollutant)

s <- with(NEI, tapply(Emissions, year, sum))
s
barplot(s, col = "wheat", main = "Total Emissions for each of the years")
dev.copy(png, file = "proj2_plot1.png")
dev.off() 

#---variant2

library(dplyr)

total.em <- summarise(group_by(NEI,year), Emissions=sum(Emissions))
total.em
x1 <- barplot(height=total.em$Emissions/1000, names.arg=total.em$year, 
              xlab="years", ylim=c(0,8000), 
              ylab=expression('totalPM'[2.5]*' emission in kilotons'),
              main=expression('Total PM'[2.5]*' emissions at various years'))
# Add text at top of bars
text(x=x1, y=round(total.em$Emissions/1000,2), 
     label=round(total.em$Emissions/1000,2), pos=3, cex=0.8, col="black")
dev.copy(png, file = "proj2_plot1_v2.png")
dev.off() 

# 2. Have total emissions from PM2.5 decreased in the Baltimore City, Maryland 
#    (fips == "24510") from 1999 to 2008? Use the base plotting system to make a 
#    plot answering this question.

NEI.BC <- subset(NEI, fips == "24510")
s.BC <- with(NEI.BC, tapply(Emissions, year, sum))
s.BC
barplot(s.BC, col = "aliceblue", main = "Total Emissions in the BC for each 
        of the years")
dev.copy(png, file = "proj2_plot2.png")
dev.off()

#---variant2

#library(dplyr)

bc.em <- summarise(group_by(filter(NEI, fips=="24510"), year), 
                   Emissions=sum(Emissions))
bc.em
x2 <- barplot(height=bc.em$Emissions/1000, names.arg=bc.em$year, 
              xlab="years", ylim=c(0,4), col=c("grey30","grey","grey30","grey"),
              ylab=expression('totalPM'[2.5]*' emission in kilotons'),
              main=expression('Total PM'[2.5]*' emissions in Baltimore City'))
# Add text at top of bars
text(x=x2, y=round(bc.em$Emissions/1000,3), 
     label=round(bc.em$Emissions/1000,3), pos=3, cex=0.8, col="black")
dev.copy(png, file = "proj2_plot2_v2.png")
dev.off() 

# 3. Of the four types of sources indicated by the type (point, nonpoint, onroad,
#    nonroad) variable, which of these four sources have seen decreases in 
#    emissions from 1999–2008 for Baltimore City? Which have seen increases in 
#    emissions from 1999–2008? Use the ggplot2 plotting system to make a plot 
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

png(file = "proj2_plot3.png")
g <- ggplot(s.em.BC.t, aes(year, emissions))
g + geom_point(size = 3) + facet_grid(. ~ type)
dev.off() 

# 4. Across the United States, how have emissions from coal combustion-related 
#    sources changed from 1999–2008?

library(stringr)

SCC.coal <- str_detect(SCC$Short.Name, pattern = fixed("Coal"))
str(SCC.coal)
sum(SCC.coal)
SCC.c  <- filter(SCC)[SCC.coal, ]
SCC.c$SCC <- as.character(SCC.c$SCC)
NEI.c  <- NEI[NEI$SCC %in% SCC.c$SCC, ]
s.c <- with(NEI.c, tapply(Emissions, year, sum))
s.c
barplot(s.c, space = 0.8, col = "gray40", density = 80, 
        main = "Emissions from coal combustion-related sourses")
dev.copy(png, file = "proj2_plot4.png")
dev.off() 

# 5. How have emissions from motor vehicle sources changed from 1999–2008 in 
#    Baltimore City?

NEI.BC <- subset(NEI, fips == "24510")

#library(dplyr)
#library(stringr)

SCC.motorVehicle <- str_detect(SCC$Short.Name, pattern = fixed("Motor Vehicle"))
str(SCC.motorVehicle)
sum(SCC.motorVehicle)
SCC.mv  <- filter(SCC)[SCC.motorVehicle, ]
SCC.mv$SCC <- as.character(SCC.mv$SCC)
SCC.mv$SCC

NEI.BC.mv  <- NEI.BC[NEI.BC$SCC %in% SCC.mv$SCC, ]
NEI.BC.mv

#library(ggplot2)

png(file = "proj2_plot5.png")
g <- ggplot(NEI.BC.mv, aes(year, Emissions))
g + geom_point(size = 4) + theme_bw() + ylim(10.15, 10.19) +
    ggtitle("Emissions from motor vehicle sources \n in Baltimore City") + 
    theme(plot.title = element_text(color="blue", size=14, face="bold.italic"))
dev.off() 

#---variant_better

#NEI.BC <- subset(NEI, fips == "24510")

#library(dplyr)
#library(stringr)

SCC.motorVehicle.b1 <- str_detect(SCC$Short.Name, pattern = fixed("Motor Vehicle"))
SCC.motorVehicle.b2 <- str_detect(SCC$EI.Sector, pattern = fixed("Vehicle"))
SCC.motorVehicle.b <- SCC.motorVehicle.b1 | SCC.motorVehicle.b2   # lub
str(SCC.motorVehicle.b)
sum(SCC.motorVehicle.b)
SCC.mv.b  <- filter(SCC)[SCC.motorVehicle.b, ]
SCC.mv.b$SCC <- as.character(SCC.mv.b$SCC)

NEI.BC.mv.b  <- NEI.BC[NEI.BC$SCC %in% SCC.mv.b$SCC, ]
head(NEI.BC.mv.b)
s.c.b <- summarise(group_by(NEI.BC.mv.b, year), Emissions = sum(Emissions))
s.c.b

#library(ggplot2)

png(file = "proj2_plot5_vbetter.png")
g <- ggplot(s.c.b, aes(year, Emissions))
g + geom_point(size = 4) + theme_bw() + ylim(85, 350) +
    ggtitle("Emissions from motor vehicle sources \n in Baltimore City") + 
    theme(plot.title = element_text(color="blue", size=14, face="bold.italic"))
dev.off() 

# 6. Compare emissions from motor vehicle sources in Baltimore City with 
#    emissions from motor vehicle sources in Los Angeles County, California 
#    (fips == "06037"). Which city has seen greater changes over time in motor 
#    vehicle emissions?

NEI.BC <- subset(NEI, fips == "24510")

#library(dplyr)
#library(stringr)

SCC.motorVehicle <- str_detect(SCC$Short.Name, pattern = fixed("Motor Vehicle"))
str(SCC.motorVehicle)
sum(SCC.motorVehicle)
SCC.mv  <- filter(SCC)[SCC.motorVehicle, ]
SCC.mv$SCC <- as.character(SCC.mv$SCC)
SCC.mv$SCC

NEI.BC.mv  <- NEI.BC[NEI.BC$SCC %in% SCC.mv$SCC, ]
NEI.BC.mv

NEI.LA <- subset(NEI, fips == "06037")

NEI.LA.mv  <- NEI.LA[NEI.LA$SCC %in% SCC.mv$SCC, ]
NEI.LA.mv

NEI.BC.LA.mv <- rbind(NEI.BC.mv, NEI.LA.mv)
c <- NEI.BC.LA.mv$fips
C <- str_replace(c, "24510", "BC")
County <- str_replace(C, "06037", "LA")

#library(ggplot2)

png(file = "proj2_plot6.png") 
g <- ggplot(NEI.BC.LA.mv, aes(year, Emissions, col = County))
g + geom_point(size = 4) + theme_bw() + ylim(10, 70) +
    ggtitle("Emissions from motor vehicle sources \n in Baltimore City and Los Angeles")
dev.off() 

#---variant_better

#NEI.BC <- subset(NEI, fips == "24510")

#library(dplyr)
#library(stringr)

SCC.motorVehicle.b1 <- str_detect(SCC$Short.Name, pattern = fixed("Motor Vehicle"))
SCC.motorVehicle.b2 <- str_detect(SCC$EI.Sector, pattern = fixed("Vehicle"))
SCC.motorVehicle.b <- SCC.motorVehicle.b1 | SCC.motorVehicle.b2
str(SCC.motorVehicle.b)
sum(SCC.motorVehicle.b)
SCC.mv.b  <- filter(SCC)[SCC.motorVehicle.b, ]
SCC.mv.b$SCC <- as.character(SCC.mv.b$SCC)

NEI.BC.mv.b  <- NEI.BC[NEI.BC$SCC %in% SCC.mv.b$SCC, ]
head(NEI.BC.mv.b)
s.c.b <- summarise(group_by(NEI.BC.mv.b, year), Emissions = sum(Emissions))
s.c.b$County <- rep("BC",4)
s.c.b

NEI.LA <- subset(NEI, fips == "06037")

NEI.LA.mv.b  <- NEI.LA[NEI.LA$SCC %in% SCC.mv.b$SCC, ]
head(NEI.LA.mv.b)
s.c.LA.b <- summarise(group_by(NEI.LA.mv.b, year), Emissions = sum(Emissions))
s.c.LA.b$County <- rep("LA",4)

NEI.BC.LA.mv.b <- rbind(s.c.b, s.c.LA.b)
NEI.BC.LA.mv.b

#library(ggplot2)

png(file = "proj2_plot6_vbetter.png") 
g <- ggplot(NEI.BC.LA.mv.b, aes(year, Emissions, col = County))
g + geom_point(size = 4) + theme_bw() + ylim(85, 4670) +
    ggtitle("Emissions from motor vehicle sources \n in Baltimore City and Los Angeles")
dev.off() 