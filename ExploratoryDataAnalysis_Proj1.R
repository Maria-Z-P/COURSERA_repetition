setwd("C:/Maja/M/R Programming_Coursera")

p1 <- read.csv2('household_power_consumption.txt')
dim(p1)

#-----------
#library(dplyr)

#proj1 <- tbl_df(p1)
#rm("p1")
#head(proj1)

#my_proj <- filter(proj1, Date == "1/2/2007" | Date == "2/2/2007")
#dim(my_proj)
#head(my_proj)
#tail(my_proj)
#my_proj[1,1:2]
#my_proj[nrow(my_proj),1:2]
#------------

my_proj <- subset(p1, Date == "1/2/2007" | Date == "2/2/2007")
dim(my_proj)
head(my_proj)
tail(my_proj)
my_proj[1,1:2]
my_proj[nrow(my_proj),1:2]
rm("p1")

for (i in 3:9) my_proj[[i]] <- as.numeric(my_proj[[i]])
str(my_proj)
any(is.na(my_proj))
summary(my_proj)

d <- as.Date(my_proj$Date, "%d/%m/%y")
head(d)
class(d)
gap <- my_proj$Global_active_power

#------------
# Plot1
hist(gap, col = "red", xlab = "Global Active Power (kilowatts)", 
     main = "Global Active Power")
dev.copy(png, file = "plot1.png")
dev.off()  
#------------

dates <- my_proj$Date
times <- my_proj$Time
dt <- paste(dates, times)
head(dt)

library(lubridate)

start <- ymd_hms("2007-02-01 00:00:00")
dif <- as.period(interval(start, dmy_hms(dt)))
head(dif)

#------------
# Plot2
plot(x=dif, y = gap, xlab = "", type = "l", xaxt = "n", 
     ylab = "Global Active Power (kilowatts)", main = "")
axis(1, c(1,24*60*60,24*60*60*2), c("Thu","Fri","Sat"))
dev.copy(png, file = "plot2.png")
dev.off() 
#------------

sm1 <- my_proj$Sub_metering_1
sm2 <- my_proj$Sub_metering_2
sm3 <- my_proj$Sub_metering_3

#------------
# Plot3
plot(x=dif, y=sm1, xlab = "", type = "l", xaxt = "n", 
     ylab = "Energy sub metering", main = "")
lines(x=dif, y=sm2, col = "red")
lines(x=dif, y=sm3, col = "blue")
legend("topright", lty=c(1,1,1),col=c("black","red","blue"),
       legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"))
axis(1, c(1,24*60*60,24*60*60*2), c("Thu","Fri","Sat"))
dev.copy(png, file = "plot3.png")
dev.off()
#------------


#------------
# Plot4
par(mfcol=c(2,2), mar = c(4, 4, 1, 1), oma = c(0, 0, 1, 0))

plot(x=dif, y=gap, xlab = "", type = "l", xaxt = "n", 
     ylab = "Global Active Power", cex = 0.5, main = "")
axis(1, c(1,24*60*60,24*60*60*2), c("Thu","Fri","Sat"))

plot(x=dif, y=sm1, xlab = "", type = "l", xaxt = "n", 
     ylab = "Energy sub metering", main = "")
lines(x=dif, y=sm2, col = "red")
lines(x=dif, y=sm3, col = "blue")
legend("topright",lty=c(1,1,1),col=c("black","red","blue"),  cex = 0.7,
       legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"))
axis(1, c(1,24*60*60,24*60*60*2), c("Thu","Fri","Sat"))

plot(x=dif, y=my_proj$Voltage, xlab = "datetime", type = "l", xaxt = "n", 
     ylab = "Voltage", main = "")
axis(1, c(1,24*60*60,24*60*60*2), c("Thu","Fri","Sat"))

plot(x=dif, y=my_proj$Global_reactive_power, xlab = "datetime", type = "l",
     xaxt = "n", ylab = "Global_reactive_power", main = "")
axis(1, c(1,24*60*60,24*60*60*2), c("Thu","Fri","Sat"))

dev.copy(png, file = "plot4.png")
dev.off()
#------------
# https://github.com/Maria-Z-P/ExData_Plotting1.git
