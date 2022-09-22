setwd("C:/Maja/M/R Programming_Coursera/Getting and Cleaning Data")

# Finding data and reading different file types
#-----
# Quiz 1

# q1

URL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
datafile <- basename(URL)
if (!file.exists(datafile)) download.file(URL, datafile)

dt <- read.csv(datafile, stringsAsFactors = FALSE)

# lub:
# dt <- read.csv("getdata%2Fdata%2Fss06hid.csv")

dim(dt)
str(dt)
unique(as.factor(dt$VAL))
dt[1:5, c(1:6, 35:39)]

library(dplyr)

# how many properties are worth $1,000,000 or more?
# (VAL - property value, VAL: 24 -> .$1000000+)

by_VAL <- group_by(dt, dt$VAL)
by_VAL
summarize(by_VAL, count = n())[24,]

# q3

library(readxl)

dat <- read_xlsx("getdata_data_DATA.gov_NGAP.xlsx", range = "G18:O23")
dat
sum(dat$Zip*dat$Ext, na.rm=T)

dat_b <- read_xlsx("data.govngap2019.xlsx", sheet=1, range = "A18:C23")
dat_b

# q4

#getdata_data_restaurants.xml

# q5

URL3 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv"
datafile3 <- basename(URL3)
if (!file.exists(datafile3)) download.file(URL3, datafile3)

DT <- data.table::fread(input = datafile3)

# lub: 
# DT <- data.table::fread(input = "getdata%2Fdata%2Fss06pid.csv")

dim(DT)
class(DT)

#library(data.table)

# calculate the average value of the variable pwgtp15 broken down by sex:

names(DT)
DT[1:5, c(55,174)]

tapply(DT$pwgtp15, DT$SEX, mean)
mean(DT[DT$SEX==1,]$pwgtp15); mean(DT[DT$SEX==2,]$pwgtp15) 
sapply(split(DT$pwgtp15, DT$SEX), mean)

system.time(sapply(split(DT$pwgtp15, DT$SEX), mean))
system.time(mean(DT[DT$SEX==1,]$pwgtp15))

DT[ , mean(pwgtp15), by = SEX]
system.time(DT[ , mean(pwgtp15), by = SEX])

#-----
# Quiz 2

# q2

# dane jak w Quiz1, q5

acs <- data.table::fread(input = "getdata%2Fdata%2Fss06pid.csv")
dim(acs)
names(acs)

#install.packages("sqldf")

library(sqldf)

# select only the data for the probability weight pwgtp1 with ages less than 50

x <- sqldf("select pwgtp1 from acs where AGEP < 50")
class(x)
dim(x)
head(x)

acs_x <- subset(acs, AGEP < 50)
dim(acs_x)
head(acs_x$pwgtp1)

# q3

unique((acs$AGEP))
sqldf("select distinct AGEP from acs")

# q4

y <- url("http://biostat.jhsph.edu/~jleek/contact.html")
htmlCode <- readLines(y)
close(y)
class(htmlCode)
htmlCode[10]

# how many characters are in the 10th, 20th, 30th and 100th lines of HTML

nchar(htmlCode[10])
nchar(htmlCode[20])
nchar(htmlCode[30])
nchar(htmlCode[100])

# q5

# Wczytywanie i zapisywanie plików: https://nowosad.github.io/elp/io.html

?read.fortran

# report the sum of the numbers in the fourth of the nine columns

ff <- read.fwf("getdata_wksst8110.for", widths = 800)
class(ff)
dim(ff)
head(ff)
mff <- ff[5:length(ff$V1), ]
head(mff)
tail(mff)

library(stringr)

nchar(" 04DEC2013     22.0-0.3     ")
mf <- substr(mff, 29, 32)
head(mf)
sum(as.numeric(mf))

#---------------------------------------
# Organizing, merging and managing the data you have collected 
#-----
# Quiz 3

# q1

# dane jak w Quiz1, q1

dt <- read.csv("getdata%2Fdata%2Fss06hid.csv")
dim(dt)

# identify the households on greater than 10 acres who sold more than $10,000 
# worth of agriculture products

# ACR: 3 .House on ten or more acres
# AGS: 6 .$10000+

names(dt)[1:50]
head(dt[, 1:12])
agricultureLogical <- dt$ACR == 3 & dt$AGS == 6
which(agricultureLogical)[1:3]
head(dt[which(agricultureLogical), 1:12], 3)

# q2

library(jpeg)

img <- readJPEG("getdata_jeff.jpg", native=TRUE)
dim(img)
class(img)
img[1:5, 1:5]

# what are the 30th and 80th quantiles of the data?

quantile(img, probs = c(30, 80)/100)

# q3

URL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
datafile <- basename(URL)
if (!file.exists(datafile)) download.file(URL, datafile)

dtGDP <- read.csv(datafile, stringsAsFactors = FALSE)

# lub:
# dtGDP <- read.csv("getdata%2Fdata%2FGDP.csv")

dim(dtGDP)
str(dtGDP)
head(dtGDP)
dtGDP$Gross.domestic.product.2012 <- as.numeric(dtGDP$Gross.domestic.product.2012)
mdtGDP <- dtGDP[which(dtGDP$Gross.domestic.product.2012 > 0), c(1,2,4,5)]
head(mdtGDP)
tail(mdtGDP)

mdtGDP[1:25, ]

URL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv"
datafile <- basename(URL)
if (!file.exists(datafile)) download.file(URL, datafile)

dtE <- read.csv(datafile, stringsAsFactors = FALSE)

# lub:
# dtE <- read.csv("getdata%2Fdata%2FEDSTATS_Country.csv")

dim(dtE)
names(dtE)
head(dtE[,1:3])
dtE$CountryCode

library(dplyr)

mdtGDP <- rename(mdtGDP, CountryCode = X)
head(mdtGDP)

mdtE <- dtE[,1:3]
tail(mdtE)

merged.dt <- merge(mdtGDP, mdtE)               # merge two data.tables - po³¹cz
dim(merged.dt)
head(merged.dt)
mdt <- arrange(merged.dt, desc(Gross.domestic.product.2012))
head(mdt, 15)
tail(mdt)

mdt[165:189, c(3, 4, 6)]

# q4

# what is the average GDP ranking for the "High income: OECD" and "High income: 
# nonOECD" group?

robA <- filter(merged.dt, merged.dt$Income.Group == "High income: OECD")
mean(robA$Gross.domestic.product.2012)
robB <- filter(merged.dt, merged.dt$Income.Group == "High income: nonOECD")
mean(robB$Gross.domestic.product.2012)

table(merged.dt$Income.Group)
robA[ , c(2, 3, 4, 6)]
robB[ , c(2, 3, 4, 6)]

# q5

# Cut the GDP ranking into 5 separate quantile groups. Make a table versus 
# Income.Group. How many countries are Lower middle income but among the 38 nations
# with highest GDP?

rob <- merged.dt$Gross.domestic.product.2012
quantile(rob, probs = seq(0, 1, 0.20))
merged.dt$GDP_groups <- cut(rob, breaks = quantile(rob, probs = seq(0, 1, 0.20)))
table(merged.dt$GDP_groups)
table(merged.dt$GDP_groups, merged.dt$Income.Group)

# ---------
# Text and date manipulation in R
#-----
# Quiz 4

# q1

# dane jak w Quiz1, q1

# Apply strsplit() to split all the names of the data frame on the characters 
# "wgtp". What is the value of the 123 element of the resulting list?

dt <- read.csv("getdata%2Fdata%2Fss06hid.csv")
dim(dt)
names(dt)
rob <- strsplit(names(dt), "wgtp")
rob[[123]]

# q2

# dane jak w Quiz3, q3

# Remove the commas from the GDP numbers in millions of dollars and average them.
# What is the average?

dtGDP <- read.csv("getdata%2Fdata%2FGDP.csv")
head(dtGDP)
dtGDP$X.3 <- gsub(",", "", dtGDP$X.3)
head(dtGDP)
dtGDP$X.3 <- as.numeric(dtGDP$X.3)
View(dtGDP)
mean(dtGDP$X.3[5:194])
mean(dtGDP$X.3[1:194], na.rm = TRUE)

# q3

# which country names begin with United?

grep("^United", dtGDP$X.2)

# q4

# dane jak w Quiz3, q3

dtGDP <- read.csv("getdata%2Fdata%2FGDP.csv")
dtGDP$Gross.domestic.product.2012 <- as.numeric(dtGDP$Gross.domestic.product.2012)
mdtGDP <- dtGDP[which(dtGDP$Gross.domestic.product.2012 > 0), 1:2]

library(dplyr)

mdtGDP <- rename(mdtGDP, CountryCode = X)
head(mdtGDP)

dtE <- read.csv("getdata%2Fdata%2FEDSTATS_Country.csv")
names(dtE)
mdtE <- dtE[, c(1, 2, 11, 22)]
#View(mdtE)
tail(mdtE)

# merged.dt - the matched data based on the country shortcode

merged.dt <- merge(mdtGDP, mdtE)
dim(merged.dt)
head(merged.dt)

# Source of most recent Income and expenditure data - ród³o najnowszych danych 
# o dochodach i wydatkach

# Of the countries for which the end of the fiscal year is available, how many 
# end in June?

grep("/", merged.dt$National.accounts.base.year) # znajduje te wiersze, gdzie jest /
merged.dt[c(2, 16, 29, 51, 55, 75, 78, 80, 124, 130, 131, 133, 148, 169, 175), -5]

grep("-06$", merged.dt$Source.of.most.recent.Income.and.expenditure.data)
# znajduje te wiersze, gdzie jest -06 na koncu
merged.dt[c(89, 187), ]

# q5

#install.packages("quantmod")

library(quantmod)
amzn = getSymbols("AMZN", auto.assign =  FALSE)
sampleTimes = index(amzn)
dim(amzn)
head(amzn)
