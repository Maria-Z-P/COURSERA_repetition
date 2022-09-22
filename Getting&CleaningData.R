setwd("C:/Maja/M/R Programming_Coursera")

#-----
## Manipulating Data with dplyr

library(dplyr)
packageVersion("dplyr")

mydf <- read.csv("C:/Maja/M/R Programming_Coursera/path2csvB.csv", 
                 stringsAsFactors = FALSE)
#dim(mydf)
#mydf 
head(mydf)

cran <- as_tibble(mydf)  

# as_tibble() turns an existing object, such as a data frame or matrix, into 
# a so-called tibble, a data frame with class tbl_df. 
# To avoid confusion and keep things running smoothly, let's remove the original
# data frame from the workspace

rm("mydf")

cran  

# This output is much more informative and compact than what we would get if we 
# printed the original data frame (mydf) to the console

# dplyr supplies five 'verbs' that cover most fundamental data manipulation 
# tasks: select(), filter(), arrange(), mutate(), and summarize()

?select
select(cran, ip_id, package, country)

# The select() function knows we are referring to columns of the cran dataset
# to select all columns starting from r_arch and ending with country:

select(cran, r_arch:country)
select(cran, -time)                    # to omit the time column
select(cran, -(X:size))                # to omit all columns X:size

# to select all rows for which the package variable is equal to "party"

filter(cran, package == "party")
filter(cran, country == "CA" | country == "CN")  # OR
filter(cran, size > 90500, r_os != "linux-gnu")  # AND
is.na(c(3, 5, NA, 10))
!is.na(c(3, 5, NA, 10))

# to return all rows of cran for which r_version is NOT NA

filter(cran, !is.na(r_version))

cran2 <- select(cran, size:ip_id)
cran2

# To order the ROWS of cran2 so that ip_id is in ascending order (from small to 
# large)

arrange(cran2, ip_id)
arrange(cran2, desc(ip_id))

# will first arrange by package names (ascending alphabetically), then by ip_id

arrange(cran2, package, ip_id)

cran3 <- select(cran, ip_id, package, size)
cran3

# One megabyte is equal to 2^20 bytes. That's 2 to the power of 20, which is 
# approximately one million bytes!

mutate(cran3, size_mb = size / 2^20)

# A gigabyte (GB) is equal to 2^10 megabytes

mutate(cran3, size_mb = size / 2^20, size_gb = size_mb / 2^10)
mutate(cran3, correct_size = size + 1000)

summarize(cran, avg_bytes = mean(size))

#-----
## Grouping and Chaining with dplyr

# summarize() is most useful when working with data that has been grouped by the
# values of a particular variable.The idea is that summarize() can give you the 
# requested value FOR EACH group in your dataset.

# The group_by() function is reponsible for breaking up your dataset into groups
# of rows based on the values of one or more variables.

?group_by
by_package <- group_by(cran, package)
by_package
summarize(by_package, mean(size))

# summarize() now returns the mean size for EACH package in our dataset

# Compute four values, in the following order, from
# the grouped data:
#
# 1. count = n()
# 2. unique = n_distinct(ip_id)
# 3. countries = n_distinct(country)
# 4. avg_bytes = mean(size)
#
# A few thing to be careful of:
#
# 1. Separate arguments by commas
# 2. Make sure you have a closing parenthesis
# 3. Check your spelling!
# 4. Store the result in pack_sum (for 'package summary')
#
# You should also take a look at ?n and ?n_distinct, so
# that you really understand what is going on.

pack_sum <- summarize(by_package,
                      count = n(),                     # 1
                      unique = n_distinct(ip_id),      # 2
                      countries = n_distinct(country), # 3
                      avg_bytes = mean(size))          # 4

# 1 - contains the total number of rows (i.e. downloads) for each package
# 2 - gives the total number of unique downloads for each package
# 3 - provides the number of countries in which each package was downloaded
# 4 - contains the mean download size (in bytes) for each package

pack_sum

#-----

View(cran)
by_country <- group_by(cran, country)
summarize(by_country, mean(size))
coun_sum <- summarize(by_country,
                      count = n(),
                      unique = n_distinct(package),
                      r_os_ = n_distinct(r_os),
                      avg_bytes = mean(size))
coun_sum

#-----

## Funkcje z library(dplyr)
# select() - wybor kolumn
# filter() - wybor podzbioru wierszy
# slice() - wybiera wiersze wedlug pozycji
# arrange() - pozwala zmienic kolejnosc wierszy (mozna dodac desc() - porzadek malejacy)
# rename() - zmiana nazw kolumn
# distinct() - zwraca tylko unikalne wartosci w tabeli
# mutate() - dodaje nowe kolumny, bedace funkcjami istniejacych kolumn
# transmutate() - jezeli chcemy tylko nowe kolumny
# summarise() lub summarize() - aby szybko zwinac ramki danych w pojedyncze wiersze
#                               za pomoca funkcji, ktore agreguja wyniki
# (uzycie na.rm = TRUE pozwala usunac wartosci NA)
# sample_n() lub sample_frac() - to take a random sample of rows
## Pipe operator %>%
## Funkcje z library(tidyr)
# gather() - zbieranie
# spread() - rozpowszechnianie
# gather() lub spread() - operacje analogiczne do pivot tables w Excelu
# separate() - przeksztalca kolumne znakow w wiele kolumn
# unite() - laczy wiele kolumn w jedna
## Wszystkie te funkcje omowione w DataManipulation.R (w katalogu 
## R Programming_MachineLearning, Jose Portilla, Udemy) 

#-----

?n   # n() gives the current group size

library(nycflights13)

dim(flights)
head(flights)
my_f <- as_tibble(flights)
head(my_f)
carriers <- group_by(my_f, carrier)
summarise(carriers, n())
mutate(carriers, n = n())
filter(carriers, n() < 100)
?n_distinct # n_distinct {dplyr} counts the number of unique values in a set of vectors
carr_sum <- summarize(carriers,
                      count = n(),
                      unique = n_distinct(flight),
                      destination = n_distinct(dest),
                      avg_distance = mean(distance))
carr_sum

#-----

pack_sum

# Metrics of popularity
# We need to know the value of 'count' that splits the data into the top 1% and 
# bottom 99% of packages based on total downloads. In statistics, this is called
# the 0.99, or 99%, sample quantile:

quantile(pack_sum$count, probs = 0.99)

#-----

quantile(carr_sum$count, probs = 0.80)

# Now we can isolate only those carriers which had more than 48110 total flights

top_counts <- filter(carr_sum, count > 48110)
top_counts
View(top_counts)
top_counts_sorted <- arrange(top_counts,desc(count))
View(top_counts_sorted)
quantile(carr_sum$unique, probs = 0.80)

# Now select all rows corresponding to values of 'unique' that are strictly 
# greater than 600

top_unique <- filter(carr_sum, unique > 600)
top_unique

# Now arrange() top_unique by the 'unique' column, in descending order

top_unique_sorted <- arrange(top_unique,desc(unique))
top_unique_sorted

#-----
#...
# Our final metric of popularity is the number of distinct countries from which 
# each package was downloaded. 
# We'll approach this one a little differently to introduce you to a method 
# called 'chaining' (or 'piping').

?chain

# Chaining allows you to string together multiple function calls in a way that 
# is compact and readable, while still accomplishing the desired result. To make
# it more concrete, let's compute our last popularity metric from scratch,
# starting with our original data.
# You can pronounce the %>% operator as the word 'then'
#-----

result <-
  my_f %>%
  group_by(carrier) %>%
  summarize(count = n(),
            unique = n_distinct(flight),
            destination = n_distinct(dest),
            avg_distance = mean(distance)
  ) %>%
  filter(destination > 40) %>%
  arrange(desc(destination), avg_distance)

# Print result to console

print(result)   

# This script above provides a convenient and concise alternative to the more 
# traditional method, which involves saving results as we go along.
#-----

cran %>%
  select(ip_id, country, package, size) %>%
  mutate(size_mb = size / 2^20) %>%   # 1
  filter(size_mb <= 0.5) %>%
  arrange(desc(size_mb)) %>%          # 2
  print

# 1 - to add a column that contains the size of each download in megabytes
# 2 - to arrange the result by size_mb, in descending order

# The call to print() at the end of the chain is optional, but necessary if you 
# want your results printed to the console. Note that since there are no 
# additional arguments to print(), you can leave off the parentheses after
# the function name. This is a convenient feature of the %>% operator.


##-------------------------------
#-----
## Tidying Data with tidyr

library(tidyr)

# http://vita.had.co.nz/papers/tidy-data.pdf
# (tidy-data.pdf zapisany w folderze: R Programming_Coursera)
# tidy data satisfies three conditions:
# 1) Each variable forms a column
# 2) Each observation forms a row
# 3) Each type of observational unit forms a table

students <- read.csv("C:/Maja/M/R Programming_Coursera/students.csv")

# This dataset actually has three variables: grade, sex, and count. The first 
# variable, grade, is already a column, so that should remain as it is. The 
# second variable, sex, is captured by the second and third column headings. 
# The third variable, count, is the number of students for each combination of 
# grade and sex.

students

# To tidy the students data, we need to have one column for each of these three 
# variables.

?gather
gather(students, sex, count, -grade)

# the minus sign before grade says we want to gather all columns EXCEPT grade

students2 <- read.csv("C:/Maja/M/R Programming_Coursera/students2.csv")
students2
res <- gather(students2, sex_class, count, -grade)
res  

# we still have two different variables, sex and class, stored together in the 
# sex_class column

?separate
separate(data = res, col = sex_class, into = c("sex", "class"))

students3 <- read.csv("C:/Maja/M/R Programming_Coursera/students3.csv")
students3

# The first variable, name, is already a column and should remain as it is. The
# headers of the last five columns, class1 through class5, are all different 
# values of what should be a class variable. The values in the test column, 
# midterm and final, should each be its own variable containing the respective 
# grades for each student.

# Repeat your calls to gather() and separate(), but this time use the %>% 
# operator to chain the commands together without storing an intermediate result

students3 %>%
  gather(key = class, value = grade, class1:class5, na.rm = TRUE)  

# na.rm = TRUE to omit missing values from the final result

?spread     # spread - rozszerzaæ

# spread() will allow us to turn the values of the test column, midterm and 
# final, into column headers (i.e. variables)

students3 %>%
  gather(class, grade, class1:class5, na.rm = TRUE) %>%
  spread(test, grade)

library(readr)
parse_number("class5")  # to change class5 to be simply 5
?parse_number
?mutate
students3 %>%
  gather(class, grade, class1:class5, na.rm = TRUE) %>%
  spread(test, grade) %>%
  mutate(class = parse_number(class)) 

students4 <- read.csv("C:/Maja/M/R Programming_Coursera/students4.csv")
students4   

# Each id, name, and sex is repeated twice, which seems quite redundant. This is 
# a hint that our data contains multiple observational units in a single table.
# Our solution will be to break students4 into two separate tables -- one 
# containing basic student information (id, name, and sex) and the other 
# containing grades (id, class, midterm, final)

student_info <- students4 %>%
  select(id, name, sex) %>%
  print

# Add a call to unique() below, which will remove duplicate rows from 
# student_info

student_info <- students4 %>%
  select(id, name, sex) %>%
  unique %>%
  print

# You can omit the parentheses after the function name. This is a nice feature 
# of %>% that applies when there are no additional arguments to specify 

gradebook <- students4 %>%
  select(id, class, midterm, final) %>%
  print

# It's important to note that we left the id column in both tables. In the world
# of relational databases, 'id' is called our 'primary key' since it allows us 
# to connect each student listed in student_info with their grades listed in 
# gradebook. Without a unique identifier, we might not know how the tables are 
# related.

?bind_rows   # to join two tables together into a single unit
merge(student_info, gradebook)                 # merge two data.tables - po³¹cz

# Exercise

# The SAT is a popular college-readiness exam in the United States that consists
# of three sections: critical reading, mathematics, and writing. Students can 
# earn up to 800 points on each section. This dataset presents the total number
# of students, for each combination of exam section and sex, within each of six 
# score ranges. It comes from the 'Total Group Report 2013', which can be found 
# here: http://research.collegeboard.org/programs/sat/data/cb-seniors-2013

dane <- read.csv("C:/Maja/M/R Programming_Coursera/sat.csv")

# contains data on all college-bound seniors who took the SAT exam in 2013

sat <- as_tibble(dane)
rm("dane")
sat

# Accomplish the following three goals:
#
# 1. select() all columns that do NOT contain the word "total", since if we have
# the male and female data, we can always recreate the total count in a separate
# column, if we want it.
# Hint: Use the contains() function, which you'll find detailed in 'Special 
# functions' section of ?select.
#
# 2. gather() all columns EXCEPT score_range, using key = part_sex and 
# value = count.
#
# 3. separate() part_sex into two separate variables (columns), called "part" 
# and "sex", respectively. You may need to check the 'Examples' section of 
# ?separate to remember how the 'into' argument should be phrased.

?separate

sat %>%
  select(-contains("total")) %>%
  gather(key = part_sex, value = count, -score_range) %>%
  separate(col = part_sex, into = c("part","sex"))

# Append two more function calls to accomplish the following:
#
# 1. Use group_by() (from dplyr) to group the data by part and sex, in that
# order.
# 2. Use mutate to add two new columns, whose values will be automatically 
# computed group-by-group:
#
#   * total = sum(count)
#   * prop = count / total

sat %>%
  select(-contains("total")) %>%
  gather(part_sex, count, -score_range) %>%
  separate(part_sex, c("part", "sex")) %>%
  group_by(part, sex) %>%
  mutate(total = sum(count),
         prop = count / total
  ) %>% print

##-------------------------------
#-----
## Dates and Times with lubridate

library(lubridate)

Sys.getlocale("LC_TIME")    # "Polish_Poland.1250"
help(package = lubridate)

this_day <- today()
this_day
year(this_day)
wday(this_day)                  # 1 = Sunday, 2 = Monday, 3 = Tuesday, etc.
wday(this_day, label = TRUE)    
# to display the *name* of the weekday (represented as an ordered factor)

this_moment <- now()
this_moment
hour(this_moment)

my_date <- ymd("1989-05-17")    # ymd(), dmy(), hms(), ymd_hms(), etc.
my_date
class(my_date)
ymd("1989 May 17")
mdy("March 12, 1975")
dmy(25081985)
ymd("1920/1/2")
ymd_hms("2014-08-23 17:23:02")  # UTC - the Universal Coordinated Time
hms("03:22:14")
ymd(c("2014-05-14", "2014-09-22", "2014-07-11"))

# The update() function allows us to update one or more components of a 
# date-time. For example, let's say the current time is 08:34:55 (hh:mm:ss). 
# Update this_moment to the new time

update(this_moment, hours = 8, minutes = 34, seconds = 55)
this_moment                    # CEST - the Central European Summer Time (UTC+2)
this_moment <- update(this_moment, hours = 8, minutes = 34, seconds = 55)
this_moment

# Pretend you are in New York City and you are planning to visit a friend in 
# Hong Kong. You know that your flight departs New York at 17:34 (5:34pm) the 
# day after tomorrow. Your flight is scheduled to arrive in Hong Kong exactly 
# 15 hours and 50 minutes after departure. Now we will find the current date in 
# New York, adding 2 full days, then setting the time to 17:34

?now
nyc <- now("America/New_York")   # the time zone that we want: "America/New_York"
nyc                              # EDT - Eastern Daylight Time (UTC-4)

# For a complete list of valid time zones for use with lubridate, check out the 
# following Wikipedia page: 
# http://en.wikipedia.org/wiki/List_of_tz_database_time_zones

# One nice aspect of lubridate is that it allows you to use arithmetic operators
# on dates and times

depart <- nyc + days(2)
depart
depart <- update(depart, hours = 17, minutes = 34)
depart

# Now that we have the exact date and time of your departure from New York, we 
# can figure out the exact time of your arrival in Hong Kong

arrive <- depart + hours(15) + minutes(50)
arrive

?with_tz
arrive <- with_tz(arrive, tzone = "Asia/Hong_Kong")
arrive                           # HKT - Hong Kong Time (UTC+8)

# with_tz returns a date-time as it would appear in a different time zone
# Fast forward to your arrival in Hong Kong. You and your friend have just met 
# at the airport and you realize that the last time you were together was in 
# Singapore on June 17, 2008. Naturally, you'd like to know exactly how long it 
# has been

last_time <- mdy("June 17, 2008", tz = "Singapore")
last_time
?interval

# interval() creates an Interval object with the specified start and end dates

how_long <- interval(last_time, arrive)
as.period(how_long)

# This is where things get a little tricky. Because of things like leap years, 
# leap seconds, and daylight savings time, the length of any given minute, day, 
# month, week, or year is relative to when it occurs.
# In contrast, the length of a second is always the same, regardless of when it 
# occurs. To address these complexities, the authors of lubridate introduce four
# classes of time related objects: instants, intervals, durations, and periods. 
# You can find a complete discussion in the 2011 Journal of Statistical Software
# paper titled 'Dates and Times Made Easy with lubridate'.
