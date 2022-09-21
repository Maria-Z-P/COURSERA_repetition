## R Programming - Coursera - Data Science Specialization - swirl

# 1: Basic Building Blocks      2: Workspace and Files     
# 3: Sequences of Numbers       4: Vectors                 
# 5: Missing Values             6: Subsetting Vectors      
# 7: Matrices and Data Frames   8: Logic                   
# 9: Functions                 10: lapply and sapply       
#11: vapply and tapply         12: Looking at Data         
#13: Simulation                14: Dates and Times         
#15: Base Graphics    
 
-------------------------------------
# BASIC BUILDING BLOCKS

x <- 5 + 7  # (x gets 5 plus 7)
x^2         # (x squared)       

# WORKSPACE AND FILES

ls()              # list all the objects in your local workspace 
list.files()      # or dir()   - list all the files in your working directory
?list.files       # the help page
args(list.files)  # to determine the arguments to list.files()

old.dir <- getwd()
dir.create("testdir")
setwd("testdir")
file.create("mytest.R")
list.files()
file.exists("mytest.R")
file.info("mytest.R")
file.rename("mytest.R", "mytest2.R")
file.copy('mytest2.R','mytest3.R')
file.path("folder1","folder2") 

# to construct file and directory paths that are independent of the operating 
# system your R code is running on

?dir.create
dir.create(file.path('testdir2', 'testdir3'), recursive = TRUE)  

# rekursywny - powtarzalny, powtarzajacy sie
# to create a directory in the current working directory called "testdir2" and 
# a subdirectory for it called "testdir3". 
# In order to create nested directories, 'recursive' must be set to TRUE

setwd(old.dir)              # to go back to the original working directory
file.remove("./testdir/mytest2.R")

# SEQUENCES OF NUMBERS

# to access a documentation of an operator : (The backtick (`) key is located 
# in the top left corner of a keyboard, above the Tab key)

?`:`   
seq(1, 20)  # the same as  1:20
seq(0, 10, by = 0.5)
my_seq <- seq(5, 10, length = 30)
length(my_seq)
1:length(my_seq)  # the same as  seq(along.with = my_seq)  or  seq_along(my_seq)
rep(0, times = 20)
rep(c(0, 1, 2), times = 10)
rep(c(0, 1, 2), each = 10)

# MISSING VALUES

# NA - not available or missing value
y <- rnorm(1000)
z <- rep(NA, 1000)
my_data <- sample(c(y, z), 100)
my_na <- is.na(my_data)
my_na   

# a vector, my_na, that has a TRUE for every NA and FALSE for every numeric 
# value R represents TRUE as the number 1 and FALSE as the number 0

sum(my_na)          # to count the total number of TRUEs in my_na
my_data

# NaN - not a number
0/0              # NaN
# Inf - infinity
Inf - Inf        # NaN

# SUBSETTING VECTORS

x <- c(NA, 3, 4, NA, 5)
x[is.na(x)]
y <- x[!is.na(x)]
y[y > 3]
x[!is.na(x) & x > 3]
x[c(2, 3)]                # 3 4
x[10]

# You should always make sure that what you are asking for is within the bounds
# of the vector you're working with.

x[c(-2, -5)]              # NA  4 NA

# or x[-c(2, 5)] gives all elements of x EXCEPT for the 2nd and 5 elements

vect <- c(foo = 11, bar = 2, norf = NA)
vect
names(vect)
vect2 <- c(11, 2, NA)
names(vect2) <- c("foo", "bar", "norf")
vect2
identical(vect, vect2)
vect["bar"]

# MATRICES AND DATA FRAMES

my_vector <- 1:20
length(my_vector)

# to give my_vector 4 rows and 5 columns. Now it's a matrix

dim(my_vector) <- c(4, 5)
attributes(my_vector)
class(my_vector) # a matrix is simply an atomic vector with a dimension attribute
my_matrix <- my_vector
my_matrix2 <- matrix(1:20, 4, 5)
identical(my_matrix, my_matrix2)
patients <- c("Bill", "Gina", "Kelly", "Sean")
cbind(patients, my_matrix)   

# implicit coercion (domniemany przymus), because we didn't ask for: 'coerce' the 
# numbers to characters

my_data <- data.frame(patients, my_matrix)
my_data
class(my_data)
cnames <- c("patient", "age", "weight", "bp", "rating", "test")
colnames(my_data) <- cnames
my_data

# LOGIC

6 == 7    # == the equality operator
5 != 7    # != the 'not equals' operator
!(5 == 7) # ! the NOT operator

# & and && - two AND operators in R. You can use the `&` operator to evaluate 
# AND across a vector. The `&&` version of AND only evaluates the first member 
# of a vector.

TRUE & c(TRUE, FALSE, FALSE)   # TRUE FALSE FALSE
TRUE && c(TRUE, FALSE, FALSE)  # TRUE

# | the vectorized version of the OR operator, || the non-vectorized version of 
# the OR operator

TRUE | c(TRUE, FALSE, FALSE)   # TRUE TRUE TRUE
TRUE || c(TRUE, FALSE, FALSE)  # TRUE
5 > 8 || 6 != 8 && 4 > 3.9     

# TRUE - all AND operators are evaluated before OR operators

isTRUE(6 > 4)        # TRUE
xor(5 == 6, !FALSE)  # TRUE

# The xor() function stands for exclusive OR. If one argument evaluates to TRUE 
# and one argument evaluates to FALSE, then this function will return TRUE,
# otherwise it will return FALSE.

# A random sampling of integers from 1 to 10 without replacement:

ints <- sample(10)  
ints
ints > 5
which(ints > 7)
any(ints < 0)   # FALSE
all(ints > 0)   # TRUE

# FUNCTIONS

# to see the source code for any function, just type the function name without 
# any arguments or parentheses

my_mean <- function(my_vector) {
  sum(my_vector)
  length(my_vector)
  sum(my_vector)/length(my_vector)
}
my_mean
my_mean(c(4, 5, 0))   # 3

remainder <- function(num, divisor = 2) {
  num %% divisor
}
remainder(5)
remainder(divisor = 5, num = 11)
args(remainder)       # to examine the arguments for the remainder function

evaluate <- function(func, dat){
  func(dat)
}        

# passing functions as arguments to other functions is an important and 
# fundamental concept in programming

evaluate(sd, c(1.4, 3.6, 7.9, 8.8))
evaluate(function(x){x[1]}, c(8, 4, 0))           # 1  # wynik: 8
evaluate(function(x){x[length(x)]}, c(8, 4, 0))   # 2

# sd() - function returning the standard deviation of a vector
# 1 - an anonymous function returning the first element of the vector
# 2 - an anonymous function returning the last element of the vector

evaluate(floor, 11.1)      # wynik: 11
evaluate(sum, c(2, 4, 6))  # 12

?paste   # to concatenate (powiazac) vectors after converting to character
telegram <- function(...){
  paste("START", ..., "STOP")
}
telegram(12)   #  "START 12 STOP"

"%p%" <- function(left, right){       # creating user_defined binary operator
  paste(left, right)
}
'I' %p% 'love' %p% 'R!'

# It's recommended making your own binary operator only if you plan on using it 
# often!

# Powyzsze funkcje znajduja sie w katalogu: 
# C:/Maja/M/R Programming_Coursera/Functions

# LAPPLY AND SAPPLY

unique(c(1,3,4,2,4,2))  # 1 3 4 2

# unique() returns a vector of only the 'unique' elements

getwd()
setwd("C:/Maja/M/R Programming_Coursera")

flags <- read.csv("flags.csv")    # http://archive.ics.uci.edu/ml/datasets/Flags
dim(flags)

# to open a more complete description of the dataset in a separate text file

View(flags)
flags[139,]  # Poland
class(flags)
cls_list <- lapply(flags, class)
cls_list

# The powerful lapply() and sapply() functions apply an operation over the 
# elements of a list. Both take a list as input, apply a function to each 
# element of the list, then combine and return the result. 
# lapply() always returns a list, whereas sapply() attempts to simplify the 
# result

class(cls_list)  # to confirm that lapply() returned a list
as.character(cls_list)
cls_vect <- sapply(flags, class)

# to confirm that sapply() simplified the result to a character vector

class(cls_vect)
flag_colors <- flags[, 11:17]
sapply(flag_colors, sum)  

# the first argument is the object over which we are looping (i.e. flag_colors) 
# and the second argument is the name of the function we wish to apply to each 
# column (i.e. sum). Remember that the second argument is just the name of the 
# function with no parentheses. This tells us that of the 194 flags in our
# dataset, 153 contain the color red, 91 contain green, 99 contain blue, and so 
# on.

# range() returns the minimum and maximum of its first argument, which should be
# a numeric vector

flag_shapes <- flags[ , 19:26]
shape_mat <- sapply(flag_shapes, range)
shape_mat   

# Each column of shape_mat gives the minimum (row 1) and maximum (row 2) number
# of times its respective shape appears in different flags

class(shape_mat)                   # to confirm that shape_mat is a matrix
unique_vals <- lapply(flags, unique)  

# returns a list containing one vector of unique values for each column of the 
# flags dataset

sapply(unique_vals, length)  

# to determine the length of each element of unique_vals (i.e. the number of 
# unique values for each variable)
# Occasionally, you may need to apply a function that is not yet defined, thus 
# requiring you to write your own.
# Pretend you are interested in only the second item from each element of the 
# unique_vals list that you just created. 
# Since each element of the unique_vals list is a vector and we're not aware of 
# any built-in function in R that returns the second element of a vector, we 
# will construct our own function:

lapply(unique_vals, function(elem) elem[2])

# VAPPLY AND TAPPLY

?vapply

# Whereas sapply() tries to 'guess' the correct format of the result, vapply() 
# allows you to specify it explicitly. If the result doesn't match the format 
# you specify, vapply() will throw an error, causing the operation to stop.
# This can prevent significant problems in your code that might be caused by 
# getting unexpected return values from sapply().
# If we wish to be explicit about the format of the result we expect, we can use
# vapply(flags, class, character(1)). The 'character(1)' argument tells R that 
# we expect the class function to return a character vector of length 1 when
# applied to EACH column of the flags dataset.
# You might think of vapply() as being 'safer' than sapply(), since it requires 
# you to specify the format of the output in advance, instead of just allowing R
# to 'guess' what you wanted. In addition, vapply() may perform faster than 
# sapply() for large datasets. However, when doing data analysis interactively 
# (at the prompt), sapply() saves you some typing and will often be good enough.

?tapply

# Thanks to tapply() function you can split your data up into groups based on 
# the value of some variable, then apply a function to the members of each group.

tapply(flags$population, flags$landmass, summary)  

# You can see a summary of populations for each of the six landmasses by calling
# tapply() with three arguments: flags$population, flags$landmass, and summary
# landmass: 1=N.America, 2=S.America, 3=Europe, 4=Africa, 4=Asia, 6=Oceania
# For example: the maximum population (in millions) for the fourth landmass 
# group (Africa) is 56.00.

# LOOKING AT DATA

ls()   # to list the variables in your workspace

object.size(flags)  

# to see how much space the dataset flags is occupying in memory

dim(flags)
nrow(flags)  # to see only the number of rows
ncol(flags)
names(flags) # return a character vector of column (i.e. variable) names
table(flags$landmass) 

# Since landmass is a categorical/factor variable, we can see how many times each
# value actually occurs in the data

str(flags) 

# SIMULATION

?sample                         # to generate random numbers
sample(1:6, 4, replace = TRUE)  # to simulate rolling four six-sided dice
LETTERS
sample(LETTERS)              # to permute all 26 letters of the English alphabet

# To simulate 100 flips of an unfair two-sided coin which has a 0.7 probability 
# of landing 'heads', the values 0 for 'tails':

flips <- sample(c(0,1), 100, replace = TRUE, prob = c(0.3, 0.7))
flips
sum(flips)

# Each probability distribution in R has an r*** function (for "random"), a 
# d*** function (for "density"), a p*** (for "probability"), and q*** (for 
# "quantile")

?rbinom
rbinom(1, size = 100, prob = 0.7)   

# only specify the probability of 'success' (heads)

flips2 <- rbinom(n = 100, size = 1, prob = 0.7)   # to see all of the 0s and 1s
flips2
sum(flips2)
?rnorm     

# The standard normal distribution has mean 0 and standard deviation 1
# to generate 10 random numbers from a standard normal distribution:

rnorm(10)
rnorm(10, mean = 100, sd = 25)

# to generate 5 random values from a Poisson distribution with mean 10:

rpois(5, 10)
my_pois <- replicate(100, rpois(5, 10)) # to perform this operation 100 times
my_pois
cm <- colMeans(my_pois)           # to find the mean of each column in my_pois

# to look at the distribution of our column means by plotting a histogram:

hist(cm) 

# DATES AND TIMES

# Dates are represented by the 'Date' class and times are represented by the 
# 'POSIXct' and 'POSIXlt' classes.
# Internally, dates are stored as the number of days since 1970-01-01 and times
# are stored as either the number of seconds since 1970-01-01 (for 'POSIXct') or
# a list of seconds, minutes, hours, etc. (for 'POSIXlt').

d1 <- Sys.Date()   # to get the current date
class(d1)

# to see what d1 looks like internally (wewnetrznie) (that's the exact number of
# days since 1970-01-01)

unclass(d1)
d1
d2 <- as.Date("1969-01-01")
unclass(d2)
t1 <- Sys.time()  # to access the current date and time
t1
class(t1)  

# "POSIXct" "POSIXt" (POSIXt, which just functions as a common language between 
# POSIXct and POSIXlt)

unclass(t1)  # the (large) number of seconds since the beginning of 1970

# Sys.time() returns an object of class POSIXct, but we can coerce the result 
# to POSIXlt

t2 <- as.POSIXlt(Sys.time())
class(t2)  #"POSIXlt" "POSIXt"
t2
unclass(t2)  # a list of values that make up the date and time
str(unclass(t2))
t2$min
weekdays(d1) # weekdays() returns the day of week from any date or time object
months(t1)
quarters(t2)

# strptime() converts character vectors to POSIXlt:

t3 <- "October 17, 1986 08:24"
t4 <- strptime(t3, "%B %d, %Y %H:%M")
t4         # NA
class(t4)  # "POSIXlt" "POSIXt"
Sys.time() > t1
Sys.time() - t1
difftime(Sys.time(), t1, units = 'days')  

# difftime() allows you to specify a 'units' parameter
# If you find yourself working with dates and times often, you may want to check
# out the lubridate package by Hadley Wickham.

# BASE GRAPHICS

data(cars)  # the included data frame cars
?cars
head(cars)  # tail(), dim(), names() and summary() to get a sense of the data
plot(cars)  # 'plot' is short for scatterplot
plot(x = cars$speed, y = cars$dist)  # the same: plot(dist ~ speed, cars)
plot(x = cars$speed, y = cars$dist, xlab = "Speed")
plot(x = cars$speed, y = cars$dist, xlab = "Speed", ylab = "Stopping Distance")

# the same as plot(cars) because
# cars is a simple data frame and the cars data set has only two variables

plot(cars, main = "My Plot")          # a main title: "My Plot"
plot(cars, sub = "My Plot Subtitle")  # a sub title: "My Plot Subtitle"
?par                      # par can be used to set or query graphical parameters
plot(cars, col = 2)       # the plotted points are colored red
plot(cars, xlim = c(10, 15))

# to plot cars while limiting the x-axis to 10 through 15

?points
plot(cars, pch = 2)       # to plot cars using triangles

# Arguments like "col" and "pch" may not seem very intuitive. And that is because
# they aren't! So, many/most people use more modern packages, like ggplot2 or 
# lattice, for creating their graphics in R.

data(mtcars)
head(mtcars)  # tail(), dim(), names() and summary() to get a sense of the data
?boxplot
boxplot(mpg ~ cyl, mtcars)
hist(mtcars$mpg)   

# Like plot(), hist() is best used by just passing in a single vector
