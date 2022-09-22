## Exploratory Data Analysis - Coursera - Data Science Specialization - swirl

# 1: Principles of Analytic Graphs   2: Exploratory Graphs           
# 3: Graphics Devices in R           4: Plotting Systems             
# 5: Base Plotting System            6: Lattice Plotting System      
# 7: Working with Colors             8: GGPlot2 Part1                
# 9: GGPlot2 Part2                  10: GGPlot2 Extras               
#11: Hierarchical Clustering        12: K Means Clustering           
#13: Dimension Reduction            14: Clustering Example           
#15: CaseStudy   

#install.packages("jpeg")
#install.packages("lattice")
#install.packages("ggplot2")
#-----------------------------------------

#-----
#----------
# 1. Principles of Analytic Graphs

# the first principle is to show a comparison /for example by showing the two 
# boxplots side by side, you can clearly see that using the air cleaner increases 
# the number of symptom-free days for most asthmatic children.  The plot on 
# the right (using the air cleaner) is generally higher than the one on the left
# (the control group)/

# the second principle is to show causality or a mechanism of how your theory of
# the data works /Slide_R1 - by showing the two sets of boxplots side by side 
# you're explaining your theory of why the air cleaner increases the number of 
# symptom-free days/

# the third principle: multivariate data (more than 2 variables). Sometimes, if 
# you restrict yourself to two variables you'll be misled and draw an incorrect 
# conclusion /when we divide mortality/pollution data into the four seasons, we
# can see that as pollution increases more people die in all seasons, what makes 
# sense - Slide_R2/

# the fourth principle of analytic graphing involves integrating evidence. This 
# means not limiting yourself to one form of expression. You can use words, 
# numbers, images as well as diagrams. Graphics should make use of many modes 
# of data presentation. Remember, "Don't let the tool drive the analysis!"

# the fifth principle of graphing involves describing and documenting the 
# evidence with sources and appropriate labels and scales. Credibility is 
# important so the data graphics should tell a complete story. Also, using R, 
# you want to preserve any code you use to generate your data and graphics so 
# that the research can be replicated if necessary. This allows for easy 
# verification or finding bugs in your analysis.

# the sixth and final principle of analytic graphing is maybe the most important.
# Content is king! If you don't have something interesting to report, your 
# graphs won't save you. Analytical presentations ultimately stand or fall 
# depending on the quality, relevance, and integrity of their content.

#-----
#----------
# 2. Exploratory Graphs

pollution <- read.csv('C:/Maja/M/R Programming_Coursera/avgpm25.csv')
pollution <- pollution[,-1]
head(pollution)
dim(pollution)
pollution$fips <- as.character(pollution$fips)
pollution$region <- as.factor(pollution$region)
str(pollution)
ppm <- pollution$pm25
summary(ppm)
quantile(ppm)

# one-dimensional plots

boxplot(ppm, col = "blue")
abline(h = 12)  # this command draws a horizontal line at 12

# We see from the plot that the bulk of the measured counties comply with the 
# standard since they fall under the line marking that standard.

hist(ppm, col = "green")
rug(ppm)    

# through density of tick marks we can see that the greatest concentration of 
# counties has between 9 and 12 micrograms per cubic meter just as the histogram
# shows.

hist(ppm, col = "green", breaks = 100) 

# the breaks argument specifies in this case the number of buckets to split the 
# data into

hist(ppm, col = "green", breaks = 50)
abline(v = 12, lwd = 2)
abline(v = median(ppm), col = "magenta", lwd = 4)  

# This shows that although the median (50%) is below the standard, there are a 
# fair number of counties in the U.S that have pollution levels higher than the
# standard.

names(pollution)
reg <- table(pollution$region)
reg

barplot(reg, col = "wheat", main = "Number of Counties in Each Region")

# We use the R formula y ~ x to show that y (in this case pm25) depends on x 
# (region). Since both come from the same data frame (pollution) we can specify 
# a data argument set equal to pollution.

boxplot(pm25~region, data = pollution, col = "red")

par(mfrow=c(2,1), mar=c(4,4,2,1)) 

# to set up the plot window for two rows and one column with the mfrow argument,
# the mar argument set up the margins: the bottom, left, top and right

east <- subset(pollution, region=="east")
head(east)
hist(east$pm25, col = "green")
hist(subset(pollution, region=="west")$pm25, col = "green")

# two-dimensional plots

with(pollution, plot(latitude, pm25))          # a scatterplot

# we call plot with the arguments latitude and pm25 which are both from our data
# frame pollution

abline(h = 12, lwd = 2, lty = 2)

plot(pollution$latitude, ppm, col = pollution$region)
abline(h = 12, lwd = 2, lty =2)

par(mfrow = c(1, 2), mar = c(5, 4, 2, 1))
west <- subset(pollution,region=="west")
plot(west$latitude, west$pm25, main = "West")
plot(east$latitude, east$pm25, main = "East")
par(mfrow = c(1, 1))

# It looks like there are more dirty counties in the east but the extreme dirt 
# (greater than 15) is in the west.

# The following characterizes exploratory plots: quick and dirty.
# Plots let you summarize the data (usually graphically) and highlight any broad
# features.

#-----
#----------
# 3. Graphics Devices in R

# When you make a plot in R, it has to be "sent" to a specific graphics device. 
# Usually this is the screen (the default device), especially when you're doing 
# exploratory work. You'll send your plots to files when you're ready to publish
# a report, make a presentation, or send info to colleagues.

?Devices  # to see what graphics devices are available on your system

?faithful   # faithful- a dataset, which comes with R

# To see the relationship between eruptions of the geyser Old Faithful and 
# waiting time:

with(faithful, plot(eruptions, waiting))  
title(main = "Old Faithful Geyser data")
dev.cur()                     # to show the current plotting device, the screen

# To create the pdf file myplot.pdf in the working directory:

pdf(file="myplot.pdf") 
with(faithful, plot(eruptions, waiting))
title(main = "Old Faithful Geyser data")

# Specific vector formats:
# Pdf format is useful for line-type graphics and papers. It resizes well, is
# usually portable, but it is not efficient if a plot has many objects/points.
# Svg is XML-based, scalable vector graphics. This supports animation and 
# interactivity and is potentially useful for web-based plots.

# 3 bitmap formats:
# Png (Portable Network Graphics) which is good for line drawings or images with
# solid colors. It uses lossless compression (like the old GIF format), and most
# web browsers can read this format natively. In addition, png is good for plots
# with many points, but it does not resize well.
# Jpeg files are good for photographs or natural scenes. They use lossy 
# compression, so they're good for plots with many points. Files in jpeg format 
# don't resize well, but they can be read by almost any computer and any web 
# browser. They're not great for line drawings.
# Bmp is a native Windows bitmapped format.

dev.cur()

# To close the device. Very important! Then you'll be able to view the file:

dev.off()

dev.cur()

with(faithful, plot(eruptions, waiting))
title(main = "Old Faithful Geyser data")
dev.copy(png, file = "geyserplot.png") # to copy a plot from one device to another
dev.off()

#-----
#----------
# 4. Plotting Systems

# The Base Plotting System is a plotting system which comes with R. It's the 
# oldest system which uses a simple "Artist's palette" model. What this means is
# that you start with a blank canvas and build your plot up from there, step by 
# step. A disadvantage of the Base Plotting System is that you can't go back 
# once a plot has started.

# cars - the dataset (recorded in the 1920's) collates the speeds and distances 
# needed to stop for 50 cars

head(cars)
with(cars,plot(speed, dist))
text(mean(x = cars$speed), y = max(cars$dist), "SWIRL rules!")

# The Lattice System is most useful for conditioning types of plots which display
# how y changes with x across levels of z. The variable z might be a categorical
# variable of your data. This system is also good for putting many plots on a 
# screen at once, but it forces you to make your entire plot with one call.

library(lattice)

state  <- read.csv('C:/Maja/M/R Programming_Coursera/state.csv')
head(state)
state$region <- as.factor(state$region)

# To see how many categories there are and how many states are in each:

table(state$region)

xyplot(Life.Exp ~ Income | region, data=state, layout=c(4,1))   

# Life.Exp ~ Income | region - R formula, which indicates  we're plotting life 
# expectancy as it depends on income for each region.
# We see the data for each of the 4 regions plotted in one row.

# The plotting system ggplot2 is a hybrid of the base and lattice systems. It 
# automatically deals with spacing, text, titles (as Lattice does) but also 
# allows you to annotate by "adding" to a plot (as Base does), so it's the best 
# of both worlds. The package is based on a "grammar of graphics" (hence the gg 
# in the name), so you can control the aesthetics of your plots.

library(ggplot2)

head(mpg)
dim(mpg)
table(mpg$model)

# We're interested in the effect engine displacement (displ) has on highway gas 
# mileage (hwy):

qplot(displ, hwy, data=mpg)

#-----
#----------
# 5. Base Plotting System

# airquality shows ozone and other air measurements for New York City for 5 
# months in 1973

head(airquality)

range(airquality$Ozone, na.rm=TRUE)
hist(airquality$Ozone)

table(airquality$Month)
boxplot(Ozone~Month, airquality)
boxplot(Ozone~Month, airquality, xlab = "Month", ylab = "Ozone (ppb)", 
        col.axis = "blue", col.lab = "red")
title(main = "Ozone in New York City")

with(airquality, plot(Wind, Ozone))    # a two-dimensional scatterplot
title(main = "Ozone and Wind in New York City")

length(par())   # a lot of basic plotting parameters
names(par())
?par
par()$pin    # plot dimensions in inches
par("fg")    # or par()$fg - it specifies foreground color
par("pch")   # to see the integer value of the default plot character; 
             # 1 is the code for the open circle
par("lty")   # to see the default line type
colors()

# Use dev.off or plot.new to reset to the defaults

plot(airquality$Wind, airquality$Ozone, type = "n")  

# type="n" tells R to set up the plot but not to put the data in it

title(main = "Wind and Ozone in NYC")
may <- subset(airquality, Month == 5)
points(may$Wind, may$Ozone, col = "blue", pch = 17)
notmay <- subset(airquality, Month != 5)
points(notmay$Wind, notmay$Ozone, col = "red", pch = 8)
legend("topright",pch=c(17,8),col=c("blue","red"),legend=c("May","Other Months"))
abline(v=median(airquality$Wind), lty=2, lwd=2)  

# to add a vertical line at the median of airquality$Wind. It will be dashed 
# (lty=2) with a width of 2 (lwd=2).

par(mfrow=c(1,2))   # to set up the plot window for two plots side by side. You 
                    # won't see a result.

plot(airquality$Wind, airquality$Ozone, main = "Ozone and Wind")
plot(airquality$Ozone, airquality$Solar.R, main = "Ozone and Solar Radiation")

par(mfrow = c(1, 3), mar = c(4, 4, 2, 1), oma = c(0, 0, 2, 0))

# The numbers are assigned clockwise starting at the bottom. The default for the 
# inner margin is c(5.1, 4.1, 4.1, 2.1) so you can see we reduced each of these 
# so we'll have room for some outer text.

plot(airquality$Wind, airquality$Ozone, main="Ozone and Wind")
plot(airquality$Solar.R, airquality$Ozone, main = "Ozone and Solar Radiation")
plot(airquality$Temp, airquality$Ozone, main = "Ozone and Temperature")
mtext("Ozone and Weather in New York City", outer=TRUE)       # the main title

dev.off()

# Review:
# When we use R's base plotting system, we can't easily go back once the plot has
# started (e.g., to adjust margins or correct a typo).

#-----
#----------
# 6. Lattice Plotting System

# xyplot produces a scatterplot, bwplot produces box-and-whiskers plots or 
# boxplots, histogram produces histograms.
# Unlike base plotting, the lattice system does not have a "two-phase" aspect 
# with separate plotting and annotation. Instead all plotting and annotation is
# done at once with a single function call.
# Lattice functions generally take a formula for their first argument, usually 
# of the form y ~ x. This indicates that y depends on x.
# Here's an example of typical lattice plot call, xyplot(y ~ x | f * g, data). 
# The f and g represent the optional conditioning variables. The * represents 
# interaction between them. Remember when we said that lattice is good for 
# plotting multivariate data? That's where these conditioning variables come 
# into play. The second argument is the data frame or list from which the 
# variables in the formula should be looked up. If no data frame or list is 
# passed, then the parent frame is used. If no other arguments are passed, the 
# default values are used.

head(airquality)
xyplot(Ozone~Wind, data = airquality)
xyplot(Ozone ~ Wind, data = airquality, pch=8, col="red", main="Big Apple Data")
xyplot(Ozone ~ Wind | as.factor(Month), data = airquality, layout = c(5,1))  

# as.factor(Month) displays the data by individual months and labels each 
# subplot with the month's integer

# Lattice functions behave differently from base graphics functions in one 
# critical way. Recall that base graphics functions plot data directly to the 
# graphics device (e.g., screen, or file such as a PDF file). In contrast,
# lattice graphics functions return an object of class trellis. (trellis - krata)

p <- xyplot(Ozone~Wind, data=airquality)

class(p)
p
names(p)   # We see that the trellis object p has 45 named properties
mynames <- c( "formula","as.table","aspect.fill","legend","panel","page",
              "layout","skip","strip","strip.left","xscale.components",
              "yscale.components","axis","xlab","ylab","xlab.default",
              "ylab.default","xlab.top","ylab.right","main","sub","x.between",
              "y.between","par.settings","plot.args","lattice.options",
              "par.strip.text","index.cond","perm.cond","condlevels","call",
              "x.scales","y.scales","panel.args.common","panel.args",
              "packet.sizes","x.limits","y.limits","x.used.at","y.used.at",
              "x.num.limit","y.num.limit","aspect.ratio","prepanel.default",
              "prepanel")

# a boolean vector, which has TRUE values for nonnull entries of p

myfull <- c(TRUE,TRUE,TRUE,FALSE,TRUE,FALSE,FALSE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,
            TRUE,TRUE,TRUE,TRUE,FALSE,FALSE,FALSE,FALSE,TRUE,TRUE,FALSE,FALSE,
            FALSE,FALSE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,
            FALSE,FALSE,FALSE,FALSE,TRUE,TRUE,FALSE)
mynames[myfull]                    # to see which entries of p are not NULL

# Wow! 29 nonNull values for one little plot.

p[["formula"]]
p[["x.limits"]]

# The ease of making multi-panel plots makes lattice very appealing. The lattice
# package comes with default panel functions, but you can customize what happens
# in each panel.

x <- c(0.01874617,-0.18425254,-1.37133055,-0.59916772,0.29454513,0.38979430,
       -1.20807618,-0.36367602,-1.62667268,-0.25647839,1.10177950,0.75578151,
       -0.23823356,0.98744470,0.74139013,0.08934727,-0.95494386,-0.19515038,
       0.92552126,0.48297852,-0.59631064,-2.18528684,-0.67486594,-2.11906119,
       -1.26519802,-0.37366156,-0.68755543,-0.87215883,-0.10176101,-0.25378053,
       -1.85374045,-0.07794607,0.96856634,0.18492596,-1.37994358,-1.43551436,
       0.36208723,-1.75908675,-0.32454401,-0.65156299,1.08655140,-0.76254488,
       -0.82866254,0.83447390,-0.96765199,-0.02881534,0.23252515,-0.30120868,
       -0.67761458,0.65522764,-0.40063755,-0.33455657,1.36795395,2.13776710,
       0.50581926,0.78634238,-0.90221194,0.53289699,-0.64589425,0.29098749,
       -1.23759447,-0.45617628,-0.83032265,0.34011564,1.06637640,1.21612584,
       0.73569066,-0.48120862,0.56274476,-1.24631971,0.38092221,-1.43042725,
       -1.04844550,-0.21850355,-1.48993624,1.17270628,-1.47982702,-0.43038782,
       -1.05163864,1.52258634,0.59282805,-0.22266151,0.71289428,0.71660083,
       0.44024186,0.15883062,0.65976414,2.22051966,-1.18394507,-0.07395583,
       -0.41635467,-0.19148234,0.06954478,1.15534832,0.59495735,-1.41964511,
       -1.60667725,0.89292590,0.14816796,1.22702839)
y <- c(-0.362155999,0.025435161,-1.891302232,-0.243380733,-0.022061381,
       0.671381633,-0.877582833,-1.192701446,-1.112588693,0.307498413,
       0.461702201,1.320215622,-0.470300820,0.829564599,1.203536702,0.127919628,
       -0.434982054,0.175792719,1.553293691,0.958438008,-0.836993440,
       -2.083845949,-0.690735810,-2.716851342,-0.953357403,-0.831063797,
       -0.563176427,-1.403470223,-0.283752130,-0.857277957,-1.139134064,
       0.238771879,-0.029841468,-0.155990127,-1.609971318,-1.927048959,
       0.609753085,-1.396178004,0.009105356,-0.174169770, 0.248885310,
       -1.365137576,-1.810288780,1.569850058,-0.781415817,0.504124332,
       0.497850087,-0.250216959,-0.008723350,0.698845020,0.804447896,0.875066258,
       1.577552373,0.567636380,0.566660829,-0.160508515,1.304415084,1.575003024,
       0.400201164,0.209999623,1.326583097,0.725295757,1.260527263,0.650298467,
       0.780545343,0.661340352,1.479570597,0.265913335,1.091881946,0.282426409,
       0.431300051,0.792677337,1.071967144,1.531012165,0.714603049,1.638590688,
       1.114144660,0.845593468,1.479914565,1.274411187,1.212756547,1.321750018,
       0.319846928,0.900746947,1.309651338,2.034104803,0.847357623,1.140622806,
       1.345658668,1.023180719,1.056514681,1.497665937,0.659424319,0.361471377,
       0.265651125,0.843262967,0.148170254,0.324742672,0.448953161,0.450228493)

# Two 100-long vectors, x and y. For its first 50 values y is a function of x, 
# for the last 50 values, y is random.

f <- as.factor(c(rep("Group 1", 50), rep("Group 2", 50)))
table(f)   #the first 50 entries of f are "Group 1" and the last 50 are "Group 2"
xyplot(y ~ x | f, layout = c(2, 1))
v1 <- c(-2.185287, 1.101780, -2.716851,  1.569850)  

# The first two numbers are the range of the x values of Group 1 and the last
# two numbers are the range of y values of Group 1. See how they match the values
# of the left panel (Group 1) in the plot.

range(x[1:50])
range(y[1:50])
v2 <- c(-1.6066772,  2.2205197, -0.1605085,  2.0341048)
range(x[51:100])
range(y[51:100])

# The values match the plot. That's reassuring. (reassuring - uspokajaj¹cy)

p <- xyplot(y ~ x | f, panel = function(x, y, ...) {
  panel.xyplot(x, y, ...) ## First call the default panel function for 'xyplot'
  panel.abline(h = median(y), lty = 2)  ## Add a horizontal line at the median
})

# The panel function has 3 arguments, x, y and ... . This last stands for all 
# other arguments (such as graphical parameters) you might want to include. 
# There are 2 lines in the panel function. Each invokes a panel method, the first
# to plot the data in each panel (panel.xyplot), the second to draw a horizontal
# line in each panel (panel.abline). Note the similarity of this last call to 
# that of the base plotting function of the same name.

print(p)        # The plot shows two panels because f contains 2 factors

#invisible()
p2 <- xyplot(y ~ x | f, panel = function(x, y, ...) {
  panel.xyplot(x, y, ...)      ## First call default panel function
  panel.lmline(x, y, col = 2)  ## Overlay a simple linear regression line
})
print(p2)

# The regression lines are red because the custom panel function specified a col
# argument

#invisible()

# Some diamond data, which comes with the ggplot2 package

# diamonds - the data frame, which contains 10 pieces of information for each of
# 53940 diamonds

head(diamonds)
str(diamonds)
table(diamonds$color)
table(diamonds$color, diamonds$cut)  

# We see a 7 by 5 array with counts indicating how many diamonds in the data 
# frame have a particular color and cut

myxlab <- "Carat"
myylab <- "Price"
mymain <- "Diamonds are Sparkly!"
xyplot(price~carat | color*cut, data = diamonds, strip = FALSE, pch = 20, 
       xlab = myxlab, ylab = myylab, main = mymain)

# 35 panels, one for each combination of color and cut. The dots (pch=20) show 
# how prices for the diamonds in each category (panel) vary depending on carat:
# colors are defining the columns of the plot the strip argument labels each 
# panel and has a default value of TRUE:

xyplot(price~carat | color*cut, data = diamonds, pch = 20, xlab = myxlab, 
       ylab = myylab, main = mymain)

# Review:
# 1. Lattice plots are NOT constructed by a series of calls to core functions.
# 2. Lattice plots are constructed with a single function call to a core lattice
#    function (e.g. xyplot)
# 3. Aspects like margins and spacing are automatically handled and defaults are
#    usually sufficient.
# 4. The lattice system is ideal for creating conditioning plots where you 
#    examine the same kind of plot under many different conditions.
# 5. Lattice DOES return a trellis plot object but base plotting system doesn't.
# 6. Panel functions can be customized to modify what is plotted in each of the 
#    plot panels.
# 7. Lattice plots can display a great many panels in a single plot. (The sky's 
#    the limit.)

#-----
#----------
# 7. Working with Colors

# Color choice is secondary to your data and how you analyze it, but effectively 
# using colors can enhance your plots and presentations, emphasizing the 
# important points you're trying to convey.

# We'll first discuss some functions that the grDevices package offers. The 
# function colors() lists the names of 657 predefined colors you can use in any 
# plotting function.  These names are returned as strings:

sample(colors(), 10)

# Two additional functions from grDevices, colorRamp and colorRampPalette, give 
# you more options. Both of these take color names as arguments and use them as
# "palettes", that is, these argument colors are blended in different proportion
# to form new colors.

# The first, colorRamp, takes a palette of colors (the arguments) and returns a 
# function that takes values between 0 and 1 as arguments. The 0 and 1 correspond
# to the extremes of the color palette. Arguments between 0 and 1 return blends
# of these extremes:

pal <- colorRamp(c("red", "blue"))
pal(0)   # the vector (255,0,0) contains only red (no green or blue), and moreover,
         # it's the highest possible value of red.

# This 3 long vector corresponds to red, green, blue (RGB) color encoding 
# commonly used in televisions and monitors. In R, 24 bits are used to represent
# colors. Think of these 24 bits as 3 sets of 8 bits, each of which represents an
# intensity for one of the colors red, green, and blue.

pal(1)   # the vector (0, 0, 255)
pal(.5)  # the vector (127.5, 0, 127.5)

# The function pal can take more than one argument. It returns one 3-long 
# (or 4-long) vector for each argument:

pal(seq(0, 1, len=6))

# In this example none of pal's outputs will ever contain green since it wasn't 
# in our initial palette.

# colorRampPalette is a function similar to colorRamp. It also takes a palette of
# colors and returns a function. This function, however, takes integer argument
# (instead of numbers between 0 and 1) and returns a vector of colors each of
# which is a blend of colors of the original palette:

p1 <- colorRampPalette(c("red", "blue"))
p1(2) 

# We see a 2-long vector is returned. The first entry FF0000 represents red. The
# FF is hexadecimal for 255, the same value returned by our call pal(0). The 
# second entry 0000FF represents blue, also with intensity 255.

p1(6) 

# Now we get the 6-long vector (FF0000, CC0033, 990066, 650099, 3200CC, 0000FF).
# We see the two ends (FF0000 and 0000FF) are consistent with the colors red and
# blue.

0xcc    # or 0xCC - to see the decimal equivalent of this hex number
# 0xCC equals 204 and we can easily convert hex 33 to decimal, as in 
# 0x33 = 3*16 + 3 = 51.

# We can also form palettes using colors other than red, green and blue:

p2 <- colorRampPalette(c("red", "yellow"))
p2(2) 

# The first color we see is FF0000, which we know represents red. The second
# color returned, FFFF00, must represent yellow, a combination of full intensity
# red and full intensity green.

p2(10)  

# This shows us how the two extremes, red and yellow, are blended together.
# We see the 10-long vector. For each element, the red component is fixed at FF,
# and the green component grows from 00 (at the first element) to FF (at the last).

?rgb
# rgb is a color specification function that can be used to produce any color 
# with red, green, blue proportions.
# We see the maxColorValue is 1 by default, so if we called rgb with values for 
# red, green and blue, we would specify numbers at most 1 (assuming we didn't 
# change the default for maxColorValue).
# The maximum number of arguments rgb can have is 6. The fourth argument is alpha
# which can be a logical, i.e., either TRUE or FALSE, or a numerical value.

p3 <- colorRampPalette(c("blue","green"), alpha = .5)
p3(5) 

# In the 5-long vector that the call returned, each element has 32 bits, 4 groups
# of 8 bits each. The last 8 bits represent the value of alpha. Since it was NOT
# ZERO in the call to colorRampPalette, it gets the maximum FF value. (The same 
# result would happen if alpha had been set to TRUE.) When it was 0 or FALSE (as
# in previous calls to colorRampPalette) it was given the value 00 and wasn't
# shown. The leftmost 24 bits of each element are the same RGB encoding we 
# previously saw. Alpha represents an opacity level, that is, how transparent 
# should the colors be. We can add color transparency with the alpha parameter 
# to calls to rgb.  (opacity - nieprzezroczystoœæ)

rgb(0,.5,.5)
x <- rnorm(1000)
y <- rnorm(1000)
plot(x, y, pch = 19, col = rgb(0, .5, .5))  # pch = 19 - filled circles
plot(x, y, pch = 19, col = rgb(0, .5, .5, alpha = .3)) # to see more information 
                            # in the center portion, where the points are thick.
# It shows where, specifically, the densest areas of the scatterplot really are.

#install.packages("RColorBrewer")

library(RColorBrewer)

display.brewer.all()
# http://www.sthda.com/english/wiki/colors-in-r

# The RColorBrewer Package contains interesting and useful color palettes, of 
# which there are 3 types:
# sequential, divergent, and qualitative (sekwencyjne, rozbie¿ne i jakoœciowe).
# In the sequential palettes the colors are ordered from light to dark. In the 
# divergent palettes the neutral color (white) is in the center, and as you move
# from the middle to the two ends of each palette, the colors increase in 
# intensity. The qualitative palettes look like collections of random colors. 
# These might be used to distinguish factors in your data.
# These colorBrewer palettes can be used in conjunction with the colorRamp() and
# colorRampPalette() functions.
# You would use colors from a colorBrewer palette as your base palette, i.e., as
# arguments to colorRamp or colorRampPalette which would interpolate them to 
# create new colors.

cols <- brewer.pal(3, "BuGn")  # 3 colors, mixes of blue and green

# The string "BuGn" is the second last palette in the sequential display. The 3
# tells the function how many different colors we want.

cols
display.brewer.pal(n = 3, name = 'BuGn')

pal <- colorRampPalette(cols)
pal(20)

head(volcano)  # a matrix volcano (Auckland's Maunga Whau Volcano) is included 
               # in the package datasets
image(volcano) # to display topographic information on this volvano
image(volcano, col = pal(20))

# Review:
# 1. Careful use of colors in plots/maps/etc. can make it easier for the reader 
# to understand what points you're trying to convey. 
# 2. R package RColorBrewer provides color palettes for sequential, categorical,
# and diverging data.
# 3. The colorRamp and colorRampPalette functions can be used in conjunction 
# with color palettes to connect data to colors.
# 4. Transparency can be used to clarify plots with many points.
# 5. Transparency is determined by alpha parameter of the rgb function.
# 6. The function colors returns 657 colors.

#-----
#----------
# 8. GGPlot2

# The ggplot2 package is an implementation of The Grammar of Graphics, an 
# abstract concept (as well as book) authored and invented by Leland Wilkinson 
# and implemented by Hadley Wickham while he was a graduate student at Iowa State.
# A grammar of graphics represents an abstraction of graphics, that is, a theory
# of graphics which conceptualizes basic pieces from which you can build new 
# graphics and graphical objects. The goal of the grammar is to “Shorten the 
# distance from mind to page”.

# The ggplot2 package "is composed of a set of independent components that can 
# be composed in many different ways. These components include aesthetics which 
# are attributes such as colour, shape, and size, and geometric objects or geoms
# such as points, lines, and bars.
# ggplot2 combines the best of base and lattice. It allows for multipanel 
# (conditioning) plots (as lattice does) but also post facto annotation (as base 
# does), so you can add titles and labels. It uses the low-level grid package 
# (which comes with R) to draw the graphics.

# The ggplot2 package has 2 workhorse functions. 

# The more basic workhorse function is qplot, (think quick plot), which works 
# like the plot function in the base graphics system. It can produce many types 
# of plots (scatter, histograms, box and whisker) while hiding tedious details 
# from the user. Similar to lattice functions, it looks for data in a data frame
# or parent environment.

# The more advanced workhorse function in the package is ggplot, which is more 
# flexible and can be customized for doing things qplot cannot do.

str(mpg)             # mpg - a data frame, which comes with the ggplot2 package
qplot(displ, hwy, data = mpg)

# front-wheel, rear-wheel, and 4-wheel - the 3 factors (subsets) of different 
# types of drive (drv)

qplot(displ, hwy, data = mpg, color = drv)
qplot(displ, hwy, data = mpg, color = drv, geom = c("point", "smooth"))

# Notice the gray areas surrounding each trend lines. These indicate the 95% 
# confidence intervals for the lines.

qplot(y=hwy, data = mpg, color = drv)   

# each point in the plot represents one of the hwy values (indicated by the 
# y-axis)

myhigh <- mpg$hwy
myhigh   

# specifying the y parameter only, without an x argument, plots the values of 
# the y argument in the order in which they occur in the data

qplot(drv, hwy, data = mpg, geom = "boxplot")
qplot(drv, hwy, data = mpg, geom = "boxplot", color = manufacturer)

# There are still 3 regions of the plot (determined by the factor drv). Each is 
# subdivided into several boxes depicting different manufacturers.

# Histograms display frequency counts for a single variable.

qplot(hwy, data = mpg, fill = drv)

# The color lets us see right away that 4-wheel drive vehicles in this dataset 
# don't have gas mileages exceeding 30 miles per gallon.

# Some people may find this multi-color histogram hard to interpret. Instead of 
# using colors to distinguish between the drive factors let's use facets or 
# panels. (That's what lattice called them.) This just means we'll split the 
# data into 3 subsets (according to drive) and make 3 smaller individual plots 
# of each subset in one plot.

# A scatterplot (with 3 facets)

qplot(displ, hwy, data = mpg, facets = . ~ drv)  

# number of rows (to the left of the ~) and number of columns (to the right of 
# the ~)
# This shows us more detailed information than the histogram. We see the 
# relationship between displacement and highway mileage for each of the 3 drive
# factors.

# A histogram (with 3 facets)

qplot(hwy, data = mpg, facets = drv ~ ., binwidth = 2)

# There are 7 basic components of ggplot2 plot:
# 1. There's a DATA FRAME which contains the data you're trying to plot. 
# 2. Then the AESTHETIC MAPPINGS determine how data are mapped to color, size, 
#    etc. 
# 3. The GEOMS (geometric objects) are what you see in the plot (points, lines, 
#    shapes)
# 4. FACETS are the panels used in conditional plots
# 5. A COORDINATE SYSTEM
# 6. STATS
# 7. SCALES

# As in the base plotting system (and in contrast to the lattice system), when 
# building plots with ggplot2, the plots are built up in layers, maybe in several
# steps. You can plot the data, then overlay a summary (for instance, a 
# regression line or smoother) and then add any metadata and annotations you need.

qplot(displ, hwy, data=mpg, geom=c("point","smooth"), facets=.~drv)

g <- ggplot(mpg, aes(displ, hwy) )
summary(g)
g + geom_point()            # by calling the function geom_point we add a layer
g + geom_point() + geom_smooth()  # the gray shadow around the blue line is the 
                                  # confidence band
g + geom_point() + geom_smooth(method="lm")
g + geom_point() + geom_smooth(method="lm") + facet_grid(. ~ drv)
g + geom_point() + geom_smooth(method="lm") + facet_grid(. ~ drv) + 
  ggtitle("Swirl Rules!")

# The function theme() can be used to modify aspects of the entire plot, e.g. 
# the position of the legend. Two standard appearance themes are included in 
# ggplot. These are theme_gray() which is the default theme (gray background 
# with white grid lines) and theme_bw() which is a plainer (black and white) 
# color scheme.

g + geom_point(color="pink", size=4, alpha=1/2) 

# The alpha aesthetic tells ggplot how transparent the points should be.
# Darker circles indicate values hit by multiple data points.

g + geom_point(size=4, alpha=1/2, aes(color=drv))

# You MUST use the function aes since the color of the points is data dependent 
# and not a constant as it was in the previous example.

g + geom_point(aes(color = drv)) + labs(title="Swirl Rules!") + 
  labs(x = "Displacement", y = "Hwy Mileage")
g + geom_point(aes(color=drv), size=2, alpha=1/2) + 
  geom_smooth(size=4, linetype=3, method="lm", se=FALSE)

# linetype = 3 -> line is dashed (not continuous)
# se = FALSE   -> the gray shadows indicating standard errors (confidence  
#                 intervals) are turned off

g + geom_point(aes(color = drv)) + theme_bw(base_family="Times")

#---  
testdat <- data.frame(x = 1:100, y = rnorm(100))
testdat[50,2] <- 100    # Outlier!
plot(testdat$x, testdat$y, type = "l", ylim = c(-3,3))

# The outlier at (50,100) is NOT shown on the line plot.

g <- ggplot(testdat, aes(x = x, y = y))
g + geom_line()
g + geom_line() + ylim(-3,3)   

# There's a break in the line which isn't very noticeable.

g + geom_line() + coord_cartesian(ylim = c(-3,3))

# The outlier y value at x=50 is not shown, but the plot indicates that it is
# larger than 3.
#---

# A more complicated example:

g <- ggplot(mpg, aes(x=displ, y=hwy, color=factor(year)))
g + geom_point()

g + geom_point() + facet_grid(drv~cyl, margins =TRUE)

# A 4 by 5 plot. The margins argument tells ggplot to display the marginal
# totals over each row and column, so instead of seeing 3 rows (the number of 
# drv factors) and 4 columns (the number of cyl factors) we see a 4 by 5 display.
# Note that the panel in position (4,5) is a tiny version of the scatterplot of 
# the entire dataset.

g + geom_point() + facet_grid(drv~cyl, margins =TRUE) + 
  geom_smooth(method = "lm", se = FALSE, size = 2, color = "black")

g + geom_point() + facet_grid(drv~cyl, margins =TRUE) + 
  geom_smooth(method = "lm", se = FALSE, size = 2, color = "black") + 
  labs(x = "Displacement", y = "Highway Mileage", title = "Swirl Rules!")

#---

# An example with diamond data which comes with the ggplot2 package:
str(diamonds)

# A histogram with the binwidth defaulting to range/30:

qplot(price, data = diamonds)

# Range refers to the spread or dispersion of the data, in this case price of 
# diamonds (dispersion - rozproszenie)

range(diamonds$price) 

# the range (difference between the minimum and maximum prices) is $18497

qplot(price, data = diamonds, binwidth = 18497/30)  # 18497/30 = 617

# the height of each bin tells you how many diamonds have a price between x and 
# x+617 where x is the left edge of the bin

brk <- rep(0, 31)
for (i in 1:31) { brk[i+1] = brk[i] + 617 }
brk

# If you create a vector named counts containing the number of diamonds with 
# prices between each pair of adjacent entries of brk (for instance, the first 
# count is the number of diamonds with prices between 0 and $617, and the second
# is the number of diamonds with prices between $617 and $1234), you can see 
# that this vector matches the plotted histogram.

qplot(price, data = diamonds, binwidth = 18497/30, fill = cut)

# Notice how qplot displays distributions relative to the cut legend on the right.
# The fair cut diamonds are at the bottom of each bin, the good cuts are above 
# them, then the very good above them, until the ideal cuts are at the top of 
# each bin. You can quickly see from this display that there are very few fair 
# cut diamonds priced above $5000.

# We'll replot the histogram as a density function which will show the proportion 
# of diamonds in each bin. This means that the shape will be similar but the 
# scale on the y-axis will be different since, by definition, the density 
# function is nonnegative everywhere, and the area under the curve is one.

qplot(price, data = diamonds, geom = "density")

# Notice that the shape is similar to that of the histogram we saw previously. 
# The highest peak is close to 0 on the x-axis meaning that most of the diamonds
# in the dataset were inexpensive. In general, as prices increase (move right 
# along the x-axis) the number of diamonds (at those prices) decrease. The
# exception to this is when the price is around $4000; there's a slight increase
# in frequency. Let's see if cut is responsible for this increase.

qplot(price, data = diamonds, geom = "density", color = cut)

# Four of the five cuts have 2 peaks, one at price $1000 and the other between 
# $4000 and $5000. The exception is the Fair cut which has a single peak at 
# $2500. This gives us a little more understanding of the histogram we saw before.

# Scatterplots

qplot(carat, price, data = diamonds)

# We see the positive trend here, as the number of carats increases the price 
# also goes up.

qplot(carat, price, data = diamonds, shape = cut)  # it's hard to read
qplot(carat, price, data = diamonds, color = cut)

qplot(carat, price, data = diamonds, color = cut) + geom_smooth(method = "lm")

# We see the same scatterplot, but slightly more compressed and showing 5 
# regression lines, one for each cut of diamonds. It might be hard to see, but 
# around each line is a shadow showing the 95% confidence interval. We see, 
# unsurprisingly, that the better the cut, the steeper (more positive) the slope
# of the lines.

qplot(carat, price, data=diamonds, color=cut, facets=.~cut) + 
  geom_smooth(method="lm")

#
g <- ggplot(diamonds, aes(depth, price))
summary(g)

g + geom_point(alpha = 1/3)
range(diamonds$depth)

# That's somewhat interesting. We see that depth ranges from 43 to 79, but the 
# densest distribution is around 60 to 65. Suppose we want to see if this 
# relationship (between depth and price) is affected by cut or carat. We know 
# cut is a factor with 5 levels (Fair, Good, Very Good, Premium, and Ideal). But
# carat is numeric and not a discrete factor.

# Let's divide the data into 3 pockets, so 1/3 of the data falls into each. 
# We'll use the R command quantile to do this.

cutpoints <- quantile(diamonds$carat, seq(0, 1, length=4), na.rm = TRUE)
cutpoints                         #  cutpoints are the places where we'll cut

# We see that .2 is the smallest carat size in the dataset and 5.01 is the 
# largest. One third of the diamonds are between .2 and .5 carats and another 
# third are between .5 and 1 carat in size. The remaining third are between 1 
# and 5.01 carats.
# We'll create a new name in diamonds, diamonds$car2:

diamonds$car2 <- cut(diamonds$carat, cutpoints)
g <- ggplot(diamonds, aes(depth, price))

g + geom_point(alpha = 1/3) + facet_grid(cut ~ car2)

# The first 3 columns are labeled with the cutpoint boundaries.
# The fourth is labeled NA and shows us where the data points with missing data 
# (NA or Not Available) occurred. It' because diamonds with carat size .2 were 
# excluded from the car2 field (we see at the top of each column that the first
# column is labeled (0.2,0.5]).

g + geom_point(alpha = 1/3) + facet_grid(cut ~ car2) + 
  geom_smooth(method="lm", size=3, color="pink")

# Boxplots

ggplot(diamonds, aes(carat, price)) + geom_boxplot()

#---

library(ggplot2movies)

head(movies)
qplot(votes, rating, data = movies) + geom_smooth()

head(airquality)
str(airquality)
airquality = transform(airquality, Month=factor(Month))
str(airquality)
qplot(Wind, Ozone, data=airquality, facets=.~Month)
