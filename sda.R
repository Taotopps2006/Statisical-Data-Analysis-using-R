########## ANALYSIS OF AUTOMOBILE DATASET CALLED cars93
######### BY TAWOSE OLAMIDE TIMOTHY
######### UB_NUMBER: *******
######## M.Sc Big Data Science and Technology
######## Faculty of Engineering and Informatics
####### University of Bradford.
####### Session: 2017
########################################## SECTION 2: LIBRARY INSTALLATION AND DATA IMPORTATION ####################################################
## Loading the Dataset and Installing required packages
# R environment preparation
rm(list=ls()) # clears all variables declared in the memory
# rm(list = ls(all.names = TRUE))
# Installing and loading R packages
require(dplyr) # provides data manipulating functions. # library(dplyr) would also work
library(magrittr) # provides pipe operator
library(ggplot2)
install.packages("Amelia")
library(Amelia)
install.packages('DescTools')
library(DescTools)
install.packages('plotrix') # for 3D pie chart
library(plotrix)
install.packages('vcd') # for mosaic and assoc function
library(vcd)
library(car) # provides "Boxplot" function,leveneTest from the {car} package which labels outliers and scatter3d function
install.packages('data.table') # installs as.data.table function
library(data.table)
install.packages('psych')
library(psych)
install.packages('sm')
library(sm)
install.packages('nortest')
require(nortest)
#library(Hmisc) # for using describe function
library(e1071) # for using skewness function
library(lattice) # for change text size of label
library(latticeExtra) # for changing text size of label

# the parameter na.strings is equal to c("") so that each missing value is coded as a NA.
cars93M <- read.csv('M:\\Rstudio codes\\1993CarsaM.csv',header=T,na.strings=c(""))
#cars93N <- read.csv('M:\\Rstudio codes\\93carsN.csv',header=T,na.strings=c(""))
###################################################################################################################################################
########################################## SECTION 3: MANAGING AND UNDERSTANDING DATA ####################################################
### 2. Data Exploration and Preprocessing Exploratory data avalysis
## Data Description
######################################################
################ SECTION 3.1 : BASIC EXPLORATION ##################
## Exploration Data Analysis and Handling Missing Data
#Obtaining first several rows of the data frame using head. tail is used to obtain the last several rows
# To check if the data is loaded properly in the workspace
## Basic Exploration
head(cars93M)
tail(cars93M)
#to see only column names of the data or names of variables
names(cars93M)
# types for each attribute
sapply(cars93M, class)
###################################################################
################ SECTION 3.2 : DESCRIPTIVE DATA SUMMARIZATION ##################
# Descriptive statistics 
# descriptive statistics refers to mean, range etc 
nrow(cars93M) # returns the number of rows
ncol(cars93M) #returns the number of columns
dim(cars93M) # dimensions of dataset
str(cars93M) #returns the structure of the data frame # understand the covariates
summary(cars93M) # returns summary statistics
describe(cars93M)
################################################################################
################ SECTION 3.3 : DATA PRE-PROCESSING / PREPARATION ##################
###########################
######## Step 3.3.1: Data Labelling and Cleaning and Preparation
# This preprocessing step often is crucial for obtaining a good fit of the model and better predictive ability.
#################################################
# Rename a column in R
colnames(cars93M)[colnames(cars93M)=="ManualTrans"] <- "CarTrans"
colnames(cars93M)[colnames(cars93M)=="Domestic"] <- "Origin"
names(cars93M) # to check if column rename took effect as required
# creating a new column called Make in the dataset (the 27th column)
x<- as.data.table(list(cars93M$Manufacturer,cars93M$Model))
cars93M$Make = paste(x$V1,x$V2)
# rename the row to the 27th column (MAKE) values
rownames(cars93M) <- cars93M[,27]
# cars93M[,27] <- NULL # redundant code
cars93M <- cars93M[,-27] # delete 27th column cammed MAKE since it has been used to rename the column. It is not needed again to avoid redundant lines of code
# subdata <- cars93M[-2,] delete the 2nd row
#attach(cars93M)
#cars93M$MPG <- ( cars93M$CMPG + cars93M$HMPG ) / 2 # creates another column that includes average of CMPG and HMPG
# Air Bags came in as an integer, when it should be a multi-valued discrete,
# otherwise known as a "factor" in R.
# %>% (a.k.a: pipe operator)
##### mtcars$am = factor(mtcars$am, levels = c(0,1), labels = c("Automatic", "Manual"))
#I have noticed that all variables are loaded as numeric even if some of them are factor viarables.
#Before, starting our analysis, let's first change the factor vaiables
# Cylinder was correctly imported as a multi-valued discrete/factor, but we converted it to continuous numerical inorder to be able to remove the * completely from the R memory
# and converted it back to a factor as required
cars93M$Cylinders = as.numeric(levels(cars93M$Cylinders))[cars93M$Cylinders]
# converting cylinders back to categorical / factor
cars93M$Cylinders = cars93M$Cylinders %>% factor(labels = sort(unique(cars93M$Cylinders)))
# Rearseat was converted to numeric from factor
cars93M$RearSeat = as.numeric(levels(cars93M$RearSeat))[cars93M$RearSeat]
# LuggageCapacity was converted to numeric from factor
cars93M$LuggageCapacity = as.numeric(levels(cars93M$LuggageCapacity))[cars93M$LuggageCapacity]
# I will change the AirBags column from an integer to a categorical factor.
cars93M$AirBags = cars93M$AirBags %>% factor(labels = sort(unique(cars93M$AirBags)))
# changing the labels of the AirBags column 0 = None, 1 = Drive only, 2 = Driver & Passenger
AB <- c('None','Drive only','Driver & Passenger')
cars93M$AirBags <- factor(cars93M$AirBags, labels = AB)
# I will change the DriveTrainType column from an integer to a categorical factor.
DTT = sort(unique(cars93M$DriveTrainType))
cars93M$DriveTrainType = cars93M$DriveTrainType %>% factor(labels = DTT)
# changing the labels of the DriveTrainType column 0 = Rear, 1 = Front, 2 = 4WD / All wheel drive
TT <- c('Rear','Front','4WD')
cars93M$DriveTrainType <- factor(cars93M$DriveTrainType, labels = TT)
# I will change the CarTransmission column from an integer to a categorical factor.
CT = sort(unique(cars93M$CarTrans))
cars93M$CarTrans = cars93M$CarTrans %>% factor(labels = CT)
# changing the labels of the CarTransmission (FORMERLY ManualTransmission) column 0 = No, 1 = Yes
MANU <- c('Automatic','Manual')
cars93M$CarTrans <- factor(cars93M$CarTrans, labels = MANU)
# I will change the Origin column from an integer to a categorical factor.
D = sort(unique(cars93M$Origin))
cars93M$Origin = cars93M$Origin %>% factor(labels = D)
# changing the labels of the Origin column 0 = Non-USA, 1 = USA
origins <- c('non-USA','USA')
cars93M$Origin <- factor(cars93M$Origin, labels = origins)
# to confirm changes made took effect
head(cars93M)
## Check missing values
sapply(cars93M, function(x) sum(is.na(x))) # total NA in each column
## Visualise missing values
missmap(cars93M, main = "Missing values vs observed")
## Handle important columns with few missing values
# cYLINDERS
# since there is only one, we will discard/remove this single row (we could also have replaced the missing values with the mode and keep the datapoints)
cars93M<- cars93M[!is.na(cars93M$Cylinders),]
# rownames(cars93M) <- NULL
dim(cars93M) # dimensions of dataset
############################################## HANDLING AND EXPLORING CATEGORICAL VARIABLES #############################
# class distribution for categorical variables
t = table(cars93M$Type) # with (cars93M, table(Type))
b = table(cars93M$AirBags)
co = table(cars93M$DriveTrainType)
# checking numbers of cylinders per cars
cyl = table(cars93M$Cylinders)
cartran = table(cars93M$CarTrans)
or = table(cars93M$Origin)
prop.table(t) # Proportions or distributions for a single variable table
prop.table(t) * 100 # Percentage Proportions or distributions for a single variable table
cbind(freq=t, percentage=prop.table(t) * 100)
cbind(freq=b, percentage=prop.table(b) * 100)
cbind(freq=co, percentage=prop.table(co) * 100)
cbind(freq=cyl, percentage=prop.table(cyl) * 100)
cbind(freq=cartran, percentage=prop.table(cartran) * 100)
cbind(freq=or, percentage=prop.table(or) * 100)
# prop.table(cyl) * 100 # then visualize using pie chart code written
# Visualizing distribution of categorical variables using Bar plots
qplot(cars93M$Cylinders, xlab = 'Cylinders', ylab = 'Count', main='Bar Plot: Distribution of Number of Cylinders')
# Based on the relatively tiny counts of three- and five-cylinder cars (3 and 2, respectively), I am removing those completely because they end up being a distraction in later plots
cars93M = cars93M[!cars93M$Cylinders %in% c(3, 5),]
qplot(cars93M$Cylinders, ylab = 'Count', xlab = 'Cylinders',main='Distribution of Number of Cylinders \n after pre-processing')
###############################
# Cylinder was correctly imported as a factor, but we converted it to continuous numerical inorder to be able to remove the 3 and 5 completely from the R memory
# and converted it back to a factor as required
table(cars93M$Cylinders)
cars93M$Cylinders = as.numeric(levels(cars93M$Cylinders))[cars93M$Cylinders]
# Substituting '3 and 5' with NA
cars93M[cars93M== 3 && 5] = NA
cars93M <- cars93M[!is.na(cars93M$Cylinders),]
# converting cylinders back to categorical / factor
cars93M$Cylinders = cars93M$Cylinders %>% factor(labels = sort(unique(cars93M$Cylinders)))
table(cars93M$Cylinders)
dim(cars93M)
#############################
# types for each attribute (to confirm after preprocessing or changing column types)
str(cars93M)
#################### SECTION 3.3.3 GRAPHICAL REPRESENTATIONS ##############################
#################### UNIVARIATE DATA ANALYSIS OF CATEGORICAL VARIABLES #######
# barplot of how many cars in each type category
#barplot(table(cars93M$Type))
qplot(cars93M$Type, ylab = 'Frequency', xlab = 'Car Type', main='Bar plot: Number of Car Type')
#3D piechart showing how many cars in each category
slices <-table(cars93M$Type)
lbls <- levels(cars93M$Type)
pct <- round(slices/sum(slices)*100)
lbls <- paste0(lbls, ": ", pct, "%") # add percents to labels
pie3D(slices,
      labels = lbls, explode=0.1,
      col=rainbow(length(lbls)),
      main="3D Pie Chart of Car Types in Cars93 Dataset",cex.main=0.8)
qplot(cars93M$AirBags, ylab = 'Frequency', xlab = 'AirBags', main='Bar plot: AirBags in Cars')
#3D pie chart of Vehicle AirBags from Cars93 data showing distribution of cars with airbag standard, distribution of categorical variable
# Display each category's percentage of the total dataset.
par(mfrow=c(1,1))
slices <-table(cars93M$AirBags)
lbls <- levels(cars93M$AirBags)
pct <- round(slices/sum(slices)*100)
lbls <- paste0(lbls, ": ", pct, "%") # add percents to labels
pie3D(slices,
      labels = lbls,
      col=rainbow(length(lbls)),
      main="\n \n \n \n \n \n \n \n \n \n \n 3D Pie Chart of Airbags in Cars93 Dataset")
#3D pie chart of Vehicle AirBags in USA cars from Cars93 data
BagUS <- subset(cars93M,Origin=='USA')
slice <-table(BagUS$AirBags)
lbl <- levels(BagUS$AirBags)
pc <- round(slice/sum(slice)*100)
lbl <- paste0(lbl, ": ", pc, "%") # add percents to labels
pie3D(slice,
      labels = lbl,
      col=rainbow(length(lbl)),
      main="\n \n \n \n \n \n \n \n \n \n \n Distribution of Airbags in USA Cars")
#3D pie chart of Vehicle AirBags in non-USA cars from Cars93 data
BagNUS <- subset(cars93M,Origin=='non-USA')
slic <-table(BagNUS$AirBags)
lb <- levels(BagNUS$AirBags)
p <- round(slic/sum(slic)*100)
lb <- paste0(lb, ": ", p, "%") # add percents to labels
pie3D(slice,
      labels = lb,
      col=rainbow(length(lb)),
      main="\n \n \n \n \n \n \n \n \n \n \n Distribution of Airbags in non-USA Cars")
qplot(cars93M$DriveTrainType, ylab = 'Frequency', xlab = 'AirBags', main='Bar plot: DriveTrainType')
## 3D pie-chart
slices <-table(cars93M$DriveTrainType)
lbls <- levels(cars93M$DriveTrainType)
pct <- round(slices/sum(slices)*100)
lbls <- paste0(lbls, ": ", pct, "%") # add percents to labels
pie3D(slices,
      labels = lbls,
      col=rainbow(length(lbls)),
      main="\n \n \n \n \n \n \n \n \n \n \n Distribution of DriveTrainType")
qplot(cars93M$CarTrans, ylab = 'Frequency', xlab = 'Types of Car Transmission', main='Bar plot: Distribution of the Car Transmission')
# 3D pie chart
slices <-table(cars93M$CarTrans)
lbls <- levels(cars93M$CarTrans)
pct <- round(slices/sum(slices)*100)
lbls <- paste0(lbls, ": ", pct, "%") # add percents to labels
pie3D(slices,
      labels = lbls,
      col=rainbow(length(lbls)),
      main="\n \n \n \n \n \n \n \n \n \n \n Distribution of Car Transmission")
qplot(cars93M$Origin, ylab = 'Frequency', xlab = 'Origin', main='Bar plot: Distribution of the Car Origin')
slices <-table(cars93M$Origin)
lbls <- levels(cars93M$Origin)
pct <- round(slices/sum(slices)*100)
lbls <- paste0(lbls, ": ", pct, "%") # add percents to labels
pie3D(slices,
      labels = lbls,
      col=rainbow(length(lbls)),
      main="\n \n \n \n \n \n \n \n \n \n \n Distribution of Car Manufacturer")
##################### DISTRIBUTION OF CONTINOUS VARABLES : univariate data analysis
summary(cars93M$MidPrice)
summary(cars93M$EngSize)
# extracting multiple columns from a dataframe using either their column names or column numbers
n_car <- cars93M[,c(5,12)] # or n_car <- cars93M[,c("MidPrice","EngSize")]
library(e1071)
apply(n_car,2,skewness)
#Histogram of car prices with gaussian kernel density ploT
# Histogram: To see some continous variables distribution
####CHECKING DISTRIBUTION AND CHECKING TO SEE IF MEAN IS GREATER THAN MEDIAN
hist(cars93M$MidPrice,prob = TRUE,cex.main=0.8,xlab = 'Price of Cars (in $1,000)',ylab='Frequency',main = 'Histogram and Density Plot of Car Prices')
lines(density(cars93M$MidPrice),col='blue')
#histogram and density plot of Engine Size (litres)
#par(mfrow=c(1,2))
hist(cars93M$EngSize,prob = TRUE, xlab="Engine Size in liters",cex.main=0.8, breaks=12,main = 'Histogram and Density Plot of Engine Size')
lines(density(cars93M$EngSize),col='blue')
outarea = Boxplot(cars93M$CMPG,main = "Achieved city mileage per number of cylinders",col = (c="green"), ylab = "City Miles per gallon",labels = rownames(cars93M),id.n=Inf)
outarea2 = Boxplot(cars93M$HMPG,main = "Boxplot showing distribution of HMPG",col = (c="blue"), ylab = "Highway Miles per gallon",labels = rownames(cars93M),id.n=Inf)
outarea3 = Boxplot(cars93M$HP,main = "Boxplot showing distribution of HP",col = (c="pink"), ylab = "Horse Power",labels = rownames(cars93M),id.n=Inf)
#cars93M[outarea,"CMPG"]
# display the full data frame for cars with high CMPG values
cars93M[outarea,]
cars93M[outarea2,]
cars93M[outarea3,]
par(mfrow=c(1,3))
Boxplot(cars93M$CMPG,main = "Boxplot showing distribution of CMPG",col = (c="green"), ylab = "City Miles per gallon",labels = rownames(cars93M),id.n=Inf)
Boxplot(cars93M$HMPG,main = "Boxplot showing distribution of HMPG",col = (c="blue"), ylab = "Highway Miles per gallon",labels = rownames(cars93M),id.n=Inf)
Boxplot(cars93M$HP,main = "Boxplot showing distribution of HP",col = (c="pink"), ylab = "Horse Power",labels = rownames(cars93M),id.n=Inf)
par(mfrow=c(1,1))
# shows statistics used to build the graph
boxplot.stats(cars93M$CMPG)
boxplot.stats(cars93M$HMPG)
boxplot.stats(cars93M$HP)
#################################################
##Normality test
# Q-Q plot was used to check for normality of the continuous univariate variables of the cars93 dataset
# Q-Q plot was used to check if the variables comes from a normal distribution or not.
# We observed/ checked the data to see if it falls close to the identity line in the Q-Q plot
reduced <- cars93M[,c(sapply(cars93M, class) != "factor") ]
## extracting some variables of interest from the continous data subsetted .LUGGAGECAPACITY,REARSEAT removed cos of missing values.we could have used some missing value handling methods like replacing with mean,mode
reduced_new <- reduced [,c('MidPrice','CMPG','EngSize','HP','RPM','EngRev','FuelTankCapacity','PassengerCapacity','Length','Wheelbase','Width','Weight') ]
summary(reduced_new) # summary
sapply(reduced_new, function(x) sum(is.na(x))) # check for missing values
# remove missing values. in this case none since LUGGAGECAPACITY abd REARSEAT were removed from the subset earlier
#reduced_new = reduced_new[!is.na(reduced_new)]
# having removed missing values, find the number of non-missing values in say, MidPrice of reduced_new
# length of MidPrice
n = length(reduced_new$MidPrice)
# calculate mean, variance and standard deviation of the selected attributes
mean.midprice = mean(reduced_new$MidPrice,na.rm = T)
mean.cmpg = mean(reduced_new$CMPG)
mean.engsize = mean(reduced_new$EngSize)
mean.hp = mean(reduced_new$HP)
mean.rpm = mean(reduced_new$RPM)
mean.engrev = mean(reduced_new$EngRev)
mean.fueltank = mean(reduced_new$FuelTankCapacity)
mean.passenger = mean(reduced_new$PassengerCapacity)
mean.length = mean(reduced_new$Length)
mean.wheelbase = mean(reduced_new$Wheelbase)
mean.width = mean(reduced_new$Width)
mean.weight = mean(reduced_new$Weight)
var.midprice = var(reduced_new$MidPrice,na.rm = T)
var.cmpg = var(reduced_new$CMPG)
var.engsize = var(reduced_new$EngSize)
var.hp = var(reduced_new$HP)
var.rpm = var(reduced_new$RPM)
var.engrev = var(reduced_new$EngRev)
var.fueltank = var(reduced_new$FuelTankCapacity)
var.passenger = var(reduced_new$PassengerCapacity)
var.length = var(reduced_new$Length)
var.wheelbase = var(reduced_new$Wheelbase)
var.width = var(reduced_new$Width)
var.weight = var(reduced_new$Weight)
sd.midprice = sd(reduced_new$MidPrice,na.rm = T)
sd.cmpg = sd(reduced_new$CMPG)
sd.engsize = sd(reduced_new$EngSize)
sd.hp = sd(reduced_new$HP)
sd.rpm = sd(reduced_new$RPM)
sd.engrev = sd(reduced_new$EngRev)
sd.fueltank = sd(reduced_new$FuelTankCapacity)
sd.passenger = sd(reduced_new$PassengerCapacity)
sd.length = sd(reduced_new$Length)
sd.wheelbase = sd(reduced_new$Wheelbase)
sd.width = sd(reduced_new$Width)
sd.weight = sd(reduced_new$Weight)
#next, we set the n points in the interval (0,1) for the n equi-probable point-wise probabilities,
#each of which is assigned to the correspondingly ranked quantile.
#(The smallest probability is assigned to the smallest quantile, and the largest probability is assigned to the largest quantile.)
#These probabilities will be used to calculate the quantiles for each hypothesized theoretical distribution.
# set n points in the interval (0,1)
# the formula k/(n+1), for k = 1,..,n is used
# this is a vector of the n probabilities
prob = (1:n)/(n+1) # n = 87 as calculated i.e number of rows/observation
#since all variables are continous, we fitted it to a normal distribution
# normal distribution is the most commonly used distribution for continuous variables.
#the qnorm() function to calculate the quantiles from the normal distribution.
#Specifically, I used the sample mean and the sample standard deviation of the variables to specify the parameters of the normal distribution.
# calculate normal quantiles using mean and standard deviation from the 12 continous variables selected
normal.quantiles.midprice = qnorm(prob, mean.midprice, sd.midprice)
normal.quantiles.cmpg = qnorm(prob, mean.cmpg, sd.cmpg)
normal.quantiles.engsize = qnorm(prob, mean.engsize, sd.engsize)
normal.quantiles.hp = qnorm(prob, mean.hp, sd.hp)
normal.quantiles.rpm = qnorm(prob, mean.rpm, sd.rpm)
normal.quantiles.engrev = qnorm(prob, mean.engrev, sd.engrev)
normal.quantiles.fueltank = qnorm(prob, mean.fueltank, sd.fueltank)
normal.quantiles.passenger = qnorm(prob, mean.passenger, sd.passenger)
normal.quantiles.length = qnorm(prob, mean.length, sd.length)
normal.quantiles.wheelbase = qnorm(prob, mean.wheelbase, sd.wheelbase)
normal.quantiles.width = qnorm(prob, mean.width, sd.width)
normal.quantiles.weight = qnorm(prob, mean.weight, sd.weight)
#Finally, I plot the theoretical quantiles on the horizontal axis and the sample quantiles on the vertical axis.
# abline() function was used to add the identify line. The two parameters call for a line with an intercept of 0 and a slope of 1.
par(mfrow=c(2,2))
# normal quantile-quantile plot for the 12 variables
plot(sort(reduced_new$MidPrice)~sort(normal.quantiles.midprice) ,
     col.lab='blue',xlab='Theoretical Quantiles from Normal Distribution',
     ylab = "Sample Quantiles of MidPrice",cex.lab=0.89)
abline(0,1,col='red')
plot(sort(normal.quantiles.cmpg), sort(reduced_new$CMPG) , xlab = 'Theoretical Quantiles from Normal Distribution'
     , ylab = "Sample Quantiles of CMPG",cex.lab=0.89,col.lab='blue')
abline(0,1,col='red')
plot(sort(normal.quantiles.engsize), sort(reduced_new$EngSize) , xlab = 'Theoretical Quantiles from Normal Distribution'
     , ylab = "Sample Quantiles of Engine Size",cex.lab=0.89,col.lab='blue')
abline(0,1,col='red')
plot(sort(normal.quantiles.hp), sort(reduced_new$HP) , xlab = 'Theoretical Quantiles from Normal Distribution'
     , ylab = "Sample Quantiles of HP",cex.lab=0.89,col.lab='blue')
abline(0,1,col='blue')
mtext("Normal Quantile-Quantile Plot of the 12 selected variables",side=3,line=-2,outer=TRUE) # change the line argument for new position
mtext("Normal Quantile-Quantile Plot of the 12 selected variables",side=3,line=-2,outer=TRUE)
plot(sort(normal.quantiles.rpm), sort(reduced_new$RPM) , xlab = 'Theoretical Quantiles from Normal Distribution'
     , ylab = "Sample Quantiles of RPM",cex.lab=0.89,col.lab='blue')
abline(0,1,col='blue')
plot(sort(normal.quantiles.engrev), sort(reduced_new$EngRev) , xlab = 'Theoretical Quantiles from Normal Distribution'
     , ylab = "Sample Quantiles of Engine Revolution ",cex.lab=0.89,col.lab='blue')
abline(0,1,col='blue')
plot(sort(normal.quantiles.fueltank), sort(reduced_new$FuelTankCapacity) , xlab = 'Theoretical Quantiles from Normal Distribution'
     , ylab = "Sample Quantiles of Fuel Tank Capacity",cex.lab=0.89,col.lab='blue')
abline(0,1,col='green')
plot(sort(normal.quantiles.passenger), sort(reduced_new$PassengerCapacity) , xlab = 'Theoretical Quantiles from Normal Distribution'
     , ylab = "Sample Quantiles of Passenger Capacity",cex.lab=0.89,col.lab='blue')
abline(0,1,col='green')
plot(sort(normal.quantiles.length), sort(reduced_new$Length) , xlab = 'Theoretical Quantiles from Normal Distribution'
     , ylab = "Sample Quantiles of Length",cex.lab=0.89,col.lab='blue')
abline(0,1,col='green')
plot(sort(normal.quantiles.wheelbase), sort(reduced_new$Wheelbase) , xlab = 'Theoretical Quantiles from Normal Distribution'
     , ylab = "Sample Quantiles of Wheel base",cex.lab=0.89,col.lab='blue')
abline(0,1,col='purple')
plot(sort(normal.quantiles.width), sort(reduced_new$Width) , xlab = 'Theoretical Quantiles from Normal Distribution'
     , ylab = "Sample Quantiles of Width",cex.lab=0.89,col.lab='blue')
abline(0,1,col='purple')
plot(sort(normal.quantiles.weight), sort(reduced_new$Weight) , xlab = 'Theoretical Quantiles from Normal Distribution'
     , ylab = "Sample Quantiles of Weight",cex.lab=0.89,col.lab='blue')
abline(0,1,col='purple')
mtext("Normal Quantile-Quantile Plot of the 12 selected variables",side=3,line=-2,outer=TRUE) # change the line argument for new position
mtext("Normal Quantile-Quantile Plot of the 12 selected variables",side=3,line=-2,outer=TRUE)
mtext("Normal Quantile-Quantile Plot of the 12 selected variables",side=3,line=-2,outer=TRUE)
mtext("Normal Quantile-Quantile Plot of the 12 selected variables",side=3,line=-2,outer=TRUE)
par(mfrow=c(1,1))
#dev.off()
### checking normality using Anderson-Darling Test for the selected 12 conntinous variables
ad.test(reduced_new$MidPrice)
ad.test(reduced_new$CMPG)
ad.test(reduced_new$EngSize)
ad.test(reduced_new$HP)
ad.test(reduced_new$RPM)
ad.test(reduced_new$EngRev)
ad.test(reduced_new$FuelTankCapacity)
ad.test(reduced_new$PassengerCapacity)
ad.test(reduced_new$Length)
ad.test(reduced_new$Wheelbase)
ad.test(reduced_new$Width)
ad.test(reduced_new$Weight)
######################################
################### BIVARIATE DATA ANALYSIS #####################
######### i. Numeric-to-categorical relationship
# box plots for each category/group to understand data distribution and identify outliers
par(mfrow=c(1,3))
Boxplot(HP~Cylinders,data=cars93M,main = "Box plot of Horse Power versus no. of Cylinders",cex.main=1.0, xlab = "Number of Cylinders",col = (c("green","blue")), ylab = "Horse Power",id.method="y")
Boxplot(CMPG~CarTrans,data=cars93M,main = "Box plot of CMPG versus Transmission type", cex.main=1.0,xlab = "Car Transmission",col = (c("green","blue")), ylab = "City Miles per gallon",id.method="y")
Boxplot(HMPG~Origin,data=cars93M,main = "Box plot of HMPG versus Car Origin", cex.main=1.0,xlab = "Car Origin",col = (c("green","blue")), ylab = "Highway Miles per gallon",id.method="y")
par(mfrow=c(1,1))
# mean for each category of cars using aggregate function / Descriptive statistics by group using aggregate()
aggregate(HP~Cylinders,data=cars93M,FUN = mean)
aggregate(CMPG~CarTrans,data=cars93M,FUN = mean)
aggregate(HMPG~Origin,data=cars93M,FUN = mean)
# aggregate(HP~Cylinders,data=cars93M,FUN = median) # median for each category of cars using aggregate function
## box plots for more than one grouping factor
Boxplot(CMPG ~ CarTrans * Cylinders,
        data=cars93M,
        varwidth=TRUE,
        col=c("gold","darkgreen"),
        main="CMPG Distribution by Car Transmission Type",
        xlab="Car Transmission Type",id.method="y",cex.main=1.0)# mean for each category of cars using aggregate function
aggregate(CMPG ~ CarTrans * Cylinders,data=cars93M,FUN = mean)
########## ii. Categorical-to-categorical relationship
##grouped barplot of the Cars93 dataset by origin (USA/non USA) broken down by car transmission (automatic/manual).
#legend indicates the color coding of the categorical variables
par(mfrow=c(1,1))
#cross-tabulation of CarTrans and Origin / two-contigency table
total <- table(cars93M$CarTrans, cars93M$Origin)
cbind(freq=total, percentage=prop.table(total) * 100)
colors<- c("darkblue","red")
title="Cars93 Car Distribution by Car Transmission and Origin"
barplot(total,main=title,xlab="Car Origin", ylab="car count from Cars93 dataset", col=colors,
        legend=rownames(total),beside=TRUE)
######### iii.Numeric-to-numeric relationship:
# Scatterplot : to check relationship between two variables
plot(cars93M$HMPG~cars93M$CMPG,main='Scatterplot of HMPG VS CMPG',xlab = 'CMPG',ylab = 'HMPG',ylim=c(20,max(cars93M$CMPG)+5),xlim=c(15,max(cars93M$HMPG)),col='red',pch=19)
hlev <- which(cars93M$HMPG>40)
# Add numeric labels to the graph:
text(cars93M$HMPG[hlev]~cars93M$CMPG[hlev],labels = rownames(cars93M)[hlev])
#sunflowerplot
sunflowerplot(cars93M$HMPG~cars93M$CMPG,main='Sunflowerplot of HMPG VS CMPG',xlab = 'CMPG',ylab = 'HMPG',ylim=c(20,max(cars93M$CMPG)+5),xlim=c(15,max(cars93M$HMPG)),col='dark blue',pch=19)
text(cars93M$HMPG[hlev]~ cars93M$CMPG[hlev],labels = rownames(cars93M)[hlev])
# new variable called MPG is created because of many repeated values between HMPG and CMPG
cars93M$MPG <- ( cars93M$CMPG + cars93M$HMPG ) / 2 # creates another column that includes average of CMPG and HMPG
dim(cars93M)
################### MULTIVARIATE DATA ANALYSIS #####################
#subset all continous coumns first
## subsetting all continous variables
reduced <- cars93M[,c(sapply(cars93M, class) != "factor") ]
pairs(reduced,main='Scatter plot matrix of all continous variables')
panel.cor <- function(x, y, digits = 2, prefix = "r = ", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  # correlation coefficient
  r <- (cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt,cex = 2.5)
  # p-value calculation using 0.05 level of significance
  p <- cor.test(x, y)$p.value
  txt2 <- format(c(p, 0.123456789), digits = digits)[1]
  txt2 <- paste("p = ", txt2, sep = "")
  if(p<0.05) txt2 <- paste(" \n p-value ", "< 0.05", sep = "")
  text(0.5, 0.4, txt2,cex = 2.5)
}
redu <- cars93M[,c(4,5,6) ]
pairs(redu, upper.panel = panel.cor,main="Correlation Plot of all three variables") # with smooth line
pairs(redu, lower.panel = panel.smooth, upper.panel = panel.cor,main="Correlation Plot of all three variables") # with smooth line
####################################################################################
####################################################################################
########################################## SECTION 4 : RESEARCH QUESTIONS ####################################################
################### Research question 1 MPG vs HorsePower using Pearson's product-moment correlation method / linear correlation test
par(mfrow=c(1,1))
plot(cars93M$HP,cars93M$MPG, xlab = "Horsepower", ylab = "MPG", main = "ScatterPlot of MPG vs HP ",pch=20,col.lab='dark blue',cex.lab=1.1,col="blue")
panel.sca <- function(x, y, digits = 2, prefix = "r = ", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  # correlation coefficient
  r <- (cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.8, 0.9, txt,cex = 2.5)
}
plot(cars93M$HP,cars93M$MPG,legend=panel.sca(cars93M$HP,cars93M$MPG), xlab = "Horsepower", ylab = "MPG", main = "ScatterPlot of MPG vs HP ",pch=20,col.lab='dark blue',cex.lab=1.1,col="blue",xlim=c(50,max(cars93M$HP)+20))
hlev <- which(cars93M$MPG>40 | cars93M$HP>250 | cars93M$HP<74)
text(cars93M$HP[hlev],cars93M$MPG[hlev],labels = rownames(cars93M)[hlev])
cor.test(cars93M$HP, cars93M$MPG,method = c("pearson"))
####################################################################################################################
################### Research question 2 PRICE vs RPM using Spearman's rank correlation / rank correlation test / non-parametric correlation
## Spearman's rank correlation / rank correlation test / non-parametric correlation
plot(cars93M$RPM,cars93M$MidPrice, xlab = "RPM", ylab = "PRICE", main = "ScatterPlot of PRICE vs RPM ",pch=20,col.lab='dark blue',cex.lab=1.1,col="blue")
hlev <- which(cars93M$MidPrice>60)
text(cars93M$RPM[hlev],cars93M$MidPrice[hlev],labels = rownames(cars93M)[hlev])
rank(cars93M$MidPrice,cars93M$RPM) # calculates the vector of the ranks of the values in a vector
cor.test(cars93M$RPM,cars93M$MidPrice,method = "spearman") # correlation between two vectors
############################################################################
################### Research question 3 Car types dependent vs Drive Train Type using chi-square test
## chi-square test btw car types and drive train type
## nominal/categorical vs nominal/categorical
#cross-tabulation of CarType and drive train type / two-contigency table
total <- table(cars93M$Type,cars93M$DriveTrainType)
colors<- c("darkblue","red",'green',"pink","yellow","brown")
title="Car Type vs Drive Train Type"
barplot(total,main=title,xlab="Drive Train Type", ylab="car count from Cars93 dataset", col=colors,
        legend=rownames(total),beside=TRUE)
new = table(cars93M$Type,cars93M$DriveTrainType)
chisq.test(new)
############################################################################
################################# SECTION 5: FURTHER DATA ANALYSIS###############
####### QUESTION 1: Box-whisker plot: PRICE or MPG variables give good examples of somewhat skewed data with potential outliers among the upper fences.
par(mfrow=c(1,2))
Boxplot(cars93M$MidPrice,main = "Box-whisker plot of PRICE",col = (c="green"), ylab = "PRICE",labels = rownames(cars93M),id.n=Inf)
Boxplot(cars93M$MPG,main = "Box-whisker plot of of MPG",col = (c="blue"),ylab = "MPG",labels = rownames(cars93M),id.n=Inf)
par(mfrow=c(1,1))
#######################################
####### QUESTION 3: 2-Independent sample t-test or Independent t test for checking difference in mean of two groups / levels
## interval vs nominal / categorical
#Calculate mean for each origin of cars
aggregate(MidPrice~Origin,data=cars93M,FUN = mean)
#Calculate the range for each origin of cars
aggregate(MidPrice~Origin,data=cars93M,FUN = range)
# box plots for each category to understand data distribution and identify outliers
library(car)
Boxplot(MidPrice~Origin,data=cars93M,main = "PRICE of USA and non-USA cars ", xlab = "Origin of car ",col = (c("green","blue")), ylab = "PRICE",id.method="y")
# Test for normality in each group( US and non-US ) try Automatic and non Automatic
# Perform anderson darling normality test for each category
aggregate(MidPrice~Origin,data=cars93M,FUN = function(x) ad.test(x))
#There is a warning because anderson,test function returns four values
# Extract the p values
aggregate(MidPrice~Origin,data=cars93M,FUN = function(x) ad.test(x)$p.value)
#Perform a Levene test for equality of variances in the two groups
# Check if the variance in the two groups is equal
# run the levene test centered around the mean
leveneTest(cars93M$MidPrice~cars93M$Origin, center = mean)
#Confirm from link: The variance in the two groups is not equal. We will transform the data to remedy this
#Apply a log transformation to stabilize data NORMALITY
log.transformed.price = log(cars93M$MidPrice)
#Perform a t test on the transformed variable
#Perform a t test assuming equal variance
t.test(log.transformed.price~cars93M$Origin,var.equal = TRUE)
t.test(cars93M$MidPrice~cars93M$Origin,var.equal = TRUE) # t- test to confirm robustness to outliers since both sample sizes are greater than 30
# if variance was not welch- two sample t-test could have been used
# t.test(cars93M$MidPrice~cars93M$Origin) # not used since variance are equal/there is no signiticant difference in variance.this was confirmed using using levene's test
#######################################################################################
####### QUESTION 4: One-way ANOVA for difference in means: Check out city MPG ratings between the three DRIVETRAIN categories.
## Compare MPG distributions for cars with 4,6, or 8 cylinders
## Comparing Groups VIA Kernal Density.
library(sm)
# plot densities to check distribution
sm.density.compare(cars93M$MPG,cars93M$DriveTrainType, xlab="Miles Per Gallon")
title(main="MPG Distribution by Drive Train Type")
# add legend via mouse click
colfill<-c(2:(2+length(levels(cars93M$DriveTrainType))))
legend(locator(1), levels(cars93M$DriveTrainType), fill=colfill)
#Create boxplots to identify any outliers
Boxplot(MPG~DriveTrainType,data=cars93M,main = "Achieved mileage per drive train types", xlab = "Drive Train Type",col = (c("green","blue")), ylab = "Miles per gallon",id.method="y")
# Check for normality in each group using anderson darling test
with(cars93M,tapply(MPG,DriveTrainType,ad.test))
# Check for equality of variance
leveneTest(MPG~DriveTrainType, data = cars93M)
#Transform data and check for normality and equality of variance
# #Apply a log transformation to MPG and check for normality and equality of variance.
cars93M$log.MPG = log(cars93M$MPG)
with(cars93M,tapply(log.MPG,DriveTrainType,ad.test))
leveneTest(log.MPG~DriveTrainType, data = cars93M)
# Running one way ANOVA test
model.aov = aov(log.MPG~DriveTrainType,cars93M)
summary(model.aov)
##############################################################################
#################################################################
####### QUESTION 5: CONTIGENCY TABLE OF AIRBAGS BY CAR ORIGIN
#cross-tabulation of AIRBAGS BY CAR ORIGIN / two-contigency table
table(cars93M$AirBags, cars93M$Origin) # Two-way contigency table
total <- table(cars93M$AirBags, cars93M$Origin)
prop.table(total) * 100 # Percentage Proportions or distributions of the table
# subset of USA cars
BagUS <- subset(cars93M,Origin=='USA')
table(BagUS$AirBags, BagUS$Origin) # Two-way contigency table
total <- table(BagUS$AirBags, BagUS$Origin)
prop.table(total) * 100 # Percentage Proportions or distributions of the table
# subset of non-USA cars
BagNUS <- subset(cars93M,Origin=='non-USA')
table(BagNUS$AirBags, BagNUS$Origin) # Two-way contigency table
total <- table(BagNUS$AirBags, BagNUS$Origin)
prop.table(total) * 100 # Percentage Proportions or distributions of the table
#############################################################################
############### QUESTION 6: scatter plot of PRICE vs MPG showing correlation coefficient value
panel.sca <- function(x, y, digits = 2, prefix = "r = ", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  # correlation coefficient
  r <- (cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.8, 0.9, txt,cex = 2.5)
}
plot(cars93M$MPG,cars93M$MidPrice,legend=panel.sca(cars93M$MidPrice,cars93M$MPG),main='Scatterplot of PRICE vs MPG \n with correlation coefficient value',xlab='MPG',ylab='Price',col='red',pch=19,xlim=c(17.5,max(cars93M$MPG)+2))
hlev <- which(cars93M$MPG>40 | cars93M$MidPrice>60 )
text(cars93M$MidPrice[hlev]~cars93M$MPG[hlev],labels = rownames(cars93M)[hlev])
#############################################################################
####### QUESTION 8: One-way ANOVA and a simple linear regression was used to investigating the relationship between
###### number of CYLINDERS and MPG using only 4, 6, and 8 cylinder cars.
## Compare MPG distributions for cars with 4,6, or 8 cylinders
## Comparing Groups VIA Kernal Density.
library(sm)
# create value labels. changing 4 to 4 cylinder
levels(cars93M$Cylinders)[levels(cars93M$Cylinders)=="4"] <- "4 cylinder"
levels(cars93M$Cylinders)[levels(cars93M$Cylinders)=="6"] <- "6 cylinder"
levels(cars93M$Cylinders)[levels(cars93M$Cylinders)=="8"] <- "8 cylinder"
# plot densities
sm.density.compare(cars93M$MPG,cars93M$Cylinders, xlab="Miles Per Gallon")
title(main="MPG Distribution by Car Cylinders")
# add legend via mouse click
colfill<-c(2:(2+length(levels(cars93M$Cylinders))))
legend(locator(1), levels(cars93M$Cylinders), fill=colfill)
describeBy(cars93M$MPG,cars93M$Cylinders) #Get summary statistics for each cylinder # install 'psych' package to use describeBy() function
#Create boxplots to identify any outliers
ggplot(cars93M,aes(x = Cylinders,y=MPG, color = Cylinders)) + geom_boxplot() + ggtitle("MPG of Cars by different cylinders")
table(cars93M$Cylinders)
# Check for normality in each group using anderson darling test
with(cars93M,tapply(MPG,Cylinders,ad.test))
# Check for normality in each group using shapiro-wilk test
with(cars93M,tapply(MPG,Cylinders,shapiro.test))
# Check for equality of variance
leveneTest(MPG~Cylinders, data = cars93M)
#Transform data and check for normality and equality of variance
# #Apply a log transformation to MPG and check for normality and equality of variance.
cars93M$log.MPG = log(cars93M$MPG)
with(cars93M,tapply(log.MPG,Cylinders,shapiro.test))
leveneTest(log.MPG~Cylinders, data = cars93M)
# Running one way ANOVA test
aov1 = aov(log.MPG~Cylinders,cars93M)
summary(aov1)
print(aov1$fitted)
cor(cars93M$MPG, aov1$fitted) # strength of this relationship
model.lm <- lm(MPG ~ Cylinders, data = cars93M)
summary(model.lm)
rsq <- summary(model.lm)$r.squared
rsq
sqrt(rsq)
print(model.lm$fitted)
# Strngth of both models: the Pearson correlation between observed and fitted values is:
cor(cars93M$MPG, aov1$fitted) # strength of this relationship : ANOVA
cor(cars93M$MPG, model.lm$fitted) # strength of this relationship : REGRESSION
# visualisation on a scatter plot
par(mfrow=c(1,1))
panel.sca <- function(x, y, digits = 2, prefix = "r = ", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  # correlation coefficient
  r <- (cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.8, 0.9, txt,cex = 2.5)
}
plot(x = aov1$fitted, y = cars93M$MPG,legend=panel.sca(cars93M$MPG, aov1$fitted),
     xlab = "Fitted duration", ylab = "Observed duration",main='ANOVA')
abline(lm(cars93M$MPG ~ aov1$fitted), col="blue")
plot(x = model.lm$fitted, y = cars93M$MPG,legend=panel.sca(cars93M$MPG, model.lm$fitted),
     xlab = "Fitted duration", ylab = "Observed duration",main='REGRESSION MODEL')
abline(lm(cars93M$MPG ~ model.lm$fitted), col="red")
