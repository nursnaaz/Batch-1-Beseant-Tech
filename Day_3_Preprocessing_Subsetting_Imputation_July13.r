#########################################Subsetting###############################
##This might form an important aspect in Data analysis where we might want to work on a subset of data

##Subset on vectors
v<-c(1,2,3,4,5)
v[v>3]  #Output all elements greater than 3

attach(mtcars)
data<-mtcars

str(data)
##Subset on matrices and data frames
#a. Calling by cell positions


data1<-data[,2:11]
data1
data1<-data[1:10,2:11]
data1

data[,-1]
data[setdiff(names(data),'mpg')]

class(data1[,c(2,3)]) # explain this

data1[,1, drop=F] #droping str ?

#b. By using column names- two methods
data1<-data[,c("mpg","cyl")]

name<-c("mpg","cyl","disp","hp")
data1<-data[names(data) %in% name] ## %in% comes in handy for subsetting

#c. Using a subset function ##from help identify the argument to be given
data1<-subset(data,mpg>25,select=mpg:carb) #From data extracts all the records whose mpg>25 and all columns

#d. The same dataframe can be obtained in another way
data1<-data[mpg>25,]

##Multiple conditions can be given using "&" or "|"
data2<-data[mpg>25 & hp>75,]
data2<-subset(data,mpg>25 | gear==5,select=mpg:carb)

##Using which.max
data[which.max(mpg),]

##Using which.min
data[which.min(mpg),]

##Using which
data[which(data$mpg==max(data$mpg)),]
data[which(row.names(data) %in% c("Mazda RX4","Datsun 710")),]

detach(mtcars)

################################Data Exploration and Data Aggregation Methods#######################
##These form an important aspect especially for data exploration, data understanding and to processing
## the data for model building
##A data frame can have multiple datatypes in it like numeric, factor and logical.
library(plyr)
attach(baseball)
dfBB<-baseball
str(dfBB) ##outputs what to which type each variable belong to.
summary(dfBB) ## gives the overall summary of the data,we observe that the stats are given for numerical
## attributes, if characters then class and mode are mentioned.

##Conversion of variable types if necessary
##We can consider "teams" as a factor so that we can compare runs batted and home runs for teams
dfBB$team<-as.factor(dfBB$team)
str(dfBB$team)
##We do this appropriate conversions first

##Missing Values
##To count the number of missing values
sum(is.na(dfBB)) ##Gives the number of missing values in the data. What to do with the missing values ?

#option1. Omit all records with NA values
data1<-na.omit(dfBB)  ##it omits all the records which has atleast one NA value in it
data2<-dfBB[complete.cases(dfBB),]  ##another way

#Option2. If the missing values are few, then we can impute these missing values
library(DMwR)
data3<-centralImputation(dfBB) #Central Imputation
sum(is.na(data3))

data4<-knnImputation(dfBB[,-c(1,4,5)],scale=T,k=5) #KNN Imputation
sum(is.na(data4))

write.csv(data3, "data_imputed.csv", row.names=FALSE)
