
# What if x<-c(1,2,3,4,5) and y<-c("a","b","c","d","e")

x <- c(1,2,3,4,5)

y <- c("a","b","c","d","e")

A <- rbind(x, y)  # observe the matrix data types

#c.  matrix--2d arrangement of elements (elements should be of same data type)

x <- matrix(c(1,2,3,4,5,6), nrow=3, ncol=2, byrow=T) #try with character data type
x

z <- matrix(c(1,2,3,4,5,6,7,8,9,0),nrow  = 3,byrow = T,ncol = 5,dimnames =list( c('a','b','c'),c('d','e','f','g','h')))

#Row sums and Column sums ,sum of diagonal elements, sum of all elements in Matrix

rowSums(z)

colSums(x)

sum(diag(z))

sum(x)

#d.  dataframe-- it is also a matrix representation but can have multiple data types in it.
##creating an empty data frame

data <-data.frame()

#Creating a data frame

data <- data.frame( Name=c("Alpha","Beta","Gamma"), 
                    Marks=c(29,NA,27),Sex=c("M","F","F"),Location=c("Chennai","Delhi","Bengaluru")) 
data

data[1,3]
str(data)

#For both matrix and data frames: calling referring "elements" by position

x <- matrix(c(1,2,3,4), nrow=2, ncol=2, byrow=T)
x
x[1,1] # extracting element in first row and first column
x[1,]  #extracting all the elements from first row
x[,2]  #extracting all the elements from second column
x[2,1] #extracting  the element in second row and first column

#To get the dimensions of the matrix

dim(x)

#To name the rows or columns in the matrix
x
dimnames(x)<-list(c("One","Two","Three"),c("Four","Five"))
x

data
names(data)
dimnames(data)

names(data)<-c("Radiation", "Count")

data

#list-- a  "vector" containing other objects which could be a vector, a dataframe or a list.
#Creating a list

x <- c(1,2,3,4)
x1 <- c(5,6,7,8)
x2 <- list(x,x1)
y <- c("a","b")

z <-  data.frame(name=c("Alpha","Beta","Gamma"), Marks=c(29,30,27))

A <- list(x2,y,z)

#Try out the following and observe the output
A
A[1]
A[[1]]
A[1][1]
A[[1]][[1]]

A[[1]][[1]]
A[[1]][[2]]
A[[2]][[1]]
A[[2]][[2]]

#################Saving the work space as image, reading and loading data##################
# The workspace is your current R working environment and includes any user-defined
# objects (vectors, matrices, functions, data frames, and lists)
# The current working directory is the directory from which R will read files and to
# which it will save results by default. You can find out what the current working direc-
#   tory is by using the getwd() function. You can set the current working directory by
# using the setwd() function. If you need to input a file that isn't in the current working
# directory, use the full pathname in the call.

setwd("D:/Ravikanth/Academics/Batch25/20161211/ToShare/Data")

getwd()

#a.	Saving workspace

save.image() 

save.image("Save_20161211.RData")

# Loading saved workspace

load("Save_20161211.RData")

#b.	How do save only a few variables from environment

save(x, y, file="xy.RData")

#c.	Writing data to a file

write.csv(z,"./data.csv", row.names=F)  #for different delimited file ??

#d.	Reading the csv files and RData files into R environment

grade <- read.csv("./Grade.csv", header=T, sep = ",")

##Reading other formats we use read.table command  # comma as decimal & : as field sep ?

read <- read.table("./greek.txt",sep="\t",header=T)

# removing objects from the workspace

rm(x)

x  # Error: object 'x' not found

# remove all objects from the workspace

rm(list=ls(all=TRUE))

