######R as a Calculator######
2+2
2-3+1
2/3*2
2*3/2
2^3
2^3*2
2^(3*2)

###Mathematical functions in R
log(10)
log(10,2) ##log(number,base)
sqrt(25)
sin(1)
cos(x = 45)

# Some of the Built in constants

pi

LETTERS

letters

month.name

month.abb[12]

months(.leap.seconds)

############################Data Types in R######################################
# a.  Numeric --real numbers 
# b.  Integer-- positive and negative whole  numbers including zero
# c.	logical-- True or False 
# d.	character-- alphabets/special characters
# e.	complex--  z<-1+2i;  Arg(0+1i); Mod(2-3i)

nos <- 10:1
#setting a directory
setwd("/home/tiger028/Besant/")

#Getting the directory path
getwd()

#Generating a sequence of numbers using scope operator and assigning to a variable
numbers<-10:15
numbers

#Generating a sequence of numbers using a "seq" function
numbers <- seq(1,10)
numbers
numbers<-seq(1,10,2) 
numbers

#Using  the "c" (concatenate) , we use this most of the time
numbers<-c(1,2,10)
numbers

#########################Variables in R########################################
# a.  Scalar -- a single number/character
x=5
x="a" 

#b.	Vector-- a sequence of elements
x<-c(1,2,3,4,5) 
x<-c("a","c")
y<-c("alpha",7)  
z <- c(T,F,TRUE,FALSE)


#To know the data type or class

class(z)


x <- c(1,3,5,7,9)

y <- c(2,4,6,8,10)

# Element wise addition

x+y

# Elementwise subtraction

x-y

# Elementwise multiplication

x * y 

# Elementwise division

x / y

########################################

x <- c(1,2,3,4,5,6)

y <- c(10,20) 

x + y # here vector y gets replicated so that vector addition be completed 


### Binding the vectors- Row binding and column binding

A <- rbind(x, y) 
A

B <- cbind(x, y)
B
