#Free  the environment
rm(list=ls(all=TRUE))
setwd(choose.dir())

#install required packages
#install.packages("FNN") #"Fast Nearest Neighbours" for knn regression
#install.packages("Metrics") #to calculate error metrics for regression
library(FNN)
library(Metrics)

#set.seed()
set.seed(12345) #to get same random numbers generated every time

data=read.table(file="airfoil_self_noise.txt")
names(data)=c("Frequency","Angle_of_attack","Chord length","Free_stream_velocity","Suction_side_displacement_thickness","sound_pressure_level")
# target attribute is x25

#slip the data in train and test
#Split the data into train and test
set.seed(1234)
dt = sort(sample(nrow(data), nrow(data)*.7))
train<-data[dt,]
test<-data[-dt,]

### Applying KNN

## Excluding Target Variable 
testData <- test[,1:5]
test.tgt <- test[,6]
trainData <- train[,1:5]
train.tgt <- train[,6]


# Run the model
pred <- knn.reg(train = trainData, test = testData, y = train.tgt, k = 1 )
actual <- test.tgt
pred <- data.frame(pred$pred)
result2 <- rmse(actual = actual, predicted = pred)
result2

# Run the model
pred <- knn.reg(train = trainData, test = testData, y = train.tgt, k = 3 )
actual <- test.tgt
pred <- data.frame(pred$pred)
result2 <- rmse(actual = actual, predicted = pred)
result2

# Run the model
pred <- knn.reg(train = trainData, test = testData, y = train.tgt, k = 5)
plot(pred)
actual <- test.tgt
pred <- data.frame(pred$pred)
result2 <- rmse(actual = actual, predicted = pred)
result2

# Run the model
pred <- knn.reg(train = trainData, test = testData, y = train.tgt, k = 16)
actual <- test.tgt
pred <- data.frame(pred$pred)
result2 <- rmse(actual = actual, predicted = pred)
result2




