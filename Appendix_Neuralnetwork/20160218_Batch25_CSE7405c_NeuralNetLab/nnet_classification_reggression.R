rm(list=ls(all=TRUE))

# Load required libraries
library(nnet)
library(devtools)


#------Classification Model-----------------

# Prepare a random data.frame
Fat   = c(0.2, 0.1, 0.2, 0.2, 0.4, 0.3)
Salt  = c(0.9, 0.1, 0.4, 0.5, 0.5, 0.8)
Wg   = c(1, 0, 0, 0, 1, 1)
df   = data.frame(Fat, Salt, Wg)
rm(Fat, Salt, Wg)

# Build the NN model using nnet 
nnModel = nnet(Wg ~ ., data = df, size = 3)

nnModel$fitted.values

# Check the model performance on train data by generating confusion matrix
table(df$Wg, ifelse(nnModel$fitted.values > 0.05, 1, 0))

# Prepare some randoam test data.frame
Fat = c(0.3, 0.1, 0.4)
Salt = c(0.5, 0.7, 0.3)
Wg = c(1,1,0)
df_Test = data.frame(Fat, Salt, Wg)
rm(Fat, Salt, Wg)

pred = predict(nnModel, df_Test)

# Check model performance on test data by generating confusion matrix
table(df_Test$Wg, ifelse(pred > 0.05, 1, 0))

source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')

plot.nnet(nnModel,circle.cex = 4, cex.val = .9, nid = F, max.sp = T)

#circle.cex = 1.5, cex.val = 0.4, nid = F, max.sp = T)


#------Regression Model-----------------

rm(list=ls(all=TRUE))

# Build the Neural network regression model using nnet
library(nnet)
# Load mlbench library that has BostonHousing data. 
library(mlbench)
library(devtools)
source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')

data(BostonHousing)

# The original data are 506 observations on 14 variables, medv being the target variable:
# crim      per capita crime rate by town
# zn 	      proportion of residential land zoned for lots over 25,000 sq.ft
# indus 	  proportion of non-retail business acres per town
# chas 	    Charles River dummy variable (= 1 if tract bounds river; 0 otherwise)
# nox 	    nitric oxides concentration (parts per 10 million)
# rm 	      average number of rooms per dwelling
# age 	    proportion of owner-occupied units built prior to 1940
# dis 	    weighted distances to five Boston employment centres
# rad 	    index of accessibility to radial highways
# tax 	    full-value property-tax rate per USD 10,000
# ptratio 	pupil-teacher ratio by town
# b 	      1000(B - 0.63)^2 where B is the proportion of blacks by town
# lstat 	  percentage of lower status of the population
# medv 	    median value of owner-occupied homes in USD 1000's 

summary(BostonHousing)
head(BostonHousing)

nnet.fit <- nnet(medv ~ ., data=BostonHousing, size=9, linout = TRUE, maxit=500) 
nnet.predict <- predict(nnet.fit)

mean((nnet.predict - BostonHousing$medv)^2) 

data.frame(nnet.predict, BostonHousing$medv)

plot.nnet(nnet.fit,circle.cex = 2, cex.val = 0.4, nid = T, max.sp = T)
