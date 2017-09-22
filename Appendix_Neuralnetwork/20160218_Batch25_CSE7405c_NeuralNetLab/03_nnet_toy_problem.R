rm(list=ls(all=TRUE))

# Load required librarie(s)
library(nnet)

#------Classification Model-----------------

# Prepare some randoam data.frame
Fat   = c(0.2, 0.1, 0.2, 0.2, 0.4, 0.3)
Salt  = c(0.9, 0.1, 0.4, 0.5, 0.5, 0.8)
Acc   = c(1, 0, 0, 0, 1, 1)
df   = data.frame(Fat, Salt, Acc)
rm(Fat, Salt, Acc)

set.seed(123)

# Build the NN model using nnet 
nnModel = nnet(Acc ~ ., data = df, size = 3)

nnModel$n
nnModel$wts

nnModel$fitted.values

# Check the model performance on train data by generating confusion matrix
table(df$Acc, ifelse(nnModel$fitted.values > 0.05, 1, 0))

# Prepare some randoam test data.frame
Fat = c(0.3, 0.1, 0.4)
Salt = c(0.5, 0.7, 0.3)
Acct = c(1,1,0)
df_Test = data.frame(Fat, Salt, Acct)
rm(Fat, Salt, Acct)

pred = predict(nnModel, df_Test)

# Check model performance on test data by generating confusion matrix
table(df_Test$Acc, ifelse(pred > 0.05, 1, 0))


#------Regression Model-----------------

rm(list=ls(all=TRUE))

# Build the Neural network regression model using nnet

# Load mlbench library that has BostonHousing data. 
library(mlbench)
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

nnet.fit <- nnet(medv/50 ~ ., data=BostonHousing, size=5, linout = TRUE) 
# linout: switch for linear output units. Default logistic output units. 

# multiply 50 to restore original scale
nnet.predict <- predict(nnet.fit) * 50

# mean squared error: 16.40581
mean((nnet.predict - BostonHousing$medv)^2) 

data.frame(nnet.predict, BostonHousing$medv)