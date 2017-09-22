# Build Neural Network for classification using neuralnet library.
rm(list=ls(all=TRUE))

# Set the working directory
setwd("D:/naveen/Bigdata - hadoop and related/ActivitiesRepo/20161225_Batch22_CSE7405c_NeuralNetLab/20161225_CSE7405c_Batch22_NeuralNetLab")

# Load required libraries 
library(neuralnet)

# Look at the sample data
head(iris)

num_Records = nrow(iris)
train_Index = sample(1:num_Records, round(num_Records * 0.80, digits = 0))
train_Data = iris[train_Index,] 
test_Data = iris[-train_Index,] 
rm(train_Index, num_Records)

train_Data <- cbind(train_Data[, 1:4], class.ind(train_Data$Species))

nn = neuralnet(setosa + versicolor + virginica ~ 
                 Sepal.Length + Sepal.Width + 
                 Petal.Length + Petal.Width, 
               train_Data, hidden = 2)


# formula <- as.formula(paste(paste(colnames(class.ind(iris$Species)),
#                                   collapse = " + "),
#                             "~",
#                             paste(names(iris[,1:4]),
#                                   collapse = " + ")))
# 
# nn <- neuralnet(formula, data=train_Data, hidden = 2)
# rm(formula)

plot(nn)

nn$net.result[[1]]

target_Levels = colnames(class.ind(iris$Species))

predicted = target_Levels[max.col(nn$net.result[[1]])]
actual = target_Levels[max.col(nn$response)]

# Compute confusion matrix and calculate recall on Train Data
conf_Matrix = table(actual, predicted)
conf_Matrix 

rm(predicted, actual, conf_Matrix)

# Remove target attribute from Test Data
test_Data_No_Target = subset(test_Data, select=-c(Species))

# Predict 
nn_predict <- compute(nn, covariate= test_Data_No_Target)
rm(test_Data_No_Target)

# View the predicted values
nn_predict$net.result

# Construct Confusion Matrix 
predicted = target_Levels[max.col(nn_predict$net.result)]
actual = test_Data$Species

# Compute confusion matrix and calculate recall on Train Data
conf_Matrix = table(actual, predicted)
conf_Matrix