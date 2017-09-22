# Build Neural Network for classification using neuralnet library.
rm(list=ls(all=TRUE))

# Set the working directory
setwd("D:/naveen/Bigdata - hadoop and related/ActivitiesRepo/20161225_Batch22_CSE7405c_NeuralNetLab/20161225_CSE7405c_Batch22_NeuralNetLab")

# Importing "UniversalBank.csv" files's data into R dataframe using read.csv function.
data = read.csv(file="UniversalBank.csv", header=TRUE, sep=",")

# Understand the structure the summary of the data using str and summary R commands
str(data)

# Using subset remove 'ID' and 'ZIP.Code' columns from  the data
data = subset(data , select = -c(ID, ZIP.Code)) 
str(data)

# Convert all the variables to appropriate type
#   To numeric using as.numeric()
#   To categoical using as.factor()
#   To ordinal using as.factor() with ordered argument set to TRUE or using as.ordered()
data$Education = as.ordered(data$Education)
str(data)

# R NN library takes only numeric attribues as input 
# Convert all categorical and ordinal attributes to numeric using appropriate technique. Hint: dummies

# Convert "Education" categorical attribute to numeric using dummy function in dummies R library
# Drop actual Education attribute from orginal data set 
# Add created dummy Education variables to orginal data set
library(dummies)

education = dummy(data$Education)
head(education)

data  = subset(data, select=-c(Education)) 
str(data)

data  = cbind(data, education)
rm(education)
str(data)


# Separate Target Variable and Independent Variables.
# In this case "Personal.Loan" is a target variable and all others are independent variable. 

target_Variable = data$Personal.Loan
independent_Variables = subset(data, select = -c(Personal.Loan))

# Standardization the independent variables using decostand funcion in vegan R library
library(vegan)

# Note: To standardize the data using 'Range' method
independent_Variables = decostand(independent_Variables, "range")

data  = data.frame(independent_Variables, Personal.Loan = target_Variable)

rm(independent_Variables, target_Variable)
str(data)
# Use set.seed to get same test and train data 
set.seed(123) 

# Prepare train and test data in 60:40 ratio
num_Records = nrow(data )

# to take a random sample of  60% of the records for train data 
train_Index = sample(1:num_Records, round(num_Records * 0.6, digits = 0))
train_Data = data [train_Index,] 
test_Data = data [-train_Index,] 
rm(train_Index, num_Records, data)

# See data distribution in response variable in both Train and Test data:
table(train_Data$Personal.Loan)

table(test_Data$Personal.Loan)

# Load neuralnet R library
library(neuralnet)

# Build a Neural Network having 1 hidden layer with 2 nodes 
# nn = neuralnet(Personal.Loan ~ Age+Experience+Income+
#                                Family+CCAvg+Mortgage+
#                                Securities.Account+
#                                CD.Account+Online+
#                                CreditCard+Education1+
#                                Education2+Education3, 
#                data=train_Data, hidden=2)

formula = as.formula(paste("Personal.Loan ~", 
                           paste(setdiff(names(train_Data),"Personal.Loan"), 
                                 collapse = " + ")))

formula

nn = neuralnet(formula, data=train_Data, hidden=2)  


# See covariate and result varaibles of neuralnet model
out = cbind(nn$covariate, nn$net.result[[1]])

out
#covariate tells about the variables that had been used to train the neural network
#net.result[[1]] a matrix containing the overall result of the neural network
#y= nn$covariate

# Remove rownames and set column names
dimnames(out) = list(NULL, c("Age","Experience",
                             "Income","Family",
                             "CCAvg","Mortgage",
                             "Securities.Account",
                             "CD.Account","Online",
                             "CreditCard","Education1",
                             "Education2","Education3",
                             "nn_Output"))

# To view top records in the data set
head(out) 
rm(out)

# Plot the neural network
plot(nn)

# Compute confusion matrix and calculate recall on Train Data
predicted = factor(ifelse(nn$net.result[[1]] > 0.5, 1, 0))
conf_Matrix = table(train_Data$Personal.Loan, predicted)
recall = (conf_Matrix[2,2] / (conf_Matrix[2,1] + conf_Matrix[2,2])) * 100
recall
rm(predicted, conf_Matrix, recall)

# Remove target attribute from Test Data
test_Data_No_Target = subset(test_Data, select=-c(Personal.Loan))

# Predict 
nn_predict = compute(nn, covariate= test_Data_No_Target)
rm(test_Data_No_Target)

# View the predicted values
nn_predict$net.result

# Compute confusion matrix and calculate recall for Test Data
predicted = factor(ifelse(nn_predict$net.result > 0.5, 1, 0))
conf_Matrix = table(test_Data$Personal.Loan, predicted)
recall = (conf_Matrix[2,2]/(conf_Matrix[2,1]+conf_Matrix[2,2]))*100
recall
rm(nn_predict, predicted, conf_Matrix, recall)

rm(test_Data, train_Data, nn)

