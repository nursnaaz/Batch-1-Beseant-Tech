# Build Neural Network for Multi-Class classification using neuralnet library.
rm(list=ls(all=TRUE))

# Set the working directory
setwd("C:/Users/jeevan/Desktop/NN")


# Importing "all_features.txt" files's data into R dataframe using read.table function
data<-read.table("all_features.txt", sep="", header=F)

# Understand the structure the summary of the data using str and summary R commands
str(data)
summary(data)

# Separate Target Variable and Independent Variables.
# In this case First variable is target variable and remaining are independent variables
target_Variable = data[, 1]
independent_Variables = data[,2:73]

# Standardization the independent variables using decostand funcion in vegan R library
library(vegan)
# Note: To standardize the data using 'Range' method
independent_Variables = decostand(independent_Variables, "range") 

# Recombine Target and independent variables
data = data.frame(independent_Variables, target = target_Variable)
rm(independent_Variables, target_Variable)

# Convert class/target variable as factor
data$target = as.factor(data$target)

# compute mean, min, max, and median number records for each level in the target attribute
target_Var_Dist = data.frame(table(data$target))

  mean(target_Var_Dist$Freq)
  min(target_Var_Dist$Freq)
  max(target_Var_Dist$Freq)
  median(target_Var_Dist$Freq)

#-----------------------------------------------

#Splitting as Train & Test sets
trainID = sample(1:nrow(data),(nrow(data)*0.6))
train_Data = data[trainID,]
test_Data = data[-trainID,]
rm(trainID)
  
#Executing NN on Train Data
library(nnet)
nn = nnet(target ~ ., data = train_Data, 
          size = 8, rang = 0.1, 
          decay = 5e-4, maxit = 100)
  
#Validating the results on test data
pred = predict(nn, test_Data, type = "class")

sort(as.numeric(unique(pred)))
  conf_Matrix = table(pred, test_Data$target)
  conf_Matrix = conf_Matrix[order(as.numeric(rownames(conf_Matrix))), ] 

#Accuracy of model
sum(diag(conf_Matrix))/sum(conf_Matrix)*100
#-----------------------------------------------  

# In this case median is more appropriate, so take median number of samples from each level of the target attribute
min_Num_Train_Records = as.integer(median(target_Var_Dist$Freq))

train_RowNames = c();
test_RowNames = c();

#Selecing train samples randomly for each class and also making sure to have equal sample in each class
makeTrainCountsEqualForEachClass = 1;

for (i in target_Var_Dist$Var1){
  
  subset_RowNames = rownames(data[which(data$target==i),])
  
  subset_Count = target_Var_Dist[target_Var_Dist$Var1==i, "Freq"]
  
  subset_Train_Count = round(subset_Count * 0.8, digits = 0)

  set.seed(123);
  subset_RowNames_Train = sample(subset_RowNames, subset_Train_Count)
  
  subset_RowNames_Test = setdiff(subset_RowNames, subset_RowNames_Train)
  
  if (subset_Train_Count == 0){
    subset_RowNames_Train = subset_RowNames;
    subset_Train_Count = subset_Count;    
  }
  
  if (length(subset_RowNames_Test) == 0){
    subset_RowNames_Test = subset_RowNames;    
  }
  
  if  (subset_Train_Count < min_Num_Train_Records && 
       makeTrainCountsEqualForEachClass == 1){ 
    cat("\nEnsuring all classes have similar training data sizes");
    # If you DO NOT have enough samples to be selected for training. 
    # repeat the selectedRowNamesTrain. 
    num_Times_Repeat = round(min_Num_Train_Records/subset_Train_Count)
    subset_RowNames_Train = rep.int(subset_RowNames_Train, num_Times_Repeat);
    subset_RowNames_Test = rep.int(subset_RowNames_Test, num_Times_Repeat);
  }    
  
  cat("\nTrain data: ", length(subset_RowNames_Train), ", Test data:", length(subset_RowNames_Test))
  
  train_RowNames = c(train_RowNames, as.numeric(subset_RowNames_Train))
  test_RowNames = c(test_RowNames, as.numeric(subset_RowNames_Test))
}

rm(i, num_Times_Repeat, subset_Count, subset_Train_Count, 
   subset_RowNames, subset_RowNames_Test, subset_RowNames_Train, 
   subset_Train_Counttarget_Var_Dist, makeTrainCountsEqualForEachClass )

# Make sure all the training data is presented randomly. 
train_RowNames = sample(train_RowNames);

length(train_RowNames)
length(unique(data$target)) * min_Num_Train_Records

rm(min_Num_Train_Records)

#Splitting as Train & Test sets
train_Data = data[train_RowNames,]
test_Data = data[test_RowNames,]
rm(train_RowNames, test_RowNames)

#Executing NN on Train Data
library(nnet)
nn = nnet(target ~ ., data = train_Data, size = 8, rang = 0.1, decay = 5e-4, maxit = 100)
  
#Validating the results on test data
pred = predict(nn, test_Data, type = "class")
sort(as.numeric(unique(pred)))
conf_Matrix = table(pred, test_Data$target)
conf_Matrix = conf_Matrix[order(as.numeric(rownames(conf_Matrix))), ] 
#Accuracy of model
sum(diag(conf_Matrix))/sum(conf_Matrix)*100

