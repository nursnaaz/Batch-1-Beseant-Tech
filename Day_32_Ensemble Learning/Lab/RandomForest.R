rm(list = ls(all = T))

setwd("C:/Users/kumar/Desktop/lab ensebles/20170212_Batch25_CSE7405c_EnsembleLab")

# Load the required libraries
library(DMwR)
library(randomForest)
#install.packages("randomForest")
# Read the data into R
data = read.table('hepatitis.txt', header=F, dec='.',
                        col.names=c('target','age','gender','steroid','antivirals',
                                    'fatigue','malaise','anorexia','liverBig',
                                    'liverFirm','spleen','spiders','ascites',
                                    'varices','bili','alk','sgot','albu','protime',
                                    'histology'), 
                        na.strings=c('?'), sep=',')

# Understand the data 
str(data)
summary(data)

table(data$target)
str(data$target) # 1: Die; 2: Live 

# Convert 1s and 2s into 1s and 0s 
data$target= ifelse(data$target==1, 1, 0 ) # 1: Die(+ve); 0: Live (-ve)

# The numerical variables are: age, bili, alk, sgot, albu and protime
# The categorical variables are: the remaining 14 variables

num_Attr = c("age", "bili", "alk", "sgot", "albu", "protime")
cat_Attr = setdiff(names(data), num_Attr)

# Seperate numerical and categorical variables and convert them into appropriate type
data = data.frame(sapply(data,as.character))
cat_Data = data.frame(sapply(data[,cat_Attr], as.factor))
num_Data = data.frame(sapply(data[,num_Attr], as.numeric))
data = cbind(num_Data, cat_Data)
rm(num_Attr, cat_Attr)

rm(cat_Data, num_Data)

# Handle missing values using knn imputation
sum(is.na(data))

data = knnImputation(data = data, k = 5)
sum(is.na(data))

summary(data)
str(data)

# Split dataset into train and test
set.seed(123)

train_RowIDs = sample(1:nrow(data), nrow(data)*0.7)
train_Data = data[train_RowIDs,]
test_Data = data[-train_RowIDs,]
rm(train_RowIDs)

# Check how records are split with respect to target attribute.
table(data$target)
table(train_Data$target)
table(test_Data$target)
rm(data)

# Build the classification model using randomForest
model = randomForest(target ~ ., data=train_Data, 
                      keep.forest=TRUE, ntree=50) 

# Print and understand the model
print(model)

# Important attributes

model$importance  
round(importance(model), 2)   

# Extract and store important variables obtained from the random forest model
rf_Imp_Attr = data.frame(model$importance)
rf_Imp_Attr = data.frame(row.names(rf_Imp_Attr),rf_Imp_Attr[,1])
colnames(rf_Imp_Attr) = c('Attributes', 'Importance')
rf_Imp_Attr = rf_Imp_Attr[order(rf_Imp_Attr$Importance, decreasing = TRUE),]

# plot (directly prints the important attributes) 
varImpPlot(model)


# Predict on Train data 
pred_Train = predict(model, train_Data[,setdiff(names(train_Data),"target")],
                     type="response", norm.votes=TRUE)


# Build confusion matrix and find accuracy   
cm_Train = table("actual"= train_Data$target, "predicted" = pred_Train);
accu_Train= sum(diag(cm_Train))/sum(cm_Train)
rm(pred_Train, cm_Train)

# Predicton Test Data
pred_Test = predict(model, test_Data[,setdiff(names(test_Data),"target")],
                    type="response", norm.votes=TRUE)

# Build confusion matrix and find accuracy   
cm_Test = table("actual"= test_Data$target, "predicted" = pred_Test);
accu_Test= sum(diag(cm_Test))/sum(cm_Test)
rm(pred_Test, cm_Test)

accu_Train
accu_Test

# Build randorm forest using top 9 important attributes. 
top_Imp_Attr = as.character(rf_Imp_Attr$Attributes[1:9])

# Build the classification model using randomForest
model_Imp = randomForest(target ~ ., data=train_Data[,c(top_Imp_Attr,"target")], 
                         keep.forest=TRUE, ntree=50) 

# Print and understand the model
print(model_Imp)

# Important attributes
model_Imp$importance  

# Predict on Train data 
pred_Train = predict(model_Imp, train_Data[,top_Imp_Attr],
                     type="response", norm.votes=TRUE)


# Build confusion matrix and find accuracy   
cm_Train = table("actual"= train_Data$target, "predicted" = pred_Train);
accu_Train_Imp = sum(diag(cm_Train))/sum(cm_Train)
rm(pred_Train, cm_Train)

# Predicton Test Data
pred_Test = predict(model_Imp, test_Data[,top_Imp_Attr],
                    type="response", norm.votes=TRUE)

# Build confusion matrix and find accuracy   
cm_Test = table("actual"= test_Data$target, "predicted" = pred_Test);
accu_Test_Imp = sum(diag(cm_Test))/sum(cm_Test)
rm(pred_Test, cm_Test)

accu_Train
accu_Test
accu_Train_Imp
accu_Test_Imp

top_Imp_Attr = as.character(rf_Imp_Attr$Attributes[1:9])

# Build the classification model using randomForest
model_Imp = randomForest(target ~ ., data=train_Data[,c(top_Imp_Attr,"target")], 
                         keep.forest=TRUE, ntree=50) 

# Print and understand the model
print(model_Imp)

# Important attributes
model_Imp$importance  

# Predict on Train data 
pred_Train = predict(model_Imp, train_Data[,top_Imp_Attr],
                     type="response", norm.votes=TRUE)


# Build confusion matrix and find accuracy   
cm_Train = table("actual"= train_Data$target, "predicted" = pred_Train);
accu_Train_Imp = sum(diag(cm_Train))/sum(cm_Train)
rm(pred_Train, cm_Train)

# Predicton Test Data
pred_Test = predict(model_Imp, test_Data[,top_Imp_Attr],
                    type="response", norm.votes=TRUE)

# Build confusion matrix and find accuracy   
cm_Test = table("actual"= test_Data$target, "predicted" = pred_Test);
accu_Test_Imp = sum(diag(cm_Test))/sum(cm_Test)
rm(pred_Test, cm_Test)

accu_Train
accu_Test
accu_Train_Imp
accu_Test_Imp

