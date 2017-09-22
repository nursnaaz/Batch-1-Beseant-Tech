rm(list=ls(all=TRUE))

# Load required libraries
library(vegan)
library(infotheo)
library(C50)
library(rpart)

# Set  working directory
setwd("C:/Users/jeevan/Desktop/Ensemble/")

attr = c('id', 'age', 'exp', 'inc', 'zip', 'family', 
         'ccavg', 'edu', 'mortgage', 'loan', 
         'securities', 'cd', 'online', 'cc')

# Read the data from csv file
data = read.csv(file = "UniversalBank.csv", 
                header = TRUE, col.names = attr)

# Removing the id, zip and experience. 
drop_Attr = c("id", "zip", "exp")
attr = setdiff(attr, drop_Attr)
data = data[, attr]
rm(drop_Attr)

# Convert attribute to appropriate type  
cat_Attr = c("family", "edu", "securities", 
             "cd", "online", "cc", "loan")
num_Attr = setdiff(attr, cat_Attr)

cat_Data = data.frame(sapply(data[,cat_Attr], as.factor))
num_Data = data.frame(sapply(data[,num_Attr], as.numeric))

# Using Equal Frequency Convert numeric attributes into categorical. 
num_2_Cat_Data = data.frame(sapply(data[,num_Attr],  
                                   function(x){discretize(x, disc = "equalfreq", 
                                                          nbins = 4)}))
names(num_2_Cat_Data) = num_Attr

num_2_Cat_Data = data.frame(sapply(num_2_Cat_Data, as.factor))

data = cbind(num_2_Cat_Data, cat_Data)
rm(cat_Data, num_Data, num_2_Cat_Data, cat_Attr, num_Attr)

# Do the summary statistics and check for missing values and outliers.
summary(data)

#------------------------------------------------------

ind_Attr = setdiff(attr, "loan")
rm(attr)

# Divide the data into test and train
set.seed(123)

train_RowIDs = sample(1:nrow(data), nrow(data)*0.7)
train_Data = data[train_RowIDs,]
test_Data = data[-train_RowIDs,]
rm(train_RowIDs)

# Check how records are split with respect to target attribute.
table(data$loan)
table(train_Data$loan)
table(test_Data$loan)
rm(data)

#----------------Ensemble:Stacking-------------------- 

# Build CART model on the training dataset
cart_Model = rpart(loan ~ ., train_Data, method = "class")
summary(cart_Model)

# Build C5.0 model on the training dataset
c50_Model = C5.0(loan ~ ., train_Data, rules = T)
summary(c50_Model)

# Build Logistic regression on the training dataset
glm_Model = glm(loan ~ ., train_Data, family = binomial)
summary(glm_Model)

#---------Predict on Train Data----------

# Using CART Model predict on train data
cart_Train = predict(cart_Model, train_Data, type = "vector") 
table(cart_Train)

# if we choose type=vector, then replace 1 with 0 and 2 with 1
cart_Train = ifelse(cart_Train == 1, 0, 1)
table(cart_Train)

# Using C5.0 Model predicting with the train dataset
c50_Train = predict(c50_Model, train_Data, type = "class")
c50_Train = as.vector(c50_Train)
table(c50_Train)

# Using GLM Model predicting on train dataset
glm_Train = predict(glm_Model, train_Data, type = "response")
#it gives probabilities, so we #need to convert to 1's and 0's; 
# if >0.5 show as 1 or else show as 0.
glm_Train = ifelse(glm_Train > 0.5, 1, 0) 
table(glm_Train)

# Combining training predictions of CART, C5.0 & Log Regression together
train_Pred_All_Models = data.frame(CART = cart_Train, 
                                   C50 = c50_Train,
                                   GLM = glm_Train)
train_Pred_All_Models = data.frame(sapply(train_Pred_All_Models, as.factor))

# or first use "apply" then type data_ensemble = data.frame(data_ensemble)
str(train_Pred_All_Models)
summary(train_Pred_All_Models)
rm(cart_Train, glm_Train, c50_Train)

# Viewing the predictions of each model
table(train_Pred_All_Models$CART) #CART 
table(train_Pred_All_Models$C50)  #C5.0
table(train_Pred_All_Models$GLM)  #Logistic Regression
table(train_Data$loan) #Original Dataset DV

# Adding the original DV to the dataframe
train_Pred_All_Models = cbind(train_Pred_All_Models, loan = train_Data$loan)

# Ensemble Model with GLM as Meta Learner
str(train_Pred_All_Models)
head(train_Pred_All_Models)

ensemble_Model = glm(loan ~ ., train_Pred_All_Models, family = binomial)
summary(ensemble_Model)

# Check the "ensemble_Model model" on the train data
ensemble_Train = predict(ensemble_Model, train_Pred_All_Models, 
                         type = "response")
ensemble_Train = ifelse(ensemble_Train > 0.5, 1, 0)
table(ensemble_Train)

cm_Ensemble = table(ensemble_Train, train_Pred_All_Models$loan)
sum(diag(cm_Ensemble))/sum(cm_Ensemble)

#---------Predict on Test Data----------

# Using CART Model prediction on test dataset
cart_Test = predict(cart_Model, test_Data, type="vector")
cart_Test = ifelse(cart_Test == 1, 0, 1)

cm_CART = table(cart_Test, test_Data$loan)
sum(diag(cm_CART))/sum(cm_CART)

# Using C50 Model prediction on test dataset 
c50_Test = predict(c50_Model, test_Data, type = "class")
c50_Test = as.vector(c50_Test)

cm_C50 = table(c50_Test, test_Data$loan)
sum(diag(cm_C50))/sum(cm_C50)

# Using GLM Model prediction on test dataset
glm_Test = predict(glm_Model, test_Data, type="response")
glm_Test = ifelse(glm_Test > 0.5, 1, 0)

cm_Glm = table(glm_Test, test_Data$loan)
sum(diag(cm_Glm))/sum(cm_Glm)

###########################################################

# Combining test predictions of CART, C5.0 & Log Regression together 
test_Pred_All_Models = data.frame(CART = cart_Test, 
                                  C50 = c50_Test, 
                                  GLM = glm_Test) 
rm(cart_Test, c50_Test, glm_Test)

test_Pred_All_Models = data.frame(sapply(test_Pred_All_Models, as.factor))
str(test_Pred_All_Models)
head(test_Pred_All_Models)

# Check the "glm_ensemble model" on the test data
ensemble_Test = predict(ensemble_Model, test_Pred_All_Models, type = "response")
ensemble_Test = ifelse(ensemble_Test > 0.5, 1, 0)
table(ensemble_Test)

cm_Ensemble = table(ensemble_Test, test_Data$loan)
sum(diag(cm_Ensemble))/sum(cm_Ensemble)