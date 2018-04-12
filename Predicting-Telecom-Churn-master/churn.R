# To install the package if not already installed, 
# if installed then will only load the library of that package

#As this case study is computationally intensive,use all the cores of cpu
registerDoParallel(cores = 12)


if (!"install.load" %in% rownames(installed.packages()))
  install.packages("install.load")
library(install.load)
#install the required packages
pkgs_to_install_load <- c("readr","dplyr","tidyr","lubridate","ggplot2","car",
                          "Hmisc","ROCR","caret","dummies","caTools",
                          "MASS", "gridExtra", "e1071", "klaR", "arules", "class", "scales", 
                          "purrr","kernlab","doParallel","png")
sapply(pkgs_to_install_load,install_load)


####Package installation and libaray loading completed#################################


#######Source File Loading and its merging#########################################################
# path to folder that holds multiple .csv files
# folder <- "All the files are in the current folder hence "." as input to list.files"
# create list of all .csv files in folder
churn <- list.files(path=".", pattern="*.csv") %>% 
        map(read_csv) %>%           # read in all the files individually, using the function read_csv() from the readr package
        reduce(merge, all=F)        # reduce with merge into one dataframe
#########Soure file loading and its merging completed#############################

######Data cleaning and Preparation############################################


# Checkpoint 1 - Missing Value treatment

sum(is.na(churn)) # 11

# There are 11 rows which has got missing values
# Let's find out the column in which we are getting missing value

sapply(churn, function(x) sum(is.na(x)))

sapply(churn,function(x) unique(x))

# Missing values are present in Total charge only
# After analysing the data we realised, these all rows contains data for those customers
# Who are not getting churned
# So removing those rows as these rows are not much significant for the analysis
# and a very less numbers as compare to all records
dim(churn)

churn <- churn %>% na.omit()

# Let's us run the is.na function again to check if missing values got ommited or not

sum(is.na(churn)) # 0

# No missing values


#  Chekpoint 2 - Duplicacy removal


nrow(unique(churn)) == nrow(churn)

# As no of unique rows and total records are equal which means no duplicate values

# Checkpoint 3 - Variable data type conversion

# Understand the structure of the collated file

str(churn)
glimpse(churn)

# By looking at sturcture and values through glimpse function
# we can say, we need to convert majority of variables from character to factor

# Convert the variables from character type to factor except churn column

churn_char <- as.data.frame(unclass(churn[,-c(1,2,7,8,9,11)]))

# convert senior citizen attribute to factor type

churn$SeniorCitizen <- as.factor(churn$SeniorCitizen)

# Converting churn to factor type

churn$Churn <- as.factor(churn$Churn)


# combine the churn_char with main data i.e. churn

churn <- cbind(churn[,c(2,7,8,9,11)],churn_char)

# Let's look at the sturcture of churn again

str(churn)

# Now all variables data type are appropriate

# Chekpoint 4 - Outlier detection and treatment
# We have three numeric variables i.e. tenure, monthly charges and Total charges
# We will be checking outliers for these three variables through box plot

# Outliers for tenure

boxplot(churn$tenure)

# Can not see any outliers, so no need of outlier treatment

# Outliers for monthly charge

boxplot(churn$MonthlyCharges)

# Can not see any outliers, so no need of outlier treatment

# Outlier for total charge

boxplot(churn$TotalCharges)

# Can not see any outliers so need of outlier treatment


# Checkpoint 5 - EDA

# Make bar charts to find interesting relationships between variables.
#A function to make bar plots and save it in the working directory

churn_bargraph <- function(z, na.rm = TRUE, ...) {
  nm <- names(z)
  for (i in seq_along(nm)) {
    plots <-ggplot(z,aes_string(x=nm[i],fill=factor(z$Churn))) + geom_bar(position = "fill")+
      guides(fill=guide_legend(reverse=TRUE))+
      scale_fill_discrete(labels=c("Good Customer","Churned Customer"))+
      labs(fill='churn status')
    ggsave(plots,width = 20, height = 8, units = "cm",filename=paste("myplot",nm[i],".png",sep=""))
  }
}

churn_bargraph(churn[,-c(1,2,7,8)])



# Please check your working directory to see all the graphs

# Below are the findings after plotting all the graphs - 

# Monthly plan customer the most likely to churn
# Person having no dependents are churning more
# No device protecting - more likely to churn
# Gender has no impact on churning
# Whoever using fiber optic internet services, they are more likely to churn
# Multiple lines have no impact on churning
# Person having no online backup is more likely to churn
# Person using no online security is more likely to churn
# Unmarried people are churning more as compare to married people
# Phone services have no impact on churning
# Percentage of churning on senior citizen is higher
# No tech support, Streaming movies, Streaming TV more likely to churn
# No internet services - Less likely to churn

# Analysis on numeric data

ggplot(churn, aes(x= churn$Churn, y= churn$tenure)) + geom_boxplot()

# A person who is churing, their tenure are comparatively lesser as compare to those who are not churning

ggplot(churn, aes(x= churn$Churn, y= churn$MonthlyCharges)) + geom_boxplot()

# Monthly charges for churning customers are higher as compare to non churning customers

ggplot(churn, aes(x= churn$Churn, y= churn$TotalCharges)) + geom_boxplot()

# Total charges for churning customers are less as compare to non churning
# As majority of them are new so their total charges would be less



# Checkpoint 6 - Data transformation

# As we need to create four models i.e.  KNN,Logistics,SVM and Naive bayes
# For first three models we need to perform some data transformation
# Like doing scaling of all the numeric variabls
# Conversion of all categorical variables into dummy variables
# As above two steps are not required for Naives bayes 
# so coping churn data into a variable churn_nb for naive bayes model

churn_nb <- churn


# Data transformation for KNN, Logistics and svm model

#scale the numeric variables
churn[ ,c(1,2,3)] <- scale(churn[ ,c(1,2,3)])

#Dummy Variable creation 

# create dummy variables for the factor variables using dummies package

churn_char_dummy <- dummy.data.frame(churn_char,all = FALSE)

# Create dummy variable for senior citizen

churn_char_dummy_SC <- dummy.data.frame(data.frame(churn$SeniorCitizen),all = FALSE)


# Remove one dummy column for senior citizen

churn_char_dummy_SC <-data.frame(churn_char_dummy_SC[,2])

# Changing the column names of dummy variables Senior Citizen

colnames(churn_char_dummy_SC)<- c("SeniorCitizen1")

#Remove one  dummy column  from all factor variables

churn_char_dummy <- churn_char_dummy[,-c(1,3,6,8,12,14,16,18,21,24,27,30,33,36,39)]

#Remove the original character variables from the dataframe including customer ID

churn <- churn[,-c(5:20)]


#convert the response variable to numeric type
churn$Churn <- as.numeric(as.factor(churn$Churn))
#Replace the value of not churning to 0
churn$Churn[which(churn$Churn==1)] <- 0
#Replace the value of churning to 1
churn$Churn[which(churn$Churn==2)] <- 1
#Bind the churn dataframe with dummy variables dataframe
churn <- cbind(churn,churn_char_dummy_SC,churn_char_dummy)


########Data cleansing and preparation done#######################################

####Creation of traing and test for Naive bayes##################################

set.seed(100)
s <- sample.split(churn_nb,SplitRatio = .7)
train_nb <- churn_nb[s==T,]
test_nb <-  churn_nb[s==F,]

######Training and test for naive bayes done#######################################


####Training and test data for KNN, Logistics and SVM#############################
#split the dataframe into train and test data frames
#Ensure the same proportion of class labels in train and test datasets
#Set seed 100 for ensuring same results every time

set.seed(100)
split_indices <- sample.split(churn$Churn,0.7)
train <- churn[split_indices==TRUE,]
test <- churn[split_indices==FALSE,]



### Training and test data for KNN,Logistics and SVM done#########################

########Naive bayes###############################################################

# Naive bayes model creation

model_nb <- naiveBayes(Churn~.,data = train_nb)

# Removal of response variable from the data

test_nb_1 <- test_nb[,-4]

# Predicting the lables of test data from model which has been created

pred_nb <- predict(model_nb, test_nb_1)

# Accuracy check

confusionMatrix(pred_nb, test_nb$Churn,positive='Yes')

#Roc with probablities

pred_nb_raw <- predict(model_nb, test_nb_1, type = "raw")
pred_nb_prob <- pred_nb_raw[,2]
real_nb_vec <- ifelse(test_nb$Churn=="Yes",1,0)
pr_nb <- prediction(pred_nb_prob,real_nb_vec)


prf <- performance(pr_nb,"tpr","fpr")
plot(prf,colorize=T)
abline(a=0,b=1,lwd=2,lty=2)
auc_nb <- performance(pr_nb, measure= "auc")
auc_nb@y.values[[1]]

# Area under curve is 81.61



########Naive bayes completed#######################################################

# Logistic Regression:
# Bring the data in the correct format to implement Logistic regression model.
str(churn)
#As all the attributes are of numeric type the data is ready for logistic regression


# Initial Model with all variables
logistic_initial_model <- glm(Churn~.,family = binomial, data=train)
summary(logistic_initial_model)


# Perform Stepwise selection for removing the insignificant variables
step <- stepAIC(logistic_initial_model,direction = "both")
step$call

#Create model from the variables selected from stepAIC
logistic_model2 <- glm(formula = Churn ~ tenure + MonthlyCharges + TotalCharges + 
                         SeniorCitizen1 + PhoneServiceYes + `ContractOne year` + `ContractTwo year` + 
                         PaperlessBillingYes + `PaymentMethodElectronic check` + MultipleLinesYes + 
                         `InternetServiceFiber optic` + InternetServiceNo + OnlineBackupYes + 
                         DeviceProtectionYes + StreamingTVYes + StreamingMoviesYes, 
                       family = binomial, data = train)

summary(logistic_model2)
vif(logistic_model2)

#Check the correlation between variables with high VIF values
cor(train$TotalCharges,train$tenure)


#As the correlation is high among them remove total charges which is
#having high VIF
logistic_model3 <- glm(formula = Churn ~ tenure + MonthlyCharges +  
                         SeniorCitizen1 + PhoneServiceYes + `ContractOne year` + 
                         `ContractTwo year` + PaperlessBillingYes + `PaymentMethodElectronic check` + 
                         MultipleLinesYes + `InternetServiceFiber optic` + InternetServiceNo + 
                         OnlineBackupYes + DeviceProtectionYes + StreamingTVYes + 
                         StreamingMoviesYes, family = binomial, data = train)

summary(logistic_model3)
vif(logistic_model3)

# Select the variables using VIF criterion.

#Check the correlation 
cor(train$MonthlyCharges,train$`InternetServiceFiber optic`)
#As the variables are highly correlated remove the 
#variable with highest VIF value i.e Monthly charges

logistic_model4 <- glm(formula = Churn ~ tenure + 
                         SeniorCitizen1 + PhoneServiceYes + `ContractOne year` + 
                         `ContractTwo year` + PaperlessBillingYes + `PaymentMethodElectronic check` + 
                         MultipleLinesYes + `InternetServiceFiber optic` + InternetServiceNo + 
                         OnlineBackupYes + DeviceProtectionYes + StreamingTVYes + 
                         StreamingMoviesYes, family = binomial, data = train)

summary(logistic_model4)
vif(logistic_model4)



#Remove Deviceprotectionyes which is insignificant
#and also reducing the AIC
logistic_model5 <- glm(formula = Churn ~ tenure + 
                         SeniorCitizen1 + PhoneServiceYes + `ContractOne year` + 
                         `ContractTwo year` + PaperlessBillingYes + `PaymentMethodElectronic check` + 
                         MultipleLinesYes + `InternetServiceFiber optic` + InternetServiceNo + 
                         OnlineBackupYes +  StreamingTVYes + 
                         StreamingMoviesYes, family = binomial, data = train)

summary(logistic_model5)
vif(logistic_model5)


#Remove onlinebackupyes which is insignificant
#and also reducing the AIC
logistic_model6 <- glm(formula = Churn ~ tenure + 
                         SeniorCitizen1 + PhoneServiceYes + `ContractOne year` + 
                         `ContractTwo year` + PaperlessBillingYes + `PaymentMethodElectronic check` + 
                         MultipleLinesYes + `InternetServiceFiber optic` + InternetServiceNo + 
                          StreamingTVYes + 
                         StreamingMoviesYes, family = binomial, data = train)

summary(logistic_model6)
vif(logistic_model6)



# Make the final logistic regression model.
#Remove streamingTVyes which is insignificant
lr_final_model <- glm(formula = Churn ~ tenure + 
                        SeniorCitizen1 + PhoneServiceYes + `ContractOne year` + 
                        `ContractTwo year` + PaperlessBillingYes + `PaymentMethodElectronic check` + 
                        MultipleLinesYes + `InternetServiceFiber optic` + InternetServiceNo + 
                        StreamingMoviesYes, family = binomial, data = train)

summary(lr_final_model)
vif(lr_final_model)


#Further removing the variables are increasing the AIC
#And all are significant. So this is considered as final model


# c-statistic and KS -statistic
#Calculate the predicted probabilities of train churn status
predicted_prob <- predict(lr_final_model,type="response")
#calculate the c-statistic
#Higher value of c-statistic indicate that the model is good
rcorr.cens(predicted_prob,train$Churn)

#Utility of the model will be determined by the results from test dataset
#calculate the c-statistic
#Higher value of c-statistic indicate that the model is good
predicted_probs <- (predict(lr_final_model,newdata=test[,-4],type="response"))
rcorr.cens(predicted_probs,test$Churn)
#c-statistic of 0.846 indicates that the model is good

#calculate the KS statistic
#create 'predictions' object
model_score <- prediction(predicted_prob,train$Churn)
#Evaluate the performace of the model
model_perf <- performance(model_score, "tpr", "fpr")
#plot the model
plot(model_perf)
#subtract the cumulative TPR from cumulative FPR
ks_table <- attr(model_perf, "y.values")[[1]] - (attr(model_perf, "x.values")[[1]])
#Find out the value with max distance, which is the KS statistic
ks = max(ks_table)
#Find the decile in which the max value occurs
which(ks_table == ks)/nrow(train)


# calculating auc
#plot the ROC curve for the test data
#ROC_churn <- performance(model_score_test,measure="tpr",x.measure="fpr")
#plot(ROC_churn,col="red",label=c(10,10,10))
#auc <- performance(model_score_test,measure = "auc")
#lr_auc <- auc@y.values[[1]]
#AUC is 84.6% 


#ConfusionMatrix For Threshold level 0.3 
#confusionMatrix(as.numeric(predicted_probs > 0.3),test$Churn, positive = "1")
#For the threshold level of 0.3, the sensitivity is 78.25% and accuracy of the model is 76.9%
#specificity is 76.50%

#ConfusionMatrix For Threshold level 0.5 
#confusionMatrix(as.numeric(predicted_probs > 0.5),test$Churn, positive = "1")
#For the threshold level of 0.5, the sensitivity is 54.01% and accuracy of the model is 80.95%
#specificity is 90.70%

#ConfusionMatrix For Threshold level 0.7 
#confusionMatrix(as.numeric(predicted_probs > 0.7),test$Churn, positive = "1")
#For the threshold level of 0.7, the sensitivity is 17.11% and accuracy of the model is 76.78%
#specificity is 98.38%


##########################################################################################

# SVM:
# Bring the data in the correct format to implement the SVM algorithm.
#Data is in the correct format for implementing svm


# Implement the SVM algorithm using the optimal cost.
tune.svm = tune(svm,as.factor(Churn)~.,data=train,
             kernel="linear",ranges = list(cost=c(0.0001,0.001,0.01,0.1,1,10,100,1000)))
#check the best model from tune function
tune.svm$best.model
#check the summary for the svm model
summary(tune.svm)
#predict the class labels using the best modelfrom tune function
svm_predict <- predict(tune.svm$best.model,test)
#calculate the confusion matrix for the best model
table(predicted=svm_predict,truth=test$Churn)
confusionMatrix(svm_predict,test$Churn,positive='1')

#use the parameter from the tune function and check the confusion matrix
svm.model_1 <- svm(as.factor(Churn)~.,data=train,
                   kernel="linear",cost=0.01,scale = FALSE,decision.values=T,probability=TRUE)
svm1_predict <- predict(svm.model_1,test,decision.values=T,probability=T)
confusionMatrix(svm1_predict,test$Churn,positive='1')
#Accuracy is 80.57%, sensitivity is 50.09% and specificity is 91.61%


#Tune the model using train function and radial kernel and check the results
tune_ksvm <- train(as.factor(Churn)~.,data=train,method="svmRadial",
                   tuneGrid=expand.grid(sigma=c(0.001,0.01,0.1,1,0.0001),C=c(0.01,0.1,1,20,21,22,23,24,25,26,27,28,29,1,10,30,40,50)),
                   metric="Accuracy",trControl=trainControl(method='repeatedcv',
                                                            number=5,repeats=10))

#check the results from the tuning of kvsm final model
tune_ksvm$finalModel
ksvm_predict <- predict(tune_ksvm,test)
confusionMatrix(ksvm_predict,test$Churn,positive ='1')
#The results from linear kernel is better than Radial kernel


#Use the kernlab package and use linear kernel and check the results
ksvm_linear <- ksvm (as.factor(Churn)~.,data=train,kernel="vanilladot",prob.model=T,
                     C=0.01)
ksvm_predict <- as.data.frame(predict(ksvm_linear,test,type="probabilities"))
ksvm_predict_1 <- predict(ksvm_linear,test,type='response')
confusionMatrix(ksvm_predict_1,test$Churn,positive = '1')

#As the objective is to predict the churning customer
#sensitivity of the model should be high
#From all these models the best result is from the ksvm function using linear kernal
#The final results are as follows:-
confusionMatrix(ksvm_predict_1,test$Churn,positive = '1')
#Accuracy is 0.8033, sensitivity is 0.5348, specificity is 0.9006


#Function for creating a prediction object, plotting ROC curve and AUC
rocplot <- function(pred, truth, ...){
  predob =  prediction(pred, truth)
  perf = performance(predob, 'tpr', 'fpr')
  plot(perf, colorize=T,...)
  abline(a=0,b=1,lwd=2,lty=2)
  print(performance(predob,'auc'))
}

#plot the ROC curve and see the auc
rocplot(ksvm_predict[,2],test$Churn)
#The AUC is 84.26%


#############################################################################################
#KNN

# Bring the data in the correct format to implement K-NN model.
str(churn)
#Data is in the correct format required for KNN
#Churn attribute will be considered as factor in the model itself
#Find the optimal value of K using cross validation
#computation time of this code is lenghty. 
knn_model_optk <- train(as.factor(Churn)~.,data=train,method='knn',
                   tuneGrid=expand.grid(k=1:50),metric="Accuracy",
                  trControl = trainControl(method = 'repeatedcv',
                                          number=10,repeats = 15))

#From the above cross-validation method it was found that 
#the optimal value of k is 31.
#Plot the model to see the graph for accuracy
plot(knn_model_optK)

# Implement the K-NN model for optimal K.
knn_model <- knn(train[,-4],test[,-4],train$Churn,k=31,prob=TRUE)
#see the confusion matrix for the KNN model
table(knn_model,test[,4])
confusionMatrix(knn_model, test[,4], positive ="1")
#Accuracy of the model is 0.7957,
#Sensitivity is 0.5579
#specificity is 0.8819
#convert the predicted probability of winning class to the positive class
attr(knn_model,"prob") <- ifelse(knn_model==1,attr(knn_model,"prob"),1 - attr(knn_model,"prob"))
#create the prediction object for knn model
knn_pred <- prediction(attr(knn_model,"prob"),test[,"Churn"])
knn_pref <- performance(knn_pred,"tpr","fpr")
#plot the ROC curve for knn model
plot(knn_pref,col="red",lty=3,lwd=3,colorize=T)
abline(a=0,b=1,lty=2,lwd=2)
#see the area under the curve for knn model
knn_auc <- performance(knn_pred,"auc")
#AUC for the KNN model is 83.32%

#################End of Case Study ########################################################











