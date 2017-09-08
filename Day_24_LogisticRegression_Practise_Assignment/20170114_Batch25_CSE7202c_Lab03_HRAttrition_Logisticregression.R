# Data pre-processing 
# Splitting the data into Train and Test
# Building the first logistic regression
# Model summary and interpret the output
# Model Evaluation
# Creating confusion matrix and 
# Chosing the threshold value
# Computing the error metrics
# Constructing  the ROC Curves
# Step AIC
# VIF 
# Model performance tuning
# Startified random samping

####HR attrition data
rm(list=ls(all=TRUE))

# data<-read.csv("HR_Attrition.csv",header=T,sep=",")
data<-read.csv("HR_Attrition1.csv",header=T,sep=",")

names(data)
a=setdiff(names(data),c("Age","DistanceFromHome","MonthlyIncome",
                        "NumCompaniesWorked","PercentSalaryHike",
                        "StandardHours","TotalWorkingYears",
                        "TrainingTimesLastYear","YearsAtCompany",
                        "YearsInCurrentRole","YearsSinceLastPromotion",
                        "YearsWithCurrManager"))
b=setdiff(names(data),a)

cat_data<-data[which(names(data)%in% a)]
num_data<-data[which(names(data)%in% b)]
rm(a,b)


cat_data<- data.frame(apply(cat_data,2,as.factor))
cat_data<- data.frame(EmployeeNumber=cat_data[,6],
                     cat_data[,c(-6)])
names(cat_data)
data1<-cbind(cat_data,num_data)

#Removing EmployeeNumber, Over18, standardhours
Final_data<-data1[,-c(1,14,25)] 

names(Final_data)
table(Final_data$Attrition)

#Split into train and test
table(Final_data$Attrition)

rows<-seq(1,nrow(Final_data),1)
set.seed(1234)
trainrows<-sample(rows,0.7*nrow(Final_data))
train<-Final_data[trainrows,]
test<-Final_data[-trainrows,]
names(train)


##Logistic Regression
mod_lm<-glm(Attrition~Gender,
            data=train,family="binomial")

summary(mod_lm)

mod_lm<- glm(train$Attrition~DistanceFromHome+Gender+WorkLifeBalance,
            data=train,family="binomial")

summary(mod_lm)
# Ensure you pass the family , else you will see 
# the errors
# mod_lm<-glm(Attrition~.,data=train)
# 
summary(train)
mod_lm <- glm(Attrition~.,data=train,
            family="binomial")
summary(mod_lm)

PredictRaw <- predict(object = mod_lm, 
                      newdata = train[,-1])

summary(PredictRaw)
PredictRaw <- predict(object = mod_lm, 
                   newdata = train[,-1],
                   type="response")
str(train)
write.csv(PredictRaw,"results11.csv")
# if you are not specifying the "type=response",
head(PredictRaw)

dev.off()
plot(train$Attrition,PredictRaw, type="p")
abline(0.25,0)

threshold <- 0.25
Predict <- predict(object = mod_lm, 
                   newdata = train[,-1])

PredictRaw[PredictRaw>threshold]=1
PredictRaw[PredictRaw<=threshold]=0
summary(mod_lm)
names(Final_data)
##Test Model efficency using confusion matrix
# build confusion matrix
table(train$Attrition)
ConfusionMatrix <- table(train[,1], PredictRaw)
ConfusionMatrix
LRacc = sum(diag(ConfusionMatrix))/nrow(train)
LRrec = ConfusionMatrix[2,2]/sum(ConfusionMatrix[2,])
LRpre = ConfusionMatrix[2,2]/sum(ConfusionMatrix[,2])
print(c("accuracy"=LRacc,"precision"=LRpre,
        "recall"=LRrec,"threshold"=threshold))

#####
# Applying on the test data 
pred<-predict(object = mod_lm, 
              newdata = test[,-1],type="response")

summary(pred)
pred[pred>threshold]=1
pred[pred<=threshold]=0
ConfusionMatrix <- table(test[,1], pred)
ConfusionMatrix
LRacc = sum(diag(ConfusionMatrix))/nrow(test)
LRrec = ConfusionMatrix[2,2]/sum(ConfusionMatrix[2,])
LRpre = ConfusionMatrix[2,2]/sum(ConfusionMatrix[,2])
print(c("accuracy"=LRacc,"precision"=LRpre,"recall"=LRrec,"threshold"=threshold))

library(car)
vif(mod_lm)

library(ROCR)
# to make an ROC curve one needs 
#actual values and predicted values, 
# both are given below.
ROCRpred = prediction(mod_lm$fitted.values,train$Attrition)
# Performance function
ROCRperf  = performance(ROCRpred, "tpr", "fpr")

# Plot ROC curve
plot(ROCRperf)
plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), 
     text.adj=c(-0.2,1.7))
attributes(performance(ROCRpred, 'auc'))$y.values[[1]]


##stepaic
library(MASS)
step = stepAIC(mod_lm, direction="backward")
step = stepAIC(mod_lm, direction="both")

BestModel <- glm(train$Attrition ~ BusinessTravel + Department + EducationField + 
                   EnvironmentSatisfaction + Gender + JobInvolvement + JobLevel + 
                   JobSatisfaction + MaritalStatus + OverTime + RelationshipSatisfaction + 
                   StockOptionLevel + WorkLifeBalance + Age + DistanceFromHome + 
                   NumCompaniesWorked + PercentSalaryHike + TotalWorkingYears + 
                   TrainingTimesLastYear + YearsAtCompany + YearsInCurrentRole + 
                   YearsSinceLastPromotion + YearsWithCurrManager, data=train, family=binomial)
vif(BestModel)
summary(BestModel)

#Smoting the data as there exists class imbalance
# 300 perc.over means increasing the
# class level by 4 times 
library(DMwR)
set.seed(4321)
data_smote<-SMOTE(Attrition~.,
                  data=Final_data,perc.over=300)

summary(data_smote$Attrition)
summary(Final_data$Attrition)

### on smote data
#Split into train and test
rows<-seq(1,nrow(data_smote),1)
set.seed(1234)
trainrows<-sample(rows,0.7*nrow(data_smote))
train<-data_smote[trainrows,]
test<-data_smote[-trainrows,]

##Applying Logistic regression and decision trees on the smoted data. we can
##reuse the code from line 28 to 65 to get results.
mod_lm<-glm(train$Attrition~.,data=train,family="binomial")


pred<-predict(object = mod_lm, 
              newdata = train[,-1],type="response")

pred[pred>0.5]=1
pred[pred<=0.5]=0
ConfMat <- table(train[,1], pred)
ConfMat

pred<-predict(object = mod_lm, 
              newdata = test[,-1],type="response")

pred[pred>0.5]=1
pred[pred<=0.5]=0
ConfMat <- table(test[,1], pred)
ConfMat


##Observe the precision and recall
##Logistic regression
#      pred
#      0   1
# No  378  69
# Yes  77 187

##Stratified sampling
strat_train<-data.frame()
strat_test<-data.frame()
str_data<-Final_data
str_data$Attrition<-ifelse(str_data$Attrition=="0",0,1)
for(i in 0:1){
  set<-str_data[str_data$Attrition==i,]
  rows<-seq(1,nrow(set),1)
  set.seed(1234)
  trainrows<-sample(rows,nrow(set)*0.7)
  Train_Data<-set[trainrows,]
  Test_Data<-set[-trainrows,]
  strat_train<-rbind(strat_train,Train_Data)
  strat_test<-rbind(strat_test,Test_Data)
}
##Logistic Regression
mod_lm<-glm(strat_train$Attrition~.,
            data=strat_train,family="binomial")
Predict <- predict(object = mod_lm, 
                   newdata = strat_train[,-1],type="response")

Predict[Predict>0.5]=1
Predict[Predict<=0.5]=0

##Test Model efficency using confusion matrix
# build confusion matrix
ConfusionMatrix <- table(strat_train[,1], Predict)
ConfusionMatrix

pred<-predict(object = mod_lm, 
              newdata = strat_test[,-1],type="response")
pred[pred>0.5]=1
pred[pred<=0.5]=0
ConfMat <- table(strat_test[,1], pred)
ConfMat



#### 
library(ROCR)
# to make an ROC curve one needs actual values and predicted values, both are given below.
# These functions will do the groupings on their own (p > 0.2, etc.) like we were doing above
ROCRpred = prediction(Predict,strat_train$Attrition)
str(ROCRpred)
# Performance function
ROCRperf = performance(ROCRpred, "tpr", "fpr")
ROCRperf
# Plot ROC curve
plot(ROCRperf)

# Add colors
plot(ROCRperf, colorize=TRUE)

# Add threshold labels 
plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), 
     text.adj=c(-0.2,1.7))
attributes(performance(ROCRpred, 'auc'))$y.values[[1]]
auc(train$Attrition,Predict)

##################################
summary(train)
mod_lm <- glm(Attrition~1,data=train,
              family="binomial")

##stepaic
library(MASS)
step = stepAIC(mod_lm, direction="forward",scope = list(lower=mod_lm,upper=~Attrition+BusinessTravel+Department+Education))

