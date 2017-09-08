rm(list=ls())
setwd(choose.dir())
library(XLConnect)
# read data from part 1 and part 2 of the sheets
Part1 <- readWorksheetFromFile("German_CreditRating.xls",sheet=2)
Part2 <- readWorksheetFromFile("German_CreditRating.xls",sheet=3)
names(Part1)
names(Part2)
#merge them by observation ids
CustData <- merge(Part1,Part2,by="OBS",all.x=TRUE) 

# structure of data types
names(CustData)
str(CustData)
#note there are few missing observations
summary(CustData)
#CustData = CustData[,-1]
#type conversion
num_vars = c(2,9,14,18,19,23)
#Catdata <- CustData[,c(3:8, 10:13, 15:17, 20:22, 24:29)]
Catdata = CustData[,-c(1,num_vars)]
Catdata <- data.frame(apply(Catdata,2,factor))
str(Catdata)
NumData<- data.frame(CustData[,c(2,9,14,18,19,23)])
NewData <- cbind(Catdata,NumData)
str(NewData)
summary(NewData)

#check for missing values
sum(is.na((NewData)))
which(apply(NewData, 1, function(x){sum(is.na(x))})!=0)
dim(NewData)

Full_data = na.omit(NewData)
dim(Full_data)

# #Knn imputation
# library(DMwR)
# data_imputed<-knnImputation(NewData,k=1) #KNN Imputation
# sum(is.na(data_imputed))

table(Full_data$RESPONSE)

# #Split the data into train and test data sets
# data_imputed = Full_data
# rows=seq(1,nrow(data_imputed),1)
# set.seed(123)
# trainRows=sample(rows,(70*nrow(data_imputed))/100)
# train = data_imputed[trainRows,] 
# test = data_imputed[-trainRows,]
# names(train)

library(caret)
train_rows = createDataPartition(y = Full_data$RESPONSE, 
                                 p=0.7, list = F) 
train = Full_data[train_rows,]
test = Full_data[-train_rows,]
table(Full_data$RESPONSE)
table(train$RESPONSE)
table(test$RESPONSE)
#logistic regression model
# dependent variable = Churned
LogReg <- glm(RESPONSE ~., data=train,family=binomial)
summary(LogReg)
residuals(LogReg)
fitted(LogReg)
#deviance(LogReg)

table(train$RESPONSE)
table(test$RESPONSE)
#To get the significance for the overall model we use the following command
# 1-pchisq(deviance(LogReg), df=df.residual(LogReg))

# now let us select features using StepAIC
library(MASS)
step = stepAIC(LogReg, direction="both")
step$anova

BestModel <- glm(RESPONSE ~ NEW_CAR + USED_CAR + EDUCATION + GUARANTOR + OTHER_INSTALL + 
                   OWN_RES + FOREIGN + CHK_ACCT + HISTORY + SAV_ACCT + EMPLOYMENT + 
                   PRESENT_RESIDENT + DURATION + INSTALL_RATE + AGE + AMOUNT, data=train, family=binomial)
summary(BestModel)
coefficients(BestModel)
vif(BestModel)
#interpretation
#What is the equation?
#log(p/1-p) = 2.32 -.073*NewCar1+0.018*UserCar1-...+0.00269*Age-0.00000737*Amount
#For every 1 unit change in Age the log odds of response = credit rating is good
#(versus non-response) increases by 0.0267
#Having the purpose of credit Newcar1 = 1 versus not,changes the 
#log odds of Credit rating (good) by -0.0735  

#goodness of fit
#1-pchisq(deviance(BestModel), df=df.residual(BestModel))
#the higher the better and evidence to reject the 
#hypothesis that the fitted model is correct

dev.off()
plot(train$RESPONSE, BestModel$fitted.values, type="p")
abline(0.5,0)
prob<-predict(BestModel, type="response")
pred_class <- factor(ifelse(prob>0.7, 1, 0))
metrics = table(train$RESPONSE,pred_class)
metrics
accuracy =(metrics[1,1]+metrics[2,2])/(length(pred_class)) 
Recall = metrics[2,2]/(metrics[2,2]+metrics[2,1])
Precision = metrics[2,2]/(metrics[2,2]+metrics[1,2])
accuracy
Recall
Precision
 
### on test data
probt <- predict(BestModel, newdata = test, type="response")
predt_class <- factor(ifelse(probt>0.5, 1, 0))
metricst = table(test$RESPONSE,predt_class)
metricst
accuracyt =(metricst[1,1]+metricst[2,2])/(length(predt_class)) 
Recallt = metricst[2,2]/(metricst[2,2]+metricst[2,1])
Precisiont = metricst[2,2]/(metricst[2,2]+metricst[1,2])
accuracyt
Recallt
Precisiont

library(ROCR)
library(ggplot2)
pred <- prediction(prob, train$RESPONSE)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
#perf
plot(perf, col=rainbow(10), 
     colorize=T, 
     print.cutoffs.at = seq(0,1,0.1))
auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]
auc

roc.data <- data.frame(fpr=unlist(perf@x.values),
                      tpr=unlist(perf@y.values),
                      model="GLM")
ggplot(roc.data, aes(x=fpr, ymin=0, ymax=tpr)) +
  geom_ribbon(alpha=0.2) +
  geom_line(aes(y=tpr)) +
  ggtitle(paste0("ROC Curve w/ AUC=", auc))







