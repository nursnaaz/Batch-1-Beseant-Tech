# Telecom Churn Prediction Using KNN, SVM, Logistic Regression and Naive Bayes

### Company Information:
A telecom company called ‘Firm X’ is a leading telecommunications provider in the country. The company earns most of its revenue by providing internet services. Based on the past and current customer information, the company has maintained a database containing personal/demographic information, the services availed by a customer and the expense information related to each customer.
 
### Problem Statement:
 ‘Firm X’ has a customer base set across the country. In a city ‘Y’, which is a significant revenue base for the company, due to heavy marketing and promotion schemes by other companies, this company is losing customers i.e. the customers are churning. 
Whether a customer will churn or not will depend on data from the following three buckets:-
* Demographic Information
* Services Availed by the customer
* Overall Expenses

### The goal of this case study:
To develop predictive models using each of the 4 models namely K-NN, Naive Bayes, Logistic Regression and SVM
 
#### Step-1: Data Understanding and Preparation of Master File.
* Load the 3 files (Churn_data, customer_data, internet_data) given in separate data frames.
* Collate the data together in one single file.
 
#### Step -2: Exploratory Data Analysis
* Plot bar charts displaying the relationship between the target variable and various other features and report them.
 
#### Step -3: Data Preparation
* Perform de-duplication of data.
* Bring the data in the correct format
* Find the variables having missing values and impute them.  
* Perform outlier treatment 

#### Checkpoint 4: Model Building
##### Model - K-NN:
Data Prep:
Prepare data for K-NN (convert categorical variables to numeric)
Modelling and Evaluation.
Build a K-NN model using the optimal K
Reporting the performance metrics - Accuracy, Sensitivity, Specificity and AUC.  

##### Model - Naive Bayes:
Data Prep:
Making sure that the variables have the correct data type.
Modelling:
Building the Naive Bayes model.
Reporting the performance metrics - Accuracy, Sensitivity, Specificity and AUC.

#### Model -Logistic Regression:
Data Prep:
Prepare the data for logistic regression. 
Modelling: Part 1: Keep the probability threshold as 0.3, 0.5 and 0.7 respectively.
Perform Variable Selection
Reporting the final logistic regression model.
Reporting the performance metrics - Accuracy, Sensitivity, Specificity and AUC, KS-Statistic and C-Statistic.

#### Model - SVM:
Data Prep:
Prepare the data for SVM.
Modelling:
Make the SVM model by finding the optimal value of ‘cost’ and report its performance metrics - Accuracy, Sensitivity, Specificity and AUC.
 
As a final result, reporting the best model for each algorithm along with the most impactful variables and its performance metrics (wherever required).


**A presentation (Churn) and a word document is also included which further elobarates the procedure followed and analyses the result**





