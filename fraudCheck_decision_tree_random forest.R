#fraud check decision tree problem and random forests

library(caret)
install.packages("tree")
library(tree)
library(C50)

fraud_check <- read.csv(file.choose())
View(fraud_check)

# EDA processing
str(fraud_check)
attach(fraud_check)

summary(fraud_check)


tax_inc = ifelse(fraud_check$Taxable.Income <= 30000, "Risky", "Good")
View(tax_inc)
fraud_check <- data.frame(fraud_check,tax_inc)
fraud_check <- fraud_check[1:600,-3]
fraud_check$tax_inc = factor(fraud_check$tax_inc)
fraud_check$Undergrad = factor(fraud_check$Undergrad)
fraud_check$Marital.Status = factor(fraud_check$Marital.Status)
fraud_check$Urban = factor(fraud_check$Urban)
str(fraud_check)


#split data in training n test set
library(caTools)
set.seed(123)
split = sample.split(fraud_check$tax_inc, SplitRatio = 0.75)
FC_train = subset(fraud_check, split == TRUE)
FC_test = subset(fraud_check, split == FALSE)

#The rpart algorithm works by splitting the dataset recursively, 
#which means that the subsets that arise from a split are further split 
#until a predetermined termination criterion is reached.

# Fitting Decision Tree Classification to the Training set using rpart package
install.packages('rpart')
library(rpart)
install.packages("rpart.plot")
library(rpart.plot)
fraud_class = rpart(formula = tax_inc ~ ., data = FC_train, method = 'class')
summary(fraud_class)
# Predicting the Test set results
y_predict = predict(fraud_class, newdata = FC_test[,-6], type = 'class')

# Making the Confusion Matrix
confusionMatrix(FC_test$tax_inc,y_predict)  

rpart.plot(fraud_class, box.palette = "RdBu", shadow.col = "gray", nn = TRUE)

#model building using C50 package
c_model = C5.0(tax_inc ~., data = FC_train , method = 'class')
summary(c_model)

y_predict2 <- predict.C5.0(c_model,FC_test[,-6])
confusionMatrix(FC_test$tax_inc,y_predict2)  
plot(c_model)                 #model accuracy improved than previous model

# To further improve the model, bagging algorithm is applied 
# Bagging Application
library(ipred)
set.seed(300)
bagging_model <- bagging(tax_inc~.,data = FC_train, nbagg=25)
summary(bagging_model)
predict_bagging <- predict(bagging_model,FC_test[,-6])
confusionMatrix(FC_test$tax_inc,predict_bagging)
#model decrease in bagging at 0.74

# To check further improvement in accuracy, boosting algorithm was applied
# Boosting Application
install.packages("xgboost")
library(xgboost)
set.seed(123)
boosting_model<- C5.0(tax_inc~.,data = FC_train, trials=10)
summary(boosting_model)
predict_boost<-predict.C5.0(boosting_model,FC_test[,-6])
confusionMatrix(FC_test$tax_inc,predict_boost)
#model improve here at 0.79

#conclusion is model using C50 package and boosting application gives 
#good results compare to other models 



# Further Random Forest was applied



# Random Forest for further improvement in accuracy
install.packages("randomForest")
library(randomForest)
#ntree = 500
model_rf<-randomForest(tax_inc~., data=FC_train, ntree=500)
print(model_rf)
# OOB error was found to be 20.67%

#ntree = 800
model_rf<-randomForest(tax_inc~., data=FC_train, ntree=800)
print(model_rf)
# OOB error was found to be 20.89%

#ntree = 1000
model_rf<-randomForest(tax_inc~., data=FC_train, ntree=1000)
print(model_rf)
# OOB error was found to be 20.89%


# prediction

pred_rf<-predict(model_rf,FC_test[,-6])
confusionMatrix(FC_test$tax_inc,pred_rf)
plot(model_rf)
# Testing accuracy was found to be 0.76.


