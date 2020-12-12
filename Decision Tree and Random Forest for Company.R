# Loading the required packages
install.packages("caret")
install.packages("C50")
install.packages("gmodels")
install.packages("psych")
library(caret)
library(C50)
library(gmodels)
library(psych)
# Loading the company dataset
Company_Data<-read.csv(file.choose())
View(Company_Data)


# EDA process
str(Company_Data)
# graphic representation of Sales to check if normally distributed
hist(Company_Data$Sales)
summary(Company_Data)



# Defining the categorical data in the given data set
Company_Data$ShelveLoc<-as.factor(Company_Data$ShelveLoc)
Company_Data$Urban<-as.factor(Company_Data$Urban)
Company_Data$US<-as.factor(Company_Data$US)

pairs(Company_Data)
# The scatter diagram shows decreasing relationship between price and Sales, increasing relatioship between competitor price and Price

# calculate correlation coefficient
cor(Company_Data$Sales,Company_Data$Price)
cor(Company_Data$CompPrice,Company_Data$Price)

#linear regression with the dataset
attach(Company_Data)
mod1<-lm(Sales~., data=Company_Data)
summary(mod1)
# R^2 value = 0.87 # good model as R^2 value greater than 0.85
plot(mod1)



library(MASS)
stepAIC(mod1) # AIC value less than 10

sqrt(sum(mod1$residuals^2)/nrow(Company_Data)) # Root Mean Square Erro is 1.00

mod2<-lm(Sales~ CompPrice+Income+Advertising+Price+ShelveLoc+Age, data= Company_Data)
summary(mod2)
sqrt(sum(mod2$residuals^2)/nrow(Company_Data))

# Both the models give R^2 value is 0.87 and mean square error as 1.003

## Decision Tree Model

High = ifelse(Company_Data$Sales>10, "Yes", "No")
CD = data.frame(Company_Data, High)
View(CD)
CD1<-CD[,-1]
CD1$High<-as.factor(CD1$High)
View(CD1)

# Splitting data into training and testing
inTraininglocal<-createDataPartition(CD1$High,p=.70,list = F)
training<-CD1[inTraininglocal,]
testing<-CD1[-inTraininglocal,]
#Model Building
model<-C5.0(High~ .,data = training) 
#Generate the model summary
summary(model)
plot(model)

# Prediction
pred<-predict.C5.0(model,testing[,-11])
confusionMatrix(testing$High,pred)
plot(model)
# Accuracy of the model is 0.81
# Kappa Accuracy of the model is 0.31 

# To further improve the model accuracy, bagging algorithm is applied 

library(ipred)
set.seed(300)
bag_model<-bagging(High~.,data=training, nbagg=25)
summary(bag_model)
pred_bagg<-predict(bag_model,testing[,-11])
confusionMatrix(testing$High,pred_bagg)


# Boosting Application
library(xgboost)
set.seed(123)
boost_model<- C5.0(High~.,data=training, trials=15)
summary(boost_model)
plot(boost_model)
pred_boost<-predict.C5.0(boost_model,testing[,-11])
confusionMatrix(testing$High,pred_boost)
plot(boost_model)
# Testing Accuracy improved to 0.87




# Random Forest for further improvement in accuracy
install.packages("randomForest")
library(randomForest)
#ntree = 500
model_rf<-randomForest(High~., data=training, ntree=500)
print(model_rf)
# OOB error was found to be 13.17%

#ntree = 800
model_rf<-randomForest(High~., data=training, ntree=800)
print(model_rf)
# OOB error was found to be 14.23%

#ntree = 1000
model_rf<-randomForest(High~., data=training, ntree=1000)
print(model_rf)
# OOB error was found to be 13.17%


# prediction

pred_rf<-predict(model_rf,testing[,-11])
confusionMatrix(testing$High,pred_rf)
plot(model_rf)
# Testing accuracy was found to be 0.85.

# To find out further improvement model was prepared with Stacking algorithm
# Stacking
set.seed(123)
install.packages("mboost")
install.packages("caretEnsemble")
library(caretEnsemble)
library(mboost)
# In stacking algorithm, 5 algorithms were used at a time to check the best accuracy
# 5 algorithms are "rpart","glm","rf","treebag","glmbost"
algo_use <- c("rpart","glm","rf","treebag","glmboost")
model_stack<-caretList(High~.,data=training, methodList=algo_use, 
    trControl=trainControl("cv",number=10,savePredictions = T,classProbs = T))
summary(model_stack)
stacking_results<-resamples(model_stack)
summary(stacking_results)
# The best mean accuracy of 0.92 was observed in "glm" model.
# Therefore, the final model was chosen as "glm" model
final_stack <- caretStack(model_stack, method="glm",trControl=trainControl("cv",
                          number=10,savePredictions=T, classProbs=T))
pred_stack<- predict(final_stack,testing[,-11])
confusionMatrix(testing$High,pred_stack)
# Testing Accuracy was observed to be 0.92

# For above problem, following techniques were used
# Multiliner, Decision Tree, Boosting and Bagging, Random Forests and finally stacking with glm, rpart, treebag, boosting randomforest etc were applied.
# Finally the glm ( Generalised Linear Model) was found to be with very good accuracy 0.92
# The final model and predictions were prepared with "glm" model
