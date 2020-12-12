#classify the Size_Categorie using SVM

#month	month of the year: 'jan' to 'dec'
#day	day of the week: 'mon' to 'sun'
#FFMC	FFMC index from the FWI system: 18.7 to 96.20
#DMC	DMC index from the FWI system: 1.1 to 291.3
#DC	DC index from the FWI system: 7.9 to 860.6
#ISI	ISI index from the FWI system: 0.0 to 56.10
#temp	temperature in Celsius degrees: 2.2 to 33.30
#RH	relative humidity in %: 15.0 to 100
#wind	wind speed in km/h: 0.40 to 9.40
#rain	outside rain in mm/m2 : 0.0 to 6.4
#Size_Categorie 	the burned area of the forest ( Small , Large)


# Loading the required packages
library(kernlab)
library(caret)
# Loading the Forest Fire Dataset
FF <-read.csv(file.choose())

View(FF)
str(FF)
summary(FF)
# After viewing the summary, certain features are removed from dataset
FF <- FF[,c(-1,-2)]
View(FF)
FF <- FF[,-c(10:28)]
View(FF)
summary(FF)

# Normalization of the data
normalise <- function(x) {
  return((x - min(x)) / (max(x) - min(x))) 
}

# Normalizing the data except size_category

FF$FFMC <- normalise(FF$temp)
FF$DMC <- normalise(FF$rain)
FF$DC  <- normalise(FF$RH)
FF$ISI <- normalise(FF$wind)
FF$temp <- normalise(FF$temp)
FF$RH <- normalise(FF$RH)
FF$wind <- normalise(FF$wind)
FF$rain <- normalise(FF$rain)
FF$area <- normalise(FF$area)
summary(FF)
describe(FF)



# converting the size category to factor

FF$size_category <- factor(FF$size_category)
table(FF$size_category)
barplot(prop.table(table(FF$size_category)), col = rainbow(2))
# Data imbalance is found as the majority values in class variable is "small"

# Partitioning the data and different sampling techniques are applied due to data imbalance.
install.packages("ROSE")
library(ROSE)


set.seed(113)
FF_str <- createDataPartition(y = FF$size_category,p =0.70, list=F)
FF_train <- FF[FF_str,]
FF_test <- FF[-FF_str,]

# checking the dimension of the training and testing data sets
dim(FF_train)
dim(FF_test)

# Over, Under, Both and SMote techniques are used below for meodel
FF_train_over <- ovun.sample(size_category ~., data = FF_train, method ="over", N = 500)$data
table(FF_train_over$size_category)
FF_train_under <- ovun.sample(size_category~., data = FF_train, method = "under", N = 278)$data
table(FF_train_under$size_category)
FF_train_both <- ovun.sample(size_category~., data = FF_train, method = "both", p = 0.5, seed = 222, N = 363)$data
table(FF_train_both$size_category)
FF_train_rose <- ROSE(size_category~., data = FF_train, N = 500, seed = 111)$data
table(FF_train_rose$size_category)

# out of the four results obtained from above, ROSE sample technique is better, hence it is chosen for models




#Training a model on the data ----
# a simple linear SVM
model1<- ksvm(size_category ~.,data= FF_train, kernel = "vanilladot")
model1 # Training error - 0.063, Support Vectors - 109
modelR <- ksvm(size_category~.,data = FF_train_rose, kernel ="vanilladot")
modelR # Training error - 0.334, Support Vectors - 415


## Evaluating model performance ----
# predictions on testing dataset
Area_pred <- predict(model1, FF_test)
confusionMatrix(Area_pred,FF_test$size_category)
# Accuracy is high - 0.95 % however, 
# No Information Rate : 0.7338 which means by default 73% without model applying, size_category will be small as it is majority in class
# Sensitivity : 0.8293 which means it is giving 82% correct prediction for size_category = 'large'
# Specificity : 1.0000 which means it is giving 100% correct prediction for size_category ='small'       

# Predictions on ROSE technique applied on training dataset
Area_pred <- predict(modelR, FF_test)
confusionMatrix(Area_pred,FF_test$size_category)
# Accuracy is reduced to 0.6818         
# No Information Rate : 0.7338 remains same as above
# Sensitivity : 0.5854         
# Specificity : 0.7168       



#Improving model performance ----
model_rfdot<-ksvm(size_category~.,data= FF_train,kernel = "rbfdot")
model_rfdot # Training error - 0.16 , Support Vectors - 199

model_rfdotR<-ksvm(size_category~.,data= FF_train_rose,kernel = "rbfdot")
model_rfdotR # Training error - 0.09, Support Vectors - 297



pred_rfdot<-predict(model_rfdot,FF_test)
confusionMatrix(pred_rfdot, FF_test$size_category)
# Accuracy is 0.87         
# No Information Rate : 0.7338 remains same as above
# Sensitivity : 0.53 
# Specificity : 1.00  


pred_rfdot<-predict(model_rfdotR,FF_test)
confusionMatrix(pred_rfdot, FF_test$size_category)
# Accuracy is 0.79        
# No Information Rate : 0.7338 remains same as above
# Sensitivity : 0.34 
# Specificity : 0.96  




# Improving model performance ----
#   By using the non-linear model "besseldot"
model_besseldot<-ksvm(size_category~., data= FF_train,kernel = "besseldot")
model_besseldot # Training error - 0.17 , Number of Support Vectors - 188

model_besseldotR<-ksvm(size_category~., data= FF_train_rose,kernel = "besseldot")
model_besseldotR # Training error - 0.108, Number of Support Vectors - 282


pred_bessel<-predict(model_besseldot,FF_test)
confusionMatrix(pred_bessel,FF_test$size_category)
# Accuracy is 0.87         
# No Information Rate : 0.7338 remains same as above
# Sensitivity : 0.53 
# Specificity : 1.00  



pred_bessel<-predict(model_besseldotR,FF_test)
confusionMatrix(pred_bessel,FF_test$size_category)
# Accuracy is 0.78        
# No Information Rate : 0.7338 remains same as above
# Sensitivity : 0.36
# Specificity : 0.93 




## Further improving model performance by using plydot:
model_poly<-ksvm(size_category~.,data= FF_train,kernel = "polydot")
model_poly # Training error - 0.063 , Number of Support Vectors - 109

model_polyR<-ksvm(size_category~.,data= FF_train_rose,kernel = "polydot")
model_polyR # Training error - 0.334, Number of Support Vectors - 414



pred_poly<-predict(model_poly,FF_test)
confusionMatrix(pred_poly,FF_test$size_category)

# Accuracy is 0.95       
# No Information Rate : 0.7338 remains same as above
# Sensitivity : 0.82
# Specificity : 1.00 


pred_poly<-predict(model_polyR,FF_test)
confusionMatrix(pred_poly,FF_test$size_category)

# Accuracy is 0.68      
# No Information Rate : 0.7338 remains same as above
# Sensitivity : 0.58
# Specificity : 0.72 

# Conclusion:
# Depending on the prediction interest in predicting large areas of forest fire, Polydot model is good 
# Accuracy after applying sampling technique is 0.68
# Model is able to predict 58% correct predictions for larger areas
# Model is able to predict 72% correct preidctions for smaller areas

