#load the data
glass <- read.csv(file.choose())
View(glass)
# see proportion % of class variable
prop.table(table(glass$Type))*100
#creating training and test data
colnames(glass)
str(glass)

glass$Type <- as.factor(glass$Type)
table(glass$Type)

library (caret)
library(C50)
set.seed(7)
glass$Type
glasstr<-createDataPartition(glass$Type,p=0.70,list=F)
glass_train<-glass[glasstr,]
glass_test<-glass[-glasstr,]

# create labels for training and test data
glass_train_label <- glass_train[,10]
glass_test_label <- glass_test[,10]
#TRaining a model
#load class library

install.packages("class")
install.packages("gmodels")
library(class)
library(gmodels)
glass_test_pred <-knn(train = glass_train[,-10],test = glass_test[,-10],cl = glass_train_label,k=2)
table(glass_test_label,glass_test_pred)






library(caret)


# creating another model using grid search method
ctrl<-trainControl(method="repeatedcv",repeats=10)
my_knn_model <-train(Type ~.,method ="knn",data = glass_train,trControl=ctrl,tuneGrid = expand.grid(k=c(2,3,5,7,9,10,13,15,17)))
my_knn_model
plot(my_knn_model)

pred <- predict(my_knn_model, newdata = glass_test)
confusionMatrix(pred, glass_test$Type)
