# Read the dataset
zoo <- read.csv(file.choose())
View(zoo)

# droping the animal name 
zoo <- zoo[,2:18]
str(zoo)

# converting the fields to factor 

zoo$hair <- as.factor(zoo$hair)
zoo$feathers <- as.factor(zoo$feathers)
zoo$eggs <- as.factor(zoo$eggs)
zoo$milk <- as.factor(zoo$milk)
zoo$airborne <- as.factor(zoo$airborne)
zoo$aquatic <- as.factor(zoo$aquatic)
zoo$predator <- as.factor(zoo$predator)
zoo$toothed <- as.factor(zoo$toothed)
zoo$backbone <- as.factor(zoo$backbone)
zoo$breathes <- as.factor(zoo$breathes)
zoo$venomous <- as.factor(zoo$venomous)
zoo$fins <- as.factor(zoo$fins)
zoo$legs <- as.factor(zoo$legs)
zoo$tail <- as.factor(zoo$tail)
zoo$domestic <- as.factor(zoo$domestic)
zoo$catsize <- as.factor(zoo$catsize)
zoo$type <- as.factor(zoo$type)
View(zoo)

#create training and test datasets

library(caret)
library(C50)
library(pROC)
library(mlbench)
library(lattice)

set.seed(7)
  
zoo_str <-createDataPartition(zoo$type,p=0.70,list=F)
zoo_str
zoo_train <- zoo[zoo_str,]
zoo_test <- zoo[-zoo_str,]


# create labels for training and test data
  
zoo_train_labels <- zoo_train[, 17]
zoo_test_labels <- zoo_test[, 17]

# selecting the optimal k-value
trcontrol <- trainControl(method = "repeatedcv", number = 10,repeats = 3)
set.seed(222)
fit <- train(type ~., data = zoo_train, method = 'knn', tuneLength = 20,
trControl = trcontrol, preProc = c("center","scale"))

fit
plot(fit)
varImp(fit)
  
pred <- predict(fit, newdata = zoo_test)
confusionMatrix(pred, zoo_test$type)
  
# Build a KNN model on taining dataset
install.packages("class")
install.packages("gmodels")
library("class")
library("gmodels")

# Building the KNN model on training dataset with k = 7

zoo_pred <- knn(train = zoo_train[,-17], test = zoo_test[,-17], cl = zoo_train_labels, k=7)
CrossTable(x = zoo_test_labels, y = zoo_pred, prop.chisq = FALSE)
result <- data.frame(zoo_pred,zoo_test_labels)
confusionMatrix(zoo_pred,zoo_test_labels)
View(result)

# Building the KNN model on training dataset with k = 5

zoo_pred1 <- knn(train = zoo_train[,-17], test = zoo_test[,-17], cl = zoo_train_labels, k=5)

CrossTable(x = zoo_test_labels, y = zoo_pred, prop.chisq = FALSE)

result <- data.frame(zoo_pred1,zoo_test_labels)
confusionMatrix(zoo_pred1,zoo_test_labels)
View(result)

# Building the KNN model on training dataset with k = 2

zoo_pred2 <- knn(train = zoo_train[,-17], test = zoo_test[,-17], cl = zoo_train_labels, k=2)
CrossTable(x = zoo_test_labels, y = zoo_pred, prop.chisq = FALSE)
result <- data.frame(zoo_pred2,zoo_test_labels)
confusionMatrix(zoo_pred2,zoo_test_labels)
View(result)



