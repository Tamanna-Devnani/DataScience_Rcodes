# Predicting the burned area of Forest Fires using Nueral Network

# Load the forest fire data

# custom normalization function
normalize <- function(x) { 
  return((x - min(x)) / (max(x) - min(x)))
}
# apply normalization to entire data frame
forest<-read.csv(file.choose())
View(forest)

# EDA Part
str(forest)
summary(forest) 

# Removing the columns 1 & 2 as they seems insignificant in model 
forest <- forest[,c(-1,-2)]
summary(forest)
  
# Convert the size into categorical value and further to integer data type
 forest$size_category <- as.factor(forest$size_category)
 table(forest$size_category)
 category = ifelse(forest$size_category == "small", "0", "1")
 View(category)
 table(category)
 forest$size_category <- as.factor(category)
 forest$size_category <- as.integer(forest$size_category)
 View(forest)
 str(forest) 
 
 # Normalizing the data  
forest_norm <- as.data.frame(lapply(forest, normalize))
View(forest_norm)
str(forest_norm)


# Installation of Neural Network and other packages
install.packages(c("nnet","neuralnet", "caret", "ggplot2"))
library(nnet)
library(neuralnet)
library(caret)
library(ggplot2)


# plotting the data for checking normal distribution
attach(forest_norm)
ggplot(forest_norm) + geom_histogram(aes(temp), binwidth = 0.1) + ggtitle("Temperature") #The Temperature Data is Normally Distributed

ggplot(forest_norm) + geom_bar(aes(wind), width = 0.1) + ggtitle("Wind") #The Wind Data is Slightly Skewed on the right

ggplot(forest_norm) + geom_point(aes(FFMC, DMC)) 

ggplot(forest_norm) + geom_histogram(aes(area), binwidth = 0.5)



# create training and test data

set.seed(7)

forest_str <-createDataPartition(forest_norm$size_category,p=0.70,list=F)
forest_train <- forest_norm[forest_str,]
forest_test <- forest_norm[-forest_str,]

View(forest_train)
View(forest_test)


## Training a model on the data ----

# simple ANN with only a single hidden neuron
View(category)
colnames(forest_train)
forest_model <- neuralnet(size_category ~., data = forest_train)

# visualize the network topology
windows();plot(forest_model)
## Evaluating model performance ----
# obtain model results
View(forest_test)
model_results <- compute(forest_model, forest_test[1:28])
# obtain predicted area values
predicted_area <- model_results$net.result
# examine the correlation between predicted and actual values
cor(predicted_area, forest_test$size_category)
# Coorelation between predicted and test area is 0.93 %

## Increasing complexity of network with 6 hidden neurons
forest_model2 <- neuralnet(size_category ~., data = forest_train, hidden =c(4,2))
# plot the network
windows();
plot(forest_model2)
# evaluate the results
model_results2 <- compute(forest_model2, forest_test[1:28])
predicted_area2 <- model_results2$net.result
cor(predicted_area2, forest_test$size_category)
# Coorelation between predicted area and test area decreses slightly

colnames(forest_train)
# Building model 3 by elimating the features "dayfri"        "daymon"        "daysat"        "daysun"        "daythu"       
# "daytue"        "daywed"        "monthapr"      "monthaug"      "monthdec"      "monthfeb"      "monthjan"     
# "monthjul"      "monthjun"      "monthmar"      "monthmay"      "monthnov"      "monthoct"      "monthsep"  



forest_train <- forest_train[,-c(10:28)]
forest_test <- forest_test[,-c(10:28)]
View(forest_train)
View(forest_test)

forest_model3 <- neuralnet(size_category ~., data = forest_train)
# plot the network
windows();plot(forest_model3)
# evaluate the results
model_results3 <- compute(forest_model3, forest_test[1:9])
predicted_area3 <- model_results3$net.result
cor(predicted_area3, forest_test$size_category)

# coorelation between the predicted area and testing area is 0.93% which is strong


## Increasing complexity of network with 6 hidden neurons

forest_model4 <- neuralnet(size_category ~., data = forest_train, hidden =c(4,2))
# plot the network
windows();plot(forest_model4)
# evaluate the results
model_results4 <- compute(forest_model4, forest_test[1:9])
predicted_area4 <- model_results4$net.result
cor(predicted_area4, forest_test$size_category)

# corelation between the predicted area and size category has increased and it has reached to 0.99%
# Conclusion: Model 4 is best model with two hidden layers of 4 and 2 neurons

