#Installing and loading the libraries
install.packages("recommenderlab", dependencies=TRUE)
install.packages("Matrix")
install.packages("caTools")
library(recommenderlab)
library(Matrix)
library(caTools)
#movie rating data
movie_rate_data <- read.csv("C:/Users/HP/Desktop/Movie.csv")
#metadata about the variable
str(movie_rate_data)
#rating distribution
hist(movie_rate_data$rating)
#the datatype should be realRatingMatrix inorder to build recommendation engine
movie_rate_data_matrix <- as(movie_rate_data, 'realRatingMatrix')
movie_rate_data_matrix@data

#User Based Collaborative Filtering

movie_recomm_model2 <- Recommender(movie_rate_data_matrix, method="UBCF")

#Predictions for all users 
recommended_items2 <- predict(movie_recomm_model2, movie_rate_data_matrix, n=5)
as(recommended_items2, "list")
