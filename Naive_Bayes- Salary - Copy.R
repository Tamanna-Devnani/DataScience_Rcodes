##### Classification using Naive Bayes --------------------
install.packages("naivebayes")
install.packages("ggplot2")
install.packages("caret")
install.packages("psych")
library(naivebayes)
library(ggplot2)
library(caret)
library(psych)



# read the employee data 
SalaryData_Train <- read.csv("C:/Users/HP/Desktop/RCodes/Assignments/NAive BAyes/SalaryData_Train.csv")
View(SalaryData_Train)

# plotting the salary against all fields

plot(factor(SalaryData_Train$Salary),factor(SalaryData_Train$age))
plot(factor(SalaryData_Train$Salary),factor(SalaryData_Train$workclass))
plot(factor(SalaryData_Train$Salary),factor(SalaryData_Train$education))
plot(factor(SalaryData_Train$Salary),factor(SalaryData_Train$educationno))
plot(factor(SalaryData_Train$Salary),factor(SalaryData_Train$maritalstatus))
plot(factor(SalaryData_Train$Salary),factor(SalaryData_Train$occupation))
plot(factor(SalaryData_Train$Salary),factor(SalaryData_Train$relationship))

plot(factor(SalaryData_Train$Salary),factor(SalaryData_Train$race))
plot(factor(SalaryData_Train$Salary),factor(SalaryData_Train$sex))
plot(factor(SalaryData_Train$Salary),factor(SalaryData_Train$capitalgain))
plot(factor(SalaryData_Train$Salary),factor(SalaryData_Train$capitalloss))
plot(factor(SalaryData_Train$Salary),factor(SalaryData_Train$hoursperweek))
plot(factor(SalaryData_Train$Salary),factor(SalaryData_Train$native))



# read the test data

SalaryData_Test <- read.csv("C:/Users/HP/Desktop/RCodes/Assignments/NAive BAyes/SalaryData_Test.csv")
View(SalaryData_Test)


# build the model1 with all fields

library(e1071)
library(caret)
emp_nb <- naiveBayes(SalaryData_Train$Salary ~., data = SalaryData_Train)
emp_nb

predemp <- predict(emp_nb, SalaryData_Test)

confusionMatrix(predemp,SalaryData_Test$Salary)

# build the model2 with selective fields

attach(SalaryData_Train)
emp_nb1 <- naiveBayes(Salary~ age+workclass+education+occupation+hoursperweek+native, data = SalaryData_Train)
emp_nb1

predemp <- predict(emp_nb1, SalaryData_Test$Salary)

confusionMatrix(predemp,SalaryData_Test$Salary)




