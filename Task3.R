#Task-03

## PREPARING THE DATA FOR ANALYSIS
# importing the given data
bank <- read.csv("C://Users//HP//Desktop//Prodigy//Task3//bank.csv", sep=";")
#finding the positions of missing observations
colSums(is.na(bank))
#Data structure and Summary
str(bank)
summary(bank)
#coverting the target variable y to factor
bank$y <- as.factor(bank$y)

#Splitting the dataset into training set and test set with 80% and 20% respectively
set.seed(10)
SampleIndex <- sample(2, nrow(bank), prob= c(0.8, 0.2), replace = TRUE)
TrainSet <- bank[SampleIndex == 1,]
TestSet <- bank[SampleIndex == 2,]
#checking number of obs. in training and test sets
dim(TrainSet)
dim(TestSet)

#ANALYSIS
#importing required libraries
library("rpart")
library("rpart.plot")
library("caret")
library("e1071")

#building Decision Tree model with rpart()
tree_model <- rpart(y ~ ., data=TrainSet, method = 'class')
print(tree_model)

#visualizing the decision tree
rpart.plot(tree_model, type = 3, extra = 102, fallen.leaves = TRUE)

#Making Predicting on Test dataset
predictions <- predict(tree_model, TestSet, type = "class")
head(predictions)

#MODEL ACCURACY
#confusion matrix
confusionMatrix(predictions, TestSet$y)
#accuracy
accuracy <- sum(predictions == TestSet$y) / nrow(TestSet)
print(paste('Accuracy of the decision tree model is :', round(accuracy, 4)))