#Task-03
set.seed(17)
## PREPARING THE DATA FOR ANALYSIS
# importing the given data
bank <- read.csv("C://Users//HP//Desktop//others//Prodigy//Task3//bank.csv", sep=";")
#finding the positions of missing observations
colSums(is.na(bank))
#Data structure and Summary
str(bank)
summary(bank)
#coverting the target variable y to factor
bank$y <- as.factor(bank$y)

#Splitting the dataset into training set and test set with 80% and 20% respectively
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
library("pROC")
library("ggplot2")

#building Decision Tree model with rpart()
tree_model <- rpart(y ~ ., data=TrainSet, method = 'class')
print(tree_model)

#visualizing the decision tree
rpart.plot(tree_model, type = 3, extra = 102, fallen.leaves = TRUE)

#Making Predicting on Test dataset
predictions <- predict(tree_model, TestSet, type = "class")
pred_prob <- predict(tree_model, TestSet, type = "prob")[,2]
head(predictions)

#Variable Importance
tree_model$variable.importance
#plot
importance_df <- data.frame(
  Variable = names(tree_model$variable.importance),
  Importance = tree_model$variable.importance
)
ggplot(importance_df, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Variable Importance - Decision Tree", 
       x = "Variables",y = "Importance")

#MODEL ACCURACY
#confusion matrix
confusionMatrix(predictions, TestSet$y)
#accuracy
accuracy <- sum(predictions == TestSet$y) / nrow(TestSet)
print(paste('Accuracy of the decision tree model is :', round(accuracy, 4)))
# ROC curve
roc_obj <- roc(TestSet$y, pred_prob)
plot(roc_obj, col = "blue", main = "ROC Curve - Decision Tree")
auc(roc_obj)





#Defining train control
ctrl <- trainControl(method = "cv", number = 5, classProbs = TRUE, 
                     summaryFunction = twoClassSummary)

#training the model
model_cv <- train(y ~ ., data = bank, method = "rpart",
                  trControl = ctrl, metric = "ROC")

model_cv
#visualizing the decision tree
rpart.plot(model_cv$finalModel)

#Variable Importance
model_cv$finalModel$variable.importance
#plot
importance_df <- data.frame(
  Variable = names(model_cv$finalModel$variable.importance),
  Importance = model_cv$finalModel$variable.importance
)
ggplot(importance_df, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Variable Importance - Decision Tree", 
       x = "Variables",y = "Importance")
#result
model_cv$results
#If we choose cp = 0.01055662 then ROC = s0.73