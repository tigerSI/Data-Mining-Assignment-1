library(mlbench)
library(rpart)
library(ggplot2)
library(dplyr)
library(caret)
library(lattice)
library(e1071)


# Function: finding accuracy
accuracy <- function(truth, prediction){
  tbl <- table(truth, prediction)   # Create confusion matrix
  sum(diag(tbl))/sum(tbl)           # Calculate accuracy
}



# Run algorithms using 10-fold cross validation
control <- trainControl(method="cv", number=10)
metric <- "Accuracy"


# b) nonlinear algorithms
# Implement
# CART
set.seed(7)
fit.cart_1 <- train(income_50k~., data=preprocessed_data_for_task_1, method="rpart", metric=metric, trControl=control)
fit.cart_2 <- train(marital_stat~., data=preprocessed_data_for_task_2, method="rpart", metric=metric, trControl=control)
fit.cart_3 <- train(WhiteIncomeLess50k~., data=preprocessed_data_for_task_3, method="rpart", metric=metric, trControl=control)


# kNN #k-Nearest Neighbors
set.seed(7)
fit.knn_1 <- train(income_50k~., data=preprocessed_data_for_task_1, method="knn", metric=metric, trControl=control)
fit.knn_1$finalModel
fit.knn_2 <- train(marital_stat~., data=preprocessed_data_for_task_2, method="knn", metric=metric, trControl=control)
fit.knn_2$finalModel
fit.knn_3 <- train(WhiteIncomeLess50k~., data=preprocessed_data_for_task_3, method="knn", metric=metric, trControl=control)
fit.knn_3$finalModel


# Test with test set
predicted_cart_1 <- predict(fit.cart_1, t_data_1)
predicted_cart_2 <- predict(fit.cart_2, t_data_2)
predicted_cart_3 <- predict(fit.cart_3, t_data_3)

predicted_knn_1 <- predict(fit.knn_1, t_data_1)
predicted_knn_2 <- predict(fit.knn_2, t_data_2)
predicted_knn_3 <- predict(fit.knn_3, t_data_3)

# Find accuracry
accuracy(t_data_1$income_50k, predicted_cart_1)
accuracy(t_data_2$marital_stat, predicted_cart_2)
accuracy(t_data_3$WhiteIncomeLess50k, predicted_cart_3)

accuracy(t_data_1$income_50k, predicted_knn_1)
accuracy(t_data_2$marital_stat, predicted_knn_2)
accuracy(t_data_3$WhiteIncomeLess50k, predicted_knn_3)

# Create trainn and testsets using createDataPartition function in caret package
#p denotes how much data we want in the training set
#0.7 = 70% of the data in training set 
#rest in cross validation set, list = F -> indices we obtain should be in form of a vector.

#index = createDataPartition(task1$income_50k, p = 0.75, list = F )
#trainn = task1[index,]
#validationn = task1[-index,]

# estimate skill of cart on the validation dataset
#predicted_cart <- predict(fit.cart, validationn)
# Create confusion matrix
#confusion_table <- confusionMatrix(predicted_cart, validationn$income_50k)
#print(confusion_table)
#accuracy(task1$income_50k, predicted_cart)


# c) advanced algorithms
#set.seed(7)
#fit.svm <- train(income_50k~., data=task1, method="svmRadial", metric=metric, trControl=control)
# Random Forest
#set.seed(7)
#fit.rf <- train(income_50k~., data=task1, method="rf", metric=metric, trControl=control)



#Artificial Neural Network
#nnetWork <- train(income_50k ~ ., method = "nnet", data = preprocessed_data_for_task_1, tuneLength = 5,trControl = trainControl( method = "cv", indexOut = train),trace = FALSE)
#nnetWork
#nnetWork$finalModel



