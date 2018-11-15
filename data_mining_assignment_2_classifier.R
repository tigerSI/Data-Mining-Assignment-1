library(mlbench)
library(rpart)
library(rpart.plot)
library(ggplot2)
library(dplyr)
library(caret)
library(e1071)

# Function: finding accuracy
accuracy <- function(truth, prediction){
  tbl <- table(truth, prediction)   # Create confusion matrix
  sum(diag(tbl))/sum(tbl)           # Calculate accuracy
}

# Classification
# 1. Predict if a person’s income is greater than 50,000.
# Create decision tree
tree_1 <- rpart(income_50k ~ ., data = preprocessed_data_for_task_1)

# Test with test set
predicted_1 <- predict(tree_1, t_data_1, type = "class")

# Create confusion matrix
confusion_table_1 <- table(t_data_1$income_50k, predicted_1)

# Find accuracy
accuracy(t_data_1$income_50k, predicted_1)

# Classification
# 2. Predict if a person’s marital status is never married.
# Create decision tree
tree_2 <- rpart(marital_stat ~ ., data = preprocessed_data_for_task_2)

# Test with test set
predicted_2 <- predict(tree_2, t_data_2, type = "class")

# Create confusion matrix
confusion_table_2 <- table(t_data_2$marital_stat, predicted_2)

# Find accuracy
accuracy(t_data_2$marital_stat, predicted_2)

# Classification
# 3. Predict if a white person’s income is less than 50,000.
# Create decision tree
tree_3 <- rpart(WhiteIncomeLess50k ~ ., data = preprocessed_data_for_task_3)

# Test with test set
predicted_3 <- predict(tree_3, t_data_3, type = "class")

# Create confusion matrix
confusion_table_3 <- table(t_data_3$WhiteIncomeLess50k, predicted_3)

# Find accuracy
accuracy(t_data_3$WhiteIncomeLess50k, predicted_3)






