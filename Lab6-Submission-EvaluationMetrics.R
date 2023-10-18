#Load Customer Churn dataset
churn_dataset <- read_csv(
  "data/Customer Churn.csv",
  col_types = cols(
    Complains = col_factor(levels = c("0",
                                      "1")),
    `Age Group` = col_factor(levels = c("1",
                                        "2", "3", "4", "5")),
    `Tariff Plan` = col_factor(levels = c("1",
                                          "2")),
    Status = col_factor(levels = c("1",
                                   "2")),
    Churn = col_factor(levels = c("0",
                                  "1"))
  )
)
View(churn_dataset)

#summary of dataset
summary(churn_dataset)

#1. Determine Baseline Accuracy
#Identify the number of instances that belong to each class (distribution or class breakdown).

# Assuming "Churn" is the column in your dataset that indicates the class (0 or 1),
# we will calculate the distribution of "Churn."

churn_distribution <- churn_dataset$Churn
baseline_accuracy <- prop.table(table(churn_distribution)) * 100
cbind(frequency = table(churn_distribution), percentage = baseline_accuracy)

# Split the dataset
# Define a 75:25 train:test data split of the dataset.
# That is, 75% of the original data will be used to train the model and
# 25% of the original data will be used to test the model.

library(caret)
train_index <- createDataPartition(churn_dataset$Churn,
                                   p = 0.75,
                                   list = FALSE)

churn_train_data <- churn_dataset[train_index, ]
churn_test_data <- churn_dataset[-train_index, ]

#Train the Model
# We apply the 5-fold cross-validation resampling method
train_control <- trainControl(method = "cv", number = 5)

# We then train a Generalized Linear Model to predict the value of Churn
# (whether the customer will churn or not).
set.seed(7)
churn_model_glm <-
  train(Churn ~ ., data = churn_train_data, method = "glm",
        metric = "Accuracy", trControl = train_control)

# Display the Model's Performance
# Option 1: Use the metric calculated by caret when training the model

# Display the model's performance metrics
print(churn_model_glm)

#try to fix the age group error
colnames(churn_test_data)[colnames(churn_test_data) == 'Age Group'] <- 'Age_Group'

# Option 2
predictions <- predict(churn_model_glm, churn_test_data[, 1:8])
# Calculate the confusion matrix
confusion_matrix <- caret::confusionMatrix(predictions, churn_test_data[, 1:9]$Churn)
print(confusion_matrix)

# Visualizing Confusion Matrix
fourfoldplot(as.table(confusion_matrix), color = c("grey", "lightblue"),
             main = "Confusion Matrix")

# RMSE, R Squared, and MAE 

