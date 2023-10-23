# Install the readr package 
install.packages("readr")

# Load the readr package
library(readr)

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

#1. Accuracy and Cohen's Kappa
#Determine Baseline Accuracy
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

# 2. RMSE, R Squared, and MAE
## Split the dataset ----
set.seed(7)
# We apply simple random sampling using the base::sample function to get
# 10 samples
train_index <- sample(1:nrow(churn_dataset), 10)
churn_train_data <- churn_dataset[train_index, ]
churn_test_data <- churn_dataset[-train_index, ]

# Train the Model for Classification
# We apply bootstrapping with 1,000 repetitions
train_control <- trainControl(method = "boot", number = 1000)

# We then train a logistic regression model to predict the value of Churn
# (whether the customer will churn or not given the independent variables).

churn_model_logistic <- train(Churn ~ ., data = churn_train_data,
                              method = "glm", family = "binomial", metric = "Accuracy",
                              trControl = train_control)

# model perfomance
print(churn_model_logistic)

# Option 2: Compute the metric yourself using the test dataset
predictions <- predict(churn_model_glm, churn_test_data[, 1:8])

# These are the predicted values for Churn from the model:
print(predictions)

# Assuming 'Age Group' is the column to be excluded
#We excluded because it brought error when computing RMSE
churn_train_data <- churn_train_data[, !colnames(churn_train_data) %in% "Age Group"]
churn_test_data <- churn_test_data[, !colnames(churn_test_data) %in% "Age Group"]

churn_model_logistic <- train(Churn ~ ., data = churn_train_data,
                              method = "glm", family = "binomial", metric = "Accuracy",
                              trControl = train_control)

# Make predictions without the 'Age Group' column
predictions <- predict(churn_model_logistic, churn_test_data[, !colnames(churn_test_data) %in% "Age Group"])



# RMSE
rmse <- sqrt(mean((churn_test_data$Churn - predictions)^2))
print(paste("RMSE =", rmse))

