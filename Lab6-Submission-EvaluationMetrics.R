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

# Make predictions without the 'Age Group' column
age_group_removed_test_data <- churn_test_data[, !colnames(churn_test_data) %in% "Age Group"]
predictions <- predict(churn_model_glm, newdata = age_group_removed_test_data)

# RMSE
rmse <- sqrt(mean((churn_test_data$Churn - predictions)^2))
print(paste("RMSE =", rmse))

# Calculate SSR for the churn predictions
ssr <- sum((churn_test_data$Churn - predictions)^2)
print(paste("SSR =", ssr))

# Calculate SSR for the churn predictions
ssr <- sum((churn_test_data$Churn - predictions)^2)

# Calculate SST (Total Sum of Squares)
sst <- sum((churn_test_data$Churn - mean(churn_test_data$Churn))^2)

# Calculate R-squared
r_squared <- 1 - (ssr / sst)

print(paste("R Squared =", r_squared))

# Calculate absolute errors for the churn predictions
absolute_errors <- abs(predictions - churn_test_data$Churn)

# Calculate MAE (Mean Absolute Error)
mae <- mean(absolute_errors)

print(paste("MAE =", mae))

# 3. Area Under ROC Curve
#Dataset is already loaded so we determine baseline accuracy
# The baseline accuracy calculation for your Churn dataset
churn_freq <- churn_test_data$Churn
cbind(frequency = table(churn_freq),
      percentage = prop.table(table(churn_freq)) * 100)

# Define an 80:20 train:test data split of the dataset.
train_index <- createDataPartition(churn_test_data$Churn,
                                   p = 0.8,
                                   list = FALSE)

churn_train_data <- churn_test_data[train_index, ]
churn_test_data <- churn_test_data[-train_index, ]

# We apply the 10-fold cross-validation resampling method
train_control <- trainControl(method = "cv", number = 10, classProbs = TRUE, summaryFunction = twoClassSummary)

# List unique factor levels
unique(churn_test_data$Churn)

# We then train a k Nearest Neighbors Model to predict the value of Customer Churn
set.seed(7)
churn_model_knn <- train(Churn ~ ., data = churn_train_data, method = "knn",
                         metric = "ROC", trControl = train_control)

# Display the model's performance metrics
print(churn_model_knn)

# 4. Logarithmic Loss (LogLoss)
# We apply the 5-fold repeated cross-validation resampling method with 3 repeats
train_control <- trainControl(method = "repeatedcv", number = 5, repeats = 3,
                              classProbs = TRUE,
                              summaryFunction = mnLogLoss)
set.seed(7)

# Train a decision tree model (CART) to predict Churn
churn_model_cart <- train(Churn ~ ., data = churn_train_data, method = "rpart",
                          metric = "logLoss", trControl = train_control)

# Display the model's performance metrics
print(churn_model_cart)
