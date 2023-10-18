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
