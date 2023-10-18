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
