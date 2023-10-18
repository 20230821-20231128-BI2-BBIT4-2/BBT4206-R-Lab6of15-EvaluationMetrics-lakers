#Load Customer Churn dataset
churn_dateset <- read_csv(
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
View(churn_dateset)
