# data --------------------------------------------------------------------
predictors <- c(
  "seniority", "time", "age", "expenses", "income",
  "assets", "debt", "amount", "price"
)

data <- modeldata::credit_data
names(data) <- tolower(names(data))
data <- data[complete.cases(data[, c("status", predictors)]), ]
data$status_bad <- as.integer(data$status == "bad")
data <- data[, c("status_bad", predictors)]

set.seed(2026)
train_rows <- sample.int(nrow(data), floor(0.8 * nrow(data)))
train <- data[train_rows, ]
test <- data[-train_rows, ]

# randomized rpart ensemble -----------------------------------------------
set.seed(2026)
trees <- lapply(seq_len(500), function(i) {
  rows <- sample.int(nrow(train), nrow(train), replace = TRUE)
  variables <- sample(predictors, ceiling(sqrt(length(predictors))))
  formula <- reformulate(variables, response = "status_bad")

  rpart::rpart(
    formula,
    data = train[rows, ],
    method = "anova",
    control = rpart::rpart.control(
      cp = 0.002,
      minsplit = 40,
      maxdepth = 6,
      xval = 0
    )
  )
})

probability_rpart <- Reduce(
  `+`,
  lapply(trees, predict, newdata = test)
) / length(trees)

# random forest -----------------------------------------------------------
set.seed(2026)
forest <- randomForest::randomForest(
  factor(status_bad) ~ .,
  data = train,
  ntree = 500,
  mtry = ceiling(sqrt(length(predictors)))
)

probability_forest <- predict(
  forest,
  newdata = test,
  type = "prob"
)[, "1"]

# comparison --------------------------------------------------------------
auc <- function(observed, probability) {
  positive <- observed == 1
  ranks <- rank(probability)
  (sum(ranks[positive]) - sum(seq_len(sum(positive)))) /
    (sum(positive) * sum(!positive))
}

auc_rpart <- auc(test$status_bad, probability_rpart)
auc_forest <- auc(test$status_bad, probability_forest)

cat(
  "Correlation:", round(cor(probability_rpart, probability_forest), 3),
  "\nRMSE:", round(sqrt(mean((probability_rpart - probability_forest)^2)), 3),
  "\nRandomized rpart - AUC:", round(auc_rpart, 3),
  "Gini:", round(2 * auc_rpart - 1, 3),
  "\nRandom Forest - AUC:", round(auc_forest, 3),
  "Gini:", round(2 * auc_forest - 1, 3),
  "\n"
)

plot(
  probability_forest,
  probability_rpart,
  pch = 19,
  col = grDevices::adjustcolor("#007BC2", alpha.f = 0.45),
  xlim = c(0, 1),
  ylim = c(0, 1),
  xlab = "Random Forest probability",
  ylab = "Randomized rpart probability"
)
abline(0, 1, col = "#D1495B", lwd = 2)
