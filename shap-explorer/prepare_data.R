# data --------------------------------------------------------------------
data <- modeldata::credit_data
names(data) <- tolower(names(data))

predictors <- c(
  "seniority", "time", "age", "expenses", "income",
  "assets", "debt", "amount", "price"
)

data <- data[complete.cases(data[, c("status", predictors)]), , drop = FALSE]
data$status_bad <- as.integer(data$status == "bad")

development <- data[, c("status_bad", predictors), drop = FALSE]

control_meta <- lapply(predictors, function(variable) {
  x <- development[[variable]]
  span <- diff(range(x))

  list(
    min = min(x),
    max = max(x),
    value = unname(stats::median(x)),
    step = max(1, round(span / 100))
  )
})
names(control_meta) <- predictors

# model helpers -----------------------------------------------------------
predict_model <- function(model, newdata) {
  prediction <- switch(
    model$type,
    logistic = stats::predict(model$fit, newdata = newdata, type = "response"),
    spline = stats::predict(model$fit, newdata = newdata, type = "response"),
    tree = stats::predict(model$fit, newdata = newdata),
    bagged = Reduce(
      `+`,
      lapply(model$fit, function(tree) stats::predict(tree, newdata = newdata))
    ) / length(model$fit)
  )

  pmin(pmax(as.numeric(prediction), 0), 1)
}

shap_one <- function(model, x, background, nsim = 12L, seed = 1L) {
  set.seed(seed)

  variables <- names(x)
  p <- length(variables)
  values <- setNames(numeric(p), variables)

  for (j in seq_along(variables)) {
    variable <- variables[[j]]
    before <- background[sample.int(nrow(background), nsim, replace = TRUE), , drop = FALSE]
    after <- before

    for (s in seq_len(nsim)) {
      permutation <- sample(variables)
      position <- match(variable, permutation)
      preceding <- if (position > 1L) permutation[seq_len(position - 1L)] else character()

      if (length(preceding)) {
        before[s, preceding] <- x[1, preceding, drop = FALSE]
        after[s, preceding] <- x[1, preceding, drop = FALSE]
      }

      after[s, variable] <- x[[variable]]
    }

    values[[variable]] <- mean(
      predict_model(model, after) - predict_model(model, before)
    )
  }

  baseline <- mean(predict_model(model, background))
  prediction <- predict_model(model, x)[[1]]
  residual <- prediction - baseline - sum(values)

  if (sum(abs(values)) > 0) {
    values <- values + residual * abs(values) / sum(abs(values))
  } else {
    values <- values + residual / length(values)
  }

  values
}

# models ------------------------------------------------------------------
train <- development

logistic <- stats::glm(
  status_bad ~ .,
  data = train,
  family = stats::binomial()
)

spline_predictors <- setdiff(predictors, c("assets", "debt"))
spline_terms <- c(
  paste0("splines::ns(", spline_predictors, ", df = 3)"),
  "assets",
  "debt"
)
spline_formula <- stats::as.formula(
  paste("status_bad ~", paste(spline_terms, collapse = " + "))
)

spline <- stats::glm(
  spline_formula,
  data = train,
  family = stats::binomial()
)

tree <- rpart::rpart(
  status_bad ~ .,
  data = train,
  method = "anova",
  control = rpart::rpart.control(
    cp = 0.004,
    minsplit = 60,
    maxdepth = 5,
    xval = 0
  )
)

set.seed(2026)
bagged <- lapply(seq_len(40), function(i) {
  rows <- sample.int(nrow(train), nrow(train), replace = TRUE)
  vars <- sample(predictors, ceiling(sqrt(length(predictors))))
  formula <- stats::as.formula(
    paste("status_bad ~", paste(vars, collapse = " + "))
  )

  rpart::rpart(
    formula,
    data = train[rows, , drop = FALSE],
    method = "anova",
    control = rpart::rpart.control(
      cp = 0.002,
      minsplit = 40,
      maxdepth = 6,
      xval = 0
    )
  )
})

models <- list(
  logistic = list(type = "logistic", label = "Logistic", fit = logistic),
  spline = list(type = "spline", label = "Spline logistic", fit = spline),
  tree = list(type = "tree", label = "Decision tree", fit = tree),
  bagged = list(type = "bagged", label = "Bagged trees", fit = bagged)
)

# predictions -------------------------------------------------------------
X <- development[, predictors, drop = FALSE]
pd <- lapply(models, predict_model, newdata = X)
baseline <- vapply(pd, mean, numeric(1))

# global SHAP sample ------------------------------------------------------
set.seed(2026)
background <- X[sample.int(nrow(X), min(150L, nrow(X))), , drop = FALSE]
shap_rows <- sample.int(nrow(X), min(350L, nrow(X)))

shap_data <- do.call(
  rbind,
  lapply(names(models), function(model_name) {
    model <- models[[model_name]]

    do.call(
      rbind,
      lapply(seq_along(shap_rows), function(i) {
        row_id <- shap_rows[[i]]
        x <- X[row_id, , drop = FALSE]
        values <- shap_one(
          model,
          x = x,
          background = background,
          nsim = 10L,
          seed = 1000L + i
        )

        data.frame(
          model = model_name,
          row_id = row_id,
          variable = predictors,
          value = as.numeric(x[1, predictors]),
          shap = unname(values),
          stringsAsFactors = FALSE
        )
      })
    )
  })
)

# artifact ----------------------------------------------------------------
dir.create("shap-explorer/data", recursive = TRUE, showWarnings = FALSE)

saveRDS(
  list(
    development = development,
    predictors = predictors,
    control_meta = control_meta,
    models = models,
    pd = pd,
    baseline = baseline,
    background = background,
    shap_data = shap_data
  ),
  "shap-explorer/data/shap-credit.rds",
  compress = "xz"
)

message("Saved shap-explorer/data/shap-credit.rds")
