################################################################################
# Feature Selection Pipeline to select relevant features in RNAseq dataset
# Author: Damir Zhakparov
# Date: 16-02-2023
# TODO
#
################################################################################
{library(tidymodels)
library(boruta)
library(glmnet)
library(randomForest)}
rm(list = ls())
################################################################################
# 1. Loading and splitting the data
################################################################################
data <- your_data

set.seed(123)
data_split <- initial_split(data, prop = 0.7, strata = your_strata_column)
data_train <- training(data_split)
data_test <- testing(data_split)

# Optional: normalization of the data
# Is it needed?
# Not really, since RNAseq data is usually already normalized
# However, maybe implementing log transformation should be helpful
rec <- recipe(your_response_column ~ ., data = data_train) %>%
  step_normalize(all_predictors())

################################################################################
# 2. Declare feature selection models
################################################################################

models <- list(
  logistic_reg = logistic_reg(mode = "classification"),
  lasso_reg = linear_reg(penalty = 1, mode = "classification"),
  rfe_rf = rand_forest_wrapper(mtry = tune(), importance = "impurity",
                               recursive = TRUE),
  boruta_rf = rand_forest_wrapper(mtry = tune(), importance = "impurity",
                                  recursive = FALSE)
)

# Create nested cross-validation object
cv <- vfold_cv(data_train, v = 5, strata = your_strata_column)

# Define hyperparameter grid
grid <- grid_regular(
  mtry(range = c(3, ncol(data_train) - 1)),
  levels = 10
)

################################################################################
# 3.  Specify the workflows
################################################################################

workflows <- list()

# Logistic Regression
workflows$logistic_reg <- workflow() %>%
  add_recipe(rec) %>%
  add_model(models$logistic_reg)

# Lasso Regression
workflows$lasso_reg <- workflow() %>%
  add_recipe(rec) %>%
  add_model(models$lasso_reg)

# Recursive Feature Elimination with Random Forest
workflows$rfe_rf <- workflow() %>%
  add_recipe(rec) %>%
  add_model(models$rfe_rf)

# Boruta with Random Forest
workflows$boruta_rf <- workflow() %>%
  add_recipe(rec) %>%
  add_model(models$boruta_rf)

################################################################################
# 4. Fetch the results
################################################################################

results <- list()

# Logistic Regression
results$logistic_reg <- workflow_map(
  workflows$logistic_reg,
  tune_grid = NULL, # no tuning necessary for logistic regression
  resamples = cv,
  metrics = metric_set(roc_auc)
)

# Lasso Regression
results$lasso_reg <- workflow_map(
  workflows$lasso_reg,
  tune_grid = grid,
  resamples = cv,
  metrics = metric_set(roc_auc)
)

# Recursive Feature Elimination with Random Forest
results$rfe_rf <- workflow_map(
  workflows$rfe_rf,
  tune_grid = grid,
  resamples = cv,
  metrics = metric_set(roc_auc)
)

# Boruta with Random Forest
results$boruta_rf <- workflow_map(
  workflows$boruta_rf,
  tune_grid = grid,
  resamples = cv,
  metrics = metric_set(roc_auc)
)

best_models <- list()
importance <- list()

# Logistic Regression
best_models$logistic_reg <- select_best(results$logistic_reg, "roc_auc")
# Lasso Regression
best_models$lasso_reg <- select_best(results$lasso_reg, "roc_auc")
lasso_coef <- tidy(best_models$lasso_reg) %>%
    filter(term != "(Intercept)") %>%
    mutate(abs_estimate = abs(estimate)) %>%
    arrange(desc(abs_estimate)) %>%
    select(term)

# Recursive Feature Elimination with Random Forest
best_models$rfe_rf <- select_best(results$rfe_rf, "roc_auc")
rfe_importance <- collect(best_models$rfe_rf) %>%
    select(-.metric, -.estimator, -.pred) %>%
    pivot_longer(cols = -c(your_response_column, ".y")) %>%
    group_by(name) %>%
    summarize(importance = mean(value)) %>%
    arrange(desc(importance)) %>%
    select(name)

# Boruta with Random Forest
best_models$boruta_rf <- select_best(results$boruta_rf, "roc_auc")
boruta_importance <- collect(best_models$boruta_rf) %>%
    select(-.metric, -.estimator, -.pred) %>%
    pivot_longer(cols = -c(your_response_column, ".y")) %>%
    group_by(name) %>%
    summarize(importance = mean(value)) %>%
    arrange(desc(importance)) %>%
    select(name)

final_results <- list()

# Logistic Regression
final_results$logistic_reg <- predict(best_models$logistic_reg, data_test) %>%
  bind_cols(data_test) %>%
  metrics(truth = !!sym(your_response_column), estimate = .pred_class)

# Lasso Regression
final_results$lasso_reg <- predict(best_models$lasso_reg, data_test) %>%
  bind_cols(data_test) %>%
  metrics(truth = !!sym(your_response_column), estimate = .pred_class)

# Recursive Feature Elimination with Random Forest
final_results$rfe_rf <- predict(best_models$rfe_rf, data_test) %>%
  bind_cols(data_test) %>%
  metrics(truth = !!sym(your_response_column), estimate = .pred_class)

# Boruta with Random Forest
final_results$boruta_rf <- predict(best_models$boruta_rf, data_test) %>%
  bind_cols(data_test) %>%
  metrics(truth = !!sym(your_response_column), estimate = .pred_class)

selected_features <- list(
  logistic_reg = rec %>%
    update_select(all_of(names(best_models$logistic_reg$fit$coefficients))),
  lasso_reg = rec %>%
    update_select(all_of(lasso_coef$name[1:10])),
  rfe_rf = rec %>%
    update_select(all_of(rfe_importance$name[1:10])),
  boruta_rf = rec %>%
    update_select(all_of(boruta_importance$name[1:10]))
)

evaluation_metrics <- list(
  logistic_reg = final_results$logistic_reg,
  lasso_reg = final_results$lasso_reg,
  rfe_rf = final_results$rfe_rf,
  boruta_rf = final_results$boruta_rf
)

# Logistic Regression
best_models$logistic_reg <- select_best(results$logistic_reg, "roc_auc")
logistic_coef <- tidy(best_models$logistic_reg) %>%
  filter(term != "(Intercept)") %>%
  mutate(abs_estimate = abs(estimate)) %>%
  arrange(desc(abs_estimate)) %>%
  select(term)

# Lasso Regression
best_models$lasso_reg <- select_best(results$lasso_reg, "roc_auc")
lasso_coef <- tidy(best_models$lasso_reg) %>%
  filter(term != "(Intercept)") %>%
  mutate(abs_estimate = abs(estimate)) %>%
  arrange(desc(abs_estimate)) %>%
  select(term)

# Recursive Feature Elimination with Random Forest
best_models$rfe_rf <- select_best(results$rfe_rf, "roc_auc")
rfe_importance <- collect(best_models$rfe_rf) %>%
  select(-.metric, -.estimator, -.pred) %>%
  pivot_longer(cols = -c(your_response_column, ".y")) %>%
  group_by(name) %>%
  summarize(importance = mean(value)) %>%
  arrange(desc(importance)) %>%
  select(name)

# Boruta with Random Forest
best_models$boruta_rf <- select_best(results$boruta_rf, "roc_auc")
boruta_importance <- collect(best_models$boruta_rf) %>%
  select(-.metric, -.estimator, -.pred) %>%
  pivot_longer(cols = -c(your_response_column, ".y")) %>%
  group_by(name) %>%
  summarize(importance = mean(value)) %>%
  arrange(desc(importance)) %>%
  select(name)

# Get the top 10 most important features for each method
logistic_top_features <- logistic_coef %>%
  slice_max(n = 10, order_by = abs_estimate)
lasso_top_features <- lasso_coef %>%
  slice_max(n = 10, order_by = abs_estimate)
rfe_top_features <- rfe_importance %>%
  slice_max(n = 10, order_by = importance)
boruta_top_features <- boruta_importance %>%
  slice_max(n = 10, order_by = importance)


# Logistic Regression
logistic_top_features <- logistic_coef %>%
  group_by(term) %>%
  summarize(rank = median(row_number())) %>%
  arrange(rank) %>%
  slice_max(n = 10, order_by = rank) %>%
  pull(term)

# Lasso Regression
lasso_top_features <- lasso_coef %>%
  group_by(term) %>%
  summarize(rank = median(row_number())) %>%
  arrange(rank) %>%
  slice_max(n = 10, order_by = rank) %>%
  pull(term)

# Recursive Feature Elimination with Random Forest
rfe_top_features <- rfe_importance %>%
  group_by(name) %>%
  summarize(rank = median(row_number())) %>%
  arrange(rank) %>%
  slice_max(n = 10, order_by = rank) %>%
  pull(name)

# Boruta with Random Forest
boruta_top_features <- boruta_importance %>%
  group_by(name) %>%
  summarize(rank = median(row_number())) %>%
  arrange(rank) %>%
  slice_max(n = 10, order_by = rank) %>%
  pull(name)

# Select the most important features based on their median rank for each method separately
logistic_reg_formula <- reformulate(logistic_top_features, response = your_response_column)
lasso_reg_formula <- reformulate(lasso_top_features, response = your_response_column)
rfe_rf_formula <- reformulate(rfe_top_features, response = your_response_column)
boruta_rf_formula <- reformulate(boruta_top_features, response = your_response_column)

# Fit the final models using the selected features
final_models <- list(
  logistic_reg = logistic_reg %>% set_engine("glm") %>% set_mode("classification"),
  lasso_reg = lasso_reg %>% set_engine("glmnet") %>% set_mode("classification"),
  rfe_rf = rand_forest() %>% set_mode("classification"),
  boruta_rf = rand_forest() %>% set_mode("classification")
)

final_workflows <- list(
  logistic_reg = workflow() %>% add_model(final_models$logistic_reg) %>% add_formula(logistic_reg_formula),
  lasso_reg = workflow() %>% add_model(final_models$lasso_reg) %>% add_formula(lasso_reg_formula),
  rfe_rf = workflow() %>% add_model(final_models$rfe_rf) %>% add_formula(rfe_rf_formula),
  boruta_rf = workflow() %>% add_model(final_models$boruta_rf) %>% add_formula(boruta_rf_formula)
)

final_results <- fit_resamples(
  final_workflows,
  resamples = nested_resamples,
  metrics = metric_set(roc_auc, accuracy, f_meas),
  control = control_resamples(save_pred = TRUE, extract = function(x) x$.pred_class)
)
