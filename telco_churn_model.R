library(tidyverse)
library(scales)
library(tidymodels)
library(finetune)
library(vip)

telco_churn <- read_csv("data/telco/telco_churn_sample.csv")


# Data split train/test dan v-fold CV -------------------------------------

set.seed(1001)
telco_split <- telco_churn |> 
  select(-MSISDN) |> 
  mutate(churn = factor(churn, levels = c(1, 0))) |> 
  initial_split(prop = 0.8, strata = churn)

# Data training
telco_train <- telco_split |> 
  training()

# Data testing
telco_test <- telco_split |> 
  testing()

# Data k-fod cross validation, k = v = 5
cv <- telco_train |> 
  vfold_cv(v = 5, strata = churn)


# Simple Model ------------------------------------------------------------

reglog <- glm(churn ~ ., data = telco)

# Recipe and data preprocessing ------------------------------------------------

telco_recipe <- telco_train |> 
  # recipe(formula = churn ~ los + 
  #          voice_rev + voice_trx + voice_mou + voice_dou + 
  #          sms_rev + sms_trx + sms_dou + 
  #          broadband_rev + broadband_usg + broadband_dou + 
  #          voice_package_rev + voice_package_trx + voice_package_dou) |> 
  recipe(formula = churn ~ .)

telco_rec_normalize <- telco_train |> 
  recipe(formula = churn ~ .) |> 
  step_normalize(all_numeric_predictors())


# Model specification -----------------------------------------------------

logreg <- logistic_reg(penalty = tune(), mixture = tune()) |> 
  set_mode("classification") |> 
  set_engine("glmnet")

dtree <- decision_tree(cost_complexity = tune(), 
                       tree_depth = tune(), 
                       min_n = tune()) |> 
  set_mode("classification") |> 
  set_engine("rpart")

rf <- rand_forest(mtry = tune(), 
                  min_n = tune(), 
                  trees = 1000) |> 
  set_mode("classification") |> 
  set_engine("ranger", importance = "permutation")


# Workflow ----------------------------------------------------------------

models <- list(
  logreg = logreg, 
  dtree = dtree, 
  rand_forest = rf
)

telco_set <- workflow_set(preproc = list(simple = telco_recipe, 
                                         normalize = telco_rec_normalize), 
                          models = models, cross = TRUE)

# Membutuhkan waktu yang cukup lama dan komputasi yang berat

grid_ctrl <- control_race(
  save_pred = TRUE,
  parallel_over = "everything", 
  save_workflow = TRUE
)

telco_models <- telco_set |> 
  workflow_map(fn = "tune_race_anova", 
               resamples = cv, 
               grid = 15, 
               control = grid_ctrl, 
               verbose = TRUE, 
               seed = 1001)

telco_models |> 
  collect_metrics() |> 
  filter(.metric == "roc_auc") |> 
  select(wflow_id, .config, model, mean) |> 
  arrange(desc(mean))

telco_models |> 
  autoplot(rank_metric = "roc_auc", 
           metric = "roc_auc", 
           select_best = TRUE) + 
  theme_bw()

telco_models |> 
  collect_predictions() |> 
  group_by(wflow_id) |> 
  roc_curve(churn, .pred_1) |> 
  autoplot()

best_param <- telco_models |> 
  extract_workflow_set_result("simple_rand_forest") |> 
  select_best(metric = "roc_auc")
best_param

best_telco <- telco_models |> 
  extract_workflow("simple_rand_forest") |> 
  finalize_workflow(best_param) |> 
  last_fit(split = telco_split)

best_telco |> 
  collect_metrics()

pred_result <- best_telco |> 
  collect_predictions()

bind_rows(
  pred_result |> 
    sensitivity(truth = churn, estimate = .pred_class), 
  pred_result |> 
    recall(truth = churn, estimate = .pred_class), 
  pred_result |> 
    specificity(truth = churn, estimate = .pred_class), 
  pred_result |> 
    precision(truth = churn, estimate = .pred_class), 
  pred_result |> 
    roc_auc(churn, .pred_1)
)

pred_result |> 
  roc_curve(truth = churn, .pred_1) |> 
  autoplot()

final_model <- best_telco |> 
  extract_workflow()

telco_to_pred <- read_csv("data/telco/telco_churn_to_pred.csv")

telco_to_pred |> 
  bind_cols(
    final_model |> 
      predict(new_data = telco_to_pred), 
    final_model |> 
      predict(new_data = telco_to_pred, type = "prob")
    )

final_model |> 
  extract_fit_engine() |> 
  vip::vi_model() |> 
  ggplot(aes(x = Importance, 
             y = reorder(Variable, Importance))) + 
  geom_col() + 
  labs(y = "Variable") + 
  theme_bw()
