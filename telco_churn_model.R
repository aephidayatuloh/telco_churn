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
  mutate(churn = factor(churn, 
                        levels = c(1, 0), 
                        label = c("Yes", "No"))) |> 
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

reglog <- glm(churn ~ ., data = telco_train, family = "binomial")
reglog |> 
  summary()


# Recipe and data preprocessing ------------------------------------------------

# Recipe tanpa pra-proses apapun
telco_recipe <- telco_train |> 
  ## menuliskan semua variabel dalam formula
  # recipe(formula = churn ~ los + 
  #          voice_rev + voice_trx + voice_mou + voice_dou + 
  #          sms_rev + sms_trx + sms_dou + 
  #          broadband_rev + broadband_usg + broadband_dou + 
  #          voice_package_rev + voice_package_trx + voice_package_dou) |> 
  ## Tanda . sebagai pengganti untuk menyebutkan semua variabel 
  recipe(formula = churn ~ .)

# Recipe dengan normalisasi variabel numerik
telco_rec_normalize <- telco_train |> 
  recipe(formula = churn ~ .) |> 
  step_normalize(all_numeric_predictors())


# Model specification -----------------------------------------------------

# Masing2 model mempunyai hyperparameter yang bisa diganti2 agar 
# dapat menghasilkan model yang terbaik (hyperparameter tunning)
# Untuk melakukan hyperparameter tunning gunakan tune() untuk parameter
# yang diinginkan

# Logistic Regression dengan package glmnet
logreg <- logistic_reg(penalty = tune(), mixture = tune()) |> 
  set_mode("classification") |> 
  set_engine("glmnet")

# Decision Tree dengan package rpart
dtree <- decision_tree(cost_complexity = tune(), 
                       tree_depth = tune(), 
                       min_n = tune()) |> 
  set_mode("classification") |> 
  set_engine("rpart")

# Random Forest dengan package ranger dan perhitungan importance 
# variable dengan metode permutasi
rf <- rand_forest(mtry = tune(), 
                  min_n = tune(), 
                  trees = 1000) |> 
  set_mode("classification") |> 
  set_engine("ranger", importance = "permutation")


# Workflow ----------------------------------------------------------------

# List semua spesifikasi model 
models <- list(
  logreg = logreg, 
  dtree = dtree, 
  rand_forest = rf
)

# Set workflow recipe dan model
telco_set <- workflow_set(preproc = list(simple = telco_recipe, 
                                         normalize = telco_rec_normalize), 
                          models = models, cross = TRUE)

# Grid control untuk menyimpan hasil prediksi dan workflow saat proses
# membuat model
grid_ctrl <- control_race(
  save_pred = TRUE,
  parallel_over = "everything", 
  save_workflow = TRUE
)

# Lakukan pemodelan dan cross validation
# !!!Membutuhkan waktu yang cukup lama dan komputasi yang berat

telco_models <- telco_set |> 
  workflow_map(fn = "tune_race_anova", 
               resamples = cv, 
               grid = 15, 
               control = grid_ctrl, 
               verbose = TRUE, 
               seed = 1001)

# Tampilkan list workflow berdasarkan AUC 
telco_models |> 
  collect_metrics() |> 
  filter(.metric == "roc_auc") |> 
  select(wflow_id, model, mean) |> 
  arrange(desc(mean)) |> 
  print(width = Inf)

# Plot perbandingan model dan pra-prosesnya berdasarkan AUC 
telco_models |> 
  autoplot(rank_metric = "roc_auc", 
           metric = "roc_auc", 
           select_best = TRUE) + 
  theme_bw()

# Plot perbandingan ROC semua model dan pra-prosesnya
telco_models |> 
  collect_predictions() |> 
  group_by(wflow_id) |> 
  roc_curve(churn, .pred_Yes) |> 
  autoplot()

# Mengambil parameter hasil tunning terbaik
best_param <- telco_models |> 
  extract_workflow_set_result("simple_rand_forest") |> 
  select_best(metric = "roc_auc")
best_param

# Fit terakhir dengan parameter terbaik sehingga mendapatkan 
# workflow terbaik 
best_telco <- telco_models |> 
  extract_workflow("simple_rand_forest") |> 
  finalize_workflow(best_param) |> 
  last_fit(split = telco_split)

# Menampilkan metric dari model dengan parameter terbaik
best_telco |> 
  collect_metrics()

# Menyimpan hasil prediksi dari data testing
pred_result <- best_telco |> 
  collect_predictions()

# Menghitung metric klasifikasi 
bind_rows(
  pred_result |> 
    accuracy(truth = churn, estimate = .pred_class), 
  pred_result |> 
    sensitivity(truth = churn, estimate = .pred_class), 
  pred_result |> 
    recall(truth = churn, estimate = .pred_class), 
  pred_result |> 
    specificity(truth = churn, estimate = .pred_class), 
  pred_result |> 
    precision(truth = churn, estimate = .pred_class), 
  pred_result |> 
    f_meas(truth = churn, estimate = .pred_class), 
  
  # untuk menghitung accuracy, sensitivity, specificity (recall), 
  # precision dan f-score menggunakan nilai aktual dan prediksi kelas
  # Sedangkan untuk menghitung AUC menggunakan nilai aktual dan nilai 
  # prediksi peluangnya
  pred_result |> 
    roc_auc(churn, .pred_Yes)
)

# Kurva ROC dari model terbaik
pred_result |> 
  roc_curve(truth = churn, .pred_Yes) |> 
  autoplot()

# Workflow terbaik yang nantinya digunakan untuk prediksi 
# sebagai 'model' akhir
final_model <- best_telco |> 
  extract_workflow()

# Contoh data untuk prediksi
telco_to_pred <- read_csv("data/telco/telco_churn_to_pred.csv")

# Melakukan prediksi dan menggabungkan hasil prediksi dengan 
# data asli
# Hasil prediksi berupa kelas dan probability 
telco_to_pred |> 
  bind_cols(
    final_model |> 
      predict(new_data = telco_to_pred), 
    final_model |> 
      predict(new_data = telco_to_pred, type = "prob")
    ) |> 
  print(width = Inf)

# Melihat variable importance 
final_model |> 
  extract_fit_engine() |> 
  vip::vi_model() |> 
  ggplot(aes(x = Importance, 
             y = reorder(Variable, Importance))) + 
  geom_col() + 
  labs(y = "Variable") + 
  theme_bw()
