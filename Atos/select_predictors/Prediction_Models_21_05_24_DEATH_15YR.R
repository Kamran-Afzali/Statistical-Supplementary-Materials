setwd(paste(dirname(rstudioapi::getActiveDocumentContext()$path),"",sep = ""))
load("~/OneDrive - Universite de Montreal/Usydney/data_04_2021.RData")

library(haven)
library(tidyverse)
library(tidymodels)
library(stacks)
library(vip)
library(pdp)
library(sparkline)
library(plotly)

preds_t=tibble::tribble(
  ~nb,        ~OVERALL,      ~HEROIN.USE, ~SHORT.TERM.ABSTINENCE, ~MEDIUM.TERM.ABSTINENCE, ~LONG.TERM.ABSTINENCE,       ~OVERDOSE,       ~MORTALITY,
  1L,        "dg0102",      "aust_born",        "SEXUAL_TRAUMA",                "dg0102",   "unstablehousingBL", "SEXUAL_TRAUMA",         "h0101b",
  2L,        "h0101b",         "dg0102",               "dg0106",                "h0101b",               "h0104",       "crime_1",           "as01",
  3L,        "h0101a", "first_high_cat",        "first_inj_cat",                 "h0104",           "antidep_1",        "h0101a",         "dg0106",
  4L, "SEXUAL_TRAUMA",          "h0104",               "h0101b",         "first_heroin3",       "first_heroin3",       "dg0112a",      "BLEVEREOD",
  5L,     "BLEVEREOD",         "od1201",            "alcohol_1",                   "bpd",              "od1201",        "dg0102",  "first_heroin3",
  6L,        "dg0106",          "dp401",            "BLEVEREOD",                    "T3",       "SEXUAL_TRAUMA",     "BLEVEREOD",  "first_inj_cat",
  7L,         "h0104",        "othop_1",        "sev_dis_mcs01",              "benzos_1",              "dg0102",       "msigov1",  "SEXUAL_TRAUMA",
  8L,     "alcohol_1",  "SEXUAL_TRAUMA",       "first_high_cat",                  "as01",              "h0101b",        "dg0106", "first_high_cat",
  9L,       "dg0112a",         "h0101a",              "msigov1",                "h0101a",       "sev_dis_pcs01", "first_inj_cat",      "alcohol_1",
  10L, "first_heroin3",         "dg0106",              "crime_1",         "sev_dis_mcs01",            "amphet_1",      "benzos_1",        "dp01j59"
)

table(Data_merged$DEATH_15YR)
out="DEATH_15YR"

df=Data_merged%>%
  select(c(out,preds_t$OVERALL))
df$DEATH_15YR=as.factor(df$DEATH_15YR)

df <- recipe( ~ ., data = df) %>%
  step_upsample(DEATH_15YR) %>%
  prep(training = df) %>% bake(new_data = NULL)
df$BLEVEREOD[is.na(df$BLEVEREOD)]=0
df=df%>% 
  drop_na()


table(df$DEATH_15YR)

df_split <- initial_split(df)
train_data <- training(df_split)
test_data <- testing(df_split)
cv_train <- vfold_cv(train_data, v = 10, repeats = 5, strata = out)

rec_obj <- recipe(DEATH_15YR ~ ., data = train_data)
standardized <- rec_obj %>%
  step_center(all_predictors())  %>%
  step_scale(all_predictors()) %>%
  themis::step_smote (DEATH_15YR)

train_preped <- prep(standardized) %>%
  bake(new_data = NULL)

test_preped <-  prep(standardized) %>%
  bake(new_data = test_data)

require(doParallel)
cores <- parallel::detectCores(logical = FALSE)
registerDoParallel(cores = cores)



#SVM
svm_mod <- 
  svm_rbf(
    cost = tune(), 
    rbf_sigma = tune()
  ) %>%
  set_engine("kernlab") %>%
  set_mode("classification")

svm_mod

svm_workflow <- 
  workflow() %>% 
  add_model(svm_mod) %>% 
  add_recipe(standardized)

svm_workflow

set.seed(345)
svm_res <- 
  svm_workflow %>% 
  tune_grid(grid = 50,
            control = control_stack_grid(),
            metrics = metric_set(roc_auc,f_meas,sens,bal_accuracy), 
            resamples = cv_train)

svm_res %>%
  collect_metrics()

svm_best <- 
  svm_res %>% 
  select_best(metric = "f_meas")

autoplot(svm_res)

final_svm <- finalize_model(
  svm_mod,
  svm_best
)

final_svm

svm_mod_pred = final_svm %>%
  set_engine("kernlab", importance = "permutation") %>%
  fit(DEATH_15YR ~ .,
      data = train_preped
  ) %>% predict(test_preped)%>% 
  bind_cols(test_preped %>% select(DEATH_15YR))

svm_mod_pred%>% accuracy(truth = DEATH_15YR, .pred_class)%>%bind_rows(svm_mod_pred%>% sens(truth = DEATH_15YR, .pred_class))%>%
  bind_rows(svm_mod_pred%>% spec(truth = DEATH_15YR, .pred_class))%>%bind_rows(svm_mod_pred%>% f_meas(truth = DEATH_15YR, .pred_class))

#RF
rf_mod <- 
  rand_forest(mtry = tune(), min_n = tune(), trees = 1000) %>% 
  set_engine("ranger", num.threads = cores) %>% 
  set_mode("classification")

rf_mod


rf_workflow <- 
  workflow() %>% 
  add_model(rf_mod) %>% 
  add_recipe(standardized)

rf_workflow

set.seed(345)
rf_res <- 
  rf_workflow %>% 
  tune_grid(grid = 50,
            control = control_stack_grid(),
            metrics = metric_set(roc_auc,f_meas,sens,bal_accuracy), 
            resamples = cv_train)

rf_res %>%
  collect_metrics()

rf_best <- 
  rf_res %>% 
  select_best(metric = "f_meas")

rf_best

rf_res %>% 
  show_best(metric = "f_meas")

final_rf <- finalize_model(
  rf_mod,
  rf_best
)

final_rf

rf_mod_pred= final_rf %>%
  set_engine("ranger", importance = "permutation") %>%
  fit(DEATH_15YR ~ .,
      data = train_preped
  ) %>% predict(test_preped)%>% 
  bind_cols(test_preped %>% select(DEATH_15YR))

rf_mod_pred%>% accuracy(truth = DEATH_15YR, .pred_class)%>%bind_rows(rf_mod_pred%>% sens(truth = DEATH_15YR, .pred_class))%>%
  bind_rows(rf_mod_pred%>% spec(truth = DEATH_15YR, .pred_class))%>%bind_rows(rf_mod_pred%>% f_meas(truth = DEATH_15YR, .pred_class))

#Elasticent

lr_mod <- 
  logistic_reg(penalty = tune(), mixture = tune()) %>% 
  set_engine("glmnet")
lr_mod

lr_workflow <- 
  workflow() %>% 
  add_model(lr_mod) %>% 
  add_recipe(standardized)

lr_workflow

set.seed(345)
lr_res <- 
  lr_workflow %>% 
  tune_grid(grid = 50,
            control = control_stack_grid(),
            metrics = metric_set(roc_auc,f_meas,sens,bal_accuracy), 
            resamples = cv_train)

lr_res %>%
  collect_metrics()

lr_best <- 
  lr_res %>% 
  select_best(metric = "f_meas")

lr_best

lr_res %>% 
  show_best(metric = "f_meas")

autoplot(lr_res)

final_lr <- finalize_model(
  lr_mod,
  lr_best
)

final_lr

lr_mod_pred= final_lr %>%
  fit(DEATH_15YR ~ .,
      data = train_preped
  ) %>% predict(test_preped)%>% 
  bind_cols(test_preped %>% select(DEATH_15YR))


lr_mod_pred%>% accuracy(truth = DEATH_15YR, .pred_class)%>%bind_rows(lr_mod_pred%>% sens(truth = DEATH_15YR, .pred_class))%>%
  bind_rows(lr_mod_pred%>% spec(truth = DEATH_15YR, .pred_class))%>%bind_rows(lr_mod_pred%>% f_meas(truth = DEATH_15YR, .pred_class))

#Stack package

ens_mod_pred_overal <-rf_mod_pred%>%bind_rows(lr_mod_pred)%>%bind_rows(svm_mod_pred)
ens_mod_pred_overal%>% accuracy(truth = DEATH_15YR, .pred_class)%>%bind_rows(ens_mod_pred_overal%>% sens(truth = DEATH_15YR, .pred_class))%>%
  bind_rows(ens_mod_pred_overal%>% spec(truth = DEATH_15YR, .pred_class))%>%bind_rows(ens_mod_pred_overal%>% f_meas(truth = DEATH_15YR, .pred_class))

#Select

out="DEATH_15YR"

df=Data_merged%>%
  select(c(out,preds_t$MORTALITY))%>% 
  drop_na()

df$DEATH_15YR=as.factor(df$DEATH_15YR)

df <- recipe( ~ ., data = df) %>%
  step_upsample(DEATH_15YR) %>%
  prep(training = df) %>% bake(new_data = NULL)

df_split <- initial_split(df)
train_data <- training(df_split)
test_data <- testing(df_split)
cv_train <- vfold_cv(train_data, v = 10, repeats = 5, strata = out)

rec_obj <- recipe(DEATH_15YR ~ ., data = train_data)
standardized <- rec_obj %>%
  step_center(all_predictors())  %>%
  step_scale(all_predictors()) %>%
  themis::step_smote (DEATH_15YR)

train_preped <- prep(standardized) %>%
  bake(new_data = NULL)

test_preped <-  prep(standardized) %>%
  bake(new_data = test_data)

require(doParallel)
cores <- parallel::detectCores(logical = FALSE)
registerDoParallel(cores = cores)



#SVM
svm_mod <- 
  svm_rbf(
    cost = tune(), 
    rbf_sigma = tune()
  ) %>%
  set_engine("kernlab") %>%
  set_mode("classification")

svm_mod

svm_workflow <- 
  workflow() %>% 
  add_model(svm_mod) %>% 
  add_recipe(standardized)

svm_workflow

set.seed(345)
svm_res <- 
  svm_workflow %>% 
  tune_grid(grid = 50,
            control = control_stack_grid(),
            metrics = metric_set(roc_auc,f_meas,sens,bal_accuracy), 
            resamples = cv_train)

svm_res %>%
  collect_metrics()

svm_best <- 
  svm_res %>% 
  select_best(metric = "f_meas")

autoplot(svm_res)

final_svm <- finalize_model(
  svm_mod,
  svm_best
)

final_svm

svm_mod_pred = final_svm %>%
  set_engine("kernlab", importance = "permutation") %>%
  fit(DEATH_15YR ~ .,
      data = train_preped
  ) %>% predict(test_preped)%>% 
  bind_cols(test_preped %>% select(DEATH_15YR))

svm_mod_pred%>% accuracy(truth = DEATH_15YR, .pred_class)%>%bind_rows(svm_mod_pred%>% sens(truth = DEATH_15YR, .pred_class))%>%
  bind_rows(svm_mod_pred%>% spec(truth = DEATH_15YR, .pred_class))%>%bind_rows(svm_mod_pred%>% f_meas(truth = DEATH_15YR, .pred_class))

#RF
rf_mod <- 
  rand_forest(mtry = tune(), min_n = tune(), trees = 1000) %>% 
  set_engine("ranger", num.threads = cores) %>% 
  set_mode("classification")

rf_mod


rf_workflow <- 
  workflow() %>% 
  add_model(rf_mod) %>% 
  add_recipe(standardized)

rf_workflow

set.seed(345)
rf_res <- 
  rf_workflow %>% 
  tune_grid(grid = 50,
            control = control_stack_grid(),
            metrics = metric_set(roc_auc,f_meas,sens,bal_accuracy), 
            resamples = cv_train)

rf_res %>%
  collect_metrics()

rf_best <- 
  rf_res %>% 
  select_best(metric = "f_meas")

rf_best

rf_res %>% 
  show_best(metric = "f_meas")

final_rf <- finalize_model(
  rf_mod,
  rf_best
)

final_rf

rf_mod_pred= final_rf %>%
  set_engine("ranger", importance = "permutation") %>%
  fit(DEATH_15YR ~ .,
      data = train_preped
  ) %>% predict(test_preped)%>% 
  bind_cols(test_preped %>% select(DEATH_15YR))

rf_mod_pred%>% accuracy(truth = DEATH_15YR, .pred_class)%>%bind_rows(rf_mod_pred%>% sens(truth = DEATH_15YR, .pred_class))%>%
  bind_rows(rf_mod_pred%>% spec(truth = DEATH_15YR, .pred_class))%>%bind_rows(rf_mod_pred%>% f_meas(truth = DEATH_15YR, .pred_class))

#Elasticent

lr_mod <- 
  logistic_reg(penalty = tune(), mixture = tune()) %>% 
  set_engine("glmnet")
lr_mod

lr_workflow <- 
  workflow() %>% 
  add_model(lr_mod) %>% 
  add_recipe(standardized)

lr_workflow

set.seed(345)
lr_res <- 
  lr_workflow %>% 
  tune_grid(grid = 50,
            control = control_stack_grid(),
            metrics = metric_set(roc_auc,f_meas,sens,bal_accuracy), 
            resamples = cv_train)

lr_res %>%
  collect_metrics()

lr_best <- 
  lr_res %>% 
  select_best(metric = "f_meas")

lr_best

lr_res %>% 
  show_best(metric = "f_meas")

autoplot(lr_res)

final_lr <- finalize_model(
  lr_mod,
  lr_best
)

final_lr

lr_mod_pred= final_lr %>%
  fit(DEATH_15YR ~ .,
      data = train_preped
  ) %>% predict(test_preped)%>% 
  bind_cols(test_preped %>% select(DEATH_15YR))


lr_mod_pred%>% accuracy(truth = DEATH_15YR, .pred_class)%>%bind_rows(lr_mod_pred%>% sens(truth = DEATH_15YR, .pred_class))%>%
  bind_rows(lr_mod_pred%>% spec(truth = DEATH_15YR, .pred_class))%>%bind_rows(lr_mod_pred%>% f_meas(truth = DEATH_15YR, .pred_class))

#Stack package

ens_mod_pred_select <-rf_mod_pred%>%bind_rows(lr_mod_pred)%>%bind_rows(svm_mod_pred)
ens_mod_pred_select%>% accuracy(truth = DEATH_15YR, .pred_class)%>%bind_rows(ens_mod_pred_select%>% sens(truth = DEATH_15YR, .pred_class))%>%
  bind_rows(ens_mod_pred_select%>% spec(truth = DEATH_15YR, .pred_class))%>%bind_rows(ens_mod_pred_select%>% f_meas(truth = DEATH_15YR, .pred_class))



setwd("~/Usydney/Atos/select_preds")
save(ens_mod_pred_select,ens_mod_pred_overal,  file = "DEATH_15YR_vips.RData")
