setwd(paste(dirname(rstudioapi::getActiveDocumentContext()$path),"",sep = ""))
load("~/OneDrive - Universite de Montreal/Usydney/data.RData")
library(haven)
library(tidyverse)
library(tidymodels)

#data_Atos <- read_sav("HU OD 1 5 10y model.sav")
#View(data_Atos)


Out_Comes=c("STA_1YR","STA_5YR","MTA_5YR","STA_10YR","MTA_10YR","LTA_10yr","HU_1YR","HU_5YR","HU_10YR","OD_upto1YR","OD_upto5YR","OD_upto10YR","DEATH_1YR","DEATH_5YR","DEATH_10YR","DEATH_15YR")
#colnames(data_Atos)%in%preds

#DATA$id0101=as.numeric(as.character(DATA$id0101))
#data_Atos$id0101=as.numeric(as.character(data_Atos$id0101))
#merge the data 
Data_merged=DATA %>% inner_join(data_Atos, by = "id0101")


#check the pred input with new Excel file
##good for now but have to check later!
table(Data_merged$HU_1YR)
out="HU_1YR"

df=Data_merged%>%
  select(c(out,preds))

df$HU_1YR=as.factor(df$HU_1YR)

df_split <- initial_split(df)
train_data <- training(df_split)
test_data <- testing(df_split)
cv_train <- vfold_cv(train_data, v = 10, repeats = 5, strata = out)

rec_obj <- recipe(HU_1YR ~ ., data = df)
imputed <- rec_obj %>%
  step_knnimpute(all_predictors(),all_outcomes()) 
ind_vars <- imputed %>%
  step_dummy(all_predictors(), -all_numeric()) 
standardized <- ind_vars %>%
  step_center(all_predictors())  %>%
  step_scale(all_predictors()) 
cores <- parallel::detectCores()
cores
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
  tune_grid(grid = 25,
            control = control_grid(save_pred = TRUE),
            metrics = metric_set(roc_auc), 
            resamples = cv_train)

svm_res %>%
  collect_metrics()
svm_best <- 
  svm_res %>% 
  select_best(metric = "roc_auc")

svm_best

svm_res %>% 
  show_best(metric = "roc_auc")

autoplot(svm_res)



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
  tune_grid(grid = 25,
            control = control_grid(save_pred = TRUE),
            metrics = metric_set(roc_auc), 
            resamples = cv_train)

rf_res %>%
  collect_metrics()
rf_best <- 
  rf_res %>% 
  select_best(metric = "roc_auc")

rf_best

rf_res %>% 
  show_best(metric = "roc_auc")

autoplot(rf_res)
#Elasticent

lr_mod <- 
  logistic_reg(penalty = tune(), mixture = 1) %>% 
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
  tune_grid(grid = 25,
            control = control_grid(save_pred = TRUE),
            metrics = metric_set(roc_auc), 
            resamples = cv_train)

lr_res %>%
  collect_metrics()
lr_best <- 
  rf_res %>% 
  select_best(metric = "roc_auc")

lr_best

lr_res %>% 
  show_best(metric = "roc_auc")

autoplot(lr_res)

#Stack package
https://stacks.tidymodels.org/articles/classification.html

https://blog--simonpcouch.netlify.app/blog/gentle-intro-stacks/