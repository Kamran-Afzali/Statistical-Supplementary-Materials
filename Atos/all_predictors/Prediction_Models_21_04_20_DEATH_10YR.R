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


table(Data_merged$DEATH_10YR)
out="DEATH_10YR"

df=Data_merged%>%
  select(c(out,preds))

df$DEATH_10YR=as.factor(df$DEATH_10YR)


df <- recipe( ~ ., data = df) %>%
  step_knnimpute(DEATH_10YR,BLEVEREOD) %>%
  step_upsample(DEATH_10YR) %>%
  prep(training = df) %>% bake(new_data = NULL)%>% 
  drop_na()


summary(df)

table(df$DEATH_10YR)

df_split <- initial_split(df)
train_data <- training(df_split)
test_data <- testing(df_split)
cv_train <- vfold_cv(train_data, v = 10, repeats = 5, strata = out)

rec_obj <- recipe(DEATH_10YR ~ ., data = train_data)
standardized <- rec_obj %>%
  step_center(all_predictors())  %>%
  step_scale(all_predictors()) %>%
  themis::step_smote (DEATH_10YR)

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
  select_best(metric = "roc_auc")



autoplot(svm_res)

final_svm <- finalize_model(
  svm_mod,
  svm_best
)

final_svm

final_svm %>%
  set_engine("kernlab", importance = "permutation") %>%
  fit(DEATH_10YR ~ .,
      data = train_preped
  ) %>%
  vi( method = "permute", nsim = 10, target = "DEATH_10YR",
      pred_wrapper = kernlab::predict, metric = "auc", reference_class = 1, train = train_preped)%>%
  mutate(rank = dense_rank(desc(Importance)),mod="svm")%>% select(Variable,rank,mod)

svm_mod_pred = final_svm %>%
  set_engine("kernlab", importance = "permutation") %>%
  fit(DEATH_10YR ~ .,
      data = train_preped
  ) %>% predict(test_preped)%>% 
  bind_cols(test_preped %>% select(DEATH_10YR))

svm_mod_pred%>% accuracy(truth = DEATH_10YR, .pred_class)%>%bind_rows(svm_mod_pred%>% sens(truth = DEATH_10YR, .pred_class))%>%
  bind_rows(svm_mod_pred%>% spec(truth = DEATH_10YR, .pred_class))%>%bind_rows(svm_mod_pred%>% f_meas(truth = DEATH_10YR, .pred_class))
#RF
rf_mod <- 
  rand_forest(mtry = tune(), min_n = tune(), trees = 500) %>% 
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

autoplot(rf_res)

final_rf <- finalize_model(
  rf_mod,
  rf_best
)

final_rf

final_rf %>%
  set_engine("ranger", importance = "permutation") %>%
  fit(DEATH_10YR ~ .,
      data = train_preped
  ) %>%
  vi()%>%mutate(rank = dense_rank(desc(Importance)),
                mod="rf")%>% select(Variable,rank,mod)


rf_mod_pred= final_rf %>%
  set_engine("ranger", importance = "permutation") %>%
  fit(DEATH_10YR ~ .,
      data = train_preped
  ) %>% predict(test_preped)%>% 
  bind_cols(test_preped %>% select(DEATH_10YR))

rf_mod_pred%>% accuracy(truth = DEATH_10YR, .pred_class)%>%bind_rows(rf_mod_pred%>% sens(truth = DEATH_10YR, .pred_class))%>%
  bind_rows(rf_mod_pred%>% spec(truth = DEATH_10YR, .pred_class))%>%bind_rows(rf_mod_pred%>% f_meas(truth = DEATH_10YR, .pred_class))

#pfun <- function(object, newdata) predict(object, data = newdata)$predictions
#final_rf %>%
#  set_engine("ranger", importance = "permutation") %>%
#  fit(DEATH_10YR ~ .,
#      data = train_preped
#  ) %>%
#  vi(method = "permute", nsim = 10, target = "DEATH_10YR",
#     pred_wrapper = pfun, metric = "auc", reference_class = 1, train = train_preped)

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

final_lr %>%
  fit(DEATH_10YR ~ .,
      data = train_preped
  ) %>%
  vi()%>%
  mutate(rank = dense_rank(desc(Importance)),mod="glmnet")%>% select(Variable,rank,mod)


lr_mod_pred= final_lr %>%
  fit(DEATH_10YR ~ .,
      data = train_preped
  ) %>% predict(test_preped)%>% 
  bind_cols(test_preped %>% select(DEATH_10YR))


lr_mod_pred%>% accuracy(truth = DEATH_10YR, .pred_class)%>%bind_rows(lr_mod_pred%>% sens(truth = DEATH_10YR, .pred_class))%>%
  bind_rows(lr_mod_pred%>% spec(truth = DEATH_10YR, .pred_class))%>%bind_rows(lr_mod_pred%>% f_meas(truth = DEATH_10YR, .pred_class))
#Stack package
model_ensemble <- 
  stacks() %>%
  add_candidates(svm_res) %>%
  add_candidates(rf_res) %>%
  add_candidates(lr_res) %>%
  blend_predictions() %>%
  fit_members()

model_ensemble

theme_set(theme_bw())
autoplot(model_ensemble)

autoplot(model_ensemble, type = "members")


autoplot(model_ensemble, type = "weights")

collect_parameters(model_ensemble, "svm_res")


ens_mod_pred <-
  test_preped%>%
  bind_cols(predict(model_ensemble, test_preped, type = "class"))

ens_mod_pred <-rf_mod_pred%>%bind_rows(lr_mod_pred)%>%bind_rows(svm_mod_pred)
ens_mod_pred%>% accuracy(truth = DEATH_10YR, .pred_class)%>%bind_rows(ens_mod_pred%>% sens(truth = DEATH_10YR, .pred_class))%>%
  bind_rows(ens_mod_pred%>% spec(truth = DEATH_10YR, .pred_class))%>%bind_rows(ens_mod_pred%>% f_meas(truth = DEATH_10YR, .pred_class))


##importance graroc_auc()##importance graph



svmvip=final_svm %>%
  set_engine("kernlab", importance = "permutation") %>%
  fit(DEATH_10YR ~ .,
      data = train_preped
  ) %>%
  vi( method = "permute", nsim = 10, target = "DEATH_10YR",
      pred_wrapper = kernlab::predict, metric = "auc", reference_class = 1, train = train_preped)%>%
  mutate(rank = dense_rank(desc(Importance)),mod="svm")%>% select(Variable,rank,mod)


rfvip=final_rf %>%
  set_engine("ranger", importance = "permutation") %>%
  fit(DEATH_10YR ~ .,
      data = train_preped
  ) %>%
  vi()%>%mutate(rank = dense_rank(desc(Importance)),
                mod="rf")%>% select(Variable,rank,mod)


lrvip=final_lr %>%
  set_engine("glmnet", importance = "permutation") %>%
  fit(DEATH_10YR ~ .,
      data = train_preped
  ) %>%
  vi()%>%
  mutate(rank = dense_rank(desc(Importance)),mod="glmnet")%>% select(Variable,rank,mod)

vips=svmvip%>%
  bind_rows(rfvip)%>%
  bind_rows(lrvip)%>%
  group_by(Variable)%>%
  summarise(importance=mean(rank))%>%
  arrange(importance)

vips$Variable=as.factor(vips$Variable)
p=ggplot(vips,aes(x=reorder(Variable, importance),y=importance))+
  scale_fill_gradient(low = "green", high = "red") + 
  geom_bar(position="dodge", stat="identity", aes(fill = importance))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  xlab("Predictor")+ ylab("Importance Rank")

ggplotly(p)

setwd("~/Usydney/Atos")
save(vips,ens_mod_pred, file = "DEATH_10YR_vips.RData")
