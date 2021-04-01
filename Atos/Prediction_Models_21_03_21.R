setwd(paste(dirname(rstudioapi::getActiveDocumentContext()$path),"",sep = ""))
load("~/OneDrive - Universite de Montreal/Usydney/data.RData")

library(haven)
library(tidyverse)
library(tidymodels)
library(stacks)
library(vip)
library(pdp)
library(sparkline)
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
standardized <- imputed %>%
  step_center(all_predictors())  %>%
  step_scale(all_predictors()) %>%
  themis::step_smote (HU_1YR)

data_prep <- prep(standardized) %>%
bake(new_data = NULL)

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
            control = control_stack_grid(),
            metrics = metric_set(roc_auc), 
            resamples = cv_train)

svm_res %>%
  collect_metrics()
svm_best <- 
  svm_res %>% 
  select_best(metric = "roc_auc")



svm_res %>% 
  show_best(metric = "roc_auc")

autoplot(svm_res)

final_svm <- finalize_model(
  svm_mod,
  svm_best
)

final_svm

final_svm %>%
  set_engine("kernlab", importance = "permutation") %>%
  fit(HU_1YR ~ .,
      data = data_prep
  ) %>%
  vi( method = "permute", nsim = 10, target = "HU_1YR",
     pred_wrapper = kernlab::predict, metric = "auc", reference_class = 1, train = data_prep)%>%
  mutate(rank = dense_rank(desc(Importance)),mod="svm")%>% select(Variable,rank,mod)


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
  tune_grid(grid = 10,
            control = control_stack_grid(),
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

final_rf <- finalize_model(
  rf_mod,
  rf_best
)

final_rf

final_rf %>%
  set_engine("ranger", importance = "permutation") %>%
  fit(HU_1YR ~ .,
      data = data_prep
  ) %>%
  vi()%>%mutate(rank = dense_rank(desc(Importance)),
             mod="rf")%>% select(Variable,rank,mod)

pfun <- function(object, newdata) predict(object, data = newdata)$predictions
final_rf %>%
  set_engine("ranger", importance = "permutation") %>%
  fit(HU_1YR ~ .,
      data = data_prep
  ) %>%
  vi(method = "permute", nsim = 10, target = "HU_1YR",
     pred_wrapper = pfun, metric = "auc", reference_class = 1, train = data_prep)

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
            control = control_stack_grid(),
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

final_lr <- finalize_model(
  lr_mod,
  lr_best
)

final_lr

final_lr %>%
  set_engine("glmnet", importance = "permutation") %>%
  fit(HU_1YR ~ .,
      data = data_prep
  ) %>%
  vi()%>%
  mutate(rank = dense_rank(desc(Importance)),mod="glmnet")%>% select(Variable,rank,mod)


#Stack package
model_ensemble <- 
  # initialize the stack
  stacks() %>%
  # add candidate members
  add_candidates(svm_res) %>%
  add_candidates(rf_res) %>%
  add_candidates(lr_res) %>%
  # determine how to combine their predictions
  blend_predictions() %>%
  # fit the candidates with nonzero stacking coefficients
  fit_members()

model_ensemble

theme_set(theme_bw())
autoplot(model_ensemble)

autoplot(model_ensemble, type = "members")


autoplot(model_ensemble, type = "weights")

collect_parameters(model_ensemble, "svm_res")


ens_mod_pred <-
  data_prep%>%
  bind_cols(predict(model_ensemble, ., type = "prob"))


##importance graph



svmvip=final_svm %>%
  set_engine("kernlab", importance = "permutation") %>%
  fit(HU_1YR ~ .,
      data = data_prep
  ) %>%
  vi( method = "permute", nsim = 10, target = "HU_1YR",
      pred_wrapper = kernlab::predict, metric = "auc", reference_class = 1, train = data_prep)%>%
  mutate(rank = dense_rank(desc(Importance)),mod="svm")%>% select(Variable,rank,mod)


rfvip=final_rf %>%
  set_engine("ranger", importance = "permutation") %>%
  fit(HU_1YR ~ .,
      data = data_prep
  ) %>%
  vi()%>%mutate(rank = dense_rank(desc(Importance)),
                mod="rf")%>% select(Variable,rank,mod)


lrvip=final_lr %>%
  set_engine("glmnet", importance = "permutation") %>%
  fit(HU_1YR ~ .,
      data = data_prep
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
  p=ggplot(vips,aes(x=reorder(Variable, -importance),y=importance))+
  scale_fill_gradient(low = "green", high = "red") + 
  geom_bar(position="dodge", stat="identity", aes(fill = importance))+
   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
    xlab("Predictor")+ ylab("Importance Rank")

  ggplotly(p)

#References

https://stacks.tidymodels.org/articles/classification.html

https://blog--simonpcouch.netlify.app/blog/gentle-intro-stacks/
  
https://www.tidyverse.org/blog/2020/02/themis-0-1-0/
  
https://juliasilge.com/blog/himalayan-climbing/
  
https://www.tidymodels.org/learn/models/sub-sampling/
