
setwd(paste(dirname(rstudioapi::getActiveDocumentContext()$path),"",sep = ""))
load("~/OneDrive - Universite de Montreal/Usydney/data.RData")

library(keras)
library(haven)
library(tidyverse)
library(tidymodels)
library(stacks)
library(vip)
library(pdp)
library(sparkline)
library(plotly)
library(tensorflow)
library(keras)
library(tensorflow)


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
  select(c(out,preds))%>% 
  drop_na()

df$HU_1YR=as.factor(df$HU_1YR)

df_split <- initial_split(df)
train_data <- training(df_split)
test_data <- testing(df_split)
cv_train <- vfold_cv(train_data, v = 10, repeats = 5, strata = out)

rec_obj <- recipe(HU_1YR ~ ., data = train_data)
standardized <- rec_obj %>%
  step_center(all_predictors())  %>%
  step_scale(all_predictors()) %>%
  themis::step_smote (HU_1YR)

train_preped <- prep(standardized) %>%
  bake(new_data = NULL)

test_preped <-  prep(standardized) %>%
  bake(new_data = test_data)

train_preped_y=train_preped$HU_1YR
test_preped_y=test_preped$HU_1YR


train_preped_x=train_preped%>%select(-HU_1YR) 
test_preped_x=test_preped%>%select(-HU_1YR) 



devtools::install_github("rstudio/keras",dependencies = T)
devtools::install_github("rstudio/tensorflow",dependencies = T)
keras::install_keras()
tensorflow::install_tensorflow()
tensorflow::tf_config()
reticulate::py_install(envname = "r-reticulate", packages = "numpy")
reticulate::conda_list()
reticulate::py_config()
tf$constant("Hellow Tensorflow")
reticulate::py_available()
keras:::keras_version()
tensorflow::tf_config()
keras::install_keras(method = c("auto"),
              conda = "auto", version = "default", tensorflow = "default")
tensorflow::install_tensorflow(method = c("auto"),
                   conda = "auto", version = "default", conda_python_version = "3.7")
reticulate::py_config()
virtualenv_remove("r-reticulate")
miniconda_update(path = miniconda_path())
reticulate::configure_environment()
reticulate::use_condaenv("r-reticulate")
reticulate::py_config()
numpy <- import("numpy")



model <- keras::keras_model_sequential()
model_4 <- keras_model_sequential() %>% 
  layer_dense(units = 512, input_shape = ncol(train_preped_x), activation = 'relu') %>%
  layer_batch_normalization() %>% 
  layer_dense(units = 512, activation = "relu", kernel_regularizer = regularizer_l2(0.001)) %>% 
  layer_dropout(rate = 0.5) %>% 
  layer_dense(units = 512, activation = 'relu', kernel_regularizer = regularizer_l2(0.001)) %>%
  layer_dense(units = 1, activation = "sigmoid")
model_4 %>% compile(
  optimizer = "adam",
  loss = "binary_crossentropy",
  metrics = "accuracy"
)
model_4_hist <- model_4 %>% fit(
  train_preped_x,
  train_preped_y,
  epochs = 30,
  batch_size = 128,
  validation_split = 0.8)
plot(model_4_hist)


