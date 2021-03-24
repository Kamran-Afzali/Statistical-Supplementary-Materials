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


df_split <- initial_split(df)
train_data <- training(df_split)
test_data <- testing(df_split)
cv_train <- vfold_cv(train_data, v = 10, repeats = 5, strata = out)

rec_obj <- recipe(HU_1YR ~ ., data = df)
imputed <- rec_obj %>%
  step_knnimpute(all_predictors()) 
ind_vars <- imputed %>%
  step_dummy(all_predictors(), -all_numeric()) 
standardized <- ind_vars %>%
  step_center(all_predictors())  %>%
  step_scale(all_predictors()) 

#SVM

#RF

#Elasticent

#Stack package
https://stacks.tidymodels.org/articles/classification.html

https://blog--simonpcouch.netlify.app/blog/gentle-intro-stacks/