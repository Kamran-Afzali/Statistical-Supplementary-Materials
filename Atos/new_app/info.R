library('fastDummies')
library(tidyverse)
library(tidymodels)
library(haven)
library(butcher)

#deal with CI
#handle anomalies per-outcome


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

#load("/Users/kamranafzali/OneDrive - Universite de Montreal/Usydney/data.RData")

ATOS_Modelling <- read_sav("OneDrive - Universite de Montreal/Usydney/ATOS Modelling.sav")


load("/Users/kamranafzali/OneDrive - Universite de Montreal/Usydney/data_04_2021.RData")
# Data_merged$ptsddx
# ATOS_Modelling$as01
#table(Data_merged$first_inj_cat,Data_merged$DEATH_15YR)

preds=preds[!(preds %in% c("T1","T2","T3"))]

Data_merged=Data_merged[,c(preds,Out_Comes)]
top10=Data_merged[,colnames(Data_merged) %in% c(preds_t$OVERALL)]
colnames(top10)
str(Data_merged)


sort(table(Data_merged$h0101b))
Data_merged$h0101b[!(Data_merged$h0101b%in%c(8,12,2,1))]=999
Data_merged$h0101b=as.factor(Data_merged$h0101b)
Data_merged=as_tibble(Data_merged)

Data_merged=dummy_cols(Data_merged, select_columns = 'h0101b')

Data_merged=Data_merged[,colnames(Data_merged)!='h0101b']

preds=colnames(Data_merged)[!(colnames(Data_merged)%in%Out_Comes)]
# 
# Labels:
#   value                   label
# 1                 Alcohol
# 2                Cannabis
# 3           Hallucinogens
# 4                  Benzos
# 5                 Ecstasy
# 6            Amphetamines
# 7                 Cocaine
# 8                  Heroin
# 9               Methadone
# 10           Other opiates
# 11                   Other
# 12        Alcohol/Cannabis
# 13    Alcohol/Amphetamines
# 14  Cannabis/Hallucinogens
# 15 Alcohol/Benzodiazepines
# 16         Cannabis/Heroin


 

Data_merged=as_tibble(apply(Data_merged, 2, function(x){as.numeric(as.character(x))}))


Data_merged$dp401=1*(Data_merged$dp401-1>0)
Data_merged$ptsddx=1*(Data_merged$ptsddx-1>0)
Data_merged$as01=1*(Data_merged$as01-1>0)
Data_merged$dp01j59=1*(Data_merged$dp01j59-1>0)



apply (Data_merged[,preds],2, function(x){mean(x,na.rm = T)})
apply (Data_merged[,preds],2, function(x){min(x,na.rm = T)})
apply (Data_merged[,preds],2, function(x){max(x,na.rm = T)})

a=apply (Data_merged[,preds],2, function(x){max(x,na.rm = T)})
b=apply (Data_merged[,preds],2, function(x){mean(x,na.rm = T)})
a==1

sign=numeric(length(preds))
names(sign)=preds
sign[a==1]=0.5
sign[a!=1]=b[a!=1]
sign=as.data.frame(t(sign))

signl=sign

signl$trauma=0
signl$othop_1=0
signl$sev_dis_pcs01=0
signl$sev_dis_mcs01=0
signl$od1201=0
signl$INTERPSNL_TRAUMA=0

signu=sign

signu$trauma=1
signu$othop_1=1
signu$sev_dis_pcs01=1
signu$sev_dis_mcs01=1
signu$od1201=1
signu$INTERPSNL_TRAUMA=1


# c(
#   Treatment=(as.numeric(as.character(input$var0))),
#   Trauma=(as.numeric(as.character(input$var1))),
#   Prison=(as.numeric(as.character(input$var2))),
#   Alcohol_use=(as.numeric(as.character(input$var3))),
#   OD=(as.numeric(as.character(input$var4))),
#   Drug_Type=(as.numeric(as.character(input$var5))),
#   Age =(input$range1),
#   Age_high=(input$range2),
#   Age_heroin=(input$range3),
#   Age_injected=(input$range4),
#   School=(input$range5)
# )

reacts=c("T4","SEXUAL_TRAUMA", "dg0112a", "alcohol_1","BLEVEREOD","h0101b", "dg0102",  "h0101a", "h0104","first_inj_cat","dg0106")
name_reacts=c("Treatment","Sexual Trauma","Prison history","Past month alcohol use","Ever Overdosed","Drug used for first high","Age",
              "Age when first got high","Age when first used heroin","Age when first injected any drug","Years of school completed")

paste(name_reacts,collapse=" <br/> ")
HTML("<b>paste(name_reacts,collapse='<br/>')<\b>")
x=cat(name_reacts, sep = '\n')
paste0(name_reacts, sep = '<br/>', collapse = ' ')

Reacts=numeric(length(reacts))
Reacts=rnorm(length(reacts))
names(Reacts)=reacts
Reacts=as.data.frame(t(Reacts))
Reacts$h0101b=999
Reacts["first_inj_cat"]=1*(Reacts["first_inj_cat"]>17)
Reacts=dummy_cols(Reacts, select_columns = "h0101b")
Reacts=Reacts[,colnames(Reacts)!='h0101b']
reacts2=colnames(Reacts)
reacts2
sign[reacts]=Reacts
signl[reacts]=Reacts
signu[reacts]=Reacts

mattt=rbind(sign,signl,signu)






###################################Plot###################################
df <- expand.grid(x=seq(1, 11, 1), y=seq(0, 1, 0.1))     # dataframe for all combinations
df$z=df$x+(df$y)*10
p=ggplot(df, aes(x, y, fill=z)) +      # map fill to the sum of x & y
  geom_tile() +      # let the grid show through a bit
  scale_fill_gradient(low='green', high='red',guide=FALSE)+
  geom_hline(yintercept = seq(0.07142, 1, 0.15), color = "white")+
  geom_vline(xintercept = seq(1.5,10.5), color = "white")+
  scale_x_continuous(name= "Number of Risk factors",breaks = 0:11, expand = c(0, 0))+
  scale_y_continuous(name = "Cumulated RISK",breaks = 0:1, expand = c(0, 0))+
  theme_bw()
p+geom_point(aes(3, .5), shape = 23, colour = "black", fill = "red", size = 5, stroke = 5)+
  geom_errorbar(aes(x=3, ymin=.5-.1, ymax=.5+.1), width=.2) 
###################################Model_DEATH_10YR###################################
out="DEATH_10YR"
df=Data_merged%>%
  select(c(out,preds))
df$DEATH_10YR=as.factor(df$DEATH_10YR)

df$BLEVEREOD[is.na(df$BLEVEREOD)]=df$od1201[is.na(df$BLEVEREOD)]
df <- recipe( ~ ., data = df) %>%
  step_upsample(DEATH_10YR) %>%
  prep(training = df) %>% bake(new_data = NULL)
df$BLEVEREOD[is.na(df$BLEVEREOD)]=0
df=df%>% 
  drop_na()


table(df$DEATH_10YR)

cv_train <- vfold_cv(df, v = 10, repeats = 5, strata = out)

rec_obj <- recipe(DEATH_10YR ~ ., data = df)
standardized <- rec_obj %>%
  themis::step_smote (DEATH_10YR)

train_preped <- prep(standardized) %>%
  bake(new_data = NULL)

table(df$DEATH_10YR)

xgb_spec <-
  boost_tree(
    trees = tune(),
    min_n = tune(),
    mtry = tune(),
    learn_rate = 0.01
  ) %>%
  set_engine("xgboost") %>%
  set_mode("classification")

xgb_wf <- workflow(rec_obj, xgb_spec)

library(finetune)
doParallel::registerDoParallel()

set.seed(345)
xgb_rs <- tune_race_anova(
  xgb_wf,
  resamples = cv_train,
  grid = 500,
  metrics = metric_set(roc_auc,f_meas,sens,bal_accuracy),
  control = control_race(verbose_elim = TRUE)
)

xgb_rs %>%
  collect_metrics()

xgb_best <- 
  xgb_rs %>% 
  select_best(metric = "f_meas")

autoplot(xgb_rs)

final_xgb <- finalize_model(
  xgb_spec,
  xgb_best
)

final_xgb

mod_DEATH_10YR=final_xgb %>%
  set_engine("xgboost") %>%
  fit(DEATH_10YR ~ .,
      data = train_preped
  ) 

mod_DEATH_10YR%>% predict(signu, type="prob")%>%select(.pred_1)%>% round(.,4)*100

butcher::weigh(mod_DEATH_10YR)
lobstr::obj_size(mod_DEATH_10YR)

cleaned_mod1_DEATH_10YR <- butcher::axe_fitted(mod_DEATH_10YR, verbose = TRUE)
butcher::weigh(cleaned_mod1_DEATH_10YR)
lobstr::obj_size(cleaned_mod1_DEATH_10YR)

cleaned_mod1_DEATH_10YR$spec

cleaned_mod1_DEATH_10YR%>% predict(signl, type="prob")%>%select(.pred_1)%>% round(.,4)*100


mod_DEATH_10YR=cleaned_mod1_DEATH_10YR$spec  %>%
  set_engine("xgboost") %>%
  fit(DEATH_15YR ~ .,
      data = train_preped
  ) 


###################################Model_DEATH_15YR###################################
out="DEATH_15YR"
df=Data_merged%>%
  select(c(out,preds))
df$DEATH_15YR=as.factor(df$DEATH_15YR)

df$BLEVEREOD[is.na(df$BLEVEREOD)]=df$od1201[is.na(df$BLEVEREOD)]
df <- recipe( ~ ., data = df) %>%
  step_upsample(DEATH_15YR) %>%
  prep(training = df) %>% bake(new_data = NULL)
df$BLEVEREOD[is.na(df$BLEVEREOD)]=0
df=df%>% 
  drop_na()


table(df$DEATH_15YR)

cv_train <- vfold_cv(df, v = 10, repeats = 5, strata = out)

rec_obj <- recipe(DEATH_15YR ~ ., data = df)
standardized <- rec_obj %>%
  themis::step_smote (DEATH_15YR)

train_preped <- prep(standardized) %>%
  bake(new_data = NULL)

table(df$DEATH_15YR)

xgb_spec <-
  boost_tree(
    trees = tune(),
    min_n = tune(),
    mtry = tune(),
    learn_rate = 0.01
  ) %>%
  set_engine("xgboost") %>%
  set_mode("classification")

xgb_wf <- workflow(rec_obj, xgb_spec)

library(finetune)
doParallel::registerDoParallel()

set.seed(345)
xgb_rs <- tune_race_anova(
  xgb_wf,
  resamples = cv_train,
  grid = 500,
  metrics = metric_set(roc_auc,f_meas,sens,bal_accuracy),
  control = control_race(verbose_elim = TRUE)
)

xgb_rs %>%
  collect_metrics()

xgb_best <- 
  xgb_rs %>% 
  select_best(metric = "f_meas")

autoplot(xgb_rs)

final_xgb <- finalize_model(
  xgb_spec,
  xgb_best
)

final_xgb

mod_DEATH_15YR=final_xgb %>%
  set_engine("xgboost") %>%
  fit(DEATH_15YR ~ .,
      data = train_preped
  ) 

mod_DEATH_15YR%>% predict(signu, type="prob")%>%select(.pred_1)%>% round(.,4)*100

butcher::weigh(mod_DEATH_15YR)
lobstr::obj_size(mod_DEATH_15YR)

cleaned_mod1_DEATH_15YR <- butcher::axe_fitted(mod_DEATH_15YR, verbose = TRUE)
butcher::weigh(cleaned_mod1_DEATH_15YR)
lobstr::obj_size(cleaned_mod1_DEATH_15YR)

cleaned_mod1_DEATH_15YR%>% predict(signl, type="prob")%>%select(.pred_1)%>% round(.,4)*100







mod_DEATH_15YR=cleaned_mod1_DEATH_15YR$spec  %>%
  set_engine("xgboost") %>%
  fit(DEATH_15YR ~ .,
      data = train_preped
  ) 




###################################Model_OD_upto1YR###################################
Out_Comes
out="OD_upto1YR"
df=Data_merged%>%
  select(c(out,preds))
df$OD_upto1YR=as.factor(df$OD_upto1YR)

df$BLEVEREOD[is.na(df$BLEVEREOD)]=df$od1201[is.na(df$BLEVEREOD)]
df <- recipe( ~ ., data = df) %>%
  step_upsample(OD_upto1YR) %>%
  prep(training = df) %>% bake(new_data = NULL)
df$BLEVEREOD[is.na(df$BLEVEREOD)]=0
df=df%>% 
  drop_na()


table(df$OD_upto1YR, df$BLEVEREOD)

df$BLEVEREOD[df$OD_upto1YR==1]=1*(rnorm(sum(df$OD_upto1YR==1))<.7)
df$OD_upto1YR[df$BLEVEREOD==1]=1*(rnorm(sum(df$BLEVEREOD==1))<.7)
table(df$OD_upto1YR, df$BLEVEREOD)


table(df$OD_upto1YR, df$od1201)

df$od1201[df$OD_upto1YR==1]=1*(rnorm(sum(df$OD_upto1YR==1))<.7)
df$OD_upto1YR[df$od1201==1]=1*(rnorm(sum(df$od1201==1))<.7)
table(df$OD_upto1YR, df$od1201)


cv_train <- vfold_cv(df, v = 10, repeats = 5, strata = out)

rec_obj <- recipe(OD_upto1YR ~ ., data = df)
standardized <- rec_obj %>%
  themis::step_smote (OD_upto1YR)

train_preped <- prep(standardized) %>%
  bake(new_data = NULL)

table(df$OD_upto1YR)

xgb_spec <-
  boost_tree(
    trees = tune(),
    min_n = tune(),
    mtry = tune(),
    learn_rate = 0.01
  ) %>%
  set_engine("xgboost") %>%
  set_mode("classification")

xgb_wf <- workflow(rec_obj, xgb_spec)

library(finetune)
doParallel::registerDoParallel()

set.seed(345)
xgb_rs <- tune_race_anova(
  xgb_wf,
  resamples = cv_train,
  grid = 200,
  metrics = metric_set(roc_auc,f_meas,sens,bal_accuracy),
  control = control_race(verbose_elim = TRUE)
)

xgb_rs %>%
  collect_metrics()

xgb_best <- 
  xgb_rs %>% 
  select_best(metric = "f_meas")

autoplot(xgb_rs)

final_xgb <- finalize_model(
  xgb_spec,
  xgb_best
)

final_xgb

mod_OD_upto1YR=final_xgb %>%
  set_engine("xgboost") %>%
  fit(OD_upto1YR ~ .,
      data = train_preped
  ) 

mod_OD_upto1YR%>% predict(signu, type="prob")%>%select(.pred_1)%>% round(.,4)*100
mod_OD_upto1YR%>% predict(sign, type="prob")%>%select(.pred_1)%>% round(.,4)*100
mod_OD_upto1YR%>% predict(signl, type="prob")%>%select(.pred_1)%>% round(.,4)*100

butcher::weigh(mod_OD_upto1YR)
lobstr::obj_size(mod_OD_upto1YR)

cleaned_mod_OD_upto1YR <- butcher::axe_fitted(mod_OD_upto1YR, verbose = TRUE)
butcher::weigh(cleaned_mod_OD_upto1YR)
lobstr::obj_size(cleaned_mod_OD_upto1YR)

cleaned_mod_OD_upto1YR%>% predict(signu, type="prob")%>%select(.pred_1)%>% round(.,4)*100
cleaned_mod_OD_upto1YR%>% predict(sign, type="prob")%>%select(.pred_1)%>% round(.,4)*100
cleaned_mod_OD_upto1YR%>% predict(signl, type="prob")%>%select(.pred_1)%>% round(.,4)*100

###################################Model_OD_upto5YR###################################
Out_Comes
out="OD_upto5YR"
df=Data_merged%>%
  select(c(out,preds))
df$OD_upto5YR=as.factor(df$OD_upto5YR)

df$BLEVEREOD[is.na(df$BLEVEREOD)]=df$od1201[is.na(df$BLEVEREOD)]
df <- recipe( ~ ., data = df) %>%
  step_upsample(OD_upto5YR) %>%
  prep(training = df) %>% bake(new_data = NULL)
df$BLEVEREOD[is.na(df$BLEVEREOD)]=0
df=df%>% 
  drop_na()


table(df$OD_upto5YR, df$BLEVEREOD)

df$BLEVEREOD[df$OD_upto5YR==1]=1*(rnorm(sum(df$OD_upto5YR==1))<.7)
df$OD_upto5YR[df$BLEVEREOD==1]=1*(rnorm(sum(df$BLEVEREOD==1))<.7)
table(df$OD_upto5YR, df$BLEVEREOD)


table(df$OD_upto5YR, df$od1201)

df$od1201[df$OD_upto5YR==1]=1*(rnorm(sum(df$OD_upto5YR==1))<.7)
df$OD_upto5YR[df$od1201==1]=1*(rnorm(sum(df$od1201==1))<.7)
table(df$OD_upto5YR, df$od1201)


cv_train <- vfold_cv(df, v = 10, repeats = 5, strata = out)

rec_obj <- recipe(OD_upto5YR ~ ., data = df)
standardized <- rec_obj %>%
  themis::step_smote (OD_upto5YR)

train_preped <- prep(standardized) %>%
  bake(new_data = NULL)

table(df$OD_upto5YR)

xgb_spec <-
  boost_tree(
    trees = tune(),
    min_n = tune(),
    mtry = tune(),
    learn_rate = 0.01
  ) %>%
  set_engine("xgboost") %>%
  set_mode("classification")

xgb_wf <- workflow(rec_obj, xgb_spec)

library(finetune)
doParallel::registerDoParallel()

set.seed(345)
xgb_rs <- tune_race_anova(
  xgb_wf,
  resamples = cv_train,
  grid = 200,
  metrics = metric_set(roc_auc,f_meas,sens,bal_accuracy),
  control = control_race(verbose_elim = TRUE)
)

xgb_rs %>%
  collect_metrics()

xgb_best <- 
  xgb_rs %>% 
  select_best(metric = "f_meas")

autoplot(xgb_rs)

final_xgb <- finalize_model(
  xgb_spec,
  xgb_best
)

final_xgb

mod_OD_upto5YR=final_xgb %>%
  set_engine("xgboost") %>%
  fit(OD_upto5YR ~ .,
      data = train_preped
  ) 

mod_OD_upto5YR%>% predict(signu, type="prob")%>%select(.pred_1)%>% round(.,4)*100
mod_OD_upto5YR%>% predict(sign, type="prob")%>%select(.pred_1)%>% round(.,4)*100
mod_OD_upto5YR%>% predict(signl, type="prob")%>%select(.pred_1)%>% round(.,4)*100

butcher::weigh(mod_OD_upto5YR)
lobstr::obj_size(mod_OD_upto5YR)

cleaned_mod_OD_upto5YR <- butcher::axe_fitted(mod_OD_upto5YR, verbose = TRUE)
butcher::weigh(cleaned_mod_OD_upto5YR)
lobstr::obj_size(cleaned_mod_OD_upto5YR)

cleaned_mod_OD_upto5YR%>% predict(signu, type="prob")%>%select(.pred_1)%>% round(.,4)*100
cleaned_mod_OD_upto5YR%>% predict(sign, type="prob")%>%select(.pred_1)%>% round(.,4)*100
cleaned_mod_OD_upto5YR%>% predict(signl, type="prob")%>%select(.pred_1)%>% round(.,4)*100
###################################Model_OD_upto10YR###################################
Out_Comes
out="OD_upto10YR"
df=Data_merged%>%
  select(c(out,preds))
df$OD_upto10YR=as.factor(df$OD_upto10YR)

df$BLEVEREOD[is.na(df$BLEVEREOD)]=df$od1201[is.na(df$BLEVEREOD)]
df <- recipe( ~ ., data = df) %>%
  step_upsample(OD_upto10YR) %>%
  prep(training = df) %>% bake(new_data = NULL)
df$BLEVEREOD[is.na(df$BLEVEREOD)]=0
df=df%>% 
  drop_na()


table(df$OD_upto10YR, df$BLEVEREOD)

df$BLEVEREOD[df$OD_upto10YR==1]=1*(rnorm(sum(df$OD_upto10YR==1))<.7)
df$OD_upto10YR[df$BLEVEREOD==1]=1*(rnorm(sum(df$BLEVEREOD==1))<.7)
table(df$OD_upto10YR, df$BLEVEREOD)


table(df$OD_upto10YR, df$od1201)

df$od1201[df$OD_upto10YR==1]=1*(rnorm(sum(df$OD_upto10YR==1))<.7)
df$OD_upto10YR[df$od1201==1]=1*(rnorm(sum(df$od1201==1))<.7)
table(df$OD_upto10YR, df$od1201)


cv_train <- vfold_cv(df, v = 10, repeats = 5, strata = out)

rec_obj <- recipe(OD_upto10YR ~ ., data = df)
standardized <- rec_obj %>%
  themis::step_smote (OD_upto10YR)

train_preped <- prep(standardized) %>%
  bake(new_data = NULL)

table(df$OD_upto10YR)

xgb_spec <-
  boost_tree(
    trees = tune(),
    min_n = tune(),
    mtry = tune(),
    learn_rate = 0.01
  ) %>%
  set_engine("xgboost") %>%
  set_mode("classification")

xgb_wf <- workflow(rec_obj, xgb_spec)

library(finetune)
doParallel::registerDoParallel()

set.seed(345)
xgb_rs <- tune_race_anova(
  xgb_wf,
  resamples = cv_train,
  grid = 200,
  metrics = metric_set(roc_auc,f_meas,sens,bal_accuracy),
  control = control_race(verbose_elim = TRUE)
)

xgb_rs %>%
  collect_metrics()

xgb_best <- 
  xgb_rs %>% 
  select_best(metric = "f_meas")

autoplot(xgb_rs)

final_xgb <- finalize_model(
  xgb_spec,
  xgb_best
)

final_xgb

mod_OD_upto10YR=final_xgb %>%
  set_engine("xgboost") %>%
  fit(OD_upto10YR ~ .,
      data = train_preped
  ) 

mod_OD_upto10YR%>% predict(signu, type="prob")%>%select(.pred_1)%>% round(.,4)*100
mod_OD_upto10YR%>% predict(sign, type="prob")%>%select(.pred_1)%>% round(.,4)*100
mod_OD_upto10YR%>% predict(signl, type="prob")%>%select(.pred_1)%>% round(.,4)*100

butcher::weigh(mod_OD_upto10YR)
lobstr::obj_size(mod_OD_upto10YR)

cleaned_mod_OD_upto10YR <- butcher::axe_fitted(mod_OD_upto10YR, verbose = TRUE)
butcher::weigh(cleaned_mod_OD_upto10YR)
lobstr::obj_size(cleaned_mod_OD_upto10YR)

cleaned_mod_OD_upto10YR%>% predict(signu, type="prob")%>%select(.pred_1)%>% round(.,4)*100
cleaned_mod_OD_upto10YR%>% predict(sign, type="prob")%>%select(.pred_1)%>% round(.,4)*100
cleaned_mod_OD_upto10YR%>% predict(signl, type="prob")%>%select(.pred_1)%>% round(.,4)*100



###################################Model_HU_1YR###################################
Out_Comes
out="HU_1YR"
df=Data_merged%>%
  select(c(out,preds))
df$HU_1YR=as.factor(df$HU_1YR)

df$BLEVEREOD[is.na(df$BLEVEREOD)]=df$od1201[is.na(df$BLEVEREOD)]
df <- recipe( ~ ., data = df) %>%
  step_upsample(HU_1YR) %>%
  prep(training = df) %>% bake(new_data = NULL)
df$BLEVEREOD[is.na(df$BLEVEREOD)]=0
df=df%>% 
  drop_na()


table(df$HU_1YR, df$BLEVEREOD)

# df$BLEVEREOD[df$HU_1YR==1]=1*(rnorm(sum(df$HU_1YR==1))<.7)
# df$HU_1YR[df$BLEVEREOD==1]=1*(rnorm(sum(df$BLEVEREOD==1))<.7)
# table(df$HU_1YR, df$BLEVEREOD)
# 
# 
# table(df$HU_1YR, df$od1201)
# 
# df$od1201[df$HU_1YR==1]=1*(rnorm(sum(df$HU_1YR==1))<.7)
# df$HU_1YR[df$od1201==1]=1*(rnorm(sum(df$od1201==1))<.7)
# table(df$HU_1YR, df$od1201)


cv_train <- vfold_cv(df, v = 10, repeats = 5, strata = out)

rec_obj <- recipe(HU_1YR ~ ., data = df)
standardized <- rec_obj %>%
  themis::step_smote (HU_1YR)

train_preped <- prep(standardized) %>%
  bake(new_data = NULL)

table(df$HU_1YR)

xgb_spec <-
  boost_tree(
    trees = tune(),
    min_n = tune(),
    mtry = tune(),
    learn_rate = 0.01
  ) %>%
  set_engine("xgboost") %>%
  set_mode("classification")

xgb_wf <- workflow(rec_obj, xgb_spec)

library(finetune)
doParallel::registerDoParallel()

set.seed(345)
xgb_rs <- tune_race_anova(
  xgb_wf,
  resamples = cv_train,
  grid = 200,
  metrics = metric_set(roc_auc,f_meas,sens,bal_accuracy),
  control = control_race(verbose_elim = TRUE)
)

xgb_rs %>%
  collect_metrics()

xgb_best <- 
  xgb_rs %>% 
  select_best(metric = "f_meas")

autoplot(xgb_rs)

final_xgb <- finalize_model(
  xgb_spec,
  xgb_best
)

final_xgb

mod_HU_1YR=final_xgb %>%
  set_engine("xgboost") %>%
  fit(HU_1YR ~ .,
      data = train_preped
  ) 

mod_HU_1YR%>% predict(signu, type="prob")%>%select(.pred_1)%>% round(.,4)*100
mod_HU_1YR%>% predict(sign, type="prob")%>%select(.pred_1)%>% round(.,4)*100
mod_HU_1YR%>% predict(signl, type="prob")%>%select(.pred_1)%>% round(.,4)*100

butcher::weigh(mod_HU_1YR)
lobstr::obj_size(mod_HU_1YR)

cleaned_mod_HU_1YR <- butcher::axe_fitted(mod_HU_1YR, verbose = TRUE)
butcher::weigh(cleaned_mod_HU_1YR)
lobstr::obj_size(cleaned_mod_HU_1YR)

cleaned_mod_HU_1YR%>% predict(signu, type="prob")%>%select(.pred_1)%>% round(.,4)*100
cleaned_mod_HU_1YR%>% predict(sign, type="prob")%>%select(.pred_1)%>% round(.,4)*100
cleaned_mod_HU_1YR%>% predict(signl, type="prob")%>%select(.pred_1)%>% round(.,4)*100

###################################Model_HU_5YR###################################
Out_Comes
out="HU_5YR"
df=Data_merged%>%
  select(c(out,preds))
df$HU_5YR=as.factor(df$HU_5YR)

df$BLEVEREOD[is.na(df$BLEVEREOD)]=df$od1201[is.na(df$BLEVEREOD)]
df <- recipe( ~ ., data = df) %>%
  step_upsample(HU_5YR) %>%
  prep(training = df) %>% bake(new_data = NULL)
df$BLEVEREOD[is.na(df$BLEVEREOD)]=0
df=df%>% 
  drop_na()


table(df$HU_5YR, df$BLEVEREOD)

# df$BLEVEREOD[df$HU_5YR==1]=1*(rnorm(sum(df$HU_5YR==1))<.7)
# df$HU_5YR[df$BLEVEREOD==1]=1*(rnorm(sum(df$BLEVEREOD==1))<.7)
# table(df$HU_5YR, df$BLEVEREOD)
# 
# 
# table(df$HU_5YR, df$od1201)
# 
# df$od1201[df$HU_5YR==1]=1*(rnorm(sum(df$HU_5YR==1))<.7)
# df$HU_5YR[df$od1201==1]=1*(rnorm(sum(df$od1201==1))<.7)
# table(df$HU_5YR, df$od1201)


cv_train <- vfold_cv(df, v = 10, repeats = 5, strata = out)

rec_obj <- recipe(HU_5YR ~ ., data = df)
standardized <- rec_obj %>%
  themis::step_smote (HU_5YR)

train_preped <- prep(standardized) %>%
  bake(new_data = NULL)

table(df$HU_5YR)

xgb_spec <-
  boost_tree(
    trees = tune(),
    min_n = tune(),
    mtry = tune(),
    learn_rate = 0.01
  ) %>%
  set_engine("xgboost") %>%
  set_mode("classification")

xgb_wf <- workflow(rec_obj, xgb_spec)

library(finetune)
doParallel::registerDoParallel()

set.seed(345)
xgb_rs <- tune_race_anova(
  xgb_wf,
  resamples = cv_train,
  grid = 200,
  metrics = metric_set(roc_auc,f_meas,sens,bal_accuracy),
  control = control_race(verbose_elim = TRUE)
)

xgb_rs %>%
  collect_metrics()

xgb_best <- 
  xgb_rs %>% 
  select_best(metric = "f_meas")

autoplot(xgb_rs)

final_xgb <- finalize_model(
  xgb_spec,
  xgb_best
)

final_xgb

mod_HU_5YR=final_xgb %>%
  set_engine("xgboost") %>%
  fit(HU_5YR ~ .,
      data = train_preped
  ) 

mod_HU_5YR%>% predict(signu, type="prob")%>%select(.pred_1)%>% round(.,4)*100
mod_HU_5YR%>% predict(sign, type="prob")%>%select(.pred_1)%>% round(.,4)*100
mod_HU_5YR%>% predict(signl, type="prob")%>%select(.pred_1)%>% round(.,4)*100

butcher::weigh(mod_HU_5YR)
lobstr::obj_size(mod_HU_5YR)

cleaned_mod_HU_5YR <- butcher::axe_fitted(mod_HU_5YR, verbose = TRUE)
butcher::weigh(cleaned_mod_HU_5YR)
lobstr::obj_size(cleaned_mod_HU_5YR)

cleaned_mod_HU_5YR%>% predict(signu, type="prob")%>%select(.pred_1)%>% round(.,4)*100
cleaned_mod_HU_5YR%>% predict(sign, type="prob")%>%select(.pred_1)%>% round(.,4)*100
cleaned_mod_HU_5YR%>% predict(signl, type="prob")%>%select(.pred_1)%>% round(.,4)*100
###################################Model_HU_10YR###################################
Out_Comes
out="HU_10YR"
df=Data_merged%>%
  select(c(out,preds))
df$HU_10YR=as.factor(df$HU_10YR)

df$BLEVEREOD[is.na(df$BLEVEREOD)]=df$od1201[is.na(df$BLEVEREOD)]
df <- recipe( ~ ., data = df) %>%
  step_upsample(HU_10YR) %>%
  prep(training = df) %>% bake(new_data = NULL)
df$BLEVEREOD[is.na(df$BLEVEREOD)]=0
df=df%>% 
  drop_na()


table(df$HU_10YR, df$BLEVEREOD)

# df$BLEVEREOD[df$HU_10YR==1]=1*(rnorm(sum(df$HU_10YR==1))<.7)
# df$HU_10YR[df$BLEVEREOD==1]=1*(rnorm(sum(df$BLEVEREOD==1))<.7)
# table(df$HU_10YR, df$BLEVEREOD)
# 
# 
# table(df$HU_10YR, df$od1201)
# 
# df$od1201[df$HU_10YR==1]=1*(rnorm(sum(df$HU_10YR==1))<.7)
# df$HU_10YR[df$od1201==1]=1*(rnorm(sum(df$od1201==1))<.7)
# table(df$HU_10YR, df$od1201)


cv_train <- vfold_cv(df, v = 10, repeats = 5, strata = out)

rec_obj <- recipe(HU_10YR ~ ., data = df)
standardized <- rec_obj %>%
  themis::step_smote (HU_10YR)

train_preped <- prep(standardized) %>%
  bake(new_data = NULL)

table(df$HU_10YR)

xgb_spec <-
  boost_tree(
    trees = tune(),
    min_n = tune(),
    mtry = tune(),
    learn_rate = 0.01
  ) %>%
  set_engine("xgboost") %>%
  set_mode("classification")

xgb_wf <- workflow(rec_obj, xgb_spec)

library(finetune)
doParallel::registerDoParallel()

set.seed(345)
xgb_rs <- tune_race_anova(
  xgb_wf,
  resamples = cv_train,
  grid = 200,
  metrics = metric_set(roc_auc,f_meas,sens,bal_accuracy),
  control = control_race(verbose_elim = TRUE)
)

xgb_rs %>%
  collect_metrics()

xgb_best <- 
  xgb_rs %>% 
  select_best(metric = "f_meas")

autoplot(xgb_rs)

final_xgb <- finalize_model(
  xgb_spec,
  xgb_best
)

final_xgb

mod_HU_10YR=final_xgb %>%
  set_engine("xgboost") %>%
  fit(HU_10YR ~ .,
      data = train_preped
  ) 

mod_HU_10YR%>% predict(signu, type="prob")%>%select(.pred_1)%>% round(.,4)*100
mod_HU_10YR%>% predict(sign, type="prob")%>%select(.pred_1)%>% round(.,4)*100
mod_HU_10YR%>% predict(signl, type="prob")%>%select(.pred_1)%>% round(.,4)*100

butcher::weigh(mod_HU_10YR)
lobstr::obj_size(mod_HU_10YR)

cleaned_mod_HU_10YR <- butcher::axe_fitted(mod_HU_10YR, verbose = TRUE)
butcher::weigh(cleaned_mod_HU_10YR)
lobstr::obj_size(cleaned_mod_HU_10YR)

cleaned_mod_HU_10YR%>% predict(signu, type="prob")%>%select(.pred_1)%>% round(.,4)*100
cleaned_mod_HU_10YR%>% predict(sign, type="prob")%>%select(.pred_1)%>% round(.,4)*100
cleaned_mod_HU_10YR%>% predict(signl, type="prob")%>%select(.pred_1)%>% round(.,4)*100


###################################Model_LTA_10yr###################################
Out_Comes
out="LTA_10yr"
df=Data_merged%>%
  select(c(out,preds))
df$LTA_10yr=as.factor(df$LTA_10yr)

df$BLEVEREOD[is.na(df$BLEVEREOD)]=df$od1201[is.na(df$BLEVEREOD)]
df <- recipe( ~ ., data = df) %>%
  step_upsample(LTA_10yr) %>%
  prep(training = df) %>% bake(new_data = NULL)
df$BLEVEREOD[is.na(df$BLEVEREOD)]=0
df=df%>% 
  drop_na()


table(df$LTA_10yr, df$BLEVEREOD)

# df$BLEVEREOD[df$LTA_10yr==1]=1*(rnorm(sum(df$LTA_10yr==1))<.7)
# df$LTA_10yr[df$BLEVEREOD==1]=1*(rnorm(sum(df$BLEVEREOD==1))<.7)
# table(df$LTA_10yr, df$BLEVEREOD)
# 
# 
# table(df$LTA_10yr, df$od1201)
# 
# df$od1201[df$LTA_10yr==1]=1*(rnorm(sum(df$LTA_10yr==1))<.7)
# df$LTA_10yr[df$od1201==1]=1*(rnorm(sum(df$od1201==1))<.7)
# table(df$LTA_10yr, df$od1201)


cv_train <- vfold_cv(df, v = 10, repeats = 5, strata = out)

rec_obj <- recipe(LTA_10yr ~ ., data = df)
standardized <- rec_obj %>%
  themis::step_smote (LTA_10yr)

train_preped <- prep(standardized) %>%
  bake(new_data = NULL)

table(df$LTA_10yr)

xgb_spec <-
  boost_tree(
    trees = tune(),
    min_n = tune(),
    mtry = tune(),
    learn_rate = 0.01
  ) %>%
  set_engine("xgboost") %>%
  set_mode("classification")

xgb_wf <- workflow(rec_obj, xgb_spec)

library(finetune)
doParallel::registerDoParallel()

set.seed(345)
xgb_rs <- tune_race_anova(
  xgb_wf,
  resamples = cv_train,
  grid = 200,
  metrics = metric_set(roc_auc,f_meas,sens,bal_accuracy),
  control = control_race(verbose_elim = TRUE)
)

xgb_rs %>%
  collect_metrics()

xgb_best <- 
  xgb_rs %>% 
  select_best(metric = "f_meas")

autoplot(xgb_rs)

final_xgb <- finalize_model(
  xgb_spec,
  xgb_best
)

final_xgb

mod_LTA_10yr=final_xgb %>%
  set_engine("xgboost") %>%
  fit(LTA_10yr ~ .,
      data = train_preped
  ) 

mod_LTA_10yr%>% predict(signu, type="prob")%>%select(.pred_1)%>% round(.,4)*100
mod_LTA_10yr%>% predict(sign, type="prob")%>%select(.pred_1)%>% round(.,4)*100
mod_LTA_10yr%>% predict(signl, type="prob")%>%select(.pred_1)%>% round(.,4)*100

butcher::weigh(mod_LTA_10yr)
lobstr::obj_size(mod_LTA_10yr)

cleaned_mod_LTA_10yr <- butcher::axe_fitted(mod_LTA_10yr, verbose = TRUE)
butcher::weigh(cleaned_mod_LTA_10yr)
lobstr::obj_size(cleaned_mod_LTA_10yr)

cleaned_mod_LTA_10yr%>% predict(signu, type="prob")%>%select(.pred_1)%>% round(.,4)*100
cleaned_mod_LTA_10yr%>% predict(sign, type="prob")%>%select(.pred_1)%>% round(.,4)*100
cleaned_mod_LTA_10yr%>% predict(signl, type="prob")%>%select(.pred_1)%>% round(.,4)*100

###################################Model_MTA_5YR###################################
Out_Comes
out="MTA_5YR"
df=Data_merged%>%
  select(c(out,preds))
df$MTA_5YR=as.factor(df$MTA_5YR)

df$BLEVEREOD[is.na(df$BLEVEREOD)]=df$od1201[is.na(df$BLEVEREOD)]
df <- recipe( ~ ., data = df) %>%
  step_upsample(MTA_5YR) %>%
  prep(training = df) %>% bake(new_data = NULL)
df$BLEVEREOD[is.na(df$BLEVEREOD)]=0
df=df%>% 
  drop_na()


table(df$MTA_5YR, df$BLEVEREOD)

# df$BLEVEREOD[df$MTA_5YR==1]=1*(rnorm(sum(df$MTA_5YR==1))<.7)
# df$MTA_5YR[df$BLEVEREOD==1]=1*(rnorm(sum(df$BLEVEREOD==1))<.7)
# table(df$MTA_5YR, df$BLEVEREOD)
# 
# 
# table(df$MTA_5YR, df$od1201)
# 
# df$od1201[df$MTA_5YR==1]=1*(rnorm(sum(df$MTA_5YR==1))<.7)
# df$MTA_5YR[df$od1201==1]=1*(rnorm(sum(df$od1201==1))<.7)
# table(df$MTA_5YR, df$od1201)


cv_train <- vfold_cv(df, v = 10, repeats = 5, strata = out)

rec_obj <- recipe(MTA_5YR ~ ., data = df)
MTAndardized <- rec_obj %>%
  themis::step_smote (MTA_5YR)

train_preped <- prep(MTAndardized) %>%
  bake(new_data = NULL)

table(df$MTA_5YR)

xgb_spec <-
  boost_tree(
    trees = tune(),
    min_n = tune(),
    mtry = tune(),
    learn_rate = 0.01
  ) %>%
  set_engine("xgboost") %>%
  set_mode("classification")

xgb_wf <- workflow(rec_obj, xgb_spec)

library(finetune)
doParallel::registerDoParallel()

set.seed(345)
xgb_rs <- tune_race_anova(
  xgb_wf,
  resamples = cv_train,
  grid = 200,
  metrics = metric_set(roc_auc,f_meas,sens,bal_accuracy),
  control = control_race(verbose_elim = TRUE)
)

xgb_rs %>%
  collect_metrics()

xgb_best <- 
  xgb_rs %>% 
  select_best(metric = "f_meas")

autoplot(xgb_rs)

final_xgb <- finalize_model(
  xgb_spec,
  xgb_best
)

final_xgb

mod_MTA_5YR=final_xgb %>%
  set_engine("xgboost") %>%
  fit(MTA_5YR ~ .,
      data = train_preped
  ) 

mod_MTA_5YR%>% predict(signu, type="prob")%>%select(.pred_1)%>% round(.,4)*100
mod_MTA_5YR%>% predict(sign, type="prob")%>%select(.pred_1)%>% round(.,4)*100
mod_MTA_5YR%>% predict(signl, type="prob")%>%select(.pred_1)%>% round(.,4)*100

butcher::weigh(mod_MTA_5YR)
lobstr::obj_size(mod_MTA_5YR)

cleaned_mod_MTA_5YR <- butcher::axe_fitted(mod_MTA_5YR, verbose = TRUE)
butcher::weigh(cleaned_mod_MTA_5YR)
lobstr::obj_size(cleaned_mod_MTA_5YR)

cleaned_mod_MTA_5YR%>% predict(signu, type="prob")%>%select(.pred_1)%>% round(.,4)*100
cleaned_mod_MTA_5YR%>% predict(sign, type="prob")%>%select(.pred_1)%>% round(.,4)*100
cleaned_mod_MTA_5YR%>% predict(signl, type="prob")%>%select(.pred_1)%>% round(.,4)*100
###################################Model_MTA_10YR###################################
Out_Comes
out="MTA_10YR"
df=Data_merged%>%
  select(c(out,preds))
df$MTA_10YR=as.factor(df$MTA_10YR)

df$BLEVEREOD[is.na(df$BLEVEREOD)]=df$od1201[is.na(df$BLEVEREOD)]
df <- recipe( ~ ., data = df) %>%
  step_upsample(MTA_10YR) %>%
  prep(training = df) %>% bake(new_data = NULL)
df$BLEVEREOD[is.na(df$BLEVEREOD)]=0
df=df%>% 
  drop_na()


table(df$MTA_10YR, df$BLEVEREOD)

# df$BLEVEREOD[df$MTA_10YR==1]=1*(rnorm(sum(df$MTA_10YR==1))<.7)
# df$MTA_10YR[df$BLEVEREOD==1]=1*(rnorm(sum(df$BLEVEREOD==1))<.7)
# table(df$MTA_10YR, df$BLEVEREOD)
# 
# 
# table(df$MTA_10YR, df$od1201)
# 
# df$od1201[df$MTA_10YR==1]=1*(rnorm(sum(df$MTA_10YR==1))<.7)
# df$MTA_10YR[df$od1201==1]=1*(rnorm(sum(df$od1201==1))<.7)
# table(df$MTA_10YR, df$od1201)


cv_train <- vfold_cv(df, v = 10, repeats = 5, strata = out)

rec_obj <- recipe(MTA_10YR ~ ., data = df)
MTAndardized <- rec_obj %>%
  themis::step_smote (MTA_10YR)

train_preped <- prep(MTAndardized) %>%
  bake(new_data = NULL)

table(df$MTA_10YR)

xgb_spec <-
  boost_tree(
    trees = tune(),
    min_n = tune(),
    mtry = tune(),
    learn_rate = 0.01
  ) %>%
  set_engine("xgboost") %>%
  set_mode("classification")

xgb_wf <- workflow(rec_obj, xgb_spec)

library(finetune)
doParallel::registerDoParallel()

set.seed(345)
xgb_rs <- tune_race_anova(
  xgb_wf,
  resamples = cv_train,
  grid = 200,
  metrics = metric_set(roc_auc,f_meas,sens,bal_accuracy),
  control = control_race(verbose_elim = TRUE)
)

xgb_rs %>%
  collect_metrics()

xgb_best <- 
  xgb_rs %>% 
  select_best(metric = "f_meas")

autoplot(xgb_rs)

final_xgb <- finalize_model(
  xgb_spec,
  xgb_best
)

final_xgb

mod_MTA_10YR=final_xgb %>%
  set_engine("xgboost") %>%
  fit(MTA_10YR ~ .,
      data = train_preped
  ) 

mod_MTA_10YR%>% predict(signu, type="prob")%>%select(.pred_1)%>% round(.,4)*100
mod_MTA_10YR%>% predict(sign, type="prob")%>%select(.pred_1)%>% round(.,4)*100
mod_MTA_10YR%>% predict(signl, type="prob")%>%select(.pred_1)%>% round(.,4)*100

butcher::weigh(mod_MTA_10YR)
lobstr::obj_size(mod_MTA_10YR)

cleaned_mod_MTA_10YR <- butcher::axe_fitted(mod_MTA_10YR, verbose = TRUE)
butcher::weigh(cleaned_mod_MTA_10YR)
lobstr::obj_size(cleaned_mod_MTA_10YR)

cleaned_mod_MTA_10YR%>% predict(signu, type="prob")%>%select(.pred_1)%>% round(.,4)*100
cleaned_mod_MTA_10YR%>% predict(sign, type="prob")%>%select(.pred_1)%>% round(.,4)*100
cleaned_mod_STA_10YR%>% predict(signl, type="prob")%>%select(.pred_1)%>% round(.,4)*100



###################################Model_STA_1YR###################################
Out_Comes
out="STA_1YR"
df=Data_merged%>%
  select(c(out,preds))
df$STA_1YR=as.factor(df$STA_1YR)

df$BLEVEREOD[is.na(df$BLEVEREOD)]=df$od1201[is.na(df$BLEVEREOD)]
df <- recipe( ~ ., data = df) %>%
  step_upsample(STA_1YR) %>%
  prep(training = df) %>% bake(new_data = NULL)
df$BLEVEREOD[is.na(df$BLEVEREOD)]=0
df=df%>% 
  drop_na()


table(df$STA_1YR, df$BLEVEREOD)

# df$BLEVEREOD[df$STA_1YR==1]=1*(rnorm(sum(df$STA_1YR==1))<.7)
# df$STA_1YR[df$BLEVEREOD==1]=1*(rnorm(sum(df$BLEVEREOD==1))<.7)
# table(df$STA_1YR, df$BLEVEREOD)
# 
# 
# table(df$STA_1YR, df$od1201)
# 
# df$od1201[df$STA_1YR==1]=1*(rnorm(sum(df$STA_1YR==1))<.7)
# df$STA_1YR[df$od1201==1]=1*(rnorm(sum(df$od1201==1))<.7)
# table(df$STA_1YR, df$od1201)


cv_train <- vfold_cv(df, v = 10, repeats = 5, strata = out)

rec_obj <- recipe(STA_1YR ~ ., data = df)
standardized <- rec_obj %>%
  themis::step_smote (STA_1YR)

train_preped <- prep(standardized) %>%
  bake(new_data = NULL)

table(df$STA_1YR)

xgb_spec <-
  boost_tree(
    trees = tune(),
    min_n = tune(),
    mtry = tune(),
    learn_rate = 0.01
  ) %>%
  set_engine("xgboost") %>%
  set_mode("classification")

xgb_wf <- workflow(rec_obj, xgb_spec)

library(finetune)
doParallel::registerDoParallel()

set.seed(345)
xgb_rs <- tune_race_anova(
  xgb_wf,
  resamples = cv_train,
  grid = 200,
  metrics = metric_set(roc_auc,f_meas,sens,bal_accuracy),
  control = control_race(verbose_elim = TRUE)
)

xgb_rs %>%
  collect_metrics()

xgb_best <- 
  xgb_rs %>% 
  select_best(metric = "f_meas")

autoplot(xgb_rs)

final_xgb <- finalize_model(
  xgb_spec,
  xgb_best
)

final_xgb

mod_STA_1YR=final_xgb %>%
  set_engine("xgboost") %>%
  fit(STA_1YR ~ .,
      data = train_preped
  ) 

mod_STA_1YR%>% predict(signu, type="prob")%>%select(.pred_1)%>% round(.,4)*100
mod_STA_1YR%>% predict(sign, type="prob")%>%select(.pred_1)%>% round(.,4)*100
mod_STA_1YR%>% predict(signl, type="prob")%>%select(.pred_1)%>% round(.,4)*100

butcher::weigh(mod_STA_1YR)
lobstr::obj_size(mod_STA_1YR)

cleaned_mod_STA_1YR <- butcher::axe_fitted(mod_STA_1YR, verbose = TRUE)
butcher::weigh(cleaned_mod_STA_1YR)
lobstr::obj_size(cleaned_mod_STA_1YR)

cleaned_mod_STA_1YR%>% predict(signu, type="prob")%>%select(.pred_1)%>% round(.,4)*100
cleaned_mod_STA_1YR%>% predict(sign, type="prob")%>%select(.pred_1)%>% round(.,4)*100
cleaned_mod_STA_1YR%>% predict(signl, type="prob")%>%select(.pred_1)%>% round(.,4)*100

###################################Model_STA_5YR###################################
Out_Comes
out="STA_5YR"
df=Data_merged%>%
  select(c(out,preds))
df$STA_5YR=as.factor(df$STA_5YR)

df$BLEVEREOD[is.na(df$BLEVEREOD)]=df$od1201[is.na(df$BLEVEREOD)]
df <- recipe( ~ ., data = df) %>%
  step_upsample(STA_5YR) %>%
  prep(training = df) %>% bake(new_data = NULL)
df$BLEVEREOD[is.na(df$BLEVEREOD)]=0
df=df%>% 
  drop_na()


table(df$STA_5YR, df$BLEVEREOD)

# df$BLEVEREOD[df$STA_5YR==1]=1*(rnorm(sum(df$STA_5YR==1))<.7)
# df$STA_5YR[df$BLEVEREOD==1]=1*(rnorm(sum(df$BLEVEREOD==1))<.7)
# table(df$STA_5YR, df$BLEVEREOD)
# 
# 
# table(df$STA_5YR, df$od1201)
# 
# df$od1201[df$STA_5YR==1]=1*(rnorm(sum(df$STA_5YR==1))<.7)
# df$STA_5YR[df$od1201==1]=1*(rnorm(sum(df$od1201==1))<.7)
# table(df$STA_5YR, df$od1201)


cv_train <- vfold_cv(df, v = 10, repeats = 5, strata = out)

rec_obj <- recipe(STA_5YR ~ ., data = df)
standardized <- rec_obj %>%
  themis::step_smote (STA_5YR)

train_preped <- prep(standardized) %>%
  bake(new_data = NULL)

table(df$STA_5YR)

xgb_spec <-
  boost_tree(
    trees = tune(),
    min_n = tune(),
    mtry = tune(),
    learn_rate = 0.01
  ) %>%
  set_engine("xgboost") %>%
  set_mode("classification")

xgb_wf <- workflow(rec_obj, xgb_spec)

library(finetune)
doParallel::registerDoParallel()

set.seed(345)
xgb_rs <- tune_race_anova(
  xgb_wf,
  resamples = cv_train,
  grid = 200,
  metrics = metric_set(roc_auc,f_meas,sens,bal_accuracy),
  control = control_race(verbose_elim = TRUE)
)

xgb_rs %>%
  collect_metrics()

xgb_best <- 
  xgb_rs %>% 
  select_best(metric = "f_meas")

autoplot(xgb_rs)

final_xgb <- finalize_model(
  xgb_spec,
  xgb_best
)

final_xgb

mod_STA_5YR=final_xgb %>%
  set_engine("xgboost") %>%
  fit(STA_5YR ~ .,
      data = train_preped
  ) 

mod_STA_5YR%>% predict(signu, type="prob")%>%select(.pred_1)%>% round(.,4)*100
mod_STA_5YR%>% predict(sign, type="prob")%>%select(.pred_1)%>% round(.,4)*100
mod_STA_5YR%>% predict(signl, type="prob")%>%select(.pred_1)%>% round(.,4)*100

butcher::weigh(mod_STA_5YR)
lobstr::obj_size(mod_STA_5YR)

cleaned_mod_STA_5YR <- butcher::axe_fitted(mod_STA_5YR, verbose = TRUE)
butcher::weigh(cleaned_mod_STA_5YR)
lobstr::obj_size(cleaned_mod_STA_5YR)

cleaned_mod_STA_5YR%>% predict(signu, type="prob")%>%select(.pred_1)%>% round(.,4)*100
cleaned_mod_STA_5YR%>% predict(sign, type="prob")%>%select(.pred_1)%>% round(.,4)*100
cleaned_mod_STA_5YR%>% predict(signl, type="prob")%>%select(.pred_1)%>% round(.,4)*100
###################################Model_STA_10YR###################################
Out_Comes
out="STA_10YR"
df=Data_merged%>%
  select(c(out,preds))
df$STA_10YR=as.factor(df$STA_10YR)

df$BLEVEREOD[is.na(df$BLEVEREOD)]=df$od1201[is.na(df$BLEVEREOD)]
df <- recipe( ~ ., data = df) %>%
  step_upsample(STA_10YR) %>%
  prep(training = df) %>% bake(new_data = NULL)
df$BLEVEREOD[is.na(df$BLEVEREOD)]=0
df=df%>% 
  drop_na()


table(df$STA_10YR, df$BLEVEREOD)

# df$BLEVEREOD[df$STA_10YR==1]=1*(rnorm(sum(df$STA_10YR==1))<.7)
# df$STA_10YR[df$BLEVEREOD==1]=1*(rnorm(sum(df$BLEVEREOD==1))<.7)
# table(df$STA_10YR, df$BLEVEREOD)
# 
# 
# table(df$STA_10YR, df$od1201)
# 
# df$od1201[df$STA_10YR==1]=1*(rnorm(sum(df$STA_10YR==1))<.7)
# df$STA_10YR[df$od1201==1]=1*(rnorm(sum(df$od1201==1))<.7)
# table(df$STA_10YR, df$od1201)


cv_train <- vfold_cv(df, v = 10, repeats = 5, strata = out)

rec_obj <- recipe(STA_10YR ~ ., data = df)
standardized <- rec_obj %>%
  themis::step_smote (STA_10YR)

train_preped <- prep(standardized) %>%
  bake(new_data = NULL)

table(df$STA_10YR)

xgb_spec <-
  boost_tree(
    trees = tune(),
    min_n = tune(),
    mtry = tune(),
    learn_rate = 0.01
  ) %>%
  set_engine("xgboost") %>%
  set_mode("classification")

xgb_wf <- workflow(rec_obj, xgb_spec)

library(finetune)
doParallel::registerDoParallel()

set.seed(345)
xgb_rs <- tune_race_anova(
  xgb_wf,
  resamples = cv_train,
  grid = 200,
  metrics = metric_set(roc_auc,f_meas,sens,bal_accuracy),
  control = control_race(verbose_elim = TRUE)
)

xgb_rs %>%
  collect_metrics()

xgb_best <- 
  xgb_rs %>% 
  select_best(metric = "f_meas")

autoplot(xgb_rs)

final_xgb <- finalize_model(
  xgb_spec,
  xgb_best
)

final_xgb

mod_STA_10YR=final_xgb %>%
  set_engine("xgboost") %>%
  fit(STA_10YR ~ .,
      data = train_preped
  ) 

mod_STA_10YR%>% predict(signu, type="prob")%>%select(.pred_1)%>% round(.,4)*100
mod_STA_10YR%>% predict(sign, type="prob")%>%select(.pred_1)%>% round(.,4)*100
mod_STA_10YR%>% predict(signl, type="prob")%>%select(.pred_1)%>% round(.,4)*100

butcher::weigh(mod_STA_10YR)
lobstr::obj_size(mod_STA_10YR)

cleaned_mod_STA_10YR <- butcher::axe_fitted(mod_STA_10YR, verbose = TRUE)
butcher::weigh(cleaned_mod_STA_10YR)
lobstr::obj_size(cleaned_mod_STA_10YR)

cleaned_mod_STA_10YR%>% predict(signu, type="prob")%>%select(.pred_1)%>% round(.,4)*100
cleaned_mod_STA_10YR%>% predict(sign, type="prob")%>%select(.pred_1)%>% round(.,4)*100
cleaned_mod_STA_10YR%>% predict(signl, type="prob")%>%select(.pred_1)%>% round(.,4)*100



