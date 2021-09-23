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

###################################Model###################################
out="DEATH_10YR"
df=Data_merged%>%
  select(c(out,preds))
df$DEATH_10YR=as.factor(df$DEATH_10YR)

df$BLEVEREOD[is.na(df$BLEVEREOD)]=df$od1201[is.na(df$BLEVEREOD)]
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
  grid = 100,
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

mod1=final_xgb %>%
  set_engine("xgboost") %>%
  fit(DEATH_10YR ~ .,
      data = train_preped
  ) 

mod1%>% predict(signu, type="prob")%>%select(.pred_1)%>% round(.,4)*100

butcher::weigh(mod1)
lobstr::obj_size(mod1)

cleaned_mod1b <- butcher::axe_fitted(mod1, verbose = TRUE)
butcher::weigh(cleaned_mod1b)
lobstr::obj_size(cleaned_mod1b)


cleaned_mod1b%>% predict(signu, type="prob")%>%select(.pred_1)%>% round(.,4)*100
