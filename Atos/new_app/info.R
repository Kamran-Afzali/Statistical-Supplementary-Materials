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
library(tidyverse)
library(haven)
ATOS_Modelling <- read_sav("OneDrive - Universite de Montreal/Usydney/ATOS Modelling.sav")
View(ATOS_Modelling)


load("/Users/kamranafzali/OneDrive - Universite de Montreal/Usydney/data_04_2021.RData")
Data_merged$ptsddx
ATOS_Modelling$as01

table(Data_merged$first_inj_cat,Data_merged$DEATH_15YR)

preds=preds[!(preds %in% c("T1","T2","T3"))]

Data_merged=Data_merged[,c(preds,Out_Comes)]

sort(table(Data_merged$h0101b))
Data_merged$h0101b[!(Data_merged$h0101b%in%c(8,12,2,1))]=999
Data_merged$h0101b=as.factor(Data_merged$h0101b)
Data_merged=as_tibble(Data_merged)

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

top10=Data_merged[,colnames(Data_merged) %in% c(preds_t$OVERALL)]
summary(as.numeric(as.character(top10$h0101b)))
sort(table(as.numeric(as.character(as.vector(c(top10$h0101b))))))
 c("Age","Drug used for first high","Age when first got high","Sexual Trauma",
  "Ever Overdosed","Years of school completed","Age when first used heroin","Past month alcohol use", "prison history","treatment")


colnames(top10)

str(Data_merged)
Data_merged=as_tibble(apply(Data_merged, 2, function(x){as.numeric(as.character(x))}))

apply(Data_merged, 2, function(x){max(x,na.rm = T)})

apply (Data_merged[,preds],2, function(x){mean(x,na.rm = T)})
apply (Data_merged[,preds],2, function(x){min(x,na.rm = T)})
apply (Data_merged[,preds],2, function(x){max(x,na.rm = T)})
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

c("T1","SEXUAL_TRAUMA", "dg0112a", "alcohol_1","BLEVEREOD","h0101b", "dg0102",  "h0101a", "h0104","dg0106","first_inj_cat")


#T4 treatment
#Alcohol cannabis alc/can heroin other
#reorganize the data
#write the pre-processer(h0101b)
#EXTRACT THE DATA
#WRITE THE UPDATER FUNCTION



