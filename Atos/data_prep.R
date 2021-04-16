#############################################OLDPREP_IGNOR############################################################

#data_Atos <- read_sav("HU OD 1 5 10y model.sav")
#View(data_Atos)
colnames( data_Atos)
Out_Comes=c("STA_1YR","STA_5YR","MTA_5YR","STA_10YR","MTA_10YR","LTA_10yr","HU_1YR","HU_5YR","HU_10YR","OD_upto1YR","OD_upto5YR","OD_upto10YR","DEATH_1YR","DEATH_5YR","DEATH_10YR","DEATH_15YR")

Out_Comes[!(Out_Comes%in%colnames( data_Atos))]
Out_Comes[(Out_Comes%in%colnames( data_Atos))]

preds
#colnames(data_Atos)%in%preds

#DATA$id0101=as.numeric(as.character(DATA$id0101))
#data_Atos$id0101=as.numeric(as.character(data_Atos$id0101))
#merge the data 
Data_merged=DATA %>% inner_join(data_Atos, by = "id0101")


#check the pred input with new Excel file
##good for now but have to check later!
#################################################OLDPREP_IGNOR########################################################

#data_Atos <- read_sav("HU OD 1 5 10y model.sav")
#View(data_Atos)

Out_Comes=c("STA_1YR","STA_5YR","MTA_5YR","STA_10YR","MTA_10YR","LTA_10yr","HU_1YR","HU_5YR","HU_10YR","OD_upto1YR","OD_upto5YR","OD_upto10YR","DEATH_1YR","DEATH_5YR","DEATH_10YR","DEATH_15YR")
#colnames(data_Atos)%in%preds

#DATA$id0101=as.numeric(as.character(DATA$id0101))
#data_Atos$id0101=as.numeric(as.character(data_Atos$id0101))
#merge the data 
Data_merged=DATA %>% inner_join(data_Atos, by = "id0101")
######################################################OLDPREP_IGNOR###################################################
#data_Atos <- read_sav("HU OD 1 5 10y model.sav")
#View(data_Atos)

Out_Comes=c("STA_1YR","STA_5YR","MTA_5YR","STA_10YR","MTA_10YR","LTA_10yr","HU_1YR","HU_5YR","HU_10YR","OD_upto1YR","OD_upto5YR","OD_upto10YR","DEATH_1YR","DEATH_5YR","DEATH_10YR","DEATH_15YR")
#colnames(data_Atos)%in%preds

#DATA$id0101=as.numeric(as.character(DATA$id0101))
#data_Atos$id0101=as.numeric(as.character(data_Atos$id0101))
#merge the data 
Data_merged=DATA %>% inner_join(data_Atos, by = "id0101")


##################################################NEW_PREP_April_2021#######################################################
load("~/OneDrive - Universite de Montreal/Usydney/data.RData")
library(haven)
outcomes_data <- read_sav("~/OneDrive - Universite de Montreal/Usydney/Recoded outcome variables for Kamran.sav")

new_predfile <- read_sav("~/Downloads/Modelling datafile_v1_1 copy.sav")

colnames(new_predfile)

dummy=as.data.frame(psych::dummy.code (new_predfile$id0104))
colnames(dummy)=c("T1","T2","T3","T4")
new_predfile=as.data.frame(cbind(new_predfile,dummy))
new_predfile=new_predfile[,!(names(new_predfile) %in% "id0104")]

preds2=preds[! preds%in% c("irhprobs1redo","inhal_1","dg0117a","shared_1")]

c("MSIGOV1","SEXUAL_TRAUMA","dp01j59","dp01j59a" ,"INTERPSNL_TRAUMA" ,"h0101b","BLEVEREOD","od1201","polysubs_1" ,"prev_tmt")

preds2=append(preds2,c("MSIGOV1","SEXUAL_TRAUMA","dp01j59","dp01j59a" ,"INTERPSNL_TRAUMA" ,"h0101b","BLEVEREOD","od1201","polysubs_1" ,"prev_tmt"))

overlap=colnames(new_predfile)[colnames(new_predfile)%in%preds2]

preds2[!preds2%in%overlap]

pred_data=new_predfile[colnames(new_predfile)%in%c("id0101",preds2)]

colnames(pred_data)
colnames(outcomes_data)



pred_data$id0101=as.numeric(as.character(pred_data$id0101))
outcomes_data$id0101=as.numeric(as.character(outcomes_data$id0101))
#merge the data 
Data_merged=pred_data %>% inner_join(outcomes_data, by = "id0101")


rm(list=setdiff(ls(), c("Data_merged","Out_Comes","preds2")))
preds=preds2

t(matrix(preds,1)) 
rm(preds2)
save.image("~/OneDrive - Universite de Montreal/Usydney/data_04_2021.RData")


apply(Data_merged , 2, function(x){sum(is.na(x))})


Data_merged$unstablehousingBL
Data_merged$dp01j59a
