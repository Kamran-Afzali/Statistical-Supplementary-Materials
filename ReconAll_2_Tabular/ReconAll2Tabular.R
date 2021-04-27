setwd("Main directory with sub directories for each recon-all output")
G=list.dirs(getwd(), recursive=FALSE)
G=paste(G,"/stats", sep="")

corlh=list()
corrh=list()
subcrt=list()
icv=list()
cor_T_data=c()
cor_S_data=c()
subcor_data=c()
for (i in 1:length(G)) {
  

lh=paste(G[i],"/lh.aparc.stats", sep="")
rh=paste(G[i],"/rh.aparc.stats", sep="")
sc=paste(G[i],"/aseg.stats", sep="")

titlesr=paste(c(strsplit(readLines(lh)[60], " "))[[1]][3:12],"_r",sep = "")
corlh[[i]]=read.table(lh, col.names = titlesr)

titlesl=paste(c(strsplit(readLines(rh)[60], " "))[[1]][3:12],"_l",sep = "")
corrh[[i]]=read.table(rh, col.names = titlesl)

titlessc=c(strsplit(readLines(sc)[80], " "))[[1]][4:13]
subcrt[[i]]=read.table(sc, col.names = titlessc)

icv[[i]]=as.numeric(c(strsplit(readLines(sc)[35], " |,"))[[1]][12])

titles=c(titlesr,titlesl)
a=c(corlh[[i]]$ThickAvg,corrh[[i]]$ThickAvg)
b=c(corlh[[i]]$SurfArea,corrh[[i]]$SurfArea)
c=c(subcrt[[i]]$Volume_mm3,icv[[i]])
cor_T_data=rbind(cor_T_data,a)
cor_S_data=rbind(cor_S_data,b)
subcor_data=rbind(subcor_data,c)
colnames(cor_T_data)=c(corlh[[i]]$ThickAvg,corrh[[i]]$ThickAvg)

}
colnames(cor_T_data) =c(paste(corrh[[i]]$StructName_l,"_l",sep = ""),paste(corrh[[i]]$StructName_l,"_r",sep = ""))
colnames(cor_S_data) =c(paste(corrh[[i]]$StructName_l,"_l",sep = ""),paste(corrh[[i]]$StructName_l,"_f",sep = ""))
colnames(subcor_data)=c(as.character(subcrt[[i]]$StructName) ,"ICV")
rownames(cor_T_data)=list.dirs(, recursive=FALSE)
rownames(cor_S_data)=list.dirs(, recursive=FALSE) 
rownames(subcor_data)=list.dirs(, recursive=FALSE)

###Thickness####
cor_T_data=as.data.frame(cor_T_data)
###Surface####
cor_S_data=as.data.frame(cor_S_data)
###Sub-cortical####
subcor_data=as.data.frame(subcor_data)
rm(list=setdiff(ls(), c("cor_T_data","cor_S_data","subcor_data")))

