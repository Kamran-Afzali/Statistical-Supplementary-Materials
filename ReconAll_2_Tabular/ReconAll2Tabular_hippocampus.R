setwd("Main directory with sub directories for each recon-all output")
G=list.dirs(getwd(), recursive=FALSE)
G1=paste(G,"/mri", sep="")

hiplh=list()
hiprh=list()
hip_data=c()

for (i in 1:length(G1)) {
  
  
  lh=paste(G1[i],"/lh.hippoSfVolumes-T1.v10.txt", sep="")
  rh=paste(G1[i],"/rh.hippoSfVolumes-T1.v10.txt", sep="")

  hiplh[[i]]=read.table(lh)
  hiprh[[i]]=read.table(rh)
  a=c(hiplh[[i]]$V2,hiprh[[i]]$V2)
  hip_data=rbind(hip_data,a)
}


colnames(hip_data)=c(paste(hiplh[[i]]$V1,'lh',sep='_'),paste(hiprh[[i]]$V1,'rh',sep='_'))
row.names(hip_data)=list.dirs(, recursive=FALSE)

rm(list=setdiff(ls(), c("hip_data")))
