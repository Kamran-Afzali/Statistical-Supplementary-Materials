
# Combines thickness, surfance, and subcortical summary data from longitudinal data from year 1 to 3
# Credit for code: Kamran Afzali

########## Useful functions

thick_and_surf_names <- function(){
  ex_file.l='/data/neuroventure/processed/smri/freesurfer/V1/NV_001/stats/lh.aparc.stats'
  ex_file.r='/data/neuroventure/processed/smri/freesurfer/V1/NV_001/stats/rh.aparc.stats'

  titlesl=paste(strsplit(readLines(ex_file.l)[60], " ")[[1]][3:12],"_l",sep = "")
  corlh=read.table(ex_file.l, col.names = titlesl)

  titlesr=paste(strsplit(readLines(ex_file.r)[60], " ")[[1]][3:12],"_r",sep = "")
  corrh=read.table(ex_file.r, col.names = titlesr)

  names<-c(paste(corrh$StructName_r,"_r",sep = ""),paste(corlh$StructName_l,"_l",sep = ""))

  return(names)
}

subcortical_names <- function(){

  ex_file.sc="/data/neuroventure/processed/smri/freesurfer/V1/NV_001/stats/aseg.stats"
  subcrt=read.table(ex_file.sc, stringsAsFactors = FALSE)
  names<-subcrt$V5
}

process_file <- function(data.type,stats,col.names=FALSE){

  if (data.type == "cortical") {

    lh=paste(stats,"/lh.aparc.stats", sep="")
    rh=paste(stats,"/rh.aparc.stats", sep="")

    if (any(!file.exists(lh,rh))) {
      return(list(thick=NA,surf=NA))
    }

    titlesr=paste(strsplit(readLines(lh)[60], " ")[[1]][3:12],"_r",sep = "")
    corlh=read.table(lh, col.names = titlesr)

    titlesl=paste(strsplit(readLines(rh)[60], " ")[[1]][3:12],"_l",sep = "")
    corrh=read.table(rh, col.names = titlesl)

    titles=c(titlesr,titlesl)

    # thickness
    a=c(corlh$ThickAvg_r,corrh$ThickAvg)

    # surface
    b=c(corlh$SurfArea,corrh$SurfArea)

    return(list(thick=a,surf=b))

  } else if (data.type == "subcortical") {
    sc=paste(stats,"/aseg.stats", sep="")

    if (any(!file.exists(sc))) {
      return(NA)
    }

    titlessc=c(strsplit(readLines(sc)[77], " "))[[1]][4:13]
    subcrt=read.table(sc, col.names = titlessc)

    icv=as.numeric(c(strsplit(readLines(sc)[32], " |,"))[[1]][12])

    c=c(subcrt$Volume_mm3,icv)

    return(c)

  }

    else {
      print("Enter a proper data type: cortical or subcortical.")
    }

}

######################### Start here

timepoints <- c(1,2,3)
allT<-data.frame()
allS<-data.frame()
allC<-data.frame()

for (tp in timepoints) {
  setwd(paste0("/data/neuroventure/processed/smri/freesurfer/V",tp))
  G=list.files(pattern="NV")
  G.stats=paste(G,"/stats", sep="")

  cor_T_data=c()
  cor_S_data=c()
  subcor_data=c()

  for (i in 1:length(G)) {

    # returns cortical data
    cortical = process_file("cortical",G.stats[i])

    # returns subcort data
    subcortical = process_file("subcortical",G.stats[i])

    cor_T_data=rbind(cor_T_data,cortical$thick)
    cor_S_data=rbind(cor_S_data,cortical$surf)
    subcor_data=rbind(subcor_data,subcortical)
  }


  ###Thickness####
  cor_T_data=as.data.frame(cor_T_data)
  cor_T_data$year=tp
  cor_T_data$subject=G

  ###Surface####
  cor_S_data=as.data.frame(cor_S_data)
  cor_S_data$year=tp
  cor_S_data$subject=G

  ###Sub-cortical####
  subcor_data=as.data.frame(subcor_data)
  subcor_data$year=tp
  subcor_data$subject=G

  allT <- rbind(cor_T_data,allT)
  allS <- rbind(cor_S_data,allS)
  allC <- rbind(subcor_data,allC)

}

colnames(allT) <- c(thick_and_surf_names(),"year","subject")
colnames(allS) <- c(thick_and_surf_names(),"year","subject")
colnames(allC) <- c(subcortical_names(),"icv","year","subject")

# For saving as csv files
# outname.T = "/data/neuroventure/processed/smri/freesurfer/thick.txt"
# outname.S = "/data/neuroventure/processed/smri/freesurfer/surf.txt"
# outname.C = "/data/neuroventure/processed/smri/freesurfer/subc.txt"

#write.csv(x=allT,file=outname.T)
#write.csv(x=allS,file=outname.S)
#write.csv(x=allC,file=outname.C)

rm(list=setdiff(ls(), c("allT","allS","allC")))
