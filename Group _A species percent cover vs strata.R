rm(list=ls())

######################### Strata Vs Biodiversity #####################3333
Bio <-read.csv("~/GitHub/Dal-Intertidal-2014/Intertidal_Master_Data_Sheet_2014.csv") 
require(plyr)

Bio[is.na(Bio)]=0

 ### High Tide ###
table_mean <- ddply(Bio,.(Strata),summarize,
               #QUADRAT SAMPLES#
               Fucus_spiralis_percentcover=mean(pc_Fucus_spiralis,na.rm = TRUE),
               Fucus_distichous_drift_percentcover=mean(pc_Fucus_distichous_drift,na.rm = TRUE),
               Fucus_distichous_attached_percentcover=mean(pc_Fucus_distichous_attached,na.rm = TRUE),
               Fucus_vesiculosus_percentcover=mean(pc_Fucus_vesiculosus,na.rm = TRUE),
               Ulva_.something..._percentcover=mean(pc_Ulva_.something...,na.rm = TRUE)  
              
)
table_sd <- ddply(Bio,.(Strata),summarize,
                    #QUADRAT SAMPLES#
                  Fucus_spiralis_percentcover=sd(pc_Fucus_spiralis,na.rm = TRUE),
                  Fucus_distichous_drift_percentcover=sd(pc_Fucus_distichous_drift,na.rm = TRUE),
                  Fucus_distichous_attached_percentcover=sd(pc_Fucus_distichous_attached,na.rm = TRUE),
                  Fucus_vesiculosus_percentcover=sd(pc_Fucus_vesiculosus,na.rm = TRUE),
                  Ulva_.something..._percentcover=sd(pc_Ulva_.something...,na.rm = TRUE)
)

sp_names=c("Fucus spiralis","Fucus distichus drift","Fucus distichus attached","Fucus vesiculosus","Ulva sp.") 

stdErr <- function(x) {
  x2=x[is.na(x)==F]              # remove the NA's
  sd(x2)/ sqrt(length(x2))       # calculate SE
}

table_stdErr <- ddply(Bio,.(Strata),summarize,
                      #QUADRAT SAMPLES#
                      Fucus_spiralis_percentcover=stdErr(pc_Fucus_spiralis),
                      Fucus_distichous_drift_percentcover=stdErr(pc_Fucus_distichous_drift),
                      Fucus_distichous_attached_percentcover=stdErr(pc_Fucus_distichous_attached),
                      Fucus_vesiculosus_percentcover=stdErr(pc_Fucus_vesiculosus),
                      Ulva_.something..._percentcover=stdErr(pc_Ulva_.something...)                    
)


par(mfrow=c(2,3))
for(sp in names(table_mean[,-1])){
  mp <- barplot(table_mean[c(1,3,2),names(table_mean)==sp], names.arg=c("High","Mid","Low"), main=sp_names[names(table_mean[,-1])==sp],xlab="Tidal Ranges",ylab= "Mean percent cover",ylim=c(0,0.4))
  segments(mp, table_mean[c(1,3,2),names(table_mean)==sp] + table_stdErr[c(1,3,2),names(table_mean)==sp], mp,table_mean[c(1,3,2),names(table_mean)==sp], lwd=2)
  segments(mp - 0.1, table_mean[c(1,3,2),names(table_mean)==sp] + table_stdErr[c(1,3,2),names(table_mean)==sp], mp + 0.1, table_mean[c(1,3,2),names(table_mean)==sp] + table_stdErr[c(1,3,2),names(table_mean)==sp], lwd=2)
}


par(mfrow=c(3,4))