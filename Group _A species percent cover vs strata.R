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
               Acrosiphonia_arcta_percentcover=mean(Bio$pc_Acrosiphonia_arcta,na.rm=T  ),  
               ascophyllum.nodosum_percentcover=mean(Bio$Pc_ascophyllum.nodosum,na.rm=T),  
               Leatherista_difformus_percentcover=mean(Bio$pc_Leatherista_difformus, na.rm=T),
               Nostoc_cyanobacteria_percentcover= mean(Bio$pc_Nostoc_cyanobacteria, na.rm=T),
               Hildenbrandia_spp_percentcover=mean(Bio$pc_Hildenbrandia_spp,na.rm=T)
              
)
table_sd <- ddply(Bio,.(Strata),summarize,
                    #QUADRAT SAMPLES#
                  Fucus_spiralis_percentcover=sd(pc_Fucus_spiralis,na.rm = TRUE),
                  Fucus_distichous_drift_percentcover=sd(pc_Fucus_distichous_drift,na.rm = TRUE),
                  Fucus_distichous_attached_percentcover=sd(pc_Fucus_distichous_attached,na.rm = TRUE),
                  Fucus_vesiculosus_percentcover=sd(pc_Fucus_vesiculosus,na.rm = TRUE),
                  Acrosiphonia_arcta_percentcover=sd(Bio$pc_Acrosiphonia_arcta,na.rm=T  ),  
                  ascophyllum.nodosum_percentcover=sd(Bio$Pc_ascophyllum.nodosum,na.rm=T),  
                  Leatherista_difformus_percentcover=sd(Bio$pc_Leatherista_difformus, na.rm=T),
                  Nostoc_cyanobacteria_percentcover= sd(Bio$pc_Nostoc_cyanobacteria, na.rm=T),
                  Hildenbrandia_spp_percentcover=sd(Bio$pc_Hildenbrandia_spp,na.rm=T)
)

sp_names=c("Fucus spiralis","Fucus \ndistichus (drift)","Fucus \ndistichus (attached)","Fucus vesiculosus", "Acrosiphonia \narcta", "Ascophyllum \nnodosum", "Leatherista \ndifformus", "Nostoc \ncyanobacteria","Hildenbrandia \nspp.") 

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
                      Acrosiphonia_arcta_percentcover=stdErr(Bio$pc_Acrosiphonia_arcta),  
                      ascophyllum.nodosum_percentcover=stdErr(Bio$Pc_ascophyllum.nodosum),  
                      Leatherista_difformus_percentcover=stdErr(Bio$pc_Leatherista_difformus),
                      Nostoc_cyanobacteria_percentcover= stdErr(Bio$pc_Nostoc_cyanobacteria),
                      Hildenbrandia_spp_percentcover=stdErr(Bio$pc_Hildenbrandia_spp)
                      
)

jpeg(paste("Biol vs phys/Percent Cover.jpg"), height=3600, width=2400, res=400, qual=100  )

par(mfrow=c(3,3))

for(sp in names(table_mean[,-1])){
  
  mp <- barplot(table_mean[c(1,3,2),names(table_mean)==sp], names.arg=c("High","Mid","Low"), main=sp_names[names(table_mean[,-1])==sp],xlab="Tidal Ranges",ylab= "Mean percent cover",ylim=c(0,0.2))
  segments(mp, table_mean[c(1,3,2),names(table_mean)==sp] + table_stdErr[c(1,3,2),names(table_mean)==sp], mp,table_mean[c(1,3,2),names(table_mean)==sp], lwd=2)
  segments(mp - 0.1, table_mean[c(1,3,2),names(table_mean)==sp] + table_stdErr[c(1,3,2),names(table_mean)==sp], mp + 0.1, table_mean[c(1,3,2),names(table_mean)==sp] + table_stdErr[c(1,3,2),names(table_mean)==sp], lwd=2)
  
}
dev.off()


