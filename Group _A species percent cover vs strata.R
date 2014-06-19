rm(list=ls())

######################### Strata Vs Biodiversity #####################3333
Bio <-read.csv("~/GitHub/Dal-Intertidal-2014/Intertidal_Master_Data_Sheet_2014.csv") 
require(plyr)

 ### High Tide ###
table_mean <- ddply(Bio,.(Strata),summarize,
               #QUADRAT SAMPLES#
               Fucus_spiralis_percentcover=mean(pc_Fucus_spiralis,na.rm = TRUE),
               Fucus_distichous_drift_percentcover=mean(pc_Fucus_distichous_drift,na.rm = TRUE),
               Fucus_distichous_attached_percentcover=mean(pc_Fucus_distichous_attached,na.rm = TRUE),
               Fucus_vesiculosus_percentcover=mean(pc_Fucus_vesiculosus,na.rm = TRUE),
               Ulva_.something..._percentcover=mean(pc_Ulva_.something...,na.rm = TRUE),  
              
)
table_sd <- ddply(Bio,.(Strata),summarize,
                    #QUADRAT SAMPLES#
                  Fucus_spiralis_percentcover=sd(pc_Fucus_spiralis,na.rm = TRUE),
                  Fucus_distichous_drift_percentcover=sd(pc_Fucus_distichous_drift,na.rm = TRUE),
                  Fucus_distichous_attached_percentcover=sd(pc_Fucus_distichous_attached,na.rm = TRUE),
                  Fucus_vesiculosus_percentcover=sd(pc_Fucus_vesiculosus,na.rm = TRUE),
                  Ulva_.something..._percentcover=sd(pc_Ulva_.something...,na.rm = TRUE),
)

stdErr <- function(x) {
  x2=x[is.na(x)==F]              # remove the NA's
  sd(x2)/ sqrt(length(x2))       # calculate SE
}
table_stdErr <- ddply(Bio,.(Strata),summarize,
                  #QUADRAT SAMPLES#
                  Mytilus_sp_abunbance=stdErr(ab_m2_Mytilus_sp/Quadrat_m2),
                  Semibalanus_balanoides_abunbance=stdErr(ab_m2_Semibalanus_balanoides/Quadrat_m2),
                  Littorina_saxatilis_abunbance=stdErr(ab_m2_Littorina_saxatilis/Quadrat_m2),
                  Pagurus_longicarpus_abunbance=stdErr(ab_m2_Pagurus_longicarpus/Quadrat_m2),
                  Carcinus_maenas_abunbance=stdErr(ab_m2_Carcinus_maenas/Quadrat_m2),
                  ab_m2_Chondrus.crispus_abundance=stdErr(ab_m2_Chondrus.crispus/Quadrat_m2),  
                  #Core Samples#
                  Mya_arenaria_abunbance=stdErr(ab_m3_Mya_arenaria/Core_m3),
                  Mytilus_sp_abunbance=stdErr(ab_m3_Mytilus_sp/Core_m3),
                  Semibalanus_balanoide_abunbance=stdErr(ab_m3_Semibalanus_balanoides/Core_m3),
                  Pagurus_longicarpus_abunbance=stdErr(ab_m3_Pagurus_longicarpus/Core_m3),
                  Glycera_dibrachiata_abunbance=stdErr(ab_m3_Glycera_dibrachiata/Core_m3),
                  ab_m3_Asterias.spp_abunbance=stdErr(ab_m3_Asterias.spp./Core_m3)
)


barplot(table_mean$Mytilus_sp_abunbance[c(1,3,2)], names.arg=c("High","Mid","Low"), xlab="Tidal Ranges",ylab= "Mean abundances")



                  
  
obj <- hist(cdata$size_mm[cdata$strata=="H"])
obj$counts <- obj$counts/sum(obj$counts)*100
plot(obj, main=" B ", ylab="Proportion  (%)", xlab="Length (mm)", ylim=c(0,100))