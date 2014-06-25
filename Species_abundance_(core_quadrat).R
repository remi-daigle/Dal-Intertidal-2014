rm(list=ls())

######################### Strata Vs Biodiversity #####################3333
Bio <-read.csv("C:\\Users\\Joe\\Documents\\GitHub\\Dal-Intertidal-2014\\Intertidal_Master_Data_Sheet_2014.csv") 
require(plyr)
# removing na from Bio graph

Bio$ab_m2_Mytilus_sp[is.na(Bio$ab_m2_Mytilus_sp)]=0
Bio$ab_m2_Semibalanus_balanoides[is.na(Bio$ab_m2_Semibalanus_balanoides)]=0
Bio$ab_m2_Littorina_saxatilis[is.na(Bio$ab_m2_Littorina_saxatilis)]=0
Bio$ab_m2_Pagurus_longicarpus[is.na(Bio$ab_m3_Pagurus_longicarpus)]=0
Bio$ab_m2_Chondrus.crispus[is.na(Bio$ab_m2_Chondrus.crispus)]=0
Bio$ab_m2_Carcinus_maenas[is.na(Bio$ab_m2_Carcinus_maenas)]=0
Bio$ab_m3_Mya_arenaria[is.na(Bio$ab_m3_Mya_arenaria)]=0
Bio$ab_m3_Mytilus_sp[is.na(Bio$ab_m3_Mytilus_sp)]=0
Bio$ab_m3_Semibalanus_balanoides[is.na(Bio$ab_m3_Semibalanus_balanoides)]=0
Bio$ab_m3_Pagurus_longicarpus[is.na(Bio$ab_m3_Pagurus_longicarpus)]=0
Bio$ab_m3_Glycera_dibrachiata[is.na(Bio$ab_m3_Glycera_dibrachiata)]=0
Bio$ab_m2_Chondrus.crispus[is.na(Bio$ab_m2_Chondrus.crispus)]=0
Bio$ab_m2_Hiatella.artica[is.na(Bio$ab_m2_Hiatella.artica)]=0  
Bio$ab_m2_Polycaetes[is.na(Bio$ab_m2_Polycaetes)]=0
Bio$ab_m3_Asterias.spp.[is.na(Bio$ab_m3_Asterias.spp.)]=0  
Bio$ab_Ceramium_spp[is.na(Bio$ab_Ceramium_spp)]=0
### High Tide ###
table_means <- ddply(Bio,.(Strata),summarize,
                     #QUADRAT SAMPLES#
                     Mytilus_sp_abunbance=mean(ab_m2_Mytilus_sp/Quadrat_m2,na.rm = TRUE),
                     Semibalanus_balanoides_abunbance=mean(ab_m2_Semibalanus_balanoides/Quadrat_m2,na.rm = TRUE),
                     Littorina_saxatilis_abunbance=mean(ab_m2_Littorina_saxatilis/Quadrat_m2,na.rm = TRUE),
                     Pagurus_longicarpus_abunbance=mean(ab_m2_Pagurus_longicarpus/Quadrat_m2,na.rm = TRUE),
                     Carcinus_maenas_abunbance=mean(ab_m2_Carcinus_maenas/Quadrat_m2,na.rm = TRUE),
                     ab_m2_Chondrus.crispus_abundance=mean(ab_m2_Chondrus.crispus/Quadrat_m2, na.rm = TRUE),  
                     ab_m2_Hiatella.artica_abundance=mean(ab_m2_Hiatella.artica/Quadrat_m2, na.rm=TRUE),
                     ab_m2_polycaetes_abundance=mean(ab_m2_Polycaetes/Quadrat_m2, na.rm= TRUE),
                     ab_Ceramium_spp_abundance= mean(ab_Ceramium_spp/Quadrat_m2, na.rm= TRUE),
                     #Core Samples#
                     Mya_arenaria_abunbance=mean(ab_m3_Mya_arenaria/Core_m3,na.rm = TRUE),
                     Mytilus_sp_abunbance_core=mean(ab_m3_Mytilus_sp/Core_m3,na.rm = TRUE),
                     Semibalanus_balanoide_abunbance=mean(ab_m3_Semibalanus_balanoides/Core_m3,na.rm = TRUE),
                     Pagurus_longicarpus_abunbance_core=mean(ab_m3_Pagurus_longicarpus/Core_m3,na.rm = TRUE),
                     Glycera_dibrachiata_abunbance=mean(ab_m3_Glycera_dibrachiata/Core_m3,na.rm = TRUE),
                     ab_m3_Asterias.spp_abunbance=mean(ab_m3_Asterias.spp./Core_m3,na.rm = TRUE)
)
table_sd <- ddply(Bio,.(Strata),summarize,
                  #QUADRAT SAMPLES#
                  Mytilus_sp_abunbance=sd(ab_m2_Mytilus_sp/Quadrat_m2,na.rm = TRUE),
                  Semibalanus_balanoides_abunbance=sd(ab_m2_Semibalanus_balanoides/Quadrat_m2,na.rm = TRUE),
                  Littorina_saxatilis_abunbance=sd(ab_m2_Littorina_saxatilis/Quadrat_m2,na.rm = TRUE),
                  Pagurus_longicarpus_abunbance=sd(ab_m2_Pagurus_longicarpus/Quadrat_m2,na.rm = TRUE),
                  Carcinus_maenas_abunbance=sd(ab_m2_Carcinus_maenas/Quadrat_m2,na.rm = TRUE),
                  ab_m2_Chondrus.crispus_abundance=sd(ab_m2_Chondrus.crispus/Quadrat_m2, na.rm = TRUE),  
                  ab_m2_Hiatella.artica_abundance=sd(ab_m2_Hiatella.artica/Quadrat_m2, na.rm= TRUE),
                  ab_m2_polycaetes_abundance=sd(ab_m2_Polycaetes/Quadrat_m2, na.rm= TRUE),
                  ab_Ceramium_spp_abundance= sd(ab_Ceramium_spp/Quadrat_m2, na.rm= TRUE),
                  
                  
                  #Core Samples#
                  Mya_arenaria_abunbance=sd(ab_m3_Mya_arenaria/Core_m3,na.rm = TRUE),
                  Mytilus_sp_abunbance_core=sd(ab_m3_Mytilus_sp/Core_m3,na.rm = TRUE),
                  Semibalanus_balanoide_abunbance=sd(ab_m3_Semibalanus_balanoides/Core_m3,na.rm = TRUE),
                  Pagurus_longicarpus_abunbance_core=sd(ab_m3_Pagurus_longicarpus/Core_m3,na.rm = TRUE),
                  Glycera_dibrachiata_abunbance=sd(ab_m3_Glycera_dibrachiata/Core_m3,na.rm = TRUE),
                  ab_m3_Asterias.spp_abunbance=sd(ab_m3_Asterias.spp./Core_m3,na.rm = TRUE)
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
                      ab_m2_Hiatella.artica_abundance=stdErr(ab_m2_Hiatella.artica/Quadrat_m2),
                      ab_m2_polycaetes_abundance=stdErr(ab_m2_Polycaetes/Quadrat_m2),
                      ab_Ceramium_spp_abundance= stdErr(ab_Ceramium_spp/Quadrat_m2),
                      
                      
                      #Core Samples#
                      Mya_arenaria_abunbance=stdErr(ab_m3_Mya_arenaria/Core_m3),
                      Mytilus_sp_abunbance_core=stdErr(ab_m3_Mytilus_sp/Core_m3),
                      Semibalanus_balanoide_abunbance=stdErr(ab_m3_Semibalanus_balanoides/Core_m3),
                      Pagurus_longicarpus_abunbance_core=stdErr(ab_m3_Pagurus_longicarpus/Core_m3),
                      Glycera_dibrachiata_abunbance=stdErr(ab_m3_Glycera_dibrachiata/Core_m3),
                      ab_m3_Asterias.spp_abunbance=stdErr(ab_m3_Asterias.spp./Core_m3)
)

sp_names <- c("","Mytilus spp.","Semibalanus balanoides", "Littorina saxatilis", "Pagurus longicarpus", "Carcinus maenas", "Chondrus crispus", "Hiatella artica", "Polycaetes spp.", "Ceramium spp.",
              "Arenaria (core)","Mytilus spp. (core)","Semibalanus balanoides (core)", "Pagurus longicarpus (core)", "Glycera dibrachiata (core)", "Asterias spp. (core)")

par(mfrow=c(3,4))
for(sp in names(table_means[,-1])){
  mean_ab <- as.matrix(table_means[names(table_means)==sp])[c(1,3,2)]
  SE <- as.matrix(table_stdErr[names(table_stdErr)==sp])[c(1,3,2)]
  mp <-barplot(mean_ab, names.arg=c("High","Mid","Low"), xlab="Tidal Ranges",ylab= "Mean abundances", ylim=c(0,max(((mean_ab)*2),na.rm=T)))
  segments(mp, mean_ab + SE, mp,mean_ab, lwd=2)  # plots positive error bar centered on mp
  segments(mp - 0.1, mean_ab + SE, mp + 0.1, mean_ab + SE, lwd=2)  #plots error bar caps
  title(sp_names[names(table_means)==sp])
}

ylim=c(0,max(((mean_ab)*2),na.rm=T))


summary(Bio$ab_m2_Hiatella.artica  )
