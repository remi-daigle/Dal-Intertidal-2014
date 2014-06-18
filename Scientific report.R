#####Strata Vs Biodiversity#####
rm(list=ls())
#load data
Bio<- read.csv("~/Desktop/Dal-Intertidal-2014/Intertidal_Master_Data_Sheet_2014.csv")
require(plyr)
#High tide#
table<- ddply(Bio.,(Strata), summarize, 
              #quadrat samples#
              mytilus_abs_m2=mean(ab_m2_Mytilus_sp/Quadrat_m2, na.rm=T),
              semibalanus_abs_2=mean(ab_m2_Semibalanus_balanoides/Quadrat_m2, na.rm=T),
              littorina_abs_m2=mean(ab_m2_Littorina_saxatilis/Quadrat_m2, na.rm=T),
              pagurus_abs_m2=mean(ab_m2_Pagurus_longicarpus/Quadrat_m2, na.rm=T),
              carcinus_abs_m2=mean(ab_m2_Carcinus_maenas/Quadrat_m2, na.rm=T),
              #core samples#
              mya_abs_m3=mean(ab_m3_Mya_arenaria/Core_m3, na.rm=T),
              mytilus_abs_m3=mean(ab_m3_Mytilus_sp/Core_m3, na.rm=T),
              semibalanus_abs_m3=mean(ab_m3_Semibalanus_balanoides/Core_m3, na.rm=T),
              pagurus_abs_m3=mean(ab_m3_Pagurus_longicarpus/Core_m3, na.rm=T),
              glycera_abs_m3=mean(ab_m3_Glycera_dibrachiata/Core_m3, na.rm=T),
              siliqua_abs_m3=mean(ab_m3_Siliqua_costata/Core_m3, na.rm=T)
              )