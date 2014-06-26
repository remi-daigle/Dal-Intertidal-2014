######## Figure List #############
#Clear Yer Stuff
rm(list=ls())

wd=("~/GitHub/Dal-Intertidal-2014/")
setwd(wd)
ls()
# site map
source("SiteMap.R")
source("Consultant_data_manipulation.R") #at the end

# Physical variables vs Biological
# SST
# Salinity
# Slope
require(plyr)
source(paste(wd,"Team B Code/Slope_vs_Abundance/Slope - ab_m2_Polycaetes.R",sep=""))
source(paste(wd,"Team B Code/Slope_vs_Abundance/Slope - ab_m2_Littorina_saxatilis.R",sep=""))
source(paste(wd,"Team B Code/Slope_vs_Abundance/Slope - ab_m3_Mya_arenaria.R",sep=""))
source(paste(wd,"Team B Code/Slope_vs_Abundance/Slope - ab_m3_Mytilus_sp.R",sep=""))
source(paste(wd,"Team B Code/Slope_vs_Abundance/Slope - ab_m3_Semibalanus_balanoides.R",sep=""))
source(paste(wd,"Team B Code/Slope_vs_Abundance/Slope - ab_m2_Mytilus.sp.R",sep=""))
source(paste(wd,"Team B Code/Slope_vs_Abundance/Slope - ab_m2_Semibalanus_balanoides.R",sep=""))
source(paste(wd,"Team B Code/Slope_vs_Abundance/Slope - ab_m3_Pagurus_longicarpus.R",sep=""))
source(paste(wd,"Team B Code/Slope_vs_Abundance/Slope - pc_Fucus_spiralis.R",sep=""))
source(paste(wd,"Team B Code/Slope_vs_Abundance/Slope - pc_Fucus_distichous_drift.R",sep=""))
# SWH
# RPD
# Tidal Height
# Grain Size
# Strata
source("Species_abundance_(core_quadrat).R")

# Derived Biological var (Biodiversity, species richness)


