######## Figure List #############
#Clear Yer Stuff
rm(list=ls())

wd=("~/GitHub/Dal-Intertidal-2014/")
setwd(wd)
ls()
# site map
source("SiteMap.R")
source("Consultant_data_manipulation.R") #at the end
source(paste(wd,"Team B Code/Slope_vs_Abundance/Slope - ab_m2_Polycaetes.R",sep=""))
# Physical variables vs Biological
# SST
# Salinity
# Slope
# SWH
# RPD
# Tidal Height
# Grain Size
# Strata
source("Species_abundance_(core_quadrat).R")

# Derived Biological var (Biodiversity, species richness)

