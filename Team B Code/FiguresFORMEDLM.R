######## Figure List #############

#Clear Yer Stuff
rm(list=ls())

wd="/Users/daniellelmanuel/Dal-Intertidal-2014/"
getwd()
ls()
# site map
source("SiteMap.R")
source("Consultant_data_manipulation.R") #at the end

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

