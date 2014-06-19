#import intertidal master data sheet

cdata <- read.csv("~/GitHub/Dal-Intertidal-2014/Intertidal_Master_Data_Sheet_2014.csv")

#abundance and species richness

species <- cdata[,substr(names(cdata),1,2)=="ab"|substr(names(cdata),1,2)=="pc"]

test <- as.data.frame(matrix(NA,nrow=length(unique(cdata$Site_name)),ncol=length(names(species))))
names(test) <- names(species)
row.names(test) <- unique(cdata$Site_name)
for(site in unique(cdata$Site_name)){
  for(sp in names(species)){
    print(site)
    print(sp)
    test[unique(cdata$Site_name)==site,names(species)==sp] <- sum(is.na(species[cdata$Site_name==site,names(species)==sp])==F)>=1
  }
}

#next, calculate diversity
#using abundance (if it starts with ab) sum up each column

#install vegan to calculate diversity

install.packages("vegan")
require(vegan)

#create a dataframe that only had abundances
species <- cdata[,substr(names(cdata),1,2)=="ab"]

#made all nas = 0
species[is.na(species)] <- 0

#runs diversity
diversity(species, "shannon")




