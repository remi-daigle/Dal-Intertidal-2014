#import intertidal master data sheet

cdata <- read.csv(paste(wd,"Intertidal_Master_Data_Sheet_2014.csv",sep=""))

#abundance and species richness

species <- cdata[,substr(names(cdata),1,2)=="ab"|substr(names(cdata),1,2)=="pc"]

#make presence/absence matrix
PA <- as.data.frame(matrix(NA,nrow=length(unique(cdata$Site_name)),ncol=length(names(species))))
names(PA) <- names(species)
row.names(PA) <- unique(cdata$Site_name)
for(site in unique(cdata$Site_name)){
  for(sp in names(PA)){
    PA[unique(cdata$Site_name)==site,names(species)==sp] <- sum(is.na(species[cdata$Site_name==site,names(species)==sp])==F)>=1
  }
}

richness <- apply(PA,1,sum)

#next, calculate diversity
#using abundance (if it starts with ab) sum up each column

#install vegan to calculate diversity

#install.packages("vegan")
require(vegan)


species2 <- PA
for(site in unique(cdata$Site_name)){
  for(sp in names(species2)){
    species2[unique(cdata$Site_name)==site,names(species2)==sp] <- mean(species[unique(cdata$Site_name)==site,names(species2)==sp],na.rm=T)
  }
}

#create a dataframe that only had abundances
species <- species2[,substr(names(species2),1,2)=="ab"]


#made all nas = 0
species[is.na(species)] <- 0

#runs diversity
diversity <- diversity(species, "shannon")




