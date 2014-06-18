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
    test[unique(cdata$Site_name)==site,names(species)==sp] <- sum(is.na(species[cdata$Site_name==site,names(species)==sp]))>1
  }
}


cdata$sp_richness <- 0
for(i in 1:dim(cdata)[1]){
  cdata$sp_richness[i]=sum(is.na(species[i,])==FALSE)
  
}

#install.packages("plyr")       # install this great package for splitting, applying and combining data
#require(plyr)  


#species$Site_name <- cdata$Site_name
#test <- ddply(cdata,.(Site_name),summarize,
#               sp_rich=
#)
