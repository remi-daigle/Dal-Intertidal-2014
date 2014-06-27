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

rich <- apply(PA,1,sum)

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
cdata[is.na(cdata)] <- 0

#runs diversity
div <- diversity(species, "shannon")

# make data frame
df <- as.data.frame(cbind(div=div,rich=rich))
df$Site <- row.names(df)


# merge with main dataframe
cdata2 <- merge(cdata,df,by.x="Site_name",by.y="Site")

# break up data
phys <- cdata2[,9:21]


# make vector of label
## TO DO: rename labels!
phys_names <- c("Slope \n(°)","SST \n(°C) ","Temp Buoy \n(°C)","Salinity \n(ppt)","Salt Buoy \n(ppt)","RPD \n(cm)","SWH \n(m)","Tidal Average \n(m)",
                "Tidal Range \n(m)","Grain size\n(<0.5 mm)","Grain size\n(0.5-1.7 mm)","Grain size\n(1.7-11.2 mm)","Grain size\n(>11.2 mm)")

## calculate means and plot
require(plyr)

# for richness

jpeg("Diversity vs Phys/richness.jpg", height=3600, width=2400, res=400, qual=100  )
par(mfrow=c(5,3))
for(ph in names(phys)){
    df=as.data.frame(cbind(x=phys[,names(phys)==ph],y=cdata2$rich))
    plot(df$x,df$y, xlab=phys_names[names(phys)==ph], ylab= "Richness (spp.)")
    
}
dev.off()

# For diversity
jpeg("Diversity vs Phys/Diversity.jpg", height=3600, width=2400, res=400, qual=100  )
par(mfrow=c(5,3))
for(ph in names(phys)){
  df=as.data.frame(cbind(x=phys[,names(phys)==ph],y=cdata2$div))
  plot(df$x,df$y, xlab=phys_names[names(phys)==ph], ylab= "Diversity (spp.)")
  
}
dev.off()


