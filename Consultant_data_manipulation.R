rm(list=ls())
#load data
cdata<- read.csv("~/Documents/GitHub/Dal-Intertidal-2014/Consultant Data Sheet 2013.csv")

# create record for each ID
cdata2 <- cbind(as.character(unique(cdata$ID)),0,0)

# count clams in each ID
counter <- 1
for(i in unique(cdata$ID)){
  cdata2[counter,2] <- sum(cdata$ID==i&cdata$size_mm!="NA")
  cdata2[counter,3] <- mean(cdata$volume_m3[cdata$ID==i])
  counter=counter+1
}

# make cdata2 a dataframe
cdata2 <- as.data.frame(cdata2)
names(cdata2) <- c("ID","count","volume")

# convert count and volume into numeric
cdata2$count=as.numeric(as.character(cdata2$count))
cdata2$volume=as.numeric(as.character(cdata2$volume))


# change NAs to 0s
cdata2$count[is.na(cdata2$count)]=0

#calculate density
cdata2$density <- cdata2$count/cdata2$volume

write.csv(cdata2,"~/Documents/GitHub/Dal-Intertidal-2014/Consultant Data Sheet 2 2013.csv")

################ 5a ############################
#regular
hist(cdata$size_mm)

#log
obj <- hist(cdata$size_mm)
obj$counts <- log10(obj$counts+1)
plot(obj)

# by tidal height
obj <- hist(cdata$size_mm[cdata$strata=="H"])
obj$counts <- obj$counts/sum(obj$counts)*100
plot(obj)
obj <- hist(cdata$size_mm[cdata$strata=="M"])
obj$counts <- obj$counts/sum(obj$counts)*100
plot(obj)
obj <- hist(cdata$size_mm[cdata$strata=="L"])
obj$counts <- obj$counts/sum(obj$counts)*100
plot(obj)

######################## 5b ##################
hist(cdata$size_mm[cdata$depth_bin==1])
hist(cdata$size_mm[cdata$depth_bin==2])
hist(cdata$size_mm[cdata$depth_bin==3])
hist(cdata$size_mm[cdata$depth_bin==4])
hist(cdata$size_mm[cdata$depth_bin==5])

######################## 5c #####################
require(plyr)
table <- ddply(cdata,.(strata),summarize,
               mean_length=mean(size_mm,na.rm = TRUE),
               sd_length=sd(size_mm,na.rm = TRUE),
               n_length=sum(is.na(size_mm)==F)
)



##################### site map #########################
install.packages("marmap")
install.packages("maps")
install.packages("mapdata")
require(maps)
require(mapdata)

# setlat/long limits
xlim=c(-67,-62)
ylim=c(43,46)

#Lat and Long in a table
location_data <- matrix(c(44.5001, -63.9250, 44.6086, -63.4936, 44.6148, -65.6774, 45.1596, -64.3581, 43.948126, -64.820485),ncol=2,byrow=TRUE)
colnames(location_data) <- c("Latitude", "Longitude")
rownames(location_data) <- c("Cranberry Cove", "Eastern Passage", "Bear River", "Wolfville", "Summerville")
location_data <- as.data.frame.matrix(location_data) 

# plot basic map
map("worldHires", xlim=xlim, ylim=ylim, col="gray90", fill=TRUE, resolution=0)    # make base map
map.axes()                                                                        # add axes
map.scale(relwidth=0.5)                                                           # add scale bar proportioned to 50% width
points(location_data$Longitude,location_data$Latitude,pch=16,cex=1.5)
locations <- c("Cranberry Cove", "Eastern Passage", "Bear River", "Wolfville", "Summerville")
text(location_data$Longitude,location_data$Latitude,locations,col="black",cex=0.7,pos=2)
title("Sampling Stations")


#####################Diversity Map##################
#import intertidal master data sheet
cdata <- read.csv("~/Documents/Dalhousie/Intertidal Ecology/Dal-Intertidal-2014/Intertidal_Master_Data_Sheet_2014.csv")

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

#runs diversity, puts into matrix
diversitydata <- diversity(species, "shannon")
#matrix into dataframe
ddiversity <- as.data.frame(diversitydata)
#adds columns from cdata
ddiversity$site <- cdata$Site_name
ddiversity$strata <- cdata$Strata
ddiversity$Longitude <- cdata$Longitude
ddiversity$Latitude <- cdata$Latitude

#map with diversity
install.packages("marmap")
install.packages("maps")
install.packages("mapdata")
require(maps)
require(mapdata)

# setlat/long limits
xlim=c(-67,-62)
ylim=c(43,46)

#Lat and Long in a table
location_data <- matrix(c(44.5001, -63.9250, 44.6086, -63.4936, 44.6148, -65.6774, 45.1596, -64.3581, 43.948126, -64.820485),ncol=2,byrow=TRUE)
colnames(location_data) <- c("Latitude", "Longitude")
rownames(location_data) <- c("Cranberry Cove", "Eastern Passage", "Bear River", "Wolfville", "Summerville")
location_data <- as.data.frame.matrix(location_data) 

map("worldHires", xlim=xlim, ylim=ylim, col="gray90", fill=TRUE, resolution=0)    # make base map
map.axes()                                                                        # add axes
map.scale(relwidth=0.5)    
locations <- c("Cranberry Cove", "Eastern Passage", "Bear River", "Wolfville", "Summerville")
text(ddiversity$Longitude,ddiversity$Latitude,ddiversity$site,col="black",cex=0.7,pos=2)
title("Sampling Stations by Diversity")
for(site in 1:length(ddiversity$diversitydata)){
  size=log10(ddiversity$diversitydata[site]+1)*10                                                                     # calculate symbol size
  points(ddiversity$Longitude[site],ddiversity$Latitude[site],pch=16,cex=size)                       # add site locations
}

#calculate richness
SpeciesRichness <- rowSums(test)
sp_richness <- as.data.frame(SpeciesRichness)
sp_richness$sites=row.names(sp_richness)
#add latitude and longitude
unique(cdata$Site_name)             
unique(sp_richness$sites)

unique(cdata$Site_name)==unique(sp_richness$sites)  

# make new dataframe by merging the species and site data by location/site_name
sp_richness_data <- merge(cdata,sp_richness, by.x="Site_name",by.y="sites")

#map
map("worldHires", xlim=xlim, ylim=ylim, col="gray90", fill=TRUE, resolution=0)    # make base map
map.axes()                                                                        # add axes
map.scale(relwidth=0.5)    
text(sp_richness_data$Longitude,sp_richness_data$Latitude,sp_richness_data$Site_name,col="black",cex=0.7,pos=2)
title("Sampling Stations by Species Richness")
for(site in 1:length(sp_richness_data$SpeciesRichness)){
  size=log10(sp_richness_data$SpeciesRichness[site]+1)*3                                                                     # calculate symbol size
  points(sp_richness_data$Longitude[site],sp_richness_data$Latitude[site],pch=16,cex=size)                       # add site locations
}
