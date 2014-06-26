#Clear Yer Stuff
rm(list=ls())
<<<<<<< HEAD
=======
#load data
cdata<- read.csv("~/GitHub/Dal-Intertidal-2014/Intertidal_Master_Data_Sheet_2014.csv")
>>>>>>> origin/master

#Load Data
TotalData<- read.csv("~/GitHub/Dal-Intertidal-2014/Intertidal_Master_Data_Sheet_2014.csv")

######### Make new data set with only salinities and abundances#########

#Gives Salinities at high tide
unique(TotalData$SST[TotalData$Strata=="H"])

#Bind salinity and abundance in data table
SSTTable <- cbind(TotalData$SST, TotalData$pc_Acrosiphonia_arcta)

#Makes SalinityTable a new data frame
SSTTable <- as.data.frame(SSTTable)

# change NAs to 0s
SSTTable$V2[is.na(SSTTable$V2)]=0

#Change Column Names

colnames(SSTTable) <- c("SST", "Percent Coverage")


##
require(plyr)
TotalData[is.na(TotalData)]=0
FigureTable<- ddply(TotalData,.(SST),summarize,
                    MeanPercentCoverage=mean(pc_Acrosiphonia_arcta,na.rm = TRUE),
                    SDPercentCoverage=sd(pc_Acrosiphonia_arcta,na.rm = TRUE),
                    NPercentCoverage=sum(is.na(pc_Acrosiphonia_arcta)==F)
)

###### Make Graph ########

jpeg('SSTPercent Coverage1.jpeg', height=1200, width=2400, res=400, qual=100 )
barplot(FigureTable$MeanPercentCoverage, names.arg=FigureTable$SST, xlab="SST", ylab= expression ("Percent Coverage (%)"), main=" ")
dev.off()
getwd()

#### Error Bars ###

AB_mean <- FigureTable$MeanPercentCoverage*100
AB_se <- FigureTable$SDPercentCoverage/sqrt(FigureTable$NPercentCoverage)*100

jpeg('SSTPercent Coverage1.jpg', height=1200, width=2400, res=400, qual=100 )
mp <- barplot(AB_mean, names.arg=FigureTable$SST, xlab="SST", ylab= expression ("Percent coverage (%)"), main=" ",ylim=c(0,25)) # plots the barplot and saves the midpoints in mp
segments(mp, AB_mean + AB_se, mp,AB_mean, lwd=2) # plots positive error bar centered on mp
segments(mp - 0.1, AB_mean + AB_se, mp + 0.1, AB_mean + AB_se, lwd=2) #plots error bar caps
dev.off()

getwd()

<<<<<<< HEAD
=======
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
location_data <- matrix(c(44.5001, -63.9250, 44.6332, -63.5987, 44.6148, -65.6774, 45.1596, -64.3581, 43.948126, -64.820485),ncol=2,byrow=TRUE)
colnames(location_data) <- c("Latitude", "Longitude")
rownames(location_data) <- c("Cranberry Cove", "South Street", "Bear River", "Wolfville", "Summerville")
location_data <- as.data.frame.matrix(location_data) 

# plot basic map
map("worldHires", xlim=xlim, ylim=ylim, col="gray90", fill=TRUE, resolution=0)    # make base map
map.axes()                                                                        # add axes
map.scale(relwidth=0.5)                                                           # add scale bar proportioned to 50% width
points(location_data$Longitude,location_data$Latitude,pch=16,cex=1.5)
locations <- c("Cranberry Cove", "South Street", "Bear River", "Wolfville", "Summerville")
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
>>>>>>> origin/master
