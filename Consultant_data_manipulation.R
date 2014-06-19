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
