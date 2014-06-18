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

