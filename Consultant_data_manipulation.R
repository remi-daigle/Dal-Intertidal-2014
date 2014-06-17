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
