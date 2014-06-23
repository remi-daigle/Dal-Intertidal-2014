#Clear Yer Stuff
rm(list=ls())

#Load Data
TotalData<- read.csv("~/GitHub/Dal-Intertidal-2014/Intertidal_Master_Data_Sheet_2014.csv")

######### Make new data set with only salinities and abundances#########

#Gives Salinities at high tide
unique(TotalData$SST[TotalData$Strata=="H"])

#Bind salinity and abundance in data table
SSTTable <- cbind(TotalData$SST, TotalData$ab_m2_Littorina_saxatilis)

#Makes SalinityTable a new data frame
SSTTable <- as.data.frame(SSTTable)

# change NAs to 0s
SSTTable$V2[is.na(SSTTable$V2)]=0

#Change Column Names

colnames(SSTTable) <- c("SST", "Abundance")

##
require(plyr)
TotalData[is.na(TotalData)]=0
FigureTable<- ddply(TotalData,.(SST),summarize,
                    MeanAbundance=mean(ab_m2_Littorina_saxatilis/Quadrat_m2,na.rm = TRUE),
                    SDAbundance=sd(ab_m2_Littorina_saxatilis/Quadrat_m2,na.rm = TRUE),
                    NAbundance=sum(is.na(ab_m2_Littorina_saxatilis)==F)
)
###### Make Graph ########
jpeg('SSTAbundance1.jpeg', height=1200, width=2400, res=400, qual=100 )
barplot(FigureTable$MeanAbundance, names.arg=FigureTable$SST, xlab="SST", ylab= expression ("Abundance (Ind/m"^2*")"), main=" ")
dev.off()
#### Error Bars ###
AB_mean <- FigureTable$MeanAbundance
AB_se <- tapply(SSTTable$Abundance,INDEX=SSTTable$SST, sd, na.rm = TRUE)/sqrt(count(SSTTable,vars="SST")$freq)

jpeg('SSTAbundance1.jpeg', height=1200, width=2400, res=400, qual=100 )
mp <- barplot(FigureTable$MeanAbundance, names.arg=FigureTable$SST, xlab="SST", ylab= expression ("Abundance (Ind/m"^2*")"), main=" ",ylim=c(0,1850)) # plots the barplot and saves the midpoints in mp
segments(mp, AB_mean + AB_se, mp,AB_mean, lwd=2) # plots positive error bar centered on mp
segments(mp - 0.1, AB_mean + AB_se, mp + 0.1, AB_mean + AB_se, lwd=2) #plots error bar caps
dev.off()

getwd()
