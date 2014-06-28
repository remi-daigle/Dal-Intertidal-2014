#Clear Yer Stuff
rm(list=ls())

#Load Data
TotalData<- read.csv("~/GitHub/Dal-Intertidal-2014/Intertidal_Master_Data_Sheet_2014.csv")

######### Make new data set with only salinities and abundances#########

#Gives Salinities at high tide
unique(TotalData$Salinity[TotalData$Strata=="H"])

#Bind salinity and abundance in data table
SSTTable <- cbind(TotalData$Salinity, TotalData$pc_Fucus_distichous_attached)

#Makes SalinityTable a new data frame
SSTTable <- as.data.frame(SalinityTable)

# change NAs to 0s
SSTTable$V2[is.na(SalinityTable$V2)]=0

#Change Column Names

colnames(SalinityTable) <- c("Salinity", "Percent Coverage")


##
require(plyr)
TotalData[is.na(TotalData)]=0
FigureTable<- ddply(TotalData,.(Salinity),summarize,
                    MeanPercentCoverage=mean(pc_Fucus_distichous_attached,na.rm = TRUE),
                    SDPercentCoverage=sd(pc_Fucus_distichous_attached,na.rm = TRUE),
                    NPercentCoverage=sum(is.na(pc_Fucus_distichous_attached)==F)
)

###### Make Graph ########



#### Error Bars ###

AB_mean <- FigureTable$MeanPercentCoverage*100
AB_se <- FigureTable$SDPercentCoverage/sqrt(FigureTable$NPercentCoverage)*100

jpeg('SalinityPercent Coverage1.jpg', height=1200, width=2400, res=400, qual=100 )
mp <- barplot(AB_mean, names.arg=FigureTable$Salinity, xlab="Salinity", ylab= expression ("Percent coverage (%)"), main=" ",ylim=c(0,50)) # plots the barplot and saves the midpoints in mp
segments(mp, AB_mean + AB_se, mp,AB_mean, lwd=2) # plots positive error bar centered on mp
segments(mp - 0.1, AB_mean + AB_se, mp + 0.1, AB_mean + AB_se, lwd=2) #plots error bar caps
dev.off()

getwd()
