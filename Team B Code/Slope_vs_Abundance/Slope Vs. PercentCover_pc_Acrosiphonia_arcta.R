#Load Data
TotalData<- read.csv("~/GitHub/Intertidal_Master_Data_Sheet_2014.csv")

######### Make new data set with only salinities and abundances#########

#Gives Salinities at high tide
unique(TotalData$Slope[TotalData$Strata=="H"])

#Bind salinity and abundance in data table
SlopeTable <- cbind(TotalData$Slope, TotalData$pc_Acrosiphonia_arcta)

#Makes SalinityTable a new data frame
SlopeTable <- as.data.frame(SlopeTable)

# change NAs to 0s
SlopeTable$V2[is.na(SlopeTable$V2)]=0

#Change Column Names

colnames(SlopeTable) <- c("Slope", "Percent Coverage")


##
require(plyr)
TotalData[is.na(TotalData)]=0
FigureTable<- ddply(TotalData,.(Slope),summarize,
                    MeanPercentCoverage=mean(pc_Acrosiphonia_arcta,na.rm = TRUE),
                    SDPercentCoverage=sd(pc_Acrosiphonia_arcta,na.rm = TRUE),
                    NPercentCoverage=sum(is.na(pc_Acrosiphonia_arcta)==F)
)

###### Make Graph ########



#### Error Bars ###

AB_mean <- FigureTable$MeanPercentCoverage*100
AB_se <- FigureTable$SDPercentCoverage/sqrt(FigureTable$NPercentCoverage)*100

jpeg('Slopepc_Acrosiphonia_arcta.jpg', height=1200, width=2400, res=400, qual=100 )
mp <- barplot(AB_mean, names.arg=FigureTable$SST, xlab="Slope", ylab= expression ("Percent coverage (%)"), main=" ",ylim=c(0,50)) # plots the barplot and saves the midpoints in mp
segments(mp, AB_mean + AB_se, mp,AB_mean, lwd=2) # plots positive error bar centered on mp
segments(mp - 0.1, AB_mean + AB_se, mp + 0.1, AB_mean + AB_se, lwd=2) #plots error bar caps
dev.off()

getwd()
