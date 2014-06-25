
#Load Data
TotalData<- read.csv(paste(wd, "Intertidal_Master_Data_Sheet_2014.csv", sep=""))


######### Make new data set with only salinities and abundances#########

#Bind salinity and abundance in data table
SWHTable <- cbind(TotalData$SWH, TotalData$pc_Fucus_vesiculosus)

#Makes SalinityTable a new data frame
SWHTable <- as.data.frame(SWHTable)

# change NAs to 0s
SWHTable$V2[is.na(SWHTable$V2)]=0

#Change Column Names

colnames(SWHTable) <- c("SWH", "Percent Coverage")


##
require(plyr)
TotalData[is.na(TotalData)]=0
FigureTable<- ddply(TotalData,.(SST),summarize,
                    MeanPercentCoverage=mean(pc_Fucus_vesiculosus,na.rm = TRUE),
                    SDPercentCoverage=sd(pc_Fucus_vesiculosus,na.rm = TRUE),
                    NPercentCoverage=sum(is.na(pc_Fucus_vesiculosus)==F)
)


#### Error Bars ###

AB_mean <- FigureTable$MeanPercentCoverage*100
AB_se <- FigureTable$SDPercentCoverage/sqrt(FigureTable$NPercentCoverage)*100

jpeg('SWH_PC_FucusVesi.jpg', height=1200, width=2400, res=400, qual=100 )
mp <- barplot(AB_mean, names.arg=FigureTable$SWH, xlab="Standard Wave Height", ylab= expression ("Percent coverage (%)"), main=" ",ylim=c(0,50)) # plots the barplot and saves the midpoints in mp
segments(mp, AB_mean + AB_se, mp,AB_mean, lwd=2) # plots positive error bar centered on mp
segments(mp - 0.1, AB_mean + AB_se, mp + 0.1, AB_mean + AB_se, lwd=2) #plots error bar caps
dev.off()

getwd()
