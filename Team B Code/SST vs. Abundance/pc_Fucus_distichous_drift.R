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
SSTTable <- cbind(TotalData$SST, TotalData$pc_Fucus_distichous_drift)

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
                    MeanPercentCoverage=mean(pc_Fucus_distichous_drift,na.rm = TRUE),
                    SDPercentCoverage=sd(pc_Fucus_distichous_drift,na.rm = TRUE),
                    NPercentCoverage=sum(is.na(pc_Fucus_distichous_drift)==F)
)

###### Make Graph ########

jpeg('SSTPercent Coverage1.jpeg', height=1200, width=2400, res=400, qual=100 )
barplot(FigureTable$MeanPercentCoverage, names.arg=FigureTable$SST, xlab="SST", ylab= expression ("Percent Coverage (%)"), main=" ")
dev.off()
getwd()

