#Clear Yer Stuff
rm(list=ls())
<<<<<<< HEAD
=======
#load data
cdata<- read.csv("~/GitHub/Dal-Intertidal-2014/Intertidal_Master_Data_Sheet_2014.csv")
>>>>>>> origin/master

#Load Data
TotalData<- read.csv("~/GitHub/Dal-Intertidal-2014/Intertidal_Master_Data_Sheet_2014.csv")

######### Make new data set with only SST and abundances#########

#Gives SST at high tide
unique(TotalData$SST[TotalData$Strata=="H"])

#Bind SST and abundance in data table
SSTTable <- cbind(TotalData$SST, TotalData$pc_Nostoc_cyanobacteria)

#Makes SSTTable a new data frame
SSTTable <- as.data.frame(SSTTable)

# change NAs to 0s
SSTTable$V2[is.na(SSTTable$V2)]=0

#Change Column Names

colnames(SSTTable) <- c("SST", "Percent Coverage")


##
require(plyr)
TotalData[is.na(TotalData)]=0
FigureTable<- ddply(TotalData,.(SST),summarize,
                    MeanPercentCoverage=mean(pc_Nostoc_cyanobacteria,na.rm = TRUE),
                    SDPercentCoverage=sd(pc_Nostoc_cyanobacteria,na.rm = TRUE),
                    NPercentCoverage=sum(is.na(pc_Nostoc_cyanobacteria)==F)
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
