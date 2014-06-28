#Low Tide (Strata) vs. Abundances (Biological)

#Clear Yer Stuff
rm(list=ls())

#Load Data
TotalData<- read.csv("~/GitHub/Dal-Intertidal-2014/Intertidal_Master_Data_Sheet_2014.csv")

######### Make new data set with only salinities and abundances#########

#Gives Salinities at high tide
unique(TotalData$SST[TotalData$Strata=="H"])

#Bind salinity and abundance in data table
SSTTable <- cbind(TotalData$SST, TotalData$ab_m3_Corophium.volutato)

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
                    MeanAbundance=mean(ab_m3_Corophium.volutato/Core_m3,na.rm = TRUE),
                    SDAbundance=sd(ab_m3_Corophium.volutato/Core_m3,na.rm = TRUE),
                    NAbundance=sum(is.na(ab_m3_Corophium.volutato)==F)
)

###### Make Graph ########

jpeg('SSTAbundanceab_m3_Corophium.volutato.jpeg', height=1200, width=2400, res=400, qual=100 )
barplot(FigureTable$MeanAbundance, names.arg=FigureTable$SST, xlab="SST", ylab= expression ("Abundance (Ind/m"^3*")"), main=" ")
dev.off()

#### Error Bars ###

AB_mean <- FigureTable$MeanAbundance
AB_se <- tapply(SSTTable$Abundance,INDEX=SSTTable$SST, sd, na.rm = TRUE)/sqrt(count(SSTTable,vars="SST")$freq)

jpeg('SSTAbundanceab_m3_Corophium.volutato.jpeg', height=1200, width=2400, res=400, qual=100 )
mp <- barplot(FigureTable$MeanAbundance, names.arg=FigureTable$SST, xlab="SST", ylab= expression ("Abundance (Ind/m"^3*")"), main=" ",ylim=c(0,2200)) # plots the barplot and saves the midpoints in mp
segments(mp, AB_mean + AB_se, mp,AB_mean, lwd=2) # plots positive error bar centered on mp
segments(mp - 0.1, AB_mean + AB_se, mp + 0.1, AB_mean + AB_se, lwd=2) #plots error bar caps
dev.off()

getwd()
