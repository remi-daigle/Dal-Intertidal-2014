#Low Tide (Strata) vs. Abundances (Biological)

#Clear Yer Stuff
rm(list=ls())

#Load Data
TotalData<- read.csv("C:/Users/Jessica/Documents/GitHub/Dal-Intertidal-2014/Intertidal_Master_Data_Sheet_2014.csv")

######### Make new data set with only salinities and abundances#########

#Gives Salinities at high tide
unique(TotalData$Salinity[TotalData$Strata=="H"])

#Bind salinity and abundance in data table
SalinityTable <- cbind(TotalData$Salinity, TotalData$ab_m3_Semibalanus_balanoides)

#Makes SalinityTable a new data frame
SalinityTable <- as.data.frame(SalinityTable)

# change NAs to 0s
SalinityTable$V2[is.na(SalinityTable$V2)]=0

#Change Column Names

colnames(SalinityTable) <- c("Salinity", "Abundance")


##
require(plyr)
TotalData[is.na(TotalData)]=0
FigureTable<- ddply(TotalData,.(Salinity),summarize,
                MeanAbundance=mean(ab_m3_Semibalanus_balanoides/Core_m3,na.rm = TRUE),
                SDAbundance=sd(ab_m3_Semibalanus_balanoides/Core_m3,na.rm = TRUE),
                NAbundance=sum(is.na(ab_m3_Semibalanus_balanoides)==F)
)

###### Make Graph ########
jpeg('SalinityAbundance1.jpeg', height=1200, width=2400, res=400, qual=100  )
barplot(FigureTable$MeanAbundance, names.arg=FigureTable$Salinity, xlab="Salinity", ylab= expression ("Abundance (Ind/m"^3*")"), main=" ")
dev.off()

getwd()

#### Error Bars ###

AB_mean <- FigureTable$MeanAbundance
AB_se <- tapply(SalinityTable$Abundance,INDEX=SalinityTable$Salinity, sd, na.rm = TRUE)/sqrt(count(SalinityTable,vars="Salinity")$freq)

jpeg('SalinityAbundance1', height=1200, width=2400, res=400, qual=100  )
mp <- barplot(FigureTable$MeanAbundance, names.arg=FigureTable$Salinity, xlab="Salinity", ylab= expression ("Abundance (Ind/m"^3*")"), main=" ",ylim=c(0,250))              # plots the barplot and saves the midpoints in mp
segments(mp, AB_mean + AB_se, mp,AB_mean, lwd=2)  # plots positive error bar centered on mp
segments(mp - 0.1, AB_mean + AB_se, mp + 0.1, AB_mean + AB_se, lwd=2)  #plots error bar caps
dev.off()

getwd()

