#Low Tide (Strata) vs. Abundances (Biological)

#Clear Yer Stuff
rm(list=ls())

#Load Data
TotalData<- read.csv("/Users/daniellelmanuel/Desktop/Intertidal_Master_Data_Sheet_2014forR.csv")

######### Make new data set with only salinities and abundances#########

#Gives Salinities at high tide
unique(TotalData$Salinity[TotalData$Strata=="H"])

#Bind salinity and abundance in data table
SalinityTable <- cbind(TotalData$Salinity, TotalData$ab_m2_Mytilus_sp)

#Makes SalinityTable a new data frame
SalinityTable <- as.data.frame(SalinityTable)

# change NAs to 0s
SalinityTable$V2[is.na(SalinityTable$V2)]=0

#Change Column Names

colnames(SalinityTable) <- c("Salinity", "Abundance")


##
TotalData[is.na(TotalData)]=0
FigureTable<- ddply(TotalData,.(Salinity),summarize,
                MeanAbundance=mean(ab_m2_Mytilus_sp/Quadrat_m2,na.rm = TRUE),
                SDAbundance=sd(ab_m2_Mytilus_sp/Quadrat_m2,na.rm = TRUE),
                NAbundance=sum(is.na(ab_m2_Mytilus_sp)==F)
)


