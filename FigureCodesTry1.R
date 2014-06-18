#Low Tide (Strata) vs. Abundances (Biological)

#Clear Yer Stuff
rm(list=ls())

#Load Data
TotalData<- read.csv("/Users/daniellelmanuel/Dal-Intertidal-2014/Intertidal_Master_Data_Sheet_2014.csv")

######### Make new data set with only salinities and abundances#########

#Gives Salinities at high tide
unique(TotalData$Salinity[TotalData$Strata=="H"])

#Bind salinity and abundance in data table
SalinityTable <- cbind(as.character(TotalData$Salinity), TotalData$ab_m2_Mytilus_sp)

#Makes SalinityTable a new data frame
SalinityTable <- as.data.frame(SalinityTable)

# change NAs to 0s
SalinityTable$V2[is.na(SalinityTable$V2)]=0

#Change Column Names

colnames(SalinityTable) <- c("Salinity", " Mytilus Abundance [M2]")

TotalData$Strata
names(TotalData)

