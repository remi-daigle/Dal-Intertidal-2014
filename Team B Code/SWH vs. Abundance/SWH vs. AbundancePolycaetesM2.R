##SWH vs. Abundances (Biological)##

#Load Data
TotalData <- read.csv(paste(wd, "/Intertidal_Master_Data_Sheet_2014.csv", sep=""))

##
require(plyr)
TotalData[is.na(TotalData)]=0
FigureTable<- ddply(TotalData,.(SWH),summarize,
                    MeanAbundance=mean(ab_m2_Carcinus_maenas/Quadrat_m2,na.rm = TRUE),
                    SDAbundance=sd(ab_m2_Carcinus_maenas/Quadrat_m2,na.rm = TRUE)
)

### Make SWH Table ####

#Bind salinity and abundance in data table
SWHTable <- cbind(TotalData$SWH, TotalData$ab_m2_Carcinus_maenas)

#Makes SalinityTable a new data frame
SWHTable <- as.data.frame(SWHTable)

# change NAs to 0s
SWHTable$V2[is.na(SWHTable$V2)]=0

#Change Column Names
colnames(SWHTable) <- c("SWH", "Abundance")

#### Error Bars ###

AB_mean <- FigureTable$MeanAbundance
AB_se <- tapply(SWHTable$Abundance,INDEX=SWHTable$SWH, sd, na.rm = TRUE)/sqrt(count(SWHTable,vars="SWH")$freq)


jpeg(file = paste(SaveHere, "SWHab_m2_Carcinus_maenas.jpg"), height=1200, width=2400, res=400, qual=100  )
mp <- barplot(FigureTable$MeanAbundance, names.arg=FigureTable$SWH, xlab="Significant Wave Height (m)", ylab= expression ("Abundance (Ind/m"^2*")"), main=" ",ylim=c(0,10))              # plots the barplot and saves the midpoints in mp
segments(mp, AB_mean + AB_se, mp,AB_mean, lwd=2)  # plots positive error bar centered on mp
segments(mp - 0.1, AB_mean + AB_se, mp + 0.1, AB_mean + AB_se, lwd=2)  #plots error bar caps
dev.off()


