Slope vs. Abundances (Biological)

#Clear Yer Stuff
rm(list=ls())


#Load Data
TotalData<- read.csv(paste(wd, "Intertidal_Master_Data_Sheet_2014.csv", sep=""))

**Be sure not to clear the loaded data after setting the directory


######### Make new data set with only Slope and abundances #########

#Take Mean of Abundances per Strata (At each site ~ Slope)

TotalData[is.na(TotalData)]=0 ##Change NA to 0


## Means
TotalData[is.na(TotalData)]=0
FigureTableSlope<- ddply(TotalData,.(Slope),summarize,
                         MeanAbundance=mean(ab_m2_Littorina_saxatilis/Quadrat_m2,na.rm = TRUE),
                         NAbundance=sum(is.na(ab_m2_Littorina_saxatilis)==F)
)

###### Make Graph ########

attach(mtcars)

jpeg('SLOPEAbundanceLittorinaSaxatilis.jpg', height=1200, width=2400, res=400, qual=100)
plot(FigureTableSlope$Slope,FigureTableSlope$MeanAbundance, main=" ",
     xlab="Slope ", ylab="Abundance (Ind/m2) ", pch=19)
dev.off()

###Add fit lines
abline(lm(FigureTableSlope$MeanAbundance~FigureTableSlope$Slope), col="red") # regression line (y~x)


getwd()