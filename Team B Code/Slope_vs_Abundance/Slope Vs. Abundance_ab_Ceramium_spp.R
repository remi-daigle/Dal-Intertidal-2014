Slope vs. Abundances (Biological)

#Clear Yer Stuff
rm(list=ls())

#Load Data
TotalData<- read.csv("~/GitHub/Intertidal_Master_Data_Sheet_2014.csv")

######### Make new data set with only Slope and abundances #########

#Take Mean of Abundances per Strata (At each site ~ Slope)

TotalData[is.na(TotalData)]=0 ##Change NA to 0


## Means
require(plyr)
TotalData[is.na(TotalData)]=0
FigureTableSlope<- ddply(TotalData,.(Slope),summarize,
                       MeanAbundance=mean(ab_Ceramium_spp/Quadrat_m2,na.rm = TRUE),
                       NAbundance=sum(is.na(ab_Ceramium_spp)==F)
)

###### Make Graph ########

attach(mtcars)

jpeg('SLOPEAbundanceab_Ceramium_spp', height=1200, width=2400, res=400, qual=100)
plot(FigureTableSlope$Slope,FigureTableSlope$MeanAbundance, main=" ", 
     xlab="Slope ", ylab="Abundance (Ind/m2) ", pch=19)
dev.off()
plot.new()

###Add fit lines
abline(lm(FigureTableSlope$MeanAbundance~FigureTableSlope$Slope), col="red") # regression line (y~x) 

getwd()

