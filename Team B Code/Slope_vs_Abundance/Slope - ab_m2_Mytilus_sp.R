#Slope vs. Abundances (Biological)


#Load Data
TotalData<- read.csv(paste(wd, "Intertidal_Master_Data_Sheet_2014.csv", sep=""))

######### Make new data set with only Slope and abundances #########

#Take Mean of Abundances per Strata (At each site ~ Slope)

TotalData[is.na(TotalData)]=0 ##Change NA to 0


## Means
TotalData[is.na(TotalData)]=0
FigureTableSlope<- ddply(TotalData,.(Slope),summarize,
                       MeanAbundance=mean(ab_m2_Mytilus_sp/Quadrat_m2,na.rm = TRUE),
                       NAbundance=sum(is.na(ab_m2_Mytilus_sp)==F)
)

###### Make Graph ########

attach(mtcars)

jpeg('SLOPEAbundanceMytilusm2.jpg', height=1200, width=2400, res=400, qual=100)
plot(FigureTableSlope$Slope,FigureTableSlope$MeanAbundance, main=" ", 
     xlab="Slope ", ylab="Abundance (Ind/m2) ", pch=19)


###Add fit lines
abline(lm(FigureTableSlope$MeanAbundance~FigureTableSlope$Slope), col="red") # regression line (y~x) 
dev.off()

getwd()

