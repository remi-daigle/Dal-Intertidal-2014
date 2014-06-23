 RPD vs. Abundances (Biological)

#Clear Yer Stuff
rm(list=ls())

#Load Data
TotalData<- read.csv("/Users/daniellelmanuel/Desktop/Intertidal_Master_Data_Sheet_2014forR.csv")

######### Make new data set with only RPD and abundances #########

#Take Mean of Abundances per Strata (At each site ~ RPD)
 
TotalData[is.na(TotalData)]=0 ##Change NA to 0


## Means
TotalData[is.na(TotalData)]=0
FigureTableRPD<- ddply(TotalData,.(RPD),summarize,
                    MeanAbundance=mean(ab_m2_Mytilus_sp/Quadrat_m2,na.rm = TRUE),
                    NAbundance=sum(is.na(ab_m2_Mytilus_sp)==F)
)

###### Make Graph ########
 
 attach(mtcars)
 
jpeg('RPDAbundanceMytilusm2', height=1200, width=2400, res=400, qual=100)
 plot(FigureTableRPD$RPD,FigureTableRPD$MeanAbundance, main=" ", 
      xlab="RPD (cm) ", ylab="Abundance (Ind/m2) ", pch=19)
dev.off()

 ###Add fit lines
 abline(lm(FigureTableRPD$MeanAbundance~FigureTableRPD$RPD), col="red") # regression line (y~x) 


getwd()

