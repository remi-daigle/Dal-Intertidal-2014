#Load Data
TotalData<- read.csv("~/Documents/GitHub/Dal-Intertidal-2014/Intertidal_Master_Data_Sheet_2014.csv")

# NA=0
TotalData[is.na(TotalData)]=0


# break up data
biol <- TotalData[,substr(names(TotalData),1,2)=="ab"|substr(names(TotalData),1,2)=="pc"]
phys <- TotalData[,9:21]

# convert biol counts to abundance
TotalData$Core_m3[TotalData$Core_m3==0] <- NA
TotalData$Quadrat_m2[TotalData$Quadrat_m2==0] <- NA


biol[,substr(names(biol),4,5)=="m2"]=biol[,substr(names(biol),4,5)=="m2"]/TotalData$Quadrat_m2
biol[,substr(names(biol),4,5)=="m3"]=biol[,substr(names(biol),4,5)=="m3"]/TotalData$Core_m3

# make vector of label
## TO DO: rename labels!
bio_names <- c("Mytilus spp. \n(Quadrat)","Semibalanus balanoides \n(Quadrat)","Littorina saxatilis \n(Quadrat)","Mya arenaria \n(Core)",
               "Mytilus spp. \n(Core)","Semibalanus balanoides \n(Core)","Polycaetes spp. \n(Quadrat)","Pagurus longicarpus \n(Core)",
               "Fucus spiralis \n(PC)","Fucus distichous drift \n(PC)","Fucus distichous attached \n(PC)","Fucus vesiculosus \n(PC)",
               "Ulva spp. \n(PC)","Glycera dibrachiata \n(Core)","Carcinus maenas \n(Quadrat)","Chondrus crispus \n(Quadrat)",
               "Asterias spp. \n(Core)","Hiatella artica \n(Quadrat)","Ceramium spp. \n(PC)","Acrosiphonia arcta \n(PC)",
               "Leatherista difformus \n(PC)","Nostoc cyanobacteria \n(PC)","Hildenbrandia spp. \n(PC)","Apohyale prevostii \n(Quadrat)",
               "Nephtys caeca \n(Core)","Chiridotea coeca \n(Core)","Chiridotea coeca \n(Quadrat)","Corophium volutato \n(Core)",
               "Corophium volutato \n(Quadrat)","Carcinus maenas \n(Core)","Littorina saxatilis \n(Core)")
## TO DO: rename labels!
phys_names <- c("Slope \n(Deg.)","SST \n(°C) ","Temp Buoy \n(°C)","Salinity \n(ppt)","Salt Buoy \n(ppt)","RPD \n(cm)","SWH \n(m)","Tidal Average \n(m)",
                "Tidal Range \n(m)","Grain size\n(<0.5 mm)","Grain size\n(0.5-1.7 mm)","Grain size\n(1.7-11.2 mm)","Grain size\n(>11.2 mm)")

## calculate means and plot
require(plyr)

for(sp in names(biol)){
  jpeg(paste("Biol vs phys/",sp,".jpg"), height=3600, width=2400, res=400, qual=100  )
  par(mfrow=c(5,3))
  for(ph in names(phys)){

    df=as.data.frame(cbind(x=phys[,names(phys)==ph],y=biol[,names(biol)==sp]))
    FigureTable<- ddply(df,.(x),summarize,
                        MeanAbundance=mean(y,na.rm = TRUE),
                        SDAbundance=sd(y,na.rm = TRUE),
                        NAbundance=sum(is.na(y)==F)
    )
    AB_mean <- FigureTable$MeanAbundance
    AB_se <- FigureTable$SDAbundance/sqrt(FigureTable$NAbundance)
    if(is.nan(max(AB_mean)*2)==F) mp <- barplot(FigureTable$MeanAbundance, names.arg=FigureTable$x, xlab=phys_names[names(phys)==ph], ylab= expression ("Abundance (Ind/m"^2*")"), ylim=c(0,max(AB_mean)*2))              # plots the barplot and saves the midpoints in mp
    if(is.nan(max(AB_mean)*2)==T) mp <- barplot(FigureTable$MeanAbundance, names.arg=FigureTable$x, xlab=phys_names[names(phys)==ph], ylab= expression ("Abundance (Ind/m"^2*")"), ylim=c(0,mean(AB_mean,na.rm=T)*10))              # plots the barplot and saves the midpoints in mp
    if(ph=="SST") title(main=bio_names[names(biol)==sp])
    segments(mp, AB_mean + AB_se, mp,AB_mean, lwd=2)  # plots positive error bar centered on mp
    segments(mp - 0.1, AB_mean + AB_se, mp + 0.1, AB_mean + AB_se, lwd=2)  #plots error bar caps
    if(ph=="X..11.2.mm") plot.new()
    if(ph=="X..11.2.mm.1") plot.new()
  }
  dev.off()
  
}

for(i in 1:500) dev.off()
