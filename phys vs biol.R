#Load Data
TotalData<- read.csv("~/Documents/GitHub/Dal-Intertidal-2014/Intertidal_Master_Data_Sheet_2014.csv")

# NA=0
TotalData[is.na(TotalData)]=0


# break up data
biol <- TotalData[,substr(names(TotalData),1,2)=="ab"|substr(names(TotalData),1,2)=="pc"]
phys <- TotalData[,9:21]

# convert biol counts to abundance
biol[,substr(names(biol),4,5)=="m2"]=biol[,substr(names(biol),4,5)=="m2"]/TotalData$Quadrat_m2
biol[,substr(names(biol),4,5)=="m3"]=biol[,substr(names(biol),4,5)=="m3"]/TotalData$Core_m3

# make vector of label
## TO DO: rename labels!
bio_names <- c("ab_m2_Mytilus_sp","ab_m2_Semibalanus_balanoides","ab_m2_Littorina_saxatilis","ab_m3_Mya_arenaria",
               "ab_m3_Mytilus_sp","ab_m3_Semibalanus_balanoides","ab_m2_Polycaetes","ab_m3_Pagurus_longicarpus",
               "pc_Fucus_spiralis","pc_Fucus_distichous_drift","pc_Fucus_distichous_attached","pc_Fucus_vesiculosus",
               "pc_Ulva_.something...","ab_m3_Glycera_dibrachiata","ab_m2_Carcinus_maenas","ab_m2_Chondrus.crispus",
               "ab_m3_Asterias.spp.","ab_m2_Hiatella.artica","ab_Ceramium_spp","pc_Acrosiphonia_arcta",
               "pc_Leatherista_difformus","pc_Nostoc_cyanobacteria","pc_Hildenbrandia_spp.","ab_m2_Apohyale_prevostii",
               "ab_m3_nephtys_caeca","ab_m3_Chiridotea_coeca","ab_m2_Chiridotea_coeca","ab_m3_Corophium.volutato",
               "ab_m2_Corophium.volutato","ab_m3_Carcinus_maenas","ab_m3_Littorina_saxatilis")
## TO DO: rename labels!
phys_names <- c("Slope","SST","Temp_Buoy","Salinity","Salt_Buoy","RPD","SWH","Tidal_Average",
                "Tidal_Range","X..0.5.mm","X..1.7.mm","X..11.2.mm","X..11.2.mm.1")

## calculate means and plot
require(plyr)

for(sp in names(biol)){
  for(ph in names(phys)){
    df=as.data.frame(cbind(x=phys[,names(phys)==ph],y=biol[,names(biol)==sp]))
    FigureTable<- ddply(df,.(x),summarize,
                        MeanAbundance=mean(y,na.rm = TRUE),
                        SDAbundance=sd(y,na.rm = TRUE),
                        NAbundance=sum(is.na(y)==F)
    )
    AB_mean <- FigureTable$MeanAbundance
    AB_se <- FigureTable$SDAbundance/sqrt(FigureTable$NAbundance)
    jpeg(paste(sp,"_",ph,".jpg"), height=1200, width=2400, res=400, qual=100  )
    if(is.nan(max(AB_mean)*2)==F) mp <- barplot(FigureTable$MeanAbundance, names.arg=FigureTable$x, xlab=phys_names[names(phys)==ph], ylab= expression ("Abundance (Ind/m"^2*")"), main=bio_names[names(biol)==sp],ylim=c(0,max(AB_mean)*2))              # plots the barplot and saves the midpoints in mp
    if(is.nan(max(AB_mean)*2)==T) mp <- barplot(FigureTable$MeanAbundance, names.arg=FigureTable$x, xlab=phys_names[names(phys)==ph], ylab= expression ("Abundance (Ind/m"^2*")"), main=bio_names[names(biol)==sp],ylim=c(0,1))              # plots the barplot and saves the midpoints in mp
    segments(mp, AB_mean + AB_se, mp,AB_mean, lwd=2)  # plots positive error bar centered on mp
    segments(mp - 0.1, AB_mean + AB_se, mp + 0.1, AB_mean + AB_se, lwd=2)  #plots error bar caps
    dev.off()
  }
}

