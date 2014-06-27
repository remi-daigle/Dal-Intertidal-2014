#import intertidal master data sheet

cdata <- read.csv(paste(wd,"Intertidal_Master_Data_Sheet_2014.csv",sep=""))

#abundance and species richness

species <- cdata[,substr(names(cdata),1,2)=="ab"|substr(names(cdata),1,2)=="pc"]

#make presence/absence matrix
PA <- as.data.frame(matrix(NA,nrow=length(unique(cdata$Site_name)),ncol=length(names(species))))
names(PA) <- names(species)
row.names(PA) <- unique(cdata$Site_name)
for(site in unique(cdata$Site_name)){
  for(sp in names(PA)){
    PA[unique(cdata$Site_name)==site,names(species)==sp] <- sum(is.na(species[cdata$Site_name==site,names(species)==sp])==F)>=1
  }
}

rich <- apply(PA,1,sum)

#next, calculate diversity
#using abundance (if it starts with ab) sum up each column

#install vegan to calculate diversity

#install.packages("vegan")
require(vegan)


species2 <- PA
for(site in unique(cdata$Site_name)){
  for(sp in names(species2)){
    species2[unique(cdata$Site_name)==site,names(species2)==sp] <- mean(species[unique(cdata$Site_name)==site,names(species2)==sp],na.rm=T)
  }
}

#create a dataframe that only had abundances
species <- species2[,substr(names(species2),1,2)=="ab"]


#made all nas = 0
species[is.na(species)] <- 0
cdata[is.na(cdata)] <- 0

#runs diversity
div <- diversity(species, "shannon")

# make data frame
df <- as.data.frame(cbind(div=div,rich=rich))
df$Site <- row.names(df)


# merge with main dataframe
cdata2 <- merge(cdata,df,by.x="Site_name",by.y="Site")

# break up data
phys <- cdata2[,9:21]


# make vector of label
## TO DO: rename labels!
phys_names <- c("Slope","SST","Temp_Buoy","Salinity","Salt_Buoy","RPD","SWH","Tidal_Average",
                "Tidal_Range","X..0.5.mm","X..1.7.mm","X..11.2.mm","X..11.2.mm.1")

## calculate means and plot
require(plyr)

# for richness
for(ph in names(phys)){
    df=as.data.frame(cbind(x=phys[,names(phys)==ph],y=cdata2$rich))
    FigureTable<- ddply(df,.(x),summarize,
                        MeanAbundance=mean(y,na.rm = TRUE),
                        SDAbundance=sd(y,na.rm = TRUE),
                        NAbundance=sum(is.na(y)==F)
    )
    AB_mean <- FigureTable$MeanAbundance
    AB_se <- FigureTable$SDAbundance/sqrt(FigureTable$NAbundance)
    jpeg(paste("Richness_",ph,".jpg"), height=1200, width=2400, res=400, qual=100  )
    if(is.nan(max(AB_mean)*2)==F) mp <- barplot(FigureTable$MeanAbundance, names.arg=FigureTable$x, xlab=phys_names[names(phys)==ph], ylab= "Richness",ylim=c(0,max(AB_mean)*2))              # plots the barplot and saves the midpoints in mp
    if(is.nan(max(AB_mean)*2)==T) mp <- barplot(FigureTable$MeanAbundance, names.arg=FigureTable$x, xlab=phys_names[names(phys)==ph], ylab= "Richness",ylim=c(0,1))              # plots the barplot and saves the midpoints in mp
    segments(mp, AB_mean + AB_se, mp,AB_mean, lwd=2)  # plots positive error bar centered on mp
    segments(mp - 0.1, AB_mean + AB_se, mp + 0.1, AB_mean + AB_se, lwd=2)  #plots error bar caps
    dev.off()
}

#for diversity
for(ph in names(phys)){
  df=as.data.frame(cbind(x=phys[,names(phys)==ph],y=cdata2$div))
  FigureTable<- ddply(df,.(x),summarize,
                      MeanAbundance=mean(y,na.rm = TRUE),
                      SDAbundance=sd(y,na.rm = TRUE),
                      NAbundance=sum(is.na(y)==F)
  )
  AB_mean <- FigureTable$MeanAbundance
  AB_se <- FigureTable$SDAbundance/sqrt(FigureTable$NAbundance)
  jpeg(paste("Biodiversity_",ph,".jpg"), height=1200, width=2400, res=400, qual=100  )
  if(is.nan(max(AB_mean)*2)==F) mp <- barplot(FigureTable$MeanAbundance, names.arg=FigureTable$x, xlab=phys_names[names(phys)==ph], ylab= "Biodiversity",ylim=c(0,max(AB_mean)*2))              # plots the barplot and saves the midpoints in mp
  if(is.nan(max(AB_mean)*2)==T) mp <- barplot(FigureTable$MeanAbundance, names.arg=FigureTable$x, xlab=phys_names[names(phys)==ph], ylab= "Biodiversity",ylim=c(0,1))              # plots the barplot and saves the midpoints in mp
  segments(mp, AB_mean + AB_se, mp,AB_mean, lwd=2)  # plots positive error bar centered on mp
  segments(mp - 0.1, AB_mean + AB_se, mp + 0.1, AB_mean + AB_se, lwd=2)  #plots error bar caps
  dev.off()
}
