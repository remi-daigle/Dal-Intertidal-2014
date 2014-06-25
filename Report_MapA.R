###################### site map #########################

install.packages("marmap")

install.packages("maps")

install.packages("mapdata")

require(maps)

require(mapdata)


# setlat/long limits

xlim=c(-67,-62)

ylim=c(43,46)


#Lat and Long in a table

location_data <- matrix(c(44.5001, -63.9250, 44.6332, -63.5987, 44.6148, -65.6774, 45.1596, -64.3581, 43.948126, -64.820485),ncol=2,byrow=TRUE)

colnames(location_data) <- c("Latitude", "Longitude")

rownames(location_data) <- c("Cranberry Cove", "South Street", "Bear River", "Wolfville", "Summerville")

location_data <- as.data.frame.matrix(location_data) 


# plot basic map

map("worldHires", xlim=xlim, ylim=ylim, col="gray90", fill=TRUE, resolution=0)    # make base map

map.axes()                                                                        # add axes

map.scale(relwidth=0.5)                                                           # add scale bar proportioned to 50% width

points(location_data$Longitude,location_data$Latitude,pch=16,cex=1.5)  