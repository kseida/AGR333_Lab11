install.packages("sp")
install.packages("terra")
install.packages("raster")
install.packages("gstat")
install.packages("animation")
install.packages("RColorBrewer")
install.packages("dplyr")
library(sp)
library(terra)
library(raster)
library(gstat)
library(animation)
library(RColorBrewer)
library(dplyr)

# load DEM
DEM = rast('Shale_Hills_DEM.tif')

# load shapefile
ws = vect('Watershed_Boundary')

# plot DEm and watershed on top of it
plot(DEM, xlab = "northing (m)", ylab = "easting (m)", main = "shale hills elevation (m)")
plot(ws, add = T)

# read soil moisture data
sm <- read.csv("SM_10cm_2010.csv")

# REMOVE STATION IDS AND CONVERT TO SPATIAL
sm = sm %>%
  select(-SID)
coordinates(sm) = ~ X+Y

# add soil moisture to plot
plot(sm, add = T, pch = 16)

# add legend
par(xpd = FALSE) # Allow legend to be drawn outside the plot area if needed
legend('topleft', legend = 'Soil Moisture Station', pch = 16)

# save color palette
display.brewer.pal(7, 'YlGnBu')
col.pal <- brewer.pal(7, 'YlGnBu')

# assign colors to moisture values
intervals <- seq(min(sm@data[,1]), max(sm@data[,1]),length=8)
intervals <- quantile(sm@data[,1], seq(0,1,length=8), na.rm = T)
idx.col <- cut(sm@data[,1], intervals, include.lowest = T)
col.pal[idx.col]

# add colors to plot
plot(DEM, col = brewer.pal(9, 'Purples'),
     xlab = 'Easting [m]', ylab = 'Northing [m]', main = 'Shale Hills Elevation [m]')
plot(sm, pch = 16, col=col.pal[idx.col], add = T)
plot(ws, add=T)
leg <- paste0(round(intervals[-8], digits = 2), '-',
              round(intervals[-1], digits = 2))
par(xpd = TRUE)
legend('bottomright', legend = leg, pch = 16, col = col.pal , title = 'Soil Moisture [%]')
par(xpd = FALSE)


########## ANIMATING TIME SERIES ###########
intervals <- quantile(sm@data[,2], seq(0,1,length=8), na.rm = T)
idx.col <- cut(sm@data[,2], intervals, include.lowest = T)
col.pal[idx.col]
plot(DEM, col = brewer.pal(9, 'Purples'),
     xlab = 'Easting [m]', ylab = 'Northing [m]', main = 'Shale Hills Elevation [m]')
plot(sm, pch = 16, col=col.pal[idx.col], add = T)
plot(ws, add=T)
leg <- paste0(round(intervals[-8], digits = 2), '-',
              round(intervals[-1], digits = 2))
par(xpd = TRUE)
legend('bottomright', legend = leg, pch = 16, col = col.pal , title = 'Soil Moisture [%]')
par(xpd = FALSE)

#for loop
x <- 1
for(i in 1:10){
  x <- x+1
}

# set animation time
ani.options(interval = .3) # It sets the time interval (in seconds) between animation frames.
# In this case, it makes the animation pause for 0.3 seconds before displaying the next frame

for(i in 1:ncol(sm@data)){
  intervals <- quantile(sm@data[,i], seq(0,1,length=8), na.rm = T)
  intervals
  idx.col <- cut(sm@data[,i], intervals, include.lowest = T)
  idx.col
  col.pal[idx.col]
  plot(DEM, col = brewer.pal(9, 'Purples'),
       xlab = 'Easting [m]', ylab = 'Northing [m]', main = 'Shale Hills Elevation [m]')
  plot(sm, pch = 16, col=col.pal[idx.col], add = T)
  plot(ws, add=T)
  leg <- paste0(round(intervals[-8], digits = 2), '-',
                round(intervals[-1], digits = 2))
  par(xpd = TRUE)
  legend('bottomright', legend = leg, pch = 16, col = col.pal , title = 'Soil Moisture [%]')
  par(xpd = FALSE)
  ani.pause() # Pause for animation effect
}

ppt <- read.csv("SHCZO_ppt_2010.csv")

# for loop w/ precip

par(mfrow = c(2, 1), mar = c(4, 4, 3, 1))

for(i in 1:ncol(sm@data)){
  intervals <- quantile(sm@data[,i], seq(0,1,length=8), na.rm = T)
  intervals
  idx.col <- cut(sm@data[,i], intervals, include.lowest = T)
  idx.col
  col.pal[idx.col]
  plot(DEM, col = brewer.pal(9, 'Purples'),
       xlab = 'Easting [m]', ylab = 'Northing [m]', main = 'Shale Hills Elevation [m]')
  plot(sm, pch = 16, col=col.pal[idx.col], add = T)
  plot(ws, add=T)
  leg <- paste0(round(intervals[-8], digits = 2), '-',
                round(intervals[-1], digits = 2))
  par(xpd = TRUE)
  legend('bottomright', legend = leg, pch = 16, col = col.pal , title = 'Soil Moisture [%]')
  par(xpd = FALSE)
  barplot(height = ppt$Total_Precip_mm, names = ppt$TmStamp, 
          xlab = "date", ylab = "precipitation (mm)")
  idx.v <- which(ppt$TmStamp == gsub('X', '', colnames(sm@data)[i]))
  abline(v = idx.v, col = 'red') 
  
  ani.pause() # Pause for animation effect
}
