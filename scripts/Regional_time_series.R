# This script extracts the climdex indices value from 2021-2100 and display them as a time series for each of the 
# 6 districts of Western Cape

library(sf)
library(ncdf4)
library(raster)
library(rgdal)
library(ggplot2)
library(stars)
library(dplyr)
library(tidyverse)
library(zoo)
library(ggplot2)


# ----------------------shapefile-----------------------------------
setwd("~/Desktop/extreme-events-project-WesternCape/data/Study_Area_Shapefiles")
Cape_Winelands <- readOGR("Cape_Winelands.shp")
plot(Cape_Winelands)

Central_Karoo <- readOGR("Central_Karoo.shp")
plot(Central_Karoo)

City_Cape_Town <- readOGR("City_Cape_Town.shp")
plot(City_Cape_Town)

Eden <- readOGR("Eden.shp")
plot(Eden)

Overberg <- readOGR("Overberg.shp")
plot(Overberg)

West_Coast <- readOGR("West_Coast.shp")
plot(West_Coast)

#------------------------ RCP 2.6-----------------------------------
##-------------------- CapeWinelands--------------------------------
###------------------------HWA--------------------------------------
setwd("~/Desktop/extreme-events-project-WesternCape/data/rcp26/output")
HWA_rcp26 <- brick('hw_ANN_climpact.sample_historical_NA_2021-2100.nc', varname = 'hwa_tx90')
lon <- ncvar_get(HW, "lon")
lat <- ncvar_get(HW, "lat", verbose = F)
#plot(HWA)
# Crop the raster-CapeWinelands
HWA_Cape_Winelands_mask_rcp26 <- mask(HWA_rcp26, Cape_Winelands) %>%
  as.matrix() 

HWA_Cape_Winelands_mask_rcp26 <- HWA_Cape_Winelands_mask_rcp26[rowSums(is.na(HWA_Cape_Winelands_mask_rcp26)) != ncol(HWA_Cape_Winelands_mask_rcp26), ]
HWA_Cape_Winelands_mask_rcp26 <-   apply(HWA_Cape_Winelands_mask_rcp26, 1, na.approx)
HWA_Cape_Winelands_mask_rcp26 <- data.frame(Reduce(rbind, HWA_Cape_Winelands_mask_rcp26))
HWA_Cape_Winelands_mask_rcp26$X80 <- 2*HWA_Cape_Winelands_mask_rcp26$'X79' - HWA_Cape_Winelands_mask_rcp26$'X78'
colnames(HWA_Cape_Winelands_mask_rcp26) <- as.character(c(2021:2100))
row.names(HWA_Cape_Winelands_mask_rcp26) <- c("grid1", "grid2", "grid3", "grid4", "grid5", "grid6", "grid7", "grid8", "grid9", "grid10", "grid11")
HWA_Cape_Winelands_mask_rcp26 <- rownames_to_column(HWA_Cape_Winelands_mask_rcp26, var="grid")
HWA_Cape_Winelands_mask_rcp26 <- pivot_longer(HWA_Cape_Winelands_mask_rcp26, cols = !grid, names_to = 'year', values_to = 'hwa')
  
# plot the HWA in CapeWinelands

ggplot(HWA_Cape_Winelands_mask_rcp26, aes(x = year, y = hwa)) +
 geom_boxplot(color = 'red', fill='orange', alpha = 0.2) +
 ggtitle("CapeWinelands HWA (RCP 2.6)") + xlab("Year") + ylab("HWA (°C)") +
 theme_classic() + 
 theme(plot.title = element_text(size=20, face="bold", hjust = 0.5), axis.title.x = element_text(size=20, face="bold" ), axis.title.y = element_text(size=20, face="bold"),axis.text.y = element_text(size = 20))

# geom_line(aes(x=year, y=hwa, color=grid, group = 11)) + geom_point() +  theme_test()

###---------------------------HWD--------------------------------------
setwd("~/Desktop/extreme-events-project-WesternCape/data/rcp26/output")
HWD_rcp26 <- brick('hw_ANN_climpact.sample_historical_NA_2021-2100.nc', varname = 'hwd_tx90')

# Crop the raster-CapeWinelands
HWD_Cape_Winelands_mask_rcp26 <- mask(HWD_rcp26, Cape_Winelands) %>%
  as.matrix() 

HWD_Cape_Winelands_mask_rcp26 <- HWD_Cape_Winelands_mask_rcp26[rowSums(is.na(HWD_Cape_Winelands_mask_rcp26)) != ncol(HWD_Cape_Winelands_mask_rcp26), ]
HWD_Cape_Winelands_mask_rcp26 <-   apply(HWD_Cape_Winelands_mask_rcp26, 1, na.approx)
HWD_Cape_Winelands_mask_rcp26 <- data.frame(Reduce(rbind, HWD_Cape_Winelands_mask_rcp26))
HWD_Cape_Winelands_mask_rcp26$X80 <- 3*HWD_Cape_Winelands_mask_rcp26$'X79' - HWD_Cape_Winelands_mask_rcp26$'X78'
colnames(HWD_Cape_Winelands_mask_rcp26) <- as.character(c(2021:2100))
row.names(HWD_Cape_Winelands_mask_rcp26) <- c("grid1", "grid2", "grid3", "grid4", "grid5", "grid6", "grid7", "grid8", "grid9", "grid10", "grid11")
HWD_Cape_Winelands_mask_rcp26 <- rownames_to_column(HWD_Cape_Winelands_mask_rcp26, var="grid")
HWD_Cape_Winelands_mask_rcp26 <- pivot_longer(HWD_Cape_Winelands_mask_rcp26, cols = !grid, names_to = 'year', values_to = 'hwd')

# plot the HWD in CapeWinelands

ggplot(HWD_Cape_Winelands_mask_rcp26, aes(x = year, y = hwd)) +
  geom_boxplot(color = 'blue', fill='blue', alpha = 0.2) +
  ggtitle("CapeWinelands HWD (RCP 2.6)") + xlab("Year") + ylab("HWD (days)") +
  theme_classic() + 
  theme(plot.title = element_text(size=20, face="bold", hjust = 0.5), axis.title.x = element_text(size=20, face="bold" ), axis.title.y = element_text(size=20, face="bold"),axis.text.y = element_text(size = 20))

###----------------------------HWF--------------------------------------
setwd("~/Desktop/extreme-events-project-WesternCape/data/rcp26/output")
HWF_rcp26 <- brick('hw_ANN_climpact.sample_historical_NA_2021-2100.nc', varname = 'hwf_tx90')

# Crop the raster-CapeWinelands
HWF_Cape_Winelands_mask_rcp26 <- mask(HWF_rcp26, Cape_Winelands) %>%
  as.matrix() 

HWF_Cape_Winelands_mask_rcp26 <- HWF_Cape_Winelands_mask_rcp26[rowSums(is.na(HWF_Cape_Winelands_mask_rcp26)) != ncol(HWF_Cape_Winelands_mask_rcp26), ]
HWF_Cape_Winelands_mask_rcp26[,"X2100.07.02"] <- HWF_Cape_Winelands_mask_rcp26[,"X2098.07.02"] - HWF_Cape_Winelands_mask_rcp26[,"X2099.07.02"]
HWF_Cape_Winelands_mask_rcp26 <- t(apply(HWF_Cape_Winelands_mask_rcp26, 1, na.approx))

colnames(HWF_Cape_Winelands_mask_rcp26) <- as.character(c(2021:2100))
row.names(HWF_Cape_Winelands_mask_rcp26) <- c("grid1", "grid2", "grid3", "grid4", "grid5", "grid6", "grid7", "grid8", "grid9", "grid10", "grid11")

HWF_Cape_Winelands_mask_rcp26 <- rownames_to_column(as.data.frame(HWF_Cape_Winelands_mask_rcp26), var="grid")
HWF_Cape_Winelands_mask_rcp26 <- pivot_longer(HWF_Cape_Winelands_mask_rcp26, cols = !grid, names_to = 'year', values_to = 'hwf')

# plot the HWF in CapeWinelands

ggplot(HWF_Cape_Winelands_mask_rcp26, aes(x = year, y = hwf)) +
  geom_boxplot(color = 'purple', fill='purple', alpha = 0.2) +
  ggtitle("CapeWinelands HWF (RCP 2.6)") + xlab("Year") + ylab("HWF (days)") +
  theme_classic() + 
  theme(plot.title = element_text(size=20, face="bold", hjust = 0.5), axis.title.x = element_text(size=20, face="bold" ), axis.title.y = element_text(size=20, face="bold"),axis.text.y = element_text(size = 20))

###----------------------------HWM--------------------------------------
setwd("~/Desktop/extreme-events-project-WesternCape/data/rcp26/output")
HWM_rcp26 <- brick('hw_ANN_climpact.sample_historical_NA_2021-2100.nc', varname = 'hwm_tx90')

# Crop the raster-CapeWinelands
HWM_Cape_Winelands_mask_rcp26 <- mask(HWM_rcp26, Cape_Winelands) %>%
  as.matrix() 

HWM_Cape_Winelands_mask_rcp26 <- HWM_Cape_Winelands_mask_rcp26[rowSums(is.na(HWM_Cape_Winelands_mask_rcp26)) != ncol(HWM_Cape_Winelands_mask_rcp26), ]
HWM_Cape_Winelands_mask_rcp26 <-  apply(HWM_Cape_Winelands_mask_rcp26, 1, na.approx)
HWM_Cape_Winelands_mask_rcp26 <- data.frame(Reduce(rbind, HWM_Cape_Winelands_mask_rcp26))
HWM_Cape_Winelands_mask_rcp26$X80 <- 2*HWM_Cape_Winelands_mask_rcp26$'X79' - HWM_Cape_Winelands_mask_rcp26$'X78'

colnames(HWM_Cape_Winelands_mask_rcp26) <- as.character(c(2021:2100))
row.names(HWM_Cape_Winelands_mask_rcp26) <- c("grid1", "grid2", "grid3", "grid4", "grid5", "grid6", "grid7", "grid8", "grid9", "grid10", "grid11")
HWM_Cape_Winelands_mask_rcp26 <- rownames_to_column(HWM_Cape_Winelands_mask_rcp26, var="grid")
HWM_Cape_Winelands_mask_rcp26 <- pivot_longer(HWM_Cape_Winelands_mask_rcp26, cols = !grid, names_to = 'year', values_to = 'hwm')

# plot the HWD in CapeWinelands

ggplot(HWM_Cape_Winelands_mask_rcp26, aes(x = year, y = hwm)) +
  geom_boxplot(color = 'orange', fill='orange', alpha = 0.2) +
  ggtitle("CapeWinelands HWM (RCP 2.6)") + xlab("Year") + ylab("HWM (°C)") +
  theme_classic() + 
  theme(plot.title = element_text(size=20, face="bold", hjust = 0.5), axis.title.x = element_text(size=20, face="bold" ), axis.title.y = element_text(size=20, face="bold"),axis.text.y = element_text(size = 20))

###----------------------------HWN--------------------------------------
setwd("~/Desktop/extreme-events-project-WesternCape/data/rcp26/output")
HWN_rcp26 <- brick('hw_ANN_climpact.sample_historical_NA_2021-2100.nc', varname = 'hwn_tx90')

# Crop the raster-CapeWinelands
HWN_Cape_Winelands_mask_rcp26 <- mask(HWN_rcp26, Cape_Winelands) %>%
  as.matrix() 

HWN_Cape_Winelands_mask_rcp26 <- HWN_Cape_Winelands_mask_rcp26[rowSums(is.na(HWN_Cape_Winelands_mask_rcp26)) != ncol(HWN_Cape_Winelands_mask_rcp26), ]
HWN_Cape_Winelands_mask_rcp26[,"X2100.07.02"] <- HWN_Cape_Winelands_mask_rcp26[,"X2098.07.02"] - HWN_Cape_Winelands_mask_rcp26[,"X2099.07.02"]
HWN_Cape_Winelands_mask_rcp26 <- t(apply(HWN_Cape_Winelands_mask_rcp26, 1, na.approx))
#HWN_Cape_Winelands_mask <- data.frame(Reduce(rbind, HWM_Cape_Winelands_mask))
#HWN_Cape_Winelands_mask$X80 <- 2*HWM_Cape_Winelands_mask$'X79' - HWM_Cape_Winelands_mask$'X78'
colnames(HWN_Cape_Winelands_mask_rcp26) <- as.character(c(2021:2100))
row.names(HWN_Cape_Winelands_mask_rcp26) <- c("grid1", "grid2", "grid3", "grid4", "grid5", "grid6", "grid7", "grid8", "grid9", "grid10", "grid11")

HWN_Cape_Winelands_mask_rcp26 <- rownames_to_column(as.data.frame(HWN_Cape_Winelands_mask_rcp26), var="grid")
HWN_Cape_Winelands_mask_rcp26 <- pivot_longer(HWN_Cape_Winelands_mask_rcp26, cols = !grid, names_to = 'year', values_to = 'hwn')

# plot the HWN in CapeWinelands

ggplot(HWN_Cape_Winelands_mask_rcp26, aes(x = year, y = hwn)) +
  geom_boxplot(color = 'pink', fill='pink', alpha = 0.2) +
  ggtitle("CapeWinelands HWN (RCP 2.6)") + xlab("Year") + ylab("HWN (events)") +
  theme_classic() + 
  theme(plot.title = element_text(size=20, face="bold", hjust = 0.5), axis.title.x = element_text(size=20, face="bold" ), axis.title.y = element_text(size=20, face="bold"),axis.text.y = element_text(size = 20))

##-------------------- Central Karoo-----------------------------------
###------------------------HWA--------------------------------------
setwd("~/Desktop/extreme-events-project-WesternCape/data/rcp26/output")

HWA_rcp26 <- brick('hw_ANN_climpact.sample_historical_NA_2021-2100.nc', varname = 'hwa_tx90')

# Crop the raster-CapeWinelands
HWA_Central_Karoo_mask_rcp26 <- mask(HWA_rcp26, Central_Karoo) %>%
  as.matrix() 

HWA_Central_Karoo_mask_rcp26 <- HWA_Central_Karoo_mask_rcp26[rowSums(is.na(HWA_Central_Karoo_mask_rcp26)) != ncol(HWA_Central_Karoo_mask_rcp26), ]
HWA_Central_Karoo_mask_rcp26[,"X2100.07.02"] <-  HWA_Central_Karoo_mask_rcp26[,"X2098.07.02"]
HWA_Central_Karoo_mask_rcp26 <- t(apply(HWA_Central_Karoo_mask_rcp26, 1, na.approx))
#HWA_Central_Karoo_mask_rcp26 <- data.frame(Reduce(rbind, HWA_Central_Karoo_mask_rcp26))
colnames(HWA_Central_Karoo_mask_rcp26) <- as.character(c(2021:2100))
row.names(HWA_Central_Karoo_mask_rcp26) <- c("grid1", "grid2", "grid3", "grid4", "grid5", "grid6", "grid7", "grid8", "grid9", "grid10", "grid11", "grid12", "grid13", "grid14", "grid15", "grid16")

HWA_Central_Karoo_mask_rcp26 <- rownames_to_column(as.data.frame(HWA_Central_Karoo_mask_rcp26), var="grid")
HWA_Central_Karoo_mask_rcp26 <- pivot_longer(HWA_Central_Karoo_mask_rcp26, cols = !grid, names_to = 'year', values_to = 'hwa')
# plot the HWA in CapeWinelands

ggplot(HWA_Central_Karoo_mask_rcp26, aes(x = year, y = hwa)) +
  geom_boxplot(color = 'red', fill='orange', alpha = 0.2) +
  ggtitle("Central Karoo HWA (RCP 2.6)") + xlab("Year") + ylab("HWA (°C)") +
  theme_classic() +
  theme(plot.title = element_text(size=20, face="bold", hjust = 0.5), axis.title.x = element_text(size=20, face="bold" ), axis.title.y = element_text(size=20, face="bold"), axis.text.y = element_text(size = 20))

###------------------------HWD--------------------------------------
setwd("~/Desktop/extreme-events-project-WesternCape/data/rcp26/output")

HWD_rcp26 <- brick('hw_ANN_climpact.sample_historical_NA_2021-2100.nc', varname = 'hwd_tx90')

# Crop the raster-CapeWinelands
HWD_Central_Karoo_mask_rcp26 <- mask(HWD_rcp26, Central_Karoo) %>%
  as.matrix() 

HWD_Central_Karoo_mask_rcp26 <- HWD_Central_Karoo_mask_rcp26[rowSums(is.na(HWD_Central_Karoo_mask_rcp26)) != ncol(HWD_Central_Karoo_mask_rcp26), ]
HWD_Central_Karoo_mask_rcp26[,"X2099.07.02"] <- 2*HWD_Central_Karoo_mask_rcp26[,"X2098.07.02"] - HWD_Central_Karoo_mask_rcp26[,"X2097.07.02"]
HWD_Central_Karoo_mask_rcp26[,"X2100.07.02"] <- 2*HWD_Central_Karoo_mask_rcp26[,"X2099.07.02"] - HWD_Central_Karoo_mask_rcp26[,"X2098.07.02"]
HWD_Central_Karoo_mask_rcp26 <- t(apply(HWD_Central_Karoo_mask_rcp26, 1, na.approx))
HWD_Central_Karoo_mask_rcp26 <- data.frame(Reduce(rbind, HWD_Central_Karoo_mask_rcp26))

colnames(HWD_Central_Karoo_mask_rcp26) <- as.character(c(2021:2100))
row.names(HWD_Central_Karoo_mask_rcp26) <- c("grid1", "grid2", "grid3", "grid4", "grid5", "grid6", "grid7", "grid8", "grid9", "grid10", "grid11", "grid12", "grid13", "grid14", "grid15", "grid16")

HWD_Central_Karoo_mask_rcp26 <- rownames_to_column(as.data.frame(HWD_Central_Karoo_mask_rcp26), var="grid")
HWD_Central_Karoo_mask_rcp26 <- pivot_longer(HWD_Central_Karoo_mask_rcp26, cols = !grid, names_to = 'year', values_to = 'hwd')
# plot the HWA in CapeWinelands

ggplot(HWD_Central_Karoo_mask_rcp26, aes(x = year, y = hwd)) +
  geom_boxplot(color = 'blue', fill='blue', alpha = 0.2) +
  ggtitle("Central Karoo HWD (RCP 2.6)") + xlab("Year") + ylab("HWD (days)") +
  theme_classic() +
  theme(plot.title = element_text(size=20, face="bold", hjust = 0.5), axis.title.x = element_text(size=20, face="bold" ), axis.title.y = element_text(size=20, face="bold"), axis.text.y = element_text(size = 20))

###------------------------HWF--------------------------------------
setwd("~/Desktop/extreme-events-project-WesternCape/data/rcp26/output")

HWF_rcp26 <- brick('hw_ANN_climpact.sample_historical_NA_2021-2100.nc', varname = 'hwf_tx90')

# Crop the raster-CapeWinelands
HWF_Central_Karoo_mask_rcp26 <- mask(HWF_rcp26, Central_Karoo) %>%
  as.matrix() 

HWF_Central_Karoo_mask_rcp26 <- HWF_Central_Karoo_mask_rcp26[rowSums(is.na(HWF_Central_Karoo_mask_rcp26)) != ncol(HWF_Central_Karoo_mask_rcp26), ]
HWF_Central_Karoo_mask_rcp26[,"X2100.07.02"] <- HWF_Central_Karoo_mask_rcp26[,"X2098.07.02"] - HWF_Central_Karoo_mask_rcp26[,"X2099.07.02"]
HWF_Central_Karoo_mask_rcp26 <- t(apply(HWF_Central_Karoo_mask_rcp26, 1, na.approx))
#HWF_Central_Karoo_mask_rcp26 <- data.frame(Reduce(rbind, HWF_Central_Karoo_mask_rcp26))

colnames(HWF_Central_Karoo_mask_rcp26) <- as.character(c(2021:2100))
row.names(HWF_Central_Karoo_mask_rcp26) <- c("grid1", "grid2", "grid3", "grid4", "grid5", "grid6", "grid7", "grid8", "grid9", "grid10", "grid11", "grid12", "grid13", "grid14", "grid15", "grid16")

HWF_Central_Karoo_mask_rcp26 <- rownames_to_column(as.data.frame(HWF_Central_Karoo_mask_rcp26), var="grid")
HWF_Central_Karoo_mask_rcp26 <- pivot_longer(HWF_Central_Karoo_mask_rcp26, cols = !grid, names_to = 'year', values_to = 'hwf')
# plot the HWA in CapeWinelands

ggplot(HWF_Central_Karoo_mask_rcp26, aes(x = year, y = hwf)) +
  geom_boxplot(color = 'purple', fill='purple', alpha = 0.2) +
  ggtitle("Central Karoo HWF (RCP 2.6)") + xlab("Year") + ylab("HWF (days)") +
  theme_classic() +
  theme(plot.title = element_text(size=20, face="bold", hjust = 0.5), axis.title.x = element_text(size=20, face="bold" ), axis.title.y = element_text(size=20, face="bold"), axis.text.y = element_text(size = 20))

###------------------------HWM--------------------------------------
setwd("~/Desktop/extreme-events-project-WesternCape/data/rcp26/output")

HWM_rcp26 <- brick('hw_ANN_climpact.sample_historical_NA_2021-2100.nc', varname = 'hwm_tx90')

# Crop the raster-CapeWinelands
HWM_Central_Karoo_mask_rcp26 <- mask(HWM_rcp26, Central_Karoo) %>%
  as.matrix() 

HWM_Central_Karoo_mask_rcp26 <- HWM_Central_Karoo_mask_rcp26[rowSums(is.na(HWM_Central_Karoo_mask_rcp26)) != ncol(HWM_Central_Karoo_mask_rcp26), ]
HWM_Central_Karoo_mask_rcp26[,"X2100.07.02"] <- HWM_Central_Karoo_mask_rcp26[,"X2098.07.02"] 
HWM_Central_Karoo_mask_rcp26 <- t(apply(HWM_Central_Karoo_mask_rcp26, 1, na.approx))

colnames(HWM_Central_Karoo_mask_rcp26) <- as.character(c(2021:2100))
row.names(HWM_Central_Karoo_mask_rcp26) <- c("grid1", "grid2", "grid3", "grid4", "grid5", "grid6", "grid7", "grid8", "grid9", "grid10", "grid11", "grid12", "grid13", "grid14", "grid15", "grid16")

HWM_Central_Karoo_mask_rcp26 <- rownames_to_column(as.data.frame(HWM_Central_Karoo_mask_rcp26), var="grid")
HWM_Central_Karoo_mask_rcp26 <- pivot_longer(HWM_Central_Karoo_mask_rcp26, cols = !grid, names_to = 'year', values_to = 'hwm')
# plot the HWA in CapeWinelands

ggplot(HWM_Central_Karoo_mask_rcp26, aes(x = year, y = hwm)) +
  geom_boxplot(color = 'orange', fill='orange', alpha = 0.2) +
  ggtitle("Central Karoo HWM (RCP 2.6)") + xlab("Year") + ylab("HWM (°C)") +
  theme_classic() +
  theme(plot.title = element_text(size=20, face="bold", hjust = 0.5), axis.title.x = element_text(size=20, face="bold" ), axis.title.y = element_text(size=20, face="bold"), axis.text.y = element_text(size = 20))

###------------------------HWN--------------------------------------
setwd("~/Desktop/extreme-events-project-WesternCape/data/rcp26/output")

HWN_rcp26 <- brick('hw_ANN_climpact.sample_historical_NA_2021-2100.nc', varname = 'hwn_tx90')

# Crop the raster-CapeWinelands
HWN_Central_Karoo_mask_rcp26 <- mask(HWN_rcp26, Central_Karoo) %>%
  as.matrix() 

HWN_Central_Karoo_mask_rcp26 <- HWN_Central_Karoo_mask_rcp26[rowSums(is.na(HWN_Central_Karoo_mask_rcp26)) != ncol(HWN_Central_Karoo_mask_rcp26), ]
HWN_Central_Karoo_mask_rcp26[,"X2100.07.02"] <- HWN_Central_Karoo_mask_rcp26[,"X2098.07.02"] - HWN_Central_Karoo_mask_rcp26[,"X2099.07.02"]
HWN_Central_Karoo_mask_rcp26 <- t(apply(HWN_Central_Karoo_mask_rcp26, 1, na.approx))
#HWN_Central_Karoo_mask_rcp26 <- data.frame(Reduce(rbind, HWN_Central_Karoo_mask_rcp26))

colnames(HWN_Central_Karoo_mask_rcp26) <- as.character(c(2021:2100))
row.names(HWN_Central_Karoo_mask_rcp26) <- c("grid1", "grid2", "grid3", "grid4", "grid5", "grid6", "grid7", "grid8", "grid9", "grid10", "grid11", "grid12", "grid13", "grid14", "grid15", "grid16")

HWN_Central_Karoo_mask_rcp26 <- rownames_to_column(as.data.frame(HWN_Central_Karoo_mask_rcp26), var="grid")
HWN_Central_Karoo_mask_rcp26 <- pivot_longer(HWN_Central_Karoo_mask_rcp26, cols = !grid, names_to = 'year', values_to = 'hwn')
# plot the HWA in CapeWinelands

ggplot(HWN_Central_Karoo_mask_rcp26, aes(x = year, y = hwn)) +
  geom_boxplot(color = 'pink', fill='pink', alpha = 0.2) +
  ggtitle("Central Karoo HWN (RCP 2.6)") + xlab("Year") + ylab("HWN (events)") +
  theme_classic() +
  theme(plot.title = element_text(size=20, face="bold", hjust = 0.5), axis.title.x = element_text(size=20, face="bold" ), axis.title.y = element_text(size=20, face="bold"), axis.text.y = element_text(size = 20))

##---------------------------Eden-------------------------------------
###------------------------HWA--------------------------------------
setwd("~/Desktop/extreme-events-project-WesternCape/data/rcp26/output")

HWA_rcp26 <- brick('hw_ANN_climpact.sample_historical_NA_2021-2100.nc', varname = 'hwa_tx90')

# Crop the raster-CapeWinelands
HWA_Eden_mask_rcp26 <- mask(HWA_rcp26,Eden) %>%
  as.matrix() 

HWA_Eden_mask_rcp26 <- HWA_Eden_mask_rcp26[rowSums(is.na(HWA_Eden_mask_rcp26)) != ncol(HWA_Eden_mask_rcp26), ]
HWA_Eden_mask_rcp26[,"X2100.07.02"] <- HWA_Eden_mask_rcp26[,"X2094.07.02"] 
HWA_Eden_mask_rcp26 <- t(apply(HWA_Eden_mask_rcp26, 1, na.approx))
#HWA_Eden_mask_rcp85 <- data.frame(Reduce(rbind, HWA_Eden_mask_rcp85))
colnames(HWA_Eden_mask_rcp26) <- as.character(c(2021:2100))
row.names(HWA_Eden_mask_rcp26) <- c("grid1", "grid2", "grid3", "grid4", "grid5", "grid6", "grid7", "grid8", "grid9", "grid10", "grid11", "grid12", "grid13", "grid14", "grid15")

HWA_Eden_mask_rcp26  <- rownames_to_column(as.data.frame(HWA_Eden_mask_rcp26), var="grid")
HWA_Eden_mask_rcp26  <- pivot_longer(HWA_Eden_mask_rcp26, cols = !grid, names_to = 'year', values_to = 'hwa')
# plot the HWA in CapeWinelands

ggplot(HWA_Eden_mask_rcp26, aes(x = year, y = hwa)) +
  geom_boxplot(color = 'red', fill='orange', alpha = 0.2) +
  ggtitle("Eden HWA (RCP 2.6)") + xlab("Year") + ylab("HWA (°C)") +
  theme_classic() +
  theme(plot.title = element_text(size=20, face="bold", hjust = 0.5), axis.title.x = element_text(size=20, face="bold" ), axis.title.y = element_text(size=20, face="bold"),axis.text.y = element_text(size = 20))

###------------------------HWD--------------------------------------
setwd("~/Desktop/extreme-events-project-WesternCape/data/rcp26/output")

HWD_rcp26 <- brick('hw_ANN_climpact.sample_historical_NA_2021-2100.nc', varname = 'hwd_tx90')

# Crop the raster-CapeWinelands
HWD_Eden_mask_rcp26 <- mask(HWD_rcp26,Eden) %>%
  as.matrix() 

HWD_Eden_mask_rcp26 <- HWD_Eden_mask_rcp26[rowSums(is.na(HWD_Eden_mask_rcp26)) != ncol(HWD_Eden_mask_rcp26), ]
HWD_Eden_mask_rcp26[,"X2100.07.02"] <- HWD_Eden_mask_rcp26[,"X2094.07.02"] 
HWD_Eden_mask_rcp26 <- t(apply(HWD_Eden_mask_rcp26, 1, na.approx))
#HWA_Eden_mask_rcp85 <- data.frame(Reduce(rbind, HWA_Eden_mask_rcp85))
colnames(HWD_Eden_mask_rcp26) <- as.character(c(2021:2100))
row.names(HWD_Eden_mask_rcp26) <- c("grid1", "grid2", "grid3", "grid4", "grid5", "grid6", "grid7", "grid8", "grid9", "grid10", "grid11", "grid12", "grid13", "grid14", "grid15")

HWD_Eden_mask_rcp26  <- rownames_to_column(as.data.frame(HWD_Eden_mask_rcp26), var="grid")
HWD_Eden_mask_rcp26  <- pivot_longer(HWD_Eden_mask_rcp26, cols = !grid, names_to = 'year', values_to = 'hwd')
# plot the HWA in CapeWinelands

ggplot(HWD_Eden_mask_rcp26, aes(x = year, y = hwd)) +
  geom_boxplot(color = 'blue', fill='blue', alpha = 0.2) +
  ggtitle("Eden HWD (RCP 2.6)") + xlab("Year") + ylab("HWD (days)") +
  theme_classic() +
  theme(plot.title = element_text(size=20, face="bold", hjust = 0.5), axis.title.x = element_text(size=20, face="bold" ), axis.title.y = element_text(size=20, face="bold"),axis.text.y = element_text(size = 20))

###------------------------HWF--------------------------------------
setwd("~/Desktop/extreme-events-project-WesternCape/data/rcp26/output")

HWF_rcp26 <- brick('hw_ANN_climpact.sample_historical_NA_2021-2100.nc', varname = 'hwf_tx90')

# Crop the raster-CapeWinelands
HWF_Eden_mask_rcp26 <- mask(HWF_rcp26,Eden) %>%
  as.matrix() 

HWF_Eden_mask_rcp26 <- HWF_Eden_mask_rcp26[rowSums(is.na(HWF_Eden_mask_rcp26)) != ncol(HWF_Eden_mask_rcp26), ]
HWF_Eden_mask_rcp26[,"X2100.07.02"] <- HWF_Eden_mask_rcp26[,"X2099.07.02"] 
HWF_Eden_mask_rcp26 <- t(apply(HWF_Eden_mask_rcp26, 1, na.approx))
#HWA_Eden_mask_rcp85 <- data.frame(Reduce(rbind, HWA_Eden_mask_rcp85))
colnames(HWF_Eden_mask_rcp26) <- as.character(c(2021:2100))
row.names(HWF_Eden_mask_rcp26) <- c("grid1", "grid2", "grid3", "grid4", "grid5", "grid6", "grid7", "grid8", "grid9", "grid10", "grid11", "grid12", "grid13", "grid14", "grid15")

HWF_Eden_mask_rcp26  <- rownames_to_column(as.data.frame(HWF_Eden_mask_rcp26), var="grid")
HWF_Eden_mask_rcp26  <- pivot_longer(HWF_Eden_mask_rcp26, cols = !grid, names_to = 'year', values_to = 'hwf')
# plot the HWA in CapeWinelands

ggplot(HWF_Eden_mask_rcp26, aes(x = year, y = hwf)) +
  geom_boxplot(color = 'purple', fill='purple', alpha = 0.2) +
  ggtitle("Eden HWF (RCP 2.6)") + xlab("Year") + ylab("HWF (days)") +
  theme_classic() +
  theme(plot.title = element_text(size=20, face="bold", hjust = 0.5), axis.title.x = element_text(size=20, face="bold" ), axis.title.y = element_text(size=20, face="bold"),axis.text.y = element_text(size = 20))

###------------------------HWM--------------------------------------
setwd("~/Desktop/extreme-events-project-WesternCape/data/rcp26/output")

HWM_rcp26 <- brick('hw_ANN_climpact.sample_historical_NA_2021-2100.nc', varname = 'hwm_tx90')

# Crop the raster-CapeWinelands
HWM_Eden_mask_rcp26 <- mask(HWM_rcp26,Eden) %>%
  as.matrix() 

HWM_Eden_mask_rcp26 <- HWM_Eden_mask_rcp26[rowSums(is.na(HWM_Eden_mask_rcp26)) != ncol(HWM_Eden_mask_rcp26), ]
HWM_Eden_mask_rcp26[,"X2100.07.02"] <- HWM_Eden_mask_rcp26[,"X2094.07.02"] 
HWM_Eden_mask_rcp26 <- t(apply(HWM_Eden_mask_rcp26, 1, na.approx))
#HWA_Eden_mask_rcp85 <- data.frame(Reduce(rbind, HWA_Eden_mask_rcp85))
colnames(HWM_Eden_mask_rcp26) <- as.character(c(2021:2100))
row.names(HWM_Eden_mask_rcp26) <- c("grid1", "grid2", "grid3", "grid4", "grid5", "grid6", "grid7", "grid8", "grid9", "grid10", "grid11", "grid12", "grid13", "grid14", "grid15")

HWM_Eden_mask_rcp26  <- rownames_to_column(as.data.frame(HWM_Eden_mask_rcp26), var="grid")
HWM_Eden_mask_rcp26  <- pivot_longer(HWM_Eden_mask_rcp26, cols = !grid, names_to = 'year', values_to = 'hwm')
# plot the HWA in CapeWinelands

ggplot(HWM_Eden_mask_rcp26, aes(x = year, y = hwm)) +
  geom_boxplot(color = 'orange', fill='orange', alpha = 0.2) +
  ggtitle("Eden HWM (RCP 2.6)") + xlab("Year") + ylab("HWM (°C)") +
  theme_classic() +
  theme(plot.title = element_text(size=20, face="bold", hjust = 0.5), axis.title.x = element_text(size=20, face="bold" ), axis.title.y = element_text(size=20, face="bold"),axis.text.y = element_text(size = 20))

###------------------------HWN--------------------------------------
setwd("~/Desktop/extreme-events-project-WesternCape/data/rcp26/output")

HWN_rcp26 <- brick('hw_ANN_climpact.sample_historical_NA_2021-2100.nc', varname = 'hwn_tx90')

# Crop the raster-CapeWinelands
HWN_Eden_mask_rcp26 <- mask(HWN_rcp26,Eden) %>%
  as.matrix() 

HWN_Eden_mask_rcp26 <- HWN_Eden_mask_rcp26[rowSums(is.na(HWN_Eden_mask_rcp26)) != ncol(HWN_Eden_mask_rcp26), ]
HWN_Eden_mask_rcp26[,"X2100.07.02"] <- HWN_Eden_mask_rcp26[,"X2099.07.02"] 
HWN_Eden_mask_rcp26 <- t(apply(HWN_Eden_mask_rcp26, 1, na.approx))
#HWA_Eden_mask_rcp85 <- data.frame(Reduce(rbind, HWA_Eden_mask_rcp85))
colnames(HWN_Eden_mask_rcp26) <- as.character(c(2021:2100))
row.names(HWN_Eden_mask_rcp26) <- c("grid1", "grid2", "grid3", "grid4", "grid5", "grid6", "grid7", "grid8", "grid9", "grid10", "grid11", "grid12", "grid13", "grid14", "grid15")

HWN_Eden_mask_rcp26  <- rownames_to_column(as.data.frame(HWN_Eden_mask_rcp26), var="grid")
HWN_Eden_mask_rcp26  <- pivot_longer(HWN_Eden_mask_rcp26, cols = !grid, names_to = 'year', values_to = 'hwn')
# plot the HWA in CapeWinelands

ggplot(HWN_Eden_mask_rcp26, aes(x = year, y = hwn)) +
  geom_boxplot(color = 'pink', fill='pink', alpha = 0.2) +
  ggtitle("Eden HWN (RCP 2.6)") + xlab("Year") + ylab("HWN (events)") +
  theme_classic() +
  theme(plot.title = element_text(size=20, face="bold", hjust = 0.5), axis.title.x = element_text(size=20, face="bold" ), axis.title.y = element_text(size=20, face="bold"),axis.text.y = element_text(size = 20))

##------------------------Overberg---------------------------------------------
###------------------------HWA-------------------------------------------------
setwd("~/Desktop/extreme-events-project-WesternCape/data/rcp26/output")

HWA_rcp26 <- brick('hw_ANN_climpact.sample_historical_NA_2021-2100.nc', varname = 'hwa_tx90')

# Crop the raster-CapeWinelands
HWA_Overberg_mask_rcp26 <- mask(HWA_rcp26,Overberg) %>%
  as.matrix() 

HWA_Overberg_mask_rcp26 <- HWA_Overberg_mask_rcp26[rowSums(is.na(HWA_Overberg_mask_rcp26)) != ncol(HWA_Overberg_mask_rcp26), ]
HWA_Overberg_mask_rcp26[,"X2100.07.02"] <- HWA_Overberg_mask_rcp26[,"X2094.07.02"] 
HWA_Overberg_mask_rcp26 <- t(apply(HWA_Overberg_mask_rcp26, 1, na.approx))
#HWA_Overberg_mask_rcp85 <- data.frame(Reduce(rbind, HWA_Overberg_mask_rcp85))
colnames(HWA_Overberg_mask_rcp26) <- as.character(c(2021:2100))
row.names(HWA_Overberg_mask_rcp26) <- c("grid1", "grid2", "grid3", "grid4", "grid5", "grid6")

HWA_Overberg_mask_rcp26  <- rownames_to_column(as.data.frame(HWA_Overberg_mask_rcp26), var="grid")
HWA_Overberg_mask_rcp26  <- pivot_longer(HWA_Overberg_mask_rcp26, cols = !grid, names_to = 'year', values_to = 'hwa')

ggplot(HWA_Overberg_mask_rcp26, aes(x = year, y = hwa)) +
  geom_boxplot(color = 'red', fill='orange', alpha = 0.2) +
  ggtitle("Overberg HWA (RCP 2.6)") + xlab("Year") + ylab("HWA (°C)") + 
  theme_classic() + 
  theme(plot.title = element_text(size=20, face="bold", hjust = 0.5), axis.title.x = element_text(size=20, face="bold" ), axis.title.y = element_text(size=20, face="bold"), axis.text.y = element_text(size = 20))

###------------------------HWD-------------------------------------------------
setwd("~/Desktop/extreme-events-project-WesternCape/data/rcp26/output")

HWD_rcp26 <- brick('hw_ANN_climpact.sample_historical_NA_2021-2100.nc', varname = 'hwd_tx90')

# Crop the raster-CapeWinelands
HWD_Overberg_mask_rcp26 <- mask(HWD_rcp26,Overberg) %>%
  as.matrix() 

HWD_Overberg_mask_rcp26 <- HWD_Overberg_mask_rcp26[rowSums(is.na(HWD_Overberg_mask_rcp26)) != ncol(HWD_Overberg_mask_rcp26), ]
HWD_Overberg_mask_rcp26[,"X2100.07.02"] <- HWD_Overberg_mask_rcp26[,"X2094.07.02"] 
HWD_Overberg_mask_rcp26 <- t(apply(HWD_Overberg_mask_rcp26, 1, na.approx))
#HWA_Overberg_mask_rcp85 <- data.frame(Reduce(rbind, HWA_Overberg_mask_rcp85))
colnames(HWD_Overberg_mask_rcp26) <- as.character(c(2021:2100))
row.names(HWD_Overberg_mask_rcp26) <- c("grid1", "grid2", "grid3", "grid4", "grid5", "grid6")

HWD_Overberg_mask_rcp26  <- rownames_to_column(as.data.frame(HWD_Overberg_mask_rcp26), var="grid")
HWD_Overberg_mask_rcp26  <- pivot_longer(HWD_Overberg_mask_rcp26, cols = !grid, names_to = 'year', values_to = 'hwd')

ggplot(HWD_Overberg_mask_rcp26, aes(x = year, y = hwd)) +
  geom_boxplot(color = 'blue', fill='blue', alpha = 0.2) +
  ggtitle("Overberg HWD (RCP 2.6)") + xlab("Year") + ylab("HWD (days)") + 
  theme_classic() + 
  theme(plot.title = element_text(size=20, face="bold", hjust = 0.5), axis.title.x = element_text(size=20, face="bold" ), axis.title.y = element_text(size=20, face="bold"), axis.text.y = element_text(size = 20))

###------------------------HWF-------------------------------------------------
setwd("~/Desktop/extreme-events-project-WesternCape/data/rcp26/output")

HWF_rcp26 <- brick('hw_ANN_climpact.sample_historical_NA_2021-2100.nc', varname = 'hwf_tx90')

# Crop the raster-CapeWinelands
HWF_Overberg_mask_rcp26 <- mask(HWF_rcp26,Overberg) %>%
  as.matrix() 

HWF_Overberg_mask_rcp26 <- HWF_Overberg_mask_rcp26[rowSums(is.na(HWF_Overberg_mask_rcp26)) != ncol(HWF_Overberg_mask_rcp26), ]
HWF_Overberg_mask_rcp26[,"X2100.07.02"] <- HWF_Overberg_mask_rcp26[,"X2099.07.02"] 
HWF_Overberg_mask_rcp26 <- t(apply(HWF_Overberg_mask_rcp26, 1, na.approx))
#HWA_Overberg_mask_rcp85 <- data.frame(Reduce(rbind, HWA_Overberg_mask_rcp85))
colnames(HWF_Overberg_mask_rcp26) <- as.character(c(2021:2100))
row.names(HWF_Overberg_mask_rcp26) <- c("grid1", "grid2", "grid3", "grid4", "grid5", "grid6")

HWF_Overberg_mask_rcp26  <- rownames_to_column(as.data.frame(HWF_Overberg_mask_rcp26), var="grid")
HWF_Overberg_mask_rcp26  <- pivot_longer(HWF_Overberg_mask_rcp26, cols = !grid, names_to = 'year', values_to = 'hwf')

ggplot(HWF_Overberg_mask_rcp26, aes(x = year, y = hwf)) +
  geom_boxplot(color = 'purple', fill='purple', alpha = 0.2) +
  ggtitle("Overberg HWF (RCP 2.6)") + xlab("Year") + ylab("HWF (days)") + 
  theme_classic() + 
  theme(plot.title = element_text(size=20, face="bold", hjust = 0.5), axis.title.x = element_text(size=20, face="bold" ), axis.title.y = element_text(size=20, face="bold"), axis.text.y = element_text(size = 20))

###------------------------HWM-------------------------------------------------
setwd("~/Desktop/extreme-events-project-WesternCape/data/rcp26/output")

HWM_rcp26 <- brick('hw_ANN_climpact.sample_historical_NA_2021-2100.nc', varname = 'hwm_tx90')

# Crop the raster-CapeWinelands
HWM_Overberg_mask_rcp26 <- mask(HWM_rcp26,Overberg) %>%
  as.matrix() 

HWM_Overberg_mask_rcp26 <- HWM_Overberg_mask_rcp26[rowSums(is.na(HWM_Overberg_mask_rcp26)) != ncol(HWM_Overberg_mask_rcp26), ]

HWM_Overberg_mask_rcp26[,"X2100.07.02"] <- HWM_Overberg_mask_rcp26[,"X2094.07.02"] 
HWM_Overberg_mask_rcp26 <- t(apply(HWM_Overberg_mask_rcp26, 1, na.approx))
#HWA_Overberg_mask_rcp85 <- data.frame(Reduce(rbind, HWA_Overberg_mask_rcp85))
colnames(HWM_Overberg_mask_rcp26) <- as.character(c(2021:2100))
row.names(HWM_Overberg_mask_rcp26) <- c("grid1", "grid2", "grid3", "grid4", "grid5", "grid6")

HWM_Overberg_mask_rcp26  <- rownames_to_column(as.data.frame(HWM_Overberg_mask_rcp26), var="grid")
HWM_Overberg_mask_rcp26  <- pivot_longer(HWM_Overberg_mask_rcp26, cols = !grid, names_to = 'year', values_to = 'hwm')

ggplot(HWM_Overberg_mask_rcp26, aes(x = year, y = hwm)) +
  geom_boxplot(color = 'orange', fill='orange', alpha = 0.2) +
  ggtitle("Overberg HWM (RCP 2.6)") + xlab("Year") + ylab("HWM (°C)") + 
  theme_classic() + 
  theme(plot.title = element_text(size=20, face="bold", hjust = 0.5), axis.title.x = element_text(size=20, face="bold" ), axis.title.y = element_text(size=20, face="bold"), axis.text.y = element_text(size = 20))

###------------------------HWN-------------------------------------------------
setwd("~/Desktop/extreme-events-project-WesternCape/data/rcp26/output")

HWN_rcp26 <- brick('hw_ANN_climpact.sample_historical_NA_2021-2100.nc', varname = 'hwn_tx90')

# Crop the raster-CapeWinelands
HWN_Overberg_mask_rcp26 <- mask(HWN_rcp26,Overberg) %>%
  as.matrix() 

HWN_Overberg_mask_rcp26 <- HWN_Overberg_mask_rcp26[rowSums(is.na(HWN_Overberg_mask_rcp26)) != ncol(HWN_Overberg_mask_rcp26), ]
HWN_Overberg_mask_rcp26[,"X2100.07.02"] <- HWN_Overberg_mask_rcp26[,"X2099.07.02"] 
HWN_Overberg_mask_rcp26 <- t(apply(HWN_Overberg_mask_rcp26, 1, na.approx))
#HWA_Overberg_mask_rcp85 <- data.frame(Reduce(rbind, HWA_Overberg_mask_rcp85))
colnames(HWN_Overberg_mask_rcp26) <- as.character(c(2021:2100))
row.names(HWN_Overberg_mask_rcp26) <- c("grid1", "grid2", "grid3", "grid4", "grid5", "grid6")

HWN_Overberg_mask_rcp26  <- rownames_to_column(as.data.frame(HWN_Overberg_mask_rcp26), var="grid")
HWN_Overberg_mask_rcp26  <- pivot_longer(HWN_Overberg_mask_rcp26, cols = !grid, names_to = 'year', values_to = 'hwn')

ggplot(HWN_Overberg_mask_rcp26, aes(x = year, y = hwn)) +
  geom_boxplot(color = 'pink', fill='pink', alpha = 0.2) +
  ggtitle("Overberg HWN (RCP 2.6)") + xlab("Year") + ylab("HWN (events)") + 
  theme_classic() + 
  theme(plot.title = element_text(size=20, face="bold", hjust = 0.5), axis.title.x = element_text(size=20, face="bold" ), axis.title.y = element_text(size=20, face="bold"), axis.text.y = element_text(size = 20))

##------------------------West_Coast---------------------------------------------
###------------------------HWA-------------------------------------------------
setwd("~/Desktop/extreme-events-project-WesternCape/data/rcp26/output")
HWA_rcp26 <- brick('hw_ANN_climpact.sample_historical_NA_2021-2100.nc', varname = 'hwa_tx90')

# Crop the raster-CapeWinelands
HWA_West_Coast_mask_rcp26 <- mask(HWA_rcp26,West_Coast) %>%
  as.matrix() 

HWA_West_Coast_mask_rcp26 <- HWA_West_Coast_mask_rcp26[rowSums(is.na(HWA_West_Coast_mask_rcp26)) != ncol(HWA_West_Coast_mask_rcp26), ]
HWA_West_Coast_mask_rcp26[,"X2100.07.02"] <- HWA_West_Coast_mask_rcp26[,"X2098.07.02"]
HWA_West_Coast_mask_rcp26 <- t(apply(HWA_West_Coast_mask_rcp26, 1, na.approx))
#HWA_West_Coast_mask_rcp85 <- data.frame(Reduce(rbind, HWA_West_Coast_mask_rcp85))
colnames(HWA_West_Coast_mask_rcp26) <- as.character(c(2021:2100))
row.names(HWA_West_Coast_mask_rcp26) <- c("grid1", "grid2", "grid3", "grid4", "grid5", "grid6", "grid7", "grid8", "grid9", "grid10", "grid11", "grid12", "grid13", "grid14", "grid15", "grid16", "grid17", "grid18", "grid19")

HWA_West_Coast_mask_rcp26  <- rownames_to_column(as.data.frame(HWA_West_Coast_mask_rcp26), var="grid")
HWA_West_Coast_mask_rcp26  <- pivot_longer(HWA_West_Coast_mask_rcp26, cols = !grid, names_to = 'year', values_to = 'hwa')

ggplot(HWA_West_Coast_mask_rcp26, aes(x = year, y = hwa)) +
  geom_boxplot(color = 'red', fill='orange', alpha = 0.2) +
  ggtitle("West Coast HWA (RCP 2.6)") + xlab("Year") + ylab("HWA (°C)") + 
  theme_classic() + 
  theme(plot.title = element_text(size=20, face="bold", hjust = 0.5), axis.title.x = element_text(size=20, face="bold" ), axis.title.y = element_text(size=20, face="bold"), axis.text.y = element_text(size = 20))

###------------------------HWD-------------------------------------------------
setwd("~/Desktop/extreme-events-project-WesternCape/data/rcp26/output")
HWD_rcp26 <- brick('hw_ANN_climpact.sample_historical_NA_2021-2100.nc', varname = 'hwd_tx90')

# Crop the raster-CapeWinelands
HWD_West_Coast_mask_rcp26 <- mask(HWD_rcp26,West_Coast) %>%
  as.matrix() 

HWD_West_Coast_mask_rcp26 <- HWD_West_Coast_mask_rcp26[rowSums(is.na(HWD_West_Coast_mask_rcp26)) != ncol(HWD_West_Coast_mask_rcp26), ]
HWD_West_Coast_mask_rcp26[,"X2100.07.02"] <- HWD_West_Coast_mask_rcp26[,"X2098.07.02"]
HWD_West_Coast_mask_rcp26 <- t(apply(HWD_West_Coast_mask_rcp26, 1, na.approx))
#HWA_West_Coast_mask_rcp85 <- data.frame(Reduce(rbind, HWA_West_Coast_mask_rcp85))
colnames(HWD_West_Coast_mask_rcp26) <- as.character(c(2021:2100))
row.names(HWD_West_Coast_mask_rcp26) <- c("grid1", "grid2", "grid3", "grid4", "grid5", "grid6", "grid7", "grid8", "grid9", "grid10", "grid11", "grid12", "grid13", "grid14", "grid15", "grid16", "grid17", "grid18", "grid19")

HWD_West_Coast_mask_rcp26  <- rownames_to_column(as.data.frame(HWD_West_Coast_mask_rcp26), var="grid")
HWD_West_Coast_mask_rcp26  <- pivot_longer(HWD_West_Coast_mask_rcp26, cols = !grid, names_to = 'year', values_to = 'hwd')

ggplot(HWD_West_Coast_mask_rcp26, aes(x = year, y = hwd)) +
  geom_boxplot(color = 'blue', fill='blue', alpha = 0.2) +
  ggtitle("West Coast HWD (RCP 2.6)") + xlab("Year") + ylab("HWD (days)") + 
  theme_classic() + 
  theme(plot.title = element_text(size=20, face="bold", hjust = 0.5), axis.title.x = element_text(size=20, face="bold" ), axis.title.y = element_text(size=20, face="bold"), axis.text.y = element_text(size = 20))

###------------------------HWF-------------------------------------------------
setwd("~/Desktop/extreme-events-project-WesternCape/data/rcp26/output")
HWF_rcp26 <- brick('hw_ANN_climpact.sample_historical_NA_2021-2100.nc', varname = 'hwf_tx90')

# Crop the raster-CapeWinelands
HWF_West_Coast_mask_rcp26 <- mask(HWF_rcp26,West_Coast) %>%
  as.matrix() 

HWF_West_Coast_mask_rcp26 <- HWF_West_Coast_mask_rcp26[rowSums(is.na(HWF_West_Coast_mask_rcp26)) != ncol(HWF_West_Coast_mask_rcp26), ]
HWF_West_Coast_mask_rcp26[,"X2100.07.02"] <- HWF_West_Coast_mask_rcp26[,"X2099.07.02"]
HWF_West_Coast_mask_rcp26 <- t(apply(HWF_West_Coast_mask_rcp26, 1, na.approx))
#HWA_West_Coast_mask_rcp85 <- data.frame(Reduce(rbind, HWA_West_Coast_mask_rcp85))
colnames(HWF_West_Coast_mask_rcp26) <- as.character(c(2021:2100))
row.names(HWF_West_Coast_mask_rcp26) <- c("grid1", "grid2", "grid3", "grid4", "grid5", "grid6", "grid7", "grid8", "grid9", "grid10", "grid11", "grid12", "grid13", "grid14", "grid15", "grid16", "grid17", "grid18", "grid19")

HWF_West_Coast_mask_rcp26  <- rownames_to_column(as.data.frame(HWF_West_Coast_mask_rcp26), var="grid")
HWF_West_Coast_mask_rcp26  <- pivot_longer(HWF_West_Coast_mask_rcp26, cols = !grid, names_to = 'year', values_to = 'hwf')

ggplot(HWF_West_Coast_mask_rcp26, aes(x = year, y = hwf)) +
  geom_boxplot(color = 'purple', fill='purple', alpha = 0.2) +
  ggtitle("West Coast HWF (RCP 2.6)") + xlab("Year") + ylab("HWF (days)") + 
  theme_classic() + 
  theme(plot.title = element_text(size=20, face="bold", hjust = 0.5), axis.title.x = element_text(size=20, face="bold" ), axis.title.y = element_text(size=20, face="bold"), axis.text.y = element_text(size = 20))

###------------------------HWM-------------------------------------------------
setwd("~/Desktop/extreme-events-project-WesternCape/data/rcp26/output")
HWM_rcp26 <- brick('hw_ANN_climpact.sample_historical_NA_2021-2100.nc', varname = 'hwm_tx90')

# Crop the raster-CapeWinelands
HWM_West_Coast_mask_rcp26 <- mask(HWM_rcp26,West_Coast) %>%
  as.matrix() 

HWM_West_Coast_mask_rcp26 <- HWM_West_Coast_mask_rcp26[rowSums(is.na(HWM_West_Coast_mask_rcp26)) != ncol(HWM_West_Coast_mask_rcp26), ]
HWM_West_Coast_mask_rcp26[,"X2100.07.02"] <- HWM_West_Coast_mask_rcp26[,"X2098.07.02"]
HWM_West_Coast_mask_rcp26 <- t(apply(HWM_West_Coast_mask_rcp26, 1, na.approx))
#HWA_West_Coast_mask_rcp85 <- data.frame(Reduce(rbind, HWA_West_Coast_mask_rcp85))
colnames(HWM_West_Coast_mask_rcp26) <- as.character(c(2021:2100))
row.names(HWM_West_Coast_mask_rcp26) <- c("grid1", "grid2", "grid3", "grid4", "grid5", "grid6", "grid7", "grid8", "grid9", "grid10", "grid11", "grid12", "grid13", "grid14", "grid15", "grid16", "grid17", "grid18", "grid19")

HWM_West_Coast_mask_rcp26  <- rownames_to_column(as.data.frame(HWM_West_Coast_mask_rcp26), var="grid")
HWM_West_Coast_mask_rcp26  <- pivot_longer(HWM_West_Coast_mask_rcp26, cols = !grid, names_to = 'year', values_to = 'hwm')

ggplot(HWM_West_Coast_mask_rcp26, aes(x = year, y = hwm)) +
  geom_boxplot(color = 'orange', fill='orange', alpha = 0.2) +
  ggtitle("West Coast HWM (RCP 2.6)") + xlab("Year") + ylab("HWM (°C)") + 
  theme_classic() + 
  theme(plot.title = element_text(size=20, face="bold", hjust = 0.5), axis.title.x = element_text(size=20, face="bold" ), axis.title.y = element_text(size=20, face="bold"), axis.text.y = element_text(size = 20))

###------------------------HWN-------------------------------------------------
setwd("~/Desktop/extreme-events-project-WesternCape/data/rcp26/output")
HWN_rcp26 <- brick('hw_ANN_climpact.sample_historical_NA_2021-2100.nc', varname = 'hwn_tx90')

# Crop the raster-CapeWinelands
HWN_West_Coast_mask_rcp26 <- mask(HWN_rcp26,West_Coast) %>%
  as.matrix() 

HWN_West_Coast_mask_rcp26 <- HWN_West_Coast_mask_rcp26[rowSums(is.na(HWN_West_Coast_mask_rcp26)) != ncol(HWN_West_Coast_mask_rcp26), ]
HWN_West_Coast_mask_rcp26[,"X2100.07.02"] <- HWN_West_Coast_mask_rcp26[,"X2099.07.02"]
HWN_West_Coast_mask_rcp26 <- t(apply(HWN_West_Coast_mask_rcp26, 1, na.approx))
#HWA_West_Coast_mask_rcp85 <- data.frame(Reduce(rbind, HWA_West_Coast_mask_rcp85))
colnames(HWN_West_Coast_mask_rcp26) <- as.character(c(2021:2100))
row.names(HWN_West_Coast_mask_rcp26) <- c("grid1", "grid2", "grid3", "grid4", "grid5", "grid6", "grid7", "grid8", "grid9", "grid10", "grid11", "grid12", "grid13", "grid14", "grid15", "grid16", "grid17", "grid18", "grid19")

HWN_West_Coast_mask_rcp26  <- rownames_to_column(as.data.frame(HWN_West_Coast_mask_rcp26), var="grid")
HWN_West_Coast_mask_rcp26  <- pivot_longer(HWN_West_Coast_mask_rcp26, cols = !grid, names_to = 'year', values_to = 'hwn')

ggplot(HWN_West_Coast_mask_rcp26, aes(x = year, y = hwn)) +
  geom_boxplot(color = 'pink', fill='pink', alpha = 0.2) +
  ggtitle("West Coast HWN (RCP 2.6)") + xlab("Year") + ylab("HWN (events)") + 
  theme_classic() + 
  theme(plot.title = element_text(size=20, face="bold", hjust = 0.5), axis.title.x = element_text(size=20, face="bold" ), axis.title.y = element_text(size=20, face="bold"), axis.text.y = element_text(size = 20))

##------------------------City_Cape_Town---------------------------------------------
###------------------------HWA-------------------------------------------------------
setwd("~/Desktop/extreme-events-project-WesternCape/data/rcp26/output")
HWA_rcp26 <- brick('hw_ANN_climpact.sample_historical_NA_2021-2100.nc', varname = 'hwa_tx90')

# Crop the raster-CapeWinelands
HWA_City_Cape_Town_mask_rcp26 <- mask(HWA_rcp26,buffer(City_Cape_Town, width = 0.05)) %>%
  as.matrix() 

HWA_City_Cape_Town_mask_rcp26 <- HWA_City_Cape_Town_mask_rcp26[rowSums(is.na(HWA_City_Cape_Town_mask_rcp26)) != ncol(HWA_City_Cape_Town_mask_rcp26), ]
HWA_City_Cape_Town_mask_rcp26[,"X2100.07.02"] <-  HWA_City_Cape_Town_mask_rcp26[,"X2098.07.02"]
HWA_City_Cape_Town_mask_rcp26 <- t(apply(HWA_City_Cape_Town_mask_rcp26, 1, na.approx))
#HWA_West_Coast_mask_rcp85 <- data.frame(Reduce(rbind, HWA_West_Coast_mask_rcp85))
colnames(HWA_City_Cape_Town_mask_rcp26) <- as.character(c(2021:2100))
row.names(HWA_City_Cape_Town_mask_rcp26) <- c("grid1", "grid2", "grid3")

HWA_City_Cape_Town_mask_rcp26  <- rownames_to_column(as.data.frame(HWA_City_Cape_Town_mask_rcp26), var="grid")
HWA_City_Cape_Town_mask_rcp26  <- pivot_longer(HWA_City_Cape_Town_mask_rcp26, cols = !grid, names_to = 'year', values_to = 'hwa')

ggplot(HWA_City_Cape_Town_mask_rcp26, aes(x = year, y = hwa)) +
  geom_boxplot(color = 'red', fill='orange', alpha = 0.2) +
  ggtitle("City of Cape Town HWA (RCP 2.6)") + xlab("Year") + ylab("HWA (°C)") + 
  theme_classic() + 
  theme(plot.title = element_text(size=20, face="bold", hjust = 0.5), axis.title.x = element_text(size=20, face="bold" ), axis.title.y = element_text(size=20, face="bold"), axis.text.y = element_text(size = 20))

###------------------------HWD-------------------------------------------------
setwd("~/Desktop/extreme-events-project-WesternCape/data/rcp26/output")
HWD_rcp26 <- brick('hw_ANN_climpact.sample_historical_NA_2021-2100.nc', varname = 'hwd_tx90')

# Crop the raster-CapeWinelands
HWD_City_Cape_Town_mask_rcp26 <- mask(HWD_rcp26,buffer(City_Cape_Town, width = 0.05)) %>%
  as.matrix() 

HWD_City_Cape_Town_mask_rcp26 <- HWD_City_Cape_Town_mask_rcp26[rowSums(is.na(HWD_City_Cape_Town_mask_rcp26)) != ncol(HWD_City_Cape_Town_mask_rcp26), ]
HWD_City_Cape_Town_mask_rcp26[,"X2100.07.02"] <- HWD_City_Cape_Town_mask_rcp26[,"X2098.07.02"]
HWD_City_Cape_Town_mask_rcp26 <- t(apply(HWD_City_Cape_Town_mask_rcp26, 1, na.approx))
#HWA_West_Coast_mask_rcp85 <- data.frame(Reduce(rbind, HWA_West_Coast_mask_rcp85))
colnames(HWD_City_Cape_Town_mask_rcp26) <- as.character(c(2021:2100))
row.names(HWD_City_Cape_Town_mask_rcp26) <- c("grid1", "grid2", "grid3")

HWD_City_Cape_Town_mask_rcp26  <- rownames_to_column(as.data.frame(HWD_City_Cape_Town_mask_rcp26), var="grid")
HWD_City_Cape_Town_mask_rcp26  <- pivot_longer(HWD_City_Cape_Town_mask_rcp26, cols = !grid, names_to = 'year', values_to = 'hwd')

ggplot(HWD_City_Cape_Town_mask_rcp26, aes(x = year, y = hwd)) +
  geom_boxplot(color = 'blue', fill='blue', alpha = 0.2) +
  ggtitle("City of Cape Town HWD (RCP 2.6)") + xlab("Year") + ylab("HWD (days)") + 
  theme_classic() + 
  theme(plot.title = element_text(size=20, face="bold", hjust = 0.5), axis.title.x = element_text(size=20, face="bold" ), axis.title.y = element_text(size=20, face="bold"), axis.text.y = element_text(size = 20))

###------------------------HWF-------------------------------------------------
setwd("~/Desktop/extreme-events-project-WesternCape/data/rcp26/output")
HWF_rcp26 <- brick('hw_ANN_climpact.sample_historical_NA_2021-2100.nc', varname = 'hwf_tx90')

# Crop the raster-CapeWinelands
HWF_City_Cape_Town_mask_rcp26 <- mask(HWF_rcp26,buffer(City_Cape_Town, width = 0.05)) %>%
  as.matrix() 

HWF_City_Cape_Town_mask_rcp26 <- HWF_City_Cape_Town_mask_rcp26[rowSums(is.na(HWF_City_Cape_Town_mask_rcp26)) != ncol(HWF_City_Cape_Town_mask_rcp26), ]
HWF_City_Cape_Town_mask_rcp26[,"X2100.07.02"] <- HWF_City_Cape_Town_mask_rcp26[,"X2099.07.02"] 
HWF_City_Cape_Town_mask_rcp26 <- t(apply(HWF_City_Cape_Town_mask_rcp26, 1, na.approx))
#HWA_West_Coast_mask_rcp85 <- data.frame(Reduce(rbind, HWA_West_Coast_mask_rcp85))
colnames(HWF_City_Cape_Town_mask_rcp26) <- as.character(c(2021:2100))
row.names(HWF_City_Cape_Town_mask_rcp26) <- c("grid1", "grid2", "grid3")

HWF_City_Cape_Town_mask_rcp26  <- rownames_to_column(as.data.frame(HWF_City_Cape_Town_mask_rcp26), var="grid")
HWF_City_Cape_Town_mask_rcp26  <- pivot_longer(HWF_City_Cape_Town_mask_rcp26, cols = !grid, names_to = 'year', values_to = 'hwf')

ggplot(HWF_City_Cape_Town_mask_rcp26, aes(x = year, y = hwf)) +
  geom_boxplot(color = 'purple', fill='purple', alpha = 0.2) +
  ggtitle("City of Cape Town HWF (RCP 2.6)") + xlab("Year") + ylab("HWF (days)") + 
  theme_classic() + 
  theme(plot.title = element_text(size=20, face="bold", hjust = 0.5), axis.title.x = element_text(size=20, face="bold" ), axis.title.y = element_text(size=20, face="bold"), axis.text.y = element_text(size = 20))

###------------------------HWM-------------------------------------------------
setwd("~/Desktop/extreme-events-project-WesternCape/data/rcp26/output")
HWM_rcp26 <- brick('hw_ANN_climpact.sample_historical_NA_2021-2100.nc', varname = 'hwm_tx90')

# Crop the raster-CapeWinelands
HWM_City_Cape_Town_mask_rcp26 <- mask(HWM_rcp26,buffer(City_Cape_Town, width = 0.05)) %>%
  as.matrix() 

HWM_City_Cape_Town_mask_rcp26 <- HWM_City_Cape_Town_mask_rcp26[rowSums(is.na(HWM_City_Cape_Town_mask_rcp26)) != ncol(HWM_City_Cape_Town_mask_rcp26), ]
HWM_City_Cape_Town_mask_rcp26[,"X2100.07.02"] <- HWM_City_Cape_Town_mask_rcp26[,"X2098.07.02"]
HWM_City_Cape_Town_mask_rcp26 <- t(apply(HWM_City_Cape_Town_mask_rcp26, 1, na.approx))
#HWA_West_Coast_mask_rcp85 <- data.frame(Reduce(rbind, HWA_West_Coast_mask_rcp85))
colnames(HWM_City_Cape_Town_mask_rcp26) <- as.character(c(2021:2100))
row.names(HWM_City_Cape_Town_mask_rcp26) <- c("grid1", "grid2", "grid3")

HWM_City_Cape_Town_mask_rcp26  <- rownames_to_column(as.data.frame(HWM_City_Cape_Town_mask_rcp26), var="grid")
HWM_City_Cape_Town_mask_rcp26  <- pivot_longer(HWM_City_Cape_Town_mask_rcp26, cols = !grid, names_to = 'year', values_to = 'hwm')

ggplot(HWM_City_Cape_Town_mask_rcp26, aes(x = year, y = hwm)) +
  geom_boxplot(color = 'orange', fill='orange', alpha = 0.2) +
  ggtitle("City of Cape Town HWM (RCP 2.6)") + xlab("Year") + ylab("HWM (days)") + 
  theme_classic() + 
  theme(plot.title = element_text(size=20, face="bold", hjust = 0.5), axis.title.x = element_text(size=20, face="bold" ), axis.title.y = element_text(size=20, face="bold"), axis.text.y = element_text(size = 20))

###------------------------HWN-------------------------------------------------
setwd("~/Desktop/extreme-events-project-WesternCape/data/rcp26/output")
HWN_rcp26 <- brick('hw_ANN_climpact.sample_historical_NA_2021-2100.nc', varname = 'hwn_tx90')

# Crop the raster-CapeWinelands
HWN_City_Cape_Town_mask_rcp26 <- mask(HWN_rcp26,buffer(City_Cape_Town, width = 0.05)) %>%
  as.matrix() 

HWN_City_Cape_Town_mask_rcp26 <- HWN_City_Cape_Town_mask_rcp26[rowSums(is.na(HWN_City_Cape_Town_mask_rcp26)) != ncol(HWN_City_Cape_Town_mask_rcp26), ]
HWN_City_Cape_Town_mask_rcp26[,"X2100.07.02"] <- HWN_City_Cape_Town_mask_rcp26[,"X2098.07.02"]
HWN_City_Cape_Town_mask_rcp26 <- t(apply(HWN_City_Cape_Town_mask_rcp26, 1, na.approx))
#HWA_West_Coast_mask_rcp85 <- data.frame(Reduce(rbind, HWA_West_Coast_mask_rcp85))
colnames(HWN_City_Cape_Town_mask_rcp26) <- as.character(c(2021:2100))
row.names(HWN_City_Cape_Town_mask_rcp26) <- c("grid1", "grid2", "grid3")

HWN_City_Cape_Town_mask_rcp26  <- rownames_to_column(as.data.frame(HWN_City_Cape_Town_mask_rcp26), var="grid")
HWN_City_Cape_Town_mask_rcp26  <- pivot_longer(HWN_City_Cape_Town_mask_rcp26, cols = !grid, names_to = 'year', values_to = 'hwn')

ggplot(HWN_City_Cape_Town_mask_rcp26, aes(x = year, y = hwn)) +
  geom_boxplot(color = 'pink', fill='pink', alpha = 0.2) +
  ggtitle("City of Cape Town HWN (RCP 2.6)") + xlab("Year") + ylab("HWN (events)") + 
  theme_classic() + 
  theme(plot.title = element_text(size=20, face="bold", hjust = 0.5), axis.title.x = element_text(size=20, face="bold" ), axis.title.y = element_text(size=20, face="bold"), axis.text.y = element_text(size = 20))

#--------------------------Save-Outputs-RCP2.6----------------------------------
write_rds(list(HWA_Cape_Winelands_rcp26 = HWA_Cape_Winelands_mask_rcp26, HWD_Cape_Winelands_rcp26 = HWD_Cape_Winelands_mask_rcp26, HWF_Cape_Winelands_rcp26 = HWF_Cape_Winelands_mask_rcp26,
               HWM_Cape_Winelands_rcp26 = HWM_Cape_Winelands_mask_rcp26, HWN_Cape_Winelands_rcp26 = HWN_Cape_Winelands_mask_rcp26, HWA_Central_Karoo_rcp26 =  HWA_Central_Karoo_mask_rcp26,
               HWD_Central_Karoo_rcp26 = HWD_Central_Karoo_mask_rcp26, HWF_Central_Karoo_rcp26 = HWF_Central_Karoo_mask_rcp26, HWM_Central_Karoo_rcp26 =  HWM_Central_Karoo_mask_rcp26,
               HWN_Central_Karoo_rcp26 = HWN_Central_Karoo_mask_rcp26, HWA_City_Cape_Town_rcp26 = HWA_City_Cape_Town_mask_rcp26, HWD_City_Cape_Town_rcp26 = HWD_City_Cape_Town_mask_rcp26,
               HWF_City_Cape_Town_rcp26 =  HWF_City_Cape_Town_mask_rcp26, HWM_City_Cape_Town_rcp26 =  HWM_City_Cape_Town_mask_rcp26, HWN_City_Cape_Town_rcp26 = HWN_City_Cape_Town_mask_rcp26,
               HWA_Eden_rcp26 =  HWA_Eden_mask_rcp26, HWD_Eden_rcp26 =  HWD_Eden_mask_rcp26, HWF_Eden_rcp26 =  HWF_Eden_mask_rcp26, HWM_Eden_rcp26 = HWM_Eden_mask_rcp26, HWN_Eden_rcp26 =  HWN_Eden_mask_rcp26,
               HWA_Overberg_rcp26 =  HWA_Overberg_mask_rcp26, HWD_Overberg_rcp26 =  HWD_Overberg_mask_rcp26, HWF_Overberg_rcp26 = HWF_Overberg_mask_rcp26, HWM_Overberg_rcp26 = HWM_Overberg_mask_rcp26, HWN_Overberg_rcp26 = HWN_Overberg_mask_rcp26,
               HWA_West_Coast_rcp26 = HWA_West_Coast_mask_rcp26, HWD_West_Coast_rcp26 = HWD_West_Coast_mask_rcp26, HWF_West_Coast_rcp26 = HWF_West_Coast_mask_rcp26, HWM_West_Coast_rcp26 = HWM_West_Coast_mask_rcp26, HWN_West_Coast_rcp26 = HWN_West_Coast_mask_rcp26),
          path = file.path(data_dir ="/Users/hebowen/Desktop/extreme-events-project-WesternCape/data/rcp26", "regional_HW_rcp26.Rds"))

#------------------------ RCP 4.5-----------------------------------
##-------------------- CapeWinelands--------------------------------
###------------------------HWA--------------------------------------
setwd("~/Desktop/extreme-events-project-WesternCape/data/rcp45/output")

HWA_rcp45 <- brick('hw_ANN_climpact.sample_historical_NA_2021-2100.nc', varname = 'hwa_tx90')

# Crop the raster-CapeWinelands
HWA_Cape_Winelands_mask_rcp45 <- mask(HWA_rcp45, Cape_Winelands) %>%
  as.matrix() 

HWA_Cape_Winelands_mask_rcp45 <- HWA_Cape_Winelands_mask_rcp45[rowSums(is.na(HWA_Cape_Winelands_mask_rcp45)) != ncol(HWA_Cape_Winelands_mask_rcp45), ]
HWA_Cape_Winelands_mask_rcp45[,"X2100.07.02"] <- 2*HWA_Cape_Winelands_mask_rcp45[,"X2099.07.02"] - HWA_Cape_Winelands_mask_rcp45[,"X2098.07.02"]
HWA_Cape_Winelands_mask_rcp45 <- t(apply(HWA_Cape_Winelands_mask_rcp45, 1, na.approx))
HWA_Cape_Winelands_mask_rcp45 <- data.frame(Reduce(rbind, HWA_Cape_Winelands_mask_rcp45))
#HWN_Cape_Winelands_mask$X80 <- 2*HWM_Cape_Winelands_mask$'X79' - HWM_Cape_Winela
colnames(HWA_Cape_Winelands_mask_rcp45) <- as.character(c(2021:2100))
row.names(HWA_Cape_Winelands_mask_rcp45) <- c("grid1", "grid2", "grid3", "grid4", "grid5", "grid6", "grid7", "grid8", "grid9", "grid10", "grid11")

HWA_Cape_Winelands_mask_rcp45 <- rownames_to_column(as.data.frame(HWA_Cape_Winelands_mask_rcp45), var="grid")
HWA_Cape_Winelands_mask_rcp45 <- pivot_longer(HWA_Cape_Winelands_mask_rcp45, cols = !grid, names_to = 'year', values_to = 'hwa')
# plot the HWA in CapeWinelands

ggplot(HWA_Cape_Winelands_mask_rcp45, aes(x = year, y = hwa)) +
  geom_boxplot(color = 'red', fill='orange', alpha = 0.2) +
  ggtitle("CapeWinelands HWA (RCP 4.5)") + xlab("Year") + ylab("HWA (°C)") +
  theme_classic() +
  theme(plot.title = element_text(size=20, face="bold", hjust = 0.5), axis.title.x = element_text(size=20, face="bold" ), axis.title.y = element_text(size=20, face="bold"),  axis.text.y = element_text(size = 20))

###---------------------------HWD--------------------------------------
setwd("~/Desktop/extreme-events-project-WesternCape/data/rcp45/output")
HWD_rcp45 <- brick('hw_ANN_climpact.sample_historical_NA_2021-2100.nc', varname = 'hwd_tx90')

# Crop the raster-CapeWinelands
HWD_Cape_Winelands_mask_rcp45  <- mask(HWD_rcp45, Cape_Winelands) %>%
  as.matrix() 

HWD_Cape_Winelands_mask_rcp45 <- HWD_Cape_Winelands_mask_rcp45[rowSums(is.na(HWD_Cape_Winelands_mask_rcp45)) != ncol(HWD_Cape_Winelands_mask_rcp45), ]
HWD_Cape_Winelands_mask_rcp45[,"X2100.07.02"] <- 2*HWD_Cape_Winelands_mask_rcp45[,"X2099.07.02"] - HWD_Cape_Winelands_mask_rcp45[,"X2098.07.02"]
HWD_Cape_Winelands_mask_rcp45 <- t(apply(HWD_Cape_Winelands_mask_rcp45, 1, na.approx))
HWD_Cape_Winelands_mask_rcp45 <- data.frame(Reduce(rbind, HWD_Cape_Winelands_mask_rcp45))
colnames(HWD_Cape_Winelands_mask_rcp45) <- as.character(c(2021:2100))
row.names(HWD_Cape_Winelands_mask_rcp45) <- c("grid1", "grid2", "grid3", "grid4", "grid5", "grid6", "grid7", "grid8", "grid9", "grid10", "grid11")

HWD_Cape_Winelands_mask_rcp45 <- rownames_to_column(as.data.frame(HWD_Cape_Winelands_mask_rcp45), var="grid")
HWD_Cape_Winelands_mask_rcp45 <- pivot_longer(HWD_Cape_Winelands_mask_rcp45, cols = !grid, names_to = 'year', values_to = 'hwd')

# plot the HWD in CapeWinelands

ggplot(HWD_Cape_Winelands_mask_rcp45, aes(x = year, y = hwd)) +
  geom_boxplot(color = 'blue', fill='blue', alpha = 0.2) +
  ggtitle("CapeWinelands HWD (RCP 4.5)") + xlab("Year") + ylab("HWD (days)") +
  theme_classic() +
  theme(plot.title = element_text(size=20, face="bold", hjust = 0.5), axis.title.x = element_text(size=20, face="bold" ), axis.title.y = element_text(size=20, face="bold"), axis.text.y = element_text(size = 20))

###----------------------------HWF--------------------------------------
setwd("~/Desktop/extreme-events-project-WesternCape/data/rcp45/output")
HWF_rcp45 <- brick('hw_ANN_climpact.sample_historical_NA_2021-2100.nc', varname = 'hwf_tx90')

# Crop the raster-CapeWinelands
HWF_Cape_Winelands_mask_rcp45 <- mask(HWF_rcp45, Cape_Winelands) %>%
  as.matrix() 

HWF_Cape_Winelands_mask_rcp45 <- HWF_Cape_Winelands_mask_rcp45[rowSums(is.na(HWF_Cape_Winelands_mask_rcp45)) != ncol(HWF_Cape_Winelands_mask_rcp45), ]
HWF_Cape_Winelands_mask_rcp45[,"X2100.07.02"] <- 2*HWF_Cape_Winelands_mask_rcp45[,"X2099.07.02"] - HWF_Cape_Winelands_mask_rcp45[,"X2098.07.02"]
HWF_Cape_Winelands_mask_rcp45 <- t(apply(HWF_Cape_Winelands_mask_rcp45, 1, na.approx))
#HWN_Cape_Winelands_mask <- data.frame(Reduce(rbind, HWM_Cape_Winelands_mask))
#HWN_Cape_Winelands_mask$X80 <- 2*HWM_Cape_Winelands_mask$'X79' - HWM_Cape_Winelands_mask$'X78'
colnames(HWF_Cape_Winelands_mask_rcp45) <- as.character(c(2021:2100))
row.names(HWF_Cape_Winelands_mask_rcp45) <- c("grid1", "grid2", "grid3", "grid4", "grid5", "grid6", "grid7", "grid8", "grid9", "grid10", "grid11")

HWF_Cape_Winelands_mask_rcp45 <- rownames_to_column(as.data.frame(HWF_Cape_Winelands_mask_rcp45), var="grid")
HWF_Cape_Winelands_mask_rcp45 <- pivot_longer(HWF_Cape_Winelands_mask_rcp45, cols = !grid, names_to = 'year', values_to = 'hwf')

# plot the HWF in CapeWinelands

ggplot(HWF_Cape_Winelands_mask_rcp45, aes(x = year, y = hwf)) +
  geom_boxplot(color = 'purple', fill='purple', alpha = 0.2) +
  ggtitle("CapeWinelands HWF (RCP 4.5)") + xlab("Year") + ylab("HWF (days)") +
  theme_classic() +
  theme(plot.title = element_text(size=20, face="bold", hjust = 0.5), axis.title.x = element_text(size=20, face="bold" ), axis.title.y = element_text(size=20, face="bold"),axis.text.y = element_text(size = 20))

###----------------------------HWM--------------------------------------
setwd("~/Desktop/extreme-events-project-WesternCape/data/rcp45/output")
HWM_rcp45 <- brick('hw_ANN_climpact.sample_historical_NA_2021-2100.nc', varname = 'hwm_tx90')

# Crop the raster-CapeWinelands
HWM_Cape_Winelands_mask_rcp45  <- mask(HWM_rcp45, Cape_Winelands) %>%
  as.matrix() 

HWM_Cape_Winelands_mask_rcp45  <- HWM_Cape_Winelands_mask_rcp45[rowSums(is.na(HWM_Cape_Winelands_mask_rcp45)) != ncol(HWM_Cape_Winelands_mask_rcp45 ), ]
HWM_Cape_Winelands_mask_rcp45[,"X2100.07.02"]  <- 2*HWM_Cape_Winelands_mask_rcp45[,"X2099.07.02"]  - HWM_Cape_Winelands_mask_rcp45[,"X2098.07.02"] 
HWM_Cape_Winelands_mask_rcp45 <- t(apply(HWM_Cape_Winelands_mask_rcp45, 1, na.approx))
HWM_Cape_Winelands_mask_rcp45<- data.frame(Reduce(rbind, HWM_Cape_Winelands_mask_rcp45))
colnames(HWM_Cape_Winelands_mask_rcp45) <- as.character(c(2021:2100))
row.names(HWM_Cape_Winelands_mask_rcp45) <- c("grid1", "grid2", "grid3", "grid4", "grid5", "grid6", "grid7", "grid8", "grid9", "grid10", "grid11")
HWM_Cape_Winelands_mask_rcp45 <- rownames_to_column(as.data.frame(HWM_Cape_Winelands_mask_rcp45), var="grid")
HWM_Cape_Winelands_mask_rcp45 <- pivot_longer(HWM_Cape_Winelands_mask_rcp45, cols = !grid, names_to = 'year', values_to = 'hwm')

# plot the HWD in CapeWinelands

ggplot(HWM_Cape_Winelands_mask_rcp45, aes(x = year, y = hwm)) +
  geom_boxplot(color = 'orange', fill='orange', alpha = 0.2) +
  ggtitle("CapeWinelands HWM (RCP 4.5)") + xlab("Year") + ylab("HWM (°C)") +
  theme_classic() +
  theme(plot.title = element_text(size=20, face="bold", hjust = 0.5), axis.title.x = element_text(size=20, face="bold" ), axis.title.y = element_text(size=20, face="bold"),axis.text.y = element_text(size = 20))

###----------------------------HWN--------------------------------------
setwd("~/Desktop/extreme-events-project-WesternCape/data/rcp45/output")
HWN_rcp45 <- brick('hw_ANN_climpact.sample_historical_NA_2021-2100.nc', varname = 'hwn_tx90')

# Crop the raster-CapeWinelands
HWN_Cape_Winelands_mask_rcp45 <- mask(HWN_rcp45, Cape_Winelands) %>%
  as.matrix() 

HWN_Cape_Winelands_mask_rcp45 <- HWN_Cape_Winelands_mask_rcp45[rowSums(is.na(HWN_Cape_Winelands_mask_rcp45)) != ncol(HWN_Cape_Winelands_mask_rcp45), ]
HWN_Cape_Winelands_mask_rcp45[,"X2100.07.02"] <- 2*HWN_Cape_Winelands_mask_rcp45[,"X2099.07.02"] - HWN_Cape_Winelands_mask_rcp45[,"X2098.07.02"]
HWN_Cape_Winelands_mask_rcp45 <- t(apply(HWN_Cape_Winelands_mask_rcp45, 1, na.approx))

colnames(HWN_Cape_Winelands_mask_rcp45) <- as.character(c(2021:2100))
row.names(HWN_Cape_Winelands_mask_rcp45) <- c("grid1", "grid2", "grid3", "grid4", "grid5", "grid6", "grid7", "grid8", "grid9", "grid10", "grid11")

HWN_Cape_Winelands_mask_rcp45 <- rownames_to_column(as.data.frame(HWN_Cape_Winelands_mask_rcp45), var="grid")
HWN_Cape_Winelands_mask_rcp45 <- pivot_longer(HWN_Cape_Winelands_mask_rcp45, cols = !grid, names_to = 'year', values_to = 'hwn')

# plot the HWN in CapeWinelands

ggplot(HWN_Cape_Winelands_mask_rcp45, aes(x = year, y = hwn)) +
  geom_boxplot(color = 'pink', fill='pink', alpha = 0.2) +
  ggtitle("CapeWinelands HWN (RCP 4.5)") + xlab("Year") + ylab("HWN (events)") +
  theme_classic() +
  theme(plot.title = element_text(size=20, face="bold", hjust = 0.5), axis.title.x = element_text(size=20, face="bold" ), axis.title.y = element_text(size=20, face="bold"),axis.text.y = element_text(size = 20))

##-------------------- Central Karoo-----------------------------------
###------------------------HWA--------------------------------------
setwd("~/Desktop/extreme-events-project-WesternCape/data/rcp45/output")

HWA_rcp45 <- brick('hw_ANN_climpact.sample_historical_NA_2021-2100.nc', varname = 'hwa_tx90')

# Crop the raster-CapeWinelands
HWA_Central_Karoo_mask_rcp45 <- mask(HWA_rcp45, Central_Karoo) %>%
  as.matrix() 

HWA_Central_Karoo_mask_rcp45 <- HWA_Central_Karoo_mask_rcp45[rowSums(is.na(HWA_Central_Karoo_mask_rcp45)) != ncol(HWA_Central_Karoo_mask_rcp45), ]
HWA_Central_Karoo_mask_rcp45[,"X2100.07.02"] <- 2*HWA_Central_Karoo_mask_rcp45[,"X2099.07.02"] - HWA_Central_Karoo_mask_rcp45[,"X2098.07.02"]
HWA_Central_Karoo_mask_rcp45 <- t(apply(HWA_Central_Karoo_mask_rcp45, 1, na.approx))
colnames(HWA_Central_Karoo_mask_rcp45) <- as.character(c(2021:2100))
row.names(HWA_Central_Karoo_mask_rcp45) <- c("grid1", "grid2", "grid3", "grid4", "grid5", "grid6", "grid7", "grid8", "grid9", "grid10", "grid11", "grid12", "grid13", "grid14", "grid15", "grid16")

HWA_Central_Karoo_mask_rcp45 <- rownames_to_column(as.data.frame(HWA_Central_Karoo_mask_rcp45), var="grid")
HWA_Central_Karoo_mask_rcp45 <- pivot_longer(HWA_Central_Karoo_mask_rcp45, cols = !grid, names_to = 'year', values_to = 'hwa')
# plot the HWA in CapeWinelands

ggplot(HWA_Central_Karoo_mask_rcp45, aes(x = year, y = hwa)) +
  geom_boxplot(color = 'red', fill='orange', alpha = 0.2) +
  ggtitle("Central Karoo HWA (RCP 4.5)") + xlab("Year") + ylab("HWA (°C)") +
  theme_classic() +
  theme(plot.title = element_text(size=20, face="bold", hjust = 0.5), axis.title.x = element_text(size=20, face="bold" ), axis.title.y = element_text(size=20, face="bold"), axis.text.y = element_text(size = 20))

###---------------------------HWD--------------------------------------
setwd("~/Desktop/extreme-events-project-WesternCape/data/rcp45/output")

HWD_rcp45 <- brick('hw_ANN_climpact.sample_historical_NA_2021-2100.nc', varname = 'hwd_tx90')

# Crop the raster-CapeWinelands
HWD_Central_Karoo_mask_rcp45 <- mask(HWD_rcp45, Central_Karoo) %>%
  as.matrix() 

HWD_Central_Karoo_mask_rcp45 <- HWD_Central_Karoo_mask_rcp45[rowSums(is.na(HWD_Central_Karoo_mask_rcp45)) != ncol(HWD_Central_Karoo_mask_rcp45), ]
HWD_Central_Karoo_mask_rcp45[,"X2100.07.02"] <- 2*HWD_Central_Karoo_mask_rcp45[,"X2099.07.02"] - HWD_Central_Karoo_mask_rcp45[,"X2098.07.02"]
HWD_Central_Karoo_mask_rcp45 <- t(apply(HWD_Central_Karoo_mask_rcp45, 1, na.approx))
colnames(HWD_Central_Karoo_mask_rcp45) <- as.character(c(2021:2100))
row.names(HWD_Central_Karoo_mask_rcp45) <- c("grid1", "grid2", "grid3", "grid4", "grid5", "grid6", "grid7", "grid8", "grid9", "grid10", "grid11", "grid12", "grid13", "grid14", "grid15", "grid16")

HWD_Central_Karoo_mask_rcp45 <- rownames_to_column(as.data.frame(HWD_Central_Karoo_mask_rcp45), var="grid")
HWD_Central_Karoo_mask_rcp45 <- pivot_longer(HWD_Central_Karoo_mask_rcp45, cols = !grid, names_to = 'year', values_to = 'hwd')
# plot the HWA in CapeWinelands

ggplot(HWD_Central_Karoo_mask_rcp45, aes(x = year, y = hwd)) +
  geom_boxplot(color = 'blue', fill='blue', alpha = 0.2) +
  ggtitle("Central Karoo HWD (RCP 4.5)") + xlab("Year") + ylab("HWD (days)") +
  theme_classic() +
  theme(plot.title = element_text(size=20, face="bold", hjust = 0.5), axis.title.x = element_text(size=20, face="bold" ), axis.title.y = element_text(size=20, face="bold"),  axis.text.y = element_text(size = 20))

###----------------------------HWF--------------------------------------
setwd("~/Desktop/extreme-events-project-WesternCape/data/rcp45/output")

HWF_rcp45 <- brick('hw_ANN_climpact.sample_historical_NA_2021-2100.nc', varname = 'hwf_tx90')

# Crop the raster-CapeWinelands
HWF_Central_Karoo_mask_rcp45 <- mask(HWF_rcp45, Central_Karoo) %>%
  as.matrix() 

HWF_Central_Karoo_mask_rcp45 <- HWF_Central_Karoo_mask_rcp45[rowSums(is.na(HWF_Central_Karoo_mask_rcp45)) != ncol(HWF_Central_Karoo_mask_rcp45), ]
HWF_Central_Karoo_mask_rcp45[,"X2100.07.02"] <- 2*HWF_Central_Karoo_mask_rcp45[,"X2099.07.02"] - HWF_Central_Karoo_mask_rcp45[,"X2098.07.02"]
HWF_Central_Karoo_mask_rcp45 <- t(apply(HWF_Central_Karoo_mask_rcp45, 1, na.approx))
colnames(HWF_Central_Karoo_mask_rcp45) <- as.character(c(2021:2100))
row.names(HWF_Central_Karoo_mask_rcp45) <- c("grid1", "grid2", "grid3", "grid4", "grid5", "grid6", "grid7", "grid8", "grid9", "grid10", "grid11", "grid12", "grid13", "grid14", "grid15", "grid16")

HWF_Central_Karoo_mask_rcp45 <- rownames_to_column(as.data.frame(HWF_Central_Karoo_mask_rcp45), var="grid")
HWF_Central_Karoo_mask_rcp45 <- pivot_longer(HWF_Central_Karoo_mask_rcp45, cols = !grid, names_to = 'year', values_to = 'hwf')
# plot the HWA in CapeWinelands

ggplot(HWF_Central_Karoo_mask_rcp45, aes(x = year, y = hwf)) +
  geom_boxplot(color = 'purple', fill='purple', alpha = 0.2) +
  ggtitle("Central Karoo HWF (RCP 4.5)") + xlab("Year") + ylab("HWF (days)") +
  theme_classic() +
  theme(plot.title = element_text(size=20, face="bold", hjust = 0.5), axis.title.x = element_text(size=20, face="bold" ), axis.title.y = element_text(size=20, face="bold"), axis.text.y = element_text(size = 20))

###----------------------------HWM--------------------------------------
setwd("~/Desktop/extreme-events-project-WesternCape/data/rcp45/output")

HWM_rcp45 <- brick('hw_ANN_climpact.sample_historical_NA_2021-2100.nc', varname = 'hwm_tx90')

# Crop the raster-CapeWinelands
HWM_Central_Karoo_mask_rcp45 <- mask(HWM_rcp45, Central_Karoo) %>%
  as.matrix() 

HWM_Central_Karoo_mask_rcp45 <- HWM_Central_Karoo_mask_rcp45[rowSums(is.na(HWM_Central_Karoo_mask_rcp45)) != ncol(HWM_Central_Karoo_mask_rcp45), ]
HWM_Central_Karoo_mask_rcp45[,"X2100.07.02"] <- 2*HWM_Central_Karoo_mask_rcp45[,"X2099.07.02"] - HWM_Central_Karoo_mask_rcp45[,"X2098.07.02"]
HWM_Central_Karoo_mask_rcp45 <- t(apply(HWM_Central_Karoo_mask_rcp45, 1, na.approx))
colnames(HWM_Central_Karoo_mask_rcp45) <- as.character(c(2021:2100))
row.names(HWM_Central_Karoo_mask_rcp45) <- c("grid1", "grid2", "grid3", "grid4", "grid5", "grid6", "grid7", "grid8", "grid9", "grid10", "grid11", "grid12", "grid13", "grid14", "grid15", "grid16")

HWM_Central_Karoo_mask_rcp45 <- rownames_to_column(as.data.frame(HWM_Central_Karoo_mask_rcp45), var="grid")
HWM_Central_Karoo_mask_rcp45 <- pivot_longer(HWM_Central_Karoo_mask_rcp45, cols = !grid, names_to = 'year', values_to = 'hwm')

# plot the HWA in CapeWinelands

ggplot(HWM_Central_Karoo_mask_rcp45, aes(x = year, y = hwm)) +
  geom_boxplot(color = 'orange', fill='orange', alpha = 0.2) +
  ggtitle("Central Karoo HWM (RCP 4.5)") + xlab("Year") + ylab("HWM (°C)") +
  theme_classic() +
  theme(plot.title = element_text(size=20, face="bold", hjust = 0.5), axis.title.x = element_text(size=20, face="bold" ), axis.title.y = element_text(size=20, face="bold"), axis.text.y = element_text(size = 20))

###----------------------------HWN--------------------------------------
setwd("~/Desktop/extreme-events-project-WesternCape/data/rcp45/output")

HWN_rcp45 <- brick('hw_ANN_climpact.sample_historical_NA_2021-2100.nc', varname = 'hwn_tx90')

# Crop the raster-CapeWinelands
HWN_Central_Karoo_mask_rcp45 <- mask(HWN_rcp45, Central_Karoo) %>%
  as.matrix() 

HWN_Central_Karoo_mask_rcp45 <- HWN_Central_Karoo_mask_rcp45[rowSums(is.na(HWN_Central_Karoo_mask_rcp45)) != ncol(HWN_Central_Karoo_mask_rcp45), ]
HWN_Central_Karoo_mask_rcp45[,"X2100.07.02"] <- 2*HWN_Central_Karoo_mask_rcp45[,"X2099.07.02"] - HWN_Central_Karoo_mask_rcp45[,"X2098.07.02"]
HWN_Central_Karoo_mask_rcp45 <- t(apply(HWN_Central_Karoo_mask_rcp45, 1, na.approx))
colnames(HWN_Central_Karoo_mask_rcp45) <- as.character(c(2021:2100))
row.names(HWN_Central_Karoo_mask_rcp45) <- c("grid1", "grid2", "grid3", "grid4", "grid5", "grid6", "grid7", "grid8", "grid9", "grid10", "grid11", "grid12", "grid13", "grid14", "grid15", "grid16")

HWN_Central_Karoo_mask_rcp45 <- rownames_to_column(as.data.frame(HWN_Central_Karoo_mask_rcp45), var="grid")
HWN_Central_Karoo_mask_rcp45 <- pivot_longer(HWN_Central_Karoo_mask_rcp45, cols = !grid, names_to = 'year', values_to = 'hwn')
# plot the HWA in CapeWinelands

ggplot(HWN_Central_Karoo_mask_rcp45, aes(x = year, y = hwn)) +
  geom_boxplot(color = 'pink', fill='pink', alpha = 0.2) +
  ggtitle("Central Karoo HWN (RCP 4.5)") + xlab("Year") + ylab("HWN (events)") +
  theme_classic() +
  theme(plot.title = element_text(size=20, face="bold", hjust = 0.5), axis.title.x = element_text(size=20, face="bold" ), axis.title.y = element_text(size=20, face="bold"),axis.text.y = element_text(size = 20))

##---------------------------Eden-------------------------------------
###------------------------HWA--------------------------------------
setwd("~/Desktop/extreme-events-project-WesternCape/data/rcp45/output")

HWA_rcp45 <- brick('hw_ANN_climpact.sample_historical_NA_2021-2100.nc', varname = 'hwa_tx90')

# Crop the raster-CapeWinelands
HWA_Eden_mask_rcp45 <- mask(HWA_rcp45,Eden) %>%
  as.matrix() 

HWA_Eden_mask_rcp45 <- HWA_Eden_mask_rcp45[rowSums(is.na(HWA_Eden_mask_rcp45)) != ncol(HWA_Eden_mask_rcp45), ]
HWA_Eden_mask_rcp45[,"X2100.07.02"] <- 2*HWA_Eden_mask_rcp45[,"X2099.07.02"] - HWA_Eden_mask_rcp45[,"X2098.07.02"]
HWA_Eden_mask_rcp45 <- t(apply(HWA_Eden_mask_rcp45, 1, na.approx))
HWA_Eden_mask_rcp45 <- data.frame(Reduce(rbind, HWA_Eden_mask_rcp45))
colnames(HWA_Eden_mask_rcp45) <- as.character(c(2021:2100))
row.names(HWA_Eden_mask_rcp45) <- c("grid1", "grid2", "grid3", "grid4", "grid5", "grid6", "grid7", "grid8", "grid9", "grid10", "grid11", "grid12", "grid13", "grid14", "grid15")

HWA_Eden_mask_rcp45  <- rownames_to_column(as.data.frame(HWA_Eden_mask_rcp45 ), var="grid")
HWA_Eden_mask_rcp45  <- pivot_longer(HWA_Eden_mask_rcp45 , cols = !grid, names_to = 'year', values_to = 'hwa')
# plot the HWA in CapeWinelands

ggplot(HWA_Eden_mask_rcp45, aes(x = year, y = hwa)) +
  geom_boxplot(color = 'red', fill='orange', alpha = 0.2) +
  ggtitle("Eden HWA (RCP 4.5)") + xlab("Year") + ylab("HWA (°C)") +
  theme_classic() +
  theme(plot.title = element_text(size=20, face="bold", hjust = 0.5), axis.title.x = element_text(size=20, face="bold" ), axis.title.y = element_text(size=20, face="bold"),axis.text.y = element_text(size = 20))

###------------------------HWD--------------------------------------
setwd("~/Desktop/extreme-events-project-WesternCape/data/rcp45/output")

HWD_rcp45 <- brick('hw_ANN_climpact.sample_historical_NA_2021-2100.nc', varname = 'hwd_tx90')

# Crop the raster-CapeWinelands
HWD_Eden_mask_rcp45 <- mask(HWD_rcp45,Eden) %>%
  as.matrix() 

HWD_Eden_mask_rcp45 <- HWD_Eden_mask_rcp45[rowSums(is.na(HWD_Eden_mask_rcp45)) != ncol(HWD_Eden_mask_rcp45), ]
HWD_Eden_mask_rcp45[,"X2100.07.02"] <- 2*HWD_Eden_mask_rcp45[,"X2099.07.02"] - HWD_Eden_mask_rcp45[,"X2098.07.02"]
HWD_Eden_mask_rcp45 <- t(apply(HWD_Eden_mask_rcp45, 1, na.approx))
HWD_Eden_mask_rcp45 <- data.frame(Reduce(rbind, HWD_Eden_mask_rcp45))
colnames(HWD_Eden_mask_rcp45) <- as.character(c(2021:2100))
row.names(HWD_Eden_mask_rcp45) <- c("grid1", "grid2", "grid3", "grid4", "grid5", "grid6", "grid7", "grid8", "grid9", "grid10", "grid11", "grid12", "grid13", "grid14", "grid15")

HWD_Eden_mask_rcp45  <- rownames_to_column(as.data.frame(HWD_Eden_mask_rcp45 ), var="grid")
HWD_Eden_mask_rcp45  <- pivot_longer(HWD_Eden_mask_rcp45 , cols = !grid, names_to = 'year', values_to = 'hwd')
# plot the HWA in CapeWinelands

ggplot(HWD_Eden_mask_rcp45, aes(x = year, y = hwd)) +
  geom_boxplot(color = 'blue', fill='blue', alpha = 0.2) +
  ggtitle("Eden HWD (RCP 4.5)") + xlab("Year") + ylab("HWD (days)") +
  theme_classic() +
  theme(plot.title = element_text(size=20, face="bold", hjust = 0.5), axis.title.x = element_text(size=20, face="bold" ), axis.title.y = element_text(size=20, face="bold"),axis.text.y = element_text(size = 20))

###------------------------HWF--------------------------------------
setwd("~/Desktop/extreme-events-project-WesternCape/data/rcp45/output")

HWF_rcp45 <- brick('hw_ANN_climpact.sample_historical_NA_2021-2100.nc', varname = 'hwf_tx90')

# Crop the raster-CapeWinelands
HWF_Eden_mask_rcp45 <- mask(HWF_rcp45,Eden) %>%
  as.matrix() 

HWF_Eden_mask_rcp45 <- HWF_Eden_mask_rcp45[rowSums(is.na(HWF_Eden_mask_rcp45)) != ncol(HWF_Eden_mask_rcp45), ]
HWF_Eden_mask_rcp45[,"X2100.07.02"] <- 2*HWF_Eden_mask_rcp45[,"X2099.07.02"] - HWF_Eden_mask_rcp45[,"X2098.07.02"]
HWF_Eden_mask_rcp45 <- t(apply(HWF_Eden_mask_rcp45, 1, na.approx))
#HWD_Eden_mask_rcp85 <- data.frame(Reduce(rbind, HWD_Eden_mask_rcp85))
colnames(HWF_Eden_mask_rcp45) <- as.character(c(2021:2100))
row.names(HWF_Eden_mask_rcp45) <- c("grid1", "grid2", "grid3", "grid4", "grid5", "grid6", "grid7", "grid8", "grid9", "grid10", "grid11", "grid12", "grid13", "grid14", "grid15")

HWF_Eden_mask_rcp45  <- rownames_to_column(as.data.frame(HWF_Eden_mask_rcp45 ), var="grid")
HWF_Eden_mask_rcp45  <- pivot_longer(HWF_Eden_mask_rcp45 , cols = !grid, names_to = 'year', values_to = 'hwf')

ggplot(HWF_Eden_mask_rcp45, aes(x = year, y = hwf)) +
  geom_boxplot(color = 'purple', fill='purple', alpha = 0.2) +
  ggtitle("Eden HWF (RCP 4.5)") + xlab("Year") + ylab("HWF (days)") +
  theme_classic() +
  theme(plot.title = element_text(size=20, face="bold", hjust = 0.5), axis.title.x = element_text(size=20, face="bold" ), axis.title.y = element_text(size=20, face="bold"),axis.text.y = element_text(size = 20))

###-----------------------HWM---------------------------------------------
setwd("~/Desktop/extreme-events-project-WesternCape/data/rcp45/output")

HWM_rcp45 <- brick('hw_ANN_climpact.sample_historical_NA_2021-2100.nc', varname = 'hwm_tx90')

# Crop the raster-CapeWinelands
HWM_Eden_mask_rcp45 <- mask(HWM_rcp45,Eden) %>%
  as.matrix() 

HWM_Eden_mask_rcp45 <- HWM_Eden_mask_rcp45[rowSums(is.na(HWM_Eden_mask_rcp45)) != ncol(HWM_Eden_mask_rcp45), ]
HWM_Eden_mask_rcp45[,"X2100.07.02"] <- 2*HWM_Eden_mask_rcp45[,"X2099.07.02"] - HWM_Eden_mask_rcp45[,"X2098.07.02"]
HWM_Eden_mask_rcp45 <- t(apply(HWM_Eden_mask_rcp45, 1, na.approx))
HWM_Eden_mask_rcp45 <- data.frame(Reduce(rbind, HWM_Eden_mask_rcp45))
colnames(HWM_Eden_mask_rcp45) <- as.character(c(2021:2100))
row.names(HWM_Eden_mask_rcp45) <- c("grid1", "grid2", "grid3", "grid4", "grid5", "grid6", "grid7", "grid8", "grid9", "grid10", "grid11", "grid12", "grid13", "grid14", "grid15")

HWM_Eden_mask_rcp45  <- rownames_to_column(as.data.frame(HWM_Eden_mask_rcp45 ), var="grid")
HWM_Eden_mask_rcp45  <- pivot_longer(HWM_Eden_mask_rcp45 , cols = !grid, names_to = 'year', values_to = 'hwm')

ggplot(HWM_Eden_mask_rcp45, aes(x = year, y = hwm)) +
  geom_boxplot(color = 'orange', fill='orange', alpha = 0.2) +
  ggtitle("Eden HWM (RCP 4.5)") + xlab("Year") + ylab("HWM (°C)") +
  theme_classic() +
  theme(plot.title = element_text(size=20, face="bold", hjust = 0.5), axis.title.x = element_text(size=20, face="bold" ), axis.title.y = element_text(size=20, face="bold"),axis.text.y = element_text(size = 20))

###-----------------------HWN---------------------------------------------
setwd("~/Desktop/extreme-events-project-WesternCape/data/rcp45/output")

HWN_rcp45 <- brick('hw_ANN_climpact.sample_historical_NA_2021-2100.nc', varname = 'hwn_tx90')

# Crop the raster-CapeWinelands
HWN_Eden_mask_rcp45 <- mask(HWN_rcp45,Eden) %>%
  as.matrix() 

HWN_Eden_mask_rcp45 <- HWN_Eden_mask_rcp45[rowSums(is.na(HWN_Eden_mask_rcp45)) != ncol(HWN_Eden_mask_rcp45), ]
HWN_Eden_mask_rcp45[,"X2100.07.02"] <- 2*HWN_Eden_mask_rcp45[,"X2099.07.02"] - HWN_Eden_mask_rcp45[,"X2098.07.02"]
HWN_Eden_mask_rcp45 <- t(apply(HWN_Eden_mask_rcp45, 1, na.approx))
#HWN_Eden_mask_rcp85 <- data.frame(Reduce(rbind, HWN_Eden_mask_rcp85))
colnames(HWN_Eden_mask_rcp45) <- as.character(c(2021:2100))
row.names(HWN_Eden_mask_rcp45) <- c("grid1", "grid2", "grid3", "grid4", "grid5", "grid6", "grid7", "grid8", "grid9", "grid10", "grid11", "grid12", "grid13", "grid14", "grid15")

HWN_Eden_mask_rcp45  <- rownames_to_column(as.data.frame(HWN_Eden_mask_rcp45 ), var="grid")
HWN_Eden_mask_rcp45  <- pivot_longer(HWN_Eden_mask_rcp45 , cols = !grid, names_to = 'year', values_to = 'hwn')

ggplot(HWN_Eden_mask_rcp45, aes(x = year, y = hwn)) +
  geom_boxplot(color = 'pink', fill='pink', alpha = 0.2) +
  ggtitle("Eden HWN (RCP 4.5)") + xlab("Year") + ylab("HWN (events)") +
  theme_classic() +
  theme(plot.title = element_text(size=20, face="bold", hjust = 0.5), axis.title.x = element_text(size=20, face="bold" ), axis.title.y = element_text(size=20, face="bold"),axis.text.y = element_text(size = 20))

##------------------------Overberg---------------------------------------------
###------------------------HWA-------------------------------------------------
setwd("~/Desktop/extreme-events-project-WesternCape/data/rcp45/output")

HWA_rcp45 <- brick('hw_ANN_climpact.sample_historical_NA_2021-2100.nc', varname = 'hwa_tx90')

# Crop the raster-CapeWinelands
HWA_Overberg_mask_rcp45 <- mask(HWA_rcp45,Overberg) %>%
  as.matrix() 

HWA_Overberg_mask_rcp45 <- HWA_Overberg_mask_rcp45[rowSums(is.na(HWA_Overberg_mask_rcp45)) != ncol(HWA_Overberg_mask_rcp45), ]
HWA_Overberg_mask_rcp45[,"X2021.07.02"] <- HWA_Overberg_mask_rcp45[,"X2025.07.02"] 
HWA_Overberg_mask_rcp45[,"X2100.07.02"] <- 2*HWA_Overberg_mask_rcp45[,"X2099.07.02"] - HWA_Overberg_mask_rcp45[,"X2098.07.02"]
HWA_Overberg_mask_rcp45 <- t(apply(HWA_Overberg_mask_rcp45, 1, na.approx))
#HWA_Overberg_mask_rcp45 <- data.frame(Reduce(rbind, HWA_Overberg_mask_rcp45))
colnames(HWA_Overberg_mask_rcp45) <- as.character(c(2021:2100))
row.names(HWA_Overberg_mask_rcp45) <- c("grid1", "grid2", "grid3", "grid4", "grid5", "grid6")

HWA_Overberg_mask_rcp45  <- rownames_to_column(as.data.frame(HWA_Overberg_mask_rcp45), var="grid")
HWA_Overberg_mask_rcp45  <- pivot_longer(HWA_Overberg_mask_rcp45, cols = !grid, names_to = 'year', values_to = 'hwa')

ggplot(HWA_Overberg_mask_rcp45, aes(x = year, y = hwa)) +
  geom_boxplot(color = 'red', fill='orange', alpha = 0.2) +
  ggtitle("Overberg HWA (RCP 4.5)") + xlab("Year") + ylab("HWA (°C)") + 
  theme_classic() + 
  theme(plot.title = element_text(size=20, face="bold", hjust = 0.5), axis.title.x = element_text(size=20, face="bold" ), axis.title.y = element_text(size=20, face="bold"), axis.text.y = element_text(size = 20))

###------------------------HWD-------------------------------------------------
setwd("~/Desktop/extreme-events-project-WesternCape/data/rcp45/output")

HWD_rcp45 <- brick('hw_ANN_climpact.sample_historical_NA_2021-2100.nc', varname = 'hwd_tx90')

# Crop the raster-CapeWinelands
HWD_Overberg_mask_rcp45 <- mask(HWD_rcp45,Overberg) %>%
  as.matrix() 

HWD_Overberg_mask_rcp45 <- HWD_Overberg_mask_rcp45[rowSums(is.na(HWD_Overberg_mask_rcp45)) != ncol(HWD_Overberg_mask_rcp45), ]
HWD_Overberg_mask_rcp45[,"X2021.07.02"] <- HWD_Overberg_mask_rcp45[,"X2025.07.02"] 
HWD_Overberg_mask_rcp45[,"X2100.07.02"] <- 2*HWD_Overberg_mask_rcp45[,"X2099.07.02"] - HWD_Overberg_mask_rcp45[,"X2098.07.02"]
HWD_Overberg_mask_rcp45 <- t(apply(HWD_Overberg_mask_rcp45, 1, na.approx))
#HWD_Overberg_mask_rcp45 <- data.frame(Reduce(rbind, HWD_Overberg_mask_rcp45))
colnames(HWD_Overberg_mask_rcp45) <- as.character(c(2021:2100))
row.names(HWD_Overberg_mask_rcp45) <- c("grid1", "grid2", "grid3", "grid4", "grid5", "grid6")

HWD_Overberg_mask_rcp45  <- rownames_to_column(as.data.frame(HWD_Overberg_mask_rcp45), var="grid")
HWD_Overberg_mask_rcp45  <- pivot_longer(HWD_Overberg_mask_rcp45, cols = !grid, names_to = 'year', values_to = 'hwd')

ggplot(HWD_Overberg_mask_rcp45, aes(x = year, y = hwd)) +
  geom_boxplot(color = 'blue', fill='blue', alpha = 0.2) +
  ggtitle("Overberg HWD (RCP 4.5)") + xlab("Year") + ylab("HWD (days)") + 
  theme_classic() + 
  theme(plot.title = element_text(size=20, face="bold", hjust = 0.5), axis.title.x = element_text(size=20, face="bold" ), axis.title.y = element_text(size=20, face="bold"), axis.text.y = element_text(size = 20))

###------------------------HWF-------------------------------------------------
setwd("~/Desktop/extreme-events-project-WesternCape/data/rcp45/output")

HWF_rcp45 <- brick('hw_ANN_climpact.sample_historical_NA_2021-2100.nc', varname = 'hwf_tx90')

# Crop the raster-CapeWinelands
HWF_Overberg_mask_rcp45 <- mask(HWF_rcp45,Overberg) %>%
  as.matrix() 

HWF_Overberg_mask_rcp45 <- HWF_Overberg_mask_rcp45[rowSums(is.na(HWF_Overberg_mask_rcp45)) != ncol(HWF_Overberg_mask_rcp45), ]
HWF_Overberg_mask_rcp45[,"X2100.07.02"] <- 2*HWF_Overberg_mask_rcp45[,"X2099.07.02"] - HWF_Overberg_mask_rcp45[,"X2098.07.02"]
HWF_Overberg_mask_rcp45 <- t(apply(HWF_Overberg_mask_rcp45, 1, na.approx))
#HWD_Overberg_mask_rcp85 <- data.frame(Reduce(rbind, HWD_Overberg_mask_rcp85))
colnames(HWF_Overberg_mask_rcp45) <- as.character(c(2021:2100))
row.names(HWF_Overberg_mask_rcp45) <- c("grid1", "grid2", "grid3", "grid4", "grid5", "grid6")

HWF_Overberg_mask_rcp45  <- rownames_to_column(as.data.frame(HWF_Overberg_mask_rcp45), var="grid")
HWF_Overberg_mask_rcp45  <- pivot_longer(HWF_Overberg_mask_rcp45, cols = !grid, names_to = 'year', values_to = 'hwf')

ggplot(HWF_Overberg_mask_rcp45, aes(x = year, y = hwf)) +
  geom_boxplot(color = 'purple', fill='purple', alpha = 0.2) +
  ggtitle("Overberg HWF (RCP 4.5)") + xlab("Year") + ylab("HWF (days)") + 
  theme_classic() + 
  theme(plot.title = element_text(size=20, face="bold", hjust = 0.5), axis.title.x = element_text(size=20, face="bold" ), axis.title.y = element_text(size=20, face="bold"), axis.text.y = element_text(size = 20))

###------------------------HWM-------------------------------------------------
setwd("~/Desktop/extreme-events-project-WesternCape/data/rcp45/output")

HWM_rcp45 <- brick('hw_ANN_climpact.sample_historical_NA_2021-2100.nc', varname = 'hwm_tx90')

# Crop the raster-CapeWinelands
HWM_Overberg_mask_rcp45 <- mask(HWM_rcp45,Overberg) %>%
  as.matrix() 

HWM_Overberg_mask_rcp45 <- HWM_Overberg_mask_rcp45[rowSums(is.na(HWM_Overberg_mask_rcp45)) != ncol(HWM_Overberg_mask_rcp45), ]
HWM_Overberg_mask_rcp45[,"X2021.07.02"] <- HWM_Overberg_mask_rcp45[,"X2025.07.02"] 
HWM_Overberg_mask_rcp45[,"X2100.07.02"] <- 2*HWM_Overberg_mask_rcp45[,"X2099.07.02"] - HWM_Overberg_mask_rcp45[,"X2098.07.02"]
HWM_Overberg_mask_rcp45 <- t(apply(HWM_Overberg_mask_rcp45, 1, na.approx))
#HWM_Overberg_mask_rcp45 <- data.frame(Reduce(rbind, HWM_Overberg_mask_rcp45))
colnames(HWM_Overberg_mask_rcp45) <- as.character(c(2021:2100))
row.names(HWM_Overberg_mask_rcp45) <- c("grid1", "grid2", "grid3", "grid4", "grid5", "grid6")

HWM_Overberg_mask_rcp45  <- rownames_to_column(as.data.frame(HWM_Overberg_mask_rcp45), var="grid")
HWM_Overberg_mask_rcp45  <- pivot_longer(HWM_Overberg_mask_rcp45, cols = !grid, names_to = 'year', values_to = 'hwm')

ggplot(HWM_Overberg_mask_rcp45, aes(x = year, y = hwm)) +
  geom_boxplot(color = 'orange', fill='orange', alpha = 0.2) +
  ggtitle("Overberg HWM (RCP 4.5)") + xlab("Year") + ylab("HWM (°C)") + 
  theme_classic() + 
  theme(plot.title = element_text(size=20, face="bold", hjust = 0.5), axis.title.x = element_text(size=20, face="bold" ), axis.title.y = element_text(size=20, face="bold"), axis.text.y = element_text(size = 20))

###------------------------HWN-------------------------------------------------
setwd("~/Desktop/extreme-events-project-WesternCape/data/rcp45/output")

HWN_rcp45 <- brick('hw_ANN_climpact.sample_historical_NA_2021-2100.nc', varname = 'hwn_tx90')

# Crop the raster-CapeWinelands
HWN_Overberg_mask_rcp45 <- mask(HWN_rcp45,Overberg) %>%
  as.matrix() 

HWN_Overberg_mask_rcp45 <- HWN_Overberg_mask_rcp45[rowSums(is.na(HWN_Overberg_mask_rcp45)) != ncol(HWN_Overberg_mask_rcp45), ]
HWN_Overberg_mask_rcp45[,"X2100.07.02"] <- 2*HWN_Overberg_mask_rcp45[,"X2099.07.02"] - HWN_Overberg_mask_rcp45[,"X2098.07.02"]
HWN_Overberg_mask_rcp45 <- t(apply(HWN_Overberg_mask_rcp45, 1, na.approx))
#HWM_Overberg_mask_rcp85 <- data.frame(Reduce(rbind, HWM_Overberg_mask_rcp85))
colnames(HWN_Overberg_mask_rcp45) <- as.character(c(2021:2100))
row.names(HWN_Overberg_mask_rcp45) <- c("grid1", "grid2", "grid3", "grid4", "grid5", "grid6")

HWN_Overberg_mask_rcp45  <- rownames_to_column(as.data.frame(HWN_Overberg_mask_rcp45), var="grid")
HWN_Overberg_mask_rcp45  <- pivot_longer(HWN_Overberg_mask_rcp45, cols = !grid, names_to = 'year', values_to = 'hwn')

ggplot(HWN_Overberg_mask_rcp45, aes(x = year, y = hwn)) +
  geom_boxplot(color = 'pink', fill='pink', alpha = 0.2) +
  ggtitle("Overberg HWN (RCP 4.5)") + xlab("Year") + ylab("HWN (events)") + 
  theme_classic() + 
  theme(plot.title = element_text(size=20, face="bold", hjust = 0.5), axis.title.x = element_text(size=20, face="bold" ), axis.title.y = element_text(size=20, face="bold"), axis.text.y = element_text(size = 20))

##------------------------West_Coast---------------------------------------------
###------------------------HWA-------------------------------------------------
setwd("~/Desktop/extreme-events-project-WesternCape/data/rcp45/output")
HWA_rcp45 <- brick('hw_ANN_climpact.sample_historical_NA_2021-2100.nc', varname = 'hwa_tx90')

# Crop the raster-CapeWinelands
HWA_West_Coast_mask_rcp45 <- mask(HWA_rcp45,West_Coast) %>%
  as.matrix() 

HWA_West_Coast_mask_rcp45 <- HWA_West_Coast_mask_rcp45[rowSums(is.na(HWA_West_Coast_mask_rcp45)) != ncol(HWA_West_Coast_mask_rcp45), ]
HWA_West_Coast_mask_rcp45[,"X2100.07.02"] <- 2*HWA_West_Coast_mask_rcp45[,"X2099.07.02"] - HWA_West_Coast_mask_rcp45[,"X2098.07.02"]
HWA_West_Coast_mask_rcp45 <- t(apply(HWA_West_Coast_mask_rcp45, 1, na.approx))
HWA_West_Coast_mask_rcp45 <- data.frame(Reduce(rbind, HWA_West_Coast_mask_rcp45))
colnames(HWA_West_Coast_mask_rcp45) <- as.character(c(2021:2100))
row.names(HWA_West_Coast_mask_rcp45) <- c("grid1", "grid2", "grid3", "grid4", "grid5", "grid6", "grid7", "grid8", "grid9", "grid10", "grid11", "grid12", "grid13", "grid14", "grid15", "grid16", "grid17", "grid18", "grid19")

HWA_West_Coast_mask_rcp45  <- rownames_to_column(as.data.frame(HWA_West_Coast_mask_rcp45), var="grid")
HWA_West_Coast_mask_rcp45  <- pivot_longer(HWA_West_Coast_mask_rcp45, cols = !grid, names_to = 'year', values_to = 'hwa')

ggplot(HWA_West_Coast_mask_rcp45, aes(x = year, y = hwa)) +
  geom_boxplot(color = 'red', fill='orange', alpha = 0.2) +
  ggtitle("West Coast HWA (RCP 4.5)") + xlab("Year") + ylab("HWA (°C)") + 
  theme_classic() + 
  theme(plot.title = element_text(size=20, face="bold", hjust = 0.5), axis.title.x = element_text(size=20, face="bold" ), axis.title.y = element_text(size=20, face="bold"), axis.text.y = element_text(size = 20))

###------------------------HWD-------------------------------------------------
setwd("~/Desktop/extreme-events-project-WesternCape/data/rcp45/output")
HWD_rcp45 <- brick('hw_ANN_climpact.sample_historical_NA_2021-2100.nc', varname = 'hwd_tx90')

# Crop the raster-CapeWinelands
HWD_West_Coast_mask_rcp45 <- mask(HWD_rcp45,West_Coast) %>%
  as.matrix() 

HWD_West_Coast_mask_rcp45 <- HWD_West_Coast_mask_rcp45[rowSums(is.na(HWD_West_Coast_mask_rcp45)) != ncol(HWD_West_Coast_mask_rcp45), ]
HWD_West_Coast_mask_rcp45[,"X2021.07.02"] <- HWD_West_Coast_mask_rcp45[,"X2022.07.02"]
HWD_West_Coast_mask_rcp45[,"X2100.07.02"] <- 2*HWD_West_Coast_mask_rcp45[,"X2099.07.02"] - HWD_West_Coast_mask_rcp45[,"X2098.07.02"]
HWD_West_Coast_mask_rcp45 <- t(apply(HWD_West_Coast_mask_rcp45, 1, na.approx))
#HWA_West_Coast_mask_rcp85 <- data.frame(Reduce(rbind, HWA_West_Coast_mask_rcp85))
colnames(HWD_West_Coast_mask_rcp45) <- as.character(c(2021:2100))
row.names(HWD_West_Coast_mask_rcp45) <- c("grid1", "grid2", "grid3", "grid4", "grid5", "grid6", "grid7", "grid8", "grid9", "grid10", "grid11", "grid12", "grid13", "grid14", "grid15", "grid16", "grid17", "grid18", "grid19")

HWD_West_Coast_mask_rcp45  <- rownames_to_column(as.data.frame(HWD_West_Coast_mask_rcp45), var="grid")
HWD_West_Coast_mask_rcp45  <- pivot_longer(HWD_West_Coast_mask_rcp45, cols = !grid, names_to = 'year', values_to = 'hwd')

ggplot(HWD_West_Coast_mask_rcp45, aes(x = year, y = hwd)) +
  geom_boxplot(color = 'blue', fill='blue', alpha = 0.2) +
  ggtitle("West Coast HWD (RCP 4.5)") + xlab("Year") + ylab("HWD (days)") + 
  theme_classic() + 
  theme(plot.title = element_text(size=20, face="bold", hjust = 0.5), axis.title.x = element_text(size=20, face="bold" ), axis.title.y = element_text(size=20, face="bold"), axis.text.y = element_text(size = 20))

###------------------------HWF-------------------------------------------------
setwd("~/Desktop/extreme-events-project-WesternCape/data/rcp45/output")
HWF_rcp45 <- brick('hw_ANN_climpact.sample_historical_NA_2021-2100.nc', varname = 'hwf_tx90')

# Crop the raster-CapeWinelands
HWF_West_Coast_mask_rcp45 <- mask(HWF_rcp45,West_Coast) %>%
  as.matrix() 

HWF_West_Coast_mask_rcp45 <- HWF_West_Coast_mask_rcp45[rowSums(is.na(HWF_West_Coast_mask_rcp45)) != ncol(HWF_West_Coast_mask_rcp45), ]
HWF_West_Coast_mask_rcp45[,"X2100.07.02"] <- 2*HWF_West_Coast_mask_rcp45[,"X2099.07.02"] - HWF_West_Coast_mask_rcp45[,"X2098.07.02"]
HWF_West_Coast_mask_rcp45 <- t(apply(HWF_West_Coast_mask_rcp45, 1, na.approx))
#HWA_West_Coast_mask_rcp85 <- data.frame(Reduce(rbind, HWA_West_Coast_mask_rcp85))
colnames(HWF_West_Coast_mask_rcp45) <- as.character(c(2021:2100))
row.names(HWF_West_Coast_mask_rcp45) <- c("grid1", "grid2", "grid3", "grid4", "grid5", "grid6", "grid7", "grid8", "grid9", "grid10", "grid11", "grid12", "grid13", "grid14", "grid15", "grid16", "grid17", "grid18", "grid19")

HWF_West_Coast_mask_rcp45  <- rownames_to_column(as.data.frame(HWF_West_Coast_mask_rcp45), var="grid")
HWF_West_Coast_mask_rcp45  <- pivot_longer(HWF_West_Coast_mask_rcp45, cols = !grid, names_to = 'year', values_to = 'hwf')

ggplot(HWF_West_Coast_mask_rcp45, aes(x = year, y = hwf)) +
  geom_boxplot(color = 'purple', fill='purple', alpha = 0.2) +
  ggtitle("West Coast HWF (RCP 4.5)") + xlab("Year") + ylab("HWF (days)") + 
  theme_classic() + 
  theme(plot.title = element_text(size=20, face="bold", hjust = 0.5), axis.title.x = element_text(size=20, face="bold" ), axis.title.y = element_text(size=20, face="bold"), axis.text.y = element_text(size = 20))

###------------------------HWM-------------------------------------------------
setwd("~/Desktop/extreme-events-project-WesternCape/data/rcp45/output")
HWM_rcp45 <- brick('hw_ANN_climpact.sample_historical_NA_2021-2100.nc', varname = 'hwm_tx90')

# Crop the raster-CapeWinelands
HWM_West_Coast_mask_rcp45 <- mask(HWM_rcp45,West_Coast) %>%
  as.matrix() 

HWM_West_Coast_mask_rcp45 <- HWM_West_Coast_mask_rcp45[rowSums(is.na(HWM_West_Coast_mask_rcp45)) != ncol(HWM_West_Coast_mask_rcp45), ]
HWM_West_Coast_mask_rcp45[,"X2100.07.02"] <- 2*HWM_West_Coast_mask_rcp45[,"X2099.07.02"] - HWM_West_Coast_mask_rcp45[,"X2098.07.02"]
HWM_West_Coast_mask_rcp45 <- t(apply(HWM_West_Coast_mask_rcp45, 1, na.approx))
HWM_West_Coast_mask_rcp45 <- data.frame(Reduce(rbind, HWM_West_Coast_mask_rcp45))
colnames(HWM_West_Coast_mask_rcp45) <- as.character(c(2021:2100))
row.names(HWM_West_Coast_mask_rcp45) <- c("grid1", "grid2", "grid3", "grid4", "grid5", "grid6", "grid7", "grid8", "grid9", "grid10", "grid11", "grid12", "grid13", "grid14", "grid15", "grid16", "grid17", "grid18", "grid19")

HWM_West_Coast_mask_rcp45  <- rownames_to_column(as.data.frame(HWM_West_Coast_mask_rcp45), var="grid")
HWM_West_Coast_mask_rcp45  <- pivot_longer(HWM_West_Coast_mask_rcp45, cols = !grid, names_to = 'year', values_to = 'hwm')

ggplot(HWM_West_Coast_mask_rcp45, aes(x = year, y = hwm)) +
  geom_boxplot(color = 'orange', fill='orange', alpha = 0.2) +
  ggtitle("West Coast HWM (RCP 4.5)") + xlab("Year") + ylab("HWM (°C)") + 
  theme_classic() + 
  theme(plot.title = element_text(size=20, face="bold", hjust = 0.5), axis.title.x = element_text(size=20, face="bold" ), axis.title.y = element_text(size=20, face="bold"), axis.text.y = element_text(size = 20))

###------------------------HWN-------------------------------------------------
setwd("~/Desktop/extreme-events-project-WesternCape/data/rcp45/output")
HWN_rcp45 <- brick('hw_ANN_climpact.sample_historical_NA_2021-2100.nc', varname = 'hwn_tx90')

# Crop the raster-CapeWinelands
HWN_West_Coast_mask_rcp45 <- mask(HWN_rcp45,West_Coast) %>%
  as.matrix() 

HWN_West_Coast_mask_rcp45 <- HWN_West_Coast_mask_rcp45[rowSums(is.na(HWN_West_Coast_mask_rcp45)) != ncol(HWN_West_Coast_mask_rcp45), ]
HWN_West_Coast_mask_rcp45[,"X2100.07.02"] <- 2*HWN_West_Coast_mask_rcp45[,"X2099.07.02"] - HWN_West_Coast_mask_rcp45[,"X2098.07.02"]
HWN_West_Coast_mask_rcp45 <- t(apply(HWN_West_Coast_mask_rcp45, 1, na.approx))
#HWA_West_Coast_mask_rcp85 <- data.frame(Reduce(rbind, HWA_West_Coast_mask_rcp85))
colnames(HWN_West_Coast_mask_rcp45) <- as.character(c(2021:2100))
row.names(HWN_West_Coast_mask_rcp45) <- c("grid1", "grid2", "grid3", "grid4", "grid5", "grid6", "grid7", "grid8", "grid9", "grid10", "grid11", "grid12", "grid13", "grid14", "grid15", "grid16", "grid17", "grid18", "grid19")

HWN_West_Coast_mask_rcp45  <- rownames_to_column(as.data.frame(HWN_West_Coast_mask_rcp45), var="grid")
HWN_West_Coast_mask_rcp45  <- pivot_longer(HWN_West_Coast_mask_rcp45, cols = !grid, names_to = 'year', values_to = 'hwn')

ggplot(HWN_West_Coast_mask_rcp45, aes(x = year, y = hwn)) +
  geom_boxplot(color = 'pink', fill='pink', alpha = 0.2) +
  ggtitle("West Coast HWN (RCP 4.5)") + xlab("Year") + ylab("HWN (events)") + 
  theme_classic() + 
  theme(plot.title = element_text(size=20, face="bold", hjust = 0.5), axis.title.x = element_text(size=20, face="bold" ), axis.title.y = element_text(size=20, face="bold"), axis.text.y = element_text(size = 20))

##------------------------City_Cape_Town---------------------------------------------
###------------------------HWA-------------------------------------------------
setwd("~/Desktop/extreme-events-project-WesternCape/data/rcp45/output")
HWA_rcp45 <- brick('hw_ANN_climpact.sample_historical_NA_2021-2100.nc', varname = 'hwa_tx90')

# Crop the raster-CapeWinelands
HWA_City_Cape_Town_mask_rcp45 <- mask(HWA_rcp45,buffer(City_Cape_Town, width = 0.05)) %>%
  as.matrix() 

HWA_City_Cape_Town_mask_rcp45 <- HWA_City_Cape_Town_mask_rcp45[rowSums(is.na(HWA_City_Cape_Town_mask_rcp45)) != ncol(HWA_City_Cape_Town_mask_rcp45), ]
HWA_City_Cape_Town_mask_rcp45[,"X2100.07.02"] <- 2*HWA_City_Cape_Town_mask_rcp45[,"X2099.07.02"] - HWA_City_Cape_Town_mask_rcp45[,"X2098.07.02"]
HWA_City_Cape_Town_mask_rcp45 <- t(apply(HWA_City_Cape_Town_mask_rcp45, 1, na.approx))
#HWA_West_Coast_mask_rcp85 <- data.frame(Reduce(rbind, HWA_West_Coast_mask_rcp85))
colnames(HWA_City_Cape_Town_mask_rcp45) <- as.character(c(2021:2100))
row.names(HWA_City_Cape_Town_mask_rcp45) <- c("grid1", "grid2", "grid3")

HWA_City_Cape_Town_mask_rcp45  <- rownames_to_column(as.data.frame(HWA_City_Cape_Town_mask_rcp45), var="grid")
HWA_City_Cape_Town_mask_rcp45  <- pivot_longer(HWA_City_Cape_Town_mask_rcp45, cols = !grid, names_to = 'year', values_to = 'hwa')

ggplot(HWA_City_Cape_Town_mask_rcp45, aes(x = year, y = hwa)) +
  geom_boxplot(color = 'red', fill='orange', alpha = 0.2) +
  ggtitle("City of Cape Town HWA (RCP 4.5)") + xlab("Year") + ylab("HWA (°C)") + 
  theme_classic() + 
  theme(plot.title = element_text(size=20, face="bold", hjust = 0.5), axis.title.x = element_text(size=20, face="bold" ), axis.title.y = element_text(size=20, face="bold"), axis.text.y = element_text(size = 20))

###------------------------HWD-------------------------------------------------
setwd("~/Desktop/extreme-events-project-WesternCape/data/rcp45/output")
HWD_rcp45 <- brick('hw_ANN_climpact.sample_historical_NA_2021-2100.nc', varname = 'hwd_tx90')

# Crop the raster-CapeWinelands
HWD_City_Cape_Town_mask_rcp45 <- mask(HWD_rcp45,buffer(City_Cape_Town, width = 0.05)) %>%
  as.matrix() 

HWD_City_Cape_Town_mask_rcp45 <- HWD_City_Cape_Town_mask_rcp45[rowSums(is.na(HWD_City_Cape_Town_mask_rcp45)) != ncol(HWD_City_Cape_Town_mask_rcp45), ]
HWD_City_Cape_Town_mask_rcp45[,"X2100.07.02"] <- 2*HWD_City_Cape_Town_mask_rcp45[,"X2099.07.02"] - HWD_City_Cape_Town_mask_rcp45[,"X2098.07.02"]
HWD_City_Cape_Town_mask_rcp45 <- t(apply(HWD_City_Cape_Town_mask_rcp45, 1, na.approx))
#HWA_West_Coast_mask_rcp85 <- data.frame(Reduce(rbind, HWA_West_Coast_mask_rcp85))
colnames(HWD_City_Cape_Town_mask_rcp45) <- as.character(c(2021:2100))
row.names(HWD_City_Cape_Town_mask_rcp45) <- c("grid1", "grid2", "grid3")

HWD_City_Cape_Town_mask_rcp45  <- rownames_to_column(as.data.frame(HWD_City_Cape_Town_mask_rcp45), var="grid")
HWD_City_Cape_Town_mask_rcp45  <- pivot_longer(HWD_City_Cape_Town_mask_rcp45, cols = !grid, names_to = 'year', values_to = 'hwd')

ggplot(HWD_City_Cape_Town_mask_rcp45, aes(x = year, y = hwd)) +
  geom_boxplot(color = 'blue', fill='blue', alpha = 0.2) +
  ggtitle("City of Cape Town HWD (RCP 4.5)") + xlab("Year") + ylab("HWD (days)") + 
  theme_classic() + 
  theme(plot.title = element_text(size=20, face="bold", hjust = 0.5), axis.title.x = element_text(size=20, face="bold" ), axis.title.y = element_text(size=20, face="bold"), axis.text.y = element_text(size = 20))

###------------------------HWF-------------------------------------------------
setwd("~/Desktop/extreme-events-project-WesternCape/data/rcp45/output")
HWF_rcp45 <- brick('hw_ANN_climpact.sample_historical_NA_2021-2100.nc', varname = 'hwf_tx90')

# Crop the raster-CapeWinelands
HWF_City_Cape_Town_mask_rcp45 <- mask(HWF_rcp45,buffer(City_Cape_Town, width = 0.05)) %>%
  as.matrix() 

HWF_City_Cape_Town_mask_rcp45 <- HWF_City_Cape_Town_mask_rcp45[rowSums(is.na(HWF_City_Cape_Town_mask_rcp45)) != ncol(HWF_City_Cape_Town_mask_rcp45), ]
HWF_City_Cape_Town_mask_rcp45[,"X2100.07.02"] <- 2*HWF_City_Cape_Town_mask_rcp45[,"X2099.07.02"] - HWF_City_Cape_Town_mask_rcp45[,"X2098.07.02"]
HWF_City_Cape_Town_mask_rcp45 <- t(apply(HWF_City_Cape_Town_mask_rcp45, 1, na.approx))
#HWA_West_Coast_mask_rcp85 <- data.frame(Reduce(rbind, HWA_West_Coast_mask_rcp85))
colnames(HWF_City_Cape_Town_mask_rcp45) <- as.character(c(2021:2100))
row.names(HWF_City_Cape_Town_mask_rcp45) <- c("grid1", "grid2", "grid3")

HWF_City_Cape_Town_mask_rcp45  <- rownames_to_column(as.data.frame(HWF_City_Cape_Town_mask_rcp45), var="grid")
HWF_City_Cape_Town_mask_rcp45  <- pivot_longer(HWF_City_Cape_Town_mask_rcp45, cols = !grid, names_to = 'year', values_to = 'hwf')

ggplot(HWF_City_Cape_Town_mask_rcp45, aes(x = year, y = hwf)) +
  geom_boxplot(color = 'purple', fill='purple', alpha = 0.2) +
  ggtitle("City of Cape Town HWF (RCP 4.5)") + xlab("Year") + ylab("HWF (days)") + 
  theme_classic() + 
  theme(plot.title = element_text(size=20, face="bold", hjust = 0.5), axis.title.x = element_text(size=20, face="bold" ), axis.title.y = element_text(size=20, face="bold"), axis.text.y = element_text(size = 20))

###------------------------HWM-------------------------------------------------
setwd("~/Desktop/extreme-events-project-WesternCape/data/rcp45/output")
HWM_rcp45 <- brick('hw_ANN_climpact.sample_historical_NA_2021-2100.nc', varname = 'hwm_tx90')

# Crop the raster-CapeWinelands
HWM_City_Cape_Town_mask_rcp45 <- mask(HWM_rcp45,buffer(City_Cape_Town, width = 0.05)) %>%
  as.matrix() 

HWM_City_Cape_Town_mask_rcp45 <- HWM_City_Cape_Town_mask_rcp45[rowSums(is.na(HWM_City_Cape_Town_mask_rcp45)) != ncol(HWM_City_Cape_Town_mask_rcp45), ]
HWM_City_Cape_Town_mask_rcp45[,"X2100.07.02"] <- 2*HWM_City_Cape_Town_mask_rcp45[,"X2099.07.02"] - HWM_City_Cape_Town_mask_rcp45[,"X2098.07.02"]
HWM_City_Cape_Town_mask_rcp45 <- t(apply(HWM_City_Cape_Town_mask_rcp45, 1, na.approx))
#HWA_West_Coast_mask_rcp85 <- data.frame(Reduce(rbind, HWA_West_Coast_mask_rcp85))
colnames(HWM_City_Cape_Town_mask_rcp45) <- as.character(c(2021:2100))
row.names(HWM_City_Cape_Town_mask_rcp45) <- c("grid1", "grid2", "grid3")

HWM_City_Cape_Town_mask_rcp45  <- rownames_to_column(as.data.frame(HWM_City_Cape_Town_mask_rcp45), var="grid")
HWM_City_Cape_Town_mask_rcp45  <- pivot_longer(HWM_City_Cape_Town_mask_rcp45, cols = !grid, names_to = 'year', values_to = 'hwm')

ggplot(HWM_City_Cape_Town_mask_rcp45, aes(x = year, y = hwm)) +
  geom_boxplot(color = 'orange', fill='orange', alpha = 0.2) +
  ggtitle("City of Cape Town HWM (RCP 4.5)") + xlab("Year") + ylab("HWM (days)") + 
  theme_classic() + 
  theme(plot.title = element_text(size=20, face="bold", hjust = 0.5), axis.title.x = element_text(size=20, face="bold" ), axis.title.y = element_text(size=20, face="bold"), axis.text.y = element_text(size = 20))

###------------------------HWN-------------------------------------------------
setwd("~/Desktop/extreme-events-project-WesternCape/data/rcp45/output")
HWN_rcp45 <- brick('hw_ANN_climpact.sample_historical_NA_2021-2100.nc', varname = 'hwn_tx90')

# Crop the raster-CapeWinelands
HWN_City_Cape_Town_mask_rcp45 <- mask(HWN_rcp45,buffer(City_Cape_Town, width = 0.05)) %>%
  as.matrix() 

HWN_City_Cape_Town_mask_rcp45 <- HWN_City_Cape_Town_mask_rcp45[rowSums(is.na(HWN_City_Cape_Town_mask_rcp45)) != ncol(HWN_City_Cape_Town_mask_rcp45), ]
HWN_City_Cape_Town_mask_rcp45[,"X2100.07.02"] <- 2*HWN_City_Cape_Town_mask_rcp45[,"X2099.07.02"] - HWN_City_Cape_Town_mask_rcp45[,"X2098.07.02"]
HWN_City_Cape_Town_mask_rcp45 <- t(apply(HWN_City_Cape_Town_mask_rcp45, 1, na.approx))
#HWA_West_Coast_mask_rcp85 <- data.frame(Reduce(rbind, HWA_West_Coast_mask_rcp85))
colnames(HWN_City_Cape_Town_mask_rcp45) <- as.character(c(2021:2100))
row.names(HWN_City_Cape_Town_mask_rcp45) <- c("grid1", "grid2", "grid3")

HWN_City_Cape_Town_mask_rcp45  <- rownames_to_column(as.data.frame(HWN_City_Cape_Town_mask_rcp45), var="grid")
HWN_City_Cape_Town_mask_rcp45  <- pivot_longer(HWN_City_Cape_Town_mask_rcp45, cols = !grid, names_to = 'year', values_to = 'hwn')

ggplot(HWN_City_Cape_Town_mask_rcp45, aes(x = year, y = hwn)) +
  geom_boxplot(color = 'pink', fill='pink', alpha = 0.2) +
  ggtitle("City of Cape Town HWN (RCP 4.5)") + xlab("Year") + ylab("HWN (events)") + 
  theme_classic() + 
  theme(plot.title = element_text(size=20, face="bold", hjust = 0.5), axis.title.x = element_text(size=20, face="bold" ), axis.title.y = element_text(size=20, face="bold"), axis.text.y = element_text(size = 20))

#--------------------------Save-Outputs-RCP4.5----------------------------------
write_rds(list(HWA_Cape_Winelands_rcp45 = HWA_Cape_Winelands_mask_rcp45, HWD_Cape_Winelands_rcp45 = HWD_Cape_Winelands_mask_rcp45, HWF_Cape_Winelands_rcp45 = HWF_Cape_Winelands_mask_rcp45,
               HWM_Cape_Winelands_rcp45 = HWM_Cape_Winelands_mask_rcp45, HWN_Cape_Winelands_rcp45 = HWN_Cape_Winelands_mask_rcp45, HWA_Central_Karoo_rcp45 =  HWA_Central_Karoo_mask_rcp45,
               HWD_Central_Karoo_rcp45 = HWD_Central_Karoo_mask_rcp45, HWF_Central_Karoo_rcp45 = HWF_Central_Karoo_mask_rcp45, HWM_Central_Karoo_rcp45 =  HWM_Central_Karoo_mask_rcp45,
               HWN_Central_Karoo_rcp45 = HWN_Central_Karoo_mask_rcp45, HWA_City_Cape_Town_rcp45 = HWA_City_Cape_Town_mask_rcp45, HWD_City_Cape_Town_rcp45 = HWD_City_Cape_Town_mask_rcp45,
               HWF_City_Cape_Town_rcp45 =  HWF_City_Cape_Town_mask_rcp45, HWM_City_Cape_Town_rcp45 =  HWM_City_Cape_Town_mask_rcp45, HWN_City_Cape_Town_rcp45 = HWN_City_Cape_Town_mask_rcp45,
               HWA_Eden_rcp45 =  HWA_Eden_mask_rcp45, HWD_Eden_rcp45 =  HWD_Eden_mask_rcp45, HWF_Eden_rcp45 =  HWF_Eden_mask_rcp45, HWM_Eden_rcp45 = HWM_Eden_mask_rcp45, HWN_Eden_rcp45 =  HWN_Eden_mask_rcp45,
               HWA_Overberg_rcp45 =  HWA_Overberg_mask_rcp45, HWD_Overberg_rcp45 =  HWD_Overberg_mask_rcp45,HWF_Overberg_rcp45 = HWF_Overberg_mask_rcp45,HWM_Overberg_rcp45 = HWM_Overberg_mask_rcp45, HWN_Overberg_rcp45 = HWN_Overberg_mask_rcp45,
               HWA_West_Coast_rcp45 = HWA_West_Coast_mask_rcp45, HWD_West_Coast_rcp45 = HWD_West_Coast_mask_rcp45, HWF_West_Coast_rcp45 = HWF_West_Coast_mask_rcp45, HWM_West_Coast_rcp45 = HWM_West_Coast_mask_rcp45, HWN_West_Coast_rcp45 = HWN_West_Coast_mask_rcp45),
          path = file.path(data_dir ="/Users/hebowen/Desktop/extreme-events-project-WesternCape/data/rcp45", "regional_HW_rcp45.Rds"))


#------------------------ RCP 8.5-----------------------------------
##-------------------- CapeWinelands--------------------------------
###------------------------HWA--------------------------------------
setwd("~/Desktop/extreme-events-project-WesternCape/data/rcp85/output")

HWA_rcp85 <- brick('hw_ANN_climpact.sample_historical_NA_2021-2100.nc', varname = 'hwa_tx90')
#lon <- ncvar_get(HW, "lon")
#lat <- ncvar_get(HW, "lat", verbose = F)
#plot(HWA)

# Crop the raster-CapeWinelands
HWA_Cape_Winelands_mask_rcp85 <- mask(HWA_rcp85, Cape_Winelands) %>%
  as.matrix() 

HWA_Cape_Winelands_mask_rcp85 <- HWA_Cape_Winelands_mask_rcp85[rowSums(is.na(HWA_Cape_Winelands_mask_rcp85)) != ncol(HWA_Cape_Winelands_mask_rcp85), ]
HWA_Cape_Winelands_mask_rcp85[,"X2100.07.02"] <- 2*HWA_Cape_Winelands_mask_rcp85[,"X2099.07.02"] - HWA_Cape_Winelands_mask_rcp85[,"X2098.07.02"]
HWA_Cape_Winelands_mask_rcp85 <- t(apply(HWA_Cape_Winelands_mask_rcp85, 1, na.approx))
colnames(HWA_Cape_Winelands_mask_rcp85) <- as.character(c(2021:2100))
row.names(HWA_Cape_Winelands_mask_rcp85) <- c("grid1", "grid2", "grid3", "grid4", "grid5", "grid6", "grid7", "grid8", "grid9", "grid10", "grid11")

HWA_Cape_Winelands_mask_rcp85 <- rownames_to_column(as.data.frame(HWA_Cape_Winelands_mask_rcp85), var="grid")
HWA_Cape_Winelands_mask_rcp85 <- pivot_longer(HWA_Cape_Winelands_mask_rcp85, cols = !grid, names_to = 'year', values_to = 'hwa')
# plot the HWA in CapeWinelands

ggplot(HWA_Cape_Winelands_mask_rcp85, aes(x = year, y = hwa)) +
  geom_boxplot(color = 'red', fill='orange', alpha = 0.2) +
  ggtitle("CapeWinelands HWA (RCP 8.5)") + xlab("Year") + ylab("HWA (°C)") +
  theme_classic() +
  theme(plot.title = element_text(size=20, face="bold", hjust = 0.5), axis.title.x = element_text(size=20, face="bold" ), axis.title.y = element_text(size=20, face="bold"),  axis.text.y = element_text(size = 20))

# geom_line(aes(x=year, y=hwa, color=grid, group = 11)) + geom_point() +  theme_test()

###---------------------------HWD--------------------------------------
setwd("~/Desktop/extreme-events-project-WesternCape/data/rcp85/output")
HWD_rcp85 <- brick('hw_ANN_climpact.sample_historical_NA_2021-2100.nc', varname = 'hwd_tx90')

# Crop the raster-CapeWinelands
HWD_Cape_Winelands_mask_rcp85  <- mask(HWD_rcp85, Cape_Winelands) %>%
  as.matrix() 

HWD_Cape_Winelands_mask_rcp85 <- HWD_Cape_Winelands_mask_rcp85[rowSums(is.na(HWD_Cape_Winelands_mask_rcp85)) != ncol(HWD_Cape_Winelands_mask_rcp85), ]
HWD_Cape_Winelands_mask_rcp85[,"X2100.07.02"] <- 2*HWD_Cape_Winelands_mask_rcp85[,"X2099.07.02"] - HWD_Cape_Winelands_mask_rcp85[,"X2098.07.02"]
HWD_Cape_Winelands_mask_rcp85 <- t(apply(HWD_Cape_Winelands_mask_rcp85, 1, na.approx))

colnames(HWD_Cape_Winelands_mask_rcp85) <- as.character(c(2021:2100))
row.names(HWD_Cape_Winelands_mask_rcp85) <- c("grid1", "grid2", "grid3", "grid4", "grid5", "grid6", "grid7", "grid8", "grid9", "grid10", "grid11")

HWD_Cape_Winelands_mask_rcp85 <- rownames_to_column(as.data.frame(HWD_Cape_Winelands_mask_rcp85), var="grid")
HWD_Cape_Winelands_mask_rcp85 <- pivot_longer(HWD_Cape_Winelands_mask_rcp85, cols = !grid, names_to = 'year', values_to = 'hwd')

# plot the HWD in CapeWinelands

ggplot(HWD_Cape_Winelands_mask_rcp85, aes(x = year, y = hwd)) +
  geom_boxplot(color = 'blue', fill='blue', alpha = 0.2) +
  ggtitle("CapeWinelands HWD (RCP 8.5)") + xlab("Year") + ylab("HWD (days)") +
  theme_classic() +
  theme(plot.title = element_text(size=20, face="bold", hjust = 0.5), axis.title.x = element_text(size=20, face="bold" ), axis.title.y = element_text(size=20, face="bold"), axis.text.y = element_text(size = 20))

###----------------------------HWF--------------------------------------
setwd("~/Desktop/extreme-events-project-WesternCape/data/rcp85/output")
HWF_rcp85 <- brick('hw_ANN_climpact.sample_historical_NA_2021-2100.nc', varname = 'hwf_tx90')

# Crop the raster-CapeWinelands
HWF_Cape_Winelands_mask_rcp85 <- mask(HWF_rcp85, Cape_Winelands) %>%
  as.matrix() 

HWF_Cape_Winelands_mask_rcp85 <- HWF_Cape_Winelands_mask_rcp85[rowSums(is.na(HWF_Cape_Winelands_mask_rcp85)) != ncol(HWF_Cape_Winelands_mask_rcp85), ]
HWF_Cape_Winelands_mask_rcp85[,"X2100.07.02"] <- 2*HWF_Cape_Winelands_mask_rcp85[,"X2099.07.02"] - HWF_Cape_Winelands_mask_rcp85[,"X2098.07.02"]
HWF_Cape_Winelands_mask_rcp85 <- t(apply(HWF_Cape_Winelands_mask_rcp85, 1, na.approx))
#HWN_Cape_Winelands_mask <- data.frame(Reduce(rbind, HWM_Cape_Winelands_mask))
#HWN_Cape_Winelands_mask$X80 <- 2*HWM_Cape_Winelands_mask$'X79' - HWM_Cape_Winelands_mask$'X78'
colnames(HWF_Cape_Winelands_mask_rcp85) <- as.character(c(2021:2100))
row.names(HWF_Cape_Winelands_mask_rcp85) <- c("grid1", "grid2", "grid3", "grid4", "grid5", "grid6", "grid7", "grid8", "grid9", "grid10", "grid11")

HWF_Cape_Winelands_mask_rcp85 <- rownames_to_column(as.data.frame(HWF_Cape_Winelands_mask_rcp85), var="grid")
HWF_Cape_Winelands_mask_rcp85 <- pivot_longer(HWF_Cape_Winelands_mask_rcp85, cols = !grid, names_to = 'year', values_to = 'hwf')

# plot the HWF in CapeWinelands

ggplot(HWF_Cape_Winelands_mask_rcp85, aes(x = year, y = hwf)) +
  geom_boxplot(color = 'purple', fill='purple', alpha = 0.2) +
  ggtitle("CapeWinelands HWF (RCP 8.5)") + xlab("Year") + ylab("HWF (days)") +
  theme_classic() +
  theme(plot.title = element_text(size=20, face="bold", hjust = 0.5), axis.title.x = element_text(size=20, face="bold" ), axis.title.y = element_text(size=20, face="bold"),axis.text.y = element_text(size = 20))


###----------------------------HWM--------------------------------------
setwd("~/Desktop/extreme-events-project-WesternCape/data/rcp85/output")
HWM_rcp85 <- brick('hw_ANN_climpact.sample_historical_NA_2021-2100.nc', varname = 'hwm_tx90')

# Crop the raster-CapeWinelands
HWM_Cape_Winelands_mask_rcp85  <- mask(HWM_rcp85, Cape_Winelands) %>%
  as.matrix() 

HWM_Cape_Winelands_mask_rcp85  <- HWM_Cape_Winelands_mask_rcp85[rowSums(is.na(HWM_Cape_Winelands_mask_rcp85)) != ncol(HWM_Cape_Winelands_mask_rcp85 ), ]
HWM_Cape_Winelands_mask_rcp85[,"X2100.07.02"]  <- 2*HWM_Cape_Winelands_mask_rcp85[,"X2099.07.02"]  - HWM_Cape_Winelands_mask_rcp85[,"X2098.07.02"] 
HWM_Cape_Winelands_mask_rcp85 <- t(apply(HWM_Cape_Winelands_mask_rcp85, 1, na.approx))

colnames(HWM_Cape_Winelands_mask_rcp85) <- as.character(c(2021:2100))
row.names(HWM_Cape_Winelands_mask_rcp85) <- c("grid1", "grid2", "grid3", "grid4", "grid5", "grid6", "grid7", "grid8", "grid9", "grid10", "grid11")
HWM_Cape_Winelands_mask_rcp85 <- rownames_to_column(as.data.frame(HWM_Cape_Winelands_mask_rcp85), var="grid")
HWM_Cape_Winelands_mask_rcp85 <- pivot_longer(HWM_Cape_Winelands_mask_rcp85, cols = !grid, names_to = 'year', values_to = 'hwm')

# plot the HWD in CapeWinelands

ggplot(HWM_Cape_Winelands_mask_rcp85, aes(x = year, y = hwm)) +
  geom_boxplot(color = 'orange', fill='orange', alpha = 0.2) +
  ggtitle("CapeWinelands HWM (RCP 8.5)") + xlab("Year") + ylab("HWM (°C)") +
  theme_classic() +
  theme(plot.title = element_text(size=20, face="bold", hjust = 0.5), axis.title.x = element_text(size=20, face="bold" ), axis.title.y = element_text(size=20, face="bold"),axis.text.y = element_text(size = 20))

###----------------------------HWN--------------------------------------
setwd("~/Desktop/extreme-events-project-WesternCape/data/rcp85/output")
HWN_rcp85 <- brick('hw_ANN_climpact.sample_historical_NA_2021-2100.nc', varname = 'hwn_tx90')

# Crop the raster-CapeWinelands
HWN_Cape_Winelands_mask_rcp85 <- mask(HWN_rcp85, Cape_Winelands) %>%
  as.matrix() 

HWN_Cape_Winelands_mask_rcp85 <- HWN_Cape_Winelands_mask_rcp85[rowSums(is.na(HWN_Cape_Winelands_mask_rcp85)) != ncol(HWN_Cape_Winelands_mask_rcp85), ]
HWN_Cape_Winelands_mask_rcp85[,"X2100.07.02"] <- 2*HWN_Cape_Winelands_mask_rcp85[,"X2099.07.02"] - HWN_Cape_Winelands_mask_rcp85[,"X2098.07.02"]
HWN_Cape_Winelands_mask_rcp85 <- t(apply(HWN_Cape_Winelands_mask_rcp85, 1, na.approx))

colnames(HWN_Cape_Winelands_mask_rcp85) <- as.character(c(2021:2100))
row.names(HWN_Cape_Winelands_mask_rcp85) <- c("grid1", "grid2", "grid3", "grid4", "grid5", "grid6", "grid7", "grid8", "grid9", "grid10", "grid11")

HWN_Cape_Winelands_mask_rcp85 <- rownames_to_column(as.data.frame(HWN_Cape_Winelands_mask_rcp85), var="grid")
HWN_Cape_Winelands_mask_rcp85 <- pivot_longer(HWN_Cape_Winelands_mask_rcp85, cols = !grid, names_to = 'year', values_to = 'hwn')

# plot the HWN in CapeWinelands

ggplot(HWN_Cape_Winelands_mask_rcp85, aes(x = year, y = hwn)) +
  geom_boxplot(color = 'pink', fill='pink', alpha = 0.2) +
  ggtitle("CapeWinelands HWN (RCP 8.5)") + xlab("Year") + ylab("HWN (events)") +
  theme_classic() +
  theme(plot.title = element_text(size=20, face="bold", hjust = 0.5), axis.title.x = element_text(size=20, face="bold" ), axis.title.y = element_text(size=20, face="bold"),axis.text.y = element_text(size = 20))


##-------------------- Central Karoo-----------------------------------
###------------------------HWA--------------------------------------
setwd("~/Desktop/extreme-events-project-WesternCape/data/rcp85/output")

HWA_rcp85 <- brick('hw_ANN_climpact.sample_historical_NA_2021-2100.nc', varname = 'hwa_tx90')

# Crop the raster-CapeWinelands
HWA_Central_Karoo_mask_rcp85 <- mask(HWA_rcp85, Central_Karoo) %>%
  as.matrix() 

HWA_Central_Karoo_mask_rcp85 <- HWA_Central_Karoo_mask_rcp85[rowSums(is.na(HWA_Central_Karoo_mask_rcp85)) != ncol(HWA_Central_Karoo_mask_rcp85), ]
HWA_Central_Karoo_mask_rcp85[,"X2100.07.02"] <- 2*HWA_Central_Karoo_mask_rcp85[,"X2099.07.02"] - HWA_Central_Karoo_mask_rcp85[,"X2098.07.02"]
HWA_Central_Karoo_mask_rcp85 <- t(apply(HWA_Central_Karoo_mask_rcp85, 1, na.approx))
colnames(HWA_Central_Karoo_mask_rcp85) <- as.character(c(2021:2100))
row.names(HWA_Central_Karoo_mask_rcp85) <- c("grid1", "grid2", "grid3", "grid4", "grid5", "grid6", "grid7", "grid8", "grid9", "grid10", "grid11", "grid12", "grid13", "grid14", "grid15", "grid16")

HWA_Central_Karoo_mask_rcp85 <- rownames_to_column(as.data.frame(HWA_Central_Karoo_mask_rcp85), var="grid")
HWA_Central_Karoo_mask_rcp85 <- pivot_longer(HWA_Central_Karoo_mask_rcp85, cols = !grid, names_to = 'year', values_to = 'hwa')
# plot the HWA in CapeWinelands

ggplot(HWA_Central_Karoo_mask_rcp85, aes(x = year, y = hwa)) +
  geom_boxplot(color = 'red', fill='orange', alpha = 0.2) +
  ggtitle("Central Karoo HWA (RCP 8.5)") + xlab("Year") + ylab("HWA (°C)") +
  theme_classic() +
  theme(plot.title = element_text(size=20, face="bold", hjust = 0.5), axis.title.x = element_text(size=20, face="bold" ), axis.title.y = element_text(size=20, face="bold"), axis.text.y = element_text(size = 20))


###---------------------------HWD--------------------------------------
setwd("~/Desktop/extreme-events-project-WesternCape/data/rcp85/output")

HWD_rcp85 <- brick('hw_ANN_climpact.sample_historical_NA_2021-2100.nc', varname = 'hwd_tx90')

# Crop the raster-CapeWinelands
HWD_Central_Karoo_mask_rcp85 <- mask(HWD_rcp85, Central_Karoo) %>%
  as.matrix() 

HWD_Central_Karoo_mask_rcp85 <- HWD_Central_Karoo_mask_rcp85[rowSums(is.na(HWD_Central_Karoo_mask_rcp85)) != ncol(HWD_Central_Karoo_mask_rcp85), ]
HWD_Central_Karoo_mask_rcp85[,"X2100.07.02"] <- 2*HWD_Central_Karoo_mask_rcp85[,"X2099.07.02"] - HWD_Central_Karoo_mask_rcp85[,"X2098.07.02"]
HWD_Central_Karoo_mask_rcp85 <- t(apply(HWD_Central_Karoo_mask_rcp85, 1, na.approx))
colnames(HWD_Central_Karoo_mask_rcp85) <- as.character(c(2021:2100))
row.names(HWD_Central_Karoo_mask_rcp85) <- c("grid1", "grid2", "grid3", "grid4", "grid5", "grid6", "grid7", "grid8", "grid9", "grid10", "grid11", "grid12", "grid13", "grid14", "grid15", "grid16")

HWD_Central_Karoo_mask_rcp85 <- rownames_to_column(as.data.frame(HWD_Central_Karoo_mask_rcp85), var="grid")
HWD_Central_Karoo_mask_rcp85 <- pivot_longer(HWD_Central_Karoo_mask_rcp85, cols = !grid, names_to = 'year', values_to = 'hwd')
# plot the HWA in CapeWinelands

ggplot(HWD_Central_Karoo_mask_rcp85, aes(x = year, y = hwd)) +
  geom_boxplot(color = 'blue', fill='blue', alpha = 0.2) +
  ggtitle("Central Karoo HWD (RCP 8.5)") + xlab("Year") + ylab("HWD (days)") +
  theme_classic() +
  theme(plot.title = element_text(size=20, face="bold", hjust = 0.5), axis.title.x = element_text(size=20, face="bold" ), axis.title.y = element_text(size=20, face="bold"),  axis.text.y = element_text(size = 20))

###----------------------------HWF--------------------------------------
setwd("~/Desktop/extreme-events-project-WesternCape/data/rcp85/output")

HWF_rcp85 <- brick('hw_ANN_climpact.sample_historical_NA_2021-2100.nc', varname = 'hwf_tx90')

# Crop the raster-CapeWinelands
HWF_Central_Karoo_mask_rcp85 <- mask(HWF_rcp85, Central_Karoo) %>%
  as.matrix() 

HWF_Central_Karoo_mask_rcp85 <- HWF_Central_Karoo_mask_rcp85[rowSums(is.na(HWF_Central_Karoo_mask_rcp85)) != ncol(HWF_Central_Karoo_mask_rcp85), ]
HWF_Central_Karoo_mask_rcp85[,"X2100.07.02"] <- 2*HWF_Central_Karoo_mask_rcp85[,"X2099.07.02"] - HWF_Central_Karoo_mask_rcp85[,"X2098.07.02"]
HWF_Central_Karoo_mask_rcp85 <- t(apply(HWF_Central_Karoo_mask_rcp85, 1, na.approx))
colnames(HWF_Central_Karoo_mask_rcp85) <- as.character(c(2021:2100))
row.names(HWF_Central_Karoo_mask_rcp85) <- c("grid1", "grid2", "grid3", "grid4", "grid5", "grid6", "grid7", "grid8", "grid9", "grid10", "grid11", "grid12", "grid13", "grid14", "grid15", "grid16")

HWF_Central_Karoo_mask_rcp85 <- rownames_to_column(as.data.frame(HWF_Central_Karoo_mask_rcp85), var="grid")
HWF_Central_Karoo_mask_rcp85 <- pivot_longer(HWF_Central_Karoo_mask_rcp85, cols = !grid, names_to = 'year', values_to = 'hwf')
# plot the HWA in CapeWinelands

ggplot(HWF_Central_Karoo_mask_rcp85, aes(x = year, y = hwf)) +
  geom_boxplot(color = 'purple', fill='purple', alpha = 0.2) +
  ggtitle("Central Karoo HWF (RCP 8.5)") + xlab("Year") + ylab("HWF (days)") +
  theme_classic() +
  theme(plot.title = element_text(size=20, face="bold", hjust = 0.5), axis.title.x = element_text(size=20, face="bold" ), axis.title.y = element_text(size=20, face="bold"), axis.text.y = element_text(size = 20))

###----------------------------HWM--------------------------------------
setwd("~/Desktop/extreme-events-project-WesternCape/data/rcp85/output")

HWM_rcp85 <- brick('hw_ANN_climpact.sample_historical_NA_2021-2100.nc', varname = 'hwm_tx90')

# Crop the raster-CapeWinelands
HWM_Central_Karoo_mask_rcp85 <- mask(HWM_rcp85, Central_Karoo) %>%
  as.matrix() 

HWM_Central_Karoo_mask_rcp85 <- HWM_Central_Karoo_mask_rcp85[rowSums(is.na(HWM_Central_Karoo_mask_rcp85)) != ncol(HWM_Central_Karoo_mask_rcp85), ]
HWM_Central_Karoo_mask_rcp85[,"X2100.07.02"] <- 2*HWM_Central_Karoo_mask_rcp85[,"X2099.07.02"] - HWM_Central_Karoo_mask_rcp85[,"X2098.07.02"]
HWM_Central_Karoo_mask_rcp85 <- t(apply(HWM_Central_Karoo_mask_rcp85, 1, na.approx))
colnames(HWM_Central_Karoo_mask_rcp85) <- as.character(c(2021:2100))
row.names(HWM_Central_Karoo_mask_rcp85) <- c("grid1", "grid2", "grid3", "grid4", "grid5", "grid6", "grid7", "grid8", "grid9", "grid10", "grid11", "grid12", "grid13", "grid14", "grid15", "grid16")

HWM_Central_Karoo_mask_rcp85 <- rownames_to_column(as.data.frame(HWM_Central_Karoo_mask_rcp85), var="grid")
HWM_Central_Karoo_mask_rcp85 <- pivot_longer(HWM_Central_Karoo_mask_rcp85, cols = !grid, names_to = 'year', values_to = 'hwm')

# plot the HWA in CapeWinelands

ggplot(HWM_Central_Karoo_mask_rcp85, aes(x = year, y = hwm)) +
  geom_boxplot(color = 'orange', fill='orange', alpha = 0.2) +
  ggtitle("Central Karoo HWM (RCP 8.5)") + xlab("Year") + ylab("HWM (°C)") +
  theme_classic() +
  theme(plot.title = element_text(size=20, face="bold", hjust = 0.5), axis.title.x = element_text(size=20, face="bold" ), axis.title.y = element_text(size=20, face="bold"), axis.text.y = element_text(size = 20))

###----------------------------HWN--------------------------------------
setwd("~/Desktop/extreme-events-project-WesternCape/data/rcp85/output")

HWN_rcp85 <- brick('hw_ANN_climpact.sample_historical_NA_2021-2100.nc', varname = 'hwn_tx90')

# Crop the raster-CapeWinelands
HWN_Central_Karoo_mask_rcp85 <- mask(HWN_rcp85, Central_Karoo) %>%
  as.matrix() 

HWN_Central_Karoo_mask_rcp85 <- HWN_Central_Karoo_mask_rcp85[rowSums(is.na(HWN_Central_Karoo_mask_rcp85)) != ncol(HWN_Central_Karoo_mask_rcp85), ]
HWN_Central_Karoo_mask_rcp85[,"X2100.07.02"] <- 2*HWN_Central_Karoo_mask_rcp85[,"X2099.07.02"] - HWN_Central_Karoo_mask_rcp85[,"X2098.07.02"]
HWN_Central_Karoo_mask_rcp85 <- t(apply(HWN_Central_Karoo_mask_rcp85, 1, na.approx))
colnames(HWN_Central_Karoo_mask_rcp85) <- as.character(c(2021:2100))
row.names(HWN_Central_Karoo_mask_rcp85) <- c("grid1", "grid2", "grid3", "grid4", "grid5", "grid6", "grid7", "grid8", "grid9", "grid10", "grid11", "grid12", "grid13", "grid14", "grid15", "grid16")

HWN_Central_Karoo_mask_rcp85 <- rownames_to_column(as.data.frame(HWN_Central_Karoo_mask_rcp85), var="grid")
HWN_Central_Karoo_mask_rcp85 <- pivot_longer(HWN_Central_Karoo_mask_rcp85, cols = !grid, names_to = 'year', values_to = 'hwn')
# plot the HWA in CapeWinelands

ggplot(HWN_Central_Karoo_mask_rcp85, aes(x = year, y = hwn)) +
  geom_boxplot(color = 'pink', fill='pink', alpha = 0.2) +
  ggtitle("Central Karoo HWN (RCP 8.5)") + xlab("Year") + ylab("HWN (events)") +
  theme_classic() +
  theme(plot.title = element_text(size=20, face="bold", hjust = 0.5), axis.title.x = element_text(size=20, face="bold" ), axis.title.y = element_text(size=20, face="bold"),axis.text.y = element_text(size = 20))

##---------------------------Eden-------------------------------------
###------------------------HWA--------------------------------------
setwd("~/Desktop/extreme-events-project-WesternCape/data/rcp85/output")

HWA_rcp85 <- brick('hw_ANN_climpact.sample_historical_NA_2021-2100.nc', varname = 'hwa_tx90')

# Crop the raster-CapeWinelands
HWA_Eden_mask_rcp85 <- mask(HWA_rcp85,Eden) %>%
  as.matrix() 

HWA_Eden_mask_rcp85 <- HWA_Eden_mask_rcp85[rowSums(is.na(HWA_Eden_mask_rcp85)) != ncol(HWA_Eden_mask_rcp85), ]
HWA_Eden_mask_rcp85[,"X2100.07.02"] <- 2*HWA_Eden_mask_rcp85[,"X2099.07.02"] - HWA_Eden_mask_rcp85[,"X2098.07.02"]
HWA_Eden_mask_rcp85 <- t(apply(HWA_Eden_mask_rcp85, 1, na.approx))
HWA_Eden_mask_rcp85 <- data.frame(Reduce(rbind, HWA_Eden_mask_rcp85))
colnames(HWA_Eden_mask_rcp85) <- as.character(c(2021:2100))
row.names(HWA_Eden_mask_rcp85) <- c("grid1", "grid2", "grid3", "grid4", "grid5", "grid6", "grid7", "grid8", "grid9", "grid10", "grid11", "grid12", "grid13", "grid14", "grid15")

HWA_Eden_mask_rcp85  <- rownames_to_column(as.data.frame(HWA_Eden_mask_rcp85 ), var="grid")
HWA_Eden_mask_rcp85  <- pivot_longer(HWA_Eden_mask_rcp85 , cols = !grid, names_to = 'year', values_to = 'hwa')
# plot the HWA in CapeWinelands

ggplot(HWA_Eden_mask_rcp85, aes(x = year, y = hwa)) +
  geom_boxplot(color = 'red', fill='orange', alpha = 0.2) +
  ggtitle("Eden HWA (RCP 8.5)") + xlab("Year") + ylab("HWA (°C)") +
  theme_classic() +
  theme(plot.title = element_text(size=20, face="bold", hjust = 0.5), axis.title.x = element_text(size=20, face="bold" ), axis.title.y = element_text(size=20, face="bold"),axis.text.y = element_text(size = 20))


###------------------------HWD--------------------------------------
setwd("~/Desktop/extreme-events-project-WesternCape/data/rcp85/output")

HWD_rcp85 <- brick('hw_ANN_climpact.sample_historical_NA_2021-2100.nc', varname = 'hwd_tx90')

# Crop the raster-CapeWinelands
HWD_Eden_mask_rcp85 <- mask(HWD_rcp85,Eden) %>%
  as.matrix() 

HWD_Eden_mask_rcp85 <- HWD_Eden_mask_rcp85[rowSums(is.na(HWD_Eden_mask_rcp85)) != ncol(HWD_Eden_mask_rcp85), ]
HWD_Eden_mask_rcp85[,"X2100.07.02"] <- 2*HWD_Eden_mask_rcp85[,"X2099.07.02"] - HWD_Eden_mask_rcp85[,"X2098.07.02"]
HWD_Eden_mask_rcp85 <- t(apply(HWD_Eden_mask_rcp85, 1, na.approx))
HWD_Eden_mask_rcp85 <- data.frame(Reduce(rbind, HWD_Eden_mask_rcp85))
colnames(HWD_Eden_mask_rcp85) <- as.character(c(2021:2100))
row.names(HWD_Eden_mask_rcp85) <- c("grid1", "grid2", "grid3", "grid4", "grid5", "grid6", "grid7", "grid8", "grid9", "grid10", "grid11", "grid12", "grid13", "grid14", "grid15")

HWD_Eden_mask_rcp85  <- rownames_to_column(as.data.frame(HWD_Eden_mask_rcp85 ), var="grid")
HWD_Eden_mask_rcp85  <- pivot_longer(HWD_Eden_mask_rcp85 , cols = !grid, names_to = 'year', values_to = 'hwd')
# plot the HWA in CapeWinelands

ggplot(HWD_Eden_mask_rcp85, aes(x = year, y = hwd)) +
  geom_boxplot(color = 'blue', fill='blue', alpha = 0.2) +
  ggtitle("Eden HWD (RCP 8.5)") + xlab("Year") + ylab("HWD (days)") +
  theme_classic() +
  theme(plot.title = element_text(size=20, face="bold", hjust = 0.5), axis.title.x = element_text(size=20, face="bold" ), axis.title.y = element_text(size=20, face="bold"),axis.text.y = element_text(size = 20))

###------------------------HWF--------------------------------------
setwd("~/Desktop/extreme-events-project-WesternCape/data/rcp85/output")

HWF_rcp85 <- brick('hw_ANN_climpact.sample_historical_NA_2021-2100.nc', varname = 'hwf_tx90')

# Crop the raster-CapeWinelands
HWF_Eden_mask_rcp85 <- mask(HWF_rcp85,Eden) %>%
  as.matrix() 

HWF_Eden_mask_rcp85 <- HWF_Eden_mask_rcp85[rowSums(is.na(HWF_Eden_mask_rcp85)) != ncol(HWF_Eden_mask_rcp85), ]
HWF_Eden_mask_rcp85[,"X2100.07.02"] <- 2*HWF_Eden_mask_rcp85[,"X2099.07.02"] - HWF_Eden_mask_rcp85[,"X2098.07.02"]
HWF_Eden_mask_rcp85 <- t(apply(HWF_Eden_mask_rcp85, 1, na.approx))
#HWD_Eden_mask_rcp85 <- data.frame(Reduce(rbind, HWD_Eden_mask_rcp85))
colnames(HWF_Eden_mask_rcp85) <- as.character(c(2021:2100))
row.names(HWF_Eden_mask_rcp85) <- c("grid1", "grid2", "grid3", "grid4", "grid5", "grid6", "grid7", "grid8", "grid9", "grid10", "grid11", "grid12", "grid13", "grid14", "grid15")

HWF_Eden_mask_rcp85  <- rownames_to_column(as.data.frame(HWF_Eden_mask_rcp85 ), var="grid")
HWF_Eden_mask_rcp85  <- pivot_longer(HWF_Eden_mask_rcp85 , cols = !grid, names_to = 'year', values_to = 'hwf')

ggplot(HWF_Eden_mask_rcp85, aes(x = year, y = hwf)) +
  geom_boxplot(color = 'purple', fill='purple', alpha = 0.2) +
  ggtitle("Eden HWF (RCP 8.5)") + xlab("Year") + ylab("HWF (days)") +
  theme_classic() +
  theme(plot.title = element_text(size=20, face="bold", hjust = 0.5), axis.title.x = element_text(size=20, face="bold" ), axis.title.y = element_text(size=20, face="bold"),axis.text.y = element_text(size = 20))

###-----------------------HWM---------------------------------------------
setwd("~/Desktop/extreme-events-project-WesternCape/data/rcp85/output")

HWM_rcp85 <- brick('hw_ANN_climpact.sample_historical_NA_2021-2100.nc', varname = 'hwm_tx90')

# Crop the raster-CapeWinelands
HWM_Eden_mask_rcp85 <- mask(HWM_rcp85,Eden) %>%
  as.matrix() 

HWM_Eden_mask_rcp85 <- HWM_Eden_mask_rcp85[rowSums(is.na(HWM_Eden_mask_rcp85)) != ncol(HWM_Eden_mask_rcp85), ]
HWM_Eden_mask_rcp85[,"X2100.07.02"] <- 2*HWM_Eden_mask_rcp85[,"X2099.07.02"] - HWM_Eden_mask_rcp85[,"X2098.07.02"]
HWM_Eden_mask_rcp85 <- t(apply(HWM_Eden_mask_rcp85, 1, na.approx))
HWM_Eden_mask_rcp85 <- data.frame(Reduce(rbind, HWM_Eden_mask_rcp85))
colnames(HWM_Eden_mask_rcp85) <- as.character(c(2021:2100))
row.names(HWM_Eden_mask_rcp85) <- c("grid1", "grid2", "grid3", "grid4", "grid5", "grid6", "grid7", "grid8", "grid9", "grid10", "grid11", "grid12", "grid13", "grid14", "grid15")

HWM_Eden_mask_rcp85  <- rownames_to_column(as.data.frame(HWM_Eden_mask_rcp85 ), var="grid")
HWM_Eden_mask_rcp85  <- pivot_longer(HWM_Eden_mask_rcp85 , cols = !grid, names_to = 'year', values_to = 'hwm')

ggplot(HWM_Eden_mask_rcp85, aes(x = year, y = hwm)) +
  geom_boxplot(color = 'orange', fill='orange', alpha = 0.2) +
  ggtitle("Eden HWM (RCP 8.5)") + xlab("Year") + ylab("HWM (°C)") +
  theme_classic() +
  theme(plot.title = element_text(size=20, face="bold", hjust = 0.5), axis.title.x = element_text(size=20, face="bold" ), axis.title.y = element_text(size=20, face="bold"),axis.text.y = element_text(size = 20))

###-----------------------HWN---------------------------------------------
setwd("~/Desktop/extreme-events-project-WesternCape/data/rcp85/output")

HWN_rcp85 <- brick('hw_ANN_climpact.sample_historical_NA_2021-2100.nc', varname = 'hwn_tx90')

# Crop the raster-CapeWinelands
HWN_Eden_mask_rcp85 <- mask(HWN_rcp85,Eden) %>%
  as.matrix() 

HWN_Eden_mask_rcp85 <- HWN_Eden_mask_rcp85[rowSums(is.na(HWN_Eden_mask_rcp85)) != ncol(HWN_Eden_mask_rcp85), ]
HWN_Eden_mask_rcp85[,"X2100.07.02"] <- 2*HWN_Eden_mask_rcp85[,"X2099.07.02"] - HWN_Eden_mask_rcp85[,"X2098.07.02"]
HWN_Eden_mask_rcp85 <- t(apply(HWN_Eden_mask_rcp85, 1, na.approx))
#HWN_Eden_mask_rcp85 <- data.frame(Reduce(rbind, HWN_Eden_mask_rcp85))
colnames(HWN_Eden_mask_rcp85) <- as.character(c(2021:2100))
row.names(HWN_Eden_mask_rcp85) <- c("grid1", "grid2", "grid3", "grid4", "grid5", "grid6", "grid7", "grid8", "grid9", "grid10", "grid11", "grid12", "grid13", "grid14", "grid15")

HWN_Eden_mask_rcp85  <- rownames_to_column(as.data.frame(HWN_Eden_mask_rcp85 ), var="grid")
HWN_Eden_mask_rcp85  <- pivot_longer(HWN_Eden_mask_rcp85 , cols = !grid, names_to = 'year', values_to = 'hwn')

ggplot(HWN_Eden_mask_rcp85, aes(x = year, y = hwn)) +
  geom_boxplot(color = 'pink', fill='pink', alpha = 0.2) +
  ggtitle("Eden HWN (RCP 8.5)") + xlab("Year") + ylab("HWN (events)") +
  theme_classic() +
  theme(plot.title = element_text(size=20, face="bold", hjust = 0.5), axis.title.x = element_text(size=20, face="bold" ), axis.title.y = element_text(size=20, face="bold"),axis.text.y = element_text(size = 20))

##------------------------Overberg---------------------------------------------
###------------------------HWA-------------------------------------------------
setwd("~/Desktop/extreme-events-project-WesternCape/data/rcp85/output")

HWA_rcp85 <- brick('hw_ANN_climpact.sample_historical_NA_2021-2100.nc', varname = 'hwa_tx90')

# Crop the raster-CapeWinelands
HWA_Overberg_mask_rcp85 <- mask(HWA_rcp85,Overberg) %>%
  as.matrix() 

HWA_Overberg_mask_rcp85 <- HWA_Overberg_mask_rcp85[rowSums(is.na(HWA_Overberg_mask_rcp85)) != ncol(HWA_Overberg_mask_rcp85), ]
HWA_Overberg_mask_rcp85[,"X2100.07.02"] <- 2*HWA_Overberg_mask_rcp85[,"X2099.07.02"] - HWA_Overberg_mask_rcp85[,"X2098.07.02"]
HWA_Overberg_mask_rcp85 <- t(apply(HWA_Overberg_mask_rcp85, 1, na.approx))
HWA_Overberg_mask_rcp85 <- data.frame(Reduce(rbind, HWA_Overberg_mask_rcp85))
colnames(HWA_Overberg_mask_rcp85) <- as.character(c(2021:2100))
row.names(HWA_Overberg_mask_rcp85) <- c("grid1", "grid2", "grid3", "grid4", "grid5", "grid6")

HWA_Overberg_mask_rcp85  <- rownames_to_column(as.data.frame(HWA_Overberg_mask_rcp85), var="grid")
HWA_Overberg_mask_rcp85  <- pivot_longer(HWA_Overberg_mask_rcp85, cols = !grid, names_to = 'year', values_to = 'hwa')

ggplot(HWA_Overberg_mask_rcp85, aes(x = year, y = hwa)) +
  geom_boxplot(color = 'red', fill='orange', alpha = 0.2) +
  ggtitle("Overberg HWA (RCP 8.5)") + xlab("Year") + ylab("HWA (°C)") + 
  theme_classic() + 
  theme(plot.title = element_text(size=20, face="bold", hjust = 0.5), axis.title.x = element_text(size=20, face="bold" ), axis.title.y = element_text(size=20, face="bold"), axis.text.y = element_text(size = 20))

###------------------------HWD-------------------------------------------------
setwd("~/Desktop/extreme-events-project-WesternCape/data/rcp85/output")

HWD_rcp85 <- brick('hw_ANN_climpact.sample_historical_NA_2021-2100.nc', varname = 'hwd_tx90')

# Crop the raster-CapeWinelands
HWD_Overberg_mask_rcp85 <- mask(HWD_rcp85,Overberg) %>%
  as.matrix() 

HWD_Overberg_mask_rcp85 <- HWD_Overberg_mask_rcp85[rowSums(is.na(HWD_Overberg_mask_rcp85)) != ncol(HWD_Overberg_mask_rcp85), ]
HWD_Overberg_mask_rcp85[,"X2100.07.02"] <- 2*HWD_Overberg_mask_rcp85[,"X2099.07.02"] - HWD_Overberg_mask_rcp85[,"X2098.07.02"]
HWD_Overberg_mask_rcp85 <- t(apply(HWD_Overberg_mask_rcp85, 1, na.approx))
HWD_Overberg_mask_rcp85 <- data.frame(Reduce(rbind, HWD_Overberg_mask_rcp85))
colnames(HWD_Overberg_mask_rcp85) <- as.character(c(2021:2100))
row.names(HWD_Overberg_mask_rcp85) <- c("grid1", "grid2", "grid3", "grid4", "grid5", "grid6")

HWD_Overberg_mask_rcp85  <- rownames_to_column(as.data.frame(HWD_Overberg_mask_rcp85), var="grid")
HWD_Overberg_mask_rcp85  <- pivot_longer(HWD_Overberg_mask_rcp85, cols = !grid, names_to = 'year', values_to = 'hwd')

ggplot(HWD_Overberg_mask_rcp85, aes(x = year, y = hwd)) +
  geom_boxplot(color = 'blue', fill='blue', alpha = 0.2) +
  ggtitle("Overberg HWD (RCP 8.5)") + xlab("Year") + ylab("HWD (days)") + 
  theme_classic() + 
  theme(plot.title = element_text(size=20, face="bold", hjust = 0.5), axis.title.x = element_text(size=20, face="bold" ), axis.title.y = element_text(size=20, face="bold"), axis.text.y = element_text(size = 20))

###------------------------HWF-------------------------------------------------
setwd("~/Desktop/extreme-events-project-WesternCape/data/rcp85/output")

HWF_rcp85 <- brick('hw_ANN_climpact.sample_historical_NA_2021-2100.nc', varname = 'hwf_tx90')

# Crop the raster-CapeWinelands
HWF_Overberg_mask_rcp85 <- mask(HWF_rcp85,Overberg) %>%
  as.matrix() 

HWF_Overberg_mask_rcp85 <- HWF_Overberg_mask_rcp85[rowSums(is.na(HWF_Overberg_mask_rcp85)) != ncol(HWF_Overberg_mask_rcp85), ]
HWF_Overberg_mask_rcp85[,"X2100.07.02"] <- 2*HWF_Overberg_mask_rcp85[,"X2099.07.02"] - HWF_Overberg_mask_rcp85[,"X2098.07.02"]
HWF_Overberg_mask_rcp85 <- t(apply(HWF_Overberg_mask_rcp85, 1, na.approx))
#HWD_Overberg_mask_rcp85 <- data.frame(Reduce(rbind, HWD_Overberg_mask_rcp85))
colnames(HWF_Overberg_mask_rcp85) <- as.character(c(2021:2100))
row.names(HWF_Overberg_mask_rcp85) <- c("grid1", "grid2", "grid3", "grid4", "grid5", "grid6")

HWF_Overberg_mask_rcp85  <- rownames_to_column(as.data.frame(HWF_Overberg_mask_rcp85), var="grid")
HWF_Overberg_mask_rcp85  <- pivot_longer(HWF_Overberg_mask_rcp85, cols = !grid, names_to = 'year', values_to = 'hwf')

ggplot(HWF_Overberg_mask_rcp85, aes(x = year, y = hwf)) +
  geom_boxplot(color = 'purple', fill='purple', alpha = 0.2) +
  ggtitle("Overberg HWF (RCP 8.5)") + xlab("Year") + ylab("HWF (days)") + 
  theme_classic() + 
  theme(plot.title = element_text(size=20, face="bold", hjust = 0.5), axis.title.x = element_text(size=20, face="bold" ), axis.title.y = element_text(size=20, face="bold"), axis.text.y = element_text(size = 20))

###------------------------HWM-------------------------------------------------
setwd("~/Desktop/extreme-events-project-WesternCape/data/rcp85/output")

HWM_rcp85 <- brick('hw_ANN_climpact.sample_historical_NA_2021-2100.nc', varname = 'hwm_tx90')

# Crop the raster-CapeWinelands
HWM_Overberg_mask_rcp85 <- mask(HWM_rcp85,Overberg) %>%
  as.matrix() 

HWM_Overberg_mask_rcp85 <- HWM_Overberg_mask_rcp85[rowSums(is.na(HWM_Overberg_mask_rcp85)) != ncol(HWM_Overberg_mask_rcp85), ]
HWM_Overberg_mask_rcp85[,"X2100.07.02"] <- 2*HWM_Overberg_mask_rcp85[,"X2099.07.02"] - HWM_Overberg_mask_rcp85[,"X2098.07.02"]
HWM_Overberg_mask_rcp85 <- t(apply(HWM_Overberg_mask_rcp85, 1, na.approx))
HWM_Overberg_mask_rcp85 <- data.frame(Reduce(rbind, HWM_Overberg_mask_rcp85))
colnames(HWM_Overberg_mask_rcp85) <- as.character(c(2021:2100))
row.names(HWM_Overberg_mask_rcp85) <- c("grid1", "grid2", "grid3", "grid4", "grid5", "grid6")

HWM_Overberg_mask_rcp85  <- rownames_to_column(as.data.frame(HWM_Overberg_mask_rcp85), var="grid")
HWM_Overberg_mask_rcp85  <- pivot_longer(HWM_Overberg_mask_rcp85, cols = !grid, names_to = 'year', values_to = 'hwm')

ggplot(HWM_Overberg_mask_rcp85, aes(x = year, y = hwm)) +
  geom_boxplot(color = 'orange', fill='orange', alpha = 0.2) +
  ggtitle("Overberg HWM (RCP 8.5)") + xlab("Year") + ylab("HWM (°C)") + 
  theme_classic() + 
  theme(plot.title = element_text(size=20, face="bold", hjust = 0.5), axis.title.x = element_text(size=20, face="bold" ), axis.title.y = element_text(size=20, face="bold"), axis.text.y = element_text(size = 20))

###------------------------HWN-------------------------------------------------
setwd("~/Desktop/extreme-events-project-WesternCape/data/rcp85/output")

HWN_rcp85 <- brick('hw_ANN_climpact.sample_historical_NA_2021-2100.nc', varname = 'hwn_tx90')

# Crop the raster-CapeWinelands
HWN_Overberg_mask_rcp85 <- mask(HWN_rcp85,Overberg) %>%
  as.matrix() 

HWN_Overberg_mask_rcp85 <- HWN_Overberg_mask_rcp85[rowSums(is.na(HWN_Overberg_mask_rcp85)) != ncol(HWN_Overberg_mask_rcp85), ]
HWN_Overberg_mask_rcp85[,"X2100.07.02"] <- 2*HWN_Overberg_mask_rcp85[,"X2099.07.02"] - HWN_Overberg_mask_rcp85[,"X2098.07.02"]
HWN_Overberg_mask_rcp85 <- t(apply(HWN_Overberg_mask_rcp85, 1, na.approx))
#HWM_Overberg_mask_rcp85 <- data.frame(Reduce(rbind, HWM_Overberg_mask_rcp85))
colnames(HWN_Overberg_mask_rcp85) <- as.character(c(2021:2100))
row.names(HWN_Overberg_mask_rcp85) <- c("grid1", "grid2", "grid3", "grid4", "grid5", "grid6")

HWN_Overberg_mask_rcp85  <- rownames_to_column(as.data.frame(HWN_Overberg_mask_rcp85), var="grid")
HWN_Overberg_mask_rcp85  <- pivot_longer(HWN_Overberg_mask_rcp85, cols = !grid, names_to = 'year', values_to = 'hwn')

ggplot(HWN_Overberg_mask_rcp85, aes(x = year, y = hwn)) +
  geom_boxplot(color = 'pink', fill='pink', alpha = 0.2) +
  ggtitle("Overberg HWN (RCP 8.5)") + xlab("Year") + ylab("HWN (events)") + 
  theme_classic() + 
  theme(plot.title = element_text(size=20, face="bold", hjust = 0.5), axis.title.x = element_text(size=20, face="bold" ), axis.title.y = element_text(size=20, face="bold"), axis.text.y = element_text(size = 20))

##------------------------West_Coast---------------------------------------------
###------------------------HWA-------------------------------------------------
setwd("~/Desktop/extreme-events-project-WesternCape/data/rcp85/output")
HWA_rcp85 <- brick('hw_ANN_climpact.sample_historical_NA_2021-2100.nc', varname = 'hwa_tx90')

# Crop the raster-CapeWinelands
HWA_West_Coast_mask_rcp85 <- mask(HWA_rcp85,West_Coast) %>%
  as.matrix() 

HWA_West_Coast_mask_rcp85 <- HWA_West_Coast_mask_rcp85[rowSums(is.na(HWA_West_Coast_mask_rcp85)) != ncol(HWA_West_Coast_mask_rcp85), ]
HWA_West_Coast_mask_rcp85[,"X2100.07.02"] <- 2*HWA_West_Coast_mask_rcp85[,"X2099.07.02"] - HWA_West_Coast_mask_rcp85[,"X2098.07.02"]
HWA_West_Coast_mask_rcp85 <- t(apply(HWA_West_Coast_mask_rcp85, 1, na.approx))
#HWA_West_Coast_mask_rcp85 <- data.frame(Reduce(rbind, HWA_West_Coast_mask_rcp85))
colnames(HWA_West_Coast_mask_rcp85) <- as.character(c(2021:2100))
row.names(HWA_West_Coast_mask_rcp85) <- c("grid1", "grid2", "grid3", "grid4", "grid5", "grid6", "grid7", "grid8", "grid9", "grid10", "grid11", "grid12", "grid13", "grid14", "grid15", "grid16", "grid17", "grid18", "grid19")

HWA_West_Coast_mask_rcp85  <- rownames_to_column(as.data.frame(HWA_West_Coast_mask_rcp85), var="grid")
HWA_West_Coast_mask_rcp85  <- pivot_longer(HWA_West_Coast_mask_rcp85, cols = !grid, names_to = 'year', values_to = 'hwa')

ggplot(HWA_West_Coast_mask_rcp85, aes(x = year, y = hwa)) +
  geom_boxplot(color = 'red', fill='orange', alpha = 0.2) +
  ggtitle("West Coast HWA (RCP 8.5)") + xlab("Year") + ylab("HWA (°C)") + 
  theme_classic() + 
  theme(plot.title = element_text(size=20, face="bold", hjust = 0.5), axis.title.x = element_text(size=20, face="bold" ), axis.title.y = element_text(size=20, face="bold"), axis.text.y = element_text(size = 20))

###------------------------HWD-------------------------------------------------
setwd("~/Desktop/extreme-events-project-WesternCape/data/rcp85/output")
HWD_rcp85 <- brick('hw_ANN_climpact.sample_historical_NA_2021-2100.nc', varname = 'hwd_tx90')

# Crop the raster-CapeWinelands
HWD_West_Coast_mask_rcp85 <- mask(HWD_rcp85,West_Coast) %>%
  as.matrix() 

HWD_West_Coast_mask_rcp85 <- HWD_West_Coast_mask_rcp85[rowSums(is.na(HWD_West_Coast_mask_rcp85)) != ncol(HWD_West_Coast_mask_rcp85), ]
HWD_West_Coast_mask_rcp85[,"X2100.07.02"] <- 2*HWD_West_Coast_mask_rcp85[,"X2099.07.02"] - HWD_West_Coast_mask_rcp85[,"X2098.07.02"]
HWD_West_Coast_mask_rcp85 <- t(apply(HWD_West_Coast_mask_rcp85, 1, na.approx))
#HWA_West_Coast_mask_rcp85 <- data.frame(Reduce(rbind, HWA_West_Coast_mask_rcp85))
colnames(HWD_West_Coast_mask_rcp85) <- as.character(c(2021:2100))
row.names(HWD_West_Coast_mask_rcp85) <- c("grid1", "grid2", "grid3", "grid4", "grid5", "grid6", "grid7", "grid8", "grid9", "grid10", "grid11", "grid12", "grid13", "grid14", "grid15", "grid16", "grid17", "grid18", "grid19")

HWD_West_Coast_mask_rcp85  <- rownames_to_column(as.data.frame(HWD_West_Coast_mask_rcp85), var="grid")
HWD_West_Coast_mask_rcp85  <- pivot_longer(HWD_West_Coast_mask_rcp85, cols = !grid, names_to = 'year', values_to = 'hwd')

ggplot(HWD_West_Coast_mask_rcp85, aes(x = year, y = hwd)) +
  geom_boxplot(color = 'blue', fill='blue', alpha = 0.2) +
  ggtitle("West Coast HWD (RCP 8.5)") + xlab("Year") + ylab("HWD (days)") + 
  theme_classic() + 
  theme(plot.title = element_text(size=20, face="bold", hjust = 0.5), axis.title.x = element_text(size=20, face="bold" ), axis.title.y = element_text(size=20, face="bold"), axis.text.y = element_text(size = 20))

###------------------------HWF-------------------------------------------------
setwd("~/Desktop/extreme-events-project-WesternCape/data/rcp85/output")
HWF_rcp85 <- brick('hw_ANN_climpact.sample_historical_NA_2021-2100.nc', varname = 'hwf_tx90')

# Crop the raster-CapeWinelands
HWF_West_Coast_mask_rcp85 <- mask(HWF_rcp85,West_Coast) %>%
  as.matrix() 

HWF_West_Coast_mask_rcp85 <- HWF_West_Coast_mask_rcp85[rowSums(is.na(HWF_West_Coast_mask_rcp85)) != ncol(HWF_West_Coast_mask_rcp85), ]
HWF_West_Coast_mask_rcp85[,"X2100.07.02"] <- 2*HWF_West_Coast_mask_rcp85[,"X2099.07.02"] - HWF_West_Coast_mask_rcp85[,"X2098.07.02"]
HWF_West_Coast_mask_rcp85 <- t(apply(HWF_West_Coast_mask_rcp85, 1, na.approx))
#HWA_West_Coast_mask_rcp85 <- data.frame(Reduce(rbind, HWA_West_Coast_mask_rcp85))
colnames(HWF_West_Coast_mask_rcp85) <- as.character(c(2021:2100))
row.names(HWF_West_Coast_mask_rcp85) <- c("grid1", "grid2", "grid3", "grid4", "grid5", "grid6", "grid7", "grid8", "grid9", "grid10", "grid11", "grid12", "grid13", "grid14", "grid15", "grid16", "grid17", "grid18", "grid19")

HWF_West_Coast_mask_rcp85  <- rownames_to_column(as.data.frame(HWF_West_Coast_mask_rcp85), var="grid")
HWF_West_Coast_mask_rcp85  <- pivot_longer(HWF_West_Coast_mask_rcp85, cols = !grid, names_to = 'year', values_to = 'hwf')

ggplot(HWF_West_Coast_mask_rcp85, aes(x = year, y = hwf)) +
  geom_boxplot(color = 'purple', fill='purple', alpha = 0.2) +
  ggtitle("West Coast HWF (RCP 8.5)") + xlab("Year") + ylab("HWF (days)") + 
  theme_classic() + 
  theme(plot.title = element_text(size=20, face="bold", hjust = 0.5), axis.title.x = element_text(size=20, face="bold" ), axis.title.y = element_text(size=20, face="bold"), axis.text.y = element_text(size = 20))

###------------------------HWM-------------------------------------------------
setwd("~/Desktop/extreme-events-project-WesternCape/data/rcp85/output")
HWM_rcp85 <- brick('hw_ANN_climpact.sample_historical_NA_2021-2100.nc', varname = 'hwm_tx90')

# Crop the raster-CapeWinelands
HWM_West_Coast_mask_rcp85 <- mask(HWM_rcp85,West_Coast) %>%
  as.matrix() 

HWM_West_Coast_mask_rcp85 <- HWM_West_Coast_mask_rcp85[rowSums(is.na(HWM_West_Coast_mask_rcp85)) != ncol(HWM_West_Coast_mask_rcp85), ]
HWM_West_Coast_mask_rcp85[,"X2100.07.02"] <- 2*HWM_West_Coast_mask_rcp85[,"X2099.07.02"] - HWM_West_Coast_mask_rcp85[,"X2098.07.02"]
HWM_West_Coast_mask_rcp85 <- t(apply(HWM_West_Coast_mask_rcp85, 1, na.approx))
#HWA_West_Coast_mask_rcp85 <- data.frame(Reduce(rbind, HWA_West_Coast_mask_rcp85))
colnames(HWM_West_Coast_mask_rcp85) <- as.character(c(2021:2100))
row.names(HWM_West_Coast_mask_rcp85) <- c("grid1", "grid2", "grid3", "grid4", "grid5", "grid6", "grid7", "grid8", "grid9", "grid10", "grid11", "grid12", "grid13", "grid14", "grid15", "grid16", "grid17", "grid18", "grid19")

HWM_West_Coast_mask_rcp85  <- rownames_to_column(as.data.frame(HWM_West_Coast_mask_rcp85), var="grid")
HWM_West_Coast_mask_rcp85  <- pivot_longer(HWM_West_Coast_mask_rcp85, cols = !grid, names_to = 'year', values_to = 'hwm')

ggplot(HWM_West_Coast_mask_rcp85, aes(x = year, y = hwm)) +
  geom_boxplot(color = 'orange', fill='orange', alpha = 0.2) +
  ggtitle("West Coast HWM (RCP 8.5)") + xlab("Year") + ylab("HWM (°C)") + 
  theme_classic() + 
  theme(plot.title = element_text(size=20, face="bold", hjust = 0.5), axis.title.x = element_text(size=20, face="bold" ), axis.title.y = element_text(size=20, face="bold"), axis.text.y = element_text(size = 20))

###------------------------HWN-------------------------------------------------
setwd("~/Desktop/extreme-events-project-WesternCape/data/rcp85/output")
HWN_rcp85 <- brick('hw_ANN_climpact.sample_historical_NA_2021-2100.nc', varname = 'hwn_tx90')

# Crop the raster-CapeWinelands
HWN_West_Coast_mask_rcp85 <- mask(HWN_rcp85,West_Coast) %>%
  as.matrix() 

HWN_West_Coast_mask_rcp85 <- HWN_West_Coast_mask_rcp85[rowSums(is.na(HWN_West_Coast_mask_rcp85)) != ncol(HWN_West_Coast_mask_rcp85), ]
HWN_West_Coast_mask_rcp85[,"X2100.07.02"] <- 2*HWN_West_Coast_mask_rcp85[,"X2099.07.02"] - HWN_West_Coast_mask_rcp85[,"X2098.07.02"]
HWN_West_Coast_mask_rcp85 <- t(apply(HWN_West_Coast_mask_rcp85, 1, na.approx))
#HWA_West_Coast_mask_rcp85 <- data.frame(Reduce(rbind, HWA_West_Coast_mask_rcp85))
colnames(HWN_West_Coast_mask_rcp85) <- as.character(c(2021:2100))
row.names(HWN_West_Coast_mask_rcp85) <- c("grid1", "grid2", "grid3", "grid4", "grid5", "grid6", "grid7", "grid8", "grid9", "grid10", "grid11", "grid12", "grid13", "grid14", "grid15", "grid16", "grid17", "grid18", "grid19")

HWN_West_Coast_mask_rcp85  <- rownames_to_column(as.data.frame(HWN_West_Coast_mask_rcp85), var="grid")
HWN_West_Coast_mask_rcp85  <- pivot_longer(HWN_West_Coast_mask_rcp85, cols = !grid, names_to = 'year', values_to = 'hwn')

ggplot(HWN_West_Coast_mask_rcp85, aes(x = year, y = hwn)) +
  geom_boxplot(color = 'pink', fill='pink', alpha = 0.2) +
  ggtitle("West Coast HWN (RCP 8.5)") + xlab("Year") + ylab("HWN (events)") + 
  theme_classic() + 
  theme(plot.title = element_text(size=20, face="bold", hjust = 0.5), axis.title.x = element_text(size=20, face="bold" ), axis.title.y = element_text(size=20, face="bold"), axis.text.y = element_text(size = 20))

##------------------------City_Cape_Town---------------------------------------------
###------------------------HWA-------------------------------------------------
setwd("~/Desktop/extreme-events-project-WesternCape/data/rcp85/output")
HWA_rcp85 <- brick('hw_ANN_climpact.sample_historical_NA_2021-2100.nc', varname = 'hwa_tx90')

# Crop the raster-CapeWinelands
HWA_City_Cape_Town_mask_rcp85 <- mask(HWA_rcp85,buffer(City_Cape_Town, width = 0.05)) %>%
  as.matrix() 

HWA_City_Cape_Town_mask_rcp85 <- HWA_City_Cape_Town_mask_rcp85[rowSums(is.na(HWA_City_Cape_Town_mask_rcp85)) != ncol(HWA_City_Cape_Town_mask_rcp85), ]
HWA_City_Cape_Town_mask_rcp85[,"X2100.07.02"] <- 2*HWA_City_Cape_Town_mask_rcp85[,"X2099.07.02"] - HWA_City_Cape_Town_mask_rcp85[,"X2098.07.02"]
HWA_City_Cape_Town_mask_rcp85 <- t(apply(HWA_City_Cape_Town_mask_rcp85, 1, na.approx))
#HWA_West_Coast_mask_rcp85 <- data.frame(Reduce(rbind, HWA_West_Coast_mask_rcp85))
colnames(HWA_City_Cape_Town_mask_rcp85) <- as.character(c(2021:2100))
row.names(HWA_City_Cape_Town_mask_rcp85) <- c("grid1", "grid2", "grid3")

HWA_City_Cape_Town_mask_rcp85  <- rownames_to_column(as.data.frame(HWA_City_Cape_Town_mask_rcp85), var="grid")
HWA_City_Cape_Town_mask_rcp85  <- pivot_longer(HWA_City_Cape_Town_mask_rcp85, cols = !grid, names_to = 'year', values_to = 'hwa')

ggplot(HWA_City_Cape_Town_mask_rcp85, aes(x = year, y = hwa)) +
  geom_boxplot(color = 'red', fill='orange', alpha = 0.2) +
  ggtitle("City of Cape Town HWA (RCP 8.5)") + xlab("Year") + ylab("HWA (°C)") + 
  theme_classic() + 
  theme(plot.title = element_text(size=20, face="bold", hjust = 0.5), axis.title.x = element_text(size=20, face="bold" ), axis.title.y = element_text(size=20, face="bold"), axis.text.y = element_text(size = 20))

###------------------------HWD-------------------------------------------------
setwd("~/Desktop/extreme-events-project-WesternCape/data/rcp85/output")
HWD_rcp85 <- brick('hw_ANN_climpact.sample_historical_NA_2021-2100.nc', varname = 'hwd_tx90')

# Crop the raster-CapeWinelands
HWD_City_Cape_Town_mask_rcp85 <- mask(HWD_rcp85,buffer(City_Cape_Town, width = 0.05)) %>%
  as.matrix() 

HWD_City_Cape_Town_mask_rcp85 <- HWD_City_Cape_Town_mask_rcp85[rowSums(is.na(HWD_City_Cape_Town_mask_rcp85)) != ncol(HWD_City_Cape_Town_mask_rcp85), ]
HWD_City_Cape_Town_mask_rcp85[,"X2100.07.02"] <- 2*HWD_City_Cape_Town_mask_rcp85[,"X2099.07.02"] - HWD_City_Cape_Town_mask_rcp85[,"X2098.07.02"]
HWD_City_Cape_Town_mask_rcp85 <- t(apply(HWD_City_Cape_Town_mask_rcp85, 1, na.approx))
#HWA_West_Coast_mask_rcp85 <- data.frame(Reduce(rbind, HWA_West_Coast_mask_rcp85))
colnames(HWD_City_Cape_Town_mask_rcp85) <- as.character(c(2021:2100))
row.names(HWD_City_Cape_Town_mask_rcp85) <- c("grid1", "grid2", "grid3")

HWD_City_Cape_Town_mask_rcp85  <- rownames_to_column(as.data.frame(HWD_City_Cape_Town_mask_rcp85), var="grid")
HWD_City_Cape_Town_mask_rcp85  <- pivot_longer(HWD_City_Cape_Town_mask_rcp85, cols = !grid, names_to = 'year', values_to = 'hwd')

ggplot(HWD_City_Cape_Town_mask_rcp85, aes(x = year, y = hwd)) +
  geom_boxplot(color = 'blue', fill='blue', alpha = 0.2) +
  ggtitle("City of Cape Town HWD (RCP 8.5)") + xlab("Year") + ylab("HWD (days)") + 
  theme_classic() + 
  theme(plot.title = element_text(size=20, face="bold", hjust = 0.5), axis.title.x = element_text(size=20, face="bold" ), axis.title.y = element_text(size=20, face="bold"), axis.text.y = element_text(size = 20))

###------------------------HWF-------------------------------------------------
setwd("~/Desktop/extreme-events-project-WesternCape/data/rcp85/output")
HWF_rcp85 <- brick('hw_ANN_climpact.sample_historical_NA_2021-2100.nc', varname = 'hwf_tx90')

# Crop the raster-CapeWinelands
HWF_City_Cape_Town_mask_rcp85 <- mask(HWF_rcp85,buffer(City_Cape_Town, width = 0.05)) %>%
  as.matrix() 

HWF_City_Cape_Town_mask_rcp85 <- HWF_City_Cape_Town_mask_rcp85[rowSums(is.na(HWF_City_Cape_Town_mask_rcp85)) != ncol(HWF_City_Cape_Town_mask_rcp85), ]
HWF_City_Cape_Town_mask_rcp85[,"X2100.07.02"] <- 2*HWF_City_Cape_Town_mask_rcp85[,"X2099.07.02"] - HWF_City_Cape_Town_mask_rcp85[,"X2098.07.02"]
HWF_City_Cape_Town_mask_rcp85 <- t(apply(HWF_City_Cape_Town_mask_rcp85, 1, na.approx))
#HWA_West_Coast_mask_rcp85 <- data.frame(Reduce(rbind, HWA_West_Coast_mask_rcp85))
colnames(HWF_City_Cape_Town_mask_rcp85) <- as.character(c(2021:2100))
row.names(HWF_City_Cape_Town_mask_rcp85) <- c("grid1", "grid2", "grid3")

HWF_City_Cape_Town_mask_rcp85  <- rownames_to_column(as.data.frame(HWF_City_Cape_Town_mask_rcp85), var="grid")
HWF_City_Cape_Town_mask_rcp85  <- pivot_longer(HWF_City_Cape_Town_mask_rcp85, cols = !grid, names_to = 'year', values_to = 'hwf')

ggplot(HWF_City_Cape_Town_mask_rcp85, aes(x = year, y = hwf)) +
  geom_boxplot(color = 'purple', fill='purple', alpha = 0.2) +
  ggtitle("City of Cape Town HWF (RCP 8.5)") + xlab("Year") + ylab("HWF (days)") + 
  theme_classic() + 
  theme(plot.title = element_text(size=20, face="bold", hjust = 0.5), axis.title.x = element_text(size=20, face="bold" ), axis.title.y = element_text(size=20, face="bold"), axis.text.y = element_text(size = 20))

###------------------------HWM-------------------------------------------------
setwd("~/Desktop/extreme-events-project-WesternCape/data/rcp85/output")
HWM_rcp85 <- brick('hw_ANN_climpact.sample_historical_NA_2021-2100.nc', varname = 'hwm_tx90')

# Crop the raster-CapeWinelands
HWM_City_Cape_Town_mask_rcp85 <- mask(HWM_rcp85,buffer(City_Cape_Town, width = 0.05)) %>%
  as.matrix() 

HWM_City_Cape_Town_mask_rcp85 <- HWM_City_Cape_Town_mask_rcp85[rowSums(is.na(HWM_City_Cape_Town_mask_rcp85)) != ncol(HWM_City_Cape_Town_mask_rcp85), ]
HWM_City_Cape_Town_mask_rcp85[,"X2100.07.02"] <- 2*HWM_City_Cape_Town_mask_rcp85[,"X2099.07.02"] - HWM_City_Cape_Town_mask_rcp85[,"X2098.07.02"]
HWM_City_Cape_Town_mask_rcp85 <- t(apply(HWM_City_Cape_Town_mask_rcp85, 1, na.approx))
#HWA_West_Coast_mask_rcp85 <- data.frame(Reduce(rbind, HWA_West_Coast_mask_rcp85))
colnames(HWM_City_Cape_Town_mask_rcp85) <- as.character(c(2021:2100))
row.names(HWM_City_Cape_Town_mask_rcp85) <- c("grid1", "grid2", "grid3")

HWM_City_Cape_Town_mask_rcp85  <- rownames_to_column(as.data.frame(HWM_City_Cape_Town_mask_rcp85), var="grid")
HWM_City_Cape_Town_mask_rcp85  <- pivot_longer(HWM_City_Cape_Town_mask_rcp85, cols = !grid, names_to = 'year', values_to = 'hwm')

ggplot(HWM_City_Cape_Town_mask_rcp85, aes(x = year, y = hwm)) +
  geom_boxplot(color = 'orange', fill='orange', alpha = 0.2) +
  ggtitle("City of Cape Town HWM (RCP 8.5)") + xlab("Year") + ylab("HWM (days)") + 
  theme_classic() + 
  theme(plot.title = element_text(size=20, face="bold", hjust = 0.5), axis.title.x = element_text(size=20, face="bold" ), axis.title.y = element_text(size=20, face="bold"), axis.text.y = element_text(size = 20))

###------------------------HWN-------------------------------------------------
setwd("~/Desktop/extreme-events-project-WesternCape/data/rcp85/output")
HWN_rcp85 <- brick('hw_ANN_climpact.sample_historical_NA_2021-2100.nc', varname = 'hwn_tx90')

# Crop the raster-CapeWinelands
HWN_City_Cape_Town_mask_rcp85 <- mask(HWN_rcp85,buffer(City_Cape_Town, width = 0.05)) %>%
  as.matrix() 

HWN_City_Cape_Town_mask_rcp85 <- HWN_City_Cape_Town_mask_rcp85[rowSums(is.na(HWN_City_Cape_Town_mask_rcp85)) != ncol(HWN_City_Cape_Town_mask_rcp85), ]
HWN_City_Cape_Town_mask_rcp85[,"X2100.07.02"] <- 2*HWN_City_Cape_Town_mask_rcp85[,"X2099.07.02"] - HWN_City_Cape_Town_mask_rcp85[,"X2098.07.02"]
HWN_City_Cape_Town_mask_rcp85 <- t(apply(HWN_City_Cape_Town_mask_rcp85, 1, na.approx))
#HWA_West_Coast_mask_rcp85 <- data.frame(Reduce(rbind, HWA_West_Coast_mask_rcp85))
colnames(HWN_City_Cape_Town_mask_rcp85) <- as.character(c(2021:2100))
row.names(HWN_City_Cape_Town_mask_rcp85) <- c("grid1", "grid2", "grid3")

HWN_City_Cape_Town_mask_rcp85  <- rownames_to_column(as.data.frame(HWN_City_Cape_Town_mask_rcp85), var="grid")
HWN_City_Cape_Town_mask_rcp85  <- pivot_longer(HWN_City_Cape_Town_mask_rcp85, cols = !grid, names_to = 'year', values_to = 'hwn')

ggplot(HWN_City_Cape_Town_mask_rcp85, aes(x = year, y = hwn)) +
  geom_boxplot(color = 'pink', fill='pink', alpha = 0.2) +
  ggtitle("City of Cape Town HWN (RCP 8.5)") + xlab("Year") + ylab("HWN (events)") + 
  theme_classic() + 
  theme(plot.title = element_text(size=20, face="bold", hjust = 0.5), axis.title.x = element_text(size=20, face="bold" ), axis.title.y = element_text(size=20, face="bold"), axis.text.y = element_text(size = 20))

#--------------------------Save-Outputs-RCP8.5----------------------------------
write_rds(list(HWA_Cape_Winelands_rcp85 = HWA_Cape_Winelands_mask_rcp85, HWD_Cape_Winelands_rcp85 = HWD_Cape_Winelands_mask_rcp85, HWF_Cape_Winelands_rcp85 = HWF_Cape_Winelands_mask_rcp85,
               HWM_Cape_Winelands_rcp85 = HWM_Cape_Winelands_mask_rcp85, HWN_Cape_Winelands_rcp85 = HWN_Cape_Winelands_mask_rcp85, HWA_Central_Karoo_rcp85 =  HWA_Central_Karoo_mask_rcp85,
               HWD_Central_Karoo_rcp85 = HWD_Central_Karoo_mask_rcp85, HWF_Central_Karoo_rcp85 = HWF_Central_Karoo_mask_rcp85, HWM_Central_Karoo_rcp85 =  HWM_Central_Karoo_mask_rcp85,
               HWN_Central_Karoo_rcp85 = HWN_Central_Karoo_mask_rcp85, HWA_City_Cape_Town_rcp85 =HWA_City_Cape_Town_mask_rcp85, HWD_City_Cape_Town_rcp85 = HWD_City_Cape_Town_mask_rcp85,
               HWF_City_Cape_Town_rcp85 =  HWF_City_Cape_Town_mask_rcp85, HWM_City_Cape_Town_rcp85 =  HWM_City_Cape_Town_mask_rcp85, HWN_City_Cape_Town_rcp85 =HWN_City_Cape_Town_mask_rcp85,
               HWA_Eden_rcp85 =  HWA_Eden_mask_rcp85, HWD_Eden_rcp85 =  HWD_Eden_mask_rcp85, HWF_Eden_rcp85 =  HWF_Eden_mask_rcp85, HWM_Eden_rcp85 = HWM_Eden_mask_rcp85, HWN_Eden_rcp85 =  HWN_Eden_mask_rcp85,
               HWA_Overberg_rcp85 =  HWA_Overberg_mask_rcp85, HWD_Overberg_rcp85 =  HWD_Overberg_mask_rcp85,HWF_Overberg_rcp85 = HWF_Overberg_mask_rcp85,HWM_Overberg_rcp85 =HWM_Overberg_mask_rcp85, HWN_Overberg_rcp85 = HWN_Overberg_mask_rcp85,
               HWA_West_Coast_rcp85 = HWA_West_Coast_mask_rcp85, HWD_West_Coast_rcp85 = HWD_West_Coast_mask_rcp85, HWF_West_Coast_rcp85 = HWF_West_Coast_mask_rcp85, HWM_West_Coast_rcp85 = HWM_West_Coast_mask_rcp85, HWN_West_Coast_rcp85 = HWN_West_Coast_mask_rcp85),
          path = file.path(data_dir ="/Users/hebowen/Desktop/extreme-events-project-WesternCape/data/rcp85", "regional_HW_rcp85.Rds"))






















