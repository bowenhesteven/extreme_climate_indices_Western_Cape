library(ncdf4)
library(raster)
library(rgdal)
library(ggplot2)
library(abind)


#--------------------------rcp2.6 scenario--------------------------------------------------------
##--------------------------202101-204012----------------------------------------
###------------------------------CDD-----------------------------------------------
setwd("~/Desktop/extreme-events-project-WesternCape/data/rcp26/output/21-40")
cdd_rcp26_21_40 <- nc_open('cdd_ANN_climpact.sample_historical_NA_2021-2040.nc')
lon <- ncvar_get(cdd_rcp26_21_40, "lon")
lat <- ncvar_get(cdd_rcp26_21_40, "lat", verbose = F)

cdd_rcp26_21_40.array <- ncvar_get(cdd_rcp26_21_40, "cdd")

avg_cdd_rcp26_21_40 <- apply(cdd_rcp26_21_40.array, c(1,2), mean)
avg_cdd_rcp26_21_40_raster <- raster(t(avg_cdd_rcp26_21_40), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
avg_cdd_rcp26_21_40_raster <- flip(avg_cdd_rcp26_21_40_raster, direction = 'y')
plot(avg_cdd_rcp26_21_40_raster, main = "Mean CDD Index 2021/01-2040/12")
writeRaster(avg_cdd_rcp26_21_40_raster, filename = "~/Desktop/extreme-events-project-WesternCape/data/rcp26/output/avg_cdd_rcp26_21_40.nc", format = "CDF", overwrite = TRUE)

#----------------------------WSDI--------------------------------------------
wsdi_rcp26_21_40 <- nc_open('wsdi_ANN_climpact.sample_historical_NA_2021-2040.nc')
lon <- ncvar_get(wsdi_rcp26_21_40, "lon")
lat <- ncvar_get(wsdi_rcp26_21_40, "lat", verbose = F)

wsdi_rcp26_21_40.array <- ncvar_get(wsdi_rcp26_21_40, "wsdi")

avg_wsdi_rcp26_21_40 <- apply(wsdi_rcp26_21_40.array, c(1,2), mean)
avg_wsdi_rcp26_21_40_raster <- raster(t(avg_wsdi_rcp26_21_40), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
avg_wsdi_rcp26_21_40_raster <- flip(avg_wsdi_rcp26_21_40_raster, direction = 'y')
plot(avg_wsdi_rcp26_21_40_raster, main = "Mean WSDI Index 2021/01-2040/12")
writeRaster(avg_wsdi_rcp26_21_40_raster, filename = "~/Desktop/extreme-events-project-WesternCape/data/rcp26/output/avg_wsdi_rcp26_21_40.nc", format = "CDF", overwrite = TRUE)

#----------------------------SPEI--------------------------------------------
spei_rcp26_21_40 <- nc_open('spei_MON_climpact.sample_historical_NA_2021-2040.nc')
lon <- ncvar_get(spei_rcp26_21_40, "lon")
lat <- ncvar_get(spei_rcp26_21_40, "lat", verbose = F)

spei_rcp26_21_40.array <- ncvar_get(spei_rcp26_21_40, "spei")

# scale = 3 month
avg_spei_rcp26_21_40_3month <- apply(spei_rcp26_21_40.array[,,,1], c(1,2), mean, na.rm = TRUE)
avg_spei_rcp26_21_40_3month_raster <- raster(t(avg_spei_rcp26_21_40_3month), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
avg_spei_rcp26_21_40_3month_raster <- flip(avg_spei_rcp26_21_40_3month_raster, direction = 'y')
plot(avg_spei_rcp26_21_40_3month_raster, main = "Mean SPEI 2021/01-2040/12 (scale = 3 month)")
writeRaster(avg_spei_rcp26_21_40_3month_raster, filename = "~/Desktop/extreme-events-project-WesternCape/data/rcp26/output/avg_spei_3mon_rcp26_21_40.nc", format = "CDF", overwrite = TRUE)


# scale = 6 month
avg_spei_rcp26_21_40_6month <- apply(spei_rcp26_21_40.array[,,,2], c(1,2), mean, na.rm = TRUE)
avg_spei_rcp26_21_40_6month_raster <- raster(t(avg_spei_rcp26_21_40_6month), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
avg_spei_rcp26_21_40_6month_raster <- flip(avg_spei_rcp26_21_40_6month_raster, direction = 'y')
plot(avg_spei_rcp26_21_40_6month_raster, main = "Mean SPEI 2021/01-2040/12 (scale = 6 month)")
writeRaster(avg_spei_rcp26_21_40_6month_raster, filename = "~/Desktop/extreme-events-project-WesternCape/data/rcp26/output/avg_spei_6mon_rcp26_21_40.nc", format = "CDF", overwrite = TRUE)

# scale = 12 month
avg_spei_rcp26_21_40_12month <- apply(spei_rcp26_21_40.array[,,,3], c(1,2), mean, na.rm = TRUE)
avg_spei_rcp26_21_40_12month_raster <- raster(t(avg_spei_rcp26_21_40_12month), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
avg_spei_rcp26_21_40_12month_raster <- flip(avg_spei_rcp26_21_40_12month_raster, direction = 'y')
plot(avg_spei_rcp26_21_40_12month_raster, main = "Mean SPEI 2021/01-2040/12 (scale = 12 month)")
writeRaster(avg_spei_rcp26_21_40_12month_raster, filename = "~/Desktop/extreme-events-project-WesternCape/data/rcp26/output/avg_spei_12mon_rcp26_21_40.nc", format = "CDF", overwrite = TRUE)

#------------------------------HW-----------------------------------------
hw_rcp26_21_40 <- nc_open('hw_ANN_climpact.sample_historical_NA_2021-2040.nc')
lon <- ncvar_get(hw_rcp26_21_40, "lon")
lat <- ncvar_get(hw_rcp26_21_40, "lat", verbose = F)

# HWF
hwf_rcp26_21_40.array <- ncvar_get(hw_rcp26_21_40, "hwf_tx90")

avg_hwf_rcp26_21_40 <- apply(hwf_rcp26_21_40.array, c(1,2), mean, na.rm = TRUE)
avg_hwf_rcp26_21_40_raster <- raster(t(avg_hwf_rcp26_21_40), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
avg_hwf_rcp26_21_40_raster <- flip(avg_hwf_rcp26_21_40_raster, direction = 'y')
plot(avg_hwf_rcp26_21_40_raster, main = "Mean HWF Index 2021/01-2040/12")
writeRaster(avg_hwf_rcp26_21_40_raster, filename = "~/Desktop/extreme-events-project-WesternCape/data/rcp26/output/avg_hwf_rcp26_21_40.nc", format = "CDF", overwrite = TRUE)

# HWD
hwd_rcp26_21_40.array <- ncvar_get(hw_rcp26_21_40, "hwd_tx90")

avg_hwd_rcp26_21_40 <- apply(hwd_rcp26_21_40.array, c(1,2), mean, na.rm = TRUE)
avg_hwd_rcp26_21_40_raster <- raster(t(avg_hwd_rcp26_21_40), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
avg_hwd_rcp26_21_40_raster <- flip(avg_hwd_rcp26_21_40_raster, direction = 'y')
plot(avg_hwd_rcp26_21_40_raster, main = "Mean HWD Index 2021/01-2040/12")
writeRaster(avg_hwd_rcp26_21_40_raster, filename = "~/Desktop/extreme-events-project-WesternCape/data/rcp26/output/avg_hwd_rcp26_21_40.nc", format = "CDF", overwrite = TRUE)

# HWM
hwm_rcp26_21_40.array <- ncvar_get(hw_rcp26_21_40, "hwm_tx90")

avg_hwm_rcp26_21_40 <- apply(hwm_rcp26_21_40.array, c(1,2), mean, na.rm = TRUE)
avg_hwm_rcp26_21_40_raster <- raster(t(avg_hwm_rcp26_21_40), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
avg_hwm_rcp26_21_40_raster <- flip(avg_hwm_rcp26_21_40_raster, direction = 'y')
plot(avg_hwm_rcp26_21_40_raster, main = "Mean HWM Index 2021/01-2040/12")
writeRaster(avg_hwm_rcp26_21_40_raster, filename = "~/Desktop/extreme-events-project-WesternCape/data/rcp26/output/avg_hwm_rcp26_21_40.nc", format = "CDF", overwrite = TRUE)

# HWA
hwa_rcp26_21_40.array <- ncvar_get(hw_rcp26_21_40, "hwa_tx90")

avg_hwa_rcp26_21_40 <- apply(hwa_rcp26_21_40.array, c(1,2), mean, na.rm = TRUE)
avg_hwa_rcp26_21_40_raster <- raster(t(avg_hwa_rcp26_21_40), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
avg_hwa_rcp26_21_40_raster <- flip(avg_hwa_rcp26_21_40_raster, direction = 'y')
plot(avg_hwa_rcp26_21_40_raster, main = "Mean HWA Index 2021/01-2040/12")
writeRaster(avg_hwa_rcp26_21_40_raster, filename = "~/Desktop/extreme-events-project-WesternCape/data/rcp26/output/avg_hwa_rcp26_21_40.nc", format = "CDF", overwrite = TRUE)

# HWN
hwn_rcp26_21_40.array <- ncvar_get(hw_rcp26_21_40, "hwn_tx90")

avg_hwn_rcp26_21_40 <- apply(hwn_rcp26_21_40.array, c(1,2), mean, na.rm = TRUE)
avg_hwn_rcp26_21_40_raster <- raster(t(avg_hwn_rcp26_21_40), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
avg_hwn_rcp26_21_40_raster <- flip(avg_hwn_rcp26_21_40_raster, direction = 'y')
plot(avg_hwn_rcp26_21_40_raster, main = "Mean HWN Index 2021/01-2040/12")
writeRaster(avg_hwn_rcp26_21_40_raster, filename = "~/Desktop/extreme-events-project-WesternCape/data/rcp26/output/avg_hwn_rcp26_21_40.nc", format = "CDF", overwrite = TRUE)
##--------------------------204001-206012----------------------------------------
###------------------------------CDD-----------------------------------------------
setwd("~/Desktop/extreme-events-project-WesternCape/data/rcp26/output/41-60")
cdd_rcp26_41_60 <- nc_open('cdd_ANN_climpact.sample_historical_NA_2041-2060.nc')
lon <- ncvar_get(cdd_rcp26_41_60, "lon")
lat <- ncvar_get(cdd_rcp26_41_60, "lat", verbose = F)

cdd_rcp26_41_60.array <- ncvar_get(cdd_rcp26_41_60, "cdd")

avg_cdd_rcp26_41_60 <- apply(cdd_rcp26_41_60.array, c(1,2), mean)
avg_cdd_rcp26_41_60_raster <- raster(t(avg_cdd_rcp26_41_60), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
avg_cdd_rcp26_41_60_raster <- flip(avg_cdd_rcp26_41_60_raster, direction = 'y')
plot(avg_cdd_rcp26_41_60_raster, main = "Mean CDD Index 2041/01-2060/12")
writeRaster(avg_cdd_rcp26_41_60_raster, filename = "~/Desktop/extreme-events-project-WesternCape/data/rcp26/output/avg_cdd_rcp26_41_60.nc", format = "CDF", overwrite = TRUE)

#----------------------------WSDI--------------------------------------------
wsdi_rcp26_41_60 <- nc_open('wsdi_ANN_climpact.sample_historical_NA_2041-2060.nc')
lon <- ncvar_get(wsdi_rcp26_41_60, "lon")
lat <- ncvar_get(wsdi_rcp26_41_60, "lat", verbose = F)

wsdi_rcp26_41_60.array <- ncvar_get(wsdi_rcp26_41_60, "wsdi")

avg_wsdi_rcp26_41_60 <- apply(wsdi_rcp26_41_60.array, c(1,2), mean)
avg_wsdi_rcp26_41_60_raster <- raster(t(avg_wsdi_rcp26_41_60), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
avg_wsdi_rcp26_41_60_raster <- flip(avg_wsdi_rcp26_41_60_raster, direction = 'y')
plot(avg_wsdi_rcp26_41_60_raster, main = "Mean WSDI Index 2041/01-2060/12")
writeRaster(avg_wsdi_rcp26_41_60_raster, filename = "~/Desktop/extreme-events-project-WesternCape/data/rcp26/output/avg_wsdi_rcp26_41_60.nc", format = "CDF", overwrite = TRUE)

#----------------------------SPEI--------------------------------------------
spei_rcp26_41_60 <- nc_open('spei_MON_climpact.sample_historical_NA_2041-2060.nc')
lon <- ncvar_get(spei_rcp26_41_60, "lon")
lat <- ncvar_get(spei_rcp26_41_60, "lat", verbose = F)

spei_rcp26_41_60.array <- ncvar_get(spei_rcp26_41_60, "spei")

# scale = 3 month
avg_spei_rcp26_41_60_3month <- apply(spei_rcp26_41_60.array[,,,1], c(1,2), mean, na.rm = TRUE)
avg_spei_rcp26_41_60_3month_raster <- raster(t(avg_spei_rcp26_41_60_3month), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
avg_spei_rcp26_41_60_3month_raster <- flip(avg_spei_rcp26_41_60_3month_raster, direction = 'y')
plot(avg_spei_rcp26_41_60_3month_raster, main = "Mean SPEI 2041/01-2060/12 (scale = 3 month)")
writeRaster(avg_spei_rcp26_41_60_3month_raster, filename = "~/Desktop/extreme-events-project-WesternCape/data/rcp26/output/avg_spei_3mon_rcp26_41_60.nc", format = "CDF", overwrite = TRUE)

# scale = 6 month
avg_spei_rcp26_41_60_6month <- apply(spei_rcp26_41_60.array[,,,2], c(1,2), mean, na.rm = TRUE)
avg_spei_rcp26_41_60_6month_raster <- raster(t(avg_spei_rcp26_41_60_6month), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
avg_spei_rcp26_41_60_6month_raster <- flip(avg_spei_rcp26_41_60_6month_raster, direction = 'y')
plot(avg_spei_rcp26_41_60_6month_raster, main = "Mean SPEI 2041/01-2060/12 (scale = 6 month)")
writeRaster(avg_spei_rcp26_41_60_6month_raster, filename = "~/Desktop/extreme-events-project-WesternCape/data/rcp26/output/avg_spei_6mon_rcp26_41_60.nc", format = "CDF", overwrite = TRUE)

# scale = 12 month
avg_spei_rcp26_41_60_12month <- apply(spei_rcp26_41_60.array[,,,3], c(1,2), mean, na.rm = TRUE)
avg_spei_rcp26_41_60_12month_raster <- raster(t(avg_spei_rcp26_41_60_12month), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
avg_spei_rcp26_41_60_12month_raster <- flip(avg_spei_rcp26_41_60_12month_raster, direction = 'y')
plot(avg_spei_rcp26_41_60_12month_raster, main = "Mean SPEI 2041/01-2060/12 (scale = 12 month)")
writeRaster(avg_spei_rcp26_41_60_12month_raster, filename = "~/Desktop/extreme-events-project-WesternCape/data/rcp26/output/avg_spei_12mon_rcp26_41_60.nc", format = "CDF", overwrite = TRUE)

#------------------------------HW-----------------------------------------
hw_rcp26_41_60 <- nc_open('hw_ANN_climpact.sample_historical_NA_2041-2060.nc')
lon <- ncvar_get(hw_rcp26_41_60, "lon")
lat <- ncvar_get(hw_rcp26_41_60, "lat", verbose = F)

# HWF
hwf_rcp26_41_60.array <- ncvar_get(hw_rcp26_41_60, "hwf_tx90")

avg_hwf_rcp26_41_60 <- apply(hwf_rcp26_41_60.array, c(1,2), mean, na.rm = TRUE)
avg_hwf_rcp26_41_60_raster <- raster(t(avg_hwf_rcp26_41_60), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
avg_hwf_rcp26_41_60_raster <- flip(avg_hwf_rcp26_41_60_raster, direction = 'y')
plot(avg_hwf_rcp26_41_60_raster, main = "Mean HWF Index 2041/01-2060/12")
writeRaster(avg_hwf_rcp26_41_60_raster, filename = "~/Desktop/extreme-events-project-WesternCape/data/rcp26/output/avg_hwf_rcp26_41_60.nc", format = "CDF", overwrite = TRUE)

# HWD
hwd_rcp26_41_60.array <- ncvar_get(hw_rcp26_41_60, "hwd_tx90")

avg_hwd_rcp26_41_60 <- apply(hwd_rcp26_41_60.array, c(1,2), mean, na.rm = TRUE)
avg_hwd_rcp26_41_60_raster <- raster(t(avg_hwd_rcp26_41_60), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
avg_hwd_rcp26_41_60_raster <- flip(avg_hwd_rcp26_41_60_raster, direction = 'y')
plot(avg_hwd_rcp26_41_60_raster, main = "Mean HWD Index 2041/01-2060/12")
writeRaster(avg_hwd_rcp26_41_60_raster, filename = "~/Desktop/extreme-events-project-WesternCape/data/rcp26/output/avg_hwd_rcp26_41_60.nc", format = "CDF", overwrite = TRUE)

# HWM
hwm_rcp26_41_60.array <- ncvar_get(hw_rcp26_41_60, "hwm_tx90")

avg_hwm_rcp26_41_60 <- apply(hwm_rcp26_41_60.array, c(1,2), mean, na.rm = TRUE)
avg_hwm_rcp26_41_60_raster <- raster(t(avg_hwm_rcp26_41_60), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
avg_hwm_rcp26_41_60_raster <- flip(avg_hwm_rcp26_41_60_raster, direction = 'y')
plot(avg_hwm_rcp26_41_60_raster, main = "Mean HWM Index 2041/01-2060/12")
writeRaster(avg_hwm_rcp26_41_60_raster, filename = "~/Desktop/extreme-events-project-WesternCape/data/rcp26/output/avg_hwm_rcp26_41_60.nc", format = "CDF", overwrite = TRUE)

# HWA
hwa_rcp26_41_60.array <- ncvar_get(hw_rcp26_41_60, "hwa_tx90")

avg_hwa_rcp26_41_60 <- apply(hwa_rcp26_41_60.array, c(1,2), mean, na.rm = TRUE)
avg_hwa_rcp26_41_60_raster <- raster(t(avg_hwa_rcp26_41_60), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
avg_hwa_rcp26_41_60_raster <- flip(avg_hwa_rcp26_41_60_raster, direction = 'y')
plot(avg_hwa_rcp26_41_60_raster, main = "Mean HWA Index 2041/01-2060/12")
writeRaster(avg_hwa_rcp26_41_60_raster, filename = "~/Desktop/extreme-events-project-WesternCape/data/rcp26/output/avg_hwa_rcp26_41_60.nc", format = "CDF", overwrite = TRUE)

# HWN
hwn_rcp26_41_60.array <- ncvar_get(hw_rcp26_41_60, "hwn_tx90")

avg_hwn_rcp26_41_60 <- apply(hwn_rcp26_41_60.array, c(1,2), mean, na.rm = TRUE)
avg_hwn_rcp26_41_60_raster <- raster(t(avg_hwn_rcp26_41_60), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
avg_hwn_rcp26_41_60_raster <- flip(avg_hwn_rcp26_41_60_raster, direction = 'y')
plot(avg_hwn_rcp26_41_60_raster, main = "Mean HWN Index 2041/01-2060/12")
writeRaster(avg_hwn_rcp26_41_60_raster, filename = "~/Desktop/extreme-events-project-WesternCape/data/rcp26/output/avg_hwn_rcp26_41_60.nc", format = "CDF", overwrite = TRUE)
##--------------------------206101-208012----------------------------------------
###------------------------------CDD-----------------------------------------------
setwd("~/Desktop/extreme-events-project-WesternCape/data/rcp26/output/61-80")
cdd_rcp26_61_80 <- nc_open('cdd_ANN_climpact.sample_historical_NA_2061-2080.nc')
lon <- ncvar_get(cdd_rcp26_61_80, "lon")
lat <- ncvar_get(cdd_rcp26_61_80, "lat", verbose = F)

cdd_rcp26_61_80.array <- ncvar_get(cdd_rcp26_61_80, "cdd")

avg_cdd_rcp26_61_80 <- apply(cdd_rcp26_61_80.array, c(1,2), mean)
avg_cdd_rcp26_61_80_raster <- raster(t(avg_cdd_rcp26_61_80), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
avg_cdd_rcp26_61_80_raster <- flip(avg_cdd_rcp26_61_80_raster, direction = 'y')
plot(avg_cdd_rcp26_61_80_raster, main = "Mean CDD Index 2061/01-2080/12")
writeRaster(avg_cdd_rcp26_61_80_raster, filename = "~/Desktop/extreme-events-project-WesternCape/data/rcp26/output/avg_cdd_rcp26_61_80.nc", format = "CDF", overwrite = TRUE)

#----------------------------WSDI--------------------------------------------
wsdi_rcp26_61_80 <- nc_open('wsdi_ANN_climpact.sample_historical_NA_2061-2080.nc')
lon <- ncvar_get(wsdi_rcp26_61_80, "lon")
lat <- ncvar_get(wsdi_rcp26_61_80, "lat", verbose = F)

wsdi_rcp26_61_80.array <- ncvar_get(wsdi_rcp26_61_80, "wsdi")

avg_wsdi_rcp26_61_80 <- apply(wsdi_rcp26_61_80.array, c(1,2), mean)
avg_wsdi_rcp26_61_80_raster <- raster(t(avg_wsdi_rcp26_61_80), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
avg_wsdi_rcp26_61_80_raster <- flip(avg_wsdi_rcp26_61_80_raster, direction = 'y')
plot(avg_wsdi_rcp26_61_80_raster, main = "Mean WSDI Index 2061/01-2080/12")
writeRaster(avg_wsdi_rcp26_61_80_raster, filename = "~/Desktop/extreme-events-project-WesternCape/data/rcp26/output/avg_wsdi_rcp26_61_80.nc", format = "CDF", overwrite = TRUE)

#----------------------------SPEI--------------------------------------------
spei_rcp26_61_80 <- nc_open('spei_MON_climpact.sample_historical_NA_2061-2080.nc')
lon <- ncvar_get(spei_rcp26_61_80, "lon")
lat <- ncvar_get(spei_rcp26_61_80, "lat", verbose = F)

spei_rcp26_61_80.array <- ncvar_get(spei_rcp26_61_80, "spei")

# scale = 3 month
avg_spei_rcp26_61_80_3month <- apply(spei_rcp26_61_80.array[,,,1], c(1,2), mean, na.rm = TRUE)
avg_spei_rcp26_61_80_3month_raster <- raster(t(avg_spei_rcp26_61_80_3month), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
avg_spei_rcp26_61_80_3month_raster <- flip(avg_spei_rcp26_61_80_3month_raster, direction = 'y')
plot(avg_spei_rcp26_61_80_3month_raster, main = "Mean SPEI 2061/01-2080/12 (scale = 3 month)")
writeRaster(avg_spei_rcp26_61_80_3month_raster, filename = "~/Desktop/extreme-events-project-WesternCape/data/rcp26/output/avg_spei_3mon_rcp26_61_80.nc", format = "CDF", overwrite = TRUE)

# scale = 6 month
avg_spei_rcp26_61_80_6month <- apply(spei_rcp26_61_80.array[,,,2], c(1,2), mean, na.rm = TRUE)
avg_spei_rcp26_61_80_6month_raster <- raster(t(avg_spei_rcp26_61_80_6month), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
avg_spei_rcp26_61_80_6month_raster <- flip(avg_spei_rcp26_61_80_6month_raster, direction = 'y')
plot(avg_spei_rcp26_61_80_6month_raster, main = "Mean SPEI 2061/01-2080/12 (scale = 6 month)")
writeRaster(avg_spei_rcp26_61_80_6month_raster, filename = "~/Desktop/extreme-events-project-WesternCape/data/rcp26/output/avg_spei_6mon_rcp26_61_80.nc", format = "CDF", overwrite = TRUE)

# scale = 12 month
avg_spei_rcp26_61_80_12month <- apply(spei_rcp26_61_80.array[,,,3], c(1,2), mean, na.rm = TRUE)
avg_spei_rcp26_61_80_12month_raster <- raster(t(avg_spei_rcp26_61_80_12month), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
avg_spei_rcp26_61_80_12month_raster <- flip(avg_spei_rcp26_61_80_12month_raster, direction = 'y')
plot(avg_spei_rcp26_61_80_12month_raster, main = "Mean SPEI 2061/01-2080/12 (scale = 12 month)")
writeRaster(avg_spei_rcp26_61_80_12month_raster, filename = "~/Desktop/extreme-events-project-WesternCape/data/rcp26/output/avg_spei_12mon_rcp26_61_80.nc", format = "CDF", overwrite = TRUE)

#------------------------------HW-----------------------------------------
hw_rcp26_61_80 <- nc_open('hw_ANN_climpact.sample_historical_NA_2061-2080.nc')
lon <- ncvar_get(hw_rcp26_61_80, "lon")
lat <- ncvar_get(hw_rcp26_61_80, "lat", verbose = F)

# HWF
hwf_rcp26_61_80.array <- ncvar_get(hw_rcp26_61_80, "hwf_tx90")

avg_hwf_rcp26_61_80 <- apply(hwf_rcp26_61_80.array, c(1,2), mean, na.rm = TRUE)
avg_hwf_rcp26_61_80_raster <- raster(t(avg_hwf_rcp26_61_80), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
avg_hwf_rcp26_61_80_raster <- flip(avg_hwf_rcp26_61_80_raster, direction = 'y')
plot(avg_hwf_rcp26_61_80_raster, main = "Mean HWF Index 2061/01-2080/12")
writeRaster(avg_hwf_rcp26_61_80_raster, filename = "~/Desktop/extreme-events-project-WesternCape/data/rcp26/output/avg_hwf_rcp26_61_80.nc", format = "CDF", overwrite = TRUE)

# HWD
hwd_rcp26_61_80.array <- ncvar_get(hw_rcp26_61_80, "hwd_tx90")

avg_hwd_rcp26_61_80 <- apply(hwd_rcp26_61_80.array, c(1,2), mean, na.rm = TRUE)
avg_hwd_rcp26_61_80_raster <- raster(t(avg_hwd_rcp26_61_80), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
avg_hwd_rcp26_61_80_raster <- flip(avg_hwd_rcp26_61_80_raster, direction = 'y')
plot(avg_hwd_rcp26_61_80_raster, main = "Mean HWD Index 2061/01-2080/12")
writeRaster(avg_hwd_rcp26_61_80_raster, filename = "~/Desktop/extreme-events-project-WesternCape/data/rcp26/output/avg_hwd_rcp26_61_80.nc", format = "CDF", overwrite = TRUE)

# HWM
hwm_rcp26_61_80.array <- ncvar_get(hw_rcp26_61_80, "hwm_tx90")

avg_hwm_rcp26_61_80 <- apply(hwm_rcp26_61_80.array, c(1,2), mean, na.rm = TRUE)
avg_hwm_rcp26_61_80_raster <- raster(t(avg_hwm_rcp26_61_80), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
avg_hwm_rcp26_61_80_raster <- flip(avg_hwm_rcp26_61_80_raster, direction = 'y')
plot(avg_hwm_rcp26_61_80_raster, main = "Mean HWM Index 2061/01-2080/12")
writeRaster(avg_hwm_rcp26_61_80_raster, filename = "~/Desktop/extreme-events-project-WesternCape/data/rcp26/output/avg_hwm_rcp26_61_80.nc", format = "CDF", overwrite = TRUE)

# HWA
hwa_rcp26_61_80.array <- ncvar_get(hw_rcp26_61_80, "hwa_tx90")

avg_hwa_rcp26_61_80 <- apply(hwa_rcp26_61_80.array, c(1,2), mean, na.rm = TRUE)
avg_hwa_rcp26_61_80_raster <- raster(t(avg_hwa_rcp26_61_80), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
avg_hwa_rcp26_61_80_raster <- flip(avg_hwa_rcp26_61_80_raster, direction = 'y')
plot(avg_hwa_rcp26_61_80_raster, main = "Mean HWA Index 2061/01-2080/12")
writeRaster(avg_hwa_rcp26_61_80_raster, filename = "~/Desktop/extreme-events-project-WesternCape/data/rcp26/output/avg_hwa_rcp26_61_80.nc", format = "CDF", overwrite = TRUE)

# HWN
hwn_rcp26_61_80.array <- ncvar_get(hw_rcp26_61_80, "hwn_tx90")

avg_hwn_rcp26_61_80 <- apply(hwn_rcp26_61_80.array, c(1,2), mean, na.rm = TRUE)
avg_hwn_rcp26_61_80_raster <- raster(t(avg_hwn_rcp26_61_80), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
avg_hwn_rcp26_61_80_raster <- flip(avg_hwn_rcp26_61_80_raster, direction = 'y')
plot(avg_hwn_rcp26_61_80_raster, main = "Mean HWN Index 2061/01-2080/12")
writeRaster(avg_hwn_rcp26_61_80_raster, filename = "~/Desktop/extreme-events-project-WesternCape/data/rcp26/output/avg_hwn_rcp26_61_80.nc", format = "CDF", overwrite = TRUE)
##--------------------------208101-210012----------------------------------------
###------------------------------CDD-----------------------------------------------
setwd("~/Desktop/extreme-events-project-WesternCape/data/rcp26/output/81-100")
cdd_rcp26_81_100 <- nc_open('cdd_ANN_climpact.sample_historical_NA_2081-2100.nc')
lon <- ncvar_get(cdd_rcp26_81_100, "lon")
lat <- ncvar_get(cdd_rcp26_81_100, "lat", verbose = F)

cdd_rcp26_81_100.array <- ncvar_get(cdd_rcp26_81_100, "cdd")

avg_cdd_rcp26_81_100 <- apply(cdd_rcp26_81_100.array, c(1,2), mean)
avg_cdd_rcp26_81_100_raster <- raster(t(avg_cdd_rcp26_81_100), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
avg_cdd_rcp26_81_100_raster <- flip(avg_cdd_rcp26_81_100_raster, direction = 'y')
plot(avg_cdd_rcp26_81_100_raster, main = "Mean CDD Index 2081/01-2100/12")
writeRaster(avg_cdd_rcp26_81_100_raster, filename = "~/Desktop/extreme-events-project-WesternCape/data/rcp26/output/avg_cdd_rcp26_81_100.nc", format = "CDF", overwrite = TRUE)

#----------------------------WSDI--------------------------------------------
wsdi_rcp26_81_100 <- nc_open('wsdi_ANN_climpact.sample_historical_NA_2081-2100.nc')
lon <- ncvar_get(wsdi_rcp26_81_100, "lon")
lat <- ncvar_get(wsdi_rcp26_81_100, "lat", verbose = F)

wsdi_rcp26_81_100.array <- ncvar_get(wsdi_rcp26_81_100, "wsdi")

avg_wsdi_rcp26_81_100 <- apply(wsdi_rcp26_81_100.array, c(1,2), mean)
avg_wsdi_rcp26_81_100_raster <- raster(t(avg_wsdi_rcp26_81_100), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
avg_wsdi_rcp26_81_100_raster <- flip(avg_wsdi_rcp26_81_100_raster, direction = 'y')
plot(avg_wsdi_rcp26_81_100_raster, main = "Mean WSDI Index 2081/01-2100/12")
writeRaster(avg_wsdi_rcp26_81_100_raster, filename = "~/Desktop/extreme-events-project-WesternCape/data/rcp26/output/avg_wsdi_rcp26_81_100.nc", format = "CDF", overwrite = TRUE)

#----------------------------SPEI--------------------------------------------
spei_rcp26_81_100 <- nc_open('spei_MON_climpact.sample_historical_NA_2081-2100.nc')
lon <- ncvar_get(spei_rcp26_81_100, "lon")
lat <- ncvar_get(spei_rcp26_81_100, "lat", verbose = F)

spei_rcp26_81_100.array <- ncvar_get(spei_rcp26_81_100, "spei")

# scale = 3 month
avg_spei_rcp26_81_100_3month <- apply(spei_rcp26_81_100.array[,,,1], c(1,2), mean, na.rm = TRUE)
avg_spei_rcp26_81_100_3month_raster <- raster(t(avg_spei_rcp26_81_100_3month), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
avg_spei_rcp26_81_100_3month_raster <- flip(avg_spei_rcp26_81_100_3month_raster, direction = 'y')
plot(avg_spei_rcp26_81_100_3month_raster, main = "Mean SPEI 2081/01-2100/12 (scale = 3 month)")
writeRaster(avg_spei_rcp26_81_100_3month_raster, filename = "~/Desktop/extreme-events-project-WesternCape/data/rcp26/output/avg_spei_3mon_rcp26_81_100.nc", format = "CDF", overwrite = TRUE)

# scale = 6 month
avg_spei_rcp26_81_100_6month <- apply(spei_rcp26_81_100.array[,,,2], c(1,2), mean, na.rm = TRUE)
avg_spei_rcp26_81_100_6month_raster <- raster(t(avg_spei_rcp26_81_100_6month), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
avg_spei_rcp26_81_100_6month_raster <- flip(avg_spei_rcp26_81_100_6month_raster, direction = 'y')
plot(avg_spei_rcp26_81_100_6month_raster, main = "Mean SPEI 2081/01-2100/12 (scale = 6 month)")
writeRaster(avg_spei_rcp26_81_100_6month_raster, filename = "~/Desktop/extreme-events-project-WesternCape/data/rcp26/output/avg_spei_6mon_rcp26_81_100.nc", format = "CDF", overwrite = TRUE)

# scale = 12 month
avg_spei_rcp26_81_100_12month <- apply(spei_rcp26_81_100.array[,,,3], c(1,2), mean, na.rm = TRUE)
avg_spei_rcp26_81_100_12month_raster <- raster(t(avg_spei_rcp26_81_100_12month), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
avg_spei_rcp26_81_100_12month_raster <- flip(avg_spei_rcp26_81_100_12month_raster, direction = 'y')
plot(avg_spei_rcp26_81_100_12month_raster, main = "Mean SPEI 2081/01-2100/12 (scale = 12 month)")
writeRaster(avg_spei_rcp26_81_100_12month_raster, filename = "~/Desktop/extreme-events-project-WesternCape/data/rcp26/output/avg_spei_12mon_rcp26_81_100.nc", format = "CDF", overwrite = TRUE)

#------------------------------HW-----------------------------------------
hw_rcp26_81_100 <- nc_open('hw_ANN_climpact.sample_historical_NA_2081-2100.nc')
lon <- ncvar_get(hw_rcp26_81_100, "lon")
lat <- ncvar_get(hw_rcp26_81_100, "lat", verbose = F)

# HWF
hwf_rcp26_81_100.array <- ncvar_get(hw_rcp26_81_100, "hwf_tx90")

avg_hwf_rcp26_81_100 <- apply(hwf_rcp26_81_100.array, c(1,2), mean, na.rm = TRUE)
avg_hwf_rcp26_81_100_raster <- raster(t(avg_hwf_rcp26_81_100), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
avg_hwf_rcp26_81_100_raster <- flip(avg_hwf_rcp26_81_100_raster, direction = 'y')
plot(avg_hwf_rcp26_81_100_raster, main = "Mean HWF Index 2081/01-2100/12")
writeRaster(avg_hwf_rcp26_81_100_raster, filename = "~/Desktop/extreme-events-project-WesternCape/data/rcp26/output/avg_hwf_rcp26_81_100.nc", format = "CDF", overwrite = TRUE)

# HWD
hwd_rcp26_81_100.array <- ncvar_get(hw_rcp26_81_100, "hwd_tx90")

avg_hwd_rcp26_81_100 <- apply(hwd_rcp26_81_100.array, c(1,2), mean, na.rm = TRUE)
avg_hwd_rcp26_81_100_raster <- raster(t(avg_hwd_rcp26_81_100), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
avg_hwd_rcp26_81_100_raster <- flip(avg_hwd_rcp26_81_100_raster, direction = 'y')
plot(avg_hwd_rcp26_81_100_raster, main = "Mean HWD Index 2081/01-2100/12")
writeRaster(avg_hwd_rcp26_81_100_raster, filename = "~/Desktop/extreme-events-project-WesternCape/data/rcp26/output/avg_hwd_rcp26_81_100.nc", format = "CDF", overwrite = TRUE)

# HWM
hwm_rcp26_81_100.array <- ncvar_get(hw_rcp26_81_100, "hwm_tx90")

avg_hwm_rcp26_81_100 <- apply(hwm_rcp26_81_100.array, c(1,2), mean, na.rm = TRUE)
avg_hwm_rcp26_81_100_raster <- raster(t(avg_hwm_rcp26_81_100), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
avg_hwm_rcp26_81_100_raster <- flip(avg_hwm_rcp26_81_100_raster, direction = 'y')
plot(avg_hwm_rcp26_81_100_raster, main = "Mean HWM Index 2081/01-2100/12")
writeRaster(avg_hwm_rcp26_81_100_raster, filename = "~/Desktop/extreme-events-project-WesternCape/data/rcp26/output/avg_hwm_rcp26_81_100.nc", format = "CDF", overwrite = TRUE)

# HWA
hwa_rcp26_81_100.array <- ncvar_get(hw_rcp26_81_100, "hwa_tx90")

avg_hwa_rcp26_81_100 <- apply(hwa_rcp26_81_100.array, c(1,2), mean, na.rm = TRUE)
avg_hwa_rcp26_81_100_raster <- raster(t(avg_hwa_rcp26_81_100), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
avg_hwa_rcp26_81_100_raster <- flip(avg_hwa_rcp26_81_100_raster, direction = 'y')
plot(avg_hwa_rcp26_81_100_raster, main = "Mean HWA Index 2081/01-2100/12")
writeRaster(avg_hwa_rcp26_81_100_raster, filename = "~/Desktop/extreme-events-project-WesternCape/data/rcp26/output/avg_hwa_rcp26_81_100.nc", format = "CDF", overwrite = TRUE)

# HWN
hwn_rcp26_81_100.array <- ncvar_get(hw_rcp26_81_100, "hwn_tx90")

avg_hwn_rcp26_81_100 <- apply(hwn_rcp26_81_100.array, c(1,2), mean, na.rm = TRUE)
avg_hwn_rcp26_81_100_raster <- raster(t(avg_hwn_rcp26_81_100), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
avg_hwn_rcp26_81_100_raster <- flip(avg_hwn_rcp26_81_100_raster, direction = 'y')
plot(avg_hwn_rcp26_81_100_raster, main = "Mean HWN Index 2081/01-2100/12")
writeRaster(avg_hwn_rcp26_81_100_raster, filename = "~/Desktop/extreme-events-project-WesternCape/data/rcp26/output/avg_hwn_rcp26_81_100.nc", format = "CDF", overwrite = TRUE)
#--------------------------rcp4.5 scenario--------------------------------------------------------
##--------------------------202001-204012------------------------------------------
###------------------------------CDD-----------------------------------------------
setwd("~/Desktop/extreme-events-project-WesternCape/data/rcp45/output/21-40")
cdd_rcp45_21_40 <- nc_open('cdd_ANN_climpact.sample_historical_NA_2021-2040.nc')
lon <- ncvar_get(cdd_rcp45_21_40, "lon")
lat <- ncvar_get(cdd_rcp45_21_40, "lat", verbose = F)

cdd_rcp45_21_40.array <- ncvar_get(cdd_rcp45_21_40, "cdd")

avg_cdd_rcp45_21_40 <- apply(cdd_rcp45_21_40.array, c(1,2), mean)
avg_cdd_rcp45_21_40_raster <- raster(t(avg_cdd_rcp45_21_40), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
avg_cdd_rcp45_21_40_raster <- flip(avg_cdd_rcp45_21_40_raster, direction = 'y')
plot(avg_cdd_rcp45_21_40_raster, main = "Mean CDD Index 2021/01-2040/12")
writeRaster(avg_cdd_rcp45_21_40_raster, filename = "~/Desktop/extreme-events-project-WesternCape/data/rcp45/output/avg_cdd_rcp45_21_40.nc", format = "CDF", overwrite = TRUE)

#----------------------------WSDI--------------------------------------------
wsdi_rcp45_21_40 <- nc_open('wsdi_ANN_climpact.sample_historical_NA_2021-2040.nc')
lon <- ncvar_get(wsdi_rcp45_21_40, "lon")
lat <- ncvar_get(wsdi_rcp45_21_40, "lat", verbose = F)

wsdi_rcp45_21_40.array <- ncvar_get(wsdi_rcp45_21_40, "wsdi")

avg_wsdi_rcp45_21_40 <- apply(wsdi_rcp45_21_40.array, c(1,2), mean)
avg_wsdi_rcp45_21_40_raster <- raster(t(avg_wsdi_rcp45_21_40), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
avg_wsdi_rcp45_21_40_raster <- flip(avg_wsdi_rcp45_21_40_raster, direction = 'y')
plot(avg_wsdi_rcp45_21_40_raster, main = "Mean WSDI Index 2021/01-2040/12")
writeRaster(avg_wsdi_rcp45_21_40_raster, filename = "~/Desktop/extreme-events-project-WesternCape/data/rcp45/output/avg_wsdi_rcp45_21_40.nc", format = "CDF", overwrite = TRUE)

#----------------------------SPEI--------------------------------------------
spei_rcp45_21_40 <- nc_open('spei_MON_climpact.sample_historical_NA_2021-2040.nc')
lon <- ncvar_get(spei_rcp45_21_40, "lon")
lat <- ncvar_get(spei_rcp45_21_40, "lat", verbose = F)

spei_rcp45_21_40.array <- ncvar_get(spei_rcp45_21_40, "spei")

# scale = 3 month
avg_spei_rcp45_21_40_3month <- apply(spei_rcp45_21_40.array[,,,1], c(1,2), mean, na.rm = TRUE)
avg_spei_rcp45_21_40_3month_raster <- raster(t(avg_spei_rcp45_21_40_3month), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
avg_spei_rcp45_21_40_3month_raster <- flip(avg_spei_rcp45_21_40_3month_raster, direction = 'y')
plot(avg_spei_rcp45_21_40_3month_raster, main = "Mean SPEI 2021/01-2040/12 (scale = 3 month)")
writeRaster(avg_spei_rcp45_21_40_3month_raster, filename = "~/Desktop/extreme-events-project-WesternCape/data/rcp45/output/avg_spei_3mon_rcp45_21_40.nc", format = "CDF", overwrite = TRUE)

# scale = 6 month
avg_spei_rcp45_21_40_6month <- apply(spei_rcp45_21_40.array[,,,2], c(1,2), mean, na.rm = TRUE)
avg_spei_rcp45_21_40_6month_raster <- raster(t(avg_spei_rcp45_21_40_6month), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
avg_spei_rcp45_21_40_6month_raster <- flip(avg_spei_rcp45_21_40_6month_raster, direction = 'y')
plot(avg_spei_rcp45_21_40_6month_raster, main = "Mean SPEI 2021/01-2040/12 (scale = 6 month)")
writeRaster(avg_spei_rcp45_21_40_6month_raster, filename = "~/Desktop/extreme-events-project-WesternCape/data/rcp45/output/avg_spei_6mon_rcp45_21_40.nc", format = "CDF", overwrite = TRUE)

# scale = 12 month
avg_spei_rcp45_21_40_12month <- apply(spei_rcp45_21_40.array[,,,3], c(1,2), mean, na.rm = TRUE)
avg_spei_rcp45_21_40_12month_raster <- raster(t(avg_spei_rcp45_21_40_12month), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
avg_spei_rcp45_21_40_12month_raster <- flip(avg_spei_rcp45_21_40_12month_raster, direction = 'y')
plot(avg_spei_rcp45_21_40_12month_raster, main = "Mean SPEI 2021/01-2040/12 (scale = 12 month)")
writeRaster(avg_spei_rcp45_21_40_12month_raster, filename = "~/Desktop/extreme-events-project-WesternCape/data/rcp45/output/avg_spei_12mon_rcp45_21_40.nc", format = "CDF", overwrite = TRUE)

#------------------------------HW-----------------------------------------
hw_rcp45_21_40 <- nc_open('hw_ANN_climpact.sample_historical_NA_2021-2040.nc')
lon <- ncvar_get(hw_rcp45_21_40, "lon")
lat <- ncvar_get(hw_rcp45_21_40, "lat", verbose = F)

# HWF
hwf_rcp45_21_40.array <- ncvar_get(hw_rcp45_21_40, "hwf_tx90")

avg_hwf_rcp45_21_40 <- apply(hwf_rcp45_21_40.array, c(1,2), mean, na.rm = TRUE)
avg_hwf_rcp45_21_40_raster <- raster(t(avg_hwf_rcp45_21_40), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
avg_hwf_rcp45_21_40_raster <- flip(avg_hwf_rcp45_21_40_raster, direction = 'y')
plot(avg_hwf_rcp45_21_40_raster, main = "Mean HWF Index 2021/01-2040/12")
writeRaster(avg_hwf_rcp45_21_40_raster, filename = "~/Desktop/extreme-events-project-WesternCape/data/rcp45/output/avg_hwf_rcp45_21_40.nc", format = "CDF", overwrite = TRUE)

# HWD
hwd_rcp45_21_40.array <- ncvar_get(hw_rcp45_21_40, "hwd_tx90")

avg_hwd_rcp45_21_40 <- apply(hwd_rcp45_21_40.array, c(1,2), mean, na.rm = TRUE)
avg_hwd_rcp45_21_40_raster <- raster(t(avg_hwd_rcp45_21_40), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
avg_hwd_rcp45_21_40_raster <- flip(avg_hwd_rcp45_21_40_raster, direction = 'y')
plot(avg_hwd_rcp45_21_40_raster, main = "Mean HWD Index 2021/01-2040/12")
writeRaster(avg_hwd_rcp45_21_40_raster, filename = "~/Desktop/extreme-events-project-WesternCape/data/rcp45/output/avg_hwd_rcp45_21_40.nc", format = "CDF", overwrite = TRUE)

# HWM
hwm_rcp45_21_40.array <- ncvar_get(hw_rcp45_21_40, "hwm_tx90")

avg_hwm_rcp45_21_40 <- apply(hwm_rcp45_21_40.array, c(1,2), mean, na.rm = TRUE)
avg_hwm_rcp45_21_40_raster <- raster(t(avg_hwm_rcp45_21_40), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
avg_hwm_rcp45_21_40_raster <- flip(avg_hwm_rcp45_21_40_raster, direction = 'y')
plot(avg_hwm_rcp45_21_40_raster, main = "Mean HWM Index 2021/01-2040/12")
writeRaster(avg_hwm_rcp45_21_40_raster, filename = "~/Desktop/extreme-events-project-WesternCape/data/rcp45/output/avg_hwm_rcp45_21_40.nc", format = "CDF", overwrite = TRUE)

# HWA
hwa_rcp45_21_40.array <- ncvar_get(hw_rcp45_21_40, "hwa_tx90")

avg_hwa_rcp45_21_40 <- apply(hwa_rcp45_21_40.array, c(1,2), mean, na.rm = TRUE)
avg_hwa_rcp45_21_40_raster <- raster(t(avg_hwa_rcp45_21_40), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
avg_hwa_rcp45_21_40_raster <- flip(avg_hwa_rcp45_21_40_raster, direction = 'y')
plot(avg_hwa_rcp45_21_40_raster, main = "Mean HWA Index 2021/01-2040/12")
writeRaster(avg_hwa_rcp45_21_40_raster, filename = "~/Desktop/extreme-events-project-WesternCape/data/rcp45/output/avg_hwa_rcp45_21_40.nc", format = "CDF", overwrite = TRUE)

#HWN
hwn_rcp45_21_40.array <- ncvar_get(hw_rcp45_21_40, "hwn_tx90")

avg_hwn_rcp45_21_40 <- apply(hwn_rcp45_21_40.array, c(1,2), mean, na.rm = TRUE)
avg_hwn_rcp45_21_40_raster <- raster(t(avg_hwn_rcp45_21_40), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
avg_hwn_rcp45_21_40_raster <- flip(avg_hwn_rcp45_21_40_raster, direction = 'y')
plot(avg_hwn_rcp45_21_40_raster, main = "Mean HWN Index 2021/01-2040/12")
writeRaster(avg_hwn_rcp45_21_40_raster, filename = "~/Desktop/extreme-events-project-WesternCape/data/rcp45/output/avg_hwn_rcp45_21_40.nc", format = "CDF", overwrite = TRUE)
##--------------------------204001-206012----------------------------------------
###------------------------------CDD-----------------------------------------------
setwd("~/Desktop/extreme-events-project-WesternCape/data/rcp45/output/41-60")
cdd_rcp45_41_60 <- nc_open('cdd_ANN_climpact.sample_historical_NA_2041-2060.nc')
lon <- ncvar_get(cdd_rcp45_41_60, "lon")
lat <- ncvar_get(cdd_rcp45_41_60, "lat", verbose = F)

cdd_rcp45_41_60.array <- ncvar_get(cdd_rcp45_41_60, "cdd")

avg_cdd_rcp45_41_60 <- apply(cdd_rcp45_41_60.array, c(1,2), mean)
avg_cdd_rcp45_41_60_raster <- raster(t(avg_cdd_rcp45_41_60), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
avg_cdd_rcp45_41_60_raster <- flip(avg_cdd_rcp45_41_60_raster, direction = 'y')
plot(avg_cdd_rcp45_41_60_raster, main = "Mean CDD Index 2041/01-2060/12")
writeRaster(avg_cdd_rcp45_41_60_raster, filename = "~/Desktop/extreme-events-project-WesternCape/data/rcp45/output/avg_cdd_rcp45_41_60.nc", format = "CDF", overwrite = TRUE)

#----------------------------WSDI--------------------------------------------
wsdi_rcp45_41_60 <- nc_open('wsdi_ANN_climpact.sample_historical_NA_2041-2060.nc')
lon <- ncvar_get(wsdi_rcp45_41_60, "lon")
lat <- ncvar_get(wsdi_rcp45_41_60, "lat", verbose = F)

wsdi_rcp45_41_60.array <- ncvar_get(wsdi_rcp45_41_60, "wsdi")

avg_wsdi_rcp45_41_60 <- apply(wsdi_rcp45_41_60.array, c(1,2), mean)
avg_wsdi_rcp45_41_60_raster <- raster(t(avg_wsdi_rcp45_41_60), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
avg_wsdi_rcp45_41_60_raster <- flip(avg_wsdi_rcp45_41_60_raster, direction = 'y')
plot(avg_wsdi_rcp45_41_60_raster, main = "Mean WSDI Index 2041/01-2060/12")
writeRaster(avg_wsdi_rcp45_41_60_raster, filename = "~/Desktop/extreme-events-project-WesternCape/data/rcp45/output/avg_wsdi_rcp45_41_60.nc", format = "CDF", overwrite = TRUE)

#----------------------------SPEI--------------------------------------------
spei_rcp45_41_60 <- nc_open('spei_MON_climpact.sample_historical_NA_2041-2060.nc')
lon <- ncvar_get(spei_rcp45_41_60, "lon")
lat <- ncvar_get(spei_rcp45_41_60, "lat", verbose = F)

spei_rcp45_41_60.array <- ncvar_get(spei_rcp45_41_60, "spei")

# scale = 3 month
avg_spei_rcp45_41_60_3month <- apply(spei_rcp45_41_60.array[,,,1], c(1,2), mean, na.rm = TRUE)
avg_spei_rcp45_41_60_3month_raster <- raster(t(avg_spei_rcp45_41_60_3month), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
avg_spei_rcp45_41_60_3month_raster <- flip(avg_spei_rcp45_41_60_3month_raster, direction = 'y')
plot(avg_spei_rcp45_41_60_3month_raster, main = "Mean SPEI 2041/01-2060/12 (scale = 3 month)")
writeRaster(avg_spei_rcp45_41_60_3month_raster, filename = "~/Desktop/extreme-events-project-WesternCape/data/rcp45/output/avg_spei_3mon_rcp45_41_60.nc", format = "CDF", overwrite = TRUE)

# scale = 6 month
avg_spei_rcp45_41_60_6month <- apply(spei_rcp45_41_60.array[,,,2], c(1,2), mean, na.rm = TRUE)
avg_spei_rcp45_41_60_6month_raster <- raster(t(avg_spei_rcp45_41_60_6month), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
avg_spei_rcp45_41_60_6month_raster <- flip(avg_spei_rcp45_41_60_6month_raster, direction = 'y')
plot(avg_spei_rcp45_41_60_6month_raster, main = "Mean SPEI 2041/01-2060/12 (scale = 6 month)")
writeRaster(avg_spei_rcp45_41_60_6month_raster, filename = "~/Desktop/extreme-events-project-WesternCape/data/rcp45/output/avg_spei_6mon_rcp45_41_60.nc", format = "CDF", overwrite = TRUE)

# scale = 12 month
avg_spei_rcp45_41_60_12month <- apply(spei_rcp45_41_60.array[,,,3], c(1,2), mean, na.rm = TRUE)
avg_spei_rcp45_41_60_12month_raster <- raster(t(avg_spei_rcp45_41_60_12month), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
avg_spei_rcp45_41_60_12month_raster <- flip(avg_spei_rcp45_41_60_12month_raster, direction = 'y')
plot(avg_spei_rcp45_41_60_12month_raster, main = "Mean SPEI 2041/01-2060/12 (scale = 12 month)")
writeRaster(avg_spei_rcp45_41_60_12month_raster, filename = "~/Desktop/extreme-events-project-WesternCape/data/rcp45/output/avg_spei_12mon_rcp45_41_60.nc", format = "CDF", overwrite = TRUE)

#------------------------------HW-----------------------------------------
hw_rcp45_41_60 <- nc_open('hw_ANN_climpact.sample_historical_NA_2041-2060.nc')
lon <- ncvar_get(hw_rcp45_41_60, "lon")
lat <- ncvar_get(hw_rcp45_41_60, "lat", verbose = F)

# HWF
hwf_rcp45_41_60.array <- ncvar_get(hw_rcp45_41_60, "hwf_tx90")

avg_hwf_rcp45_41_60 <- apply(hwf_rcp45_41_60.array, c(1,2), mean, na.rm = TRUE)
avg_hwf_rcp45_41_60_raster <- raster(t(avg_hwf_rcp45_41_60), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
avg_hwf_rcp45_41_60_raster <- flip(avg_hwf_rcp45_41_60_raster, direction = 'y')
plot(avg_hwf_rcp45_41_60_raster, main = "Mean HWF Index 2041/01-2060/12")
writeRaster(avg_hwf_rcp45_41_60_raster, filename = "~/Desktop/extreme-events-project-WesternCape/data/rcp45/output/avg_hwf_rcp45_41_60.nc", format = "CDF", overwrite = TRUE)

# HWD
hwd_rcp45_41_60.array <- ncvar_get(hw_rcp45_41_60, "hwd_tx90")

avg_hwd_rcp45_41_60 <- apply(hwd_rcp45_41_60.array, c(1,2), mean, na.rm = TRUE)
avg_hwd_rcp45_41_60_raster <- raster(t(avg_hwd_rcp45_41_60), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
avg_hwd_rcp45_41_60_raster <- flip(avg_hwd_rcp45_41_60_raster, direction = 'y')
plot(avg_hwd_rcp45_41_60_raster, main = "Mean HWD Index 2041/01-2060/12")
writeRaster(avg_hwd_rcp45_41_60_raster, filename = "~/Desktop/extreme-events-project-WesternCape/data/rcp45/output/avg_hwd_rcp45_41_60.nc", format = "CDF", overwrite = TRUE)

# HWM
hwm_rcp45_41_60.array <- ncvar_get(hw_rcp45_41_60, "hwm_tx90")

avg_hwm_rcp45_41_60 <- apply(hwm_rcp45_41_60.array, c(1,2), mean, na.rm = TRUE)
avg_hwm_rcp45_41_60_raster <- raster(t(avg_hwm_rcp45_41_60), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
avg_hwm_rcp45_41_60_raster <- flip(avg_hwm_rcp45_41_60_raster, direction = 'y')
plot(avg_hwm_rcp45_41_60_raster, main = "Mean HWM Index 2041/01-2060/12")
writeRaster(avg_hwm_rcp45_41_60_raster, filename = "~/Desktop/extreme-events-project-WesternCape/data/rcp45/output/avg_hwm_rcp45_41_60.nc", format = "CDF", overwrite = TRUE)

# HWA
hwa_rcp45_41_60.array <- ncvar_get(hw_rcp45_41_60, "hwa_tx90")

avg_hwa_rcp45_41_60 <- apply(hwa_rcp45_41_60.array, c(1,2), mean, na.rm = TRUE)
avg_hwa_rcp45_41_60_raster <- raster(t(avg_hwa_rcp45_41_60), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
avg_hwa_rcp45_41_60_raster <- flip(avg_hwa_rcp45_41_60_raster, direction = 'y')
plot(avg_hwa_rcp45_41_60_raster, main = "Mean HWA Index 2041/01-2060/12")
writeRaster(avg_hwa_rcp45_41_60_raster, filename = "~/Desktop/extreme-events-project-WesternCape/data/rcp45/output/avg_hwa_rcp45_41_60.nc", format = "CDF", overwrite = TRUE)

# HWN
hwn_rcp45_41_60.array <- ncvar_get(hw_rcp45_41_60, "hwn_tx90")

avg_hwn_rcp45_41_60 <- apply(hwn_rcp45_41_60.array, c(1,2), mean, na.rm = TRUE)
avg_hwn_rcp45_41_60_raster <- raster(t(avg_hwn_rcp45_41_60), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
avg_hwn_rcp45_41_60_raster <- flip(avg_hwn_rcp45_41_60_raster, direction = 'y')
plot(avg_hwn_rcp45_41_60_raster, main = "Mean HWN Index 2041/01-2060/12")
writeRaster(avg_hwn_rcp45_41_60_raster, filename = "~/Desktop/extreme-events-project-WesternCape/data/rcp45/output/avg_hwn_rcp45_41_60.nc", format = "CDF", overwrite = TRUE)

##--------------------------206101-208012----------------------------------------
###------------------------------CDD-----------------------------------------------
setwd("~/Desktop/extreme-events-project-WesternCape/data/rcp45/output/61-80")
cdd_rcp45_61_80 <- nc_open('cdd_ANN_climpact.sample_historical_NA_2061-2080.nc')
lon <- ncvar_get(cdd_rcp45_61_80, "lon")
lat <- ncvar_get(cdd_rcp45_61_80, "lat", verbose = F)

cdd_rcp45_61_80.array <- ncvar_get(cdd_rcp45_61_80, "cdd")

avg_cdd_rcp45_61_80 <- apply(cdd_rcp45_61_80.array, c(1,2), mean)
avg_cdd_rcp45_61_80_raster <- raster(t(avg_cdd_rcp45_61_80), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
avg_cdd_rcp45_61_80_raster <- flip(avg_cdd_rcp45_61_80_raster, direction = 'y')
plot(avg_cdd_rcp45_61_80_raster, main = "Mean CDD Index 2061/01-2080/12")
writeRaster(avg_cdd_rcp45_61_80_raster, filename = "~/Desktop/extreme-events-project-WesternCape/data/rcp45/output/avg_cdd_rcp45_61_80.nc", format = "CDF", overwrite = TRUE)

#----------------------------WSDI--------------------------------------------
wsdi_rcp45_61_80 <- nc_open('wsdi_ANN_climpact.sample_historical_NA_2061-2080.nc')
lon <- ncvar_get(wsdi_rcp45_61_80, "lon")
lat <- ncvar_get(wsdi_rcp45_61_80, "lat", verbose = F)

wsdi_rcp45_61_80.array <- ncvar_get(wsdi_rcp45_61_80, "wsdi")

avg_wsdi_rcp45_61_80 <- apply(wsdi_rcp45_61_80.array, c(1,2), mean)
avg_wsdi_rcp45_61_80_raster <- raster(t(avg_wsdi_rcp45_61_80), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
avg_wsdi_rcp45_61_80_raster <- flip(avg_wsdi_rcp45_61_80_raster, direction = 'y')
plot(avg_wsdi_rcp45_61_80_raster, main = "Mean WSDI Index 2061/01-2080/12")
writeRaster(avg_wsdi_rcp45_61_80_raster, filename = "~/Desktop/extreme-events-project-WesternCape/data/rcp45/output/avg_wsdi_rcp45_61_80.nc", format = "CDF", overwrite = TRUE)

#----------------------------SPEI--------------------------------------------
spei_rcp45_61_80 <- nc_open('spei_MON_climpact.sample_historical_NA_2061-2080.nc')
lon <- ncvar_get(spei_rcp45_61_80, "lon")
lat <- ncvar_get(spei_rcp45_61_80, "lat", verbose = F)

spei_rcp45_61_80.array <- ncvar_get(spei_rcp45_61_80, "spei")

# scale = 3 month
avg_spei_rcp45_61_80_3month <- apply(spei_rcp45_61_80.array[,,,1], c(1,2), mean, na.rm = TRUE)
avg_spei_rcp45_61_80_3month_raster <- raster(t(avg_spei_rcp45_61_80_3month), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
avg_spei_rcp45_61_80_3month_raster <- flip(avg_spei_rcp45_61_80_3month_raster, direction = 'y')
plot(avg_spei_rcp45_61_80_3month_raster, main = "Mean SPEI 2061/01-2080/12 (scale = 3 month)")
writeRaster(avg_spei_rcp45_61_80_3month_raster, filename = "~/Desktop/extreme-events-project-WesternCape/data/rcp45/output/avg_spei_3mon_rcp45_61_80.nc", format = "CDF", overwrite = TRUE)

# scale = 6 month
avg_spei_rcp45_61_80_6month <- apply(spei_rcp45_61_80.array[,,,2], c(1,2), mean, na.rm = TRUE)
avg_spei_rcp45_61_80_6month_raster <- raster(t(avg_spei_rcp45_61_80_6month), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
avg_spei_rcp45_61_80_6month_raster <- flip(avg_spei_rcp45_61_80_6month_raster, direction = 'y')
plot(avg_spei_rcp45_61_80_6month_raster, main = "Mean SPEI 2061/01-2080/12 (scale = 6 month)")
writeRaster(avg_spei_rcp45_61_80_6month_raster, filename = "~/Desktop/extreme-events-project-WesternCape/data/rcp45/output/avg_spei_6mon_rcp45_61_80.nc", format = "CDF", overwrite = TRUE)

# scale = 12 month
avg_spei_rcp45_61_80_12month <- apply(spei_rcp45_61_80.array[,,,3], c(1,2), mean, na.rm = TRUE)
avg_spei_rcp45_61_80_12month_raster <- raster(t(avg_spei_rcp45_61_80_12month), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
avg_spei_rcp45_61_80_12month_raster <- flip(avg_spei_rcp45_61_80_12month_raster, direction = 'y')
plot(avg_spei_rcp45_61_80_12month_raster, main = "Mean SPEI 2061/01-2080/12 (scale = 12 month)")
writeRaster(avg_spei_rcp45_61_80_12month_raster, filename = "~/Desktop/extreme-events-project-WesternCape/data/rcp45/output/avg_spei_12mon_rcp45_61_80.nc", format = "CDF", overwrite = TRUE)

#------------------------------HW-----------------------------------------
hw_rcp45_61_80 <- nc_open('hw_ANN_climpact.sample_historical_NA_2061-2080.nc')
lon <- ncvar_get(hw_rcp45_61_80, "lon")
lat <- ncvar_get(hw_rcp45_61_80, "lat", verbose = F)

# HWF
hwf_rcp45_61_80.array <- ncvar_get(hw_rcp45_61_80, "hwf_tx90")

avg_hwf_rcp45_61_80 <- apply(hwf_rcp45_61_80.array, c(1,2), mean, na.rm = TRUE)
avg_hwf_rcp45_61_80_raster <- raster(t(avg_hwf_rcp45_61_80), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
avg_hwf_rcp45_61_80_raster <- flip(avg_hwf_rcp45_61_80_raster, direction = 'y')
plot(avg_hwf_rcp45_61_80_raster, main = "Mean HWF Index 2061/01-2080/12")
writeRaster(avg_hwf_rcp45_61_80_raster, filename = "~/Desktop/extreme-events-project-WesternCape/data/rcp45/output/avg_hwf_rcp45_61_80.nc", format = "CDF", overwrite = TRUE)

# HWD
hwd_rcp45_61_80.array <- ncvar_get(hw_rcp45_61_80, "hwd_tx90")

avg_hwd_rcp45_61_80 <- apply(hwd_rcp45_61_80.array, c(1,2), mean, na.rm = TRUE)
avg_hwd_rcp45_61_80_raster <- raster(t(avg_hwd_rcp45_61_80), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
avg_hwd_rcp45_61_80_raster <- flip(avg_hwd_rcp45_61_80_raster, direction = 'y')
plot(avg_hwd_rcp45_61_80_raster, main = "Mean HWD Index 2061/01-2080/12")
writeRaster(avg_hwd_rcp45_61_80_raster, filename = "~/Desktop/extreme-events-project-WesternCape/data/rcp45/output/avg_hwd_rcp45_61_80.nc", format = "CDF", overwrite = TRUE)

# HWM
hwm_rcp45_61_80.array <- ncvar_get(hw_rcp45_61_80, "hwm_tx90")

avg_hwm_rcp45_61_80 <- apply(hwm_rcp45_61_80.array, c(1,2), mean, na.rm = TRUE)
avg_hwm_rcp45_61_80_raster <- raster(t(avg_hwm_rcp45_61_80), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
avg_hwm_rcp45_61_80_raster <- flip(avg_hwm_rcp45_61_80_raster, direction = 'y')
plot(avg_hwm_rcp45_61_80_raster, main = "Mean HWM Index 2061/01-2080/12")
writeRaster(avg_hwm_rcp45_61_80_raster, filename = "~/Desktop/extreme-events-project-WesternCape/data/rcp45/output/avg_hwm_rcp45_61_80.nc", format = "CDF", overwrite = TRUE)

# HWA
hwa_rcp45_61_80.array <- ncvar_get(hw_rcp45_61_80, "hwa_tx90")

avg_hwa_rcp45_61_80 <- apply(hwa_rcp45_61_80.array, c(1,2), mean, na.rm = TRUE)
avg_hwa_rcp45_61_80_raster <- raster(t(avg_hwa_rcp45_61_80), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
avg_hwa_rcp45_61_80_raster <- flip(avg_hwa_rcp45_61_80_raster, direction = 'y')
plot(avg_hwa_rcp45_61_80_raster, main = "Mean HWA Index 2061/01-2080/12")
writeRaster(avg_hwa_rcp45_61_80_raster, filename = "~/Desktop/extreme-events-project-WesternCape/data/rcp45/output/avg_hwa_rcp45_61_80.nc", format = "CDF", overwrite = TRUE)

# HWN
hwn_rcp45_61_80.array <- ncvar_get(hw_rcp45_61_80, "hwn_tx90")

avg_hwn_rcp45_61_80 <- apply(hwn_rcp45_61_80.array, c(1,2), mean, na.rm = TRUE)
avg_hwn_rcp45_61_80_raster <- raster(t(avg_hwn_rcp45_61_80), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
avg_hwn_rcp45_61_80_raster <- flip(avg_hwn_rcp45_61_80_raster, direction = 'y')
plot(avg_hwn_rcp45_61_80_raster, main = "Mean HWN Index 2061/01-2080/12")
writeRaster(avg_hwn_rcp45_61_80_raster, filename = "~/Desktop/extreme-events-project-WesternCape/data/rcp45/output/avg_hwn_rcp45_61_80.nc", format = "CDF", overwrite = TRUE)

##--------------------------208101-210012------------------------------------------
###------------------------------CDD-----------------------------------------------
setwd("~/Desktop/extreme-events-project-WesternCape/data/rcp45/output/81-100")
cdd_rcp45_81_100 <- nc_open('cdd_ANN_climpact.sample_historical_NA_2081-2100.nc')
lon <- ncvar_get(cdd_rcp45_81_100, "lon")
lat <- ncvar_get(cdd_rcp45_81_100, "lat", verbose = F)

cdd_rcp45_81_100.array <- ncvar_get(cdd_rcp45_81_100, "cdd")

avg_cdd_rcp45_81_100 <- apply(cdd_rcp45_81_100.array, c(1,2), mean)
avg_cdd_rcp45_81_100_raster <- raster(t(avg_cdd_rcp45_81_100), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
avg_cdd_rcp45_81_100_raster <- flip(avg_cdd_rcp45_81_100_raster, direction = 'y')
plot(avg_cdd_rcp45_81_100_raster, main = "Mean CDD Index 2081/01-2100/12")
writeRaster(avg_cdd_rcp45_81_100_raster, filename = "~/Desktop/extreme-events-project-WesternCape/data/rcp45/output/avg_cdd_rcp45_81_100.nc", format = "CDF", overwrite = TRUE)

#----------------------------WSDI--------------------------------------------
wsdi_rcp45_81_100 <- nc_open('wsdi_ANN_climpact.sample_historical_NA_2081-2100.nc')
lon <- ncvar_get(wsdi_rcp45_81_100, "lon")
lat <- ncvar_get(wsdi_rcp45_81_100, "lat", verbose = F)

wsdi_rcp45_81_100.array <- ncvar_get(wsdi_rcp45_81_100, "wsdi")

avg_wsdi_rcp45_81_100 <- apply(wsdi_rcp45_81_100.array, c(1,2), mean)
avg_wsdi_rcp45_81_100_raster <- raster(t(avg_wsdi_rcp45_81_100), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
avg_wsdi_rcp45_81_100_raster <- flip(avg_wsdi_rcp45_81_100_raster, direction = 'y')
plot(avg_wsdi_rcp45_81_100_raster, main = "Mean WSDI Index 2081/01-2100/12")
writeRaster(avg_wsdi_rcp45_81_100_raster, filename = "~/Desktop/extreme-events-project-WesternCape/data/rcp45/output/avg_wsdi_rcp45_81_100.nc", format = "CDF", overwrite = TRUE)

#----------------------------SPEI--------------------------------------------
spei_rcp45_81_100 <- nc_open('spei_MON_climpact.sample_historical_NA_2081-2100.nc')
lon <- ncvar_get(spei_rcp45_81_100, "lon")
lat <- ncvar_get(spei_rcp45_81_100, "lat", verbose = F)

spei_rcp45_81_100.array <- ncvar_get(spei_rcp45_81_100, "spei")

# scale = 3 month
avg_spei_rcp45_81_100_3month <- apply(spei_rcp45_81_100.array[,,,1], c(1,2), mean, na.rm = TRUE)
avg_spei_rcp45_81_100_3month_raster <- raster(t(avg_spei_rcp45_81_100_3month), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
avg_spei_rcp45_81_100_3month_raster <- flip(avg_spei_rcp45_81_100_3month_raster, direction = 'y')
plot(avg_spei_rcp45_81_100_3month_raster, main = "Mean SPEI 2081/01-2100/12 (scale = 3 month)")
writeRaster(avg_spei_rcp45_81_100_3month_raster, filename = "~/Desktop/extreme-events-project-WesternCape/data/rcp45/output/avg_spei_3mon_rcp45_81_100.nc", format = "CDF", overwrite = TRUE)

# scale = 6 month
avg_spei_rcp45_81_100_6month <- apply(spei_rcp45_81_100.array[,,,2], c(1,2), mean, na.rm = TRUE)
avg_spei_rcp45_81_100_6month_raster <- raster(t(avg_spei_rcp45_81_100_6month), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
avg_spei_rcp45_81_100_6month_raster <- flip(avg_spei_rcp45_81_100_6month_raster, direction = 'y')
plot(avg_spei_rcp45_81_100_6month_raster, main = "Mean SPEI 2081/01-2100/12 (scale = 6 month)")
writeRaster(avg_spei_rcp45_81_100_6month_raster, filename = "~/Desktop/extreme-events-project-WesternCape/data/rcp45/output/avg_spei_6mon_rcp45_81_100.nc", format = "CDF", overwrite = TRUE)

# scale = 12 month
avg_spei_rcp45_81_100_12month <- apply(spei_rcp45_81_100.array[,,,3], c(1,2), mean, na.rm = TRUE)
avg_spei_rcp45_81_100_12month_raster <- raster(t(avg_spei_rcp45_81_100_12month), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
avg_spei_rcp45_81_100_12month_raster <- flip(avg_spei_rcp45_81_100_12month_raster, direction = 'y')
plot(avg_spei_rcp45_81_100_12month_raster, main = "Mean SPEI 2081/01-2100/12 (scale = 12 month)")
writeRaster(avg_spei_rcp45_81_100_12month_raster, filename = "~/Desktop/extreme-events-project-WesternCape/data/rcp45/output/avg_spei_12mon_rcp45_81_100.nc", format = "CDF", overwrite = TRUE)

#------------------------------HW-----------------------------------------
hw_rcp45_81_100 <- nc_open('hw_ANN_climpact.sample_historical_NA_2081-2100.nc')
lon <- ncvar_get(hw_rcp45_81_100, "lon")
lat <- ncvar_get(hw_rcp45_81_100, "lat", verbose = F)

# HWF
hwf_rcp45_81_100.array <- ncvar_get(hw_rcp45_81_100, "hwf_tx90")

avg_hwf_rcp45_81_100 <- apply(hwf_rcp45_81_100.array, c(1,2), mean, na.rm = TRUE)
avg_hwf_rcp45_81_100_raster <- raster(t(avg_hwf_rcp45_81_100), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
avg_hwf_rcp45_81_100_raster <- flip(avg_hwf_rcp45_81_100_raster, direction = 'y')
plot(avg_hwf_rcp45_81_100_raster, main = "Mean HWF Index 2081/01-2100/12")
writeRaster(avg_hwf_rcp45_81_100_raster, filename = "~/Desktop/extreme-events-project-WesternCape/data/rcp45/output/avg_hwf_rcp45_81_100.nc", format = "CDF", overwrite = TRUE)

# HWD
hwd_rcp45_81_100.array <- ncvar_get(hw_rcp45_81_100, "hwd_tx90")

avg_hwd_rcp45_81_100 <- apply(hwd_rcp45_81_100.array, c(1,2), mean, na.rm = TRUE)
avg_hwd_rcp45_81_100_raster <- raster(t(avg_hwd_rcp45_81_100), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
avg_hwd_rcp45_81_100_raster <- flip(avg_hwd_rcp45_81_100_raster, direction = 'y')
plot(avg_hwd_rcp45_81_100_raster, main = "Mean HWD Index 2081/01-2100/12")
writeRaster(avg_hwd_rcp45_81_100_raster, filename = "~/Desktop/extreme-events-project-WesternCape/data/rcp45/output/avg_hwd_rcp45_81_100.nc", format = "CDF", overwrite = TRUE)

# HWM
hwm_rcp45_81_100.array <- ncvar_get(hw_rcp45_81_100, "hwm_tx90")

avg_hwm_rcp45_81_100 <- apply(hwm_rcp45_81_100.array, c(1,2), mean, na.rm = TRUE)
avg_hwm_rcp45_81_100_raster <- raster(t(avg_hwm_rcp45_81_100), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
avg_hwm_rcp45_81_100_raster <- flip(avg_hwm_rcp45_81_100_raster, direction = 'y')
plot(avg_hwm_rcp45_81_100_raster, main = "Mean HWM Index 2081/01-2100/12")
writeRaster(avg_hwm_rcp45_81_100_raster, filename = "~/Desktop/extreme-events-project-WesternCape/data/rcp45/output/avg_hwm_rcp45_81_100.nc", format = "CDF", overwrite = TRUE)

# HWA
hwa_rcp45_81_100.array <- ncvar_get(hw_rcp45_81_100, "hwa_tx90")

avg_hwa_rcp45_81_100 <- apply(hwa_rcp45_81_100.array, c(1,2), mean, na.rm = TRUE)
avg_hwa_rcp45_81_100_raster <- raster(t(avg_hwa_rcp45_81_100), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
avg_hwa_rcp45_81_100_raster <- flip(avg_hwa_rcp45_81_100_raster, direction = 'y')
plot(avg_hwa_rcp45_81_100_raster, main = "Mean HWA Index 2081/01-2100/12")
writeRaster(avg_hwa_rcp45_81_100_raster, filename = "~/Desktop/extreme-events-project-WesternCape/data/rcp45/output/avg_hwa_rcp45_81_100.nc", format = "CDF", overwrite = TRUE)

# HWN
hwn_rcp45_81_100.array <- ncvar_get(hw_rcp45_81_100, "hwn_tx90")

avg_hwn_rcp45_81_100 <- apply(hwn_rcp45_81_100.array, c(1,2), mean, na.rm = TRUE)
avg_hwn_rcp45_81_100_raster <- raster(t(avg_hwn_rcp45_81_100), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
avg_hwn_rcp45_81_100_raster <- flip(avg_hwn_rcp45_81_100_raster, direction = 'y')
plot(avg_hwn_rcp45_81_100_raster, main = "Mean HWN Index 2081/01-2100/12")
writeRaster(avg_hwn_rcp45_81_100_raster, filename = "~/Desktop/extreme-events-project-WesternCape/data/rcp45/output/avg_hwn_rcp45_81_100.nc", format = "CDF", overwrite = TRUE)

#--------------------------rcp8.5 scenario--------------------------------------------------------
##--------------------------202101-204012----------------------------------------
###------------------------------CDD-----------------------------------------------
setwd("~/Desktop/extreme-events-project-WesternCape/data/rcp85/output/21-40")
cdd_rcp85_21_40 <- nc_open('cdd_ANN_climpact.sample_historical_NA_2021-2040.nc')
lon <- ncvar_get(cdd_rcp85_21_40, "lon")
lat <- ncvar_get(cdd_rcp85_21_40, "lat", verbose = F)

cdd_rcp85_21_40.array <- ncvar_get(cdd_rcp85_21_40, "cdd")

avg_cdd_rcp85_21_40 <- apply(cdd_rcp85_21_40.array, c(1,2), mean)
avg_cdd_rcp85_21_40_raster <- raster(t(avg_cdd_rcp85_21_40), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
avg_cdd_rcp85_21_40_raster <- flip(avg_cdd_rcp85_21_40_raster, direction = 'y')
plot(avg_cdd_rcp85_21_40_raster, main = "Mean CDD Index 2021/01-2040/12")
writeRaster(avg_cdd_rcp85_21_40_raster, filename = "~/Desktop/extreme-events-project-WesternCape/data/rcp85/output/avg_cdd_rcp85_21_40.nc", format = "CDF", overwrite = TRUE)

#----------------------------WSDI--------------------------------------------
wsdi_rcp85_21_40 <- nc_open('wsdi_ANN_climpact.sample_historical_NA_2021-2040.nc')
lon <- ncvar_get(wsdi_rcp85_21_40, "lon")
lat <- ncvar_get(wsdi_rcp85_21_40, "lat", verbose = F)

wsdi_rcp85_21_40.array <- ncvar_get(wsdi_rcp85_21_40, "wsdi")

avg_wsdi_rcp85_21_40 <- apply(wsdi_rcp85_21_40.array, c(1,2), mean)
avg_wsdi_rcp85_21_40_raster <- raster(t(avg_wsdi_rcp85_21_40), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
avg_wsdi_rcp85_21_40_raster <- flip(avg_wsdi_rcp85_21_40_raster, direction = 'y')
plot(avg_wsdi_rcp85_21_40_raster, main = "Mean WSDI Index 2021/01-2040/12")
writeRaster(avg_wsdi_rcp85_21_40_raster, filename = "~/Desktop/extreme-events-project-WesternCape/data/rcp85/output/avg_wsdi_rcp85_21_40.nc", format = "CDF", overwrite = TRUE)

#----------------------------SPEI--------------------------------------------
spei_rcp85_21_40 <- nc_open('spei_MON_climpact.sample_historical_NA_2021-2040.nc')
lon <- ncvar_get(spei_rcp85_21_40, "lon")
lat <- ncvar_get(spei_rcp85_21_40, "lat", verbose = F)

spei_rcp85_21_40.array <- ncvar_get(spei_rcp85_21_40, "spei")

# scale = 3 month
avg_spei_rcp85_21_40_3month <- apply(spei_rcp85_21_40.array[,,,1], c(1,2), mean, na.rm = TRUE)
avg_spei_rcp85_21_40_3month_raster <- raster(t(avg_spei_rcp85_21_40_3month), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
avg_spei_rcp85_21_40_3month_raster <- flip(avg_spei_rcp85_21_40_3month_raster, direction = 'y')
plot(avg_spei_rcp85_21_40_3month_raster, main = "Mean SPEI 2021/01-2040/12 (scale = 3 month)")
writeRaster(avg_spei_rcp85_21_40_3month_raster, filename = "~/Desktop/extreme-events-project-WesternCape/data/rcp85/output/avg_spei_3mon_rcp85_21_40.nc", format = "CDF", overwrite = TRUE)


# scale = 6 month
avg_spei_rcp85_21_40_6month <- apply(spei_rcp85_21_40.array[,,,2], c(1,2), mean, na.rm = TRUE)
avg_spei_rcp85_21_40_6month_raster <- raster(t(avg_spei_rcp85_21_40_6month), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
avg_spei_rcp85_21_40_6month_raster <- flip(avg_spei_rcp85_21_40_6month_raster, direction = 'y')
plot(avg_spei_rcp85_21_40_6month_raster, main = "Mean SPEI 2021/01-2040/12 (scale = 6 month)")
writeRaster(avg_spei_rcp85_21_40_6month_raster, filename = "~/Desktop/extreme-events-project-WesternCape/data/rcp85/output/avg_spei_6mon_rcp85_21_40.nc", format = "CDF", overwrite = TRUE)

# scale = 12 month
avg_spei_rcp85_21_40_12month <- apply(spei_rcp85_21_40.array[,,,3], c(1,2), mean, na.rm = TRUE)
avg_spei_rcp85_21_40_12month_raster <- raster(t(avg_spei_rcp85_21_40_12month), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
avg_spei_rcp85_21_40_12month_raster <- flip(avg_spei_rcp85_21_40_12month_raster, direction = 'y')
plot(avg_spei_rcp85_21_40_12month_raster, main = "Mean SPEI 2021/01-2040/12 (scale = 12 month)")
writeRaster(avg_spei_rcp85_21_40_12month_raster, filename = "~/Desktop/extreme-events-project-WesternCape/data/rcp85/output/avg_spei_12mon_rcp85_21_40.nc", format = "CDF", overwrite = TRUE)

#------------------------------HW-----------------------------------------
hw_rcp85_21_40 <- nc_open('hw_ANN_climpact.sample_historical_NA_2021-2040.nc')
lon <- ncvar_get(hw_rcp85_21_40, "lon")
lat <- ncvar_get(hw_rcp85_21_40, "lat", verbose = F)

# HWF
hwf_rcp85_21_40.array <- ncvar_get(hw_rcp85_21_40, "hwf_tx90")

avg_hwf_rcp85_21_40 <- apply(hwf_rcp85_21_40.array, c(1,2), mean, na.rm = TRUE)
avg_hwf_rcp85_21_40_raster <- raster(t(avg_hwf_rcp85_21_40), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
avg_hwf_rcp85_21_40_raster <- flip(avg_hwf_rcp85_21_40_raster, direction = 'y')
plot(avg_hwf_rcp85_21_40_raster, main = "Mean HWF Index 2021/01-2040/12")
writeRaster(avg_hwf_rcp85_21_40_raster, filename = "~/Desktop/extreme-events-project-WesternCape/data/rcp85/output/avg_hwf_rcp85_21_40.nc", format = "CDF", overwrite = TRUE)

# HWD
hwd_rcp85_21_40.array <- ncvar_get(hw_rcp85_21_40, "hwd_tx90")

avg_hwd_rcp85_21_40 <- apply(hwd_rcp85_21_40.array, c(1,2), mean, na.rm = TRUE)
avg_hwd_rcp85_21_40_raster <- raster(t(avg_hwd_rcp85_21_40), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
avg_hwd_rcp85_21_40_raster <- flip(avg_hwd_rcp85_21_40_raster, direction = 'y')
plot(avg_hwd_rcp85_21_40_raster, main = "Mean HWD Index 2021/01-2040/12")
writeRaster(avg_hwd_rcp85_21_40_raster, filename = "~/Desktop/extreme-events-project-WesternCape/data/rcp85/output/avg_hwd_rcp85_21_40.nc", format = "CDF", overwrite = TRUE)

# HWM
hwm_rcp85_21_40.array <- ncvar_get(hw_rcp85_21_40, "hwm_tx90")

avg_hwm_rcp85_21_40 <- apply(hwm_rcp85_21_40.array, c(1,2), mean, na.rm = TRUE)
avg_hwm_rcp85_21_40_raster <- raster(t(avg_hwm_rcp85_21_40), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
avg_hwm_rcp85_21_40_raster <- flip(avg_hwm_rcp85_21_40_raster, direction = 'y')
plot(avg_hwm_rcp85_21_40_raster, main = "Mean HWM Index 2021/01-2040/12")
writeRaster(avg_hwm_rcp85_21_40_raster, filename = "~/Desktop/extreme-events-project-WesternCape/data/rcp85/output/avg_hwm_rcp85_21_40.nc", format = "CDF", overwrite = TRUE)

# HWA
hwa_rcp85_21_40.array <- ncvar_get(hw_rcp85_21_40, "hwa_tx90")

avg_hwa_rcp85_21_40 <- apply(hwa_rcp85_21_40.array, c(1,2), mean, na.rm = TRUE)
avg_hwa_rcp85_21_40_raster <- raster(t(avg_hwa_rcp85_21_40), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
avg_hwa_rcp85_21_40_raster <- flip(avg_hwa_rcp85_21_40_raster, direction = 'y')
plot(avg_hwa_rcp85_21_40_raster, main = "Mean HWA Index 2021/01-2040/12")
writeRaster(avg_hwa_rcp85_21_40_raster, filename = "~/Desktop/extreme-events-project-WesternCape/data/rcp85/output/avg_hwa_rcp85_21_40.nc", format = "CDF", overwrite = TRUE)

#HWN
hwn_rcp85_21_40.array <- ncvar_get(hw_rcp85_21_40, "hwn_tx90")

avg_hwn_rcp85_21_40 <- apply(hwn_rcp85_21_40.array, c(1,2), mean, na.rm = TRUE)
avg_hwn_rcp85_21_40_raster <- raster(t(avg_hwn_rcp85_21_40), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
avg_hwn_rcp85_21_40_raster <- flip(avg_hwn_rcp85_21_40_raster, direction = 'y')
plot(avg_hwn_rcp85_21_40_raster, main = "Mean HWN Index 2021/01-2040/12")
writeRaster(avg_hwn_rcp85_21_40_raster, filename = "~/Desktop/extreme-events-project-WesternCape/data/rcp85/output/avg_hwn_rcp85_21_40.nc", format = "CDF", overwrite = TRUE)

##--------------------------204101-206012----------------------------------------
###------------------------------CDD-----------------------------------------------
setwd("~/Desktop/extreme-events-project-WesternCape/data/rcp85/output/41-60")
cdd_rcp85_41_60 <- nc_open('cdd_ANN_climpact.sample_historical_NA_2041-2060.nc')
lon <- ncvar_get(cdd_rcp85_41_60, "lon")
lat <- ncvar_get(cdd_rcp85_41_60, "lat", verbose = F)

cdd_rcp85_41_60.array <- ncvar_get(cdd_rcp85_41_60, "cdd")

avg_cdd_rcp85_41_60 <- apply(cdd_rcp85_41_60.array, c(1,2), mean)
avg_cdd_rcp85_41_60_raster <- raster(t(avg_cdd_rcp85_41_60), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
avg_cdd_rcp85_41_60_raster <- flip(avg_cdd_rcp85_41_60_raster, direction = 'y')
plot(avg_cdd_rcp85_41_60_raster, main = "Mean CDD Index 2041/01-2060/12")
writeRaster(avg_cdd_rcp85_41_60_raster, filename = "~/Desktop/extreme-events-project-WesternCape/data/rcp85/output/avg_cdd_rcp85_41_60.nc", format = "CDF", overwrite = TRUE)

#----------------------------WSDI--------------------------------------------
wsdi_rcp85_41_60 <- nc_open('wsdi_ANN_climpact.sample_historical_NA_2041-2060.nc')
lon <- ncvar_get(wsdi_rcp85_41_60, "lon")
lat <- ncvar_get(wsdi_rcp85_41_60, "lat", verbose = F)

wsdi_rcp85_41_60.array <- ncvar_get(wsdi_rcp85_41_60, "wsdi")

avg_wsdi_rcp85_41_60 <- apply(wsdi_rcp85_41_60.array, c(1,2), mean)
avg_wsdi_rcp85_41_60_raster <- raster(t(avg_wsdi_rcp85_41_60), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
avg_wsdi_rcp85_41_60_raster <- flip(avg_wsdi_rcp85_41_60_raster, direction = 'y')
plot(avg_wsdi_rcp85_41_60_raster, main = "Mean WSDI Index 2041/01-2060/12")
writeRaster(avg_wsdi_rcp85_41_60_raster, filename = "~/Desktop/extreme-events-project-WesternCape/data/rcp85/output/avg_wsdi_rcp85_41_60.nc", format = "CDF", overwrite = TRUE)

#----------------------------SPEI--------------------------------------------
spei_rcp85_41_60 <- nc_open('spei_MON_climpact.sample_historical_NA_2041-2060.nc')
lon <- ncvar_get(spei_rcp85_41_60, "lon")
lat <- ncvar_get(spei_rcp85_41_60, "lat", verbose = F)

spei_rcp85_41_60.array <- ncvar_get(spei_rcp85_41_60, "spei")

# scale = 3 month
avg_spei_rcp85_41_60_3month <- apply(spei_rcp85_41_60.array[,,,1], c(1,2), mean, na.rm = TRUE)
avg_spei_rcp85_41_60_3month_raster <- raster(t(avg_spei_rcp85_41_60_3month), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
avg_spei_rcp85_41_60_3month_raster <- flip(avg_spei_rcp85_41_60_3month_raster, direction = 'y')
plot(avg_spei_rcp85_41_60_3month_raster, main = "Mean SPEI 2041/01-2060/12 (scale = 3 month)")
writeRaster(avg_spei_rcp85_41_60_3month_raster, filename = "~/Desktop/extreme-events-project-WesternCape/data/rcp85/output/avg_spei_3mon_rcp85_41_60.nc", format = "CDF", overwrite = TRUE)

# scale = 6 month
avg_spei_rcp85_41_60_6month <- apply(spei_rcp85_41_60.array[,,,2], c(1,2), mean, na.rm = TRUE)
avg_spei_rcp85_41_60_6month_raster <- raster(t(avg_spei_rcp85_41_60_6month), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
avg_spei_rcp85_41_60_6month_raster <- flip(avg_spei_rcp85_41_60_6month_raster, direction = 'y')
plot(avg_spei_rcp85_41_60_6month_raster, main = "Mean SPEI 2041/01-2060/12 (scale = 6 month)")
writeRaster(avg_spei_rcp85_41_60_6month_raster, filename = "~/Desktop/extreme-events-project-WesternCape/data/rcp85/output/avg_spei_6mon_rcp85_41_60.nc", format = "CDF", overwrite = TRUE)

# scale = 12 month
avg_spei_rcp85_41_60_12month <- apply(spei_rcp85_41_60.array[,,,3], c(1,2), mean, na.rm = TRUE)
avg_spei_rcp85_41_60_12month_raster <- raster(t(avg_spei_rcp85_41_60_12month), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
avg_spei_rcp85_41_60_12month_raster <- flip(avg_spei_rcp85_41_60_12month_raster, direction = 'y')
plot(avg_spei_rcp85_41_60_12month_raster, main = "Mean SPEI 2041/01-2060/12 (scale = 12 month)")
writeRaster(avg_spei_rcp85_41_60_12month_raster, filename = "~/Desktop/extreme-events-project-WesternCape/data/rcp85/output/avg_spei_12mon_rcp85_41_60.nc", format = "CDF", overwrite = TRUE)

#------------------------------HW-----------------------------------------
hw_rcp85_41_60 <- nc_open('hw_ANN_climpact.sample_historical_NA_2041-2060.nc')
lon <- ncvar_get(hw_rcp85_41_60, "lon")
lat <- ncvar_get(hw_rcp85_41_60, "lat", verbose = F)

# HWF
hwf_rcp85_41_60.array <- ncvar_get(hw_rcp85_41_60, "hwf_tx90")

avg_hwf_rcp85_41_60 <- apply(hwf_rcp85_41_60.array, c(1,2), mean, na.rm = TRUE)
avg_hwf_rcp85_41_60_raster <- raster(t(avg_hwf_rcp85_41_60), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
avg_hwf_rcp85_41_60_raster <- flip(avg_hwf_rcp85_41_60_raster, direction = 'y')
plot(avg_hwf_rcp85_41_60_raster, main = "Mean HWF Index 2041/01-2060/12")
writeRaster(avg_hwf_rcp85_41_60_raster, filename = "~/Desktop/extreme-events-project-WesternCape/data/rcp85/output/avg_hwf_rcp85_41_60.nc", format = "CDF", overwrite = TRUE)

# HWD
hwd_rcp85_41_60.array <- ncvar_get(hw_rcp85_41_60, "hwd_tx90")

avg_hwd_rcp85_41_60 <- apply(hwd_rcp85_41_60.array, c(1,2), mean, na.rm = TRUE)
avg_hwd_rcp85_41_60_raster <- raster(t(avg_hwd_rcp85_41_60), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
avg_hwd_rcp85_41_60_raster <- flip(avg_hwd_rcp85_41_60_raster, direction = 'y')
plot(avg_hwd_rcp85_41_60_raster, main = "Mean HWD Index 2041/01-2060/12")
writeRaster(avg_hwd_rcp85_41_60_raster, filename = "~/Desktop/extreme-events-project-WesternCape/data/rcp85/output/avg_hwd_rcp85_41_60.nc", format = "CDF", overwrite = TRUE)

# HWM
hwm_rcp85_41_60.array <- ncvar_get(hw_rcp85_41_60, "hwm_tx90")

avg_hwm_rcp85_41_60 <- apply(hwm_rcp85_41_60.array, c(1,2), mean, na.rm = TRUE)
avg_hwm_rcp85_41_60_raster <- raster(t(avg_hwm_rcp85_41_60), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
avg_hwm_rcp85_41_60_raster <- flip(avg_hwm_rcp85_41_60_raster, direction = 'y')
plot(avg_hwm_rcp85_41_60_raster, main = "Mean HWM Index 2041/01-2060/12")
writeRaster(avg_hwm_rcp85_41_60_raster, filename = "~/Desktop/extreme-events-project-WesternCape/data/rcp85/output/avg_hwm_rcp85_41_60.nc", format = "CDF", overwrite = TRUE)

# HWA
hwa_rcp85_41_60.array <- ncvar_get(hw_rcp85_41_60, "hwa_tx90")

avg_hwa_rcp85_41_60 <- apply(hwa_rcp85_41_60.array, c(1,2), mean, na.rm = TRUE)
avg_hwa_rcp85_41_60_raster <- raster(t(avg_hwa_rcp85_41_60), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
avg_hwa_rcp85_41_60_raster <- flip(avg_hwa_rcp85_41_60_raster, direction = 'y')
plot(avg_hwa_rcp85_41_60_raster, main = "Mean HWA Index 2041/01-2060/12")
writeRaster(avg_hwa_rcp85_41_60_raster, filename = "~/Desktop/extreme-events-project-WesternCape/data/rcp85/output/avg_hwa_rcp85_41_60.nc", format = "CDF", overwrite = TRUE)

#HWN
hwn_rcp85_41_60.array <- ncvar_get(hw_rcp85_41_60, "hwn_tx90")

avg_hwn_rcp85_41_60 <- apply(hwn_rcp85_41_60.array, c(1,2), mean, na.rm = TRUE)
avg_hwn_rcp85_41_60_raster <- raster(t(avg_hwn_rcp85_41_60), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
avg_hwn_rcp85_41_60_raster <- flip(avg_hwn_rcp85_41_60_raster, direction = 'y')
plot(avg_hwn_rcp85_41_60_raster, main = "Mean HWN Index 2041/01-2060/12")
writeRaster(avg_hwn_rcp85_41_60_raster, filename = "~/Desktop/extreme-events-project-WesternCape/data/rcp85/output/avg_hwn_rcp85_41_60.nc", format = "CDF", overwrite = TRUE)
##--------------------------206101-208012----------------------------------------
###------------------------------CDD-----------------------------------------------
setwd("~/Desktop/extreme-events-project-WesternCape/data/rcp85/output/61-80")
cdd_rcp85_61_80 <- nc_open('cdd_ANN_climpact.sample_historical_NA_2061-2080.nc')
lon <- ncvar_get(cdd_rcp85_61_80, "lon")
lat <- ncvar_get(cdd_rcp85_61_80, "lat", verbose = F)

cdd_rcp85_61_80.array <- ncvar_get(cdd_rcp85_61_80, "cdd")

avg_cdd_rcp85_61_80 <- apply(cdd_rcp85_61_80.array, c(1,2), mean)
avg_cdd_rcp85_61_80_raster <- raster(t(avg_cdd_rcp85_61_80), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
avg_cdd_rcp85_61_80_raster <- flip(avg_cdd_rcp85_61_80_raster, direction = 'y')
plot(avg_cdd_rcp85_61_80_raster, main = "Mean CDD Index 2061/01-2080/12")
writeRaster(avg_cdd_rcp85_61_80_raster, filename = "~/Desktop/extreme-events-project-WesternCape/data/rcp85/output/avg_cdd_rcp85_61_80.nc", format = "CDF", overwrite = TRUE)

#----------------------------WSDI--------------------------------------------
wsdi_rcp85_61_80 <- nc_open('wsdi_ANN_climpact.sample_historical_NA_2061-2080.nc')
lon <- ncvar_get(wsdi_rcp85_61_80, "lon")
lat <- ncvar_get(wsdi_rcp85_61_80, "lat", verbose = F)

wsdi_rcp85_61_80.array <- ncvar_get(wsdi_rcp85_61_80, "wsdi")

avg_wsdi_rcp85_61_80 <- apply(wsdi_rcp85_61_80.array, c(1,2), mean)
avg_wsdi_rcp85_61_80_raster <- raster(t(avg_wsdi_rcp85_61_80), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
avg_wsdi_rcp85_61_80_raster <- flip(avg_wsdi_rcp85_61_80_raster, direction = 'y')
plot(avg_wsdi_rcp85_61_80_raster, main = "Mean WSDI Index 2061/01-2080/12")
writeRaster(avg_wsdi_rcp85_61_80_raster, filename = "~/Desktop/extreme-events-project-WesternCape/data/rcp85/output/avg_wsdi_rcp85_61_80.nc", format = "CDF", overwrite = TRUE)

#----------------------------SPEI--------------------------------------------
spei_rcp85_61_80 <- nc_open('spei_MON_climpact.sample_historical_NA_2061-2080.nc')
lon <- ncvar_get(spei_rcp85_61_80, "lon")
lat <- ncvar_get(spei_rcp85_61_80, "lat", verbose = F)

spei_rcp85_61_80.array <- ncvar_get(spei_rcp85_61_80, "spei")

# scale = 3 month
avg_spei_rcp85_61_80_3month <- apply(spei_rcp85_61_80.array[,,,1], c(1,2), mean, na.rm = TRUE)
avg_spei_rcp85_61_80_3month_raster <- raster(t(avg_spei_rcp85_61_80_3month), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
avg_spei_rcp85_61_80_3month_raster <- flip(avg_spei_rcp85_61_80_3month_raster, direction = 'y')
plot(avg_spei_rcp85_61_80_3month_raster, main = "Mean SPEI 2061/01-2080/12 (scale = 3 month)")
writeRaster(avg_spei_rcp85_61_80_3month_raster, filename = "~/Desktop/extreme-events-project-WesternCape/data/rcp85/output/avg_spei_3mon_rcp85_61_80.nc", format = "CDF", overwrite = TRUE)

# scale = 6 month
avg_spei_rcp85_61_80_6month <- apply(spei_rcp85_61_80.array[,,,2], c(1,2), mean, na.rm = TRUE)
avg_spei_rcp85_61_80_6month_raster <- raster(t(avg_spei_rcp85_61_80_6month), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
avg_spei_rcp85_61_80_6month_raster <- flip(avg_spei_rcp85_61_80_6month_raster, direction = 'y')
plot(avg_spei_rcp85_61_80_6month_raster, main = "Mean SPEI 2061/01-2080/12 (scale = 6 month)")
writeRaster(avg_spei_rcp85_61_80_6month_raster, filename = "~/Desktop/extreme-events-project-WesternCape/data/rcp85/output/avg_spei_6mon_rcp85_61_80.nc", format = "CDF", overwrite = TRUE)

# scale = 12 month
avg_spei_rcp85_61_80_12month <- apply(spei_rcp85_61_80.array[,,,3], c(1,2), mean, na.rm = TRUE)
avg_spei_rcp85_61_80_12month_raster <- raster(t(avg_spei_rcp85_61_80_12month), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
avg_spei_rcp85_61_80_12month_raster <- flip(avg_spei_rcp85_61_80_12month_raster, direction = 'y')
plot(avg_spei_rcp85_61_80_12month_raster, main = "Mean SPEI 2061/01-2080/12 (scale = 12 month)")
writeRaster(avg_spei_rcp85_61_80_12month_raster, filename = "~/Desktop/extreme-events-project-WesternCape/data/rcp85/output/avg_spei_12mon_rcp85_61_80.nc", format = "CDF", overwrite = TRUE)

#------------------------------HW-----------------------------------------
hw_rcp85_61_80 <- nc_open('hw_ANN_climpact.sample_historical_NA_2061-2080.nc')
lon <- ncvar_get(hw_rcp85_61_80, "lon")
lat <- ncvar_get(hw_rcp85_61_80, "lat", verbose = F)

# HWF
hwf_rcp85_61_80.array <- ncvar_get(hw_rcp85_61_80, "hwf_tx90")

avg_hwf_rcp85_61_80 <- apply(hwf_rcp85_61_80.array, c(1,2), mean, na.rm = TRUE)
avg_hwf_rcp85_61_80_raster <- raster(t(avg_hwf_rcp85_61_80), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
avg_hwf_rcp85_61_80_raster <- flip(avg_hwf_rcp85_61_80_raster, direction = 'y')
plot(avg_hwf_rcp85_61_80_raster, main = "Mean HWF Index 2061/01-2080/12")
writeRaster(avg_hwf_rcp85_61_80_raster, filename = "~/Desktop/extreme-events-project-WesternCape/data/rcp85/output/avg_hwf_rcp85_61_80.nc", format = "CDF", overwrite = TRUE)

# HWD
hwd_rcp85_61_80.array <- ncvar_get(hw_rcp85_61_80, "hwd_tx90")

avg_hwd_rcp85_61_80 <- apply(hwd_rcp85_61_80.array, c(1,2), mean, na.rm = TRUE)
avg_hwd_rcp85_61_80_raster <- raster(t(avg_hwd_rcp85_61_80), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
avg_hwd_rcp85_61_80_raster <- flip(avg_hwd_rcp85_61_80_raster, direction = 'y')
plot(avg_hwd_rcp85_61_80_raster, main = "Mean HWD Index 2061/01-2080/12")
writeRaster(avg_hwd_rcp85_61_80_raster, filename = "~/Desktop/extreme-events-project-WesternCape/data/rcp85/output/avg_hwd_rcp85_61_80.nc", format = "CDF", overwrite = TRUE)

# HWM
hwm_rcp85_61_80.array <- ncvar_get(hw_rcp85_61_80, "hwm_tx90")

avg_hwm_rcp85_61_80 <- apply(hwm_rcp85_61_80.array, c(1,2), mean, na.rm = TRUE)
avg_hwm_rcp85_61_80_raster <- raster(t(avg_hwm_rcp85_61_80), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
avg_hwm_rcp85_61_80_raster <- flip(avg_hwm_rcp85_61_80_raster, direction = 'y')
plot(avg_hwm_rcp85_61_80_raster, main = "Mean HWM Index 2061/01-2080/12")
writeRaster(avg_hwm_rcp85_61_80_raster, filename = "~/Desktop/extreme-events-project-WesternCape/data/rcp85/output/avg_hwm_rcp85_61_80.nc", format = "CDF", overwrite = TRUE)

# HWA
hwa_rcp85_61_80.array <- ncvar_get(hw_rcp85_61_80, "hwa_tx90")

avg_hwa_rcp85_61_80 <- apply(hwa_rcp85_61_80.array, c(1,2), mean, na.rm = TRUE)
avg_hwa_rcp85_61_80_raster <- raster(t(avg_hwa_rcp85_61_80), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
avg_hwa_rcp85_61_80_raster <- flip(avg_hwa_rcp85_61_80_raster, direction = 'y')
plot(avg_hwa_rcp85_61_80_raster, main = "Mean HWA Index 2061/01-2080/12")
writeRaster(avg_hwa_rcp85_61_80_raster, filename = "~/Desktop/extreme-events-project-WesternCape/data/rcp85/output/avg_hwa_rcp85_61_80.nc", format = "CDF", overwrite = TRUE)

# HWN
hwn_rcp85_61_80.array <- ncvar_get(hw_rcp85_61_80, "hwn_tx90")

avg_hwn_rcp85_61_80 <- apply(hwn_rcp85_61_80.array, c(1,2), mean, na.rm = TRUE)
avg_hwn_rcp85_61_80_raster <- raster(t(avg_hwn_rcp85_61_80), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
avg_hwn_rcp85_61_80_raster <- flip(avg_hwn_rcp85_61_80_raster, direction = 'y')
plot(avg_hwn_rcp85_61_80_raster, main = "Mean HWN Index 2061/01-2080/12")
writeRaster(avg_hwn_rcp85_61_80_raster, filename = "~/Desktop/extreme-events-project-WesternCape/data/rcp85/output/avg_hwn_rcp85_61_80.nc", format = "CDF", overwrite = TRUE)
##--------------------------208101-210012------------------------------------------
###------------------------------CDD-----------------------------------------------
setwd("~/Desktop/extreme-events-project-WesternCape/data/rcp85/output/81-100")
cdd_rcp85_81_100 <- nc_open('cdd_ANN_climpact.sample_historical_NA_2081-2100.nc')
lon <- ncvar_get(cdd_rcp85_81_100, "lon")
lat <- ncvar_get(cdd_rcp85_81_100, "lat", verbose = F)

cdd_rcp85_81_100.array <- ncvar_get(cdd_rcp85_81_100, "cdd")

avg_cdd_rcp85_81_100 <- apply(cdd_rcp85_81_100.array, c(1,2), mean)
avg_cdd_rcp85_81_100_raster <- raster(t(avg_cdd_rcp85_81_100), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
avg_cdd_rcp85_81_100_raster <- flip(avg_cdd_rcp85_81_100_raster, direction = 'y')
plot(avg_cdd_rcp85_81_100_raster, main = "Mean CDD Index 2081/01-2100/12")
writeRaster(avg_cdd_rcp85_81_100_raster, filename = "~/Desktop/extreme-events-project-WesternCape/data/rcp85/output/avg_cdd_rcp85_81_100.nc", format = "CDF", overwrite = TRUE)

#----------------------------WSDI--------------------------------------------
wsdi_rcp85_81_100 <- nc_open('wsdi_ANN_climpact.sample_historical_NA_2081-2100.nc')
lon <- ncvar_get(wsdi_rcp85_81_100, "lon")
lat <- ncvar_get(wsdi_rcp85_81_100, "lat", verbose = F)

wsdi_rcp85_81_100.array <- ncvar_get(wsdi_rcp85_81_100, "wsdi")

avg_wsdi_rcp85_81_100 <- apply(wsdi_rcp85_81_100.array, c(1,2), mean)
avg_wsdi_rcp85_81_100_raster <- raster(t(avg_wsdi_rcp85_81_100), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
avg_wsdi_rcp85_81_100_raster <- flip(avg_wsdi_rcp85_81_100_raster, direction = 'y')
plot(avg_wsdi_rcp85_81_100_raster, main = "Mean WSDI Index 2081/01-2100/12")
writeRaster(avg_wsdi_rcp85_81_100_raster, filename = "~/Desktop/extreme-events-project-WesternCape/data/rcp85/output/avg_wsdi_rcp85_81_100.nc", format = "CDF", overwrite = TRUE)

#----------------------------SPEI--------------------------------------------
spei_rcp85_81_100 <- nc_open('spei_MON_climpact.sample_historical_NA_2081-2100.nc')
lon <- ncvar_get(spei_rcp85_81_100, "lon")
lat <- ncvar_get(spei_rcp85_81_100, "lat", verbose = F)

spei_rcp85_81_100.array <- ncvar_get(spei_rcp85_81_100, "spei")

# scale = 3 month
avg_spei_rcp85_81_100_3month <- apply(spei_rcp85_81_100.array[,,,1], c(1,2), mean, na.rm = TRUE)
avg_spei_rcp85_81_100_3month_raster <- raster(t(avg_spei_rcp85_81_100_3month), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
avg_spei_rcp85_81_100_3month_raster <- flip(avg_spei_rcp85_81_100_3month_raster, direction = 'y')
plot(avg_spei_rcp85_81_100_3month_raster, main = "Mean SPEI 2081/01-2100/12 (scale = 3 month)")
writeRaster(avg_spei_rcp85_81_100_3month_raster, filename = "~/Desktop/extreme-events-project-WesternCape/data/rcp85/output/avg_spei_3mon_rcp85_81_100.nc", format = "CDF", overwrite = TRUE)

# scale = 6 month
avg_spei_rcp85_81_100_6month <- apply(spei_rcp85_81_100.array[,,,2], c(1,2), mean, na.rm = TRUE)
avg_spei_rcp85_81_100_6month_raster <- raster(t(avg_spei_rcp85_81_100_6month), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
avg_spei_rcp85_81_100_6month_raster <- flip(avg_spei_rcp85_81_100_6month_raster, direction = 'y')
plot(avg_spei_rcp85_81_100_6month_raster, main = "Mean SPEI 2081/01-2100/12 (scale = 6 month)")
writeRaster(avg_spei_rcp85_81_100_6month_raster, filename = "~/Desktop/extreme-events-project-WesternCape/data/rcp85/output/avg_spei_6mon_rcp85_81_100.nc", format = "CDF", overwrite = TRUE)

# scale = 12 month
avg_spei_rcp85_81_100_12month <- apply(spei_rcp85_81_100.array[,,,3], c(1,2), mean, na.rm = TRUE)
avg_spei_rcp85_81_100_12month_raster <- raster(t(avg_spei_rcp85_81_100_12month), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
avg_spei_rcp85_81_100_12month_raster <- flip(avg_spei_rcp85_81_100_12month_raster, direction = 'y')
plot(avg_spei_rcp85_81_100_12month_raster, main = "Mean SPEI 2081/01-2100/12 (scale = 12 month)")
writeRaster(avg_spei_rcp85_81_100_12month_raster, filename = "~/Desktop/extreme-events-project-WesternCape/data/rcp85/output/avg_spei_12mon_rcp85_81_100.nc", format = "CDF", overwrite = TRUE)

#------------------------------HW-----------------------------------------
hw_rcp85_81_100 <- nc_open('hw_ANN_climpact.sample_historical_NA_2081-2100.nc')
lon <- ncvar_get(hw_rcp85_81_100, "lon")
lat <- ncvar_get(hw_rcp85_81_100, "lat", verbose = F)

# HWF
hwf_rcp85_81_100.array <- ncvar_get(hw_rcp85_81_100, "hwf_tx90")

avg_hwf_rcp85_81_100 <- apply(hwf_rcp85_81_100.array, c(1,2), mean, na.rm = TRUE)
avg_hwf_rcp85_81_100_raster <- raster(t(avg_hwf_rcp85_81_100), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
avg_hwf_rcp85_81_100_raster <- flip(avg_hwf_rcp85_81_100_raster, direction = 'y')
plot(avg_hwf_rcp85_81_100_raster, main = "Mean HWF Index 2081/01-2100/12")
writeRaster(avg_hwf_rcp85_81_100_raster, filename = "~/Desktop/extreme-events-project-WesternCape/data/rcp85/output/avg_hwf_rcp85_81_100.nc", format = "CDF", overwrite = TRUE)

# HWD
hwd_rcp85_81_100.array <- ncvar_get(hw_rcp85_81_100, "hwd_tx90")

avg_hwd_rcp85_81_100 <- apply(hwd_rcp85_81_100.array, c(1,2), mean, na.rm = TRUE)
avg_hwd_rcp85_81_100_raster <- raster(t(avg_hwd_rcp85_81_100), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
avg_hwd_rcp85_81_100_raster <- flip(avg_hwd_rcp85_81_100_raster, direction = 'y')
plot(avg_hwd_rcp85_81_100_raster, main = "Mean HWD Index 2081/01-2100/12")
writeRaster(avg_hwd_rcp85_81_100_raster, filename = "~/Desktop/extreme-events-project-WesternCape/data/rcp85/output/avg_hwd_rcp85_81_100.nc", format = "CDF", overwrite = TRUE)

# HWM
hwm_rcp85_81_100.array <- ncvar_get(hw_rcp85_81_100, "hwm_tx90")

avg_hwm_rcp85_81_100 <- apply(hwm_rcp85_81_100.array, c(1,2), mean, na.rm = TRUE)
avg_hwm_rcp85_81_100_raster <- raster(t(avg_hwm_rcp85_81_100), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
avg_hwm_rcp85_81_100_raster <- flip(avg_hwm_rcp85_81_100_raster, direction = 'y')
plot(avg_hwm_rcp85_81_100_raster, main = "Mean HWM Index 2081/01-2100/12")
writeRaster(avg_hwm_rcp85_81_100_raster, filename = "~/Desktop/extreme-events-project-WesternCape/data/rcp85/output/avg_hwm_rcp85_81_100.nc", format = "CDF", overwrite = TRUE)

# HWA
hwa_rcp85_81_100.array <- ncvar_get(hw_rcp85_81_100, "hwa_tx90")

avg_hwa_rcp85_81_100 <- apply(hwa_rcp85_81_100.array, c(1,2), mean, na.rm = TRUE)
avg_hwa_rcp85_81_100_raster <- raster(t(avg_hwa_rcp85_81_100), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
avg_hwa_rcp85_81_100_raster <- flip(avg_hwa_rcp85_81_100_raster, direction = 'y')
plot(avg_hwa_rcp85_81_100_raster, main = "Mean HWA Index 2081/01-2100/12")
writeRaster(avg_hwa_rcp85_81_100_raster, filename = "~/Desktop/extreme-events-project-WesternCape/data/rcp85/output/avg_hwa_rcp85_81_100.nc", format = "CDF", overwrite = TRUE)

# HWN
hwn_rcp85_81_100.array <- ncvar_get(hw_rcp85_81_100, "hwn_tx90")

avg_hwn_rcp85_81_100 <- apply(hwn_rcp85_81_100.array, c(1,2), mean, na.rm = TRUE)
avg_hwn_rcp85_81_100_raster <- raster(t(avg_hwn_rcp85_81_100), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
avg_hwn_rcp85_81_100_raster <- flip(avg_hwn_rcp85_81_100_raster, direction = 'y')
plot(avg_hwn_rcp85_81_100_raster, main = "Mean HWN Index 2081/01-2100/12")
writeRaster(avg_hwn_rcp85_81_100_raster, filename = "~/Desktop/extreme-events-project-WesternCape/data/rcp85/output/avg_hwn_rcp85_81_100.nc", format = "CDF", overwrite = TRUE)





