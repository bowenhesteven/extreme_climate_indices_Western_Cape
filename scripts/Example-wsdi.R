example = nc_open("climpact.sampledata.gridded.1991-2010.nc")
example_wsdi = nc_open("wsdi_ANN_climpact.sample_historical_NA_1991-2010.nc")
example_wsdi.array <- ncvar_get(example_wsdi, "wsdi")
avg_example_wsdi<-apply(example_wsdi.array, c(1,2), mean)
example_wsdi_raster <- raster(t(avg_example_wsdi), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
example_wsdi_raster <- flip(example_wsdi_raster, direction = 'y')
plot(example_wsdi_raster)

# Combine the netcdf files along time dimension
cropped_example = nc_open("cropped_example.nc")



lon <- ncvar_get(cropped_example, "lon")
lat <- ncvar_get(cropped_example, "lat", verbose = F)
cropped_example.array <- ncvar_get(cropped_example, "pr")
avg_cropped_example_raster<-apply(cropped_example.array, c(1,2), mean)
cropped_example_raster <- raster(t(avg_cropped_example_raster), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))

cropped_example_raster <- flip(cropped_example_raster, direction = 'y')
plot(cropped_example_raster, main = "Mean Precipitation during 2006/01-2010/12")


cropped_pr_rcp26_210101_251231 <- nc_open('cropped_pr_26_210101-251231.nc')
lon <- ncvar_get(cropped_pr_rcp26_210101_251231, "lon")
lat <- ncvar_get(cropped_pr_rcp26_210101_251231, "lat", verbose = F)
cropped_pr_rcp26_210101_251231.array <- ncvar_get(cropped_pr_rcp26_210101_251231, "pr")
avg_cropped_pr_rcp26_210101_251231<-apply(cropped_pr_rcp26_210101_251231.array, c(1,2), mean)
avg_cropped_pr_rcp26_210101_251231_raster <- raster(t(avg_cropped_pr_rcp26_210101_251231), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
avg_cropped_pr_rcp26_210101_251231_raster <- flip(avg_cropped_pr_rcp26_210101_251231_raster, direction = 'y')
plot(avg_cropped_pr_rcp26_210101_251231_raster)