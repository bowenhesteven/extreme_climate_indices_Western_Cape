library(ncdf4)
library(raster)
library(rgdal)
library(ggplot2)
library(tidyverse)
library(climdex.pcic)
library(doParallel)
library(abind)

# setup working directory
setwd("~/Desktop/extreme-events-project-WesternCape/data")

### -------------------------Precipitation-------------------------------------------------------###
##-------------------------rcp2.6 scenario--------------------------------------------------------
#--------------------------20210101-20251231------------------------------------------------------
setwd("~/Desktop/extreme-events-project-WesternCape/data/rcp26/pr")
pr_rcp26_21_25 = nc_open("pr_AFR-44_MPI-M-MPI-ESM-LR_rcp26_r1i1p1_MPI-CSC-REMO2009_v1_day_20210101-20251231.nc")
lon <- ncvar_get(pr_rcp26_21_25, "lon")
lat <- ncvar_get(pr_rcp26_21_25, "lat", verbose = F)
pr_rcp26_21_25.array <- ncvar_get(pr_rcp26_21_25, "pr") 

#------------------------------20260101-20301231----------------------------------------------------
pr_rcp26_26_30 = nc_open("pr_AFR-44_MPI-M-MPI-ESM-LR_rcp26_r1i1p1_MPI-CSC-REMO2009_v1_day_20260101-20301231.nc")
pr_rcp26_26_30.array <- ncvar_get(pr_rcp26_26_30, "pr")

#------------------------------20310101-20351231----------------------------------------------------
pr_rcp26_31_35 = nc_open("pr_AFR-44_MPI-M-MPI-ESM-LR_rcp26_r1i1p1_MPI-CSC-REMO2009_v1_day_20310101-20351231.nc")
pr_rcp26_31_35.array <- ncvar_get(pr_rcp26_31_35, "pr")

#------------------------------20360101-20401231----------------------------------------------------
pr_rcp26_36_40 = nc_open("pr_AFR-44_MPI-M-MPI-ESM-LR_rcp26_r1i1p1_MPI-CSC-REMO2009_v1_day_20360101-20401231.nc")
pr_rcp26_36_40.array <- ncvar_get(pr_rcp26_36_40, "pr")

#------------------------------20410101-20451231----------------------------------------------------
pr_rcp26_41_45 = nc_open("pr_AFR-44_MPI-M-MPI-ESM-LR_rcp26_r1i1p1_MPI-CSC-REMO2009_v1_day_20410101-20451231.nc")
pr_rcp26_41_45.array <- ncvar_get(pr_rcp26_41_45, "pr")

#------------------------------20460101-20501231----------------------------------------------------
pr_rcp26_46_50 = nc_open("pr_AFR-44_MPI-M-MPI-ESM-LR_rcp26_r1i1p1_MPI-CSC-REMO2009_v1_day_20460101-20501231.nc")
pr_rcp26_46_50.array <- ncvar_get(pr_rcp26_46_50, "pr")

#------------------------------20510101-20551231----------------------------------------------------
pr_rcp26_51_55 = nc_open("pr_AFR-44_MPI-M-MPI-ESM-LR_rcp26_r1i1p1_MPI-CSC-REMO2009_v1_day_20510101-20551231.nc")
pr_rcp26_51_55.array <- ncvar_get(pr_rcp26_51_55, "pr")

#------------------------------20560101-20601231----------------------------------------------------
pr_rcp26_56_60 = nc_open("pr_AFR-44_MPI-M-MPI-ESM-LR_rcp26_r1i1p1_MPI-CSC-REMO2009_v1_day_20560101-20601231.nc")
pr_rcp26_56_60.array <- ncvar_get(pr_rcp26_56_60, "pr")

#------------------------------20610101-20651231----------------------------------------------------
pr_rcp26_61_65 = nc_open("pr_AFR-44_MPI-M-MPI-ESM-LR_rcp26_r1i1p1_MPI-CSC-REMO2009_v1_day_20610101-20651231.nc")
pr_rcp26_61_65.array <- ncvar_get(pr_rcp26_61_65, "pr")

#------------------------------20660101-20701231----------------------------------------------------
pr_rcp26_66_70 = nc_open("pr_AFR-44_MPI-M-MPI-ESM-LR_rcp26_r1i1p1_MPI-CSC-REMO2009_v1_day_20660101-20701231.nc")
pr_rcp26_66_70.array <- ncvar_get(pr_rcp26_66_70, "pr")

#------------------------------20710101-20751231----------------------------------------------------
pr_rcp26_71_75 = nc_open("pr_AFR-44_MPI-M-MPI-ESM-LR_rcp26_r1i1p1_MPI-CSC-REMO2009_v1_day_20710101-20751231.nc")
pr_rcp26_71_75.array <- ncvar_get(pr_rcp26_71_75, "pr")

#------------------------------20760101-20801231----------------------------------------------------
pr_rcp26_76_80 = nc_open("pr_AFR-44_MPI-M-MPI-ESM-LR_rcp26_r1i1p1_MPI-CSC-REMO2009_v1_day_20760101-20801231.nc")
pr_rcp26_76_80.array <- ncvar_get(pr_rcp26_76_80, "pr")

#------------------------------20810101-20851231----------------------------------------------------
pr_rcp26_81_85 = nc_open("pr_AFR-44_MPI-M-MPI-ESM-LR_rcp26_r1i1p1_MPI-CSC-REMO2009_v1_day_20810101-20851231.nc")
pr_rcp26_81_85.array <- ncvar_get(pr_rcp26_81_85, "pr")

#------------------------------20860101-20901231----------------------------------------------------
pr_rcp26_86_90 = nc_open("pr_AFR-44_MPI-M-MPI-ESM-LR_rcp26_r1i1p1_MPI-CSC-REMO2009_v1_day_20860101-20901231.nc")
pr_rcp26_86_90.array <- ncvar_get(pr_rcp26_86_90, "pr")

#------------------------------20910101-20951231----------------------------------------------------
pr_rcp26_91_95 = nc_open("pr_AFR-44_MPI-M-MPI-ESM-LR_rcp26_r1i1p1_MPI-CSC-REMO2009_v1_day_20910101-20951231.nc")
pr_rcp26_91_95.array <- ncvar_get(pr_rcp26_91_95, "pr")

#------------------------------20960101-21001231----------------------------------------------------
pr_rcp26_96_100 = nc_open("pr_AFR-44_MPI-M-MPI-ESM-LR_rcp26_r1i1p1_MPI-CSC-REMO2009_v1_day_20960101-21001231.nc")
pr_rcp26_96_100.array <- ncvar_get(pr_rcp26_96_100, "pr")

write_rds(list(pr_rcp26_21_25 = pr_rcp26_21_25, pr_rcp26_26_30 = pr_rcp26_26_30, pr_rcp26_31_35 = pr_rcp26_31_35,
               pr_rcp26_36_40 = pr_rcp26_36_40, pr_rcp26_41_45 = pr_rcp26_41_45, pr_rcp26_46_50 = pr_rcp26_46_50,
               pr_rcp26_51_55 = pr_rcp26_51_55, pr_rcp26_56_60 = pr_rcp26_56_60, pr_rcp26_61_65 = pr_rcp26_61_65,
               pr_rcp26_66_70 = pr_rcp26_66_70, pr_rcp26_71_75 = pr_rcp26_71_75, pr_rcp26_76_80 = pr_rcp26_76_80,
               pr_rcp26_81_85 = pr_rcp26_81_85, pr_rcp26_86_90 = pr_rcp26_86_90, pr_rcp26_91_95 = pr_rcp26_91_95,
               pr_rcp26_96_100 = pr_rcp26_96_100),
          path = file.path(data_dir ="/Users/hebowen/Desktop/extreme-events-project-WesternCape/data/rcp26/pr", "pr_rcp26.Rds"))

##----------------------------------rcp 4.5 scenario---------------------------------------------------------------
#--------------------------20210101-20251231------------------------------------------------------
setwd("~/Desktop/extreme-events-project-WesternCape/data/rcp45/pr")
pr_rcp45_21_25 = nc_open("pr_AFR-44_MPI-M-MPI-ESM-LR_rcp45_r1i1p1_MPI-CSC-REMO2009_v1_day_20210101-20251231.nc")
lon <- ncvar_get(pr_rcp45_21_25, "lon")
lat <- ncvar_get(pr_rcp45_21_25, "lat", verbose = F)
pr_rcp45_21_25.array <- ncvar_get(pr_rcp45_21_25, "pr") 

#------------------------------20260101-20301231----------------------------------------------------
pr_rcp45_26_30 = nc_open("pr_AFR-44_MPI-M-MPI-ESM-LR_rcp45_r1i1p1_MPI-CSC-REMO2009_v1_day_20260101-20301231.nc")
pr_rcp45_26_30.array <- ncvar_get(pr_rcp45_26_30, "pr")

#------------------------------20310101-20351231----------------------------------------------------
pr_rcp45_31_35 = nc_open("pr_AFR-44_MPI-M-MPI-ESM-LR_rcp45_r1i1p1_MPI-CSC-REMO2009_v1_day_20310101-20351231.nc")
pr_rcp45_31_35.array <- ncvar_get(pr_rcp45_31_35, "pr")

#------------------------------20360101-20401231----------------------------------------------------
pr_rcp45_36_40 = nc_open("pr_AFR-44_MPI-M-MPI-ESM-LR_rcp45_r1i1p1_MPI-CSC-REMO2009_v1_day_20360101-20401231.nc")
pr_rcp45_36_40.array <- ncvar_get(pr_rcp45_36_40, "pr")

#------------------------------20410101-20451231----------------------------------------------------
pr_rcp45_41_45 = nc_open("pr_AFR-44_MPI-M-MPI-ESM-LR_rcp45_r1i1p1_MPI-CSC-REMO2009_v1_day_20410101-20451231.nc")
pr_rcp45_41_45.array <- ncvar_get(pr_rcp45_41_45, "pr")

#------------------------------20460101-20501231----------------------------------------------------
pr_rcp45_46_50 = nc_open("pr_AFR-44_MPI-M-MPI-ESM-LR_rcp45_r1i1p1_MPI-CSC-REMO2009_v1_day_20460101-20501231.nc")
pr_rcp45_46_50.array <- ncvar_get(pr_rcp45_46_50, "pr")

#------------------------------20510101-20551231----------------------------------------------------
pr_rcp45_51_55 = nc_open("pr_AFR-44_MPI-M-MPI-ESM-LR_rcp45_r1i1p1_MPI-CSC-REMO2009_v1_day_20510101-20551231.nc")
pr_rcp45_51_55.array <- ncvar_get(pr_rcp45_51_55, "pr")

#------------------------------20560101-20601231----------------------------------------------------
pr_rcp45_56_60 = nc_open("pr_AFR-44_MPI-M-MPI-ESM-LR_rcp45_r1i1p1_MPI-CSC-REMO2009_v1_day_20560101-20601231.nc")
pr_rcp45_56_60.array <- ncvar_get(pr_rcp45_56_60, "pr")

#------------------------------20610101-20651231----------------------------------------------------
pr_rcp45_61_65 = nc_open("pr_AFR-44_MPI-M-MPI-ESM-LR_rcp45_r1i1p1_MPI-CSC-REMO2009_v1_day_20610101-20651231.nc")
pr_rcp45_61_65.array <- ncvar_get(pr_rcp45_61_65, "pr")

#------------------------------20660101-20701231----------------------------------------------------
pr_rcp45_66_70 = nc_open("pr_AFR-44_MPI-M-MPI-ESM-LR_rcp45_r1i1p1_MPI-CSC-REMO2009_v1_day_20660101-20701231.nc")
pr_rcp45_66_70.array <- ncvar_get(pr_rcp45_66_70, "pr")

#------------------------------20710101-20751231----------------------------------------------------
pr_rcp45_71_75 = nc_open("pr_AFR-44_MPI-M-MPI-ESM-LR_rcp45_r1i1p1_MPI-CSC-REMO2009_v1_day_20710101-20751231.nc")
pr_rcp45_71_75.array <- ncvar_get(pr_rcp45_71_75, "pr")

#------------------------------20760101-20801231----------------------------------------------------
pr_rcp45_76_80 = nc_open("pr_AFR-44_MPI-M-MPI-ESM-LR_rcp45_r1i1p1_MPI-CSC-REMO2009_v1_day_20760101-20801231.nc")
pr_rcp45_76_80.array <- ncvar_get(pr_rcp45_76_80, "pr")

#------------------------------20810101-20851231----------------------------------------------------
pr_rcp45_81_85 = nc_open("pr_AFR-44_MPI-M-MPI-ESM-LR_rcp45_r1i1p1_MPI-CSC-REMO2009_v1_day_20810101-20851231.nc")
pr_rcp45_81_85.array <- ncvar_get(pr_rcp45_81_85, "pr")

#------------------------------20860101-20901231----------------------------------------------------
pr_rcp45_86_90 = nc_open("pr_AFR-44_MPI-M-MPI-ESM-LR_rcp45_r1i1p1_MPI-CSC-REMO2009_v1_day_20860101-20901231.nc")
pr_rcp45_86_90.array <- ncvar_get(pr_rcp45_86_90, "pr")

#------------------------------20910101-20951231----------------------------------------------------
pr_rcp45_91_95 = nc_open("pr_AFR-44_MPI-M-MPI-ESM-LR_rcp45_r1i1p1_MPI-CSC-REMO2009_v1_day_20910101-20951231.nc")
pr_rcp45_91_95.array <- ncvar_get(pr_rcp45_91_95, "pr")

#------------------------------20960101-21001231----------------------------------------------------
pr_rcp45_96_100 = nc_open("pr_AFR-44_MPI-M-MPI-ESM-LR_rcp45_r1i1p1_MPI-CSC-REMO2009_v1_day_20960101-21001231.nc")
pr_rcp45_96_100.array <- ncvar_get(pr_rcp45_96_100, "pr")


write_rds(list(pr_rcp45_21_25 = pr_rcp45_21_25, pr_rcp45_26_30 = pr_rcp45_26_30, pr_rcp45_31_35 = pr_rcp45_31_35,
               pr_rcp45_36_40 = pr_rcp45_36_40, pr_rcp45_41_45 = pr_rcp45_41_45, pr_rcp45_46_50 = pr_rcp45_46_50,
               pr_rcp45_51_55 = pr_rcp45_51_55, pr_rcp45_56_60 = pr_rcp45_56_60, pr_rcp45_61_65 = pr_rcp45_61_65,
               pr_rcp45_66_70 = pr_rcp45_66_70, pr_rcp45_71_75 = pr_rcp45_71_75, pr_rcp45_76_80 = pr_rcp45_76_80,
               pr_rcp45_81_85 = pr_rcp45_81_85, pr_rcp45_86_90 = pr_rcp45_86_90, pr_rcp45_91_95 = pr_rcp45_91_95,
               pr_rcp45_96_100 = pr_rcp45_96_100),
          path = file.path(data_dir ="/Users/hebowen/Desktop/extreme-events-project-WesternCape/data/rcp45/pr", "pr_rcp45.Rds"))

##----------------------------------rcp 8.5 scenario---------------------------------------------------------------
#--------------------------20210101-20251231------------------------------------------------------
setwd("~/Desktop/extreme-events-project-WesternCape/data/rcp85/pr")
pr_rcp85_21_25 = nc_open("pr_AFR-44_MPI-M-MPI-ESM-LR_rcp85_r1i1p1_MPI-CSC-REMO2009_v1_day_20210101-20251231.nc")
lon <- ncvar_get(pr_rcp85_21_25, "lon")
lat <- ncvar_get(pr_rcp85_21_25, "lat", verbose = F)
pr_rcp85_21_25.array <- ncvar_get(pr_rcp85_21_25, "pr") 

#------------------------------20260101-20301231----------------------------------------------------
pr_rcp85_26_30 = nc_open("pr_AFR-44_MPI-M-MPI-ESM-LR_rcp85_r1i1p1_MPI-CSC-REMO2009_v1_day_20260101-20301231.nc")
pr_rcp85_26_30.array <- ncvar_get(pr_rcp85_26_30, "pr")

#------------------------------20310101-20351231----------------------------------------------------
pr_rcp85_31_35 = nc_open("pr_AFR-44_MPI-M-MPI-ESM-LR_rcp85_r1i1p1_MPI-CSC-REMO2009_v1_day_20310101-20351231.nc")
pr_rcp85_31_35.array <- ncvar_get(pr_rcp85_31_35, "pr")

#------------------------------20360101-20401231----------------------------------------------------
pr_rcp85_36_40 = nc_open("pr_AFR-44_MPI-M-MPI-ESM-LR_rcp85_r1i1p1_MPI-CSC-REMO2009_v1_day_20360101-20401231.nc")
pr_rcp85_36_40.array <- ncvar_get(pr_rcp85_36_40, "pr")

#------------------------------20410101-20451231----------------------------------------------------
pr_rcp85_41_45 = nc_open("pr_AFR-44_MPI-M-MPI-ESM-LR_rcp85_r1i1p1_MPI-CSC-REMO2009_v1_day_20410101-20451231.nc")
pr_rcp85_41_45.array <- ncvar_get(pr_rcp85_41_45, "pr")

#------------------------------20460101-20501231----------------------------------------------------
pr_rcp85_46_50 = nc_open("pr_AFR-44_MPI-M-MPI-ESM-LR_rcp85_r1i1p1_MPI-CSC-REMO2009_v1_day_20460101-20501231.nc")
pr_rcp85_46_50.array <- ncvar_get(pr_rcp85_46_50, "pr")

#------------------------------20510101-20551231----------------------------------------------------
pr_rcp85_51_55 = nc_open("pr_AFR-44_MPI-M-MPI-ESM-LR_rcp85_r1i1p1_MPI-CSC-REMO2009_v1_day_20510101-20551231.nc")
pr_rcp85_51_55.array <- ncvar_get(pr_rcp85_51_55, "pr")

#------------------------------20560101-20601231----------------------------------------------------
pr_rcp85_56_60 = nc_open("pr_AFR-44_MPI-M-MPI-ESM-LR_rcp85_r1i1p1_MPI-CSC-REMO2009_v1_day_20560101-20601231.nc")
pr_rcp85_56_60.array <- ncvar_get(pr_rcp85_56_60, "pr")

#------------------------------20610101-20651231----------------------------------------------------
pr_rcp85_61_65 = nc_open("pr_AFR-44_MPI-M-MPI-ESM-LR_rcp85_r1i1p1_MPI-CSC-REMO2009_v1_day_20610101-20651231.nc")
pr_rcp85_61_65.array <- ncvar_get(pr_rcp85_61_65, "pr")

#------------------------------20660101-20701231----------------------------------------------------
pr_rcp85_66_70 = nc_open("pr_AFR-44_MPI-M-MPI-ESM-LR_rcp85_r1i1p1_MPI-CSC-REMO2009_v1_day_20660101-20701231.nc")
pr_rcp85_66_70.array <- ncvar_get(pr_rcp85_66_70, "pr")

#------------------------------20710101-20751231----------------------------------------------------
pr_rcp85_71_75 = nc_open("pr_AFR-44_MPI-M-MPI-ESM-LR_rcp85_r1i1p1_MPI-CSC-REMO2009_v1_day_20710101-20751231.nc")
pr_rcp85_71_75.array <- ncvar_get(pr_rcp85_71_75, "pr")

#------------------------------20760101-20801231----------------------------------------------------
pr_rcp85_76_80 = nc_open("pr_AFR-44_MPI-M-MPI-ESM-LR_rcp85_r1i1p1_MPI-CSC-REMO2009_v1_day_20760101-20801231.nc")
pr_rcp85_76_80.array <- ncvar_get(pr_rcp85_76_80, "pr")

#------------------------------20810101-20851231----------------------------------------------------
pr_rcp85_81_85 = nc_open("pr_AFR-44_MPI-M-MPI-ESM-LR_rcp85_r1i1p1_MPI-CSC-REMO2009_v1_day_20810101-20851231.nc")
pr_rcp85_81_85.array <- ncvar_get(pr_rcp85_81_85, "pr")

#------------------------------20860101-20901231----------------------------------------------------
pr_rcp85_86_90 = nc_open("pr_AFR-44_MPI-M-MPI-ESM-LR_rcp85_r1i1p1_MPI-CSC-REMO2009_v1_day_20860101-20901231.nc")
pr_rcp85_86_90.array <- ncvar_get(pr_rcp85_86_90, "pr")

#------------------------------20910101-20951231----------------------------------------------------
pr_rcp85_91_95 = nc_open("pr_AFR-44_MPI-M-MPI-ESM-LR_rcp85_r1i1p1_MPI-CSC-REMO2009_v1_day_20910101-20951231.nc")
pr_rcp85_91_95.array <- ncvar_get(pr_rcp85_91_95, "pr")

#------------------------------20960101-21001231----------------------------------------------------
pr_rcp85_96_100 = nc_open("pr_AFR-44_MPI-M-MPI-ESM-LR_rcp85_r1i1p1_MPI-CSC-REMO2009_v1_day_20960101-21001231.nc")
pr_rcp85_96_100.array <- ncvar_get(pr_rcp85_96_100, "pr")


write_rds(list(pr_rcp85_21_25 = pr_rcp85_21_25, pr_rcp85_26_30 = pr_rcp85_26_30, pr_rcp85_31_35 = pr_rcp85_31_35,
               pr_rcp85_36_40 = pr_rcp85_36_40, pr_rcp85_41_45 = pr_rcp85_41_45, pr_rcp85_46_50 = pr_rcp85_46_50,
               pr_rcp85_51_55 = pr_rcp85_51_55, pr_rcp85_56_60 = pr_rcp85_56_60, pr_rcp85_61_65 = pr_rcp85_61_65,
               pr_rcp85_66_70 = pr_rcp85_66_70, pr_rcp85_71_75 = pr_rcp85_71_75, pr_rcp85_76_80 = pr_rcp85_76_80,
               pr_rcp85_81_85 = pr_rcp85_81_85, pr_rcp85_86_90 = pr_rcp85_86_90, pr_rcp85_91_95 = pr_rcp85_91_95,
               pr_rcp85_96_100 = pr_rcp85_96_100),
          path = file.path(data_dir ="/Users/hebowen/Desktop/extreme-events-project-WesternCape/data/rcp85/pr", "pr_rcp85.Rds"))

### ----------------daily-maximum-near-surface-air-tempereature(tasmax)--------------------------###
##-------------------------rcp2.6 scenario--------------------------------------------------------
#--------------------------20210101-20251231------------------------------------------------------
setwd("~/Desktop/extreme-events-project-WesternCape/data/rcp26/tasmax")
tasmax_rcp26_21_25 = nc_open("tasmax_AFR-44_MPI-M-MPI-ESM-LR_rcp26_r1i1p1_MPI-CSC-REMO2009_v1_day_20210101-20251231.nc")
lon <- ncvar_get(tasmax_rcp26_21_25, "lon")
lat <- ncvar_get(tasmax_rcp26_21_25, "lat", verbose = F)
tasmax_rcp26_21_25.array <- ncvar_get(tasmax_rcp26_21_25, "tasmax") 

#--------------------------20260101-20301231------------------------------------------------------
tasmax_rcp26_26_30 = nc_open("tasmax_AFR-44_MPI-M-MPI-ESM-LR_rcp26_r1i1p1_MPI-CSC-REMO2009_v1_day_20260101-20301231.nc")
tasmax_rcp26_26_30.array <- ncvar_get(tasmax_rcp26_26_30, "tasmax") 

#--------------------------20310101-20351231------------------------------------------------------
tasmax_rcp26_31_35 = nc_open("tasmax_AFR-44_MPI-M-MPI-ESM-LR_rcp26_r1i1p1_MPI-CSC-REMO2009_v1_day_20310101-20351231.nc")
tasmax_rcp26_31_35.array <- ncvar_get(tasmax_rcp26_31_35, "tasmax") 

#--------------------------20360101-20401231------------------------------------------------------
tasmax_rcp26_36_40 = nc_open("tasmax_AFR-44_MPI-M-MPI-ESM-LR_rcp26_r1i1p1_MPI-CSC-REMO2009_v1_day_20360101-20401231.nc")
tasmax_rcp26_36_40.array <- ncvar_get(tasmax_rcp26_36_40, "tasmax") 

#--------------------------20410101-20451231------------------------------------------------------
tasmax_rcp26_41_45 = nc_open("tasmax_AFR-44_MPI-M-MPI-ESM-LR_rcp26_r1i1p1_MPI-CSC-REMO2009_v1_day_20410101-20451231.nc")
tasmax_rcp26_41_45.array <- ncvar_get(tasmax_rcp26_41_45, "tasmax") 

#--------------------------20460101-20501231------------------------------------------------------
tasmax_rcp26_46_50 = nc_open("tasmax_AFR-44_MPI-M-MPI-ESM-LR_rcp26_r1i1p1_MPI-CSC-REMO2009_v1_day_20460101-20501231.nc")
tasmax_rcp26_46_50.array <- ncvar_get(tasmax_rcp26_46_50, "tasmax") 

#--------------------------20510101-20551231------------------------------------------------------
tasmax_rcp26_51_55 = nc_open("tasmax_AFR-44_MPI-M-MPI-ESM-LR_rcp26_r1i1p1_MPI-CSC-REMO2009_v1_day_20510101-20551231.nc")
tasmax_rcp26_51_55.array <- ncvar_get(tasmax_rcp26_51_55, "tasmax") 

#--------------------------20560101-20601231------------------------------------------------------
tasmax_rcp26_56_60 = nc_open("tasmax_AFR-44_MPI-M-MPI-ESM-LR_rcp26_r1i1p1_MPI-CSC-REMO2009_v1_day_20560101-20601231.nc")
tasmax_rcp26_56_60.array <- ncvar_get(tasmax_rcp26_56_60, "tasmax") 

#--------------------------20610101-20651231------------------------------------------------------
tasmax_rcp26_61_65 = nc_open("tasmax_AFR-44_MPI-M-MPI-ESM-LR_rcp26_r1i1p1_MPI-CSC-REMO2009_v1_day_20610101-20651231.nc")
tasmax_rcp26_61_65.array <- ncvar_get(tasmax_rcp26_61_65, "tasmax")

#--------------------------20660101-20701231------------------------------------------------------
tasmax_rcp26_66_70 = nc_open("tasmax_AFR-44_MPI-M-MPI-ESM-LR_rcp26_r1i1p1_MPI-CSC-REMO2009_v1_day_20660101-20701231.nc")
tasmax_rcp26_66_70.array <- ncvar_get(tasmax_rcp26_66_70, "tasmax")

#--------------------------20710101-20751231------------------------------------------------------
tasmax_rcp26_71_75 = nc_open("tasmax_AFR-44_MPI-M-MPI-ESM-LR_rcp26_r1i1p1_MPI-CSC-REMO2009_v1_day_20710101-20751231.nc")
tasmax_rcp26_71_75.array <- ncvar_get(tasmax_rcp26_71_75, "tasmax")

#--------------------------20760101-20801231------------------------------------------------------
tasmax_rcp26_76_80 = nc_open("tasmax_AFR-44_MPI-M-MPI-ESM-LR_rcp26_r1i1p1_MPI-CSC-REMO2009_v1_day_20760101-20801231.nc")
tasmax_rcp26_76_80.array <- ncvar_get(tasmax_rcp26_76_80, "tasmax")

#--------------------------20810101-20851231------------------------------------------------------
tasmax_rcp26_81_85 = nc_open("tasmax_AFR-44_MPI-M-MPI-ESM-LR_rcp26_r1i1p1_MPI-CSC-REMO2009_v1_day_20810101-20851231.nc")
tasmax_rcp26_81_85.array <- ncvar_get(tasmax_rcp26_81_85, "tasmax")

#--------------------------20860101-20901231------------------------------------------------------
tasmax_rcp26_86_90 = nc_open("tasmax_AFR-44_MPI-M-MPI-ESM-LR_rcp26_r1i1p1_MPI-CSC-REMO2009_v1_day_20860101-20901231.nc")
tasmax_rcp26_86_90.array <- ncvar_get(tasmax_rcp26_86_90, "tasmax")

#--------------------------20910101-20951231------------------------------------------------------
tasmax_rcp26_91_95 = nc_open("tasmax_AFR-44_MPI-M-MPI-ESM-LR_rcp26_r1i1p1_MPI-CSC-REMO2009_v1_day_20910101-20951231.nc")
tasmax_rcp26_91_95.array <- ncvar_get(tasmax_rcp26_91_95, "tasmax")

#--------------------------20960101-21001231------------------------------------------------------
tasmax_rcp26_96_100 = nc_open("tasmax_AFR-44_MPI-M-MPI-ESM-LR_rcp26_r1i1p1_MPI-CSC-REMO2009_v1_day_20960101-21001231.nc")
tasmax_rcp26_96_100.array <- ncvar_get(tasmax_rcp26_96_100, "tasmax")

write_rds(list(tasmax_rcp26_21_25 = tasmax_rcp26_21_25, tasmax_rcp26_26_30 = tasmax_rcp26_26_30, tasmax_rcp26_31_35 = tasmax_rcp26_31_35,
               tasmax_rcp26_36_40 = tasmax_rcp26_36_40, tasmax_rcp26_41_45 = tasmax_rcp26_41_45, tasmax_rcp26_46_50 = tasmax_rcp26_46_50,
               tasmax_rcp26_51_55 = tasmax_rcp26_51_55, tasmax_rcp26_56_60 = tasmax_rcp26_56_60, tasmax_rcp26_61_65 = tasmax_rcp26_61_65,
               tasmax_rcp26_66_70 = tasmax_rcp26_66_70, tasmax_rcp26_71_75 = tasmax_rcp26_71_75, tasmax_rcp26_76_80 = tasmax_rcp26_76_80,
               tasmax_rcp26_81_85 = tasmax_rcp26_81_85, tasmax_rcp26_86_90 = tasmax_rcp26_86_90, tasmax_rcp26_91_95 = tasmax_rcp26_91_95,
               tasmax_rcp26_96_100 = tasmax_rcp26_96_100),
          path = file.path(data_dir ="/Users/hebowen/Desktop/extreme-events-project-WesternCape/data/rcp26/tasmax", "tasmax_rcp26.Rds"))

##-------------------------rcp4.5 scenario--------------------------------------------------------
#--------------------------20210101-20251231------------------------------------------------------
setwd("~/Desktop/extreme-events-project-WesternCape/data/rcp45/tasmax")
tasmax_rcp45_21_25 = nc_open("tasmax_AFR-44_MPI-M-MPI-ESM-LR_rcp45_r1i1p1_MPI-CSC-REMO2009_v1_day_20210101-20251231.nc")
lon <- ncvar_get(tasmax_rcp45_21_25, "lon")
lat <- ncvar_get(tasmax_rcp45_21_25, "lat", verbose = F)
tasmax_rcp45_21_25.array <- ncvar_get(tasmax_rcp45_21_25, "tasmax") 

#--------------------------20260101-20301231------------------------------------------------------
tasmax_rcp45_26_30 = nc_open("tasmax_AFR-44_MPI-M-MPI-ESM-LR_rcp45_r1i1p1_MPI-CSC-REMO2009_v1_day_20260101-20301231.nc")
tasmax_rcp45_26_30.array <- ncvar_get(tasmax_rcp45_26_30, "tasmax") 

#--------------------------20310101-20351231------------------------------------------------------
tasmax_rcp45_31_35 = nc_open("tasmax_AFR-44_MPI-M-MPI-ESM-LR_rcp45_r1i1p1_MPI-CSC-REMO2009_v1_day_20310101-20351231.nc")
tasmax_rcp45_31_35.array <- ncvar_get(tasmax_rcp45_31_35, "tasmax") 

#--------------------------20360101-20401231------------------------------------------------------
tasmax_rcp45_36_40 = nc_open("tasmax_AFR-44_MPI-M-MPI-ESM-LR_rcp45_r1i1p1_MPI-CSC-REMO2009_v1_day_20360101-20401231.nc")
tasmax_rcp45_36_40.array <- ncvar_get(tasmax_rcp45_36_40, "tasmax") 

#--------------------------20410101-20451231------------------------------------------------------
tasmax_rcp45_41_45 = nc_open("tasmax_AFR-44_MPI-M-MPI-ESM-LR_rcp45_r1i1p1_MPI-CSC-REMO2009_v1_day_20410101-20451231.nc")
tasmax_rcp45_41_45.array <- ncvar_get(tasmax_rcp45_41_45, "tasmax") 

#--------------------------20460101-20501231------------------------------------------------------
tasmax_rcp45_46_50 = nc_open("tasmax_AFR-44_MPI-M-MPI-ESM-LR_rcp45_r1i1p1_MPI-CSC-REMO2009_v1_day_20460101-20501231.nc")
tasmax_rcp45_46_50.array <- ncvar_get(tasmax_rcp45_46_50, "tasmax") 

#--------------------------20510101-20551231------------------------------------------------------
tasmax_rcp45_51_55 = nc_open("tasmax_AFR-44_MPI-M-MPI-ESM-LR_rcp45_r1i1p1_MPI-CSC-REMO2009_v1_day_20510101-20551231.nc")
tasmax_rcp45_51_55.array <- ncvar_get(tasmax_rcp45_51_55, "tasmax") 

#--------------------------20560101-20601231------------------------------------------------------
tasmax_rcp45_56_60 = nc_open("tasmax_AFR-44_MPI-M-MPI-ESM-LR_rcp45_r1i1p1_MPI-CSC-REMO2009_v1_day_20560101-20601231.nc")
tasmax_rcp45_56_60.array <- ncvar_get(tasmax_rcp45_56_60, "tasmax") 

#--------------------------20610101-20651231------------------------------------------------------
tasmax_rcp45_61_65 = nc_open("tasmax_AFR-44_MPI-M-MPI-ESM-LR_rcp45_r1i1p1_MPI-CSC-REMO2009_v1_day_20610101-20651231.nc")
tasmax_rcp45_61_65.array <- ncvar_get(tasmax_rcp45_61_65, "tasmax")

#--------------------------20660101-20701231------------------------------------------------------
tasmax_rcp45_66_70 = nc_open("tasmax_AFR-44_MPI-M-MPI-ESM-LR_rcp45_r1i1p1_MPI-CSC-REMO2009_v1_day_20660101-20701231.nc")
tasmax_rcp45_66_70.array <- ncvar_get(tasmax_rcp45_66_70, "tasmax")

#--------------------------20710101-20751231------------------------------------------------------
tasmax_rcp45_71_75 = nc_open("tasmax_AFR-44_MPI-M-MPI-ESM-LR_rcp45_r1i1p1_MPI-CSC-REMO2009_v1_day_20710101-20751231.nc")
tasmax_rcp45_71_75.array <- ncvar_get(tasmax_rcp45_71_75, "tasmax")

#--------------------------20760101-20801231------------------------------------------------------
tasmax_rcp45_76_80 = nc_open("tasmax_AFR-44_MPI-M-MPI-ESM-LR_rcp45_r1i1p1_MPI-CSC-REMO2009_v1_day_20760101-20801231.nc")
tasmax_rcp45_76_80.array <- ncvar_get(tasmax_rcp45_76_80, "tasmax")

#--------------------------20810101-20851231------------------------------------------------------
tasmax_rcp45_81_85 = nc_open("tasmax_AFR-44_MPI-M-MPI-ESM-LR_rcp45_r1i1p1_MPI-CSC-REMO2009_v1_day_20810101-20851231.nc")
tasmax_rcp45_81_85.array <- ncvar_get(tasmax_rcp45_81_85, "tasmax")

#--------------------------20860101-20901231------------------------------------------------------
tasmax_rcp45_86_90 = nc_open("tasmax_AFR-44_MPI-M-MPI-ESM-LR_rcp45_r1i1p1_MPI-CSC-REMO2009_v1_day_20860101-20901231.nc")
tasmax_rcp45_86_90.array <- ncvar_get(tasmax_rcp45_86_90, "tasmax")

#--------------------------20910101-20951231------------------------------------------------------
tasmax_rcp45_91_95 = nc_open("tasmax_AFR-44_MPI-M-MPI-ESM-LR_rcp45_r1i1p1_MPI-CSC-REMO2009_v1_day_20910101-20951231.nc")
tasmax_rcp45_91_95.array <- ncvar_get(tasmax_rcp45_91_95, "tasmax")

#--------------------------20960101-21001231------------------------------------------------------
tasmax_rcp45_96_100 = nc_open("tasmax_AFR-44_MPI-M-MPI-ESM-LR_rcp45_r1i1p1_MPI-CSC-REMO2009_v1_day_20960101-21001231.nc")
tasmax_rcp45_96_100.array <- ncvar_get(tasmax_rcp45_96_100, "tasmax")

write_rds(list(tasmax_rcp45_21_25 = tasmax_rcp45_21_25, tasmax_rcp45_26_30 = tasmax_rcp45_26_30, tasmax_rcp45_31_35 = tasmax_rcp45_31_35,
               tasmax_rcp45_36_40 = tasmax_rcp45_36_40, tasmax_rcp45_41_45 = tasmax_rcp45_41_45, tasmax_rcp45_46_50 = tasmax_rcp45_46_50,
               tasmax_rcp45_51_55 = tasmax_rcp45_51_55, tasmax_rcp45_56_60 = tasmax_rcp45_56_60, tasmax_rcp45_61_65 = tasmax_rcp45_61_65,
               tasmax_rcp45_66_70 = tasmax_rcp45_66_70, tasmax_rcp45_71_75 = tasmax_rcp45_71_75, tasmax_rcp45_76_80 = tasmax_rcp45_76_80,
               tasmax_rcp45_81_85 = tasmax_rcp45_81_85, tasmax_rcp45_86_90 = tasmax_rcp45_86_90, tasmax_rcp45_91_95 = tasmax_rcp45_91_95,
               tasmax_rcp45_96_100 = tasmax_rcp45_96_100),
          path = file.path(data_dir ="/Users/hebowen/Desktop/extreme-events-project-WesternCape/data/rcp45/tasmax", "tasmax_rcp45.Rds"))

##-------------------------rcp8.5 scenario--------------------------------------------------------
#--------------------------20210101-20251231------------------------------------------------------
setwd("~/Desktop/extreme-events-project-WesternCape/data/rcp85/tasmax")
tasmax_rcp85_21_25 = nc_open("tasmax_AFR-44_MPI-M-MPI-ESM-LR_rcp85_r1i1p1_MPI-CSC-REMO2009_v1_day_20210101-20251231.nc")
lon <- ncvar_get(tasmax_rcp85_21_25, "lon")
lat <- ncvar_get(tasmax_rcp85_21_25, "lat", verbose = F)
tasmax_rcp85_21_25.array <- ncvar_get(tasmax_rcp85_21_25, "tasmax") 

#--------------------------20260101-20301231------------------------------------------------------
tasmax_rcp85_26_30 = nc_open("tasmax_AFR-44_MPI-M-MPI-ESM-LR_rcp85_r1i1p1_MPI-CSC-REMO2009_v1_day_20260101-20301231.nc")
tasmax_rcp85_26_30.array <- ncvar_get(tasmax_rcp85_26_30, "tasmax") 

#--------------------------20310101-20351231------------------------------------------------------
tasmax_rcp85_31_35 = nc_open("tasmax_AFR-44_MPI-M-MPI-ESM-LR_rcp85_r1i1p1_MPI-CSC-REMO2009_v1_day_20310101-20351231.nc")
tasmax_rcp85_31_35.array <- ncvar_get(tasmax_rcp85_31_35, "tasmax") 

#--------------------------20360101-20401231------------------------------------------------------
tasmax_rcp85_36_40 = nc_open("tasmax_AFR-44_MPI-M-MPI-ESM-LR_rcp85_r1i1p1_MPI-CSC-REMO2009_v1_day_20360101-20401231.nc")
tasmax_rcp85_36_40.array <- ncvar_get(tasmax_rcp85_36_40, "tasmax") 

#--------------------------20410101-20451231------------------------------------------------------
tasmax_rcp85_41_45 = nc_open("tasmax_AFR-44_MPI-M-MPI-ESM-LR_rcp85_r1i1p1_MPI-CSC-REMO2009_v1_day_20410101-20451231.nc")
tasmax_rcp85_41_45.array <- ncvar_get(tasmax_rcp85_41_45, "tasmax") 

#--------------------------20460101-20501231------------------------------------------------------
tasmax_rcp85_46_50 = nc_open("tasmax_AFR-44_MPI-M-MPI-ESM-LR_rcp85_r1i1p1_MPI-CSC-REMO2009_v1_day_20460101-20501231.nc")
tasmax_rcp85_46_50.array <- ncvar_get(tasmax_rcp85_46_50, "tasmax") 

#--------------------------20510101-20551231------------------------------------------------------
tasmax_rcp85_51_55 = nc_open("tasmax_AFR-44_MPI-M-MPI-ESM-LR_rcp85_r1i1p1_MPI-CSC-REMO2009_v1_day_20510101-20551231.nc")
tasmax_rcp85_51_55.array <- ncvar_get(tasmax_rcp85_51_55, "tasmax") 

#--------------------------20560101-20601231------------------------------------------------------
tasmax_rcp85_56_60 = nc_open("tasmax_AFR-44_MPI-M-MPI-ESM-LR_rcp85_r1i1p1_MPI-CSC-REMO2009_v1_day_20560101-20601231.nc")
tasmax_rcp85_56_60.array <- ncvar_get(tasmax_rcp85_56_60, "tasmax") 

#--------------------------20610101-20651231------------------------------------------------------
tasmax_rcp85_61_65 = nc_open("tasmax_AFR-44_MPI-M-MPI-ESM-LR_rcp85_r1i1p1_MPI-CSC-REMO2009_v1_day_20610101-20651231.nc")
tasmax_rcp85_61_65.array <- ncvar_get(tasmax_rcp85_61_65, "tasmax")

#--------------------------20660101-20701231------------------------------------------------------
tasmax_rcp85_66_70 = nc_open("tasmax_AFR-44_MPI-M-MPI-ESM-LR_rcp85_r1i1p1_MPI-CSC-REMO2009_v1_day_20660101-20701231.nc")
tasmax_rcp85_66_70.array <- ncvar_get(tasmax_rcp85_66_70, "tasmax")

#--------------------------20710101-20751231------------------------------------------------------
tasmax_rcp85_71_75 = nc_open("tasmax_AFR-44_MPI-M-MPI-ESM-LR_rcp85_r1i1p1_MPI-CSC-REMO2009_v1_day_20710101-20751231.nc")
tasmax_rcp85_71_75.array <- ncvar_get(tasmax_rcp85_71_75, "tasmax")

#--------------------------20760101-20801231------------------------------------------------------
tasmax_rcp85_76_80 = nc_open("tasmax_AFR-44_MPI-M-MPI-ESM-LR_rcp85_r1i1p1_MPI-CSC-REMO2009_v1_day_20760101-20801231.nc")
tasmax_rcp85_76_80.array <- ncvar_get(tasmax_rcp85_76_80, "tasmax")

#--------------------------20810101-20851231------------------------------------------------------
tasmax_rcp85_81_85 = nc_open("tasmax_AFR-44_MPI-M-MPI-ESM-LR_rcp85_r1i1p1_MPI-CSC-REMO2009_v1_day_20810101-20851231.nc")
tasmax_rcp85_81_85.array <- ncvar_get(tasmax_rcp85_81_85, "tasmax")

#--------------------------20860101-20901231------------------------------------------------------
tasmax_rcp85_86_90 = nc_open("tasmax_AFR-44_MPI-M-MPI-ESM-LR_rcp85_r1i1p1_MPI-CSC-REMO2009_v1_day_20860101-20901231.nc")
tasmax_rcp85_86_90.array <- ncvar_get(tasmax_rcp85_86_90, "tasmax")

#--------------------------20910101-20951231------------------------------------------------------
tasmax_rcp85_91_95 = nc_open("tasmax_AFR-44_MPI-M-MPI-ESM-LR_rcp85_r1i1p1_MPI-CSC-REMO2009_v1_day_20910101-20951231.nc")
tasmax_rcp85_91_95.array <- ncvar_get(tasmax_rcp85_91_95, "tasmax")

#--------------------------20960101-21001231------------------------------------------------------
tasmax_rcp85_96_100 = nc_open("tasmax_AFR-44_MPI-M-MPI-ESM-LR_rcp85_r1i1p1_MPI-CSC-REMO2009_v1_day_20960101-21001231.nc")
tasmax_rcp85_96_100.array <- ncvar_get(tasmax_rcp85_96_100, "tasmax")

write_rds(list(tasmax_rcp85_21_25 = tasmax_rcp85_21_25, tasmax_rcp85_26_30 = tasmax_rcp85_26_30, tasmax_rcp85_31_35 = tasmax_rcp85_31_35,
               tasmax_rcp85_36_40 = tasmax_rcp85_36_40, tasmax_rcp85_41_45 = tasmax_rcp85_41_45, tasmax_rcp85_46_50 = tasmax_rcp85_46_50,
               tasmax_rcp85_51_55 = tasmax_rcp85_51_55, tasmax_rcp85_56_60 = tasmax_rcp85_56_60, tasmax_rcp85_61_65 = tasmax_rcp85_61_65,
               tasmax_rcp85_66_70 = tasmax_rcp85_66_70, tasmax_rcp85_71_75 = tasmax_rcp85_71_75, tasmax_rcp85_76_80 = tasmax_rcp85_76_80,
               tasmax_rcp85_81_85 = tasmax_rcp85_81_85, tasmax_rcp85_86_90 = tasmax_rcp85_86_90, tasmax_rcp85_91_95 = tasmax_rcp85_91_95,
               tasmax_rcp85_96_100 = tasmax_rcp85_96_100),
          path = file.path(data_dir ="/Users/hebowen/Desktop/extreme-events-project-WesternCape/data/rcp85/tasmax", "tasmax_rcp85.Rds"))

### -------------------daily-minimum-near-surface-air-temperature(tasmin)----------------------------------------------###
##-------------------------rcp2.6 scenario--------------------------------------------------------
#--------------------------20210101-20251231------------------------------------------------------
setwd("~/Desktop/extreme-events-project-WesternCape/data/rcp26/tasmin")
tasmin_rcp26_21_25 = nc_open("tasmin_AFR-44_MPI-M-MPI-ESM-LR_rcp26_r1i1p1_MPI-CSC-REMO2009_v1_day_20210101-20251231.nc")
lon <- ncvar_get(tasmin_rcp26_21_25, "lon")
lat <- ncvar_get(tasmin_rcp26_21_25, "lat", verbose = F)
tasmin_rcp26_21_25.array <- ncvar_get(tasmin_rcp26_21_25, "tasmin") 

#--------------------------20260101-20301231------------------------------------------------------
tasmin_rcp26_26_30 = nc_open("tasmin_AFR-44_MPI-M-MPI-ESM-LR_rcp26_r1i1p1_MPI-CSC-REMO2009_v1_day_20260101-20301231.nc")
tasmin_rcp26_26_30.array <- ncvar_get(tasmin_rcp26_26_30, "tasmin") 

#--------------------------20310101-20351231------------------------------------------------------
tasmin_rcp26_31_35 = nc_open("tasmin_AFR-44_MPI-M-MPI-ESM-LR_rcp26_r1i1p1_MPI-CSC-REMO2009_v1_day_20310101-20351231.nc")
tasmin_rcp26_31_35.array <- ncvar_get(tasmin_rcp26_31_35, "tasmin") 

#--------------------------20360101-20401231------------------------------------------------------
tasmin_rcp26_36_40 = nc_open("tasmin_AFR-44_MPI-M-MPI-ESM-LR_rcp26_r1i1p1_MPI-CSC-REMO2009_v1_day_20360101-20401231.nc")
tasmin_rcp26_36_40.array <- ncvar_get(tasmin_rcp26_36_40, "tasmin") 

#--------------------------20410101-20451231------------------------------------------------------
tasmin_rcp26_41_45 = nc_open("tasmin_AFR-44_MPI-M-MPI-ESM-LR_rcp26_r1i1p1_MPI-CSC-REMO2009_v1_day_20410101-20451231.nc")
tasmin_rcp26_41_45.array <- ncvar_get(tasmin_rcp26_41_45, "tasmin") 

#--------------------------20460101-20501231------------------------------------------------------
tasmin_rcp26_46_50 = nc_open("tasmin_AFR-44_MPI-M-MPI-ESM-LR_rcp26_r1i1p1_MPI-CSC-REMO2009_v1_day_20460101-20501231.nc")
tasmin_rcp26_46_50.array <- ncvar_get(tasmin_rcp26_46_50, "tasmin") 

#--------------------------20510101-20551231------------------------------------------------------
tasmin_rcp26_51_55 = nc_open("tasmin_AFR-44_MPI-M-MPI-ESM-LR_rcp26_r1i1p1_MPI-CSC-REMO2009_v1_day_20510101-20551231.nc")
tasmin_rcp26_51_55.array <- ncvar_get(tasmin_rcp26_51_55, "tasmin") 

#--------------------------20560101-20601231------------------------------------------------------
tasmin_rcp26_56_60 = nc_open("tasmin_AFR-44_MPI-M-MPI-ESM-LR_rcp26_r1i1p1_MPI-CSC-REMO2009_v1_day_20560101-20601231.nc")
tasmin_rcp26_56_60.array <- ncvar_get(tasmin_rcp26_56_60, "tasmin") 

#--------------------------20610101-20651231------------------------------------------------------
tasmin_rcp26_61_65 = nc_open("tasmin_AFR-44_MPI-M-MPI-ESM-LR_rcp26_r1i1p1_MPI-CSC-REMO2009_v1_day_20610101-20651231.nc")
tasmin_rcp26_61_65.array <- ncvar_get(tasmin_rcp26_61_65, "tasmin") 

#--------------------------20660101-20701231------------------------------------------------------
tasmin_rcp26_66_70 = nc_open("tasmin_AFR-44_MPI-M-MPI-ESM-LR_rcp26_r1i1p1_MPI-CSC-REMO2009_v1_day_20660101-20701231.nc")
tasmin_rcp26_66_70.array <- ncvar_get(tasmin_rcp26_66_70, "tasmin") 

#--------------------------20710101-20751231------------------------------------------------------
tasmin_rcp26_71_75 = nc_open("tasmin_AFR-44_MPI-M-MPI-ESM-LR_rcp26_r1i1p1_MPI-CSC-REMO2009_v1_day_20710101-20751231.nc")
tasmin_rcp26_71_75.array <- ncvar_get(tasmin_rcp26_71_75, "tasmin") 

#--------------------------20760101-20801231------------------------------------------------------
tasmin_rcp26_76_80 = nc_open("tasmin_AFR-44_MPI-M-MPI-ESM-LR_rcp26_r1i1p1_MPI-CSC-REMO2009_v1_day_20760101-20801231.nc")
tasmin_rcp26_76_80.array <- ncvar_get(tasmin_rcp26_76_80, "tasmin") 

#--------------------------20810101-20851231------------------------------------------------------
tasmin_rcp26_81_85 = nc_open("tasmin_AFR-44_MPI-M-MPI-ESM-LR_rcp26_r1i1p1_MPI-CSC-REMO2009_v1_day_20810101-20851231.nc")
tasmin_rcp26_81_85.array <- ncvar_get(tasmin_rcp26_81_85, "tasmin") 

#--------------------------20860101-20901231------------------------------------------------------
tasmin_rcp26_86_90 = nc_open("tasmin_AFR-44_MPI-M-MPI-ESM-LR_rcp26_r1i1p1_MPI-CSC-REMO2009_v1_day_20860101-20901231.nc")
tasmin_rcp26_86_90.array <- ncvar_get(tasmin_rcp26_86_90, "tasmin") 

#--------------------------20910101-20951231------------------------------------------------------
tasmin_rcp26_91_95 = nc_open("tasmin_AFR-44_MPI-M-MPI-ESM-LR_rcp26_r1i1p1_MPI-CSC-REMO2009_v1_day_20910101-20951231.nc")
tasmin_rcp26_91_95.array <- ncvar_get(tasmin_rcp26_91_95, "tasmin") 

#--------------------------20960101-21001231------------------------------------------------------
tasmin_rcp26_96_100 = nc_open("tasmin_AFR-44_MPI-M-MPI-ESM-LR_rcp26_r1i1p1_MPI-CSC-REMO2009_v1_day_20960101-21001231.nc")
tasmin_rcp26_96_100.array <- ncvar_get(tasmin_rcp26_96_100, "tasmin") 

write_rds(list(tasmin_rcp26_21_25 = tasmin_rcp26_21_25, tasmin_rcp26_26_30 = tasmin_rcp26_26_30, tasmin_rcp26_31_35 = tasmin_rcp26_31_35,
               tasmin_rcp26_36_40 = tasmin_rcp26_36_40, tasmin_rcp26_41_45 = tasmin_rcp26_41_45, tasmin_rcp26_46_50 = tasmin_rcp26_46_50,
               tasmin_rcp26_51_55 = tasmin_rcp26_51_55, tasmin_rcp26_56_60 = tasmin_rcp26_56_60, tasmin_rcp26_61_65 = tasmin_rcp26_61_65,
               tasmin_rcp26_66_70 = tasmin_rcp26_66_70, tasmin_rcp26_71_75 = tasmin_rcp26_71_75, tasmin_rcp26_76_80 = tasmin_rcp26_76_80,
               tasmin_rcp26_81_85 = tasmin_rcp26_81_85, tasmin_rcp26_86_90 = tasmin_rcp26_86_90, tasmin_rcp26_91_95 = tasmin_rcp26_91_95,
               tasmin_rcp26_96_100 = tasmin_rcp26_96_100),
          path = file.path(data_dir ="/Users/hebowen/Desktop/extreme-events-project-WesternCape/data/rcp26/tasmin", "tasmin_rcp26.Rds"))

##-------------------------rcp4.5 scenario--------------------------------------------------------
#--------------------------20210101-20251231------------------------------------------------------
setwd("~/Desktop/extreme-events-project-WesternCape/data/rcp45/tasmin")
tasmin_rcp45_21_25 = nc_open("tasmin_AFR-44_MPI-M-MPI-ESM-LR_rcp45_r1i1p1_MPI-CSC-REMO2009_v1_day_20210101-20251231.nc")
tasmin_rcp45_21_25.array <- ncvar_get(tasmin_rcp45_21_25, "tasmin") 

#--------------------------20260101-20301231------------------------------------------------------
tasmin_rcp45_26_30 = nc_open("tasmin_AFR-44_MPI-M-MPI-ESM-LR_rcp45_r1i1p1_MPI-CSC-REMO2009_v1_day_20260101-20301231.nc")
tasmin_rcp45_26_30.array <- ncvar_get(tasmin_rcp45_26_30, "tasmin") 

#--------------------------20310101-20351231------------------------------------------------------
tasmin_rcp45_31_35 = nc_open("tasmin_AFR-44_MPI-M-MPI-ESM-LR_rcp45_r1i1p1_MPI-CSC-REMO2009_v1_day_20310101-20351231.nc")
tasmin_rcp45_31_35.array <- ncvar_get(tasmin_rcp45_31_35, "tasmin") 

#--------------------------20360101-20401231------------------------------------------------------
tasmin_rcp45_36_40 = nc_open("tasmin_AFR-44_MPI-M-MPI-ESM-LR_rcp45_r1i1p1_MPI-CSC-REMO2009_v1_day_20360101-20401231.nc")
tasmin_rcp45_36_40.array <- ncvar_get(tasmin_rcp45_36_40, "tasmin") 

#--------------------------20410101-20451231------------------------------------------------------
tasmin_rcp45_41_45 = nc_open("tasmin_AFR-44_MPI-M-MPI-ESM-LR_rcp45_r1i1p1_MPI-CSC-REMO2009_v1_day_20410101-20451231.nc")
tasmin_rcp45_41_45.array <- ncvar_get(tasmin_rcp45_41_45, "tasmin") 

#--------------------------20460101-20501231------------------------------------------------------
tasmin_rcp45_46_50 = nc_open("tasmin_AFR-44_MPI-M-MPI-ESM-LR_rcp45_r1i1p1_MPI-CSC-REMO2009_v1_day_20460101-20501231.nc")
tasmin_rcp45_46_50.array <- ncvar_get(tasmin_rcp45_46_50, "tasmin") 

#--------------------------20510101-20551231------------------------------------------------------
tasmin_rcp45_51_55 = nc_open("tasmin_AFR-44_MPI-M-MPI-ESM-LR_rcp45_r1i1p1_MPI-CSC-REMO2009_v1_day_20510101-20551231.nc")
tasmin_rcp45_51_55.array <- ncvar_get(tasmin_rcp45_51_55, "tasmin") 

#--------------------------20560101-20601231------------------------------------------------------
tasmin_rcp45_56_60 = nc_open("tasmin_AFR-44_MPI-M-MPI-ESM-LR_rcp45_r1i1p1_MPI-CSC-REMO2009_v1_day_20560101-20601231.nc")
tasmin_rcp45_56_60.array <- ncvar_get(tasmin_rcp45_56_60, "tasmin") 

#--------------------------20610101-20651231------------------------------------------------------
tasmin_rcp45_61_65 = nc_open("tasmin_AFR-44_MPI-M-MPI-ESM-LR_rcp45_r1i1p1_MPI-CSC-REMO2009_v1_day_20610101-20651231.nc")
tasmin_rcp45_61_65.array <- ncvar_get(tasmin_rcp45_61_65, "tasmin") 

#--------------------------20660101-20701231------------------------------------------------------
tasmin_rcp45_66_70 = nc_open("tasmin_AFR-44_MPI-M-MPI-ESM-LR_rcp45_r1i1p1_MPI-CSC-REMO2009_v1_day_20660101-20701231.nc")
tasmin_rcp45_66_70.array <- ncvar_get(tasmin_rcp45_66_70, "tasmin") 

#--------------------------20710101-20751231------------------------------------------------------
tasmin_rcp45_71_75 = nc_open("tasmin_AFR-44_MPI-M-MPI-ESM-LR_rcp45_r1i1p1_MPI-CSC-REMO2009_v1_day_20710101-20751231.nc")
tasmin_rcp45_71_75.array <- ncvar_get(tasmin_rcp45_71_75, "tasmin") 

#--------------------------20760101-20801231------------------------------------------------------
tasmin_rcp45_76_80 = nc_open("tasmin_AFR-44_MPI-M-MPI-ESM-LR_rcp45_r1i1p1_MPI-CSC-REMO2009_v1_day_20760101-20801231.nc")
tasmin_rcp45_76_80.array <- ncvar_get(tasmin_rcp45_76_80, "tasmin") 

#--------------------------20810101-20851231------------------------------------------------------
tasmin_rcp45_81_85 = nc_open("tasmin_AFR-44_MPI-M-MPI-ESM-LR_rcp45_r1i1p1_MPI-CSC-REMO2009_v1_day_20810101-20851231.nc")
tasmin_rcp45_81_85.array <- ncvar_get(tasmin_rcp45_81_85, "tasmin") 

#--------------------------20860101-20901231------------------------------------------------------
tasmin_rcp45_86_90 = nc_open("tasmin_AFR-44_MPI-M-MPI-ESM-LR_rcp45_r1i1p1_MPI-CSC-REMO2009_v1_day_20860101-20901231.nc")
tasmin_rcp45_86_90.array <- ncvar_get(tasmin_rcp45_86_90, "tasmin") 

#--------------------------20910101-20951231------------------------------------------------------
tasmin_rcp45_91_95 = nc_open("tasmin_AFR-44_MPI-M-MPI-ESM-LR_rcp45_r1i1p1_MPI-CSC-REMO2009_v1_day_20910101-20951231.nc")
tasmin_rcp45_91_95.array <- ncvar_get(tasmin_rcp45_91_95, "tasmin") 

#--------------------------20960101-21001231------------------------------------------------------
tasmin_rcp45_96_100 = nc_open("tasmin_AFR-44_MPI-M-MPI-ESM-LR_rcp45_r1i1p1_MPI-CSC-REMO2009_v1_day_20960101-21001231.nc")
tasmin_rcp45_96_100.array <- ncvar_get(tasmin_rcp45_96_100, "tasmin") 

write_rds(list(tasmin_rcp45_21_25 = tasmin_rcp45_21_25, tasmin_rcp45_26_30 = tasmin_rcp45_26_30, tasmin_rcp45_31_35 = tasmin_rcp45_31_35,
               tasmin_rcp45_36_40 = tasmin_rcp45_36_40, tasmin_rcp45_41_45 = tasmin_rcp45_41_45, tasmin_rcp45_46_50 = tasmin_rcp45_46_50,
               tasmin_rcp45_51_55 = tasmin_rcp45_51_55, tasmin_rcp45_56_60 = tasmin_rcp45_56_60, tasmin_rcp45_61_65 = tasmin_rcp45_61_65,
               tasmin_rcp45_66_70 = tasmin_rcp45_66_70, tasmin_rcp45_71_75 = tasmin_rcp45_71_75, tasmin_rcp45_76_80 = tasmin_rcp45_76_80,
               tasmin_rcp45_81_85 = tasmin_rcp45_81_85, tasmin_rcp45_86_90 = tasmin_rcp45_86_90, tasmin_rcp45_91_95 = tasmin_rcp45_91_95,
               tasmin_rcp45_96_100 = tasmin_rcp45_96_100),
          path = file.path(data_dir ="/Users/hebowen/Desktop/extreme-events-project-WesternCape/data/rcp45/tasmin", "tasmin_rcp45.Rds"))

##-------------------------rcp8.5 scenario--------------------------------------------------------
#--------------------------20210101-20251231------------------------------------------------------
setwd("~/Desktop/extreme-events-project-WesternCape/data/rcp85/tasmin")
tasmin_rcp85_21_25 = nc_open("tasmin_AFR-44_MPI-M-MPI-ESM-LR_rcp85_r1i1p1_MPI-CSC-REMO2009_v1_day_20210101-20251231.nc")
tasmin_rcp85_21_25.array <- ncvar_get(tasmin_rcp85_21_25, "tasmin") 

#--------------------------20260101-20301231------------------------------------------------------
tasmin_rcp85_26_30 = nc_open("tasmin_AFR-44_MPI-M-MPI-ESM-LR_rcp85_r1i1p1_MPI-CSC-REMO2009_v1_day_20260101-20301231.nc")
tasmin_rcp85_26_30.array <- ncvar_get(tasmin_rcp85_26_30, "tasmin") 

#--------------------------20310101-20351231------------------------------------------------------
tasmin_rcp85_31_35 = nc_open("tasmin_AFR-44_MPI-M-MPI-ESM-LR_rcp85_r1i1p1_MPI-CSC-REMO2009_v1_day_20310101-20351231.nc")
tasmin_rcp85_31_35.array <- ncvar_get(tasmin_rcp85_31_35, "tasmin") 

#--------------------------20360101-20401231------------------------------------------------------
tasmin_rcp85_36_40 = nc_open("tasmin_AFR-44_MPI-M-MPI-ESM-LR_rcp85_r1i1p1_MPI-CSC-REMO2009_v1_day_20360101-20401231.nc")
tasmin_rcp85_36_40.array <- ncvar_get(tasmin_rcp85_36_40, "tasmin") 

#--------------------------20410101-20451231------------------------------------------------------
tasmin_rcp85_41_45 = nc_open("tasmin_AFR-44_MPI-M-MPI-ESM-LR_rcp85_r1i1p1_MPI-CSC-REMO2009_v1_day_20410101-20451231.nc")
tasmin_rcp85_41_45.array <- ncvar_get(tasmin_rcp85_41_45, "tasmin") 

#--------------------------20460101-20501231------------------------------------------------------
tasmin_rcp85_46_50 = nc_open("tasmin_AFR-44_MPI-M-MPI-ESM-LR_rcp85_r1i1p1_MPI-CSC-REMO2009_v1_day_20460101-20501231.nc")
tasmin_rcp85_46_50.array <- ncvar_get(tasmin_rcp85_46_50, "tasmin") 

#--------------------------20510101-20551231------------------------------------------------------
tasmin_rcp85_51_55 = nc_open("tasmin_AFR-44_MPI-M-MPI-ESM-LR_rcp85_r1i1p1_MPI-CSC-REMO2009_v1_day_20510101-20551231.nc")
tasmin_rcp85_51_55.array <- ncvar_get(tasmin_rcp85_51_55, "tasmin") 

#--------------------------20560101-20601231------------------------------------------------------
tasmin_rcp85_56_60 = nc_open("tasmin_AFR-44_MPI-M-MPI-ESM-LR_rcp85_r1i1p1_MPI-CSC-REMO2009_v1_day_20560101-20601231.nc")
tasmin_rcp85_56_60.array <- ncvar_get(tasmin_rcp85_56_60, "tasmin") 

#--------------------------20610101-20651231------------------------------------------------------
tasmin_rcp85_61_65 = nc_open("tasmin_AFR-44_MPI-M-MPI-ESM-LR_rcp85_r1i1p1_MPI-CSC-REMO2009_v1_day_20610101-20651231.nc")
tasmin_rcp85_61_65.array <- ncvar_get(tasmin_rcp85_61_65, "tasmin") 

#--------------------------20660101-20701231------------------------------------------------------
tasmin_rcp85_66_70 = nc_open("tasmin_AFR-44_MPI-M-MPI-ESM-LR_rcp85_r1i1p1_MPI-CSC-REMO2009_v1_day_20660101-20701231.nc")
tasmin_rcp85_66_70.array <- ncvar_get(tasmin_rcp85_66_70, "tasmin") 

#--------------------------20710101-20751231------------------------------------------------------
tasmin_rcp85_71_75 = nc_open("tasmin_AFR-44_MPI-M-MPI-ESM-LR_rcp85_r1i1p1_MPI-CSC-REMO2009_v1_day_20710101-20751231.nc")
tasmin_rcp85_71_75.array <- ncvar_get(tasmin_rcp85_71_75, "tasmin") 

#--------------------------20760101-20801231------------------------------------------------------
tasmin_rcp85_76_80 = nc_open("tasmin_AFR-44_MPI-M-MPI-ESM-LR_rcp85_r1i1p1_MPI-CSC-REMO2009_v1_day_20760101-20801231.nc")
tasmin_rcp85_76_80.array <- ncvar_get(tasmin_rcp85_76_80, "tasmin") 

#--------------------------20810101-20851231------------------------------------------------------
tasmin_rcp85_81_85 = nc_open("tasmin_AFR-44_MPI-M-MPI-ESM-LR_rcp85_r1i1p1_MPI-CSC-REMO2009_v1_day_20810101-20851231.nc")
tasmin_rcp85_81_85.array <- ncvar_get(tasmin_rcp85_81_85, "tasmin") 

#--------------------------20860101-20901231------------------------------------------------------
tasmin_rcp85_86_90 = nc_open("tasmin_AFR-44_MPI-M-MPI-ESM-LR_rcp85_r1i1p1_MPI-CSC-REMO2009_v1_day_20860101-20901231.nc")
tasmin_rcp85_86_90.array <- ncvar_get(tasmin_rcp85_86_90, "tasmin") 

#--------------------------20910101-20951231------------------------------------------------------
tasmin_rcp85_91_95 = nc_open("tasmin_AFR-44_MPI-M-MPI-ESM-LR_rcp85_r1i1p1_MPI-CSC-REMO2009_v1_day_20910101-20951231.nc")
tasmin_rcp85_91_95.array <- ncvar_get(tasmin_rcp85_91_95, "tasmin") 

#--------------------------20960101-21001231------------------------------------------------------
tasmin_rcp85_96_100 = nc_open("tasmin_AFR-44_MPI-M-MPI-ESM-LR_rcp85_r1i1p1_MPI-CSC-REMO2009_v1_day_20960101-21001231.nc")
tasmin_rcp85_96_100.array <- ncvar_get(tasmin_rcp85_96_100, "tasmin") 

write_rds(list(tasmin_rcp85_21_25 = tasmin_rcp85_21_25, tasmin_rcp85_26_30 = tasmin_rcp85_26_30, tasmin_rcp85_31_35 = tasmin_rcp85_31_35,
               tasmin_rcp85_36_40 = tasmin_rcp85_36_40, tasmin_rcp85_41_45 = tasmin_rcp85_41_45, tasmin_rcp85_46_50 = tasmin_rcp85_46_50,
               tasmin_rcp85_51_55 = tasmin_rcp85_51_55, tasmin_rcp85_56_60 = tasmin_rcp85_56_60, tasmin_rcp85_61_65 = tasmin_rcp85_61_65,
               tasmin_rcp85_66_70 = tasmin_rcp85_66_70, tasmin_rcp85_71_75 = tasmin_rcp85_71_75, tasmin_rcp85_76_80 = tasmin_rcp85_76_80,
               tasmin_rcp85_81_85 = tasmin_rcp85_81_85, tasmin_rcp85_86_90 = tasmin_rcp85_86_90, tasmin_rcp85_91_95 = tasmin_rcp85_91_95,
               tasmin_rcp85_96_100 = tasmin_rcp85_96_100),
          path = file.path(data_dir ="/Users/hebowen/Desktop/extreme-events-project-WesternCape/data/rcp85/tasmin", "tasmin_rcp85.Rds"))

#-----------------------------------------------WSDI------------------------------------------------------------------------
#-----------------------------------------------rcp2.6 scenario---------------------------------------------------
tmax.dates_21_40 <- as.character(seq(as.Date("2021/1/1"), as.Date("2040/12/31"), by = 1))
tmin.dates_21_40 <- as.character(seq(as.Date("2021/1/1"), as.Date("2040/12/31"), by = 1))
prec.dates_21_40 <- as.character(seq(as.Date("2021/1/1"), as.Date("2040/12/31"), by = 1))

tmax.dates_21_40 <- as.PCICt(tmax.dates_21_40, cal = 'gregorian')
tmin.dates_21_40 <- as.PCICt(tmin.dates_21_40, cal = 'gregorian')
prec.dates_21_40 <- as.PCICt(prec.dates_21_40, cal = 'gregorian')

tasmax_rcp26_21_40 <- abind(tasmax_rcp26_21_25.array, tasmax_rcp26_26_30.array, tasmax_rcp26_31_35.array, tasmax_rcp26_36_40.array, along = 3)
tasmin_rcp26_21_40 <- abind(tasmin_rcp26_21_25.array, tasmin_rcp26_26_30.array, tasmin_rcp26_31_35.array, tasmin_rcp26_36_40.array, along = 3)
pr_rcp26_21_40 <- abind(pr_rcp26_21_25.array, pr_rcp26_26_30.array, pr_rcp26_31_35.array, pr_rcp26_36_40.array, along = 3)


registerDoParallel(cl <- makeCluster(3))
res <- foreach(j = seq_len(ncol(tasmin_rcp26_21_25.array))) %:% 
  foreach(i = seq_len(nrow(tasmin_rcp26_21_25.array))) %dopar% {
  ci_rcp26_21_25 <- climdex.pcic::climdexInput.raw(
      tmax=tasmax_rcp26_21_25.array[i,j,] - 273.15, 
      tmin=tasmin_rcp26_21_25.array[i,j,] - 273.15,
      prec=pr_rcp26_21_25.array[i,j,]*60*60*24*30,
      tmax.dates_21_25, tmin.dates_21_25, prec.dates_21_25, 
      base.range=c(1971,2000)
    )
  }
stopCluster(cl)

tasmax_rcp26_21_40[1,1,] <- tasmax_rcp26_21_40[1,1,] - 273.15
tasmin_rcp26_21_40[1,1,] <- tasmin_rcp26_21_40[1,1,] - 273.15
pr_rcp26_21_40[1,1,] <- pr_rcp26_21_40[1,1,]*60*60*24

ci_rcp26_21_40 <- climdexInput.raw(tmax = tasmax_rcp26_21_40[1,1,], tmin = tasmin_rcp26_21_40[1,1,] , prec = pr_rcp26_21_40[1,1,], tmax.dates_21_40, tmin.dates_21_40, prec.dates_21_40, n=5, northern.hemisphere=FALSE)
## Create a timeseries of annual SDII values.
wsdi_rcp26_21_40 <- climdex.wsdi(ci_rcp26_21_40)
















tmax.dates <- as.PCICt(do.call(paste, ec.1018935.tmax[,c("year",
                                                         "jday")]), format="%Y %j", cal="gregorian")
tmin.dates <- as.PCICt(do.call(paste, ec.1018935.tmin[,c("year",
                                                         "jday")]), format="%Y %j", cal="gregorian")
prec.dates <- as.PCICt(do.call(paste, ec.1018935.prec[,c("year",
                                                         "jday")]), format="%Y %j", cal="gregorian")
## Load the data in.
ci <- climdexInput.raw(ec.1018935.tmax$MAX_TEMP,
                       ec.1018935.tmin$MIN_TEMP, ec.1018935.prec$ONE_DAY_PRECIPITATION,
                       tmax.dates, tmin.dates, prec.dates, base.range=c(1971, 2000))
## Create a timeseries of annual SDII values.
sdii <- climdex.sdii(ci)