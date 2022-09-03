# This script retrieves the regional data for heat wave signals and plot the time series for 3 different scenarios

library(dplyr)
library(tidyverse)
library(ggplot2)
library(stats)


# Read in the regional heat wave signal data
setwd("~/Desktop/extreme-events-project-WesternCape/data/rcp26")
regional_HW_rcp26 <- readRDS('regional_HW_rcp26.Rds')

setwd("~/Desktop/extreme-events-project-WesternCape/data/rcp45")
regional_HW_rcp45 <- readRDS('regional_HW_rcp45.Rds')

setwd("~/Desktop/extreme-events-project-WesternCape/data/rcp85")
regional_HW_rcp85 <- readRDS('regional_HW_rcp85.Rds')

#-------------------HWA---------------------------
##------------------RCP 2.6-----------------------
fltr <- rep(1, times = 10)/10
hwa_CapeWinelands_rcp26 <- regional_HW_rcp26$HWA_Cape_Winelands_rcp26 %>%
  group_by(year) %>%
  summarise(mean_hwa_capewinelands = mean(hwa)) 

hwa_CapeWinelands_rcp26_trend <- as.data.frame(stats::filter(hwa_CapeWinelands_rcp26$mean_hwa_capewinelands, filter = fltr, method = "convo", 
                           sides = 2))

hwa_Central_Karoo_rcp26 <- regional_HW_rcp26$HWA_Central_Karoo_rcp26 %>%
  group_by(year) %>%
  summarise(mean_hwa_centralkaroo = mean(hwa))

hwa_Central_Karoo_rcp26_trend <- as.data.frame(stats::filter(hwa_Central_Karoo_rcp26$mean_hwa_centralkaroo, filter = fltr, method = "convo", 
                                               sides = 2))
hwa_Eden_rcp26 <- regional_HW_rcp26$HWA_Eden_rcp26%>%
  group_by(year) %>%
  summarise(mean_hwa_eden = mean(hwa))

hwa_Eden_rcp26_trend <- as.data.frame(stats::filter(hwa_Eden_rcp26$mean_hwa_eden, filter = fltr, method = "convo", 
                                              sides = 2))

hwa_Overberg_rcp26 <- regional_HW_rcp26$HWA_Overberg_rcp26%>%
  group_by(year) %>%
  summarise(mean_hwa_overberg = mean(hwa))

hwa_Overberg_rcp26_trend <- as.data.frame(stats::filter(hwa_Overberg_rcp26$mean_hwa_overberg, filter = fltr, method = "convo", 
                                                        sides = 2))
hwa_West_Coast_rcp26 <- regional_HW_rcp26$HWA_West_Coast_rcp26%>%
  group_by(year) %>%
  summarise(mean_hwa_westcoast = mean(hwa))

hwa_West_Coast_rcp26_trend <- as.data.frame(stats::filter(hwa_West_Coast_rcp26$mean_hwa_westcoast, filter = fltr, method = "convo", 
                                                          sides = 2))
hwa_City_Cape_Town_rcp26 <- regional_HW_rcp26$HWA_City_Cape_Town_rcp26%>%
  group_by(year) %>%
  summarise(mean_hwa_citycapetown = mean(hwa))

hwa_City_Cape_Town_rcp26_trend <- as.data.frame(stats::filter(hwa_City_Cape_Town_rcp26$mean_hwa_citycapetown, filter = fltr, method = "convo", 
                                                              sides = 2))
HWA_rcp26_trend_df <-  hwa_CapeWinelands_rcp26_trend %>% rename('Cape Winelands' = x) %>%
  cbind(hwa_Central_Karoo_rcp26_trend %>% rename('Central Karoo' = x)) %>%
  cbind(hwa_Eden_rcp26_trend %>% rename(Eden = x)) %>%
  cbind(hwa_Overberg_rcp26_trend %>% rename(Overberg = x)) %>%
  cbind(hwa_West_Coast_rcp26_trend %>% rename('West Coast' = x)) %>%
  cbind(hwa_City_Cape_Town_rcp26_trend %>% rename('City of Cape Town' = x)) %>%
  cbind(tibble(time = 1:80)) %>%
  pivot_longer(cols = !time, names_to = 'region', values_to = 'hwa_trend')
  
ggplot(HWA_rcp26_trend_df, aes(x = time, y = hwa_trend, group = region, color = region)) +
  geom_line(size = 1.5) + 
  ggtitle("Regional Average HWA (RCP 2.6)") + xlab("Year") + ylab("HWA (°C)") +
  theme_classic() + ylim(31, 45) +
  theme(plot.title = element_text(size=25, face="bold", hjust = 0.5), legend.title=element_text(size=13, face = 'bold'), 
        legend.text=element_text(size=13), axis.title.x = element_text(size=20, face="bold" ), axis.title.y = element_text(size=20, face="bold"),axis.text.y = element_text(size = 20), axis.text.x = element_text(size = 20))

##------------------RCP 4.5-----------------------
hwa_CapeWinelands_rcp45 <- regional_HW_rcp45$HWA_Cape_Winelands_rcp45 %>%
  group_by(year) %>%
  summarise(mean_hwa_capewinelands = mean(hwa))

hwa_CapeWinelands_rcp45_trend <- as.data.frame(stats::filter(hwa_CapeWinelands_rcp45$mean_hwa_capewinelands, filter = fltr, method = "convo", 
                                                             sides = 2))

hwa_Central_Karoo_rcp45 <- regional_HW_rcp45$HWA_Central_Karoo_rcp45 %>%
  group_by(year) %>%
  summarise(mean_hwa_centralkaroo = mean(hwa))

hwa_Central_Karoo_rcp45_trend <- as.data.frame(stats::filter(hwa_Central_Karoo_rcp45$mean_hwa_centralkaroo, filter = fltr, method = "convo", 
                                                             sides = 2))

hwa_Eden_rcp45 <- regional_HW_rcp45$HWA_Eden_rcp45%>%
  group_by(year) %>%
  summarise(mean_hwa_eden = mean(hwa))

hwa_Eden_rcp45_trend <- as.data.frame(stats::filter(hwa_Eden_rcp45$mean_hwa_eden, filter = fltr, method = "convo", 
                                                    sides = 2))

hwa_Overberg_rcp45 <- regional_HW_rcp45$HWA_Overberg_rcp45%>%
  group_by(year) %>%
  summarise(mean_hwa_overberg = mean(hwa))

hwa_Overberg_rcp45_trend <- as.data.frame(stats::filter(hwa_Overberg_rcp45$mean_hwa_overberg, filter = fltr, method = "convo", 
                                                        sides = 2))

hwa_West_Coast_rcp45 <- regional_HW_rcp45$HWA_West_Coast_rcp45%>%
  group_by(year) %>%
  summarise(mean_hwa_westcoast = mean(hwa))

hwa_West_Coast_rcp45_trend <- as.data.frame(stats::filter(hwa_West_Coast_rcp45$mean_hwa_westcoast, filter = fltr, method = "convo", 
                                                          sides = 2))

hwa_City_Cape_Town_rcp45 <- regional_HW_rcp45$HWA_City_Cape_Town_rcp45%>%
  group_by(year) %>%
  summarise(mean_hwa_citycapetown = mean(hwa))

hwa_City_Cape_Town_rcp45_trend <- as.data.frame(stats::filter(hwa_City_Cape_Town_rcp45$mean_hwa_citycapetown, filter = fltr, method = "convo", 
                                                              sides = 2))

HWA_rcp45_trend_df <-  hwa_CapeWinelands_rcp45_trend %>% rename('Cape Winelands' = x) %>%
  cbind(hwa_Central_Karoo_rcp45_trend %>% rename('Central Karoo' = x)) %>%
  cbind(hwa_Eden_rcp45_trend %>% rename(Eden = x)) %>%
  cbind(hwa_Overberg_rcp45_trend %>% rename(Overberg = x)) %>%
  cbind(hwa_West_Coast_rcp45_trend %>% rename('West Coast' = x)) %>%
  cbind(hwa_City_Cape_Town_rcp45_trend %>% rename('City of Cape Town' = x)) %>%
  cbind(tibble(time = 1:80)) %>%
  pivot_longer(cols = !time, names_to = 'region', values_to = 'hwa_trend')

ggplot(HWA_rcp45_trend_df, aes(x = time, y = hwa_trend, group = region, color = region)) +
  geom_line(size = 1.5) + 
  ggtitle("Regional Average HWA (RCP 4.5)") + xlab("Year") + ylab("HWA (°C)") +
  theme_classic() + ylim(31, 45) +
  theme(plot.title = element_text(size=25, face="bold", hjust = 0.5), legend.title=element_text(size=13, face = 'bold'), 
        legend.text=element_text(size=13), axis.title.x = element_text(size=20, face="bold" ), axis.title.y = element_text(size=20, face="bold"),axis.text.y = element_text(size = 20), axis.text.x = element_text(size = 20))

##------------------RCP 8.5-----------------------
hwa_CapeWinelands_rcp85 <- regional_HW_rcp85$HWA_Cape_Winelands_rcp85 %>%
  group_by(year) %>%
  summarise(mean_hwa_capewinelands = mean(hwa))

hwa_CapeWinelands_rcp85_trend <- as.data.frame(stats::filter(hwa_CapeWinelands_rcp85$mean_hwa_capewinelands, filter = fltr, method = "convo", 
                                                             sides = 2))

hwa_Central_Karoo_rcp85 <- regional_HW_rcp85$HWA_Central_Karoo_rcp85 %>%
  group_by(year) %>%
  summarise(mean_hwa_centralkaroo = mean(hwa))

hwa_Central_Karoo_rcp85_trend <- as.data.frame(stats::filter(hwa_Central_Karoo_rcp85$mean_hwa_centralkaroo, filter = fltr, method = "convo", 
                                                             sides = 2))

hwa_Eden_rcp85 <- regional_HW_rcp85$HWA_Eden_rcp85%>%
  group_by(year) %>%
  summarise(mean_hwa_eden = mean(hwa))

hwa_Eden_rcp85_trend <- as.data.frame(stats::filter(hwa_Eden_rcp85$mean_hwa_eden, filter = fltr, method = "convo", 
                                                    sides = 2))

hwa_Overberg_rcp85 <- regional_HW_rcp85$HWA_Overberg_rcp85%>%
  group_by(year) %>%
  summarise(mean_hwa_overberg = mean(hwa))

hwa_Overberg_rcp85_trend <- as.data.frame(stats::filter(hwa_Overberg_rcp85$mean_hwa_overberg, filter = fltr, method = "convo", 
                                                        sides = 2))

hwa_West_Coast_rcp85 <- regional_HW_rcp85$HWA_West_Coast_rcp85%>%
  group_by(year) %>%
  summarise(mean_hwa_westcoast = mean(hwa))

hwa_West_Coast_rcp85_trend <- as.data.frame(stats::filter(hwa_West_Coast_rcp85$mean_hwa_westcoast, filter = fltr, method = "convo", 
                                                          sides = 2))

hwa_City_Cape_Town_rcp85 <- regional_HW_rcp85$HWA_City_Cape_Town_rcp85%>%
  group_by(year) %>%
  summarise(mean_hwa_citycapetown = mean(hwa))

hwa_City_Cape_Town_rcp85_trend <- as.data.frame(stats::filter(hwa_City_Cape_Town_rcp85$mean_hwa_citycapetown, filter = fltr, method = "convo", 
                                                              sides = 2))

HWA_rcp85_trend_df <-  hwa_CapeWinelands_rcp85_trend %>% rename('Cape Winelands' = x) %>%
  cbind(hwa_Central_Karoo_rcp85_trend %>% rename('Central Karoo' = x)) %>%
  cbind(hwa_Eden_rcp85_trend %>% rename(Eden = x)) %>%
  cbind(hwa_Overberg_rcp85_trend %>% rename(Overberg = x)) %>%
  cbind(hwa_West_Coast_rcp85_trend %>% rename('West Coast' = x)) %>%
  cbind(hwa_City_Cape_Town_rcp85_trend %>% rename('City of Cape Town' = x)) %>%
  cbind(tibble(time = 1:80)) %>%
  pivot_longer(cols = !time, names_to = 'region', values_to = 'hwa_trend')

ggplot(HWA_rcp85_trend_df, aes(x = time, y = hwa_trend, group = region, color = region)) +
  geom_line(size = 1.5) + 
  ggtitle("Regional Average HWA (RCP 8.5)") + xlab("Year") + ylab("HWA (°C)") +
  theme_classic() + ylim(31, 45) +
  theme(plot.title = element_text(size=25, face="bold", hjust = 0.5), legend.title=element_text(size=13, face = 'bold'), 
        legend.text=element_text(size=13), axis.title.x = element_text(size=20, face="bold" ), axis.title.y = element_text(size=20, face="bold"),axis.text.y = element_text(size = 20), axis.text.x = element_text(size = 20))

#-------------------HWD---------------------------
##------------------RCP 2.6-----------------------
hwd_CapeWinelands_rcp26 <- regional_HW_rcp26$HWD_Cape_Winelands_rcp26 %>%
  group_by(year) %>%
  summarise(mean_hwd_capewinelands = mean(hwd)) 

hwd_CapeWinelands_rcp26_trend <- as.data.frame(stats::filter(hwd_CapeWinelands_rcp26$mean_hwd_capewinelands, filter = fltr, method = "convo", 
                                                             sides = 2))

hwd_Central_Karoo_rcp26 <- regional_HW_rcp26$HWD_Central_Karoo_rcp26 %>%
  group_by(year) %>%
  summarise(mean_hwd_centralkaroo = mean(hwd))

hwd_Central_Karoo_rcp26_trend <- as.data.frame(stats::filter(hwd_Central_Karoo_rcp26$mean_hwd_centralkaroo, filter = fltr, method = "convo", 
                                                             sides = 2))
hwd_Eden_rcp26 <- regional_HW_rcp26$HWD_Eden_rcp26%>%
  group_by(year) %>%
  summarise(mean_hwd_eden = mean(hwd))

hwd_Eden_rcp26_trend <- as.data.frame(stats::filter(hwd_Eden_rcp26$mean_hwd_eden, filter = fltr, method = "convo", 
                                                    sides = 2))

hwd_Overberg_rcp26 <- regional_HW_rcp26$HWD_Overberg_rcp26%>%
  group_by(year) %>%
  summarise(mean_hwd_overberg = mean(hwd))

hwd_Overberg_rcp26_trend <- as.data.frame(stats::filter(hwd_Overberg_rcp26$mean_hwd_overberg, filter = fltr, method = "convo", 
                                                        sides = 2))

hwd_West_Coast_rcp26 <- regional_HW_rcp26$HWD_West_Coast_rcp26%>%
  group_by(year) %>%
  summarise(mean_hwd_westcoast = mean(hwd))

hwd_West_Coast_rcp26_trend <- as.data.frame(stats::filter(hwd_West_Coast_rcp26$mean_hwd_westcoast, filter = fltr, method = "convo", 
                                                          sides = 2))

hwd_City_Cape_Town_rcp26 <- regional_HW_rcp26$HWD_City_Cape_Town_rcp26%>%
  group_by(year) %>%
  summarise(mean_hwd_citycapetown = mean(hwd))

hwd_City_Cape_Town_rcp26_trend <- as.data.frame(stats::filter(hwd_City_Cape_Town_rcp26$mean_hwd_citycapetown, filter = fltr, method = "convo", 
                                                              sides = 2))

HWD_rcp26_trend_df <-  hwd_CapeWinelands_rcp26_trend %>% rename('Cape Winelands' = x) %>%
  cbind(hwd_Central_Karoo_rcp26_trend %>% rename('Central Karoo' = x)) %>%
  cbind(hwd_Eden_rcp26_trend %>% rename(Eden = x)) %>%
  cbind(hwd_Overberg_rcp26_trend %>% rename(Overberg = x)) %>%
  cbind(hwd_West_Coast_rcp26_trend %>% rename('West Coast' = x)) %>%
  cbind(hwd_City_Cape_Town_rcp26_trend %>% rename('City of Cape Town' = x)) %>%
  cbind(tibble(time = 1:80)) %>%
  pivot_longer(cols = !time, names_to = 'region', values_to = 'hwd_trend')

ggplot(HWD_rcp26_trend_df, aes(x = time, y = hwd_trend, group = region, color = region)) +
  geom_line(size = 1.5) +  ylim(3.4, 20) +
  ggtitle("Regional Average HWD (RCP 2.6)") + xlab("Year") + ylab("HWD (days)") +
  theme_classic() + 
  theme(plot.title = element_text(size=25, face="bold", hjust = 0.5), legend.title=element_text(size=13, face = 'bold'), 
        legend.text=element_text(size=13), axis.title.x = element_text(size=20, face="bold" ), axis.title.y = element_text(size=20, face="bold"),axis.text.y = element_text(size = 20), axis.text.x = element_text(size = 20))
##------------------RCP 4.5-----------------------
hwd_CapeWinelands_rcp45 <- regional_HW_rcp45$HWD_Cape_Winelands_rcp45 %>%
  group_by(year) %>%
  summarise(mean_hwd_capewinelands = mean(hwd)) 

hwd_CapeWinelands_rcp45_trend <- as.data.frame(stats::filter(hwd_CapeWinelands_rcp45$mean_hwd_capewinelands, filter = fltr, method = "convo", 
                                                             sides = 2))

hwd_Central_Karoo_rcp45 <- regional_HW_rcp45$HWD_Central_Karoo_rcp45 %>%
  group_by(year) %>%
  summarise(mean_hwd_centralkaroo = mean(hwd))

hwd_Central_Karoo_rcp45_trend <- as.data.frame(stats::filter(hwd_Central_Karoo_rcp45$mean_hwd_centralkaroo, filter = fltr, method = "convo", 
                                                             sides = 2))
hwd_Eden_rcp45 <- regional_HW_rcp45$HWD_Eden_rcp45 %>%
  group_by(year) %>%
  summarise(mean_hwd_eden = mean(hwd))

hwd_Eden_rcp45_trend <- as.data.frame(stats::filter(hwd_Eden_rcp45$mean_hwd_eden, filter = fltr, method = "convo", 
                                                    sides = 2))

hwd_Overberg_rcp45 <- regional_HW_rcp45$HWD_Overberg_rcp45 %>%
  group_by(year) %>%
  summarise(mean_hwd_overberg = mean(hwd))

hwd_Overberg_rcp45_trend <- as.data.frame(stats::filter(hwd_Overberg_rcp45$mean_hwd_overberg, filter = fltr, method = "convo", 
                                                        sides = 2))

hwd_West_Coast_rcp45 <- regional_HW_rcp45$HWD_West_Coast_rcp45%>%
  group_by(year) %>%
  summarise(mean_hwd_westcoast = mean(hwd))

hwd_West_Coast_rcp45_trend <- as.data.frame(stats::filter(hwd_West_Coast_rcp45$mean_hwd_westcoast, filter = fltr, method = "convo", 
                                                          sides = 2))

hwd_City_Cape_Town_rcp45 <- regional_HW_rcp45$HWD_City_Cape_Town_rcp45%>%
  group_by(year) %>%
  summarise(mean_hwd_citycapetown = mean(hwd))

hwd_City_Cape_Town_rcp45_trend <- as.data.frame(stats::filter(hwd_City_Cape_Town_rcp45$mean_hwd_citycapetown, filter = fltr, method = "convo", 
                                                              sides = 2))

HWD_rcp45_trend_df <-  hwd_CapeWinelands_rcp45_trend %>% rename('Cape Winelands' = x) %>%
  cbind(hwd_Central_Karoo_rcp45_trend %>% rename('Central Karoo' = x)) %>%
  cbind(hwd_Eden_rcp45_trend %>% rename(Eden = x)) %>%
  cbind(hwd_Overberg_rcp45_trend %>% rename(Overberg = x)) %>%
  cbind(hwd_West_Coast_rcp45_trend %>% rename('West Coast' = x)) %>%
  cbind(hwd_City_Cape_Town_rcp45_trend %>% rename('City of Cape Town' = x)) %>%
  cbind(tibble(time = 1:80)) %>%
  pivot_longer(cols = !time, names_to = 'region', values_to = 'hwd_trend')

ggplot(HWD_rcp45_trend_df, aes(x = time, y = hwd_trend, group = region, color = region)) +
  geom_line(size = 1.5) + 
  ggtitle("Regional Average HWD (RCP 4.5)") + xlab("Year") + ylab("HWD (days)") +
  theme_classic() + ylim(3.4, 20) +
  theme(plot.title = element_text(size=25, face="bold", hjust = 0.5), legend.title=element_text(size=13, face = 'bold'), 
        legend.text=element_text(size=13), axis.title.x = element_text(size=20, face="bold" ), axis.title.y = element_text(size=20, face="bold"),axis.text.y = element_text(size = 20), axis.text.x = element_text(size = 20))
##------------------RCP 8.5-----------------------
hwd_CapeWinelands_rcp85 <- regional_HW_rcp85$HWD_Cape_Winelands_rcp85 %>%
  group_by(year) %>%
  summarise(mean_hwd_capewinelands = mean(hwd)) 

hwd_CapeWinelands_rcp85_trend <- as.data.frame(stats::filter(hwd_CapeWinelands_rcp85$mean_hwd_capewinelands, filter = fltr, method = "convo", 
                                                             sides = 2))

hwd_Central_Karoo_rcp85 <- regional_HW_rcp85$HWD_Central_Karoo_rcp85 %>%
  group_by(year) %>%
  summarise(mean_hwd_centralkaroo = mean(hwd))

hwd_Central_Karoo_rcp85_trend <- as.data.frame(stats::filter(hwd_Central_Karoo_rcp85$mean_hwd_centralkaroo, filter = fltr, method = "convo", 
                                                             sides = 2))
hwd_Eden_rcp85 <- regional_HW_rcp85$HWD_Eden_rcp85 %>%
  group_by(year) %>%
  summarise(mean_hwd_eden = mean(hwd))

hwd_Eden_rcp85_trend <- as.data.frame(stats::filter(hwd_Eden_rcp85$mean_hwd_eden, filter = fltr, method = "convo", 
                                                    sides = 2))

hwd_Overberg_rcp85 <- regional_HW_rcp85$HWD_Overberg_rcp85 %>%
  group_by(year) %>%
  summarise(mean_hwd_overberg = mean(hwd))

hwd_Overberg_rcp85_trend <- as.data.frame(stats::filter(hwd_Overberg_rcp85$mean_hwd_overberg, filter = fltr, method = "convo", 
                                                        sides = 2))

hwd_West_Coast_rcp85 <- regional_HW_rcp85$HWD_West_Coast_rcp85%>%
  group_by(year) %>%
  summarise(mean_hwd_westcoast = mean(hwd))

hwd_West_Coast_rcp85_trend <- as.data.frame(stats::filter(hwd_West_Coast_rcp85$mean_hwd_westcoast, filter = fltr, method = "convo", 
                                                          sides = 2))

hwd_City_Cape_Town_rcp85 <- regional_HW_rcp85$HWD_City_Cape_Town_rcp85%>%
  group_by(year) %>%
  summarise(mean_hwd_citycapetown = mean(hwd))

hwd_City_Cape_Town_rcp85_trend <- as.data.frame(stats::filter(hwd_City_Cape_Town_rcp85$mean_hwd_citycapetown, filter = fltr, method = "convo", 
                                                              sides = 2))

HWD_rcp85_trend_df <- hwd_CapeWinelands_rcp85_trend %>% rename('Cape Winelands' = x) %>%
  cbind(hwd_Central_Karoo_rcp85_trend %>% rename('Central Karoo' = x)) %>%
  cbind(hwd_Eden_rcp85_trend %>% rename(Eden = x)) %>%
  cbind(hwd_Overberg_rcp85_trend %>% rename(Overberg = x)) %>%
  cbind(hwd_West_Coast_rcp85_trend %>% rename('West Coast' = x)) %>%
  cbind(hwd_City_Cape_Town_rcp85_trend %>% rename('City of Cape Town' = x)) %>%
  cbind(tibble(time = 1:80)) %>%
  pivot_longer(cols = !time, names_to = 'region', values_to = 'hwd_trend')

ggplot(HWD_rcp85_trend_df, aes(x = time, y = hwd_trend, group = region, color = region)) +
  geom_line(size = 1.5) + ylim(3.4, 20) +
  ggtitle("Regional Average HWD (RCP 8.5)") + xlab("Year") + ylab("HWD (days)") +
  theme_classic() + 
  theme(plot.title = element_text(size=25, face="bold", hjust = 0.5), legend.title=element_text(size=13, face = 'bold'), 
        legend.text=element_text(size=13), axis.title.x = element_text(size=20, face="bold" ), axis.title.y = element_text(size=20, face="bold"),axis.text.y = element_text(size = 20), axis.text.x = element_text(size = 20))

#-------------------HWF---------------------------
##------------------RCP 2.6-----------------------
hwf_CapeWinelands_rcp26 <- regional_HW_rcp26$HWF_Cape_Winelands_rcp26 %>%
  group_by(year) %>%
  summarise(mean_hwf_capewinelands = mean(hwf))

hwf_CapeWinelands_rcp26_trend <- as.data.frame(stats::filter(hwf_CapeWinelands_rcp26$mean_hwf_capewinelands, filter = fltr, method = "convo", 
                                                             sides = 2))

hwf_Central_Karoo_rcp26 <- regional_HW_rcp26$HWF_Central_Karoo_rcp26 %>%
  group_by(year) %>%
  summarise(mean_hwf_centralkaroo = mean(hwf))

hwf_Central_Karoo_rcp26_trend <- as.data.frame(stats::filter(hwf_Central_Karoo_rcp26$mean_hwf_centralkaroo, filter = fltr, method = "convo", 
                                                             sides = 2))

hwf_Eden_rcp26 <- regional_HW_rcp26$HWF_Eden_rcp26%>%
  group_by(year) %>%
  summarise(mean_hwf_eden = mean(hwf))

hwf_Eden_rcp26_trend <- as.data.frame(stats::filter(hwf_Eden_rcp26$mean_hwf_eden, filter = fltr, method = "convo", 
                                                    sides = 2))

hwf_Overberg_rcp26 <- regional_HW_rcp26$HWF_Overberg_rcp26%>%
  group_by(year) %>%
  summarise(mean_hwf_overberg = mean(hwf))

hwf_Overberg_rcp26_trend <- as.data.frame(stats::filter(hwf_Overberg_rcp26$mean_hwf_overberg, filter = fltr, method = "convo", 
                                                        sides = 2))

hwf_West_Coast_rcp26 <- regional_HW_rcp26$HWF_West_Coast_rcp26%>%
  group_by(year) %>%
  summarise(mean_hwf_westcoast = mean(hwf))

hwf_West_Coast_rcp26_trend <- as.data.frame(stats::filter(hwf_West_Coast_rcp26$mean_hwf_westcoast, filter = fltr, method = "convo", 
                                                          sides = 2))

hwf_City_Cape_Town_rcp26 <- regional_HW_rcp26$HWF_City_Cape_Town_rcp26%>%
  group_by(year) %>%
  summarise(mean_hwf_citycapetown = mean(hwf))

hwf_City_Cape_Town_rcp26_trend <- as.data.frame(stats::filter(hwf_City_Cape_Town_rcp26$mean_hwf_citycapetown, filter = fltr, method = "convo", 
                                                              sides = 2))

HWF_rcp26_trend_df <-  hwf_CapeWinelands_rcp26_trend %>% rename('Cape Winelands' = x) %>%
  cbind(hwf_Central_Karoo_rcp26_trend %>% rename('Central Karoo' = x)) %>%
  cbind(hwf_Eden_rcp26_trend %>% rename(Eden = x)) %>%
  cbind(hwf_Overberg_rcp26_trend %>% rename(Overberg = x)) %>%
  cbind(hwf_West_Coast_rcp26_trend %>% rename('West Coast' = x)) %>%
  cbind(hwf_City_Cape_Town_rcp26_trend %>% rename('City of Cape Town' = x)) %>%
  cbind(tibble(time = 1:80)) %>%
  pivot_longer(cols = !time, names_to = 'region', values_to = 'hwf_trend')

ggplot(HWF_rcp26_trend_df, aes(x = time, y = hwf_trend, group = region, color = region)) +
  geom_line(size = 1.5) + 
  ggtitle("Regional Average HWF (RCP 2.6)") + xlab("Year") + ylab("HWF (days)") +
  theme_classic() + ylim(2.8, 74.2) +
  theme(plot.title = element_text(size=25, face="bold", hjust = 0.5), legend.title=element_text(size=13, face = 'bold'), 
        legend.text=element_text(size=13), axis.title.x = element_text(size=20, face="bold" ), axis.title.y = element_text(size=20, face="bold"),axis.text.y = element_text(size = 20))
##------------------RCP 4.5-----------------------
hwf_CapeWinelands_rcp45 <- regional_HW_rcp45$HWF_Cape_Winelands_rcp45 %>%
  group_by(year) %>%
  summarise(mean_hwf_capewinelands = mean(hwf))

hwf_CapeWinelands_rcp45_trend <- as.data.frame(stats::filter(hwf_CapeWinelands_rcp45$mean_hwf_capewinelands, filter = fltr, method = "convo", 
                                                             sides = 2))

hwf_Central_Karoo_rcp45 <- regional_HW_rcp45$HWF_Central_Karoo_rcp45 %>%
  group_by(year) %>%
  summarise(mean_hwf_centralkaroo = mean(hwf))

hwf_Central_Karoo_rcp45_trend <- as.data.frame(stats::filter(hwf_Central_Karoo_rcp45$mean_hwf_centralkaroo, filter = fltr, method = "convo", 
                                                         sides = 2))

hwf_Eden_rcp45 <- regional_HW_rcp45$HWF_Eden_rcp45%>%
  group_by(year) %>%
  summarise(mean_hwf_eden = mean(hwf))

hwf_Eden_rcp45_trend <- as.data.frame(stats::filter(hwf_Eden_rcp45$mean_hwf_eden, filter = fltr, method = "convo", 
                                                    sides = 2))

hwf_Overberg_rcp45 <- regional_HW_rcp45$HWF_Overberg_rcp45%>%
  group_by(year) %>%
  summarise(mean_hwf_overberg = mean(hwf))

hwf_Overberg_rcp45_trend <- as.data.frame(stats::filter(hwf_Overberg_rcp45$mean_hwf_overberg, filter = fltr, method = "convo", 
                                                        sides = 2))

hwf_West_Coast_rcp45 <- regional_HW_rcp45$HWF_West_Coast_rcp45%>%
  group_by(year) %>%
  summarise(mean_hwf_westcoast = mean(hwf))

hwf_West_Coast_rcp45_trend <- as.data.frame(stats::filter(hwf_West_Coast_rcp45$mean_hwf_westcoast, filter = fltr, method = "convo", 
                                                          sides = 2))

hwf_City_Cape_Town_rcp45 <- regional_HW_rcp45$HWF_City_Cape_Town_rcp45%>%
  group_by(year) %>%
  summarise(mean_hwf_citycapetown = mean(hwf))

hwf_City_Cape_Town_rcp45_trend <- as.data.frame(stats::filter(hwf_City_Cape_Town_rcp45$mean_hwf_citycapetown, filter = fltr, method = "convo", 
                                                              sides = 2))

HWF_rcp45_trend_df <-  hwf_CapeWinelands_rcp45_trend %>% rename('Cape Winelands' = x) %>%
  cbind(hwf_Central_Karoo_rcp45_trend %>% rename('Central Karoo' = x)) %>%
  cbind(hwf_Eden_rcp45_trend %>% rename(Eden = x)) %>%
  cbind(hwf_Overberg_rcp45_trend %>% rename(Overberg = x)) %>%
  cbind(hwf_West_Coast_rcp45_trend %>% rename('West Coast' = x)) %>%
  cbind(hwf_City_Cape_Town_rcp45_trend %>% rename('City of Cape Town' = x)) %>%
  cbind(tibble(time = 1:80)) %>%
  pivot_longer(cols = !time, names_to = 'region', values_to = 'hwf_trend')

ggplot(HWF_rcp45_trend_df, aes(x = time, y = hwf_trend, group = region, color = region)) +
  geom_line(size = 1.5) + 
  ggtitle("Regional Average HWF (RCP 4.5)") + xlab("Year") + ylab("HWF (days)") +
  theme_classic() + ylim(2.8, 74.2) +
  theme(plot.title = element_text(size=25, face="bold", hjust = 0.5), legend.title=element_text(size=13, face = 'bold'), 
        legend.text=element_text(size=13), axis.title.x = element_text(size=20, face="bold" ), axis.title.y = element_text(size=20, face="bold"),axis.text.y = element_text(size = 20))
##------------------RCP 8.5-----------------------
hwf_CapeWinelands_rcp85 <- regional_HW_rcp85$HWF_Cape_Winelands_rcp85 %>%
  group_by(year) %>%
  summarise(mean_hwf_capewinelands = mean(hwf))

hwf_CapeWinelands_rcp85_trend <- as.data.frame(stats::filter(hwf_CapeWinelands_rcp85$mean_hwf_capewinelands, filter = fltr, method = "convo", 
                                                             sides = 2))

hwf_Central_Karoo_rcp85 <- regional_HW_rcp85$HWF_Central_Karoo_rcp85 %>%
  group_by(year) %>%
  summarise(mean_hwf_centralkaroo = mean(hwf))

hwf_Central_Karoo_rcp85_trend <- as.data.frame(stats::filter(hwf_Central_Karoo_rcp85$mean_hwf_centralkaroo, filter = fltr, method = "convo", 
                                                             sides = 2))

hwf_Eden_rcp85 <- regional_HW_rcp85$HWF_Eden_rcp85%>%
  group_by(year) %>%
  summarise(mean_hwf_eden = mean(hwf))

hwf_Eden_rcp85_trend <- as.data.frame(stats::filter(hwf_Eden_rcp85$mean_hwf_eden, filter = fltr, method = "convo", 
                                                    sides = 2))

hwf_Overberg_rcp85 <- regional_HW_rcp85$HWF_Overberg_rcp85%>%
  group_by(year) %>%
  summarise(mean_hwf_overberg = mean(hwf))

hwf_Overberg_rcp85_trend <- as.data.frame(stats::filter(hwf_Overberg_rcp85$mean_hwf_overberg, filter = fltr, method = "convo", 
                                                        sides = 2))

hwf_West_Coast_rcp85 <- regional_HW_rcp85$HWF_West_Coast_rcp85%>%
  group_by(year) %>%
  summarise(mean_hwf_westcoast = mean(hwf))

hwf_West_Coast_rcp85_trend <- as.data.frame(stats::filter(hwf_West_Coast_rcp85$mean_hwf_westcoast, filter = fltr, method = "convo", 
                                                          sides = 2))

hwf_City_Cape_Town_rcp85 <- regional_HW_rcp85$HWF_City_Cape_Town_rcp85%>%
  group_by(year) %>%
  summarise(mean_hwf_citycapetown = mean(hwf))

hwf_City_Cape_Town_rcp85_trend <- as.data.frame(stats::filter(hwf_City_Cape_Town_rcp85$mean_hwf_citycapetown, filter = fltr, method = "convo", 
                                                              sides = 2))

HWF_rcp85_trend_df <-  hwf_CapeWinelands_rcp85_trend %>% rename('Cape Winelands' = x) %>%
  cbind(hwf_Central_Karoo_rcp85_trend %>% rename('Central Karoo' = x)) %>%
  cbind(hwf_Eden_rcp85_trend %>% rename(Eden = x)) %>%
  cbind(hwf_Overberg_rcp85_trend %>% rename(Overberg = x)) %>%
  cbind(hwf_West_Coast_rcp85_trend %>% rename('West Coast' = x)) %>%
  cbind(hwf_City_Cape_Town_rcp85_trend %>% rename('City of Cape Town' = x)) %>%
  cbind(tibble(time = 1:80)) %>%
  pivot_longer(cols = !time, names_to = 'region', values_to = 'hwf_trend')

ggplot(HWF_rcp85_trend_df, aes(x = time, y = hwf_trend, group = region, color = region)) +
  geom_line(size = 1.5) + ylim(2.8, 74.2) +
  ggtitle("Regional Average HWF (RCP 8.5)") + xlab("Year") + ylab("HWF (days)") +
  theme_classic() + ylim(2.8, 74.2) +
  theme(plot.title = element_text(size=25, face="bold", hjust = 0.5), legend.title=element_text(size=13, face = 'bold'), 
        legend.text=element_text(size=13), axis.title.x = element_text(size=20, face="bold" ), axis.title.y = element_text(size=20, face="bold"),axis.text.y = element_text(size = 20))

#-------------------HWM---------------------------
##------------------RCP 2.6-----------------------
hwm_CapeWinelands_rcp26 <- regional_HW_rcp26$HWM_Cape_Winelands_rcp26 %>%
  group_by(year) %>%
  summarise(mean_hwm_capewinelands = mean(hwm))

hwm_CapeWinelands_rcp26_trend <- as.data.frame(stats::filter(hwm_CapeWinelands_rcp26$mean_hwm_capewinelands, filter = fltr, method = "convo", 
                                                             sides = 2))

hwm_Central_Karoo_rcp26 <- regional_HW_rcp26$HWM_Central_Karoo_rcp26 %>%
  group_by(year) %>%
  summarise(mean_hwm_centralkaroo = mean(hwm))

hwm_Central_Karoo_rcp26_trend <- as.data.frame(stats::filter(hwm_Central_Karoo_rcp26$mean_hwm_centralkaroo, filter = fltr, method = "convo", 
                                                             sides = 2))

hwm_Eden_rcp26 <- regional_HW_rcp26$HWM_Eden_rcp26%>%
  group_by(year) %>%
  summarise(mean_hwm_eden = mean(hwm))

hwm_Eden_rcp26_trend <- as.data.frame(stats::filter(hwm_Eden_rcp26$mean_hwm_eden, filter = fltr, method = "convo", 
                                                    sides = 2))

hwm_Overberg_rcp26 <- regional_HW_rcp26$HWM_Overberg_rcp26%>%
  group_by(year) %>%
  summarise(mean_hwm_overberg = mean(hwm))

hwm_Overberg_rcp26_trend <- as.data.frame(stats::filter(hwm_Overberg_rcp26$mean_hwm_overberg, filter = fltr, method = "convo", 
                                                        sides = 2))

hwm_West_Coast_rcp26 <- regional_HW_rcp26$HWM_West_Coast_rcp26%>%
  group_by(year) %>%
  summarise(mean_hwm_westcoast = mean(hwm))

hwm_West_Coast_rcp26_trend <- as.data.frame(stats::filter(hwm_West_Coast_rcp26$mean_hwm_westcoast, filter = fltr, method = "convo", 
                                                          sides = 2))

hwm_City_Cape_Town_rcp26 <- regional_HW_rcp26$HWM_City_Cape_Town_rcp26%>%
  group_by(year) %>%
  summarise(mean_hwm_citycapetown = mean(hwm))

hwm_City_Cape_Town_rcp26_trend <- as.data.frame(stats::filter(hwm_City_Cape_Town_rcp26$mean_hwm_citycapetown, filter = fltr, method = "convo", 
                                                              sides = 2))

HWM_rcp26_trend_df <-  hwm_CapeWinelands_rcp26_trend %>% rename('Cape Winelands' = x) %>%
  cbind(hwm_Central_Karoo_rcp26_trend %>% rename('Central Karoo' = x)) %>%
  cbind(hwm_Eden_rcp26_trend %>% rename(Eden = x)) %>%
  cbind(hwm_Overberg_rcp26_trend %>% rename(Overberg = x)) %>%
  cbind(hwm_West_Coast_rcp26_trend %>% rename('West Coast' = x)) %>%
  cbind(hwm_City_Cape_Town_rcp26_trend %>% rename('City of Cape Town' = x)) %>%
  cbind(tibble(time = 1:80)) %>%
  pivot_longer(cols = !time, names_to = 'region', values_to = 'hwm_trend')


ggplot(HWM_rcp26_trend_df, aes(x = time, y = hwm_trend, group = region, color = region)) +
  geom_line(size = 1.5) + 
  ggtitle("Regional Average HWM (RCP 2.6)") + xlab("Year") + ylab("HWM (°C)") +
  theme_classic()  +  ylim(29, 39) +
  theme(plot.title = element_text(size=25, face="bold", hjust = 0.5), legend.title=element_text(size=13, face = 'bold'), 
        legend.text=element_text(size=13), axis.title.x = element_text(size=20, face="bold" ), axis.title.y = element_text(size=20, face="bold"),axis.text.y = element_text(size = 20))

##------------------RCP 4.5-----------------------
hwm_CapeWinelands_rcp45 <- regional_HW_rcp45$HWM_Cape_Winelands_rcp45 %>%
  group_by(year) %>%
  summarise(mean_hwm_capewinelands = mean(hwm))

hwm_CapeWinelands_rcp45_trend <- as.data.frame(stats::filter(hwm_CapeWinelands_rcp45$mean_hwm_capewinelands, filter = fltr, method = "convo", 
                                                             sides = 2))

hwm_Central_Karoo_rcp45 <- regional_HW_rcp45$HWM_Central_Karoo_rcp45 %>%
  group_by(year) %>%
  summarise(mean_hwm_centralkaroo = mean(hwm))

hwm_Central_Karoo_rcp45_trend <- as.data.frame(stats::filter(hwm_Central_Karoo_rcp45$mean_hwm_centralkaroo, filter = fltr, method = "convo", 
                                                             sides = 2))

hwm_Eden_rcp45 <- regional_HW_rcp45$HWM_Eden_rcp45%>%
  group_by(year) %>%
  summarise(mean_hwm_eden = mean(hwm))

hwm_Eden_rcp45_trend <- as.data.frame(stats::filter(hwm_Eden_rcp45$mean_hwm_eden, filter = fltr, method = "convo", 
                                                    sides = 2))

hwm_Overberg_rcp45 <- regional_HW_rcp45$HWM_Overberg_rcp45%>%
  group_by(year) %>%
  summarise(mean_hwm_overberg = mean(hwm))

hwm_Overberg_rcp45_trend <- as.data.frame(stats::filter(hwm_Overberg_rcp45$mean_hwm_overberg, filter = fltr, method = "convo", 
                                                        sides = 2))

hwm_West_Coast_rcp45 <- regional_HW_rcp45$HWM_West_Coast_rcp45%>%
  group_by(year) %>%
  summarise(mean_hwm_westcoast = mean(hwm))

hwm_West_Coast_rcp45_trend <- as.data.frame(stats::filter(hwm_West_Coast_rcp45$mean_hwm_westcoast, filter = fltr, method = "convo", 
                                                          sides = 2))

hwm_City_Cape_Town_rcp45 <- regional_HW_rcp45$HWM_City_Cape_Town_rcp45%>%
  group_by(year) %>%
  summarise(mean_hwm_citycapetown = mean(hwm))

hwm_City_Cape_Town_rcp45_trend <- as.data.frame(stats::filter(hwm_City_Cape_Town_rcp45$mean_hwm_citycapetown, filter = fltr, method = "convo", 
                                                              sides = 2))

HWM_rcp45_trend_df <-  hwm_CapeWinelands_rcp45_trend %>% rename('Cape Winelands' = x) %>%
  cbind(hwm_Central_Karoo_rcp45_trend %>% rename('Central Karoo' = x)) %>%
  cbind(hwm_Eden_rcp45_trend %>% rename(Eden = x)) %>%
  cbind(hwm_Overberg_rcp45_trend %>% rename(Overberg = x)) %>%
  cbind(hwm_West_Coast_rcp45_trend %>% rename('West Coast' = x)) %>%
  cbind(hwm_City_Cape_Town_rcp45_trend %>% rename('City of Cape Town' = x)) %>%
  cbind(tibble(time = 1:80)) %>%
  pivot_longer(cols = !time, names_to = 'region', values_to = 'hwm_trend')


ggplot(HWM_rcp45_trend_df, aes(x = time, y = hwm_trend, group = region, color = region)) +
  geom_line(size = 1.5) + 
  ggtitle("Regional Average HWM (RCP 4.5)") + xlab("Year") + ylab("HWM (°C)") +
  theme_classic()  +  ylim(29, 39) +
  theme(plot.title = element_text(size=25, face="bold", hjust = 0.5), legend.title=element_text(size=13, face = 'bold'), 
        legend.text=element_text(size=13), axis.title.x = element_text(size=20, face="bold" ), axis.title.y = element_text(size=20, face="bold"),axis.text.y = element_text(size = 20))

##------------------RCP 8.5-----------------------
hwm_CapeWinelands_rcp85 <- regional_HW_rcp85$HWM_Cape_Winelands_rcp85 %>%
  group_by(year) %>%
  summarise(mean_hwm_capewinelands = mean(hwm))

hwm_CapeWinelands_rcp85_trend <- as.data.frame(stats::filter(hwm_CapeWinelands_rcp85$mean_hwm_capewinelands, filter = fltr, method = "convo", 
                                                             sides = 2))

hwm_Central_Karoo_rcp85 <- regional_HW_rcp85$HWM_Central_Karoo_rcp85 %>%
  group_by(year) %>%
  summarise(mean_hwm_centralkaroo = mean(hwm))

hwm_Central_Karoo_rcp85_trend <- as.data.frame(stats::filter(hwm_Central_Karoo_rcp85$mean_hwm_centralkaroo, filter = fltr, method = "convo", 
                                                             sides = 2))

hwm_Eden_rcp85 <- regional_HW_rcp85$HWM_Eden_rcp85%>%
  group_by(year) %>%
  summarise(mean_hwm_eden = mean(hwm))

hwm_Eden_rcp85_trend <- as.data.frame(stats::filter(hwm_Eden_rcp85$mean_hwm_eden, filter = fltr, method = "convo", 
                                                    sides = 2))

hwm_Overberg_rcp85 <- regional_HW_rcp85$HWM_Overberg_rcp85%>%
  group_by(year) %>%
  summarise(mean_hwm_overberg = mean(hwm))

hwm_Overberg_rcp85_trend <- as.data.frame(stats::filter(hwm_Overberg_rcp85$mean_hwm_overberg, filter = fltr, method = "convo", 
                                                        sides = 2))

hwm_West_Coast_rcp85 <- regional_HW_rcp85$HWM_West_Coast_rcp85%>%
  group_by(year) %>%
  summarise(mean_hwm_westcoast = mean(hwm))

hwm_West_Coast_rcp85_trend <- as.data.frame(stats::filter(hwm_West_Coast_rcp85$mean_hwm_westcoast, filter = fltr, method = "convo", 
                                                          sides = 2))

hwm_City_Cape_Town_rcp85 <- regional_HW_rcp85$HWM_City_Cape_Town_rcp85%>%
  group_by(year) %>%
  summarise(mean_hwm_citycapetown = mean(hwm))

hwm_City_Cape_Town_rcp85_trend <- as.data.frame(stats::filter(hwm_City_Cape_Town_rcp85$mean_hwm_citycapetown, filter = fltr, method = "convo", 
                                                              sides = 2))

HWM_rcp85_trend_df <-  hwm_CapeWinelands_rcp85_trend %>% rename('Cape Winelands' = x) %>%
  cbind(hwm_Central_Karoo_rcp85_trend %>% rename('Central Karoo' = x)) %>%
  cbind(hwm_Eden_rcp85_trend %>% rename(Eden = x)) %>%
  cbind(hwm_Overberg_rcp85_trend %>% rename(Overberg = x)) %>%
  cbind(hwm_West_Coast_rcp85_trend %>% rename('West Coast' = x)) %>%
  cbind(hwm_City_Cape_Town_rcp85_trend %>% rename('City of Cape Town' = x)) %>%
  cbind(tibble(time = 1:80)) %>%
  pivot_longer(cols = !time, names_to = 'region', values_to = 'hwm_trend')


ggplot(HWM_rcp85_trend_df, aes(x = time, y = hwm_trend, group = region, color = region)) +
  geom_line(size = 1.5) + 
  ggtitle("Regional Average HWM (RCP 8.5)") + xlab("Year") + ylab("HWM (°C)") +
  theme_classic()  +  ylim(29, 39) +
  theme(plot.title = element_text(size=25, face="bold", hjust = 0.5), legend.title=element_text(size=13, face = 'bold'), 
        legend.text=element_text(size=13), axis.title.x = element_text(size=20, face="bold" ), axis.title.y = element_text(size=20, face="bold"),axis.text.y = element_text(size = 20))
#-------------------HWN---------------------------
##------------------RCP 2.6-----------------------
hwn_CapeWinelands_rcp26 <- regional_HW_rcp26$HWN_Cape_Winelands_rcp26 %>%
  group_by(year) %>%
  summarise(mean_hwn_capewinelands = mean(hwn))

hwn_CapeWinelands_rcp26_trend <- as.data.frame(stats::filter(hwn_CapeWinelands_rcp26$mean_hwn_capewinelands, filter = fltr, method = "convo", 
                                                             sides = 2))

hwn_Central_Karoo_rcp26 <- regional_HW_rcp26$HWN_Central_Karoo_rcp26 %>%
  group_by(year) %>%
  summarise(mean_hwn_centralkaroo = mean(hwn))

hwn_Central_Karoo_rcp26_trend <- as.data.frame(stats::filter(hwn_Central_Karoo_rcp26$mean_hwn_centralkaroo, filter = fltr, method = "convo", 
                                                             sides = 2))

hwn_Eden_rcp26 <- regional_HW_rcp26$HWN_Eden_rcp26%>%
  group_by(year) %>%
  summarise(mean_hwn_eden = mean(hwn))

hwn_Eden_rcp26_trend <- as.data.frame(stats::filter(hwn_Eden_rcp26$mean_hwn_eden, filter = fltr, method = "convo", 
                                                    sides = 2))

hwn_Overberg_rcp26 <- regional_HW_rcp26$HWN_Overberg_rcp26%>%
  group_by(year) %>%
  summarise(mean_hwn_overberg = mean(hwn))

hwn_Overberg_rcp26_trend <- as.data.frame(stats::filter(hwn_Overberg_rcp26$mean_hwn_overberg, filter = fltr, method = "convo", 
                                                        sides = 2))

hwn_West_Coast_rcp26 <- regional_HW_rcp26$HWN_West_Coast_rcp26%>%
  group_by(year) %>%
  summarise(mean_hwn_westcoast = mean(hwn))

hwn_West_Coast_rcp26_trend <- as.data.frame(stats::filter(hwn_West_Coast_rcp26$mean_hwn_westcoast, filter = fltr, method = "convo", 
                                                          sides = 2))

hwn_City_Cape_Town_rcp26 <- regional_HW_rcp26$HWN_City_Cape_Town_rcp26%>%
  group_by(year) %>%
  summarise(mean_hwn_citycapetown = mean(hwn))

hwn_City_Cape_Town_rcp26_trend <- as.data.frame(stats::filter(hwn_City_Cape_Town_rcp26$mean_hwn_citycapetown, filter = fltr, method = "convo", 
                                                              sides = 2))

HWN_rcp26_trend_df <-  hwn_CapeWinelands_rcp26_trend %>% rename('Cape Winelands' = x) %>%
  cbind(hwn_Central_Karoo_rcp26_trend %>% rename('Central Karoo' = x)) %>%
  cbind(hwn_Eden_rcp26_trend %>% rename(Eden = x)) %>%
  cbind(hwn_Overberg_rcp26_trend %>% rename(Overberg = x)) %>%
  cbind(hwn_West_Coast_rcp26_trend %>% rename('West Coast' = x)) %>%
  cbind(hwn_City_Cape_Town_rcp26_trend %>% rename('City of Cape Town' = x)) %>%
  cbind(tibble(time = 1:80)) %>%
  pivot_longer(cols = !time, names_to = 'region', values_to = 'hwn_trend')

ggplot(HWN_rcp26_trend_df, aes(x = time, y = hwn_trend, group = region, color = region)) +
  geom_line(size = 1.5) + 
  ggtitle("Regional Average HWN (RCP 2.6)") + xlab("Year") + ylab("HWN (events)") +
  theme_classic()  +  ylim(0.8, 12.63) +
  theme(plot.title = element_text(size=25, face="bold", hjust = 0.5), legend.title=element_text(size=13, face = 'bold'), 
        legend.text=element_text(size=13), axis.title.x = element_text(size=20, face="bold" ), axis.title.y = element_text(size=20, face="bold"),axis.text.y = element_text(size = 20))
##------------------RCP 4.5-----------------------
hwn_CapeWinelands_rcp45 <- regional_HW_rcp45$HWN_Cape_Winelands_rcp45 %>%
  group_by(year) %>%
  summarise(mean_hwn_capewinelands = mean(hwn))

hwn_CapeWinelands_rcp45_trend <- as.data.frame(stats::filter(hwn_CapeWinelands_rcp45$mean_hwn_capewinelands, filter = fltr, method = "convo", 
                                                             sides = 2))

hwn_Central_Karoo_rcp45 <- regional_HW_rcp45$HWN_Central_Karoo_rcp45 %>%
  group_by(year) %>%
  summarise(mean_hwn_centralkaroo = mean(hwn))

hwn_Central_Karoo_rcp45_trend <- as.data.frame(stats::filter(hwn_Central_Karoo_rcp45$mean_hwn_centralkaroo, filter = fltr, method = "convo", 
                                                             sides = 2))

hwn_Eden_rcp45 <- regional_HW_rcp45$HWN_Eden_rcp45%>%
  group_by(year) %>%
  summarise(mean_hwn_eden = mean(hwn))

hwn_Eden_rcp45_trend <- as.data.frame(stats::filter(hwn_Eden_rcp45$mean_hwn_eden, filter = fltr, method = "convo", 
                                                    sides = 2))

hwn_Overberg_rcp45 <- regional_HW_rcp45$HWN_Overberg_rcp45%>%
  group_by(year) %>%
  summarise(mean_hwn_overberg = mean(hwn))

hwn_Overberg_rcp45_trend <- as.data.frame(stats::filter(hwn_Overberg_rcp45$mean_hwn_overberg, filter = fltr, method = "convo", 
                                                        sides = 2))

hwn_West_Coast_rcp45 <- regional_HW_rcp45$HWN_West_Coast_rcp45%>%
  group_by(year) %>%
  summarise(mean_hwn_westcoast = mean(hwn))

hwn_West_Coast_rcp45_trend <- as.data.frame(stats::filter(hwn_West_Coast_rcp45$mean_hwn_westcoast, filter = fltr, method = "convo", 
                                                          sides = 2))

hwn_City_Cape_Town_rcp45 <- regional_HW_rcp45$HWN_City_Cape_Town_rcp45%>%
  group_by(year) %>%
  summarise(mean_hwn_citycapetown = mean(hwn))

hwn_City_Cape_Town_rcp45_trend <- as.data.frame(stats::filter(hwn_City_Cape_Town_rcp45$mean_hwn_citycapetown, filter = fltr, method = "convo", 
                                                              sides = 2))

HWN_rcp45_trend_df <-  hwn_CapeWinelands_rcp45_trend %>% rename('Cape Winelands' = x) %>%
  cbind(hwn_Central_Karoo_rcp45_trend %>% rename('Central Karoo' = x)) %>%
  cbind(hwn_Eden_rcp45_trend %>% rename(Eden = x)) %>%
  cbind(hwn_Overberg_rcp45_trend %>% rename(Overberg = x)) %>%
  cbind(hwn_West_Coast_rcp45_trend %>% rename('West Coast' = x)) %>%
  cbind(hwn_City_Cape_Town_rcp45_trend %>% rename('City of Cape Town' = x)) %>%
  cbind(tibble(time = 1:80)) %>%
  pivot_longer(cols = !time, names_to = 'region', values_to = 'hwn_trend')

ggplot(HWN_rcp45_trend_df, aes(x = time, y = hwn_trend, group = region, color = region)) +
  geom_line(size = 1.5) + 
  ggtitle("Regional Average HWN (RCP 4.5)") + xlab("Year") + ylab("HWN (events)") +
  theme_classic()  + ylim(0.8, 12.63) +
  theme(plot.title = element_text(size=25, face="bold", hjust = 0.5), legend.title=element_text(size=13, face = 'bold'), 
        legend.text=element_text(size=13), axis.title.x = element_text(size=20, face="bold" ), axis.title.y = element_text(size=20, face="bold"),axis.text.y = element_text(size = 20))


##------------------RCP 8.5-----------------------

hwn_CapeWinelands_rcp85 <- regional_HW_rcp85$HWN_Cape_Winelands_rcp85 %>%
  group_by(year) %>%
  summarise(mean_hwn_capewinelands = mean(hwn))

hwn_CapeWinelands_rcp85_trend <- as.data.frame(stats::filter(hwn_CapeWinelands_rcp85$mean_hwn_capewinelands, filter = fltr, method = "convo", 
                                                             sides = 2))

hwn_Central_Karoo_rcp85 <- regional_HW_rcp85$HWN_Central_Karoo_rcp85 %>%
  group_by(year) %>%
  summarise(mean_hwn_centralkaroo = mean(hwn))

hwn_Central_Karoo_rcp85_trend <- as.data.frame(stats::filter(hwn_Central_Karoo_rcp85$mean_hwn_centralkaroo, filter = fltr, method = "convo", 
                                                             sides = 2))

hwn_Eden_rcp85 <- regional_HW_rcp85$HWN_Eden_rcp85%>%
  group_by(year) %>%
  summarise(mean_hwn_eden = mean(hwn))

hwn_Eden_rcp85_trend <- as.data.frame(stats::filter(hwn_Eden_rcp85$mean_hwn_eden, filter = fltr, method = "convo", 
                                                    sides = 2))

hwn_Overberg_rcp85 <- regional_HW_rcp85$HWN_Overberg_rcp85%>%
  group_by(year) %>%
  summarise(mean_hwn_overberg = mean(hwn))

hwn_Overberg_rcp85_trend <- as.data.frame(stats::filter(hwn_Overberg_rcp85$mean_hwn_overberg, filter = fltr, method = "convo", 
                                                        sides = 2))

hwn_West_Coast_rcp85 <- regional_HW_rcp85$HWN_West_Coast_rcp85%>%
  group_by(year) %>%
  summarise(mean_hwn_westcoast = mean(hwn))

hwn_West_Coast_rcp85_trend <- as.data.frame(stats::filter(hwn_West_Coast_rcp85$mean_hwn_westcoast, filter = fltr, method = "convo", 
                                                          sides = 2))

hwn_City_Cape_Town_rcp85 <- regional_HW_rcp85$HWN_City_Cape_Town_rcp85%>%
  group_by(year) %>%
  summarise(mean_hwn_citycapetown = mean(hwn))

hwn_City_Cape_Town_rcp85_trend <- as.data.frame(stats::filter(hwn_City_Cape_Town_rcp85$mean_hwn_citycapetown, filter = fltr, method = "convo", 
                                                              sides = 2))

HWN_rcp85_trend_df <-  hwn_CapeWinelands_rcp85_trend %>% rename('Cape Winelands' = x) %>%
  cbind(hwn_Central_Karoo_rcp85_trend %>% rename('Central Karoo' = x)) %>%
  cbind(hwn_Eden_rcp85_trend %>% rename(Eden = x)) %>%
  cbind(hwn_Overberg_rcp85_trend %>% rename(Overberg = x)) %>%
  cbind(hwn_West_Coast_rcp85_trend %>% rename('West Coast' = x)) %>%
  cbind(hwn_City_Cape_Town_rcp85_trend %>% rename('City of Cape Town' = x)) %>%
  cbind(tibble(time = 1:80)) %>%
  pivot_longer(cols = !time, names_to = 'region', values_to = 'hwn_trend')

ggplot(HWN_rcp85_trend_df, aes(x = time, y = hwn_trend, group = region, color = region)) +
  geom_line(size = 1.5) + 
  ggtitle("Regional Average HWN (RCP 8.5)") + xlab("Year") + ylab("HWN (events)") +
  theme_classic()  +  ylim(0.8, 12.63) +
  theme(plot.title = element_text(size=25, face="bold", hjust = 0.5), legend.title=element_text(size=13, face = 'bold'), 
        legend.text=element_text(size=13), axis.title.x = element_text(size=20, face="bold" ), axis.title.y = element_text(size=20, face="bold"),axis.text.y = element_text(size = 20))
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
write_rds(list(HWA_rcp26_df = HWA_rcp26_df, HWD_rcp26_df = HWD_rcp26_df, HWF_rcp26_df = HWF_rcp26_df,
               HWM_rcp26_df = HWM_rcp26_df, HWN_rcp26_df = HWN_rcp26_df, HWA_rcp45_df = HWA_rcp45_df, HWD_rcp45_df = HWD_rcp45_df, HWF_rcp45_df = HWF_rcp45_df,
               HWM_rcp45_df = HWM_rcp45_df, HWN_rcp45_df = HWN_rcp45_df, HWA_rcp85_df = HWA_rcp85_df, HWD_rcp85_df = HWD_rcp85_df, HWF_rcp85_df = HWF_rcp85_df,
               HWM_rcp85_df = HWM_rcp85_df, HWN_rcp85_df =HWN_rcp85_df),
          path = file.path(data_dir ="/Users/hebowen/Desktop/extreme-events-project-WesternCape/data", "regional_HW_ts.Rds"))


write_rds(list(HWA_rcp26_trend_df = HWA_rcp26_trend_df, HWD_rcp26_trend_df = HWD_rcp26_trend_df, HWF_rcp26_trend_df = HWF_rcp26_trend_df,
               HWM_rcp26_trend_df = HWM_rcp26_trend_df, HWN_rcp26_trend_df = HWN_rcp26_trend_df, HWA_rcp45_trend_df = HWA_rcp45_trend_df, HWD_rcp45_trend_df = HWD_rcp45_trend_df, HWF_rcp45_trend_df = HWF_rcp45_trend_df,
               HWM_rcp45_trend_df = HWM_rcp45_trend_df, HWN_rcp45_trend_df = HWN_rcp45_trend_df, HWA_rcp85_trend_df = HWA_rcp85_trend_df, HWD_rcp85_trend_df = HWD_rcp85_trend_df, HWF_rcp85_trend_df = HWF_rcp85_trend_df,
               HWM_rcp85_trend_df = HWM_rcp85_trend_df, HWN_rcp85_trend_df = HWN_rcp85_trend_df),
          path = file.path(data_dir ="/Users/hebowen/Desktop/extreme-events-project-WesternCape/data", "regional_HW_trend_ts.Rds"))
#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
## Plot all the HW index trend time series of 3 scenarios in panels
grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, nrow = 5)

p1 <- ggplot(HWA_rcp26_trend_df, aes(x = time, y = hwa_trend, group = region, color = region)) +
  geom_line(size = 1.5) + scale_color_manual(values=c("red","blue", "brown", "black", "green", "yellow")) +
  xlab("Year") + ylab("HWA (°C)") +
  theme_classic() + ylim(31, 45) +
  theme(legend.title=element_text(size=13, face = 'bold'), legend.text=element_text(size=13), axis.title.x = element_text(size=20, face="bold" ), axis.title.y = element_text(size=20, face="bold"), axis.text.y = element_text(size = 20), axis.text.x = element_text(size = 20))

p2 <- ggplot(HWA_rcp45_trend_df, aes(x = time, y = hwa_trend, group = region, color = region)) +
  geom_line(size = 1.5, show.legend = FALSE) + scale_color_manual(values=c("red","blue", "brown", "black", "green", "yellow")) +
  xlab("Year") + ylab("HWA (°C)") +
  theme_classic() + ylim(31, 45) +
  theme(axis.title.x = element_text(size=20, face="bold" ), axis.title.y = element_text(size=20, face="bold"),axis.text.y = element_text(size = 20), axis.text.x = element_text(size = 20))


p3 <- ggplot(HWA_rcp85_trend_df, aes(x = time, y = hwa_trend, group = region, color = region)) +
  geom_line(size = 1.5, show.legend = FALSE) + scale_color_manual(values=c("red","blue", "brown", "black", "green", "yellow")) +
  xlab("Year") + ylab("HWA (°C)") +
  theme_classic() + ylim(31, 45) +
  theme(plot.title = element_text(size=25, face="bold", hjust = 0.5), legend.title=element_text(size=13, face = 'bold'), 
        legend.text=element_text(size=13), axis.title.x = element_text(size=20, face="bold" ), axis.title.y = element_text(size=20, face="bold"),axis.text.y = element_text(size = 20), axis.text.x = element_text(size = 20))


p4 <- ggplot(HWD_rcp26_trend_df, aes(x = time, y = hwd_trend, group = region, color = region)) +
  geom_line(size = 1.5, show.legend = FALSE) +  ylim(3.4, 20) +scale_color_manual(values=c("red","blue", "brown", "black", "green", "yellow")) +
  xlab("Year") + ylab("HWD (days)") +
  theme_classic() + 
  theme(plot.title = element_text(size=25, face="bold", hjust = 0.5), legend.title=element_text(size=13, face = 'bold'), 
        legend.text=element_text(size=13), axis.title.x = element_text(size=20, face="bold" ), axis.title.y = element_text(size=20, face="bold"),axis.text.y = element_text(size = 20), axis.text.x = element_text(size = 20))

p5 <- ggplot(HWD_rcp45_trend_df, aes(x = time, y = hwd_trend, group = region, color = region)) +
  geom_line(size = 1.5, show.legend = FALSE) + scale_color_manual(values=c("red","blue", "brown", "black", "green", "yellow")) +
  xlab("Year") + ylab("HWD (days)") +
  theme_classic() + ylim(3.4, 20) +
  theme(plot.title = element_text(size=25, face="bold", hjust = 0.5), legend.title=element_text(size=13, face = 'bold'), 
        legend.text=element_text(size=13), axis.title.x = element_text(size=20, face="bold" ), axis.title.y = element_text(size=20, face="bold"),axis.text.y = element_text(size = 20), axis.text.x = element_text(size = 20))

p6 <- ggplot(HWD_rcp85_trend_df, aes(x = time, y = hwd_trend, group = region, color = region)) +
  geom_line(size = 1.5, show.legend = FALSE) + ylim(3.4, 20) +scale_color_manual(values=c("red","blue", "brown", "black", "green", "yellow")) +
   xlab("Year") + ylab("HWD (days)") +
  theme_classic() + 
  theme(plot.title = element_text(size=25, face="bold", hjust = 0.5), legend.title=element_text(size=13, face = 'bold'), 
        legend.text=element_text(size=13), axis.title.x = element_text(size=20, face="bold" ), axis.title.y = element_text(size=20, face="bold"),axis.text.y = element_text(size = 20), axis.text.x = element_text(size = 20))

p7 <- ggplot(HWF_rcp26_trend_df, aes(x = time, y = hwf_trend, group = region, color = region)) +
  geom_line(size = 1.5, show.legend = FALSE) + scale_color_manual(values=c("red","blue", "brown", "black", "green", "yellow")) +
  xlab("Year") + ylab("HWF (days)") +
  theme_classic() + ylim(2.8, 74.2) +
  theme(plot.title = element_text(size=25, face="bold", hjust = 0.5), legend.title=element_text(size=13, face = 'bold'), 
        legend.text=element_text(size=13), axis.title.x = element_text(size=20, face="bold" ), axis.title.y = element_text(size=20, face="bold"),axis.text.y = element_text(size = 20), axis.text.x = element_text(size = 20))

p8 <- ggplot(HWF_rcp45_trend_df, aes(x = time, y = hwf_trend, group = region, color = region)) +
  geom_line(size = 1.5, show.legend = FALSE) + scale_color_manual(values=c("red","blue", "brown", "black", "green", "yellow")) +
  xlab("Year") + ylab("HWF (days)") +
  theme_classic() + ylim(2.8, 74.2) +
  theme(plot.title = element_text(size=25, face="bold", hjust = 0.5), legend.title=element_text(size=13, face = 'bold'), 
        legend.text=element_text(size=13), axis.title.x = element_text(size=20, face="bold" ), axis.title.y = element_text(size=20, face="bold"),axis.text.y = element_text(size = 20), axis.text.x = element_text(size = 20))

p9 <- ggplot(HWF_rcp85_trend_df, aes(x = time, y = hwf_trend, group = region, color = region)) +
  geom_line(size = 1.5, show.legend = FALSE) + ylim(2.8, 74.2) +scale_color_manual(values=c("red","blue", "brown", "black", "green", "yellow")) +
  xlab("Year") + ylab("HWF (days)") +
  theme_classic() + ylim(2.8, 74.2) +
  theme(plot.title = element_text(size=25, face="bold", hjust = 0.5), legend.title=element_text(size=13, face = 'bold'), 
        legend.text=element_text(size=13), axis.title.x = element_text(size=20, face="bold" ), axis.title.y = element_text(size=20, face="bold"),axis.text.y = element_text(size = 20), axis.text.x = element_text(size = 20))


p10 <- ggplot(HWM_rcp26_trend_df, aes(x = time, y = hwm_trend, group = region, color = region)) +
  geom_line(size = 1.5, show.legend = FALSE) + scale_color_manual(values=c("red","blue", "brown", "black", "green", "yellow")) +
  xlab("Year") + ylab("HWM (°C)") +
  theme_classic()  +  ylim(29, 39) +
  theme(plot.title = element_text(size=25, face="bold", hjust = 0.5), legend.title=element_text(size=13, face = 'bold'), 
        legend.text=element_text(size=13), axis.title.x = element_text(size=20, face="bold" ), axis.title.y = element_text(size=20, face="bold"),axis.text.y = element_text(size = 20), axis.text.x = element_text(size = 20))
  

p11 <- ggplot(HWM_rcp45_trend_df, aes(x = time, y = hwm_trend, group = region, color = region)) +
  geom_line(size = 1.5,  show.legend = FALSE) + scale_color_manual(values=c("red","blue", "brown", "black", "green", "yellow")) +
  xlab("Year") + ylab("HWM (°C)") +
  theme_classic()  +  ylim(29, 39) +
  theme(plot.title = element_text(size=25, face="bold", hjust = 0.5), legend.title=element_text(size=13, face = 'bold'), 
        legend.text=element_text(size=13), axis.title.x = element_text(size=20, face="bold" ), axis.title.y = element_text(size=20, face="bold"),axis.text.y = element_text(size = 20), axis.text.x = element_text(size = 20))
  
p12 <- ggplot(HWM_rcp85_trend_df, aes(x = time, y = hwm_trend, group = region, color = region)) +
  geom_line(size = 1.5, show.legend = FALSE) + scale_color_manual(values=c("red","blue", "brown", "black", "green", "yellow")) +
  xlab("Year") + ylab("HWM (°C)") +
  theme_classic()  +  ylim(29, 39) +
  theme(plot.title = element_text(size=25, face="bold", hjust = 0.5), legend.title=element_text(size=13, face = 'bold'), 
        legend.text=element_text(size=13), axis.title.x = element_text(size=20, face="bold" ), axis.title.y = element_text(size=20, face="bold"),axis.text.y = element_text(size = 20), axis.text.x = element_text(size = 20))
  
p13 <-  ggplot(HWN_rcp26_trend_df, aes(x = time, y = hwn_trend, group = region, color = region)) +
  geom_line(size = 1.5, show.legend = FALSE) + scale_color_manual(values=c("red","blue", "brown", "black", "green", "yellow")) +
  xlab("Year") + ylab("HWN (events)") +
  theme_classic()  +  ylim(0.8, 12.63) +
  theme(plot.title = element_text(size=25, face="bold", hjust = 0.5), legend.title=element_text(size=13, face = 'bold'), 
        legend.text=element_text(size=13), axis.title.x = element_text(size=20, face="bold" ), axis.title.y = element_text(size=20, face="bold"),axis.text.y = element_text(size = 20), axis.text.x = element_text(size = 20))
  
p14 <-  ggplot(HWN_rcp45_trend_df, aes(x = time, y = hwn_trend, group = region, color = region)) +
  geom_line(size = 1.5, show.legend = FALSE) + scale_color_manual(values=c("red","blue", "brown", "black", "green", "yellow")) +
  xlab("Year") + ylab("HWN (events)") +
  theme_classic()  + ylim(0.8, 12.63) +
  theme(plot.title = element_text(size=25, face="bold", hjust = 0.5), legend.title=element_text(size=13, face = 'bold'), 
        legend.text=element_text(size=13), axis.title.x = element_text(size=20, face="bold" ), axis.title.y = element_text(size=20, face="bold"),axis.text.y = element_text(size = 20), axis.text.x = element_text(size = 20))
  
p15 <- ggplot(HWN_rcp85_trend_df, aes(x = time, y = hwn_trend, group = region, color = region)) +
  geom_line(size = 1.5, show.legend = FALSE) + scale_color_manual(values=c("red","blue", "brown", "black", "green", "yellow")) +
  xlab("Year") + ylab("HWN (events)") +
  theme_classic()  +  ylim(0.8, 12.63) +
  theme(plot.title = element_text(size=25, face="bold", hjust = 0.5), legend.title=element_text(size=13, face = 'bold'), 
        legend.text=element_text(size=13), axis.title.x = element_text(size=20, face="bold" ), axis.title.y = element_text(size=20, face="bold"),axis.text.y = element_text(size = 20), axis.text.x = element_text(size = 20))
  
legend <- cowplot::get_legend(p1)

grid.newpage()
grid.draw(legend)
  





