# 07 August 2025
rm(list=ls())
pacman::p_load(tidyverse)
setwd("~/Documents/OWUSS/experiences/Subelab ECIM")
list.files("Inputs")

dat <- read_csv("Inputs/las_cruces_detections.csv")
xy <- read_csv("Inputs/las_cruces_receiver_info.csv")
tag <- read_delim("Inputs/tag_info.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)

str(xy)
str(dat)
str(tag)

dat1 <- dat %>% 
  left_join(xy %>% 
      select(station_name, station_latitude, station_longitude, installation_name,
             substrate_general),
    by = "station_name") %>% 
  rename(latitude  = station_latitude,
         longitude = station_longitude,
         datetime = Date_Time,
         protection = installation_name,
         substrate = substrate_general) %>% 
  distinct() %>% 
  janitor::clean_names() %>% 
  dplyr::select(-1, -date_time_utc) %>%   
  filter(if_all(everything(), ~ !is.na(.))) # remove NAs and their entire row

tag1 <- janitor::clean_names(tag)

dat2 <- dat1 %>% 
  left_join(tag1 %>% 
              select(transmitter,sitio_captura, sitio_liberacion),
            by = "transmitter")

# structure ---------------------------------------------------------------

str(dat1)

dat3 <- dat2 %>% 
  mutate(datetime = dmy_hm(datetime),
         hour = hour(datetime),
         minute = minute(datetime)) %>%   
  filter(if_all(everything(), ~ !is.na(.))) %>%  # remove NAs and their entire row
  distinct(transmitter, minute, datetime, station_name, .keep_all = T)

sum(is.na(dat3))
str(dat3)

dat3$tag_id <- str_extract(dat3$transmitter, "\\d{4,5}$") #shorten tag ID strings

# plot it -----------------------------------------------------------------

datxy <- dat3 %>%
  group_by(station_name, latitude, longitude) %>%
  summarise(num_det = n(), .groups = 'drop')

datxy_sf <- sf::st_as_sf(datxy, coords = c("longitude",
                                           "latitude"), crs = 4326, agr = "constant")

mapview::mapview(datxy_sf, cex = "num_det", zcol = "station_name", fbg = FALSE)


# save it as an RDS 
write_rds(dat3, file= "Inputs/250807_sube1.rds")
