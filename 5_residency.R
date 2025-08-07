rm(list=ls())
pacman::p_load(tidyverse)
setwd("~/Documents/OWUSS/experiences/Subelab ECIM")
list.files("Inputs")
dat <- read_rds("Inputs/250807_sube1.rds")

# inputs ------------------------------------------------------------------

min_detections <- 1 # minimum detections per day to enter residency
min_res_period <- 2 # minimum duration threshold in days for 'residency' to occur
max_gap_secs <- 86400  # 1 day gap allowed between detections (in seconds: 86400 seconds in a day)

60*60*24 # 15 days in seconds

# residency ---------------------------------------------------------------

tic() # tic toc times functions 

residency <- dat %>%
  arrange(tag_id, station_name, datetime) %>% # location or receiver grouping
  group_by(tag_id, station_name) %>% # arrange by date, group by ID
  mutate(time_gap = as.numeric(difftime(datetime, # compute time gap between det 1 & 2
                                        lag(datetime, default = first(datetime)), # lag finds the second ping and grabs the time stamp
                                        units = "secs")), # calculate gap in seconds for filtering late
         new_event = ifelse(is.na(time_gap) | time_gap > max_gap_secs, 1, 0), #filter rows that have a gap larger than the one specified  
         event_id = cumsum(new_event)) %>% # number residency events 
  group_by(tag_id, station_name, event_id) %>% # re-group
  summarise(start_datetime = min(datetime), # start date
            end_datetime = max(datetime), # end date
            n_detections = n(), # total number of pings within the resideny event
            duration_days = round(as.numeric(difftime(max(datetime), # num of days 
                                                      min(datetime), units = "days")), 2), .groups = "drop",
            scientific_name = scientific_name) %>% # bring sex over                  
  dplyr::filter(n_detections >= min_detections, duration_days >= min_res_period) %>% # filter residency that is less than min days
  arrange(tag_id, start_datetime) # return a clean df 

toc() # 2.165 seconds to process 2.5 million rows
# we love vectorised functions 

residency
table(residency$location)
# save your beautiful work
write_rds(residency, "Inputs/250730_residency.rds")

