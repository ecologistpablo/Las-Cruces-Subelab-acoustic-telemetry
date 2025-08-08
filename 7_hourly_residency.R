
rm(list=ls())
pacman::p_load(tidyverse, purrr, furrr)
setwd("~/Documents/OWUSS/experiences/Subelab ECIM")
list.files("Inputs")
dat <- read_rds("Inputs/250807_sube1.rds")

# params ---------------------------------------------------------------

min_detections_per_hour <- 2   # detections required in each hour bin
min_res_hours <- 1   # minimum event duration (hours)
max_gap_secs <- 3600  # 1 hour timeout between sequential detections

# hourly residency -----------------------------------------------------

summary(dat1$datetime)
# 1) Identify continuous events by 1h timeout
dat_events <- dat %>%
  arrange(tag_id, protection, datetime) %>%
  group_by(tag_id, protection) %>%
  mutate(gap_secs = as.numeric(difftime(datetime, lag(datetime), units = "secs")),
         new_event = if_else(is.na(gap_secs) | gap_secs > max_gap_secs, 1L, 0L),
         event_id = cumsum(new_event)) %>% # residency event
  ungroup()

# 2) Bin detections to the hour (for the "2 per hour" rule)
det_hourly <- dat_events %>%
  mutate(hour_bin = lubridate::floor_date(datetime, "hour")) %>%
  group_by(tag_id, scientific_name, protection, event_id, hour_bin) %>%
  summarise(dets_in_hour = n(),hour_start = min(datetime),
            hour_end = max(datetime), .groups = "drop")

# 3) Collapse to event-level metrics
residency <- det_hourly %>%
  group_by(tag_id, scientific_name, protection, event_id) %>%
  summarise(start_datetime = min(hour_start),
    end_datetime = max(hour_end),
    n_detections = sum(dets_in_hour),
    duration_hours= as.numeric(difftime(max(hour_end), min(hour_start), units = "hours")),
    hours_covered = n(),                        # number of hour bins
    min_dets_per_hour  = min(dets_in_hour),         # strict per-hour threshold
    .groups = "drop") %>%
  filter(duration_hours >= min_res_hours,
         min_dets_per_hour >= min_detections_per_hour) %>%
  arrange(tag_id, start_datetime)

residency
# write_rds(residency, "Inputs/250807_residency_hourly.rds")

# optional: table for roseplots (one row per *resident hour*)
# keeps only hour bins that met the >= 2 detections criterion
residency_hours <- det_hourly %>%
  filter(dets_in_hour >= min_detections_per_hour) %>%
  mutate(hour_of_day = lubridate::hour(hour_bin)) %>%  # 0..23 for polar plots
  select(tag_id, scientific_name, protection,
         event_id, hour_bin, hour_of_day, dets_in_hour)

# example roseplot (per protection, coloured by species)
a <- ggplot(residency_hours, aes(x = hour_of_day, fill = scientific_name)) +
  geom_histogram(breaks = seq(0, 24, by = 1), colour = "grey20", size = 0.2) +
  coord_polar(start = 0) +
  facet_grid(scientific_name~protection, scales = "free_y") +
  scale_x_continuous(limits = c(0, 24),breaks = seq(0, 24, by = 3),
                     labels = sprintf("%02d:00", seq(0, 24, by = 3))) +
  labs(x = NULL, y = NULL, fill = "Species") +
  theme_bw() +
  scale_fill_viridis_d()
a


ggsave("Outputs/250808_residency_sp_hourly.pdf",
       plot = a, width = 12, height = 9)
