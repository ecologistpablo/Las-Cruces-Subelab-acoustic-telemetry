# 08 August 2025
  # making rose plots for each tag_id 
rm(list=ls())
pacman::p_load(tidyverse, purrr, furrr, fs)
setwd("~/Documents/OWUSS/experiences/Subelab ECIM")
list.files("Inputs")
dat <- read_rds("Inputs/250807_sube1.rds")

str(dat)

# --- params --------------------------------------------------------------
min_dets_per_hour <- 2
min_res_hours     <- 1
max_gap_secs      <- 3600
workers           <- 6

# single output folder for ALL PNGs
out_dir <- fs::path("Outputs", "tag_ids")
fs::dir_create(out_dir)

# --- build resident-hours (same as before, with a small fix) ------------
dat_events <- dat %>%
  arrange(tag_id, protection, datetime) %>%
  group_by(tag_id, protection) %>%
  mutate(gap_secs = as.numeric(datetime - lag(datetime), units = "secs"),
         new_event = if_else(is.na(gap_secs) | gap_secs > max_gap_secs, 1L, 0L),
         event_id = cumsum(new_event)) %>%
  ungroup()

det_hourly <- dat_events %>%
  mutate(hour_bin = lubridate::floor_date(datetime, "hour")) %>%
  group_by(tag_id, scientific_name, protection, event_id, hour_bin) %>%
  summarise(dets_in_hour = n(),
            hour_start = min(datetime),
            hour_end   = max(datetime),
            .groups = "drop")

residency_hours <- det_hourly %>%
  group_by(tag_id, scientific_name, protection, event_id) %>%
  summarise(duration_hours   = as.numeric(max(hour_end) - min(hour_start), units = "hours"),
            min_dets_per_hr = min(dets_in_hour),
            .groups = "drop") %>%
  # IMPORTANT: compare the column to the threshold var with .env
  dplyr::filter(duration_hours >= min_res_hours,
                min_dets_per_hr >= .env$min_dets_per_hour) %>%
  select(tag_id, scientific_name, protection, event_id) %>%
  right_join(det_hourly %>% filter(dets_in_hour >= min_dets_per_hour),
             by = c("tag_id","scientific_name","protection","event_id")) %>%
  mutate(hour_of_day = lubridate::hour(hour_bin)) %>%
  select(tag_id, scientific_name, protection, event_id, hour_of_day)

# --- plotting function: save all PNGs into Outputs/tag_ids --------------
plot_tag_rose <- function(tag_x, df = residency_hours, out_dir = out_dir) {
  df_tag <- df %>% filter(tag_id == tag_x)
  if (nrow(df_tag) == 0) {
    message("No resident hours for tag ", tag_x, " — skipping.")
    return(invisible(NULL))
  }
  sp <- df_tag$scientific_name[1]
  
  p <- ggplot(df_tag, aes(x = hour_of_day, fill = protection)) +
    geom_histogram(breaks = seq(0, 24, by = 1), colour = "grey20", size = 0.2) +
    coord_polar(start = 0) +
    facet_wrap(~ protection, ncol = 1, scales = "free_y") +
    scale_x_continuous(limits = c(0, 24),
                       breaks = seq(0, 24, by = 3),
                       labels = sprintf("%02d:00", seq(0, 24, by = 3))) +
    labs(title = paste0("Tag ", tag_x, " — ", sp),
         x = NULL, y = NULL, fill = "Protection") +
    theme_bw() +
    theme(axis.text.x = element_text(size = 8),
          strip.text  = element_text(face = "bold"),
          panel.grid.minor = element_blank())
  
  ggsave(fs::path(out_dir, paste0(tag_x, "_rose.png")),
         p, width = 7, height = 6, dpi = 300)
  invisible(NULL)
}

# --- run in parallel -----------------------------------------------------
future::plan(multisession, workers = workers)
unique_tags <- residency_hours %>% distinct(tag_id) %>% pull(tag_id)
furrr::future_walk(unique_tags, plot_tag_rose)
future::plan(sequential)
