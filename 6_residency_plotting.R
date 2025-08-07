rm(list=ls())
pacman::p_load(tidyverse)
setwd("~/Documents/OWUSS/experiences/Subelab ECIM")
list.files("Inputs")
dat <- read_rds("Inputs/250807_residency_subelab.rds")

# 1. Expand all residency events into daily rows, extract month and year
dat_days <- dat %>%
  mutate(start_date = as.Date(start_datetime),
         end_date = as.Date(end_datetime)) %>%
  rowwise() %>% # push through function at each row 
  mutate(date = list(seq(start_date, end_date, by = "day"))) %>%
  ungroup() %>%
  dplyr::select(tag_id, station_name, date, scientific_name,
                protection, substrate) %>%
  unnest(date) 

# choose a block size so format() never allocates more than ~5 million strings at once
block_size <- 5e6L    
n <- nrow(dat_days)
# pre-allocate integer columns
dat_days$month <- integer(n)
dat_days$year  <- integer(n)
# fill in by chunks
for (i in seq(1L, n, by = block_size)) {
  j <- min(i + block_size - 1L, n)
  chunk_dates <- dat_days$date[i:j]
  dat_days$month[i:j] <- lubridate::month(chunk_dates)
  dat_days$year[i:j]  <- lubridate::year(chunk_dates)
}

# 2. For each tag/station_name/month, count days present
monthly_days <- dat_days %>%
  group_by(tag_id, protection, station_name, month, year, scientific_name) %>%
  summarise(n_days_present = n_distinct(date), .groups = "drop")

# 3. For each tag/station_name/month, get number of years tag was active
years_per_tag_month <- monthly_days %>%
  group_by(tag_id, station_name, month) %>%
  summarise(n_years = n(), .groups = "drop")   # n() counts years (since year is grouped above)

# 4. Calculate mean days per year per tag/station_name/month
monthly_days <- monthly_days %>%
  left_join(years_per_tag_month, by = c("tag_id", "station_name", "month")) %>%
  mutate(days_per_year = n_days_present / n_years)

# 5. For each station_name/sex/month, get mean residency index
mean_residency <- monthly_days %>%
  group_by(station_name, protection, scientific_name, month) %>%
  summarise(mean_days_present = mean(days_per_year),
            sd_days_present = sd(days_per_year),          # Add SD
            se_days_present = sd(days_per_year) / sqrt(n()), # Add SE if you want
            .groups = "drop")

summary(mean_residency$mean_days_present)


# unique tags -------------------------------------------------------------

unique_tags_monthly <- dat_days %>%
  group_by(station_name, protection, month, scientific_name) %>%
  summarise(unique_tags = n_distinct(tag_id), .groups = "drop")

# plot --------------------------------------------------------------------

# residency index plot
a <- ggplot(mean_residency, aes(x = month, y = mean_days_present,
                                colour = scientific_name,
                                 group = scientific_name)) +
  geom_pointrange(aes(ymin = mean_days_present - sd_days_present, 
                      ymax = mean_days_present + sd_days_present),
                  position = position_dodge(width = 0.5)) +
  facet_wrap(~protection, ncol = 1) +
  #scale_colour_manual(values = c("F" = "firebrick4", "M" = "deepskyblue4")) +
  labs( x = "Month", y = "Mean Days Present per Sex (± SD)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank())
a
# unique tags plot
b <- ggplot(unique_tags_monthly, aes(x = month, y = unique_tags, fill = scientific_name)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~station_name, scales = "fixed", ncol = 1) +
  #scale_fill_manual(values = c("F" = "firebrick4", "M" = "deepskyblue4"),
  #                  name = "Sex") +
  labs(x = "Month", y = "Number of Unique Tags Detected") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank())

z <- ggarrange(a,b, common.legend = T, legend = "right")
z

ggsave(path = "Outputs/", "250807_residency_stationame.pdf",
       plot = z, width = 7, height = 9) #in inches because gg weird


