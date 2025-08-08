# 08 August 2025 
  # calculating residency averages across tag and management zone
    # the plotting seperately
  # for the Subelab at ECIM in Chile
# Pablo Fuenzalida

rm(list=ls())
pacman::p_load(tidyverse)
setwd("~/Documents/OWUSS/experiences/Subelab ECIM")
list.files("Inputs")
dat <- read_rds("Inputs/250807_residency_subelab.rds")


# ---- 1. expand each event to one row per day ----
dat_days <- dat %>%
  transmute(tag_id,
            station_name,
            scientific_name,
            protection,
            date = map2(start_datetime, end_datetime,
                ~ seq.Date(as.Date(.x), as.Date(.y), by = "day"))) %>%
  unnest(date) %>%
  mutate(month = factor(month(date), levels = 1:12,
                        labels = month.abb, ordered = TRUE),
    year  = year(date))

# ---- 2. count days present per tag/station/month ----
monthly_days <- dat_days %>%
  distinct(tag_id, station_name, protection, 
           scientific_name, month, year, date) %>%
  count(tag_id, station_name, protection, scientific_name, month, year,
        name = "n_days_present")

# ---- 3. count years active per tag/station/month (distinct years) ----
years_per_month <- monthly_days %>%
  distinct(tag_id, station_name, month, year) %>%
  count(tag_id, station_name, month, name = "n_years")

# ---- 4. merge & compute days_per_year ----
monthly_days <- monthly_days %>%
  left_join(years_per_month,
            by = c("tag_id","station_name","month")) %>%
  mutate(days_per_year = n_days_present / n_years)

# ---- 5. summarize mean ± SD (and SE if wanted) ----
mean_residency <- monthly_days %>%
  group_by(station_name, protection, scientific_name, month) %>%
  summarise(mean_days_present = mean(days_per_year),
    sd_days_present  = sd(days_per_year),
    n_tags = n_distinct(tag_id),
    se_days_present = sd_days_present / sqrt(n_tags),
    .groups = "drop")

# ---- 6. count unique tags per station/species/month ----
unique_tags <- dat_days %>%
  distinct(tag_id, station_name, protection, scientific_name, month) %>%
  count(station_name, protection, scientific_name, month,
        name = "unique_tags")

# collapse to one row per protection × species × month
mean_residency_prot <- monthly_days %>%
  group_by(protection, scientific_name, month) %>%
  summarise(mean_days_present = mean(days_per_year),
    sd_days_present = sd(days_per_year),
    n_tags = n_distinct(tag_id),
    se_days_present = sd_days_present / sqrt(n_tags),
    .groups = "drop") 

# ---- 7. plot mean ± SD ----
# one point per species×month×protection + a line connecting months
p1 <- ggplot(mean_residency_prot,
             aes(x = month, y = mean_days_present,
                 colour = scientific_name, group = scientific_name)) +
  geom_line(size = 0.7) +
  geom_pointrange(aes(ymin = mean_days_present - sd_days_present,
                      ymax = mean_days_present + sd_days_present),
                  linewidth = 0.3) +
  facet_grid(scientific_name~protection) +
  scale_x_discrete(drop = FALSE, limits = month.abb) +
  labs(x = "Month", y = "Mean Days Present ± SD", colour = "Species") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major.x = element_blank()) +
  scale_colour_viridis_d(direction =-1)
p1

# ---- 8. plot unique tags ----
p2 <- ggplot(unique_tags,
             aes(x = month, y = unique_tags,
                 fill = scientific_name)) +
  geom_col(position = "stack") +
  facet_wrap(~protection, ncol = 1, scales = "free_y") +
  labs(x = "Month", y = "Unique Tags Detected",
       fill = "Species") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major.x = element_blank()) +
  scale_fill_viridis_d()
p2

# ---- 9. combine & save ----
combined <- ggpubr::ggarrange(p1, p2,
                      ncol = 2,
                      common.legend = TRUE,
                      legend = "right")
combined

ggsave("Outputs/250808_residency_sp.pdf",
       plot = combined, width = 12, height = 9)
