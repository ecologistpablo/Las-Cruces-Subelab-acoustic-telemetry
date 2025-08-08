
rm(list=ls())
pacman::p_load(tidyverse, purrr, furrr)
setwd("~/Documents/OWUSS/experiences/Subelab ECIM")
list.files("Inputs")
dat <- read_rds("Inputs/250807_sube1.rds")

# Order tags by first detection (top = earliest)
tag_levels <- dat %>%
  group_by(tag_id) %>%
  summarise(first_det = min(datetime, na.rm = TRUE), .groups = "drop") %>%
  arrange(first_det) %>%
  pull(tag_id)

dat_abacus <- dat %>%
  mutate(tag_id = factor(tag_id, levels = tag_levels))

# Abacus plot
a <- ggplot(dat_abacus, aes(x = datetime, y = tag_id, colour = protection)) +
  geom_point(shape = 1, size = 0.6, alpha = 1) +  # shape 124 = vertical tick "|"
  scale_x_datetime(date_breaks = "1 month", date_labels = "%Y-%m") +
  labs(x = "Datetime", y = "Tag ID", title = "Detections by Tag (Abacus Plot)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.minor = element_blank())
a

ggsave("Outputs/250808_abacus_tag-id.pdf",
       plot = a, width = 12, height = 9)
