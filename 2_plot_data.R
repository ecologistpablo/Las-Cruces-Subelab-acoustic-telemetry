# 07 August 2025

rm(list=ls())
dat <- read_rds("Inputs/250807_sube1.rds")

anyNA(dat)
str(dat)
unique(dat$scientific_name)
 sc <- dat %>% 
  filter(scientific_name == "Schroederichthys chilensis")

sc <- ggplot(sc, aes(x = hour, fill = protection)) +
  geom_histogram(breaks = seq(0, 24, by = 1),
                  closed = "left",colour = "grey20", size = 0.2) +
  coord_polar(start = 0) +
  facet_wrap(~sitio_captura, scales = 'free_y') + # tidy up axes , scales = "free_y"
  scale_x_continuous(limits = c(0, 24),
    breaks = seq(0, 24, by = 3),
    labels = sprintf("%02d:00", seq(0, 24, by = 3))) +
  scale_fill_viridis_d(name = "management cover") +
  labs(title = "S. chilensis", x = NULL, y = NULL) +
  theme_bw() +
  theme(panel.grid.major.y = element_line(colour = "grey80"),
    panel.grid.minor = element_blank(),
    axis.title.y = element_text(margin = margin(r = 10)),
    strip.text  = element_text(face = "bold"))

# otro pez ----------------------------------------------------------------

sc <- dat %>% 
  filter(scientific_name == "Schroederichthys chilensis")

sc <- ggplot(sc, aes(x = hour, fill = protection)) +
  geom_histogram(breaks = seq(0, 24, by = 1),
                 closed = "left",colour = "grey20", size = 0.2) +
  coord_polar(start = 0) +
  facet_wrap(~sitio_captura, scales = 'free_y') + # tidy up axes , scales = "free_y"
  scale_x_continuous(limits = c(0, 24),
                     breaks = seq(0, 24, by = 3),
                     labels = sprintf("%02d:00", seq(0, 24, by = 3))) +
  scale_fill_viridis_d(name = "management cover") +
  labs(title = "S. chilensis", x = NULL, y = NULL) +
  theme_bw() +
  theme(panel.grid.major.y = element_line(colour = "grey80"),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(margin = margin(r = 10)),
        strip.text  = element_text(face = "bold"))
