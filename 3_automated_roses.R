

# A helper that takes one species name and returns its rose‐plot
make_rose <- function(sp) {
  dat %>%
    filter(scientific_name == sp) %>%
    ggplot(aes(x = hour, fill = protection)) +
    geom_histogram(
      breaks = seq(0, 24, by = 1),
      closed = "left",
      colour = "grey20", size = 0.2) +
    coord_polar(start = 0) +
    facet_wrap(~ sitio_captura, scales = "free_y") +
    scale_x_continuous(
      limits = c(0, 24),
      breaks = seq(0, 24, by = 3),
      labels = sprintf("%02d:00", seq(0, 24, by = 3))) +
    scale_fill_viridis_d(name = "management cover") +
    labs(title = sp,x = NULL, y = NULL) +
    theme_bw() +
    theme(panel.grid.major.y = element_line(colour = "grey80"),
      panel.grid.minor = element_blank(),
      strip.text = element_text(face = "bold"))
}

# Get unique species
species <- unique(dat$scientific_name)

# Build a named list of plots
rose_plots <- map(set_names(species), make_rose)

# Now you can:
# • print a single one, e.g. rose_plots[["Psamobatis rudis"]]
# • arrange them all: ggpubr::ggarrange(plotlist = rose_plots, ncol = 2)
walk2(
  rose_plots, names(rose_plots),
  ~ ggsave(
    filename = paste0("Outputs/roseplot_", .y, ".png"),
    plot     = .x,
    width    = 6, height = 4
  )
)

ggpubr::ggarrange(plotlist = rose_plots)
