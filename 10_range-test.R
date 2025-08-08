pacman::p_load(tidyverse,lubridate)

rm(list=ls())
# ---- 1) Load & clean ----
# Read CSV; adjust the path as needed
dat <- read_delim("Inputs/Rangetestag_DATA_01-2022.csv",
                  delim = ";", escape_double = FALSE, trim_ws = TRUE)

dat <- janitor::clean_names(dat) 
# ^ Standardize column names to snake_case

# ---- 2) Create datetime in UTC, then convert to Santiago ----
# IMPORTANT: we stamp the input as UTC, then convert to America/Santiago
dat1 <- dat %>%
  mutate(
    detections = 1,  # each row is a detection event
    datetime_utc = as.POSIXct(
      paste(date_utc, time_utc), 
      format = "%d-%m-%y %H:%M:%S", 
      tz = "UTC"
    ),
    datetime = with_tz(datetime_utc, tzone = "America/Santiago"),
    hour_time = floor_date(datetime, unit = "hour") # <-- crucial for hourly summaries
  )

# Quick sanity check (optional)
summary(dat1$datetime)

# ---- 3) Standardize distance to numeric meters ----
# Your 'distance' is like "150 metros" → 150
dat1 <- dat1 %>%
  mutate(
    distance = readr::parse_number(distance)  # extracts numeric part
  )

# ---- 4) Hourly detections per receiver/transmitter/distance ----
# This is the level where "expected per hour" makes sense.
dat_hour <- dat1 %>%
  group_by(hour_time, receiver, transmitter, distance) %>%
  summarise(n_det = sum(detections), .groups = "drop")
# ^ n_det = number of successful detections in that hour, at that distance, for that Rx/Tx

# ---- 5) Add expected detections per hour (detec.esperadas) ----
# Define your time windows in Santiago local time (since HourlyTime is in that tz).
# NOTE: Replace these windows if they should be in UTC; consistency matters.
dat_hour <- dat_hour %>%
  mutate(
    detec_esperadas = case_when(
      hour_time >= as.POSIXct("2021-12-16 17:00:00", tz = "America/Santiago") &
        HourlyTime <= as.POSIXct("2021-12-21 16:00:00", tz = "America/Santiago") ~ 180,  # e.g., 20s ping rate
      HourlyTime >= as.POSIXct("2021-12-21 17:00:00", tz = "America/Santiago") &
        HourlyTime <= as.POSIXct("2022-01-07 18:00:00", tz = "America/Santiago") ~ 6,    # e.g., 10 min ping rate
      TRUE ~ NA_real_
    )
  )

# ---- 6) Filter to valid rows and compute hourly detection proportion ----
dat_hour <- dat_hour %>%
  filter(!is.na(detec_esperadas) & detec_esperadas > 0) %>%
  filter(n_det <= detec_esperadas) %>%   # sanity
  mutate(tasa_detec = n_det / detec_esperadas)

# Safety check
stopifnot(nrow(dat_hour) > 0)

# ---- 7) Collapse to distance level for modeling ----
# Binomial GLM needs (successes, failures). We aggregate across hours and units.
sumadet_data <- dat_hour %>%
  group_by(Distance) %>%
  summarise(
    total_detections = sum(n_det, na.rm = TRUE),            # successes
    total_expected   = sum(detec_esperadas, na.rm = TRUE),  # trials
    tasa_detec       = total_detections / total_expected,   # observed proportion
    .groups = "drop"
  ) %>%
  filter(total_expected > 0, total_detections <= total_expected)

# ---- 8) Fit logistic model: p ~ Distance ----
glm_model <- glm(
  cbind(total_detections, total_expected - total_detections) ~ Distance,
  family = binomial(link = "logit"),
  data = sumadet_data
)

# ---- 9) Predictions + 95% CI on response scale ----
new_data <- tibble(Distance = seq(0, max(sumadet_data$Distance, na.rm = TRUE), length.out = 200))

# Predict on link scale with SE, then transform to response with 95% CI
pred <- predict(glm_model, newdata = new_data, type = "link", se.fit = TRUE)

new_data <- new_data %>%
  mutate(
    fit_link = pred$fit,
    se_link  = pred$se.fit,
    lwr_link = fit_link - 1.96 * se_link,
    upr_link = fit_link + 1.96 * se_link,
    predicted_prob = plogis(fit_link),
    lwr = plogis(lwr_link),
    upr = plogis(upr_link)
  )

# ---- 10) Distances at 5%, 50%, 95% detection (point estimates) ----
b0 <- coef(glm_model)[1]
b1 <- coef(glm_model)[2]
logit <- function(p) log(p/(1 - p))

distance_05 <- (logit(0.05) - b0) / b1
distance_50 <- (logit(0.50) - b0) / b1
distance_95 <- (logit(0.95) - b0) / b1

# ---- 11) Plot: points (alpha 0.5), fit line, 95% CI ribbon ----
p <- ggplot(sumadet_data, aes(x = Distance, y = tasa_detec)) +
  geom_point(alpha = 0.5) +  # raw proportions by distance
  geom_ribbon(
    data = new_data,
    aes(ymin = lwr, ymax = upr, y = NULL), 
    inherit.aes = FALSE,
    alpha = 0.2
  ) +
  geom_line(
    data = new_data, 
    aes(y = predicted_prob)
  ) +
  # Reference lines and markers
  geom_hline(yintercept = c(0.95, 0.50, 0.05), linetype = "dashed") +
  geom_segment(aes(x = distance_05, xend = distance_05, y = 0, yend = 0.05), linetype = "dashed") +
  geom_segment(aes(x = distance_50, xend = distance_50, y = 0, yend = 0.50), linetype = "dashed") +
  geom_segment(aes(x = distance_95, xend = distance_95, y = 0, yend = 0.95), linetype = "dashed") +
  geom_point(aes(x = distance_50, y = 0.50), color = "red", size = 2) +
  coord_cartesian(xlim = c(0, max(sumadet_data$Distance, na.rm = TRUE)), ylim = c(0, 1)) +
  theme_minimal(base_size = 14) +
  theme(
    axis.line = element_line(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  labs(
    x = "Distance from receiver (m)",
    y = "Proportion of transmissions received",
    title = "Detection range test (Las Cruces, Chile)",
    subtitle = paste(
      "Distances at 5%, 50%, 95% detection:",
      paste0("5%: ", round(distance_05, 1), " m"),
      paste0("| 50%: ", round(distance_50, 1), " m"),
      paste0("| 95%: ", round(distance_95, 1), " m")
    )
  )

# Save figure
ggsave("detection_range_plot.png", plot = p, width = 8, height = 6, dpi = 300, bg = "white")
