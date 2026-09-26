# 06_temporal_modeling.R
# Comfort Mismatch Thesis
# Purpose: test whether comfort prediction depends on prior comfort state and temporal transition.

source("scripts/00_setup_packages.R")

dir.create("temporal_outputs", showWarnings = FALSE)
dir.create("temporal_outputs/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("temporal_outputs/figures", recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------
# 1. Load primary model table
# ------------------------------------------------------------

df_temporal_raw <- read_csv("audit_v2_primary_model_table.csv")

# ------------------------------------------------------------
# 2. Build temporal modeling table
# ------------------------------------------------------------

temporal_df <- df_temporal_raw %>%
  transmute(
    PID = factor(PID),
    location = factor(location),
    datetime = as.POSIXct(datetime, tz = "UTC"),
    not_comfy_binary = as.integer(not_comfy_binary),

    temp_nb = sht_nearbody_temperature_mean,
    humidity_nb = sht_nearbody_humidity_mean,
    voc_nb = sgp_nearbody_voc_mean,
    nox_nb = sgp_nearbody_nox_mean,
    skin_proxy = thermopile_overall_skin_proxy_mean,
    local_hour = local_hour
  ) %>%
  drop_na(
    PID, location, datetime, not_comfy_binary,
    temp_nb, humidity_nb, voc_nb, nox_nb, skin_proxy, local_hour
  ) %>%
  arrange(PID, datetime) %>%
  group_by(PID) %>%
  mutate(
    prev_datetime = lag(datetime),
    prev_not_comfy = lag(not_comfy_binary),
    time_since_prev_hours = as.numeric(
      difftime(datetime, prev_datetime, units = "hours")
    ),
    comfort_transition = if_else(
      !is.na(prev_not_comfy) & not_comfy_binary != prev_not_comfy,
      1L,
      0L,
      missing = NA_integer_
    )
  ) %>%
  ungroup() %>%
  mutate(
    temp_nb_z = as.numeric(scale(temp_nb)),
    humidity_nb_z = as.numeric(scale(humidity_nb)),
    voc_nb_z = as.numeric(scale(voc_nb)),
    nox_nb_z = as.numeric(scale(nox_nb)),
    skin_proxy_z = as.numeric(scale(skin_proxy)),
    local_hour_z = as.numeric(scale(local_hour)),
    time_since_prev_hours_z = as.numeric(scale(time_since_prev_hours))
  )

write_csv(
  temporal_df,
  "temporal_outputs/tables/temporal_full_table_with_lags.csv"
)

# ------------------------------------------------------------
# 3. Temporal validity check
# ------------------------------------------------------------

temporal_validity <- temporal_df %>%
  summarise(
    rows_total = n(),
    participants = n_distinct(PID),
    rows_with_previous = sum(!is.na(prev_not_comfy)),
    rows_missing_previous = sum(is.na(prev_not_comfy)),
    median_time_gap_hours = median(time_since_prev_hours, na.rm = TRUE),
    max_time_gap_hours = max(time_since_prev_hours, na.rm = TRUE),
    gaps_over_12h = sum(time_since_prev_hours > 12, na.rm = TRUE),
    gaps_over_24h = sum(time_since_prev_hours > 24, na.rm = TRUE),
    transitions = sum(comfort_transition, na.rm = TRUE),
    transition_rate = mean(comfort_transition, na.rm = TRUE)
  )

print(temporal_validity)

write_csv(
  temporal_validity,
  "temporal_outputs/tables/temporal_validity_summary.csv"
)

# ------------------------------------------------------------
# 4. Create temporal model table
# ------------------------------------------------------------
# We use rows with a previous survey and exclude gaps > 24h.
# This avoids treating very distant surveys as direct previous states.

temporal_model_df <- temporal_df %>%
  filter(
    !is.na(prev_not_comfy),
    !is.na(time_since_prev_hours),
    time_since_prev_hours <= 24
  )

temporal_model_summary <- temporal_model_df %>%
  summarise(
    rows = n(),
    participants = n_distinct(PID),
    not_comfy_events = sum(not_comfy_binary),
    not_comfy_rate = mean(not_comfy_binary),
    previous_not_comfy_events = sum(prev_not_comfy),
    transition_events = sum(comfort_transition),
    transition_rate = mean(comfort_transition)
  )

print(temporal_model_summary)

write_csv(
  temporal_model_summary,
  "temporal_outputs/tables/temporal_model_sample_summary.csv"
)

write_csv(
  temporal_model_df,
  "temporal_outputs/tables/temporal_model_table.csv"
)

# ------------------------------------------------------------
# 5. Fit baseline sensor model and lagged comfort model
# ------------------------------------------------------------

m_temporal_sensor <- glmer(
  not_comfy_binary ~ temp_nb_z + humidity_nb_z + voc_nb_z + nox_nb_z +
    skin_proxy_z + location + local_hour_z +
    (1 | PID),
  data = temporal_model_df,
  family = binomial(),
  control = glmerControl(optimizer = "bobyqa")
)

m_temporal_lagged <- glmer(
  not_comfy_binary ~ prev_not_comfy + time_since_prev_hours_z +
    temp_nb_z + humidity_nb_z + voc_nb_z + nox_nb_z +
    skin_proxy_z + location + local_hour_z +
    (1 | PID),
  data = temporal_model_df,
  family = binomial(),
  control = glmerControl(optimizer = "bobyqa")
)

temporal_model_comparison <- tibble(
  model = c("Sensor mixed model", "Lagged comfort mixed model"),
  AIC = c(AIC(m_temporal_sensor), AIC(m_temporal_lagged)),
  BIC = c(BIC(m_temporal_sensor), BIC(m_temporal_lagged))
)

print(temporal_model_comparison)
print(performance::r2(m_temporal_lagged))
print(performance::icc(m_temporal_lagged))
print(isSingular(m_temporal_lagged))

write_csv(
  temporal_model_comparison,
  "temporal_outputs/tables/temporal_model_comparison.csv"
)

# ------------------------------------------------------------
# 6. Save lagged model effects
# ------------------------------------------------------------

temporal_lagged_effects <- broom.mixed::tidy(
  m_temporal_lagged,
  effects = "fixed",
  conf.int = TRUE,
  exponentiate = TRUE
)

temporal_lagged_random <- broom.mixed::tidy(
  m_temporal_lagged,
  effects = "ran_pars"
)

print(temporal_lagged_effects)
print(temporal_lagged_random)

write_csv(
  temporal_lagged_effects,
  "temporal_outputs/tables/temporal_lagged_model_fixed_effects_odds_ratios.csv"
)

write_csv(
  temporal_lagged_random,
  "temporal_outputs/tables/temporal_lagged_model_random_effects.csv"
)

# ------------------------------------------------------------
# 7. Plot transition distribution
# ------------------------------------------------------------

transition_summary <- temporal_model_df %>%
  count(comfort_transition) %>%
  mutate(
    transition_label = if_else(
      comfort_transition == 1,
      "Comfort state changed",
      "Comfort state stayed same"
    ),
    percent = n / sum(n)
  )

write_csv(
  transition_summary,
  "temporal_outputs/tables/temporal_transition_summary.csv"
)

p_transition <- ggplot(
  transition_summary,
  aes(x = transition_label, y = percent)
) +
  geom_col() +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Comfort Transition Frequency",
    subtitle = "Rows with a previous survey within 24 hours",
    x = "Transition status",
    y = "Percent of temporal model rows"
  ) +
  theme_minimal()

ggsave(
  "temporal_outputs/figures/temporal_transition_frequency.png",
  p_transition,
  width = 8,
  height = 5,
  dpi = 300
)

message("Temporal modeling script complete. Outputs saved in temporal_outputs/.")

# ------------------------------------------------------------
# 8. Exploratory transition model
# ------------------------------------------------------------
# Purpose:
# Test whether comfort-state changes are associated with prior state,
# time gap, sensor variables, and participant-level clustering.
#
# This is exploratory because transitions are relatively rare:
# 80 transition events out of 587 temporal model rows.

m_transition <- glmer(
  comfort_transition ~ prev_not_comfy + time_since_prev_hours_z +
    temp_nb_z + humidity_nb_z + voc_nb_z + nox_nb_z +
    skin_proxy_z + location + local_hour_z +
    (1 | PID),
  data = temporal_model_df,
  family = binomial(),
  control = glmerControl(optimizer = "bobyqa")
)

transition_model_effects <- broom.mixed::tidy(
  m_transition,
  effects = "fixed",
  conf.int = TRUE,
  exponentiate = TRUE
)

transition_model_random <- broom.mixed::tidy(
  m_transition,
  effects = "ran_pars"
)

transition_model_checks <- tibble(
  check = c(
    "conditional_r2",
    "marginal_r2",
    "adjusted_icc",
    "unadjusted_icc",
    "is_singular",
    "AIC",
    "BIC"
  ),
  value = c(
    performance::r2(m_transition)$R2_conditional,
    performance::r2(m_transition)$R2_marginal,
    performance::icc(m_transition)$ICC_adjusted,
    performance::icc(m_transition)$ICC_unadjusted,
    as.numeric(isSingular(m_transition)),
    AIC(m_transition),
    BIC(m_transition)
  )
)

print(transition_model_effects)
print(transition_model_random)
print(transition_model_checks)

write_csv(
  transition_model_effects,
  "temporal_outputs/tables/transition_model_fixed_effects_odds_ratios.csv"
)

write_csv(
  transition_model_random,
  "temporal_outputs/tables/transition_model_random_effects.csv"
)

write_csv(
  transition_model_checks,
  "temporal_outputs/tables/transition_model_checks.csv"
)

message("Exploratory transition model complete.")