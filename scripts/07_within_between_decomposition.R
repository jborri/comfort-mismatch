# 07_within_between_decomposition.R
# Comfort Mismatch Thesis
# Purpose:
# Test whether discomfort is associated with universal sensor levels
# or within-person deviations from each participant's own baseline.

source("scripts/00_setup_packages.R")

dir.create("within_between_outputs", showWarnings = FALSE)
dir.create("within_between_outputs/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("within_between_outputs/figures", recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------
# 1. Load primary model table
# ------------------------------------------------------------

df_wb_raw <- read_csv("audit_v2_primary_model_table.csv")

wb_df <- df_wb_raw %>%
  transmute(
    PID = factor(PID),
    location = factor(location),
    not_comfy_binary = as.integer(not_comfy_binary),

    temp_nb = sht_nearbody_temperature_mean,
    humidity_nb = sht_nearbody_humidity_mean,
    voc_nb = sgp_nearbody_voc_mean,
    nox_nb = sgp_nearbody_nox_mean,
    skin_proxy = thermopile_overall_skin_proxy_mean,
    local_hour = local_hour
  ) %>%
  drop_na()

wb_sample_summary <- wb_df %>%
  summarise(
    rows = n(),
    participants = n_distinct(PID),
    not_comfy_events = sum(not_comfy_binary),
    not_comfy_rate = mean(not_comfy_binary)
  )

print(wb_sample_summary)

write_csv(
  wb_sample_summary,
  "within_between_outputs/tables/within_between_sample_summary.csv"
)

# ------------------------------------------------------------
# 2. Create within-person and between-person variables
# ------------------------------------------------------------
# Between-person value = participant mean sensor value
# Within-person value = current value - participant mean
#
# This asks whether discomfort relates to:
#   A. a participant's typical exposure level, or
#   B. deviation from that participant's own usual exposure.

wb_df <- wb_df %>%
  group_by(PID) %>%
  mutate(
    temp_nb_between = mean(temp_nb, na.rm = TRUE),
    humidity_nb_between = mean(humidity_nb, na.rm = TRUE),
    voc_nb_between = mean(voc_nb, na.rm = TRUE),
    nox_nb_between = mean(nox_nb, na.rm = TRUE),
    skin_proxy_between = mean(skin_proxy, na.rm = TRUE),

    temp_nb_within = temp_nb - temp_nb_between,
    humidity_nb_within = humidity_nb - humidity_nb_between,
    voc_nb_within = voc_nb - voc_nb_between,
    nox_nb_within = nox_nb - nox_nb_between,
    skin_proxy_within = skin_proxy - skin_proxy_between
  ) %>%
  ungroup() %>%
  mutate(
    temp_nb_between_z = as.numeric(scale(temp_nb_between)),
    humidity_nb_between_z = as.numeric(scale(humidity_nb_between)),
    voc_nb_between_z = as.numeric(scale(voc_nb_between)),
    nox_nb_between_z = as.numeric(scale(nox_nb_between)),
    skin_proxy_between_z = as.numeric(scale(skin_proxy_between)),

    temp_nb_within_z = as.numeric(scale(temp_nb_within)),
    humidity_nb_within_z = as.numeric(scale(humidity_nb_within)),
    voc_nb_within_z = as.numeric(scale(voc_nb_within)),
    nox_nb_within_z = as.numeric(scale(nox_nb_within)),
    skin_proxy_within_z = as.numeric(scale(skin_proxy_within)),

    local_hour_z = as.numeric(scale(local_hour))
  )

write_csv(
  wb_df,
  "within_between_outputs/tables/within_between_model_table.csv"
)

# ------------------------------------------------------------
# 3. Fit baseline primary mixed model for comparison
# ------------------------------------------------------------

m_primary_reference <- glmer(
  not_comfy_binary ~
    as.numeric(scale(temp_nb)) +
    as.numeric(scale(humidity_nb)) +
    as.numeric(scale(voc_nb)) +
    as.numeric(scale(nox_nb)) +
    as.numeric(scale(skin_proxy)) +
    location +
    local_hour_z +
    (1 | PID),
  data = wb_df,
  family = binomial(),
  control = glmerControl(optimizer = "bobyqa")
)

# ------------------------------------------------------------
# 4. Parsimonious within-between model
# ------------------------------------------------------------
# Uses temperature and humidity only.
# Why:
#   - These are the clearest thermal/microclimate variables.
#   - Humidity was one of the strongest predictors earlier.
#   - This avoids overfitting with too many within/between terms.

m_wb_thermal <- glmer(
  not_comfy_binary ~
    temp_nb_within_z + temp_nb_between_z +
    humidity_nb_within_z + humidity_nb_between_z +
    location +
    local_hour_z +
    (1 | PID),
  data = wb_df,
  family = binomial(),
  control = glmerControl(optimizer = "bobyqa")
)

# ------------------------------------------------------------
# 5. Exploratory full within-between model
# ------------------------------------------------------------
# Includes all primary sensor variables.
# Interpret cautiously because the sample has 90 not-comfy events.

m_wb_full <- glmer(
  not_comfy_binary ~
    temp_nb_within_z + temp_nb_between_z +
    humidity_nb_within_z + humidity_nb_between_z +
    voc_nb_within_z + voc_nb_between_z +
    nox_nb_within_z + nox_nb_between_z +
    skin_proxy_within_z + skin_proxy_between_z +
    location +
    local_hour_z +
    (1 | PID),
  data = wb_df,
  family = binomial(),
  control = glmerControl(optimizer = "bobyqa")
)

# ------------------------------------------------------------
# 6. Compare models
# ------------------------------------------------------------

wb_model_comparison <- tibble(
  model = c(
    "Primary reference mixed model",
    "Thermal within-between mixed model",
    "Full exploratory within-between mixed model"
  ),
  AIC = c(
    AIC(m_primary_reference),
    AIC(m_wb_thermal),
    AIC(m_wb_full)
  ),
  BIC = c(
    BIC(m_primary_reference),
    BIC(m_wb_thermal),
    BIC(m_wb_full)
  ),
  is_singular = c(
    isSingular(m_primary_reference),
    isSingular(m_wb_thermal),
    isSingular(m_wb_full)
  )
)

print(wb_model_comparison)

write_csv(
  wb_model_comparison,
  "within_between_outputs/tables/within_between_model_comparison.csv"
)

# ------------------------------------------------------------
# 7. Save model effects
# ------------------------------------------------------------

wb_thermal_effects <- broom.mixed::tidy(
  m_wb_thermal,
  effects = "fixed",
  conf.int = TRUE,
  exponentiate = TRUE
)

wb_full_effects <- broom.mixed::tidy(
  m_wb_full,
  effects = "fixed",
  conf.int = TRUE,
  exponentiate = TRUE
)

wb_thermal_random <- broom.mixed::tidy(
  m_wb_thermal,
  effects = "ran_pars"
)

wb_full_random <- broom.mixed::tidy(
  m_wb_full,
  effects = "ran_pars"
)

print(wb_thermal_effects)
print(wb_full_effects)

write_csv(
  wb_thermal_effects,
  "within_between_outputs/tables/thermal_within_between_fixed_effects_odds_ratios.csv"
)

write_csv(
  wb_full_effects,
  "within_between_outputs/tables/full_within_between_fixed_effects_odds_ratios.csv"
)

write_csv(
  wb_thermal_random,
  "within_between_outputs/tables/thermal_within_between_random_effects.csv"
)

write_csv(
  wb_full_random,
  "within_between_outputs/tables/full_within_between_random_effects.csv"
)

# ------------------------------------------------------------
# 8. Save R2 / ICC checks
# ------------------------------------------------------------

wb_model_checks <- tibble(
  model = c(
    "Thermal within-between mixed model",
    "Full exploratory within-between mixed model"
  ),
  conditional_r2 = c(
    performance::r2(m_wb_thermal)$R2_conditional,
    performance::r2(m_wb_full)$R2_conditional
  ),
  marginal_r2 = c(
    performance::r2(m_wb_thermal)$R2_marginal,
    performance::r2(m_wb_full)$R2_marginal
  ),
  adjusted_icc = c(
    performance::icc(m_wb_thermal)$ICC_adjusted,
    performance::icc(m_wb_full)$ICC_adjusted
  ),
  unadjusted_icc = c(
    performance::icc(m_wb_thermal)$ICC_unadjusted,
    performance::icc(m_wb_full)$ICC_unadjusted
  ),
  is_singular = c(
    isSingular(m_wb_thermal),
    isSingular(m_wb_full)
  )
)

print(wb_model_checks)

write_csv(
  wb_model_checks,
  "within_between_outputs/tables/within_between_model_checks.csv"
)

# ------------------------------------------------------------
# 9. Plot selected within-between effects
# ------------------------------------------------------------

wb_plot_effects <- wb_thermal_effects %>%
  filter(term != "(Intercept)") %>%
  mutate(
    term_label = recode(
      term,
      "temp_nb_within_z" = "Temperature: within-person deviation",
      "temp_nb_between_z" = "Temperature: participant mean",
      "humidity_nb_within_z" = "Humidity: within-person deviation",
      "humidity_nb_between_z" = "Humidity: participant mean",
      "locationUS" = "Location: US",
      "local_hour_z" = "Local hour"
    )
  )

p_wb_thermal <- ggplot(
  wb_plot_effects,
  aes(x = reorder(term_label, estimate), y = estimate)
) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.15) +
  coord_flip() +
  scale_y_log10() +
  labs(
    title = "Within-Person vs Between-Person Thermal Predictors",
    subtitle = "Odds ratios from parsimonious mixed-effects model",
    x = "Predictor",
    y = "Odds ratio, log scale"
  ) +
  theme_minimal()

ggsave(
  "within_between_outputs/figures/thermal_within_between_odds_ratios.png",
  p_wb_thermal,
  width = 9,
  height = 6,
  dpi = 300
)

message("Within-between decomposition complete. Outputs saved in within_between_outputs/.")