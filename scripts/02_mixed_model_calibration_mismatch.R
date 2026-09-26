# 02_mixed_model_calibration_mismatch.R
# Purpose: fit fixed and mixed models, compare participant calibration, evaluate calibration and mismatch.

library(tidyverse)
library(lme4)
library(performance)
library(broom.mixed)
library(yardstick)
library(scales)

dir.create("mixed_model_outputs/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("mixed_model_outputs/figures", recursive = TRUE, showWarnings = FALSE)

model_df <- read_csv("outputs/tables/model_df_prepared.csv") %>%
  mutate(PID = factor(PID), location = factor(location))

m1_fixed <- glm(
  not_comfy_binary ~ temp_nb_z + humidity_nb_z + voc_nb_z + nox_nb_z +
    skin_proxy_z + location + local_hour_z,
  data = model_df,
  family = binomial()
)

m2_mixed_intercept <- glmer(
  not_comfy_binary ~ temp_nb_z + humidity_nb_z + voc_nb_z + nox_nb_z +
    skin_proxy_z + location + local_hour_z + (1 | PID),
  data = model_df,
  family = binomial(),
  control = glmerControl(optimizer = "bobyqa")
)

model_comparison <- tibble(
  model = c("Fixed model", "Mixed random-intercept model"),
  AIC = c(AIC(m1_fixed), AIC(m2_mixed_intercept)),
  BIC = c(BIC(m1_fixed), BIC(m2_mixed_intercept))
)

print(model_comparison)
print(performance::r2(m2_mixed_intercept))
print(performance::icc(m2_mixed_intercept))
print(isSingular(m2_mixed_intercept))

write_csv(model_comparison, "mixed_model_outputs/tables/model_aic_bic_comparison.csv")

fixed_results <- broom.mixed::tidy(
  m2_mixed_intercept,
  effects = "fixed",
  conf.int = TRUE,
  exponentiate = TRUE
)

random_results <- broom.mixed::tidy(
  m2_mixed_intercept,
  effects = "ran_pars"
)

write_csv(fixed_results, "mixed_model_outputs/tables/mixed_model_fixed_effects_odds_ratios.csv")
write_csv(random_results, "mixed_model_outputs/tables/mixed_model_random_effect_parameters.csv")

ranef_df <- ranef(m2_mixed_intercept)$PID %>%
  as.data.frame() %>%
  rownames_to_column("PID") %>%
  rename(random_intercept = `(Intercept)`) %>%
  arrange(random_intercept)

write_csv(ranef_df, "mixed_model_outputs/tables/mixed_model_participant_random_intercepts.csv")

p_random_intercepts <- ggplot(
  ranef_df,
  aes(x = reorder(PID, random_intercept), y = random_intercept)
) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(size = 2) +
  coord_flip() +
  labs(
    title = "Participant-Specific Baseline Discomfort",
    subtitle = "Random intercepts from mixed-effects logistic model",
    x = "Participant ID",
    y = "Random intercept"
  ) +
  theme_minimal()

ggsave(
  "mixed_model_outputs/figures/mixed_model_random_intercepts_plot.png",
  p_random_intercepts,
  width = 8,
  height = 6,
  dpi = 300
)

model_df_preds <- model_df %>%
  mutate(
    pred_fixed = predict(m1_fixed, type = "response"),
    pred_mixed_known = predict(m2_mixed_intercept, type = "response", re.form = NULL),
    pred_mixed_population = predict(m2_mixed_intercept, type = "response", re.form = NA)
  )

write_csv(model_df_preds, "mixed_model_outputs/tables/model_predictions_fixed_vs_mixed.csv")

make_calibration_table <- function(data, pred_col, outcome_col = "not_comfy_binary", bins = 10) {
  pred_col <- rlang::ensym(pred_col)
  outcome_col <- rlang::ensym(outcome_col)

  data %>%
    mutate(
      pred = !!pred_col,
      outcome = !!outcome_col,
      bin = ntile(pred, bins)
    ) %>%
    group_by(bin) %>%
    summarise(
      n = n(),
      mean_predicted = mean(pred, na.rm = TRUE),
      observed_rate = mean(outcome, na.rm = TRUE),
      events = sum(outcome, na.rm = TRUE),
      .groups = "drop"
    )
}

calibration_all <- bind_rows(
  make_calibration_table(model_df_preds, pred_fixed) %>% mutate(model = "Fixed model"),
  make_calibration_table(model_df_preds, pred_mixed_known) %>% mutate(model = "Mixed model: known participants"),
  make_calibration_table(model_df_preds, pred_mixed_population) %>% mutate(model = "Mixed model: population-level")
)

write_csv(calibration_all, "mixed_model_outputs/tables/calibration_bins_fixed_vs_mixed.csv")

p_calibration <- ggplot(
  calibration_all,
  aes(x = mean_predicted, y = observed_rate, group = model)
) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  geom_line() +
  geom_point(aes(size = n)) +
  facet_wrap(~ model) +
  labs(
    title = "Calibration of Fixed and Mixed Comfort Prediction Models",
    subtitle = "Predicted discomfort probability vs. observed discomfort rate",
    x = "Mean predicted probability of not comfy",
    y = "Observed not-comfy rate",
    size = "Rows"
  ) +
  theme_minimal()

ggsave(
  "mixed_model_outputs/figures/calibration_fixed_vs_mixed.png",
  p_calibration,
  width = 10,
  height = 6,
  dpi = 300
)

brier_scores <- tibble(
  model = c("Fixed model", "Mixed model: known participants", "Mixed model: population-level"),
  brier_score = c(
    mean((model_df_preds$pred_fixed - model_df_preds$not_comfy_binary)^2),
    mean((model_df_preds$pred_mixed_known - model_df_preds$not_comfy_binary)^2),
    mean((model_df_preds$pred_mixed_population - model_df_preds$not_comfy_binary)^2)
  )
)

write_csv(brier_scores, "mixed_model_outputs/tables/brier_scores_fixed_vs_mixed.csv")

label_mismatch <- function(actual, predicted) {
  case_when(
    actual == 1 & predicted == 1 ~ "true_discomfort",
    actual == 0 & predicted == 0 ~ "true_comfort",
    actual == 0 & predicted == 1 ~ "false_discomfort",
    actual == 1 & predicted == 0 ~ "false_comfort",
    TRUE ~ NA_character_
  )
}

model_df_preds <- model_df_preds %>%
  mutate(
    class_fixed_05 = if_else(pred_fixed >= 0.5, 1L, 0L),
    class_mixed_known_05 = if_else(pred_mixed_known >= 0.5, 1L, 0L),
    class_mixed_population_05 = if_else(pred_mixed_population >= 0.5, 1L, 0L),
    mismatch_fixed_05 = label_mismatch(not_comfy_binary, class_fixed_05),
    mismatch_mixed_known_05 = label_mismatch(not_comfy_binary, class_mixed_known_05),
    mismatch_mixed_population_05 = label_mismatch(not_comfy_binary, class_mixed_population_05)
  )

mismatch_summary <- bind_rows(
  model_df_preds %>%
    count(mismatch_fixed_05, name = "n") %>%
    mutate(model = "Fixed model", mismatch_type = mismatch_fixed_05) %>%
    select(model, mismatch_type, n),

  model_df_preds %>%
    count(mismatch_mixed_known_05, name = "n") %>%
    mutate(model = "Mixed model: known participants", mismatch_type = mismatch_mixed_known_05) %>%
    select(model, mismatch_type, n),

  model_df_preds %>%
    count(mismatch_mixed_population_05, name = "n") %>%
    mutate(model = "Mixed model: population-level", mismatch_type = mismatch_mixed_population_05) %>%
    select(model, mismatch_type, n)
) %>%
  group_by(model) %>%
  mutate(total = sum(n), percent = n / total) %>%
  ungroup()

write_csv(mismatch_summary, "mixed_model_outputs/tables/mismatch_summary_threshold_05.csv")

participant_mismatch <- model_df_preds %>%
  group_by(PID) %>%
  summarise(
    n = n(),
    not_comfy_events = sum(not_comfy_binary),
    not_comfy_rate = mean(not_comfy_binary),
    fixed_false_comfort = sum(mismatch_fixed_05 == "false_comfort", na.rm = TRUE),
    mixed_known_false_comfort = sum(mismatch_mixed_known_05 == "false_comfort", na.rm = TRUE),
    mixed_population_false_comfort = sum(mismatch_mixed_population_05 == "false_comfort", na.rm = TRUE),
    fixed_false_comfort_rate = fixed_false_comfort / n,
    mixed_known_false_comfort_rate = mixed_known_false_comfort / n,
    mixed_population_false_comfort_rate = mixed_population_false_comfort / n,
    .groups = "drop"
  ) %>%
  arrange(desc(not_comfy_rate))

write_csv(participant_mismatch, "mixed_model_outputs/tables/participant_mismatch_summary_threshold_05.csv")
