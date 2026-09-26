# 05_sensitivity_pid29.R
# Comfort Mismatch Thesis
# Purpose: test whether calibration findings depend on PID 29, the most extreme high-discomfort participant.

# Load packages and prepared model table
source("scripts/00_setup_packages.R")
source("scripts/01_load_and_prepare_primary_model_table.R")

dir.create("sensitivity_outputs", showWarnings = FALSE)
dir.create("sensitivity_outputs/tables", recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------
# 1. Create sensitivity dataset excluding PID 29
# ------------------------------------------------------------

model_df_no29 <- model_df %>%
  filter(PID != "29")

sensitivity_sample_summary <- model_df_no29 %>%
  summarise(
    rows = n(),
    participants = n_distinct(PID),
    not_comfy_events = sum(not_comfy_binary),
    not_comfy_rate = mean(not_comfy_binary)
  )

print(sensitivity_sample_summary)

write_csv(
  sensitivity_sample_summary,
  "sensitivity_outputs/tables/sensitivity_sample_summary_without_pid29.csv"
)

write_csv(
  model_df_no29,
  "sensitivity_outputs/tables/model_df_without_pid29.csv"
)

# ------------------------------------------------------------
# 2. Fit fixed and mixed models without PID 29
# ------------------------------------------------------------

m1_fixed_no29 <- glm(
  not_comfy_binary ~ temp_nb_z + humidity_nb_z + voc_nb_z + nox_nb_z +
    skin_proxy_z + location + local_hour_z,
  data = model_df_no29,
  family = binomial()
)

m2_mixed_no29 <- glmer(
  not_comfy_binary ~ temp_nb_z + humidity_nb_z + voc_nb_z + nox_nb_z +
    skin_proxy_z + location + local_hour_z +
    (1 | PID),
  data = model_df_no29,
  family = binomial(),
  control = glmerControl(optimizer = "bobyqa")
)

# ------------------------------------------------------------
# 3. Model fit and participant-effect checks
# ------------------------------------------------------------

sensitivity_fit_no29 <- tibble(
  model = c("Fixed model without PID 29", "Mixed model without PID 29"),
  AIC = c(AIC(m1_fixed_no29), AIC(m2_mixed_no29)),
  BIC = c(BIC(m1_fixed_no29), BIC(m2_mixed_no29))
)

print(sensitivity_fit_no29)
print(performance::r2(m2_mixed_no29))
print(performance::icc(m2_mixed_no29))
print(isSingular(m2_mixed_no29))

write_csv(
  sensitivity_fit_no29,
  "sensitivity_outputs/tables/sensitivity_fit_without_pid29.csv"
)

# Save R2 / ICC / singularity check manually as a small table
sensitivity_model_checks_no29 <- tibble(
  check = c(
    "conditional_r2",
    "marginal_r2",
    "adjusted_icc",
    "unadjusted_icc",
    "is_singular"
  ),
  value = c(
    performance::r2(m2_mixed_no29)$R2_conditional,
    performance::r2(m2_mixed_no29)$R2_marginal,
    performance::icc(m2_mixed_no29)$ICC_adjusted,
    performance::icc(m2_mixed_no29)$ICC_unadjusted,
    as.numeric(isSingular(m2_mixed_no29))
  )
)

write_csv(
  sensitivity_model_checks_no29,
  "sensitivity_outputs/tables/sensitivity_model_checks_without_pid29.csv"
)

# ------------------------------------------------------------
# 4. Predictions and Brier scores without PID 29
# ------------------------------------------------------------

model_df_no29_preds <- model_df_no29 %>%
  mutate(
    pred_fixed = predict(m1_fixed_no29, type = "response"),
    pred_mixed_known = predict(m2_mixed_no29, type = "response", re.form = NULL),
    pred_mixed_population = predict(m2_mixed_no29, type = "response", re.form = NA)
  )

write_csv(
  model_df_no29_preds,
  "sensitivity_outputs/tables/sensitivity_predictions_without_pid29.csv"
)

brier_scores_no29 <- tibble(
  model = c(
    "Fixed model without PID 29",
    "Mixed model known participants without PID 29",
    "Mixed model population-level without PID 29"
  ),
  brier_score = c(
    mean((model_df_no29_preds$pred_fixed - model_df_no29_preds$not_comfy_binary)^2),
    mean((model_df_no29_preds$pred_mixed_known - model_df_no29_preds$not_comfy_binary)^2),
    mean((model_df_no29_preds$pred_mixed_population - model_df_no29_preds$not_comfy_binary)^2)
  )
)

print(brier_scores_no29)

write_csv(
  brier_scores_no29,
  "sensitivity_outputs/tables/sensitivity_brier_scores_without_pid29.csv"
)

# ------------------------------------------------------------
# 5. ROC-AUC and PR-AUC without PID 29
# ------------------------------------------------------------

eval_no29 <- model_df_no29_preds %>%
  mutate(
    truth = factor(
      if_else(not_comfy_binary == 1, "not_comfy", "comfy"),
      levels = c("not_comfy", "comfy")
    )
  )

auc_metrics_no29 <- bind_rows(
  eval_no29 %>%
    roc_auc(truth = truth, pred_fixed, event_level = "first") %>%
    mutate(model = "Fixed model without PID 29", metric_type = "ROC-AUC"),

  eval_no29 %>%
    roc_auc(truth = truth, pred_mixed_known, event_level = "first") %>%
    mutate(model = "Mixed known without PID 29", metric_type = "ROC-AUC"),

  eval_no29 %>%
    roc_auc(truth = truth, pred_mixed_population, event_level = "first") %>%
    mutate(model = "Mixed population without PID 29", metric_type = "ROC-AUC"),

  eval_no29 %>%
    pr_auc(truth = truth, pred_fixed, event_level = "first") %>%
    mutate(model = "Fixed model without PID 29", metric_type = "PR-AUC"),

  eval_no29 %>%
    pr_auc(truth = truth, pred_mixed_known, event_level = "first") %>%
    mutate(model = "Mixed known without PID 29", metric_type = "PR-AUC"),

  eval_no29 %>%
    pr_auc(truth = truth, pred_mixed_population, event_level = "first") %>%
    mutate(model = "Mixed population without PID 29", metric_type = "PR-AUC")
) %>%
  select(model, metric_type, .estimate)

print(auc_metrics_no29)

write_csv(
  auc_metrics_no29,
  "sensitivity_outputs/tables/sensitivity_auc_pr_auc_without_pid29.csv"
)

# ------------------------------------------------------------
# 6. Participant random intercepts without PID 29
# ------------------------------------------------------------

ranef_no29 <- ranef(m2_mixed_no29)$PID %>%
  as.data.frame() %>%
  rownames_to_column("PID") %>%
  rename(random_intercept = `(Intercept)`) %>%
  arrange(random_intercept)

write_csv(
  ranef_no29,
  "sensitivity_outputs/tables/sensitivity_random_intercepts_without_pid29.csv"
)

p_ranef_no29 <- ggplot(
  ranef_no29,
  aes(x = reorder(PID, random_intercept), y = random_intercept)
) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(size = 2) +
  coord_flip() +
  labs(
    title = "Participant-Specific Baseline Discomfort Without PID 29",
    subtitle = "Sensitivity analysis excluding the most extreme high-discomfort participant",
    x = "Participant ID",
    y = "Random intercept"
  ) +
  theme_minimal()

dir.create("sensitivity_outputs/figures", recursive = TRUE, showWarnings = FALSE)

ggsave(
  "sensitivity_outputs/figures/sensitivity_random_intercepts_without_pid29.png",
  p_ranef_no29,
  width = 8,
  height = 6,
  dpi = 300
)

# ------------------------------------------------------------
# 7. End message
# ------------------------------------------------------------

message("PID 29 sensitivity analysis complete. Outputs saved in sensitivity_outputs/.")