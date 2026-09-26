# 04_portability_validation.R
# Purpose: compare random row validation with participant-grouped validation and portability threshold sensitivity.

library(tidyverse)
library(lme4)
library(yardstick)

dir.create("portability_outputs/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("portability_outputs/figures", recursive = TRUE, showWarnings = FALSE)

model_df <- read_csv("outputs/tables/model_df_prepared.csv") %>%
  mutate(PID = factor(PID), location = factor(location))

classification_metrics <- function(data, pred_col, model_name, validation_type, threshold = 0.25) {
  pred_col <- rlang::ensym(pred_col)

  temp <- data %>%
    mutate(
      pred_prob = !!pred_col,
      actual = not_comfy_binary,
      truth = factor(if_else(actual == 1, "not_comfy", "comfy"), levels = c("not_comfy", "comfy")),
      pred_class = if_else(pred_prob >= threshold, 1L, 0L)
    )

  tp <- sum(temp$actual == 1 & temp$pred_class == 1, na.rm = TRUE)
  tn <- sum(temp$actual == 0 & temp$pred_class == 0, na.rm = TRUE)
  fp <- sum(temp$actual == 0 & temp$pred_class == 1, na.rm = TRUE)
  fn <- sum(temp$actual == 1 & temp$pred_class == 0, na.rm = TRUE)

  recall <- if_else((tp + fn) > 0, tp / (tp + fn), NA_real_)
  specificity <- if_else((tn + fp) > 0, tn / (tn + fp), NA_real_)
  precision <- if_else((tp + fp) > 0, tp / (tp + fp), NA_real_)
  f1 <- if_else(!is.na(precision) & !is.na(recall) & (precision + recall) > 0,
                2 * precision * recall / (precision + recall), NA_real_)

  roc_auc_value <- tryCatch(
    yardstick::roc_auc(temp, truth = truth, pred_prob, event_level = "first")$.estimate,
    error = function(e) NA_real_
  )
  pr_auc_value <- tryCatch(
    yardstick::pr_auc(temp, truth = truth, pred_prob, event_level = "first")$.estimate,
    error = function(e) NA_real_
  )

  tibble(
    model = model_name,
    validation_type = validation_type,
    threshold = threshold,
    rows = nrow(temp),
    participants = n_distinct(temp$PID),
    events = sum(temp$actual, na.rm = TRUE),
    roc_auc = roc_auc_value,
    pr_auc = pr_auc_value,
    brier_score = mean((temp$pred_prob - temp$actual)^2, na.rm = TRUE),
    accuracy = (tp + tn) / (tp + tn + fp + fn),
    balanced_accuracy = mean(c(recall, specificity), na.rm = TRUE),
    recall = recall,
    precision = precision,
    f1 = f1,
    false_comfort = fn,
    false_discomfort = fp,
    true_discomfort = tp,
    true_comfort = tn
  )
}

make_stratified_folds <- function(data, k = 5) {
  data %>%
    group_by(not_comfy_binary) %>%
    mutate(fold = sample(rep(1:k, length.out = n()))) %>%
    ungroup()
}

run_random_row_cv <- function(data, k = 5, repeats = 20) {
  set.seed(123)

  map_dfr(1:repeats, function(rep_id) {
    folded <- make_stratified_folds(data, k = k)

    map_dfr(1:k, function(fold_id) {
      train <- folded %>% filter(fold != fold_id)
      test <- folded %>% filter(fold == fold_id)

      m_fixed <- glm(
        not_comfy_binary ~ temp_nb_z + humidity_nb_z + voc_nb_z + nox_nb_z +
          skin_proxy_z + location + local_hour_z,
        data = train,
        family = binomial()
      )

      m_mixed <- glmer(
        not_comfy_binary ~ temp_nb_z + humidity_nb_z + voc_nb_z + nox_nb_z +
          skin_proxy_z + location + local_hour_z + (1 | PID),
        data = train,
        family = binomial(),
        control = glmerControl(optimizer = "bobyqa")
      )

      test %>%
        mutate(
          repeat_id = rep_id,
          fold_id = fold_id,
          pred_fixed = predict(m_fixed, newdata = test, type = "response"),
          pred_mixed_known = predict(m_mixed, newdata = test, type = "response", re.form = NULL, allow.new.levels = TRUE),
          pred_mixed_population = predict(m_mixed, newdata = test, type = "response", re.form = NA, allow.new.levels = TRUE)
        )
    })
  })
}

run_leave_one_participant_out_cv <- function(data) {
  pids <- unique(data$PID)

  map_dfr(pids, function(heldout_pid) {
    train <- data %>% filter(PID != heldout_pid)
    test <- data %>% filter(PID == heldout_pid)

    if (n_distinct(train$not_comfy_binary) < 2) return(tibble())

    m_fixed <- glm(
      not_comfy_binary ~ temp_nb_z + humidity_nb_z + voc_nb_z + nox_nb_z +
        skin_proxy_z + location + local_hour_z,
      data = train,
      family = binomial()
    )

    m_mixed <- glmer(
      not_comfy_binary ~ temp_nb_z + humidity_nb_z + voc_nb_z + nox_nb_z +
        skin_proxy_z + location + local_hour_z + (1 | PID),
      data = train,
      family = binomial(),
      control = glmerControl(optimizer = "bobyqa")
    )

    test %>%
      mutate(
        heldout_PID = heldout_pid,
        pred_fixed = predict(m_fixed, newdata = test, type = "response"),
        pred_mixed_population = predict(m_mixed, newdata = test, type = "response", re.form = NA, allow.new.levels = TRUE)
      )
  })
}

random_cv_preds <- run_random_row_cv(model_df, k = 5, repeats = 20)
grouped_cv_preds <- run_leave_one_participant_out_cv(model_df)

write_csv(random_cv_preds, "portability_outputs/tables/random_row_cv_predictions.csv")
write_csv(grouped_cv_preds, "portability_outputs/tables/participant_grouped_cv_predictions.csv")

portability_metrics <- bind_rows(
  classification_metrics(random_cv_preds, pred_fixed, "Fixed model", "Random row CV", threshold = 0.25),
  classification_metrics(random_cv_preds, pred_mixed_known, "Mixed model: known participants", "Random row CV", threshold = 0.25),
  classification_metrics(random_cv_preds, pred_mixed_population, "Mixed model: population-level", "Random row CV", threshold = 0.25),
  classification_metrics(grouped_cv_preds, pred_fixed, "Fixed model", "Participant-grouped CV", threshold = 0.25),
  classification_metrics(grouped_cv_preds, pred_mixed_population, "Mixed model: population-level", "Participant-grouped CV", threshold = 0.25)
)

write_csv(portability_metrics, "portability_outputs/tables/portability_metrics_summary.csv")

grouped_participant_summary <- grouped_cv_preds %>%
  mutate(
    fixed_class_025 = if_else(pred_fixed >= 0.25, 1L, 0L),
    mixed_pop_class_025 = if_else(pred_mixed_population >= 0.25, 1L, 0L)
  ) %>%
  group_by(PID) %>%
  summarise(
    n = n(),
    not_comfy_events = sum(not_comfy_binary),
    not_comfy_rate = mean(not_comfy_binary),
    fixed_false_comfort = sum(not_comfy_binary == 1 & fixed_class_025 == 0),
    fixed_false_discomfort = sum(not_comfy_binary == 0 & fixed_class_025 == 1),
    mixed_pop_false_comfort = sum(not_comfy_binary == 1 & mixed_pop_class_025 == 0),
    mixed_pop_false_discomfort = sum(not_comfy_binary == 0 & mixed_pop_class_025 == 1),
    fixed_mean_pred = mean(pred_fixed),
    mixed_pop_mean_pred = mean(pred_mixed_population),
    .groups = "drop"
  ) %>%
  arrange(desc(not_comfy_rate))

write_csv(grouped_participant_summary, "portability_outputs/tables/grouped_cv_participant_summary.csv")

threshold_metrics <- function(data, pred_col, model_name, thresholds = seq(0.05, 0.95, by = 0.05)) {
  pred_col <- rlang::ensym(pred_col)

  map_dfr(thresholds, function(thresh) {
    temp <- data %>%
      mutate(
        pred_prob = !!pred_col,
        pred_class = if_else(pred_prob >= thresh, 1L, 0L),
        actual = not_comfy_binary
      )

    tp <- sum(temp$actual == 1 & temp$pred_class == 1, na.rm = TRUE)
    tn <- sum(temp$actual == 0 & temp$pred_class == 0, na.rm = TRUE)
    fp <- sum(temp$actual == 0 & temp$pred_class == 1, na.rm = TRUE)
    fn <- sum(temp$actual == 1 & temp$pred_class == 0, na.rm = TRUE)

    sensitivity <- if_else((tp + fn) > 0, tp / (tp + fn), NA_real_)
    specificity <- if_else((tn + fp) > 0, tn / (tn + fp), NA_real_)
    precision <- if_else((tp + fp) > 0, tp / (tp + fp), NA_real_)
    recall <- sensitivity
    f1 <- if_else(!is.na(precision) & !is.na(recall) & (precision + recall) > 0,
                  2 * precision * recall / (precision + recall), NA_real_)
    balanced_accuracy <- mean(c(sensitivity, specificity), na.rm = TRUE)

    tibble(
      model = model_name,
      threshold = thresh,
      recall = recall,
      precision = precision,
      f1 = f1,
      balanced_accuracy = balanced_accuracy,
      false_comfort = fn,
      false_discomfort = fp
    )
  })
}

portability_threshold_all <- bind_rows(
  threshold_metrics(random_cv_preds, pred_fixed, "Fixed model: random row CV"),
  threshold_metrics(random_cv_preds, pred_mixed_known, "Mixed-known: random row CV"),
  threshold_metrics(random_cv_preds, pred_mixed_population, "Mixed-population: random row CV"),
  threshold_metrics(grouped_cv_preds, pred_fixed, "Fixed model: participant-grouped CV"),
  threshold_metrics(grouped_cv_preds, pred_mixed_population, "Mixed-population: participant-grouped CV")
)

write_csv(portability_threshold_all, "portability_outputs/tables/portability_threshold_sensitivity.csv")

portability_best_thresholds <- portability_threshold_all %>%
  group_by(model) %>%
  summarise(
    best_f1 = max(f1, na.rm = TRUE),
    threshold_best_f1 = threshold[which.max(f1)],
    best_balanced_accuracy = max(balanced_accuracy, na.rm = TRUE),
    threshold_best_balanced_accuracy = threshold[which.max(balanced_accuracy)],
    best_recall = max(recall, na.rm = TRUE),
    threshold_best_recall = threshold[which.max(recall)],
    .groups = "drop"
  )

write_csv(portability_best_thresholds, "portability_outputs/tables/portability_best_thresholds.csv")

portability_long <- portability_metrics %>%
  select(model, validation_type, roc_auc, pr_auc, brier_score, balanced_accuracy, recall, precision, f1) %>%
  pivot_longer(
    cols = c(roc_auc, pr_auc, brier_score, balanced_accuracy, recall, precision, f1),
    names_to = "metric",
    values_to = "value"
  )

p_portability <- ggplot(portability_long, aes(x = model, y = value)) +
  geom_col() +
  facet_grid(metric ~ validation_type, scales = "free_y") +
  labs(
    title = "Portability Evaluation: Random Row vs Participant-Grouped Validation",
    subtitle = "Participant-grouped validation tests prediction for unfamiliar bodies",
    x = "Model",
    y = "Metric value"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))

ggsave("portability_outputs/figures/portability_metrics_random_vs_grouped.png", p_portability, width = 12, height = 10, dpi = 300)

p_portability_f1 <- portability_threshold_all %>%
  filter(!is.na(f1)) %>%
  ggplot(aes(x = threshold, y = f1, group = model)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ model) +
  labs(
    title = "Portability Threshold Sensitivity: F1 for Not-Comfy",
    subtitle = "Undefined F1 values omitted where models predict no not-comfy cases",
    x = "Prediction threshold",
    y = "F1 for not-comfy"
  ) +
  theme_minimal()

ggsave("portability_outputs/figures/portability_threshold_f1.png", p_portability_f1, width = 12, height = 8, dpi = 300)
