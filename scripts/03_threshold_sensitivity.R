# 03_threshold_sensitivity.R
# Purpose: evaluate how classification results change across thresholds.

library(tidyverse)
library(yardstick)

model_df_preds <- read_csv("mixed_model_outputs/tables/model_predictions_fixed_vs_mixed.csv")

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
    f1 <- if_else(
      !is.na(precision) & !is.na(recall) & (precision + recall) > 0,
      2 * precision * recall / (precision + recall),
      NA_real_
    )
    balanced_accuracy <- mean(c(sensitivity, specificity), na.rm = TRUE)
    accuracy <- (tp + tn) / (tp + tn + fp + fn)

    tibble(
      model = model_name,
      threshold = thresh,
      tp = tp,
      tn = tn,
      fp = fp,
      fn = fn,
      false_comfort = fn,
      false_discomfort = fp,
      sensitivity = sensitivity,
      specificity = specificity,
      precision = precision,
      recall = recall,
      f1 = f1,
      balanced_accuracy = balanced_accuracy,
      accuracy = accuracy
    )
  })
}

threshold_all <- bind_rows(
  threshold_metrics(model_df_preds, pred_fixed, "Fixed model"),
  threshold_metrics(model_df_preds, pred_mixed_known, "Mixed model: known participants"),
  threshold_metrics(model_df_preds, pred_mixed_population, "Mixed model: population-level")
)

write_csv(threshold_all, "mixed_model_outputs/tables/threshold_sensitivity_fixed_vs_mixed.csv")

best_thresholds <- threshold_all %>%
  group_by(model) %>%
  summarise(
    best_f1 = max(f1, na.rm = TRUE),
    threshold_best_f1 = threshold[which.max(f1)],
    best_balanced_accuracy = max(balanced_accuracy, na.rm = TRUE),
    threshold_best_balanced_accuracy = threshold[which.max(balanced_accuracy)],
    .groups = "drop"
  )

write_csv(best_thresholds, "mixed_model_outputs/tables/best_thresholds_fixed_vs_mixed.csv")

eval_df <- model_df_preds %>%
  mutate(
    truth = factor(
      if_else(not_comfy_binary == 1, "not_comfy", "comfy"),
      levels = c("not_comfy", "comfy")
    )
  )

auc_metrics <- bind_rows(
  eval_df %>% roc_auc(truth = truth, pred_fixed, event_level = "first") %>% mutate(model = "Fixed model", metric_type = "ROC-AUC"),
  eval_df %>% roc_auc(truth = truth, pred_mixed_known, event_level = "first") %>% mutate(model = "Mixed model: known participants", metric_type = "ROC-AUC"),
  eval_df %>% roc_auc(truth = truth, pred_mixed_population, event_level = "first") %>% mutate(model = "Mixed model: population-level", metric_type = "ROC-AUC"),
  eval_df %>% pr_auc(truth = truth, pred_fixed, event_level = "first") %>% mutate(model = "Fixed model", metric_type = "PR-AUC"),
  eval_df %>% pr_auc(truth = truth, pred_mixed_known, event_level = "first") %>% mutate(model = "Mixed model: known participants", metric_type = "PR-AUC"),
  eval_df %>% pr_auc(truth = truth, pred_mixed_population, event_level = "first") %>% mutate(model = "Mixed model: population-level", metric_type = "PR-AUC")
) %>%
  select(model, metric_type, .estimate)

write_csv(auc_metrics, "mixed_model_outputs/tables/auc_pr_auc_fixed_vs_mixed.csv")

dir.create("mixed_model_outputs/figures", showWarnings = FALSE)

p_threshold_recall <- ggplot(threshold_all, aes(x = threshold, y = recall, group = model)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Recall for Not-Comfy Across Prediction Thresholds",
    subtitle = "Higher recall means fewer false-comfort errors",
    x = "Prediction threshold",
    y = "Recall / sensitivity for not-comfy"
  ) +
  facet_wrap(~ model) +
  theme_minimal()

ggsave("mixed_model_outputs/figures/threshold_recall_fixed_vs_mixed.png", p_threshold_recall, width = 10, height = 6, dpi = 300)

p_threshold_f1 <- threshold_all %>%
  filter(!is.na(f1)) %>%
  ggplot(aes(x = threshold, y = f1, group = model)) +
  geom_line() +
  geom_point() +
  labs(
    title = "F1 Score Across Prediction Thresholds",
    subtitle = "Undefined F1 values omitted where models predict no not-comfy cases",
    x = "Prediction threshold",
    y = "F1 for not-comfy"
  ) +
  facet_wrap(~ model) +
  theme_minimal()

ggsave("mixed_model_outputs/figures/threshold_f1_fixed_vs_mixed.png", p_threshold_f1, width = 10, height = 6, dpi = 300)
