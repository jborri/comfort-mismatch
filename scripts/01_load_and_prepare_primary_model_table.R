# 01_load_and_prepare_primary_model_table.R
# Purpose: load and prepare the primary model-ready dataset.

library(tidyverse)
library(readr)

input_file <- "audit_v2_primary_model_table.csv"

df <- read_csv(input_file)

model_df <- df %>%
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
  drop_na() %>%
  mutate(
    temp_nb_z = as.numeric(scale(temp_nb)),
    humidity_nb_z = as.numeric(scale(humidity_nb)),
    voc_nb_z = as.numeric(scale(voc_nb)),
    nox_nb_z = as.numeric(scale(nox_nb)),
    skin_proxy_z = as.numeric(scale(skin_proxy)),
    local_hour_z = as.numeric(scale(local_hour))
  )

sanity_summary <- model_df %>%
  summarise(
    rows = n(),
    participants = n_distinct(PID),
    not_comfy_events = sum(not_comfy_binary, na.rm = TRUE),
    not_comfy_rate = mean(not_comfy_binary, na.rm = TRUE)
  )

print(sanity_summary)

dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)

write_csv(model_df, "outputs/tables/model_df_prepared.csv")
write_csv(sanity_summary, "outputs/tables/model_df_sanity_summary.csv")
