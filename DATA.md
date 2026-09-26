# Data and reproducibility

## Analytic starting point

The original code package starts from `audit_v2_primary_model_table.csv` (613 observations, 19 participants). It uses pre-survey 5-minute sensor summaries joined to self-reported comfort. Scripts `01`–`07` reproduce the downstream analysis **from that model-ready table**. The raw sensor/survey fusion and construction of this table are not present in the curated code package.

## Why the input is absent

The model-ready table and generated participant-level outputs contain participant IDs, session IDs, timestamps, device identifiers, survey responses, and sensor records. They are excluded from this portfolio copy. The `.gitignore` also excludes those local inputs and generated output directories. Do not force-add them to a public repository. The selected aggregate result tables and figures contain no row-level predictions or participant-labeled plots.

Before a public data release, confirm the underlying source data terms and the permission to redistribute this derived table. A separate deidentified release would need a disclosure review, including combinations of timestamps and participant attributes. This README makes no claim that the public repo alone reproduces the reported metrics.

## Expected fields

The scripts refer to `PID`, `location`, `not_comfy_binary`, `local_hour`, `datetime`, `sht_nearbody_temperature_mean`, `sht_nearbody_humidity_mean`, `sgp_nearbody_voc_mean`, `sgp_nearbody_nox_mean`, and `thermopile_overall_skin_proxy_mean`. Some analyses derive lagged values or within-participant deviations. The binary outcome codes comfortable as `0` and not comfortable as `1`.

This repository contains analysis code and selected outputs, with no explicit software license. Contact the author before reusing the code beyond applicable default rights.
