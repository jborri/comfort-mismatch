# 00_setup_packages.R
# Purpose: install/load packages needed for mixed-effects modeling and validation.

required_packages <- c(
  "tidyverse", "lme4", "performance", "broom.mixed",
  "ggeffects", "yardstick", "readr", "janitor", "scales"
)

installed <- rownames(installed.packages())
to_install <- setdiff(required_packages, installed)

if (length(to_install) > 0) {
  install.packages(to_install)
}

library(tidyverse)
library(lme4)
library(performance)
library(broom.mixed)
library(ggeffects)
library(yardstick)
library(readr)
library(janitor)
library(scales)

sessionInfo()
