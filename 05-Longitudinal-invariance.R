# Title:        Test longitudinal measurement invariance
# Author:       Ewan Carr

# This script fits a series of models to test longitudinal measurement
# invariance for the PACE mediators and outcomes (at 0, 12, and 52 weeks).
# These input files models have been specified by hand, as it would have been
# too complicated to specify them programatically.

library(tidyverse)
library(here)
library(MplusAutomation)
source(here("analysis", "functions", "misc_functions.R"))

# Run all input files =========================================================

input_files <- list.files(here("analysis", "longitudinal_invariance"),
                          recursive = TRUE,
                          pattern = ".*\\.inp",
                          full.names = TRUE) %>%
    discard(grepl("x_good_copy", .)) %>%
    map(fit_model)

# Gather output ===============================================================

output <- readModels(here("analysis", "longitudinal_invariance"),
                     recursive = TRUE)

# Make table of fit statistics (and DIFFTEST results) =========================

fit_statistics <- output %>%
    map_dfr("summaries", .id = "path") %>%
    select(χ2       = ChiSqM_Value,
           df       = ChiSqM_DF,
           cfi      = CFI,
           rmsea    = RMSEA_Estimate,
           diff     = ChiSqDiffTest_Value,
           diffdf   = ChiSqDiffTest_DF,
           diffpval = ChiSqDiffTest_PValue,
           filename = Filename) %>%
    mutate(relchi     = χ2 / df) %>%
    tidyr::extract(filename, c("factor", "invariance"),
                   "([A-Z_]+[SL]*)_([a-z]+).*") %>%
    gather(key, value, -factor, -invariance) %>%
    mutate(invariance = case_when(invariance == "configural" ~ "c",
                                  invariance == "metric" ~ "m",
                                  invariance == "scalar" ~ "s")) %>%
    unite(id, c(key, invariance)) %>%
    spread(id, value) %>%
    select(-diff_c, -diffdf_c, -diffpval_c)

write_csv(fit_statistics, "fit.csv")
