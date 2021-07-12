# Title:        Examine number and type of cross-loadings
# Author:       Ewan Carr
# Started:      2017-10-11

library(tidyverse)
library(here)
walk(list("count_loadings.R", "misc_functions.R"),
    ~ source(here("analysis", "functions", .x)))

# Load output (with and without covariances) ----------------------------------
load(here("analysis", "saved_output", "model_b.Rdata"), verbose = TRUE)
load(here("analysis", "saved_output", "model_b_final.Rdata"), verbose = TRUE)

# Extract and categorise loadings for each pair -------------------------------

loadings <- outputs %>%
        # Select output with 12/52 week data only, with covariances
    keep(~ grepl(".*12w.*WITH_XLOAD.*", attributes(.x)$filename)) %>%
    map(extract_loadings) %>%
    map(add_types) %>%
    set_names(~ str_replace(., "^.*((0w|12w))\\.([A-Z_]+).out$", "\\2_\\3"))

loadings_by_type <- map_dfr(loadings, count_loadings_by_type, .id = "id") %>%
    mutate_all(funs(replace(., is.na(.), 0))) %>%
    tidyr::extract(id, c("timepoint", "pair"),
                   "([02]w)_([A-Z]+_[LS]_[A-Z]+)+_WITH_XLOAD")

counts_by_type <- outputs %>%
    keep(~ grepl(".*12w.*", attributes(.x)$filename)) %>%
    count_loadings() %>%
    full_join(loadings_by_type)
