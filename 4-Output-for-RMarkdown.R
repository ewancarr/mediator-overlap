# Title:        Generate output for RMarkdown file
# Author:       Ewan Carr

# NOTE: This file should be fast. It will be run every time the RMarkdown file
#       is generated.

library(here)
library(tidyverse)

# Functions ===================================================================

num <- function(x) {
    x <- as.numeric(x)
    numbers <- data.frame(numbers = 1:9,
                          as_text = c("one", "two", "three",
                                      "four", "five", "six",
                                      "seven", "eight", "nine"))
    # stopifnot(is.numeric(x) & x > 0)
    if (x > 9) {
        return(as.character(x)) 
    } else {
        return(as.character(numbers[x, 2]))
    }
}

#  ┌──────────────────────────────────────────────────────────────────────────┐ 
#  │                                                                          │
#  │              Step 1: Fit statistics for single-factor model              │
#  │                                                                          │
#  └──────────────────────────────────────────────────────────────────────────┘

# Load output files
load(here("analysis", "saved_output", "model_a.Rdata"))

# Tabulate fit statistics
step1 <- model_a %>%
    map_dfr(~ as_tibble(.x$"summaries"), .id = "path") %>%
    extract(path, c("time", "mediator", "outcome"), 
            ".*([012]+w)\\.([A-Z_]+[LS]*)_([A-Z]+)\\.out") %>%
    filter(time == "2w") %>%
    mutate(chi_relative = ChiSqM_Value / ChiSqM_DF) %>%
    select(mediator, outcome, CFI, RMSEA = RMSEA_Estimate, chi_relative) %>%
    arrange(CFI)

step1_fit <- step1 %>%
    summarise_all(list(min = min, max = max), na.rm = TRUE)

#  ┌───────────────────────────────────────┐ 
#  │                Table 1                │
#  └───────────────────────────────────────┘

load(here("analysis", "saved_output", "step3.Rdata"), verbose = TRUE)

#  ┌───────────────────────────────────────┐ 
#  │                Table 3                │
#  └───────────────────────────────────────┘

load(here("analysis", "saved_output", "loadings_with_wording.Rdata"), 
     verbose = TRUE)

#  ┌──────────────────────────────────────────────────┐ 
#  │         Types of salient cross-loadings          │
#  │                  at Step 3                       │
#  └──────────────────────────────────────────────────┘

types <- all_pairs %>%
    filter(problematic > 0) %>%
    summarise_at(vars(`Shared`, `Strong cross`),
                 sum)
