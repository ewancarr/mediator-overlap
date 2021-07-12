# Create tables/figures/outputs for "PACE overlap" paper

# This file prepares figures and tables for the PACE paper, as well as various
# output files (stored as .Rdata files) for use in the RMarkdown document.

library(tidyverse)
library(assertthat)
library(here)
theme_set(theme_gray(base_family = "Source Sans Pro"))

# The paper will contain three sets of models.
#
# 1. A single factor for each pair.
# 2. Two-factor CFA model for each pair,WITHOUT cross-loadings.
# 3. Two-factor CFA model for each pair, WITH cross-loadings.
#
# Some of these will be presented in the text only (e.g. fit statistics for the
# single factor models).
#
# TABLES/FIGURES FOR THE PAPER
#
# Table 1:   Sample characteristics?
# Table 2:   Fit statistics for the two-factor model?
# Figure 1:  Percent of overlapping loadings, by outcome

#  ┌─────────────────────────────────────────┐
#  │                                         │
#  │                Functions                │
#  │                                         │
#  └─────────────────────────────────────────┘

relabel_mediators <- function(var) {
    return(case_when(var == "AVOID"  ~ "Avoidance/resting behaviour",
                     var == "FA"     ~ "Fear avoidance",
                     var == "SF"     ~ "Symptom focusing",
                     var == "EMBAR"  ~ "Embarrassment avoidance",
                     var == "DAMAGE" ~ "Damage",
                     var == "CATA"   ~ "Catastrophising",
                     var == "ALLOR"  ~ "All-or-nothing behaviour"))
}

relabel_type <- function(var) {
    return(case_when(var == "L" ~ "Long version",
                     var == "S" ~ "Short version (3-item)"))
}

#  ┌──────────────────────────────────────────────────┐
#  │                                                  │
#  │                Summary statistics                │
#  │                                                  │
#  └──────────────────────────────────────────────────┘

# These are loaded in the RMarkdown document, from the .Rdata file.

load("data/clean/pace.Rdata", verbose = TRUE)
load("data/pace_raw.Rdata", verbose = TRUE)

n_total <- nrow(df)

summary_statistics <- df %>%
    dplyr::select(r_age, r_sex) %>%
    summarise_all(funs(mean, sd))

save(n_total,
     summary_statistics, file = here("analysis",
                                     "saved_output",
                                     "summary_statistics.Rdata"))

#  ┌──────────────────────────────────────────────────────────────────────────┐
#  │                                                                          │
#  │                Table 1: Number and type of cross-loadings                │
#  │                                                                          │
#  └──────────────────────────────────────────────────────────────────────────┘

# Process output files
source(here("analysis", "02-Examine-cross-loadings.R"))
stopifnot(exists("counts_by_type"))

# Calculate precentage of cross-loading, for all pairs
all_pairs <- counts_by_type %>%
    mutate(problematic = `Shared` + `Strong cross`,
           percent = (problematic / no_xload) * 100) %>%
    arrange(-percent)

make_table <- function(df) {
    df %>%
    tidyr::extract(pair,
                   c("mediator", "type", "outcome"),
                   "(.*)_([LS])_(.*)") %>%
    mutate(mediator = relabel_mediators(mediator),
           type = if_else(type == "S", "(short)", ""),
           mediator = paste0(mediator, " ", type)) %>%
    select(-timepoint, -`% added`, -problematic, -percent, -type)
}

# Table showing only pairs with >0 cross-loadings
table1 <- all_pairs %>%
    filter(percent > 0) %>%
    make_table() %>%
    rename(Mediator = mediator,
           Outcome = outcome,
           `No.\ factor loadings before iterative procedure` = no_xload,
           `No.\ factor loadings after` = with_xload,
           `No.\ cross-loadings added` = added)

write_csv(table1, here("writing", "tables", "table_1.csv"))

save(all_pairs, table1, file = here("analysis", "saved_output", "step3.Rdata"))

# Create Supplementary Table 2 ================================================

s2 <- all_pairs %>%
    make_table() %>%
    arrange(-added)

write_csv(s2, "s2.csv")

#  ┌──────────────────────────────────────────────────────────────────────────┐
#  │                                                                          │
#  │               MODEL 2: Fit statistics for two-factor                     │
#  │                        model WITHOUT cross-loadings                      │
#  │                                                                          │
#  └──────────────────────────────────────────────────────────────────────────┘

# Load output files
load(here("analysis", "saved_output", "model_b.Rdata"), verbose = TRUE)

# Tabulate fit statistics
model_b_fit <- model_b %>%
    map_dfr("summaries", .id = "path") %>%
    tidyr::extract(path,
            c("period", "mediator", "outcome"),
            ".*(0w|12w)\\.([A-Z_]+[LS])_([A-Z]+).*") %>%
    mutate(chi_relative = ChiSqM_Value / ChiSqM_DF) %>%
    select(period,
           mediator,
           outcome,
           CFI,
           RMSEA = RMSEA_Estimate,
           chi_relative)

write_csv(model_b_fit, "model_b.csv")

#  ┌─────────────────────────────────────────────────┐
#  │                                                 │
#  │      Table with question wording and factor     │
#  │      loadings for cross-loading items           │
#  │                                                 │
#  └─────────────────────────────────────────────────┘

source(here("analysis", "functions", "question_wordings.R"))
labels <- data_frame(question_wordings) %>%
    separate(question_wordings, c("item", "label"), " \\| ")

format_loading <- function(x) {
    sprintf("%1.2f", x)
}

loadings_with_item_wording <- map_dfr(loadings, ~.x, .id = "model") %>%
    filter(!(cross_type %in% c("Non-salient",
                               "No cross-loading",
                               "Weak"))) %>%
    arrange(intended, cross) %>%
    mutate(item = str_replace(item, "_0|_52", "")) %>%
    left_join(labels) %>%
    mutate_at(vars(load_prim, load_cross), format_loading) %>%
    tidyr::extract(cross, c("mediator", "type"), "([A-Z]+)_([S]*)") %>%
    mutate(type = if_else(type == "S", " (short version)", ""),
           mediator = relabel_mediators(mediator),
           mediator = paste0(mediator, type)) %>%
    select(Outcome = `intended`,
           Mediator = `mediator`,
           Item = item,
           `Question wording` = label,
           `Loading (outcome)` = load_prim,
           `Loading (mediator)` = load_cross,
           `Type of cross-loading` = pair_type)

write_csv(loadings_with_item_wording, "wordings.csv")

save(loadings_with_item_wording,
     file = here("analysis", "saved_output", "loadings_with_wording.Rdata"))
