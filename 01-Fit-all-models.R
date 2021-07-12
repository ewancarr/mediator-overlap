# Title:        Fit all models for 'PACE overlap' paper
# Author:       Ewan Carr

library(tidyverse)
library(here)
library(glue)
library(MplusAutomation)
library(assertthat)
source(here("analysis", "functions", "misc_functions.R"))
source(here("analysis", "functions", "fit_models.R"))

#  ┌─────────────────────────────────────────────────────────┐
#  │                                                         │
#  │                Load model specifications                │
#  │                                                         │
#  └─────────────────────────────────────────────────────────┘

# Load the list of single factor models
source(here("analysis", "functions", "model_spec.R"))

# Create separate versions of the model specification, for 0w and 52w ---------

spec_0w  <- model_spec                  # Based on baseline measures
spec_12w <- map(model_spec,             # Based on measures at 12w (mediators)
                change_timepoint)       # and 52w (outcomes)

#  ┌────────────────────────────────────────────────────┐
#  │                                                    │
#  │                Single factor models                │
#  │    (single for each measure fitted separately)     │
#  │                                                    │
#  └────────────────────────────────────────────────────┘

target <- here("analysis", "separate_factors")

# Write input files -----------------------------------------------------------
refresh_path(target)
names_statement <- get_names()
list_of_separate_factors  <- map(model_spec,
                                 separate_factors,
                                 names_statement)
walk(list_of_separate_factors, save_input_file)

# Fit the models --------------------------------------------------------------
runModels(target, Mplus_command = "/opt/mplus/7.3/mplus")

# Extract model outputs; save -------------------------------------------------
separate_factors <- readModels(target)
save(separate_factors, file = here("analysis",
                                   "saved_output",
                                   "separate_factors.Rdata"))

#              ┌───────────────────────────────────────┐
#              │                                       │
#              │                Model A                │
#              │  "Single CFA factor for each pair"    │
#              │                                       │
#              └───────────────────────────────────────┘

# This section fits a single CFA model for each mediator-outcome pair.
# These models SHOULD NOT FIT WELL.


# Create a list of pairwise combinations of each factor -----------------------

make_model_list <- function(x) {
    factor <- str_split(x[2], " ")[[1]][1]
    outcome <- grepl("CFQ|PF|WSAQ", factor)
    return(c(x, factor, outcome))
}

create_pairwise_combinations <- function(model_list) {
    outcomes <- Filter(function(x) x[4] == TRUE, model_list)
    mediators <- Filter(function(x) x[4] == FALSE, model_list)
    combined <- list()
    k <- 1
    for (i in 1:length(outcomes)) {
        for (j in 1:length(mediators)) {
            combined[[k]] <- list(mediators[[j]][3], mediators[[j]][2],
                                  outcomes[[i]][3], outcomes[[i]][2])
            k <- k + 1
        }
    }
    return(combined)
}

# For 0w
combined_0w <- map(spec_0w, make_model_list) %>%
    create_pairwise_combinations()

# For 12w
combined_12w <- map(spec_12w, make_model_list) %>%
    create_pairwise_combinations()

# Load names statement for Mplus data -----------------------------------------
names_statement <- get_names()

# Create new models -----------------------------------------------------------
new_models_0w <- map(combined_0w, generate_model_a)
new_models_12w <- map(combined_12w, generate_model_a)

# Write models to file --------------------------------------------------------
refresh_path(here("analysis", "model_a"))
refresh_path(here("analysis", "model_a", "0w"))
refresh_path(here("analysis", "model_a", "12w"))

map(new_models_0w, `[`, c(1, 3, 5)) %>%
    map(., save_model, where = here("analysis", "model_a", "0w"))

map(new_models_12w, `[`, c(1, 3, 5)) %>%
    map(., save_model, where = here("analysis", "model_a", "12w"))

# Fit the models --------------------------------------------------------------
list.files(here("analysis", "model_a"),
           full.names = TRUE,
           recursive = TRUE,
           pattern = ".*inp$") %>%
    map(fit_model)

# Gather model outputs
model_a <- readModels(target = here("analysis", "model_a"), recursive = TRUE)
save(model_a, file = here("analysis", "saved_output", "model_a.Rdata"))

#  ┌────────────────────────────────────────────────────────────────────────┐
#  │                                                                        │
#  │                                MODEL B                                 │
#  │                   Examine cross-loadings for each                      │
#  │                        mediator-outcome pair                           │
#  │                                                                        │
#  └────────────────────────────────────────────────────────────────────────┘

# This section fits a series of models to identify significant cross-loadings
# for each mediator- outcome pair, using an iterative procedure described by
# Silia at the last meeting (10th August 2017). See the corresponding RMarkdown
# file for more information.

# =============================================================================
# ================== 1: Models without any cross-loadings =====================
# =============================================================================

# Choose where to save input files
target <- here("analysis", "model_b", "1_no_crossloading")

# Get NAMES statement
names_statement <- get_names()

# Generate models without cross-loadings
no_crossloadings_0w <- map(combined_0w,
                        write_model_b_two_factor,
                        allow_covariance = TRUE)

no_crossloadings_12w <- map(combined_12w,
                        write_model_b_two_factor,
                        allow_covariance = TRUE)

# Delete old/existing models
refresh_path(here("analysis", "model_b", "1_no_crossloading"))
target_0w <- here("analysis", "model_b", "1_no_crossloading", "0w")
target_12w <- here("analysis", "model_b", "1_no_crossloading", "12w")
walk(list(target_0w, target_12w), refresh_path)

# Save the models
walk(no_crossloadings_0w, save_model, where = target_0w)
walk(no_crossloadings_12w, save_model, where = target_12w)

# Fit the models
list.files(here("analysis", "model_b", "1_no_crossloading"),
           full.names = TRUE, recursive = TRUE, pattern = ".*inp$") %>%
    map(fit_model)

# Gather model outputs
model_b <- readModels(target = here("analysis", "model_b", "1_no_crossloading"),
                      recursive = TRUE)
save(model_b, file = here("analysis", "saved_output", "model_b.Rdata"))

# =============================================================================
# =============== 2. Iterative procedure to add cross-loadings ================
# =============================================================================

# Load the required functions
source(here("analysis", "functions", "iterative_functions.R"))

# Extract list of valid factor names
factors <- plyr::laply(combined_0w, function(x) { c(x[1], x[3]) }) # Same for
                                                                   # 0w and 12w

# Helper function
run_iterations <- function(model_list, timepoint) {
    refresh_path(here("analysis", "model_b", "2_iterative", timepoint))
    map(1:length(model_list), test_pair, model_list)
}

# Run the iterative procedure: USING 0w DATA ----------------------------------
no_crossloadings <- map(combined_0w, write_model_b_two_factor,
                        allow_covariance = TRUE)
with_covariances_0w <- run_iterations(combined_0w, "0w")

# Run the iterative procedure: USING 12w/52w DATA -----------------------------
no_crossloadings <- map(combined_12w, write_model_b_two_factor,
                        allow_covariance = TRUE)
with_covariances_12w <- run_iterations(combined_12w, "12w")

# Check how many models successfully converged --------------------------------
table(map_lgl(with_covariances_0w, "convergence"))
table(map_lgl(with_covariances_12w, "convergence"))

# Save output -----------------------------------------------------------------
save(with_covariances_0w,
     with_covariances_12w,
     file = here("analysis", "saved_output", "all_iterations.Rdata"))

# =============================================================================
# ============= 3. Fit final versions of model (after iterative ===============
# =============    procedure has completed)                     ===============
# =============================================================================

add_stdyx <- function(model) {
    return(str_replace(model, "OUTPUT:", "OUTPUT:\nSTDYX;"))
}

write_final_models <- function(x, where) {
    filename <- paste0(where, "/",
                       x$no_loadings[1], "_",
                       x$no_loadings[2])
    cat(add_stdyx(x$no_loadings[3]),
        file = paste0(filename, "_NO_XLOAD.inp"))
    cat(add_stdyx(x$final_model[1]),
        file = paste0(filename, "_WITH_XLOAD.inp"))
}

# Load results of iterative procedures
load(here("analysis", "saved_output", "all_iterations.Rdata"),
     verbose = TRUE)

# Run models; save output -----------------------------------------------------
refresh_path(here("analysis", "model_b", "3_final", "with_covariances"))
walk(c("0w", "12w"),
    function(x) {
        # Save models to disk
        target <- here("analysis", "model_b", "3_final", "with_covariances", x)
        refresh_path(target)
        walk(str_eval(paste0("with_covariances_", x)),
             write_final_models, where = target)
        # Fit the models in Mplus
        list.files(target, pattern = "*.inp", full.names = TRUE) %>%
            walk(fit_model)
    })


# Gather output ---------------------------------------------------------------
where <- here("analysis", "model_b", "3_final", "with_covariances")
outputs <- readModels(where, recursive = TRUE, filefilter = ".out")

# Save output for reuse later -------------------------------------------------
save(outputs,
     file = here("analysis", "saved_output", "model_b_final.Rdata"))
