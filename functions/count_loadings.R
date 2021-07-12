# Functions needed to count and categorise factor loadings
# for the "PACE overlap" analysis

# Function to count the number of factor loadings
count_loadings <- function(list_of_outputs) {
    map_dfr(list_of_outputs, ~ .x$parameters$unstandardized %>%
            filter(grepl("\\.BY$", paramHeader)) %>%
            nrow()) %>%
    gather(key, value) %>%
    tidyr::extract(key, c("timepoint", "pair", "model"),
                   "^.*([012]+w).([A-Z]+_[SL]_[A-Z]+)_([A-Z]+_XLOAD).out$") %>%
    spread(model, value) %>%
    make_lowercase() %>%
    mutate(added = with_xload - no_xload,
           `% added` = round(100 * (added / with_xload), 0)) %>%
    arrange(-`% added`) %>%
    return()
}


# Function to extract factor loadings (i.e. the values of the loadings)
extract_loadings <- function(f) {
    # Detect outcome/mediator --------------------------------------------------
    parts <- str_match(f$summaries$Filename, "(^[A-Z]+_[SL]+)_(CFQ|PF|WSAQ).*$")
    outcome <- parts[3]
    mediator <- parts[2]
    # Extract parameter estimates from the model -------------------------------
    params <- f$parameters$stdyx.standardized %>%
        filter(grepl("\\.BY", paramHeader))
    params$actual <- str_replace(params$paramHeader, ".BY", "")
    params$mediator <- mediator
    # Detect INTENDED factor ---------------------------------------------------
                                            # This is the factor that each
                                            # item SHOULD be loading onto
                                            # (i.e. if there no cross-loadings).
    for_merge <- params %>%
        mutate(intended = case_when(
            grepl("CFQ", param) ~ "CFQ",
            grepl("WSAQ", param) ~ "WSAQ",
            grepl("PF", param) ~ "PF",
            TRUE ~ mediator),
            xload = if_else(actual == intended, FALSE, TRUE))
    # Reshape this table, so we can easily compare the primary factor loading
    # and the cross-loading ----------------------------------------------------
    a <- for_merge %>%
        filter(xload) %>%
        select(actual, intended, param, est) %>%
        arrange(param)
    b <- for_merge %>%
        filter(param %in% a$param,
               xload == FALSE) %>%
        arrange(param)
    summary_table <- bind_cols(a, b) %>%
        select(intended, cross = actual, item = param,
               load_prim = est1, load_cross = est)
    return(summary_table)
}


## Determine the type of each cross-loading

# Weak:         Loads mainly onto primary factor, with small cross-loading
#               (best case).
# Shared:       Shared loading, with similar sized loadings for main factor
#               and cross-loading.
# Switched:     Switches loadings, where primary factor loading is much
#               smaller than cross-loading (worst case)
#
# We first categorise each loading as WEAK, MODERATE, or STRONG:
#
# | Label       | Loading         |
# |-------------+-----------------|
# | Non-salient | 0.0 >= x <  0.2 |
# | Weak        | 0.2 >= x <  0.3 |
# | Moderate    | 0.3 >= x <  0.6 |
# | Strong      | 0.6 >= x <= 1.0 |
#
# We then categorise each combination of loadings:

# | Primary     | Cross       | Type for pair           |
# |-------------+-------------+-------------------------|
# | Non-salient | Non-salient | Non-salient             |
# | Non-salient | Weak        | Switched                |
# | Non-salient | Moderate    | Switched                |
# | Non-salient | Strong      | Switched                |
# | Weak        | Non-salient | No cross-loading        |
# | Weak        | Weak        | Shared                  |
# | Weak        | Moderate    | Switched                |
# | Weak        | Strong      | Switched                |
# | Moderate    | Non-salient | No cross-loading        |
# | Moderate    | Weak        | No cross-loading        |
# | Moderate    | Moderate    | Shared                  |
# | Moderate    | Strong      | Strong cross (switched) |
# | Strong      | Non-salient | No cross-loading        |
# | Strong      | Weak        | No cross-loading        |
# | Strong      | Moderate    | Strong cross            |
# | Strong      | Strong      | Shared                  |

determine_loading_type <- function(loading) {
    loading <- abs(loading)
    return(case_when(
        (loading >= 0.6)                   ~ "Strong",
        (loading >= 0.3) & (loading < 0.6) ~ "Moderate",
        (loading >= 0.2) & (loading < 0.3) ~ "Weak",
        (loading <  0.2)                   ~ "Non-salient"))
}

determine_pair_type <- function(primary, cross) {
    return(case_when(
        primary == "Non-salient" & cross == "Non-salient" ~ "Non-salient",
        primary == "Non-salient" & cross == "Weak"        ~ "Switched",
        primary == "Non-salient" & cross == "Moderate"    ~ "Switched",
        primary == "Non-salient" & cross == "Strong"      ~ "Switched",
        primary == "Weak"        & cross == "Non-salient" ~ "No cross-loading",
        primary == "Weak"        & cross == "Weak"        ~ "Shared",
        primary == "Weak"        & cross == "Moderate"    ~ "Switched",
        primary == "Weak"        & cross == "Strong"      ~ "Switched",
        primary == "Moderate"    & cross == "Non-salient" ~ "No cross-loading",
        primary == "Moderate"    & cross == "Weak"        ~ "No cross-loading",
        primary == "Moderate"    & cross == "Moderate"    ~ "Shared",
        primary == "Moderate"    & cross == "Strong"      ~ "Strong cross switched",
        primary == "Strong"      & cross == "Non-salient" ~ "No cross-loading",
        primary == "Strong"      & cross == "Weak"        ~ "No cross-loading",
        primary == "Strong"      & cross == "Moderate"    ~ "Strong cross",
        primary == "Strong"      & cross == "Strong"      ~ "Shared"))
}

add_types <- function(x) {
    return(x %>%
               mutate(prim_type = determine_loading_type(load_prim),
                      cross_type = determine_loading_type(load_cross),
                      pair_type = determine_pair_type(prim_type, cross_type)))
}


# Function to replace NA with 0.
na_zero <- function(x) {
    x[is.na(x)] <- 0
    return(x)
}

count_loadings_by_type <- function(l) {
    if (nrow(l) == 0) {
        return(data_frame(`No cross-loading` = 0)) 
    }
    else {
        l %>%
            dplyr::count(pair_type) %>%
            spread(pair_type, n) %>%
            return()
    }
}


get_counts_by_type <- function(counts, loadings_table) {
    counts %>%
       make_lowercase() %>%
        mutate(Δ = with_xload - no_xload) %>%
        full_join(map_dfr(loadings_table,
                          count_loadings_by_type,
                          .id = "id") %>%
                  tidyr::extract(id,
                                 c("timepoint", "pair"), "([02]w)_(.*)")) %>%
        mutate_all(funs(replace(., is.na(.), 0))) %>%
        arrange(-Δ)
}
