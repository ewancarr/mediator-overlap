mp <- "/opt/mplus/7.3/mplus"

# Function to extract to cross-loading from modification
# indices for a given file
get_top_mi <- function(filename) {
    mi <- extractModIndices(filename) %>%
        as.data.frame() %>%
        filter(operator == "BY",
               !(modV2 %in% factors))
    if (nrow(mi) == 0) {
        return(NA)
    }
    else {
        loadings <- mi %>%
            arrange(-MI) %>%
            mutate(loading = paste0(modV1, " ", operator, " ", modV2)) %>%
            select(loading) %>%
            head(1) %>%
            unlist()
        return(loadings[[1]])
    }
}

# Function to extract first cross-loading, given pair number (1 to 36)
get_first_mi <- function(id, timepoint) {
    a <- no_crossloadings[[id]][1]
    b <- no_crossloadings[[id]][2]
    filename <- paste0(a, "_", b, ".out")
    return(get_top_mi(here("analysis", "model_b", "1_no_crossloading",
                           timepoint, filename)))
}


# Function to update the model, for each step in the iterative procedure
update_the_model <- function(old_model, cross_loading) {
    # Write H0 model -----------------------------------------------------------
    # Add DIFFTEST statement
    model_h0 <- paste(old_model, "\nSAVEDATA: DIFFTEST IS DIFFTEST.dat;")
    # Add cross-loading
    model_h0 <- str_replace(model_h0,
                            "OUTPUT:", glue("{cross_loading};\nOUTPUT:"))
    # Write H1 model -----------------------------------------------------------
    # Add DIFFTEST statement
    model_h1 <- str_replace(old_model,
                            "MODEL:",
                            "ANALYSIS:\nDIFFTEST IS DIFFTEST.dat;\nMODEL:")
    model_h1 <- str_replace(model_h1, "SAVEDATA.*", "")
    # Constrain cross-loading to 0
    model_h1 <- str_replace(model_h1, "OUTPUT:",
                            glue("{cross_loading}@0;\n\nOUTPUT:"))
    # Create model for re-use --------------------------------------------------
    next_model <- str_replace(old_model, "OUTPUT:",
                              glue("{cross_loading};\nOUTPUT:"))
    # Return all models --------------------------------------------------------
    return(list(h0 = model_h0,
                h1 = model_h1,
                next = next_model,
                old = old_model))
}

check_convergence <- function(output) {
    # Return TRUE if model did not converge.
    the_file <- paste(readLines(output), collapse = " ")
    error_1 <- "NO CONVERGENCE"
    error_2 <- "THE STANDARD ERRORS OF THE MODEL PARAMETER ESTIMATES COULD NOT"
    error_3 <- "THE CHI-SQUARE DIFFERENCE TEST COULD NOT BE COMPUTED"
    return(case_when(
        grepl(error_1, the_file) ~ list(FALSE, error_1),
        grepl(error_2, the_file) ~ list(FALSE, error_2),
        grepl(error_3, the_file) ~ list(FALSE, error_3),
        TRUE ~ list(TRUE, "")))
}

run_difftest <- function(old_model, cross_loading, iteration,
                         current_pair, timepoint) {
    spacing <- paste(rep(" ", 20 - nchar(cross_loading)),
                     collapse = "")
    cat(paste0(cross_loading, spacing, " --> "))
    # Save the models ---------------------------------------------------------
    updated_models <- update_the_model(old_model, cross_loading)
    path_for_saving <- here("analysis", "model_b", "2_iterative",
                            timepoint, current_pair, iteration)
    cat(updated_models$h0, file = paste0(path_for_saving, "/h0.inp"))
    cat(updated_models$h1, file = paste0(path_for_saving, "/h1.inp"))
    # Run models --------------------------------------------------------------
    capture.output(runModels(target = path_for_saving, Mplus_command = mp))
    # Get DIFFTEST result -----------------------------------------------------
    # capture.output(output <- readModels(path_for_saving))
    capture.output(output <- readModels(target = paste0(path_for_saving,
                                                        "/h1.out"),
                                        filefilter = ".*"))
    # Check convergence -------------------------------------------------------
    # Some models don't converge. To get around this, for now, I've chosen to
    # stop the iterative procedure, continue with the next cross-loading.
    # Check H0 model ----------------------------------------------------------
    convergence = check_convergence(paste0(path_for_saving, "/h0.out"))
    if (!(convergence[[1]])) {
        message(paste0("\n\nProblem with H0 model convergence (",
                       convergence[[2]], ")\n",
                       "Stopping iterative procedure.\n\n"))
        return(list(convergence = FALSE,
                    next_model = updated_models$next,
                    old_model = updated_models$old,
                    difftest = NA))
    }
    # Check H1 model ----------------------------------------------------------
    convergence = check_convergence(paste0(path_for_saving, "/h1.out"))
    if (!(convergence[[1]])) {
        message(paste0("\n\nProblem with h1 model convergence (",
                       convergence[[2]], ")\n",
                       "Stopping iterative procedure.\n\n"))
        return(list(convergence = FALSE,
                    next_model = updated_models$next,
                    old_model = updated_models$old,
                    difftest = NA))
    }
    difftest <- try(with(output$summaries, c(ChiSqDiffTest_Value,
                                             ChiSqDiffTest_DF,
                                             ChiSqDiffTest_PValue)))
    return(list(convergence = TRUE,
                next_model = updated_models$next,
                old_model = updated_models$old,
                difftest = difftest))
}

print_heading <- function(i, model_list) {
    heading <- glue("Adding cross-loadings to pair {i}: {model_list[[i]][[1]]} and {model_list[[i]][[3]]}")
    underline <- paste(rep("=", nchar(heading)), collapse = "")
    cat(glue("\n\n{heading}\n\n"))
    cat(glue("{underline}\n\n\n"))
}

print_pval <- function(pval) {
        cat(paste0("p = ", pval, "\n"))
}


check_timepoint <- function(model_list) {
    # Check whether the current list of models is using 0w or 12/52w data.
    timepoint <- paste0(str_match(model_list[[1]][[2]],
                                  "[a-z]+[0-9]+_([0-9]+)")[,2],
                        "w")
    return(timepoint)
}

test_pair <- function(i, model_list, allow_covariance = FALSE) {
    print_heading(i, model_list)
    current_timepoint <- check_timepoint(model_list)   # Check: are we using 0w
                                                       # or 12/52w data?
    # message(paste0("\n\nRunning models for ", current_timepoint, " data.\n"))
    current_pair <- paste(model_list[[i]][[1]],
                          model_list[[i]][[3]], sep = "_")
    path_pair <- file.path(here("analysis", "model_b", "2_iterative",
                                current_timepoint, current_pair))
    # Create folder for this pair ------------------------------------------
    unlink(path_pair)
    dir.create(path_pair)
    # Run models for FIRST iteration ---------------------------------------
    iteration = 1
    path_iter <- paste0(path_pair, "/", iteration)
    refresh_path(path_iter)
    # Get first cross-loading
    first_cross_loading <- get_first_mi(i, current_timepoint)
    base_model <- no_crossloadings[[i]][3]
    difftest_result <- run_difftest(old_model     = base_model,
                                    cross_loading = first_cross_loading,
                                    iteration     = iteration,
                                    current_pair  = current_pair,
                                    timepoint     = current_timepoint)
    print_pval(difftest_result$difftest[3])
    # Run iterative procedure until DIFFTEST returns -----------------------
    # returns non-significant result
    while (difftest_result$difftest[3] < 0.05 & !(is.na(difftest_result$difftest[3]))) {
        # Get the next cross-loadings --------------------------------------
        next_loading <- get_top_mi(paste0(path_iter, "/h0.out"))
        if (is.na(next_loading)) {
            message("\nNo cross-loadings left. Stopping iterative procedure.\n")
            break
        }
        # Update iteration counter -----------------------------------------
        iteration = iteration + 1
        path_iter <- paste0(path_pair, "/", iteration)
        refresh_path(path_iter)
        # Run the DIFFTEST -------------------------------------------------
        difftest_result <- run_difftest(difftest_result$next_model,
                                        next_loading,
                                        iteration,
                                        current_pair,
                                        current_timepoint)
        if (difftest_result$convergence == FALSE) {
            # message("\n\nModel did not converge. Stopping iterative procedure.\n\n")
            break
        }
        else {
            print_pval(difftest_result$difftest[3])
        }
    }
    return(list(no_loadings = no_crossloadings[[i]],
                final_model = difftest_result$old_model,
                convergence = difftest_result$convergence))
}

