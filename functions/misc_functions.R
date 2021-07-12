refresh_path <- function(path) {
    unlink(path, recursive = TRUE)
    dir.create(path)
}

dp <- function(x) {
    sprintf("%0.2f", x)
}

# Function to make all variables lowercase
require(stringr)
make_lowercase <- function(df) {
    names(df) <- tolower(names(df))
    return(df)
}

require(formattable)
cfi_formatter <- formatter("span",
  style = x ~ style(color = ifelse(x < -0.02, "red", "black")))

rmsea_formatter <- formatter("span",
  style = x ~ style(color = ifelse(x > 0.02, "red", "black")))

# Function to import "NAMES" statement from Mplus data file
get_names <- function(input_file) {
    if (missing(input_file)) {
        input_file <- here("data", "clean", "mplus", "mplus.inp")
    }
    input_file <- readLines(input_file)
    start <- grep("^NAMES", input_file)
    end <- grep("MISSING=", input_file)
    single_line <- str_c(input_file[start:end], collapse = "")
    wrapped <- str_wrap(single_line, width = 50)
    return(wrapped)
}

na_zero <- function(x) {
    x[is.na(x)] <- 0
    return(x)
}

save_model <- function(l, where) {
    # Requires each list item to have
    # [1] = Mediator
    # [2] = Outcome
    # [3] = Model
    factor_name <- paste0(l[[1]], "_", l[[2]])
    file_name <- paste0(where, "/", factor_name, ".inp")
    cat(l[[3]], file = file_name)
    return(print(paste0("Saved model for factor ", factor_name)))
}

fit_model <- function(path) {
    runModels(target = path, Mplus_command = "/opt/mplus/7.3/mplus")
}

get_model_summary <- function(x) {
    return(x$summaries) %>%
        select(Title,
               N = Observations,
               Parameters,
               ChiSqM_Value,
               ChiSqM_DF,
               ChiSqM_PValue,
               CFI,
               RMSEA = RMSEA_Estimate) %>%
        extract(Title, c("Mediator", "Outcome"),
                "2-factor CFA for ([A-Z_]+) and ([A-Z]+).*") %>%
        select(Outcome, Mediator, N:RMSEA)
}

str_eval <- function(x) {
    return(eval(parse(text = x)))
}

read_models <- function(list_of_files) {
    doMC::registerDoMC(cores = 8)
    return(plyr::llply(list_of_files, readModels, .parallel = TRUE))
}

load_output <- function(files) {
    map(files, ~ load(here("analysis", "saved_output", .x), verbose = TRUE))
}

change_timepoint <- function(x) {
    x <- str_replace_all(x, "(siq[0-9r]+)_0", "\\1_12")
    x <- str_replace_all(x, "((cfq|pf|wsaq)[0-9q]+)_0", "\\1_52")
    return(x)
}
