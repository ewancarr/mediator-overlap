# Function to extract USING variables from a model statement ------------------
extract_using_variables <- function(model_statement) {
    return(str_match(model_statement, "(?s)BY ([^;]*)")[2])
}

# Write separate factor models ------------------------------------------------
separate_factors <- function(list_item, names_statement) {
    using_statement <- extract_using_variables(list_item[2])
    template <- glue("TITLE: {list_item[1]};
                     DATA:
                     FILE = \"data/clean/mplus.dat\";
                     VARIABLE:
                     {names_statement}
                     USEVAR = {using_statement};
                     CATEGORICAL = ALL;
                     MODEL:
                     {list_item[2]}
                     OUTPUT:
                     CINTERVAL TECH1; !MODINDICES(ALL);")
                     return(c(list_item, template))
}

save_input_file <- function(x) {
    factor_name <- str_extract(x[2], "^\\w+")
    file_name <- paste0(here("separate_factors"),
                        "/", factor_name, ".inp")
    cat(x[3], file = file_name)
    return(print(paste0("Saved model for factor ", factor_name)))
}

# Write models for single-factor with all items (from mediator and outcomes) --

generate_model_a <- function(l) {
    a <- extract_using_variables(l[[2]])
    b <- extract_using_variables(l[[4]])
    wrapped <- str_wrap(paste(a, b), width = 50)
    input_file <- glue(
"TITLE: Single CFA for {l[[1]]} and {l[[3]]};
DATA:
FILE = \"~data/clean/mplus.dat\";
VARIABLE:
{names_statement}
USEVAR = {wrapped};
CATEGORICAL = ALL;
MODEL:
F1 BY {wrapped};")
    return(c(l, input_file))
}

# Write two-factor models -----------------------------------------------------

write_model_b_two_factor <- function(l, allow_covariance = FALSE) {
    model_statement <- glue("{l[2]}\n{l[4]}")
    required_vars <- extract_using_variables(model_statement)
    using_statement <- str_wrap(str_c(required_vars, collapse = " "),
                                width = 50)
    constraint <- if_else(allow_covariance, "", "@0")
    template <- glue("TITLE: 2-factor CFA for {l[1]} and {l[3]}.
DATA:
FILE = \"data/clean/mplus.dat\";
VARIABLE:
{names_statement}
USEVAR = {using_statement};
CATEGORICAL = ALL;
MODEL:
{model_statement}
{l[1]} WITH {l[3]}{constraint};
OUTPUT:
MODINDICES(ALL 1);")
return(c(l[[1]], l[[3]], template))
}
