library(here)
library(plyr)
library(stringr)

get_factor_loadings <- function(file) {
    all_lines <- str_c(readLines(file), collapse = "\\n")
    by_factor <- str_split(all_lines,
       "EXPLORATORY FACTOR ANALYSIS WITH [1-9]+ FACTOR\\(S\\)")
    remove_junk <- by_factor[[1]][2:length(by_factor[[1]])] %>%
        str_split("\\\\n")
    extract_loadings <- function(section) {
        part <- unlist(section)
        start <- grep("^[ ]+GEOMIN ROTATED LOADINGS", part) + 3
        end <- grep("^[ ]+GEOMIN FACTOR CORRELATIONS", part) - 3
        hold_this <- tempfile()
        cat(part[start:end], file = hold_this, sep = "\n")
        loadings <- read.table(hold_this)
        names(loadings) <- c("item",  paste0("F", seq(2, ncol(loadings)) - 1))
        loadings[,-1] <- sapply(loadings[, -1],
                                FUN = function(x) {
                                    as.numeric(str_replace(x, "\\*", ""))
                                })
        return(loadings)
    }
    return(llply(remove_junk, extract_loadings))
}
