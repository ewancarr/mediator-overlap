# Title:        Prepare data for "PACE overlap" analysis
# Author:       Ewan Carr

library(tidyverse)
library(fs)
library(haven)
library(here)
library(MplusAutomation)

# Load the raw data -----------------------------------------------------------
df <- read_sav("PACE database_25082013.sav")

items <- function(template, sequence) {
    paste(sapply(sequence, function(x) {
                     gsub("#", x, template) }),
          collapse = " ")
}

# Rename variables (replace dots with underscores) ----------------------------
names(df) <- gsub("\\.", "_", names(df))

# Select required variables (0 and 52 weeks) ----------------------------------
pick <- function(search) {
    grep(search, names(df), value = TRUE)
}

required_variables <- c("pin", "trialarm",
                        pick("^cfq[q]*[0-9]+(_0|12|52F)"), # Chalder fatigue
                        pick("^pf\\d+_(0|12F|52F)"),       # SF36 physical
                        pick("^wsaq\\d+_(0|12|52)"),       # WSAQ
                        pick("^siq\\d+[r]*_(0|12|52)"))    # Mediators

selected <- select(df, required_variables) %>%
    select(-cfq1124F)

# Rename variables for consistency --------------------------------------------
names(selected) <- names(selected) %>%
    str_replace("(pf|wsaq|siq|cfq)q*([0-9]*)[_]*(0|12|52)F*", "\\1\\2_\\3")

# Save version for future use -------------------------------------------------
save(selected, file = here("data", "clean", "pace.Rdata"))

# Prepare data for Mplus ------------------------------------------------------
model_statement <- prepareMplusData(selected,
                                    here("data", "clean", "mplus.dat"),
                                    inpfile = TRUE)

# Save version of raw data, for use elsewhere ---------------------------------
save(df, file = here("data", "clean", "pace_raw.Rdata"))
