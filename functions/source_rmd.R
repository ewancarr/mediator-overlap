source_rmd <- function(file, local = FALSE, ...) {
  options(knitr.duplicate.label = "allow")
  temp <- tempfile(tmpdir = ".", fileext = ".R")
  on.exit(unlink(temp))
  knitr::purl(file, output = temp, quiet = TRUE)
  envir <- globalenv()
  source(temp, local = envir, ...)
}
