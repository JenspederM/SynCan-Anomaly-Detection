
# Mode --------------------------------------------------------------------

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Minimum Maximum Normalization -------------------------------------------

minmax_norm <- function(x, na.rm = TRUE) {
  (x - min(x, na.rm=na.rm)) / (max(x, na.rm=na.rm) - min(x, na.rm=na.rm))
}


# Get Column names by Pattern ---------------------------------------------

get_colnames <- function(x, pattern) {
  names(x)[grepl(pattern, names(x))]
}

# Get descriptive statistics ----------------------------------------------

get_desc <- function(x, na.rm = TRUE) {
  list(
    min = min(x, na.rm = na.rm),
    max = max(x, na.rm = na.rm),
    sd = sd(x, na.rm = na.rm),
    mean = mean(x, na.rm = na.rm)
  )
}
