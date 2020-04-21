
# Reader function ---------------------------------------------------------

read_syncan_raw <- function(files) {
  
  # SynCan data have constant column names and classes
  syncan_map <- c(
    "Label"   = "integer", 
    "Time"    = "numeric",
    "ID"      = "factor", 
    "Signal1" = "numeric",
    "Signal2" = "numeric", 
    "Signal3" = "numeric",
    "Signal4" = "numeric"
  )
  
  # Construct Output Vector
  out <- vector("list", length = length(files))
  
  for(i in seq_along(files)) {
    bare_file_name <- prettify_file_name(files[[i]],
                                         remove_ext = TRUE)
    
    cat(sprintf("(%d/%d) Reading file: %s\n", i, length(files), bare_file_name))
    
    # Read data
    out[[i]] <- fread(cmd = paste0("unzip -p ", files[[i]]), 
                      fill = TRUE, col.names = names(syncan_map),
                      showProgress = TRUE, colClasses = unname(syncan_map))
    
    # Add column `file`
    out[[i]][, file := bare_file_name]
    
    # Make all column names lower case
    setnames(out[[i]], names(out[[i]]), tolower(names(out[[i]])))
    
  }
  
  cat(
    sprintf("Row-binding %s to one data.table", 
            paste(
              files, 
              collapse = ", "
            )
    )
  )
  # Bind datasets to one big data.table
  out <- rbindlist(out)
  
  
  
  return(out)
}

# Combine Datasets --------------------------------------------------------

combine_syncan_files <- function(inpath, pattern, outpath) {
  files <- list.files(path = inpath, pattern = pattern, full.names = TRUE)
  raw <- read_syncan_raw(files)
  fwrite(x = raw, file = outpath)
  return(raw)
}


# Preprocess Data ---------------------------------------------------------

prepare_syncan_train <- function(data, outpath = "./Data/", out_tag = "complete") {
  writeLines("Cast data to long")
  dt_long <- melt(
    data = data, 
    id.vars = c("file", "label", "time", "id"), 
    measure.vars = sprintf("signal%d", 1:4),
    na.rm = TRUE
  )
  
  writeLines("Cast data to wide")
  dt_wide <- dcast(
    data = dt_long[order(time)],
    formula = time + label ~ id + variable,
    value.var = "value"
  )
  
  writeLines("Fill NA's by LOCF")
  signal_cols <- names(dt_wide)[grepl("signal", names(dt_wide))]
  dt_wide[, (signal_cols) := lapply(.SD, na_locf), .SDcols = signal_cols]
  dt_wide <- dt_wide[complete.cases(dt_wide)]
  
  out_cols <- c("label", "time", names(dt_wide)[grepl("signal", names(dt_wide))])
  
  writeLines("Write Train Data")
  fwrite(
    x = dt_wide[, .SD, .SDcols = out_cols], 
    file = sprintf("%s/train_%s.csv", outpath, out_tag))
  
  return(dt_wide[, .SD, .SDcols = out_cols])
}

prepare_syncan_test <- function(data, train_complete, outpath = "./Data/", out_tag = "complete") {
  writeLines("Cast data to long")
  dt_long <- melt(
    data = data, 
    id.vars = c("file", "label", "time", "id"), 
    measure.vars = sprintf("signal%d", 1:4),
    na.rm = TRUE
  )
  
  writeLines("Add Row ID")
  dt_long[, rowid := .SD[, .I], .(file)]
  
  writeLines("Cast data to wide")
  dt_wide <- dcast(
    data = dt_long,
    formula = file + rowid + time + label ~ id + variable,
    value.var = "value"
  )
  
  files <- unique(dt_wide$file)
  out_cols <- c("label", "time", names(dt_wide)[grepl("signal", names(dt_wide))])
  out_list <- vector("list", length(files))
  names(out_list) <- files
  
  for (f in files) {
    cat(sprintf("\nFile: %s - Fill NA's by LOCF", f))
    out_list[[f]] <- fill_test(
      test = dt_wide[file == f, .SD, .SDcols = out_cols], 
      train = train_complete
    )
    
    cat(sprintf(" ---> Write file"))
    fwrite(
      x = out_list[[f]], 
      file = sprintf("%s/%s_%s.csv", outpath, f, out_tag)
    )
  }
  
  return(out_list)
}

# Make Pretty File Names --------------------------------------------------

prettify_file_name <- function(x, remove_ext = TRUE) {
  xs <- strsplit(x, "/")[[1]]
  if (isTRUE(remove_ext)) {
    return(strsplit(xs[length(xs)], "\\.")[[1]][1])
  } else {
    return(xs[length(xs)])
  }
}

# To TitleCase ------------------------------------------------------------

totitle <- function(s, strict = FALSE) {
  s <- as.character(s)
  cap <- function(s) paste(toupper(substring(s, 1, 1)),
                           {s <- substring(s, 2); if(strict) tolower(s) else s},
                           sep = "", collapse = " " )
  sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}

# Pretty IDs --------------------------------------------------------------
prettify_ids <- function(x, type = c("title", "upper"), sep = " ") {
  x <- paste(regmatches(x, regexpr("\\D+", x)),
             regmatches(x, regexpr("\\d+", x)),
             sep = sep)
  switch(match.arg(type),
         title = totitle(x),
         upper = toupper(x))
}

# Read and Fill Test Data -------------------------------------------------

fill_test <- function(inpath = NULL, test = NULL, train) {
  # We rbind the last row from Train with Test
  if (!is.null(inpath)) {
    tmp <- rbind(
      train[.N, ],
      fread(inpath)
    )
  } else if (!is.null(test)) {
    tmp <- rbind(
      train[.N, ],
      test
    ) 
  } else {
    stop("Either `inpath` or `test` must be specified")
  }
  
  # Fill signal values LOCF
  signal_cols <- names(tmp)[grepl("signal", names(tmp))]
  tmp[, (signal_cols) := lapply(.SD, na_locf), .SDcols = signal_cols]
  
  # Return Test, excluding the row from train
  return(tmp[2:.N, ])
}
