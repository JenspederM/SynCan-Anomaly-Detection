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

prettify_file_name <- function(x, remove_ext = TRUE) {
  xs <- strsplit(x, "/")[[1]]
  if (isTRUE(remove_ext)) {
    return(strsplit(xs[length(xs)], "\\.")[[1]][1])
  } else {
    return(xs[length(xs)])
  }
}

totitle <- function(s, strict = FALSE) {
  s <- as.character(s)
  cap <- function(s) paste(toupper(substring(s, 1, 1)),
                           {s <- substring(s, 2); if(strict) tolower(s) else s},
                           sep = "", collapse = " " )
  sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}

prettify_ids <- function(x, type = c("title", "upper"), sep = " ") {
  x <- paste(regmatches(x, regexpr("\\D+", x)),
             regmatches(x, regexpr("\\d+", x)),
             sep = sep)
  switch(match.arg(type),
         title = totitle(x),
         upper = toupper(x))
}
