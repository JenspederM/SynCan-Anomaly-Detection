
# Load Packages -----------------------------------------------------------

load_packages <- function(lop) {
  new_packages <- lop[!lop %in% installed.packages()[, "Package"]]
  
  if (length(new_packages) > 0) {
    install.packages(new_packages, Ncpus = parallel::detectCores() - 1L)
  }
  
  invisible(sapply(lop, library, character.only = TRUE))
}

required_packages <- c(
  #### Data Manipulation
  "data.table", 
  "slam",
  "magrittr", 
  
  #### Visualization
  "ggplot2", 
  "patchwork",
  "gridExtra", 
  
  #### Utilities
  "Rcpp", 
  
  #### Modeling
  "h2o"
)

load_packages(required_packages)


# Source Functions --------------------------------------------------------

source("./R/data_helpers.R")
source("./R/classification_helpers.R")

# Set Options -------------------------------------------------------------

options(scipen = 999)
options("h2o.use.data.table"=TRUE)