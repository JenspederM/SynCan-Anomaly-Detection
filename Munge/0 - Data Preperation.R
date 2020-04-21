
# Get data from Git repository and save to Data Folder --------------------

if (length(list.files("./Data/")) == 0L) {
  system("git clone https://github.com/etas/SynCAN.git Data")
}


# Instantiate Project -----------------------------------------------------

source("./R/Project Instantiation.R")
