
# Get data from Git repository and save to Data Folder --------------------

if (length(list.files("./Data/Raw/")) == 0L) {
  system("git clone https://github.com/etas/SynCAN.git Data/Raw")
}


# Instantiate Project -----------------------------------------------------

source("./R/Project Instantiation.R")

# Combine Data ------------------------------------------------------------

train <- combine_syncan_files(
  inpath = "./Data/Raw/", 
  pattern = "train_\\d", 
  outpath = "./Data/train_combined.csv"
)

test <- combine_syncan_files(
  inpath = "./Data/Raw/", 
  pattern = "test_", 
  outpath = "./Data/test_combined.csv"
)

# Preprocess Train Data ---------------------------------------------------

train_complete <- prepare_syncan_train(
  data = fread("./Data/train_combined.csv"),
  outpath = "./Data", 
  out_tag = "complete"
)

test_complete <- prepare_syncan_test(
  data = fread("./Data/test_combined.csv"), 
  train_complete = train_complete, 
  outpath = "./Data", 
  out_tag = "complete"
)

