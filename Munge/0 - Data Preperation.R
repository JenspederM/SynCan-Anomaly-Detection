
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

# Train Base Model
h2o.init()

# Coerce Train to H2O Hex file
train_hex <- h2o.importFile("./Data/train_complete.csv")

# Get Predictors (id/signal combinations)
predictors <- names(train_complete)[grepl("signal", names(train_complete))]

# Construct Neural Network Auto-encoder with default parameters
base_model <- h2o.deeplearning(
  x=predictors,
  training_frame=train_hex,
  activation="Tanh",
  autoencoder=TRUE
)

# Save Base Model
h2o.saveModel(
  object = base_model, 
  path = "./Models/"
)


# Prepare Test ------------------------------------------------------------

test_continuous <- fill_test(
  inpath = "./Data/test_plateau_complete.csv",
  train = train_complete
)

# Reconstruct test using base model ---------------------------------------

test_rec_error <- h2o.anomaly(
  object = base_model,
  data = as.h2o(test_continuous)
)

# Plot Reconstruction Error -----------------------------------------------

plot_reconstruction_error(
  model = as.data.frame(test_rec_error)
)

# Get Classification Report -----------------------------------------------

report <- classification_report(
  test_frame = as.data.frame(test_continuous), 
  prediction_frame = as.data.frame(test_rec_error),
  target = "label", 
  quantile = 0.90,
  title = "Plateau",
  verbose = FALSE
)

# Plot Prediction ---------------------------------------------------------

plot_prediction(
  test_frame = as.data.frame(test_continuous),
  prediction_frame = as.data.frame(test_rec_error), 
  target = "label",
  quantile = 0.90
)

evaluate_model <- function(model, test_path) {
  
}
