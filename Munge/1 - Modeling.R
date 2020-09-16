# Instantiate Project -----------------------------------------------------

source("./R/Project Instantiation.R")


# Initialize H2O Session --------------------------------------------------

h2o.init()


# Import Training Data ----------------------------------------------------

train_hex <- h2o.importFile("./Data/train_complete.csv")


# Get Predictors (id/signal combinations) ---------------------------------

predictors <- names(train_hex)[grepl("signal", names(train_hex))]


# Construct Neural Network Auto-encoder with default parameters -----------

base_model <- h2o.deeplearning(
  x=predictors,
  training_frame=train_hex[1:1e6, ],
  activation="Tanh",
  autoencoder=TRUE, 
  model_id = "base_model", 
  standardize = TRUE,
  seed = 1337L,
  hidden = c(50, 100, 50),
  epochs = 100, 
  verbose = FALSE
)

# Save Base Model ---------------------------------------------------------

h2o.saveModel(
  object = base_model, 
  path = "./Models/"
)


# Evaluate Base Model -----------------------------------------------------

base_evaluation <- evaluate_model(
  model = base_model,
  test_path = "./Data/test_continuous_complete.csv"
)

base_evaluation$Evaluation_Report
base_evaluation$Prediction_Plot
