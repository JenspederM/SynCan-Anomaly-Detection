plot_reconstruction_error <- function(model) {
  error <- as.data.frame(model)
  plot_dt <- data.table(index = 1:nrow(error),
                        MSE = sort(error$Reconstruction.MSE))
  
  plot_q <- as.data.table(list(intercept = quantile(plot_dt$MSE, probs = c(0.8, 0.85, 0.90, 0.95, 0.99))))
  plot_q$quantile = c("Q80", "Q85", "Q90", "Q95", "Q99")
  
  ggplot() +
    geom_line(aes(index, MSE), data = plot_dt) +
    geom_hline(aes(yintercept = intercept, color = quantile), data = plot_q) +
    labs(title = "Reconstruction Error",
         x = "Index",
         y = "Sorted MSE")
}

plot_prediction <- function(test_frame, prediction_frame, target = "label", quantile = 0.95) {
  plot_dt <- data.table(
    index = 1:nrow(test_frame),
    actual = as.integer(as.vector(test_frame[[target]])),
    prediction = categorize_prediction(model = prediction_frame, quantile = quantile)) %>% 
    melt(id.vars = c("index"))
  
  plot_dt %>%
    ggplot(aes(index, value, color = variable)) +
    geom_line(alpha = 0.75) +
    theme_bw()
  
}

plot_confusion_matrix <- function(conf_mat) {
  ggplot(data = conf_mat, mapping = aes(x = Prediction, y = Actual)) +
    geom_tile(aes(fill = N), colour = "white") +
    geom_text(aes(label = sprintf("%s\n%s", Type, scales::number(N, big.mark = ","))), 
              size = 8, vjust = 0.5) +
    scale_fill_gradient(low = "grey95", high = "firebrick2") +
    theme_bw() + 
    theme(legend.position = "none")
}

categorize_prediction <- function(model, quantile) {
  error <- as.data.frame(model)$Reconstruction.MSE
  q <- quantile(error, probs = quantile)
  fifelse(error < q, 0L, 1L)
}

confusion_matrix <- function(test_frame, prediction_frame, target = "label", quantile = 0.95) {
  data.table(
    x = categorize_prediction(prediction_frame, quantile = quantile), 
    y = as.integer(as.vector(test_frame[["label"]]))
  ) %>% 
    .[, .(
      Prediction = as.factor(c(1L, 0L, 1L, 0L)), 
      Actual     = as.factor(c(1L, 1L, 0L, 0L)),
      Type       = as.factor(c("True Positive", 
                               "False Negative", 
                               "False Positive",
                               "True Negative")),
      N          = c(TP = sum(x == 1L & y == 1L),
                     FN = sum(x == 0L & y == 1L),
                     FP = sum(x == 1L & y == 0L),
                     TN = sum(x == 0L & y == 0L))
    )]
}

confusion_metrics <- function(TP, TN, FP, FN) {
  # Accuracy: Classification Rate or Accuracy is given by the relation
  # However, there are problems with accuracy. 
  # It assumes equal costs for both kinds of errors. 
  # A 99% accuracy can be excellent, good, mediocre, poor or terrible depending upon the problem.
  accuracy <- (TP + TN) / (TP + TN + FP + FN)
  
  # Recall: Recall gives us an idea about when it’s actually yes, how often does it predict yes.
  # High Recall indicates the class is correctly recognized (a small number of FN).
  recall <- TP / (TP + FN)
  # Precision: Precsion tells us about when it predicts yes, how often is it correct
  # High Precision indicates an example labelled as positive is indeed positive (a small number of FP).
  precision <- TP / (TP + FP)
  
  # High recall, low precision: This means that most of the positive examples are correctly recognized (low FN) but there are a lot of false positives.
  # Low recall, high precision: This shows that we miss a lot of positive examples (high FN) but those we predict as positive are indeed positive (low FP)
  
  f_measure <- (2 * recall * precision) / (recall + precision)
  data.table(
    Accuracy = round(accuracy, 4),
    Recall = round(recall, 4),
    Precision = round(precision, 4),
    `F-Measure` = round(f_measure, 4)
  )
}

classification_report <- function(test_frame, prediction_frame, title = "", target = "label", quantile = 0.95, verbose = TRUE) {
  conf_matrix <- confusion_matrix(
    test_frame = test_frame, 
    prediction_frame = prediction_frame, 
    target = target, 
    quantile = quantile
  )
  conf_metrics <- confusion_metrics(
    TP = conf_matrix[Type == "True Positive", N], 
    TN = conf_matrix[Type == "True Negative", N], 
    FP = conf_matrix[Type == "False Positive", N], 
    FN = conf_matrix[Type == "False Negative", N]
  )
  conf_plot <- plot_confusion_matrix(
    conf_mat = conf_matrix
  )
  
  p <- conf_plot / gridExtra::tableGrob(conf_metrics, rows = NULL) +
    plot_layout(heights = c(4, 0.5)) +
    plot_annotation(title = sprintf("Confusion Matrix: %s", title),
                    subtitle = sprintf("Predictions are constituted by reconstruction error above the %s quantile", 
                                       scales::percent(quantile)))
  
  
  if (isTRUE(verbose)) print(p)
  
  return(
    invisible(
      list(
        `Confusion Matrix` = conf_matrix,
        `Confusion Metrics` = conf_metrics,
        `Confusion Plot` = p
      )
    )
  )
  
}


# Evaluate Anomaly Detection Model ----------------------------------------

evaluate_model <- function(model, test_path = NULL, test_data = NULL, quantile = 0.90, 
                           plot_reconstruction = FALSE, plot_prediction = FALSE, generate_report = FALSE) {
  
  # Global Timer
  tictoc::tic("Evaluated Model in")
  
  # Load test data
  tictoc::tic("Loading test data...")
  if (!is.null(test_path)) {
    test_hex <- h2o.importFile(test_path)
    report_title <- prettify_file_name(test_path)
  } else if (!is.null(test_data)) {
    test_hex <- as.h2o(test_data)
    report_title <- deparse(substitute(test_data))
  } else {
    stop("Test must be provided either at a path or as data")
  }
  tictoc::toc()
  
  # Construct output
  output <- list(
    "Reconstruction" = NULL,
    "Test" = as.data.frame(test_hex),
    "Reconstruction_Plot" = NULL,
    "Prediction_Plot" = NULL,
    "Evaluation_Report" = NULL
  )
  
  # Anomaly Detection  
  tictoc::tic("Testing for anomalies...")
  output[["Reconstruction"]] <- h2o.anomaly(
    object = model,
    data = test_hex
  )
  tictoc::toc()
  
  # Plot Reconstruction Error
  if (isTRUE(plot_reconstruction)) {
    tictoc::tic("Plotting reconstruction error...")
    output[["Reconstruction_Plot"]] <- plot_reconstruction_error(
      model = as.data.frame(output[["Reconstruction"]])
    )
    tictoc::toc()
  } 
  
  # Plot Prediction
  if (isTRUE(plot_prediction)) {
    tictoc::tic("Plotting prediction...")
    output[["Prediction_Plot"]] <- plot_prediction(
      test_frame = output[["Test"]],
      prediction_frame = as.data.frame(output[["Reconstruction"]]), 
      target = "label", 
      quantile = quantile
    )
    tictoc::toc()
  } 
  
  # Generate Evaluation Report
  if (isTRUE(generate_report)) {
    tictoc::tic("Generating evaluation report...")
    output[["Evaluation_Report"]] <- classification_report(
      test_frame = output[["Test"]], 
      prediction_frame = as.data.frame(output[["Reconstruction"]]),
      target = "label", 
      quantile = quantile,
      title = report_title,
      verbose = FALSE
    )
    tictoc::toc()
  } 
  
  tictoc::toc()
  
  return(output)
}
