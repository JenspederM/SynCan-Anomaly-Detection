
# Load required packages --------------------------------------------------

library(data.table)
library(ggplot2)


# Instantiate helper functions --------------------------------------------

pretty_labels <- function(x) {
  x <- gsub("id", "ID ", gsub("_", ": ", gsub("signal", "Signal ", x)))
  factor(
    x = x, 
    levels = c(
      "ID 1: Signal 1",
      "ID 1: Signal 2",
      "ID 2: Signal 1",
      "ID 2: Signal 2",
      "ID 2: Signal 3",
      "ID 3: Signal 1",
      "ID 3: Signal 2",
      "ID 4: Signal 1",
      "ID 5: Signal 1",
      "ID 5: Signal 2",
      "ID 6: Signal 1",
      "ID 6: Signal 2",
      "ID 7: Signal 1",
      "ID 7: Signal 2",
      "ID 8: Signal 1",
      "ID 9: Signal 1",
      "ID 10: Signal 1",
      "ID 10: Signal 2",
      "ID 10: Signal 3",
      "ID 10: Signal 4"
    )
  )
}

melt_rows <- function(data, rows = nrow(data)) {
  melt(data[1:rows, ], id.vars = c("label", "time"))
}


# Read training data ------------------------------------------------------

train <- fread("../Data/train_complete.csv")


# Construct facetted density plot of training data ------------------------

train_density <- ggplot() +
  geom_density(
    mapping = aes(value, fill = "All (29,669,590)"), 
    data = melt_rows(train),
    alpha = 0.5
  ) +
  geom_density(
    mapping = aes(value, fill = "10,000,000"), 
    data = melt_rows(train, 1e7),
    alpha = 0.5
  ) +
  geom_density(
    mapping = aes(value, fill = "1,000,000"), 
    data = melt_rows(train, 1e6),
    alpha = 0.5
  ) +
  geom_density(
    mapping = aes(value, fill = "100,000"), 
    data = melt_rows(train, 1e5),
    alpha = 0.5
  ) +
  geom_density(
    mapping = aes(value, fill = "10,000"),
    data = melt_rows(train, 1e4),
    alpha = 0.5
  ) +
  facet_wrap(
    facets = ~pretty_labels(variable), 
    ncol = 4, 
    scales = "free_y"
  ) +
  scale_x_continuous(
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    expand = c(0, 0)
  ) +
  guides(
    fill = guide_legend(title = "Included Rows")
  ) +
  labs(
    title = 'Kernel Density of SynCan Dataset', 
    x = 'Signal Value', 
    y = 'Kernel Density (Scaled Frequency)'
  ) +
  theme(
    legend.position = 'top'
  ) +
  theme_bw(
    base_family = 'Serif'
  )


# Save density plot -------------------------------------------------------

ggsave(
  filename = "./train_complete_density.png", 
  plot = train_density, 
  width = 12, 
  height = 10
)