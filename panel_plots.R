#Panel plots

#load packages
install.packages("cowplot")
library(ggplot2)
library(cowplot)


#PROPORTION VIGILANCE
plot_grid(
  vigilance_pred_plot,
  predicted_vigilance_pred_plot,
  vigilance_habitat_plot,
  predicted_vigilance_habitat_plot,
  vigilance_prey_plot,
  predicted_vigilance_prey_plot,
  labels = c("A", "B", "C", "D", "E", "F"),  # Your panel labels
  label_size = 14,  # You can customize the size
  ncol = 2,
  nrow = 3
)

#LATENCY TO FLEE
plot_grid(
  latency_pred_plot,
  predicted_latency_pred_plot,
  latency_habitat_plot,
  predicted_latency_habitat_plot,
  latency_prey_plot,
  predicted_latency_prey_plot,
  labels = c("A", "B", "C", "D", "E", "F"),  # Your panel labels
  label_size = 14,  # You can customize the size
  ncol = 2,
  nrow = 3
)


#FREQUENCY OF FLIGHT
plot_grid(
  frequency_pred_plot,
  predicted_frequency_pred_plot,
  frequency_habitat_plot,
  predicted_frequency_habitat_plot,
  frequency_prey_plot,
  predicted_frequency_prey_plot,
  labels = c("A", "B", "C", "D", "E", "F"),  # Your panel labels
  label_size = 14,  # You can customize the size
  ncol = 2,
  nrow = 3
)
