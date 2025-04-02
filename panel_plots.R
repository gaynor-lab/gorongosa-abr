#Panel plots

#load packages
install.packages("cowplot")
library(ggplot2)
library(cowplot)



vigilance_plots <- plot_grid(plot1, predicted_vigilance_pred_plot, plot3, plot4, plot5, plot6, 
                           ncol = 3, nrow = 2)  # Arrange plots in 3 columns and 2 rows





























#PREDATOR IDENTITY

#Vigilance plots 
plot_grid(vigilance_predcue_plot_21, vigilance_predcue_plot_24, labels = c("2021", "2024"), ncol = 2)

#latency plots
plot_grid(latency_predcue_plot_21, latency_predcue_plot_24, labels = c("2021", "2024"), ncol = 2)

#frequency plots
plot_grid(frequency_predcue_plot_21, frequency_predcue_plot_24, labels = c("2021", "2024"), ncol = 2)

#HABITAT TYPE

#vigilance plots
plot_grid(vigilance_habitat_plot_21, vigilance_habitat_plot_24, labels = c("2021", "2024"), ncol = 2)

#latency plots
plot_grid(latency_habitat_plot_21, latency_habitat_plot_24, labels = c("2021", "2024"), ncol = 2)

#frequency plots
plot_grid(frequency_habitat_plot_21, frequency_habitat_plot_24, labels = c("2021", "2024"), ncol = 2)

#PREY IDENTITY

#vigilance plots
plot_grid(vigilance_prey_plot_21, vigilance_prey_plot_24, labels = c("2021", "2024"), ncol = 2)

#latency plots
plot_grid(latency_prey_plot_21, latency_prey_plot_24, labels = c("2021", "2024"), ncol = 2)

#frequency plots
plot_grid(frequency_prey_plot_21, frequency_prey_plot_24, labels = c("2021", "2024"), ncol = 2)
