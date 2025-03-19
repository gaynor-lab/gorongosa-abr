#Panel plots

#load packages
install.packages("cowplot")
library(ggplot2)
library(cowplot)

#Vigilance plots 
plot_grid(vigilance_predcue_plot_21, vigilance_predcue_plot_24, labels = c("2021", "2024"), ncol = 2)
