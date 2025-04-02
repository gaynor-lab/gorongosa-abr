


#Reorder predator cues for graphing
Baboon_vigilance_predatorcue_24 <- Baboon_vigilance_predatorcue_24 %>%
  mutate(predator_cue = factor(predator_cue, levels = c("Leopard", "Cheetah", "Lion", "WD", "Hyena", "Control"))) %>%  # Adjust Cue names as needed
  filter(!is.na(predator_cue))  # Remove rows where predator_cue is NA

#Strip plot for proportion of vigilance by predator cue
vigilance_pred_plot <- 
  ggplot(Baboon_vigilance_stats_both, aes(x = predator_cue, y = proportion_vigilant, fill = year, color = year)) +
  geom_boxplot(alpha = 0.4, outlier.shape = NA) +  # Lighter box plot
  geom_jitter(width = 0.2, size = 1.2, alpha = 0.8) +  # Smaller points
  scale_colour_paletteer_d("nationalparkcolors::Acadia") +  # Color scheme for points
  scale_fill_paletteer_d("nationalparkcolors::Acadia") +  # Color scheme for box fill
  labs(
    x = "Predator Cue",
    y = "Proportion Vigilant") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank(),
        legend.position = "right")  

View(Baboon_vigilance_stats_both)
