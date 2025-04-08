#load packages
install.packages("ggpattern")
library(ggpattern)
library(ggplot2)

#PROPORTION VIGILANCE PRED CUE

#Reorder predator cues for graphing
Baboon_vigilance_graph_both <- Baboon_vigilance_stats_both %>%
  mutate(predator_cue = factor(predator_cue, levels = c("Leopard", "Cheetah", "Lion", "Wild dog", "Hyena", "Control"))) %>%  # Adjust Cue names as needed
  filter(!is.na(predator_cue))  # Remove rows where predator_cue is NA

#add column for hunting modes
Baboon_vigilance_graph_both <- Baboon_vigilance_graph_both %>%
  mutate(Hunting_mode = case_when(
    predator_cue %in% c("Lion", "Cheetah", "Leopard") ~ "Ambush",
    predator_cue %in% c("Hyena", "Wild dog") ~ "Coursing",
    predator_cue %in% c("Control") ~ "Control",
    
  ))

#Strip plot for proportion of vigilance by predator cue - point shape 
vigilance_pred_plot <- ggplot(Baboon_vigilance_graph_both, aes(x = predator_cue, y = proportion_vigilant, fill = year, shape = Hunting_mode)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA, position = position_dodge(width = 0.8)) +  
  geom_jitter(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8), size = 1.5, alpha = 0.8) +  
  scale_fill_manual(values = c("2021" = "#72874E", "2024" = "#476F84")) +  
  scale_shape_manual(values = c("Ambush" = 21, "Control" = 1, "Coursing" = 24)) +  # Different point shapes
  labs(x = "Predator Cue", y = "Proportion Vigilant", fill = "Year", shape = "Hunting Mode") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank(),
        legend.position = "bottom")


#PROPORTION VIGILANCE HABITAT

#Strip plot for proportion of vigilance by predator cue - point shape 
vigilance_habitat_plot <- ggplot(Baboon_vigilance_stats_both, aes(x = Habitat, y = proportion_vigilant)) +
  geom_boxplot(fill = "#023743FF", alpha = 0.6, outlier.shape = NA, position = position_dodge(width = 0.8)) +  # Set single fill color
  geom_jitter(color = "#023743FF",  # Set single color for points
              position = position_jitter(width = 0.2),  # Only jitter (no dodging needed)
              size = 1.5, alpha = 0.8) +  
  labs(x = "Habitat", y = "Proportion Vigilant") +  # Remove year labels
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank(),
        legend.position = "none")  # Remove legend since it's unnecessary

View(Baboon_vigilance_graph_both)

#PROPORTION VIGILANCE PREY
Baboon_vigilance_stats_both <- Baboon_vigilance_stats_both %>%
  mutate(age_sex_class = case_when(
    age_sex_class %in% c("Female_Adult_no_offspring") ~ "Female no offspring",
    age_sex_class %in% c("Female_Adult_with_offspring") ~ "Female with offspring",
    age_sex_class %in% c("Male_Adult") ~ "Male",
    TRUE ~ age_sex_class  # Keep all other values as they are
  ))

vigilance_prey_plot <- 
  ggplot(Baboon_vigilance_stats_both, aes(x = age_sex_class, y = proportion_vigilant)) +
  geom_boxplot(fill = "#023743FF", alpha = 0.6, outlier.shape = NA, position = position_dodge(width = 0.8)) +  # Set single fill color
  geom_jitter(color = "#023743FF",  # Set single color for points
              position = position_jitter(width = 0.2),  # Only jitter (no dodging needed)
              size = 1.5, alpha = 0.8) +  
  labs(x = "Age and Sex Class", y = "Proportion Vigilant") +  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank(),
        legend.position = "none")  # Remove legend

#LATENCY FLEE PRED CUE

#Reorder predator cues for graphing
Baboon_flight_graph_both <- Baboon_flight_stats_both %>%
  mutate(predator_cue = factor(predator_cue, levels = c("Leopard", "Cheetah", "Lion", "Wild dog", "Hyena", "Control"))) %>%  # Adjust Cue names as needed
  filter(!is.na(predator_cue))  # Remove rows where predator_cue is NA

#add column for hunting modes
Baboon_flight_graph_both <- Baboon_flight_graph_both %>%
  mutate(Hunting_mode = case_when(
    predator_cue %in% c("Lion", "Cheetah", "Leopard") ~ "Ambush",
    predator_cue %in% c("Hyena", "Wild dog") ~ "Coursing",
    predator_cue %in% c("Control") ~ "Control",
    
  ))

#Strip plot for latency to flee by predator cue - point shape 
latency_pred_plot <- ggplot(Baboon_flight_graph_both, aes(x = predator_cue, y = log_latency_to_flee, fill = year, shape = Hunting_mode)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA, position = position_dodge(width = 0.8)) +  
  geom_jitter(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8), size = 1.5, alpha = 0.8) +  
  scale_fill_manual(values = c("2021" = "#72874E", "2024" = "#476F84")) +  
  scale_shape_manual(values = c("Ambush" = 21, "Control" = 22, "Coursing" = 24)) +  # Different point shapes
  labs(x = "Predator Cue", y = "Log of Latency to Flee", fill = "Year", shape = "Hunting Mode") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank(),
        legend.position = "bottom")


#LATENCY TO FLEE HABITAT

#Strip plot for proportion of vigilance by predator cue - point shape 
latency_habitat_plot <- ggplot(Baboon_flight_stats_both, aes(x = Habitat, y = log_latency_to_flee)) +
  geom_boxplot(fill = "#023743FF", alpha = 0.6, outlier.shape = NA, position = position_dodge(width = 0.8)) +  # Set single fill color
  geom_jitter(color = "#023743FF",  # Set single color for points
              position = position_jitter(width = 0.2),  # Only jitter (no dodging needed)
              size = 1.5, alpha = 0.8) +  
  labs(x = "Habitat", y = "Log of Latency to Flee") +  # Remove year labels
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank(),
        legend.position = "none")  # Remove legend since it's unnecessary

#LATENCY TO FLEE PREY
Baboon_flight_graph_both <- Baboon_flight_graph_both %>%
  mutate(age_sex_class = case_when(
    age_sex_class %in% c("Female_Adult_no_offspring") ~ "Female no offspring",
    age_sex_class %in% c("Female_Adult_with_offspring") ~ "Female with offspring",
    age_sex_class %in% c("Male_Adult") ~ "Male",
    TRUE ~ age_sex_class  # Keep all other values as they are
  ))

latency_prey_plot <- 
  ggplot(Baboon_flight_graph_both, aes(x = age_sex_class, y = log_latency_to_flee)) +
  geom_boxplot(fill = "#023743FF", alpha = 0.6, outlier.shape = NA, position = position_dodge(width = 0.8)) +  # Set single fill color
  geom_jitter(color = "#023743FF",  # Set single color for points
              position = position_jitter(width = 0.2),  # Only jitter (no dodging needed)
              size = 1.5, alpha = 0.8) +  
  labs(x = "Age and Sex Class", y = "Log of Latency to Flee") +  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank(),
        legend.position = "none")  # Remove legend







#FREQUENCY OF FLIGHT PRED CUE

#calculate flight frequency
flight_frequency_pred <- Baboon_frequency_stats_both %>%
  group_by(year, predator_cue) %>%
  summarise(
    flight_present = sum(flight_present == 1),  # Count where flight_present = 1
    total = n(),  # Total number of occurrences
    flight_absent = total - flight_present,  # Compute count of flight_present = 0
    flight_frequency = flight_present / flight_absent,  # Compute flight frequency
    .groups = "drop"
  )

#Reorder predator cues for graphing
flight_frequency_pred <- flight_frequency_pred %>%
  mutate(predator_cue = factor(predator_cue, levels = c("Leopard", "Cheetah", "Lion", "Wild dog", "Hyena", "Control"))) %>%  # Adjust Cue names as needed
  filter(!is.na(predator_cue))  # Remove rows where predator_cue is NA

#add column for hunting modes
flight_frequency_pred <- flight_frequency_pred %>%
  mutate(Hunting_mode = case_when(
    predator_cue %in% c("Lion", "Cheetah", "Leopard") ~ "Ambush",
    predator_cue %in% c("Hyena", "Wild dog") ~ "Coursing",
    predator_cue %in% c("Control") ~ "Control",
    
  ))

#bar graph for frequency of flight by predator cue
frequency_pred_plot <-ggplot(flight_frequency_pred, aes(x = predator_cue, y = flight_frequency, 
                                  fill = year, pattern = Hunting_mode)) +
  geom_bar_pattern(stat = "identity", 
                   position = position_dodge(width = 0.9),
                   color = "black",
                   pattern_fill = "black", 
                   pattern_density = 0.1,
                   pattern_spacing = 0.05,
                   alpha = 0.8) +
  scale_fill_manual(values = c("2021" = "#72874E", "2024" = "#476F84")) +
  scale_pattern_manual(values = c("Ambush" = "stripe", "Coursing" = "circle", "Control"="none")) +  # customize as needed
  labs(x = "Predator Cue", y = "Frequency of Flight", 
       fill = "Year", pattern = "Hunting Mode") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right", 
        panel.grid = element_blank())



#FREQUENCY OF FLIGHT HABITAT

#calculate flight frequency
flight_frequency_habitat <- Baboon_frequency_stats_both %>%
  group_by(Habitat) %>%
  summarise(
    flight_present = sum(flight_present == 1),  # Count where flight_present = 1
    total = n(),  # Total number of occurrences
    flight_absent = total - flight_present,  # Compute count of flight_present = 0
    flight_frequency = flight_present / flight_absent,  # Compute flight frequency
    .groups = "drop"
  )

frequency_habitat_plot <- ggplot(flight_frequency_habitat, aes(x = Habitat, y = flight_frequency, fill = Habitat)) +
  geom_bar(stat = "identity", width = 0.6, alpha = 0.8) +
  scale_fill_manual(values = c("Open" = "#023743FF", "Closed" = "#023743FF")) +
  labs(x = "Habitat Type", y = "Flight Frequency") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.title = element_text(size = 12))

#FLIGHT FREQUENCY PREY

#calculate flight frequency
Baboon_frequency_graph_both <- Baboon_frequency_stats_both %>%
  group_by(age_sex_class) %>%
  summarise(
    flight_present = sum(flight_present == 1),  # Count where flight_present = 1
    total = n(),  # Total number of occurrences
    flight_absent = total - flight_present,  # Compute count of flight_present = 0
    flight_frequency = flight_present / flight_absent,  # Compute flight frequency
    .groups = "drop"
  )

Baboon_frequency_graph_both <- Baboon_frequency_graph_both %>%
  mutate(age_sex_class = case_when(
    age_sex_class %in% c("Female_Adult_no_offspring") ~ "Female no offspring",
    age_sex_class %in% c("Female_Adult_with_offspring") ~ "Female with offspring",
    age_sex_class %in% c("Male_Adult") ~ "Male",
    TRUE ~ age_sex_class  # Keep all other values as they are
  ))

frequency_prey_plot <- 
  ggplot(Baboon_frequency_graph_both, aes(x = age_sex_class, y = flight_frequency)) +
  geom_bar(stat = "identity", width = 0.6, alpha = 0.8, fill = "#023743FF") +
  labs(x = "Age and Sex Class", y = "Flight Frequency") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(size = 8, angle = 45, hjust = 1),
        axis.title = element_text(size = 12))





