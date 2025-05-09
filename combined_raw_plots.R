#load packages
install.packages("ggpattern")
library(ggpattern)
library(ggplot2)

#PROPORTION VIGILANCE PRED CUE BY YEAR

#Reorder predator cues for graphing
Baboon_vigilance_graph_both <- Baboon_vigilance_stats_both %>%
  mutate(predator_cue = factor(predator_cue, levels = c("Cheetah", "Wild dog", "Hyena", "Leopard", "Lion", "Control"))) %>%  # Adjust Cue names as needed
  filter(!is.na(predator_cue))  # Remove rows where predator_cue is NA

#add column for hunting modes
Baboon_vigilance_graph_both <- Baboon_vigilance_graph_both %>%
  mutate(Hunting_mode = case_when(
    predator_cue %in% c("Lion", "Leopard") ~ "Ambush",
    predator_cue %in% c("Hyena", "Wild dog", "Cheetah") ~ "Coursing",
    predator_cue %in% c("Control") ~ "Control",
    
  ))

#reorder hunting modes
Baboon_vigilance_graph_both$Hunting_mode <- factor(
  Baboon_vigilance_graph_both$Hunting_mode,
  levels = c("Coursing", "Ambush", "Control")  # put your desired legend AND plotting order here
)

#Strip plot for proportion of vigilance by predator cue - point shape 
vigilance_year_pred_plot <- ggplot(Baboon_vigilance_graph_both, aes(x = predator_cue, y = proportion_vigilant, fill = year, shape = Hunting_mode)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA, position = position_dodge(width = 0.8)) +  
  geom_jitter(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8), size = 1.5, alpha = 0.8) +  
  scale_fill_manual(values = c("2021" = "#72874E", "2024" = "#476F84")) +  
  scale_shape_manual(values = c("Coursing" = 24, "Ambush" = 21, "Control" = 1)) +  # Different point shapes
  labs(x = "Predator Cue", y = "Proportion Vigilant", fill = "Year", shape = "Hunting Mode") +
  scale_x_discrete(labels = c(
    "Lion" = "Lion",
    "Leopard" = "Leopard*",
    "Cheetah" = "Cheetah",
    "Hyena" = "Hyena",
    "Wild dog" = "Wild dog*"
  )) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank(),
        legend.position = "bottom")

#PROPORTION VIGILANCE PREDATOR CUE
vigilance_pred_plot <- ggplot(Baboon_vigilance_graph_both, 
                              aes(x = predator_cue, y = proportion_vigilant, shape = Hunting_mode)) +
  geom_boxplot(fill = "#023743", alpha = 0.6, outlier.shape = NA, 
               position = position_dodge(width = 0.8)) +
  geom_jitter(aes(fill = Hunting_mode), 
              position = position_jitter(width = 0.2),
              size = 1.5, alpha = 0.8, color = "#023743") +
  scale_shape_manual(values = c("Coursing" = 24, "Ambush" = 22, "Control" = 21)) +
  scale_fill_manual(values = c("Coursing" = "#023743", "Ambush" = "#023743", "Control" = "#023743"), 
                    guide = "none") +  # Remove fill legend
  labs(x = "Predator Cue", y = "Proportion Vigilant", shape = "Hunting Mode") +
  scale_x_discrete(labels = c(
    "Lion" = "Lion",
    "Leopard" = "Leopard",
    "Cheetah" = "Cheetah",
    "Hyena" = "Hyena",
    "Wild dog" = "Wild dog"
  )) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
        axis.text.y = element_text(size = 14),                         # Y-axis text size
        axis.title.x = element_text(size = 16),                        # X-axis title size
        axis.title.y = element_text(size = 16), 
        panel.grid = element_blank(),
        legend.position = "bottom")

#PROPORTION VIGILANCE YEAR
vigilance_year_plot <- ggplot(Baboon_vigilance_stats_both, aes(x = year, y = proportion_vigilant)) +
  geom_boxplot(fill = "#023743FF", alpha = 0.6, outlier.shape = NA, position = position_dodge(width = 0.8)) +  # Set single fill color
  geom_jitter(color = "#023743FF",  # Set single color for points
              position = position_jitter(width = 0.2),  # Only jitter (no dodging needed)
              size = 1.5, alpha = 0.8) +  
  labs(x = "Year", y = "Proportion Vigilant") +  # Remove year labels
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
        axis.text.y = element_text(size = 14),                         # Y-axis text size
        axis.title.x = element_text(size = 16),                        # X-axis title size
        axis.title.y = element_text(size = 16),
        panel.grid = element_blank(),
        legend.position = "none")  # Remove legend since it's unnecessary


#PROPORTION VIGILANCE HABITAT

#Strip plot for proportion of vigilance by habitat
vigilance_habitat_plot <- ggplot(Baboon_vigilance_stats_both, aes(x = Habitat, y = proportion_vigilant)) +
  geom_boxplot(fill = "#023743FF", alpha = 0.6, outlier.shape = NA, position = position_dodge(width = 0.8)) +  # Set single fill color
  geom_jitter(color = "#023743FF",  # Set single color for points
              position = position_jitter(width = 0.2),  # Only jitter (no dodging needed)
              size = 1.5, alpha = 0.8) +  
  labs(x = "Habitat", y = "Proportion Vigilant") +  # Remove year labels
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
        axis.text.y = element_text(size = 14),                         # Y-axis text size
        axis.title.x = element_text(size = 16),                        # X-axis title size
        axis.title.y = element_text(size = 16),
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
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
        axis.text.y = element_text(size = 14),                         # Y-axis text size
        axis.title.x = element_text(size = 16),                        # X-axis title size
        axis.title.y = element_text(size = 16),
        panel.grid = element_blank(),
        legend.position = "none")  # Remove legend

#LATENCY FLEE PRED CUE BY YEAR

#Reorder predator cues for graphing
Baboon_flight_graph_both <- Baboon_flight_stats_both %>%
  mutate(predator_cue = factor(predator_cue, levels = c("Cheetah", "Wild dog", "Hyena", "Leopard", "Lion", "Control"))) %>%  # Adjust Cue names as needed
  filter(!is.na(predator_cue))  # Remove rows where predator_cue is NA

#add column for hunting modes
Baboon_flight_graph_both <- Baboon_flight_graph_both %>%
  mutate(Hunting_mode = case_when(
    predator_cue %in% c("Lion", "Leopard") ~ "Ambush",
    predator_cue %in% c("Hyena", "Wild dog", "Cheetah") ~ "Coursing",
    predator_cue %in% c("Control") ~ "Control",
    
  ))

#reorder hunting modes
Baboon_flight_graph_both$Hunting_mode <- factor(
  Baboon_flight_graph_both$Hunting_mode,
  levels = c("Coursing", "Ambush", "Control")  # put your desired legend AND plotting order here
)


#Strip plot for latency to flee by predator cue - point shape 
latency_pred_year_plot <- ggplot(Baboon_flight_graph_both, aes(x = predator_cue, y = log_latency_to_flee, fill = year, shape = Hunting_mode)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA, position = position_dodge(width = 0.8)) +  
  geom_jitter(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8), size = 1.5, alpha = 0.8) +  
  scale_fill_manual(values = c("2021" = "#72874E", "2024" = "#476F84")) +  
  scale_shape_manual(values = c("Ambush" = 22, "Control" = 21, "Coursing" = 24)) +  # Different point shapes
  labs(x = "Predator Cue", y = "Log of Latency to Flee", fill = "Year", shape = "Hunting Mode") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 14),                         # Y-axis text size
        axis.title.x = element_text(size = 16),                        # X-axis title size
        axis.title.y = element_text(size = 16),
        panel.grid = element_blank(),http://127.0.0.1:45737/graphics/7e414758-4b0c-43c2-9a70-2556cdaff427.png
        legend.position = "bottom")

#LATENCY TO FLEE BY PREDATOR CUE
latency_pred_plot <- ggplot(Baboon_flight_graph_both, aes(x = predator_cue, y = log_latency_to_flee, shape = Hunting_mode)) +
  geom_boxplot(fill = "#023743", alpha = 0.6, outlier.shape = NA, 
               position = position_dodge(width = 0.8)) +
  geom_jitter(aes(fill = Hunting_mode), 
              position = position_jitter(width = 0.2),
              size = 1.5, alpha = 0.8, color = "#023743") +
  scale_shape_manual(values = c("Coursing" = 24, "Ambush" = 22, "Control" = 21)) +
  scale_fill_manual(values = c("Coursing" = "#023743", "Ambush" = "#023743", "Control" = "#023743"), 
                    guide = "none") +  # Remove fill legend
  labs(x = "Predator Cue", y = "Log of Latency to Flee", shape = "Hunting Mode") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
        axis.text.y = element_text(size = 14),                         # Y-axis text size
        axis.title.x = element_text(size = 16),                        # X-axis title size
        axis.title.y = element_text(size = 16),
        panel.grid = element_blank(),
        legend.position = "bottom")

#LATENCY TO FLEE BY YEAR
latency_year_plot <- ggplot(Baboon_flight_stats_both, aes(x = year, y = log_latency_to_flee)) +
  geom_boxplot(fill = "#023743FF", alpha = 0.6, outlier.shape = NA, position = position_dodge(width = 0.8)) +  # Set single fill color
  geom_jitter(color = "#023743FF",  # Set single color for points
              position = position_jitter(width = 0.2),  # Only jitter (no dodging needed)
              size = 1.5, alpha = 0.8) +  
  labs(x = "Year", y = "Log of Latency to Flee") +  # Remove year labels
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
        axis.text.y = element_text(size = 14),                         # Y-axis text size
        axis.title.x = element_text(size = 16),                        # X-axis title size
        axis.title.y = element_text(size = 16),
        panel.grid = element_blank(),
        legend.position = "none")  # Remove legend since it's unnecessary


#LATENCY TO FLEE HABITAT

#Strip plot for proportion of vigilance by predator cue - point shape 
latency_habitat_plot <- ggplot(Baboon_flight_stats_both, aes(x = Habitat, y = log_latency_to_flee)) +
  geom_boxplot(fill = "#023743FF", alpha = 0.6, outlier.shape = NA, position = position_dodge(width = 0.8)) +  # Set single fill color
  geom_jitter(color = "#023743FF",  # Set single color for points
              position = position_jitter(width = 0.2),  # Only jitter (no dodging needed)
              size = 1.5, alpha = 0.8) +  
  labs(x = "Habitat", y = "Log of Latency to Flee") +  # Remove year labels
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
        axis.text.y = element_text(size = 14),                         # Y-axis text size
        axis.title.x = element_text(size = 16),                        # X-axis title size
        axis.title.y = element_text(size = 16),
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
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
        axis.text.y = element_text(size = 14),                         # Y-axis text size
        axis.title.x = element_text(size = 16),                        # X-axis title size
        axis.title.y = element_text(size = 16),
        panel.grid = element_blank(),
        legend.position = "none")  # Remove legend







#FREQUENCY OF FLIGHT PRED CUE BY YEAR

#calculate flight frequency
flight_frequency_pred <- Baboon_frequency_stats_both %>%
  group_by(year, predator_cue) %>%
  summarise(
    flight_present = sum(flight_present == 1),
    total = n(),
    flight_absent = total - flight_present,
    flight_frequency = flight_present / total,  # corrected to be proportion
    se = sqrt((flight_frequency * (1 - flight_frequency)) / total),  # standard error
    .groups = "drop"
  )

#Reorder predator cues for graphing
flight_frequency_pred <- flight_frequency_pred %>%
  mutate(predator_cue = factor(predator_cue, levels = c("Cheetah", "Wild dog", "Hyena", "Leopard","Lion", "Control"))) %>%  # Adjust Cue names as needed
  filter(!is.na(predator_cue))  # Remove rows where predator_cue is NA

#add column for hunting modes
flight_frequency_pred <- flight_frequency_pred %>%
  mutate(Hunting_mode = case_when(
    predator_cue %in% c("Lion", "Leopard") ~ "Ambush",
    predator_cue %in% c("Hyena", "Wild dog", "Cheetah") ~ "Coursing",
    predator_cue %in% c("Control") ~ "Control",
    
  ))

#reorder hunting modes
flight_frequency_pred$Hunting_mode <- factor(
  flight_frequency_pred$Hunting_mode,
  levels = c("Coursing", "Ambush", "Control")  # put your desired legend AND plotting order here
)

#bar graph for frequency of flight by predator cue by year
frequency_pred_year_plot <-ggplot(flight_frequency_pred, aes(x = predator_cue, y = flight_frequency, 
                                  fill = year, pattern = Hunting_mode)) +
  geom_bar_pattern(stat = "identity", 
                   position = position_dodge(width = 0.9),
                   color = "black",
                   pattern_fill = "black", 
                   pattern_density = 0.2,
                   pattern_spacing = 0.05,
                   alpha = 0.8) +
  geom_errorbar(aes(ymin = flight_frequency - se, ymax = flight_frequency + se),
                width = 0.2,
                position = position_dodge(width = 0.9)) +
  scale_fill_manual(values = c("2021" = "#72874E", "2024" = "#476F84")) +
  scale_pattern_manual(values = c("Ambush" = "stripe", "Coursing" = "circle", "Control"="none")) +  # customize as needed
  labs(x = "Predator Cue", y = "Frequency of Flight", 
       fill = "Year", pattern = "Hunting Mode") +
  scale_x_discrete(labels = c(
    "Lion" = "Lion",
    "Leopard" = "Leopard",
    "Cheetah" = "Cheetah",
    "Hyena" = "Hyena",
    "Wild dog" = "Wild dog"
  )) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right", 
        panel.grid = element_blank())

#FREQUENCY OF FLIGHT BY PREDATOR CUE

#calculate flight frequency
flight_frequency_pred_only <- Baboon_frequency_stats_both %>%
  group_by(predator_cue) %>%
  summarise(
    flight_present = sum(flight_present == 1),
    total = n(),
    flight_absent = total - flight_present,
    flight_frequency = flight_present / total,  # corrected to be proportion
    se = sqrt((flight_frequency * (1 - flight_frequency)) / total),  # standard error
    .groups = "drop"
  )

#Reorder predator cues for graphing
flight_frequency_pred_only <- flight_frequency_pred_only %>%
  mutate(predator_cue = factor(predator_cue, levels = c("Cheetah", "Wild dog", "Hyena", "Leopard","Lion", "Control"))) %>%  # Adjust Cue names as needed
  filter(!is.na(predator_cue))  # Remove rows where predator_cue is NA

#add column for hunting modes
flight_frequency_pred_only <- flight_frequency_pred_only %>%
  mutate(Hunting_mode = case_when(
    predator_cue %in% c("Lion", "Leopard") ~ "Ambush",
    predator_cue %in% c("Hyena", "Wild dog", "Cheetah") ~ "Coursing",
    predator_cue %in% c("Control") ~ "Control",
    
  ))

#reorder hunting modes
flight_frequency_pred_only$Hunting_mode <- factor(
  flight_frequency_pred_only$Hunting_mode,
  levels = c("Coursing", "Ambush", "Control")  # put your desired legend AND plotting order here
)

frequency_pred_plot <-ggplot(flight_frequency_pred_only, aes(x = predator_cue, y = flight_frequency, 
                                                        pattern = Hunting_mode)) +
  geom_bar_pattern(stat = "identity", 
                   position = position_dodge(width = 0.9),
                   fill = "#023743",  # unified fill color
                   color = "black",
                   pattern_fill = "black", 
                   pattern_density = 0.2,
                   pattern_spacing = 0.05,
                   alpha = 0.8) +
  geom_errorbar(aes(ymin = flight_frequency - se, ymax = flight_frequency + se),
                width = 0.2,
                position = position_dodge(width = 0.9)) +
  scale_pattern_manual(values = c("Ambush" = "stripe", "Coursing" = "circle", "Control"="none")) +
  labs(x = "Predator Cue", y = "Frequency of Flight", 
       pattern = "Hunting Mode") +
  scale_x_discrete(labels = c(
    "Lion" = "Lion",
    "Leopard" = "Leopard",
    "Cheetah" = "Cheetah",
    "Hyena" = "Hyena",
    "Wild dog" = "Wild dog"
  )) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
        axis.text.y = element_text(size = 14),                         # Y-axis text size
        axis.title.x = element_text(size = 16),                        # X-axis title size
        axis.title.y = element_text(size = 16),
        legend.position = "right", 
        panel.grid = element_blank())


#FREQUENCY OF FLIGHT BY YEAR
#calculate flight frequency
flight_frequency_year <- Baboon_frequency_stats_both %>%
  group_by(year) %>%
  summarise(
    flight_present = sum(flight_present == 1),
    total = n(),
    flight_absent = total - flight_present,
    flight_frequency = flight_present / total,  # corrected to be proportion
    se = sqrt((flight_frequency * (1 - flight_frequency)) / total),  # standard error
    .groups = "drop"
  )

frequency_year_plot <-ggplot(flight_frequency_year, aes(x = year, y = flight_frequency)) +
  geom_bar(stat = "identity", width = 0.6, alpha = 0.8, fill = "#023743FF") +
  geom_errorbar(aes(ymin = flight_frequency - se, ymax = flight_frequency + se),
                width = 0.2,
                position = position_dodge(width = 0.9)) +
  labs(x = "Year", y = "Flight Frequency") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 14),                         # Y-axis text size
        axis.title.x = element_text(size = 16),                        # X-axis title size
        axis.title.y = element_text(size = 16),
        axis.title = element_text(size = 12))


#FREQUENCY OF FLIGHT HABITAT

#calculate flight frequency
flight_frequency_habitat <- Baboon_frequency_stats_both %>%
  group_by(Habitat) %>%
  summarise(
    flight_present = sum(flight_present == 1),
    total = n(),
    flight_absent = total - flight_present,
    flight_frequency = flight_present / total,  # corrected to be proportion
    se = sqrt((flight_frequency * (1 - flight_frequency)) / total),  # standard error
    .groups = "drop"
  )

frequency_habitat_plot <- ggplot(flight_frequency_habitat, aes(x = Habitat, y = flight_frequency, fill = Habitat)) +
  geom_bar(stat = "identity", width = 0.6, alpha = 0.8) +
  geom_errorbar(aes(ymin = flight_frequency - se, ymax = flight_frequency + se),
                width = 0.2,
                position = position_dodge(width = 0.9)) +
  scale_fill_manual(values = c("Open" = "#023743FF", "Closed" = "#023743FF")) +
  labs(x = "Habitat", y = "Flight Frequency") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 14),                         # Y-axis text size
        axis.title.x = element_text(size = 16),                        # X-axis title size
        axis.title.y = element_text(size = 16),
        axis.title = element_text(size = 12))

#FLIGHT FREQUENCY PREY

#calculate flight frequency
Baboon_frequency_graph_both <- Baboon_frequency_stats_both %>%
  group_by(age_sex_class) %>%
  summarise(
    flight_present = sum(flight_present == 1),
    total = n(),
    flight_absent = total - flight_present,
    flight_frequency = flight_present / total,  # corrected to be proportion
    se = sqrt((flight_frequency * (1 - flight_frequency)) / total),  # standard error
    .groups = "drop"
  )

Baboon_frequency_graph_both <- Baboon_frequency_graph_both %>%
  mutate(age_sex_class = case_when(
    age_sex_class %in% c("Female_Adult_no_offspring") ~ "Female no offspring",
    age_sex_class %in% c("Female_Adult_with_offspring") ~ "Female with offspring",
    age_sex_class %in% c("Male_Adult") ~ "Male",
    age_sex_class %in% c("Juvenile") ~ "Juvenile",
    TRUE ~ age_sex_class  # Keep all other values as they are
  ))

frequency_prey_plot <- 
  ggplot(Baboon_frequency_graph_both, aes(x = age_sex_class, y = flight_frequency)) +
  geom_bar(stat = "identity", width = 0.6, alpha = 0.8, fill = "#023743FF") +
  geom_errorbar(aes(ymin = flight_frequency - se, ymax = flight_frequency + se),
                width = 0.2,
                position = position_dodge(width = 0.9)) +
  labs(x = "Age and Sex Class", y = "Flight Frequency") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 14),                         # Y-axis text size
        axis.title.x = element_text(size = 16),                        # X-axis title size
        axis.title.y = element_text(size = 16),
        axis.title = element_text(size = 12))





