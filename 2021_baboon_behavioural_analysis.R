#Behavioural analysis for 2021 data

#load packages
install.packages("paletteer")
library(paletteer)#for colour scheme
library(dplyr)
library(ggplot2)
library(stringr)
library(tidyverse)

#DATAFRAME FOR VIGILANCE ANALYSIS

#filter videos that have No_sound or sound.quality = poor as they will not be included in analysis
Baboon_vigilance_data <- Final_2021 %>%
  filter(!(Sound_quality %in% c("Poor", "None")))

#create new column where Walking_V, Staring, standing and staring, Scanning, Startling = Vigilant, Flight = Flight, Occluded = Occluded, and any other behaviour is Non_vigilant
Baboon_vigilance_data <- Baboon_vigilance_data %>%
  mutate(behaviour_class = case_when(
    Behaviour %in% c("Walking_V", "Staring", "Scanning","Stand_stare","Startling") ~ "Vigilant",
    Behaviour == "Flight" ~ "Flight",
    Behaviour == "Occluded" ~ "Occluded",
    TRUE ~ "Non_vigilant"
  ))

#exclude videos where baboon fled immediately as they display not proportion of vigilance
Baboon_vigilance_data <- Baboon_vigilance_data %>%
  group_by(file_name) %>%
  filter(first(Behaviour) != "Flight") %>%  # Remove groups where the first row's Behaviour is "Flight"
  ungroup()

#calculate proportion time spent vigilant 
Baboon_vigilance_data <- Baboon_vigilance_data %>%
  group_by(file_name) %>%
  mutate(
    total_frames = n(),  # Count total frames per file_name
    vigilant_frames = sum(behaviour_class == "Vigilant", na.rm = TRUE),  # Count Vigilant frames
    occluded_frames = sum(behaviour_class == "Occluded", na.rm = TRUE),  # Count Occluded frames
    proportion_vigilant = ifelse(total_frames == occluded_frames, NA, vigilant_frames / (total_frames - occluded_frames))  # Compute proportion or set NA if occluded frames = total frames
  ) %>%
  ungroup()

#add column grouping predator cues by hunting mode
Baboon_vigilance_data <- Baboon_vigilance_data %>%
  mutate(Hunting_mode = case_when(
      Predator.cue %in% c("Lion", "Cheetah", "Leopard") ~ "Ambush",
      Predator.cue %in% c("Hyena", "Wild_dog") ~ "Coursing",
      Predator.cue %in% c("Control") ~ "Control",
      
  ))

#Dataframe for proportion vigilance model
Baboon_vigilance_stats <- Baboon_vigilance_data %>%
  mutate(Habitat = case_when(
    Camera.trap.site %in% c("E02", "F01", "F03", "F07", "I04", "J03", "J13", "N03", "N10", "N11") ~ "Open",
    Camera.trap.site %in% c("D05", "D09", "E08", "G06", "G08", "I06", "I08", "I10", "L11") ~ "Closed",
    TRUE ~ NA_character_  # Ensure other values get NA if not listed
  )) %>%
  mutate(age_sex_class = case_when(
    Focal.individual.sex == "J" & Focal.individual.age == "J" ~ "Juvenile",
    Focal.individual.sex == "F" & Focal.individual.age == "A" ~ "Female_Adult",
    Focal.individual.sex == "M" & Focal.individual.age == "A" ~ "Male_Adult",
    TRUE ~ NA_character_  # Default if nothing matches
  )) %>%
  mutate(Presence_of_offspring = case_when(
    Presence.of.offspring == "None" ~ 0,   # Assign 0 if "None"
    TRUE ~ 1   # Assign 1 if anything else
  )) %>%
  group_by(file_name, Habitat, age_sex_class, Camera.trap.site, Predator.cue, Number.of.individuals, Presence_of_offspring) %>%
  summarise(
    proportion_vigilant = first(na.omit(proportion_vigilant)),  # Get first non-NA value
    .groups = "drop"
  ) %>%
  drop_na(proportion_vigilant, Predator.cue, Habitat, age_sex_class, Number.of.individuals, Presence_of_offspring) #need to drop NAs from proportion vigilant where total_frames = occluded_frames

#DATAFRAME FOR LATENCY TO FLEE

#filter videos that have No_sound or sound.quality = poor or a sound delay as they will not be included in analysis
Baboon_flight_data <- Final_2021 %>%
  filter(
    !(Sound_quality %in% c("Poor", "None")),  # Exclude Poor and None Sound_quality
    Sound_delay_s == "None"  # Keep only rows where Sound_delay_s is "None"
  )

#create column by grouping videos by file_name and then labeling them with 0 if no flight present and 1 if flight present
Baboon_flight_data <- Baboon_flight_data %>%
  group_by(file_name) %>% 
  mutate(flight_present = if_else(any(str_detect(Behaviour, "Flight")), 1, 0)) %>%
  ungroup()

#filter for videos where flight is present (flight_present = 1) 
Baboon_flight_data <- Baboon_flight_data %>%
  filter(flight_present == 1)

#calculate latency to flee
Baboon_flight_data <- Baboon_flight_data %>%
  group_by(file_name) %>%
  arrange(frame) %>%  # Arrange by frame within each file_name
  mutate(
    first_row = first(row_number()),  # Get the first row number of each video
    rows_until_flight = if_else(
      flight_present == 1 & Behaviour == "Flight" & row_number() == min(which(Behaviour == "Flight")), 
      row_number() - first_row, 
      NA_integer_
    )
  ) %>%
  ungroup() %>%
  group_by(file_name) %>%
  mutate(rows_until_flight = if_else(flight_present == 1, min(rows_until_flight, na.rm = TRUE), NA_integer_)) %>%
  ungroup()

#convert frames until seconds = latency by dividing by 30 bc 1s = 30 frames
Baboon_flight_data <- Baboon_flight_data %>%
  mutate(latency_to_flee_s = rows_until_flight / 30)

#add column grouping predator cues by hunting mode
Baboon_flight_data <- Baboon_flight_data %>%
  mutate(Hunting_mode = case_when(
    Predator.cue %in% c("Lion", "Cheetah", "Leopard") ~ "Ambush",
    Predator.cue %in% c("Hyena", "Wild_dog") ~ "Coursing",
    Predator.cue %in% c("Control") ~ "Control",
    
  ))

#Dataframe for latency to flee model
Baboon_flight_stats <- Baboon_flight_data %>%
  mutate(Habitat = case_when(
    Camera.trap.site %in% c("E02", "F01", "F03", "F07", "I04", "J03", "J13", "N03", "N10", "N11") ~ "Open",
    Camera.trap.site %in% c("D05", "D09", "E08", "G06", "G08", "I06", "I08", "I10", "L11") ~ "Closed",
    TRUE ~ NA_character_  # Ensure other values get NA if not listed
  )) %>%
  mutate(age_sex_class = case_when(
    Focal.individual.sex == "J" & Focal.individual.age == "J" ~ "Juvenile",
    Focal.individual.sex == "F" & Focal.individual.age == "A" ~ "Female_Adult",
    Focal.individual.sex == "M" & Focal.individual.age == "A" ~ "Male_Adult",
    TRUE ~ NA_character_  # Default if nothing matches
  )) %>%
  mutate(Presence_of_offspring = case_when(
    Presence.of.offspring == "None" ~ 0,   # Assign 0 if "None"
    TRUE ~ 1   # Assign 1 if anything else
  )) %>%
  group_by(file_name, Habitat, age_sex_class, Camera.trap.site, Predator.cue, Number.of.individuals, Presence_of_offspring) %>%
  summarise(
  latency_to_flee = first(na.omit(latency_to_flee_s)),  # Get first non-NA value
    .groups = "drop"
      ) %>%
  drop_na(latency_to_flee, Predator.cue, Habitat, age_sex_class, Number.of.individuals, Presence_of_offspring)%>% #need to drop one video where age_sex_class is NA for analysis
  mutate(log_latency_to_flee = log(latency_to_flee + 1)) 

#DATAFRAME FOR FLIGHT FREQUENCY

#filter videos that have No_sound or sound.quality = poor or a sound delay as they will not be included in analysis
Baboon_frequency_data <- Final_2021 %>%
  filter(
    !(Sound_quality %in% c("Poor", "None")),  # Exclude Poor and None Sound_quality
    Sound_delay_s == "None"  # Keep only rows where Sound_delay_s is "None"
  )

#create column by grouping videos by file_name and then labeling them with 0 if no flight present and 1 if flight present
Baboon_frequency_data <- Baboon_frequency_data %>%
  group_by(file_name) %>% 
  mutate(flight_present = if_else(any(str_detect(Behaviour, "Flight")), 1, 0)) %>%
  ungroup()

#add column grouping predator cues by hunting mode
Baboon_frequency_data <- Baboon_frequency_data %>%
  mutate(Hunting_mode = case_when(
    Predator.cue %in% c("Lion", "Cheetah", "Leopard") ~ "Ambush",
    Predator.cue %in% c("Hyena", "Wild_dog") ~ "Coursing",
    Predator.cue %in% c("Control") ~ "Control",
    
  ))

#Dataframe for flight frequency model
Baboon_frequency_stats <- Baboon_frequency_data %>%
  mutate(Habitat = case_when(
    Camera.trap.site %in% c("E02", "F01", "F03", "F07", "I04", "J03", "J13", "N03", "N10", "N11") ~ "Open",
    Camera.trap.site %in% c("D05", "D09", "E08", "G06", "G08", "I06", "I08", "I10", "L11") ~ "Closed",
    TRUE ~ NA_character_  # Ensure other values get NA if not listed
  )) %>%
  mutate(age_sex_class = case_when(
    Focal.individual.sex == "J" & Focal.individual.age == "J" ~ "Juvenile",
    Focal.individual.sex == "F" & Focal.individual.age == "A" ~ "Female_Adult",
    Focal.individual.sex == "M" & Focal.individual.age == "A" ~ "Male_Adult",
    TRUE ~ NA_character_  # Default if nothing matches
  )) %>%
  mutate(Presence_of_offspring = case_when(
    Presence.of.offspring == "None" ~ 0,   # Assign 0 if "None"
    TRUE ~ 1   # Assign 1 if anything else
  )) %>%
  group_by(file_name, Habitat, age_sex_class, Camera.trap.site, Predator.cue, Number.of.individuals, Presence_of_offspring, flight_present) %>%
  summarise(
    flight_present = first(na.omit(flight_present)),  # Get first non-NA value
    .groups = "drop"
  ) %>%
  drop_na(Predator.cue, Habitat, age_sex_class, Number.of.individuals, Presence_of_offspring) #need to drop NAs from proportion vigilant where total_frames = occluded_frames

#PREDATOR IDENTITY ANALYSIS

#PREDATOR IDENTITY VIGILANCE 

#group by file_name and exclude those that fled immediately
Baboon_vigilance_predatorcue <- Baboon_vigilance_data %>%
  group_by(Predator.cue, file_name, Hunting_mode) %>%
  summarise(proportion_vigilant = first(na.omit(proportion_vigilant)), .groups = "drop") #because NA values are videos where the baboon was occluded the entire time

#Reorder predator cues for graphing
Baboon_vigilance_predatorcue <- Baboon_vigilance_predatorcue %>%
  mutate(Predator.cue = factor(Predator.cue, levels = c("Leopard", "Cheetah", "Lion","Wild_dog","Hyena", "Control")))  # Adjust Cue names as needed

#Strip plot for proportion of vigilance by predator cue
vigilance_predcue_plot_21 <-
  ggplot(Baboon_vigilance_predatorcue, aes(x = Predator.cue, y = proportion_vigilant, fill = Hunting_mode, color = Hunting_mode)) +
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
        legend.position = "none")  

#PREDATOR IDENTITY LATENCY TO FLEE

#group by file_name and reorder by predator.cue
Baboon_flight_predatorcue <- Baboon_flight_data %>%
  filter(file_name != "2021_F07_07220010_Baboon.AVI") %>%  # Exclude outlier
  group_by(file_name, Predator.cue, Hunting_mode) %>%
  summarise(
    latency_to_flee = first(na.omit(latency_to_flee_s)),  # Get first non-NA value
    .groups = "drop"
  ) %>%
  mutate(Predator.cue = factor(Predator.cue, levels = c("Leopard", "Cheetah", "Lion","Wild_dog","Hyena", "Control")))  # Adjust Cue names as needed

#Strip plot for latency to flee by predator cue
ggplot(Baboon_flight_predatorcue, aes(x = Predator.cue, y = latency_to_flee, fill = Hunting_mode, color = Hunting_mode)) +
  geom_boxplot(alpha = 0.4, outlier.shape = NA) +  # Lighter box plot
  geom_jitter(width = 0.2, size = 1.2, alpha = 0.8) +  # Smaller points
  scale_colour_paletteer_d("nationalparkcolors::Acadia") +  # Color scheme for points
  scale_fill_paletteer_d("nationalparkcolors::Acadia") +  # Color scheme for box fill
  labs(
       x = "Predator Cue",
       y = "Latency to Flee (s)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank(),
        legend.position = "right")  

#Log transform latency_to_flee because right skewed and non-normal
Baboon_flight_predatorcue <- Baboon_flight_predatorcue %>%
  mutate(log_latency = log(latency_to_flee + 1))

#Strip plot for log latency to flee by predator cue
ggplot(Baboon_flight_predatorcue, aes(x = Predator.cue, y = log_latency, fill = Hunting_mode, color = Hunting_mode)) +
  geom_boxplot(alpha = 0.4, outlier.shape = NA) +  # Lighter box plot
  geom_jitter(width = 0.2, size = 1.2, alpha = 0.8) +  # Smaller points
  scale_colour_paletteer_d("nationalparkcolors::Acadia") +  # Color scheme for points
  scale_fill_paletteer_d("nationalparkcolors::Acadia") +  # Color scheme for box fill
  labs(
    x = "Predator Cue",
    y = "Log of Latency to Flee (s)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank(),
        legend.position = "right")  

#PREDATOR IDENTITY FLIGHT FREQUENCY

#group flight data by predator cue and file_name
flight_frequency_predatorcue <- Baboon_frequency_data %>%
  group_by(Predator.cue, file_name, Hunting_mode) %>%  
  summarise(flight_present = max(flight_present), .groups = "drop") %>%  # Check if flight occurred per video
  group_by(Predator.cue, flight_present, Hunting_mode) %>%
  summarise(Count = n(), .groups = "drop") %>%  # Count occurrences of flight/no flight
  mutate(Predator.cue = factor(Predator.cue, levels = c("Leopard", "Cheetah", "Lion","Wild_dog","Hyena", "Control"))) %>%  
  mutate(flight_present = factor(flight_present, levels = c(0, 1), labels = c("No Flight", "Flight")))

#Stacked bar graph for frequency of flight by predator cue
ggplot(flight_frequency_predatorcue, aes(x = Predator.cue, y = Count, fill = Hunting_mode, alpha = flight_present)) +
  geom_bar(stat = "identity", position = "fill") +  # Proportional stacking
  scale_fill_paletteer_d("nationalparkcolors::Acadia") +  # Apply Acadia colors to Predator.cue
  scale_alpha_manual(values = c("No Flight" = 0.3, "Flight" = 1)) +  # Adjust transparency
  labs(x = "Predator Cue", y = "Relative frequency of flight", fill = "Predator Cue", alpha = "Flight Response") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
  legend.position = "right", 
  panel.grid = element_blank())

#HABITAT TYPE ANALYSIS

#HABITAT TYPE VIGILANCE

#Make new coloumn  and group by habitat classification by ABR site
Baboon_vigilance_habitat <- Baboon_vigilance_data %>%
  mutate(Habitat = case_when(
    Camera.trap.site %in% c("E02", "F01", "F03", "F07", "I04", "J03", "J13", "N03", "N10", "N11") ~ "Open",
    Camera.trap.site %in% c("D05", "D09", "E08", "G06", "G08", "I06", "I08", "I10", "L11") ~ "Closed",
    TRUE ~ NA_character_  # Ensure other values get NA if not listed
  )) %>%
  group_by(Habitat, file_name, Camera.trap.site) %>%
  summarise(proportion_vigilant = first(na.omit(proportion_vigilant)), .groups = "drop") 

#strip plot for proportion vigilance by habitat type
ggplot(Baboon_vigilance_habitat, aes(x = Habitat, y = proportion_vigilant, fill = Habitat, color = Habitat)) +
  geom_boxplot(alpha = 0.4, outlier.shape = NA) +  # Lighter box plot
  geom_jitter(width = 0.2, size = 1.2, alpha = 0.8) +  # Smaller points
  scale_colour_paletteer_d("nationalparkcolors::Acadia") +  # Color scheme for points
  scale_fill_paletteer_d("nationalparkcolors::Acadia") +  # Color scheme for box fill
  labs(
       x = "Habitat type",
       y = "Proportion of time vigilant") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank(),
        legend.position = "none") 

#HABITAT TYPE LATENCY TO FLEE

##Make new coloumn  and group by habitat classification by ABR site
Baboon_flight_habitat <- Baboon_flight_data %>%
  filter(file_name != "2021_F07_07220010_Baboon.AVI") %>%  # Exclude outlier
  mutate(Habitat = case_when(
    Camera.trap.site %in% c("E02", "F01", "F03", "F07", "I04", "J03", "J13", "N03", "N10", "N11") ~ "Open",
    Camera.trap.site %in% c("D05", "D09", "E08", "G06", "G08", "I06", "I08", "I10", "L11") ~ "Closed",
    TRUE ~ NA_character_  # Ensure other values get NA if not listed
  )) %>%
  group_by(Habitat, file_name, Camera.trap.site) %>%
  summarise(
    latency_to_flee = first(na.omit(latency_to_flee_s)),  # Get first non-NA value
    .groups = "drop"
  )

#Strip plot for latency to flight by habitat type
ggplot(Baboon_flight_habitat, aes(x = Habitat, y = latency_to_flee, fill = Habitat, color = Habitat)) +
  geom_boxplot(alpha = 0.4, outlier.shape = NA) +  # Lighter box plot
  geom_jitter(width = 0.2, size = 1.2, alpha = 0.8) +  # Smaller points
  scale_colour_paletteer_d("nationalparkcolors::Acadia") +  # Color scheme for points
  scale_fill_paletteer_d("nationalparkcolors::Acadia") +  # Color scheme for box fill
  labs(
       x = "Habitat type",
       y = "Latency to Flee (s)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank(),
        legend.position = "none") 

#Log transform latency_to_flee because zero skewed
Baboon_flight_habitat <- Baboon_flight_habitat %>%
  mutate(log_latency = log(latency_to_flee + 1))

#Strip plot for log latency to flee by predator cue
ggplot(Baboon_flight_habitat, aes(x = Habitat, y = log_latency, fill = Habitat, color = Habitat)) +
  geom_boxplot(alpha = 0.4, outlier.shape = NA) +  # Lighter box plot
  geom_jitter(width = 0.2, size = 1.2, alpha = 0.8) +  # Smaller points
  scale_colour_paletteer_d("nationalparkcolors::Acadia") +  # Color scheme for points
  scale_fill_paletteer_d("nationalparkcolors::Acadia") +  # Color scheme for box fill
  labs(
    x = "Habitat Type",
    y = "Log of Latency to Flee (s)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank(),
        legend.position = "none")  


#HABITAT TYPE FLIGHT FREQUENCY

#group flight data by Habitat type and file_name
flight_frequency_habitat <- Baboon_frequency_data %>%
  filter(file_name != "2021_F07_07220010_Baboon.AVI") %>%  # Exclude outlier
  mutate(Habitat = case_when(
    Camera.trap.site %in% c("E02", "F01", "F03", "F07", "I04", "J03", "J13", "N03", "N10", "N11") ~ "Open",
    Camera.trap.site %in% c("D05", "D09", "E08", "G06", "G08", "I06", "I08", "I10", "L11") ~ "Closed",
    TRUE ~ NA_character_  # Ensure other values get NA if not listed
  )) %>%
  group_by(Habitat, file_name) %>%  
  summarise(flight_present = max(flight_present), .groups = "drop") %>%  # Check if flight occurred per video
  group_by(Habitat, flight_present) %>%
  summarise(Count = n(), .groups = "drop") %>%  # Count occurrences of flight/no flight
  mutate(flight_present = factor(flight_present, levels = c(0, 1), labels = c("No Flight", "Flight")))

#Stacked bar graph for frequency of flight by habitat type
ggplot(flight_frequency_habitat, aes(x = Habitat, y = Count, fill = Habitat, alpha = flight_present)) +
  geom_bar(stat = "identity", position = "fill") +  # Proportional stacking
  scale_fill_paletteer_d("nationalparkcolors::Acadia") +  # Apply Acadia colors to Predator.cue
  scale_alpha_manual(values = c("No Flight" = 0.3, "Flight" = 1)) +  # Adjust transparency
  labs(x = "Habitat type", y = "Relative frequency of flight", fill = "Predator Cue", alpha = "Flight Response") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none", 
        panel.grid = element_blank())

#PREY IDENTITY ANALYSIS - AGE/SEX

#PREY IDENTITY VIGILANCE 

#Group by file_name and create a new column for age_sex_class
Baboon_vigilance_age_sex <- Baboon_vigilance_data %>%
  filter(!is.na(Focal.individual.sex)) %>%
  group_by(file_name, Focal.individual.sex, Focal.individual.age, Number.of.individuals, Presence.of.offspring) %>%
  summarise(proportion_vigilant = first(na.omit(proportion_vigilant)), .groups = "drop") %>%
  mutate(age_sex_class = case_when(
    Focal.individual.sex == "J" & Focal.individual.age == "J" ~ "Juvenile",
    Focal.individual.sex == "F" & Focal.individual.age == "A" ~ "Female_Adult",
    Focal.individual.sex == "M" & Focal.individual.age == "A" ~ "Male_Adult",
    TRUE ~ NA_character_  # Default if nothing matches
  ))

#strip plot for proportion vigilance by age_sex_class
ggplot(Baboon_vigilance_age_sex, aes(x = age_sex_class, y = proportion_vigilant, fill = age_sex_class, color = age_sex_class)) +
  geom_boxplot(alpha = 0.4, outlier.shape = NA) +  # Lighter box plot
  geom_jitter(width = 0.2, size = 1.2, alpha = 0.8) +  # Smaller points
  scale_colour_paletteer_d("nationalparkcolors::Acadia") +  # Color scheme for points
  scale_fill_paletteer_d("nationalparkcolors::Acadia") +  # Color scheme for box fill
  labs(
       x = "Baboon age and sex class",
       y = "Proportion of time vigilant") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank(),
        legend.position = "none") 

#PREY IDENTITY LATENCY TO FLIGHT

#Group by file_name and create a new column for age_sex_class
Baboon_flight_age_sex <- Baboon_flight_data %>%
  filter(!is.na(Focal.individual.sex)) %>%
  filter(file_name != "2021_F07_07220010_Baboon.AVI") %>%
  mutate(age_sex_class = case_when(
    Focal.individual.sex == "J" & Focal.individual.age == "J" ~ "Juvenile",
    Focal.individual.sex == "F" & Focal.individual.age == "A" ~ "Female_Adult",
    Focal.individual.sex == "M" & Focal.individual.age == "A" ~ "Male_Adult",
    TRUE ~ NA_character_  # Default if nothing matches
  )) %>%
  group_by(age_sex_class, file_name) %>%
  summarize(latency_to_flee = first(na.omit(latency_to_flee_s)),  # Get first non-NA value
  .groups = "drop"
    )

#Strip plot for latency to flight by age sex class
ggplot(Baboon_flight_age_sex, aes(x = age_sex_class, y = latency_to_flee, fill = age_sex_class, color = age_sex_class)) +
  geom_boxplot(alpha = 0.4, outlier.shape = NA) +  # Lighter box plot
  geom_jitter(width = 0.2, size = 1.2, alpha = 0.8) +  # Smaller points
  scale_colour_paletteer_d("nationalparkcolors::Acadia") +  # Color scheme for points
  scale_fill_paletteer_d("nationalparkcolors::Acadia") +  # Color scheme for box fill
  labs(
       x = "Baboon age and sex class",
       y = "Latency to Flee (s)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank(),
        legend.position = "none")

#Log transform latency_to_flee because zero skewed
Baboon_flight_age_sex <- Baboon_flight_age_sex %>%
  mutate(log_latency = log(latency_to_flee + 1))

#Strip plot for log latency to flee by predator cue
ggplot(Baboon_flight_age_sex, aes(x = age_sex_class, y = log_latency, fill = age_sex_class, color = age_sex_class)) +
  geom_boxplot(alpha = 0.4, outlier.shape = NA) +  # Lighter box plot
  geom_jitter(width = 0.2, size = 1.2, alpha = 0.8) +  # Smaller points
  scale_colour_paletteer_d("nationalparkcolors::Acadia") +  # Color scheme for points
  scale_fill_paletteer_d("nationalparkcolors::Acadia") +  # Color scheme for box fill
  labs(
    x = "Baboon Age and Sex Class",
    y = "Log of Latency to Flee (s)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank(),
        legend.position = "none")  


#PREY IDENTITY FLIGHT FREQUENCY

#group flight data by age_sex_class and file_name
flight_frequency_age_sex <- Baboon_frequency_data %>%
  filter(!is.na(Focal.individual.sex)) %>%
  mutate(age_sex_class = case_when(
    Focal.individual.sex == "J" & Focal.individual.age == "J" ~ "Juvenile",
    Focal.individual.sex == "F" & Focal.individual.age == "A" ~ "Female_Adult",
    Focal.individual.sex == "M" & Focal.individual.age == "A" ~ "Male_Adult",
    TRUE ~ NA_character_  # Default if nothing matches
  )) %>%
  group_by(age_sex_class, file_name) %>%  
  summarise(flight_present = max(flight_present), .groups = "drop") %>%  # Check if flight occurred per video
  group_by(age_sex_class, flight_present) %>%
  summarise(Count = n(), .groups = "drop") %>%  # Count occurrences of flight/no flight
  mutate(flight_present = factor(flight_present, levels = c(0, 1), labels = c("No Flight", "Flight")))

#Stacked bar graph for frequency of flight by habitat type
ggplot(flight_frequency_age_sex, aes(x = age_sex_class, y = Count, fill = age_sex_class, alpha = flight_present)) +
  geom_bar(stat = "identity", position = "fill") +  # Proportional stacking
  scale_fill_paletteer_d("nationalparkcolors::Acadia") +  # Apply Acadia colors to Predator.cue
  scale_alpha_manual(values = c("No Flight" = 0.3, "Flight" = 1)) +  # Adjust transparency
  labs(x = "Baboon Age and Sex Class", y = "Relative frequency of flight", fill = "age_sex_class", alpha = "Flight Response") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none", 
        panel.grid = element_blank())
