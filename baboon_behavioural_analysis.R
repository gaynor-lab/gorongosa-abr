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

#DATAFRAME FOR FLIGHT ANALYSIS

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


#PREDATOR IDENTITY ANALYSIS

#PREDATOR IDENTITY VIGILANCE 

#group by file_name and exclude those that fled immediately
Baboon_vigilance_predatorcue <- Baboon_vigilance_data %>%
  group_by(Predator.cue, file_name) %>%
  summarise(proportion_vigilant = first(na.omit(proportion_vigilant)), .groups = "drop") #because NA values are videos where the baboon was occluded the entire time

#Reorder predator cues for graphing
Baboon_vigilance_predatorcue <- Baboon_vigilance_predatorcue %>%
  mutate(Predator.cue = factor(Predator.cue, levels = c("Leopard", "Cheetah", "Lion","Wild_dog","Hyena", "Control")))  # Adjust Cue names as needed

#Strip plot for proportion of vigilance by predator cue
ggplot(Baboon_vigilance_predatorcue, aes(x = Predator.cue, y = proportion_vigilant, fill = Predator.cue, color = Predator.cue)) +
  geom_boxplot(alpha = 0.4, outlier.shape = NA) +  # Lighter box plot
  geom_jitter(width = 0.2, size = 1.2, alpha = 0.8) +  # Smaller points
  scale_fill_manual(values = c("Lion" = paletteer_d("nationalparkcolors::Acadia")[3], 
                               "Cheetah" = paletteer_d("nationalparkcolors::Acadia")[3], 
                               "Leopard" = paletteer_d("nationalparkcolors::Acadia")[3],
                               "Wild_dog" = paletteer_d("nationalparkcolors::Acadia")[5], 
                               "Hyena" = paletteer_d("nationalparkcolors::Acadia")[5],
                               "Control" = paletteer_d("nationalparkcolors::Acadia")[6])) +  # Assign colors from the palette
  scale_color_manual(values = c("Lion" = paletteer_d("nationalparkcolors::Acadia")[3], 
                                "Cheetah" = paletteer_d("nationalparkcolors::Acadia")[3], 
                                "Leopard" = paletteer_d("nationalparkcolors::Acadia")[3],
                                "Wild_dog" = paletteer_d("nationalparkcolors::Acadia")[5], 
                                "Hyena" = paletteer_d("nationalparkcolors::Acadia")[5],
                                "Control" = paletteer_d("nationalparkcolors::Acadia")[6])) +  # Assign colors for points
  labs(
    x = "Predator Cue",
    y = "Proportion Vigilant") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank(),
        legend.position = "right")  

#PREDATOR IDENTITY FLIGHT

#group by file_name and reorder by predator.cue
Baboon_flight_predatorcue <- Baboon_flight_data %>%
  filter(file_name != "2021_F07_07220010_Baboon.AVI") %>%  # Exclude outlier
  group_by(file_name, Predator.cue) %>%
  summarise(
    latency_to_flee = first(na.omit(latency_to_flee_s)),  # Get first non-NA value
    .groups = "drop"
  ) %>%
  mutate(Predator.cue = factor(Predator.cue, levels = c("Leopard", "Cheetah", "Lion","Wild_dog","Hyena", "Control")))  # Adjust Cue names as needed

View(Baboon_flight_predatorcue)
#Strip plot for latency to flee by predator cue
ggplot(Baboon_flight_predatorcue, aes(x = Predator.cue, y = latency_to_flee, fill = Predator.cue, color = Predator.cue)) +
  geom_boxplot(alpha = 0.4, outlier.shape = NA) +  # Lighter box plot
  geom_jitter(width = 0.2, size = 1.2, alpha = 0.8) +  # Smaller points
  scale_colour_paletteer_d("nationalparkcolors::Acadia") +  # Color scheme for points
  scale_fill_paletteer_d("nationalparkcolors::Acadia") +  # Color scheme for box fill
  labs(
       x = "Predator Cue",
       y = "Latency to Flee (seconds)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank(),
        legend.position = "none")  


#Make frame for frequency of flight
flight_frequency_pred <- Baboon_behaviour_data %>%
  group_by(Predator.cue, file_name) %>%  
  summarise(flight_present = max(flight_present), .groups = "drop") %>%  
  group_by(Predator.cue) %>%
  summarise(
    flight_yes = sum(flight_present == 1),  # Count unique files where flight happened
    flight_no = sum(flight_present == 0),   # Count unique files where flight didn't happen
    n = flight_yes + flight_no,  # Total unique files per predator cue
    proportion_flight = flight_yes / n,  # Calculate proportion of flight events
    se = sqrt((proportion_flight * (1 - proportion_flight)) / n),  # Standard error
    lower_ci = proportion_flight - 1.96 * se,  # Lower bound of 95% CI
    upper_ci = proportion_flight + 1.96 * se   # Upper bound of 95% CI
  )

View(flight_frequency_pred)

#Bar graph for proportion of flight by predator cue 
#filter data to remove No_sound group and reorder predator cues
flight_frequency_pred <- flight_frequency_pred %>%
  filter(Predator.cue != "No_sound") %>%
  mutate(Predator.cue = factor(Predator.cue, levels = c("Leopard", "Cheetah", "Lion","Wild_dog","Hyena", "Control")))  # Adjust Cue names as needed

ggplot(flight_frequency_pred, aes(x = Predator.cue, y = proportion_flight, fill = Predator.cue)) +
  geom_bar(stat = "identity", position = "dodge") +  # Bar plot
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2, color = "black") +  # Add error bars
  labs(
    title = "Proportion of Flight Responses by Predator Cue",
    x = "Predator Cue",
    y = "Frequency Flight"
  ) +
  scale_colour_paletteer_d("nationalparkcolors::Acadia") +  # Color scheme for points
  scale_fill_paletteer_d("nationalparkcolors::Acadia") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),  # Remove all grid lines
    legend.position = "none",  # Remove legend
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for readability
  )

#Grouped bar graph for frequency of flight 
# Reshape data to long format for ggplot
flight_frequency_long <- flight_frequency_pred %>%
  select(Predator.cue, flight_yes, flight_no) %>%
  pivot_longer(cols = c(flight_yes, flight_no), 
               names_to = "Flight_Response", 
               values_to = "Count") 

# Create grouped bar plot
ggplot(flight_frequency_long, aes(x = Predator.cue, y = Count, fill = Flight_Response)) +
  geom_col(position = "dodge") +  # Grouped bars (side-by-side)
  scale_fill_manual(
    values = c("flight_yes" = "#344E41", "flight_no" = "#A3B18A"),  # Dark Green & Light Green
    labels = c("flight_yes" = "Flight", "flight_no" = "No flight")  # Custom legend labels
  ) +
  labs(
       x = "Predator Cue",
       y = "Count of Flight Responses",
       fill = "Flight Response") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for readability
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

#retrying that but with confidence intervals

# Compute flight response data (wide format)
flight_frequency_pred <- Baboon_behaviour_data %>%
  group_by(Predator.cue, file_name) %>%
  summarise(flight_present = max(flight_present), .groups = "drop") %>%  
  group_by(Predator.cue) %>%
  summarise(
    flight_yes = sum(flight_present == 1),  
    flight_no = sum(flight_present == 0),   
    n = flight_yes + flight_no,  
    proportion_flight = flight_yes / n,  
    proportion_no_flight = flight_no / n,  
    
    # Standard errors
    se_flight = sqrt((proportion_flight * (1 - proportion_flight)) / n),
    se_no_flight = sqrt((proportion_no_flight * (1 - proportion_no_flight)) / n),
    
    # 95% Confidence Intervals
    lower_ci_flight = proportion_flight - 1.96 * se_flight,
    upper_ci_flight = proportion_flight + 1.96 * se_flight,
    
    lower_ci_no_flight = proportion_no_flight - 1.96 * se_no_flight,
    upper_ci_no_flight = proportion_no_flight + 1.96 * se_no_flight
  )

# Convert to long format and exclude No_sound
flight_frequency_long <- flight_frequency_pred %>%
  filter(Predator.cue != "No_sound") %>%
  mutate(Predator.cue = factor(Predator.cue, levels = c("Leopard", "Cheetah", "Lion","Wild_dog","Hyena", "Control"))) %>%
  pivot_longer(
    cols = c(flight_yes, flight_no),  # Convert these columns to long format
    names_to = "Flight_Response",  # New column name
    values_to = "Count"  # Store counts in this column
  ) %>%
  mutate(
    proportion = ifelse(Flight_Response == "flight_yes", proportion_flight, proportion_no_flight),
    se = ifelse(Flight_Response == "flight_yes", se_flight, se_no_flight),
    lower_ci = ifelse(Flight_Response == "flight_yes", lower_ci_flight, lower_ci_no_flight),
    upper_ci = ifelse(Flight_Response == "flight_yes", upper_ci_flight, upper_ci_no_flight)
  ) %>%
  select(Predator.cue, Flight_Response, Count, proportion, se, lower_ci, upper_ci)  # Keep only necessary columns

#graph
ggplot(flight_frequency_long, aes(x = Predator.cue, y = Count, fill = Flight_Response)) +
  geom_col(position = position_dodge(width = 0.9)) +  # Grouped bars (side-by-side)
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), 
                position = position_dodge(width = 0.9), 
                width = 0.2, color = "black") +  # Black error bars
  scale_fill_manual(
    values = c("flight_yes" = "#344E41", "flight_no" = "#A3B18A"),  
    labels = c("flight_yes" = "Flight", "flight_no" = "No flight")  
  ) +
  labs(title = "Flight Response by Predator Cue",
       x = "Predator Cue",
       y = "Count of Flight Responses",
       fill = "Flight Response") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())




#HABITAT TYPE ANALYSIS
#create new column with habitat type
Baboon_behaviour_data <- Baboon_behaviour_data %>%
  mutate(Habitat = case_when(
    Camera.trap.site %in% c("E02", "F01", "F03","F07","I04","J03","J13","N03","N10","N11") ~ "Open",
    Camera.trap.site %in% c("D05", "D09","E08","G06","G08","I06","I08","I10","L11") ~ "Closed",
  ))

#HABITAT TYPE VIGILANCE
#group by habitat type
Baboon_vigilance_habitat <- Baboon_behaviour_data %>%
  filter(is.na(latency_to_flee_s) | latency_to_flee_s != 0) %>% # Exclude files with latency_to_flee_s = 0
  group_by(Habitat, file_name, Camera.trap.site) %>%
  summarise(proportion_vigilant = first(na.omit(proportion_vigilant)), .groups = "drop") #because NA values are videos where the baboon was occluded the entire time
View(Baboon_vigilance_habitat)


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


#HABITAT TYPE FLIGHT
#Calculate mean latency to flight by habitat type
Baboon_flight_data_habitat <- Baboon_flight_data %>%
  group_by(Habitat, file_name) %>%
  summarize(mean_latency_to_flee = mean(latency_to_flee_s, na.rm = TRUE), .groups = "drop") %>%  # Keep pipeline open
  filter(file_name != "2021_F07_07220010_Baboon.AVI") 

#Strip plot for latency to flight by habitat type
ggplot(Baboon_flight_data_habitat, aes(x = Habitat, y = mean_latency_to_flee, fill = Habitat, color = Habitat)) +
  geom_boxplot(alpha = 0.4, outlier.shape = NA) +  # Lighter box plot
  geom_jitter(width = 0.2, size = 1.2, alpha = 0.8) +  # Smaller points
  scale_colour_paletteer_d("nationalparkcolors::Acadia") +  # Color scheme for points
  scale_fill_paletteer_d("nationalparkcolors::Acadia") +  # Color scheme for box fill
  labs(
       x = "Habitat type",
       y = "Latency to Flee (seconds)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank(),
        legend.position = "none")  


#frequency of flight by habitat type
flight_frequency_habitat <- Baboon_behaviour_data %>%
  filter(!is.na(Habitat)) %>% 
  group_by(Habitat, file_name) %>%  
  summarise(flight_present = max(flight_present), .groups = "drop") %>%  
  group_by(Habitat) %>%
  summarise(
    flight_yes = sum(flight_present == 1),  # Count unique files where flight happened
    flight_no = sum(flight_present == 0),   # Count unique files where flight didn't happen
    n = flight_yes + flight_no,  # Total unique files per predator cue
    proportion_flight = flight_yes / n,  # Calculate proportion of flight events
    se = sqrt((proportion_flight * (1 - proportion_flight)) / n),  # Standard error
    lower_ci = proportion_flight - 1.96 * se,  # Lower bound of 95% CI
    upper_ci = proportion_flight + 1.96 * se   # Upper bound of 95% CI
  )

#bar graph for frequency of flight by habitat type
ggplot(flight_frequency_habitat, aes(x = Habitat, y = proportion_flight, fill = Habitat)) +
  geom_bar(stat = "identity", position = "dodge") +  # Bar plot
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2, color = "black") +  # Add error bars
  labs(
    title = "Proportion of Flight Responses by Habitat Type",
    x = "Habitat type",
    y = "Frequency Flight"
  ) +
  scale_colour_paletteer_d("nationalparkcolors::Acadia") +  # Color scheme for points
  scale_fill_paletteer_d("nationalparkcolors::Acadia") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),  # Removes all grid lines
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for readability
  )

#grouped bar graph for frequency of flight by habitat type

# Reshape data to long format for ggplot
flight_frequency_long_habitat <- flight_frequency_habitat %>%
  select(Habitat, flight_yes, flight_no) %>%
  pivot_longer(cols = c(flight_yes, flight_no), 
               names_to = "Flight_Response", 
               values_to = "Count") 

# Create grouped bar plot
ggplot(flight_frequency_long_habitat, aes(x = Habitat, y = Count, fill = Flight_Response)) +
  geom_col(position = "dodge") +  # Grouped bars (side-by-side)
  scale_fill_manual(
    values = c("flight_yes" = "#344E41", "flight_no" = "#A3B18A"),  # Dark Green & Light Green
    labels = c("flight_yes" = "Flight", "flight_no" = "No flight")  # Custom legend labels
  ) +
  labs(
       x = "Habitat Type",
       y = "Count of Flight Responses",
       fill = "Flight Response") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for readability
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())



#PREY IDENTITY ANALYSIS - AGE/SEX

#PREY IDENTITY VIGILANCE
#group by age and sex class
Baboon_vigilance_age_sex <- Baboon_behaviour_data %>%
  filter(is.na(latency_to_flee_s) | latency_to_flee_s != 0) %>% # Exclude files with latency_to_flee_s = 0
  filter(!is.na(Focal.individual.sex)) %>% 
  group_by(file_name, Focal.individual.sex, Focal.individual.age, Number.of.individuals, Presence.of.offspring) %>%
  summarise(proportion_vigilant = first(na.omit(proportion_vigilant)), .groups = "drop") #because NA values are videos where the baboon was occluded the entire time
View(Baboon_vigilance_age_sex)
View(Baboon_behaviour_data)

#make new column for age_sex_category
Baboon_vigilance_age_sex <- Baboon_vigilance_age_sex %>%
  mutate(Age_sex_Category = case_when(
    Focal.individual.sex == "J" & Focal.individual.age == "J" ~ "Juvenile",
    Focal.individual.sex == "F" & Focal.individual.age == "A" ~ "Female_Adult",
    Focal.individual.sex == "M" & Focal.individual.age == "A" ~ "Male_Adult",
    TRUE ~ NA_character_  # Default if nothing matches
  ))

#strip plot for proportion vigilance by age_sex_category
ggplot(Baboon_vigilance_age_sex, aes(x = Age_sex_Category, y = proportion_vigilant, fill = Age_sex_Category, color = Age_sex_Category)) +
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

#PREY IDENTITY FLIGHT
#create column for age and sex class in flight data
Baboon_behaviour_data <- Baboon_behaviour_data %>%
  mutate(Age_sex_Category = case_when(
    Focal.individual.sex == "J" & Focal.individual.age == "J" ~ "Juvenile",
    Focal.individual.sex == "F" & Focal.individual.age == "A" ~ "Female_Adult",
    Focal.individual.sex == "M" & Focal.individual.age == "A" ~ "Male_Adult",
    TRUE ~ NA_character_  # Default if nothing matches
  ))

View(Baboon_flight_data)
#Calculate mean latency to flight by age_sex_class
Baboon_flight_age_sex <- Baboon_flight_data %>%
  group_by(Age_sex_Category, file_name) %>%
  summarize(mean_latency_to_flee = mean(latency_to_flee_s, na.rm = TRUE), .groups = "drop") %>%  # Keep pipeline open
  filter(file_name != "2021_F07_07220010_Baboon.AVI") 

Baboon_flight_age_sex <- Baboon_flight_age_sex %>%
  filter(!is.na(Age_sex_Category)) %>%
  group_by(file_name, Age_sex_Category) %>%
  filter(file_name != "2021_F07_07220010_Baboon.AVI") 

#Strip plot for latency to flight by age sex class
ggplot(Baboon_flight_age_sex, aes(x = Age_sex_Category, y = mean_latency_to_flee, fill = Age_sex_Category, color = Age_sex_Category)) +
  geom_boxplot(alpha = 0.4, outlier.shape = NA) +  # Lighter box plot
  geom_jitter(width = 0.2, size = 1.2, alpha = 0.8) +  # Smaller points
  scale_colour_paletteer_d("nationalparkcolors::Acadia") +  # Color scheme for points
  scale_fill_paletteer_d("nationalparkcolors::Acadia") +  # Color scheme for box fill
  labs(
       x = "Baboon age and sex class",
       y = "Latency to Flee (seconds)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank(),
        legend.position = "none")  


#frequency of flight by age and sex class
flight_frequency_age_sex <- Baboon_behaviour_data %>%
  filter(!is.na(Age_sex_Category)) %>%
  group_by(file_name, Age_sex_Category, Number.of.individuals, Presence.of.offspring) %>%  
  summarise(flight_present = max(flight_present), .groups = "drop") %>%  
  group_by(Age_sex_Category) %>%
  summarise(
    flight_yes = sum(flight_present == 1),  # Count unique files where flight happened
    flight_no = sum(flight_present == 0),   # Count unique files where flight didn't happen
    n = flight_yes + flight_no,  # Total unique files per predator cue
    proportion_flight = flight_yes / n,  # Calculate proportion of flight events
    se = sqrt((proportion_flight * (1 - proportion_flight)) / n),  # Standard error
    lower_ci = proportion_flight - 1.96 * se,  # Lower bound of 95% CI
    upper_ci = proportion_flight + 1.96 * se   # Upper bound of 95% CI
  )

#bar graph for frequency of flight by age_sex_Category
ggplot(flight_frequency_age_sex, aes(x = Age_sex_Category, y = proportion_flight, fill = Age_sex_Category)) +
  geom_bar(stat = "identity", position = "dodge") +  # Bar plot
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2, color = "black") +  # Add error bars
  labs(
    title = "Proportion of Flight Responses by Baboon age and sex class",
    x = "Baboon age and sex class",
    y = "Frequency Flight"
  ) +
  scale_colour_paletteer_d("nationalparkcolors::Acadia") +  # Color scheme for points
  scale_fill_paletteer_d("nationalparkcolors::Acadia") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),  # Removes all grid lines
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for readability
  )

#grouped bar graph for frequency of flight by age_sex_category

# Reshape data to long format for ggplot
flight_frequency_long_age_sex <- flight_frequency_age_sex %>%
  select(Age_sex_Category, flight_yes, flight_no) %>%
  pivot_longer(cols = c(flight_yes, flight_no), 
               names_to = "Flight_Response", 
               values_to = "Count") 

# Create grouped bar plot
ggplot(flight_frequency_long_age_sex, aes(x = Age_sex_Category, y = Count, fill = Flight_Response)) +
  geom_col(position = "dodge") +  # Grouped bars (side-by-side)
  scale_fill_manual(
    values = c("flight_yes" = "#344E41", "flight_no" = "#A3B18A"),  # Dark Green & Light Green
    labels = c("flight_yes" = "Flight", "flight_no" = "No flight")  # Custom legend labels
  ) +
  labs(
       x = "Age and Sex Class",
       y = "Count of Flight Responses",
       fill = "Flight Response") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for readability
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

