#Behavioural analysis for 2021 data

#load packages
install.packages("paletteer")
library(paletteer)#for colour scheme
library(dplyr)
library(ggplot2)
library(stringr)
library(tidyverse)

#DATA OVERVIEW
#number of videos by camera trap
site_counts <- Baboon_behaviour_data %>%
  group_by(Camera.trap.site) %>%
  summarise(unique_files = n_distinct(file_name), .groups = "drop")

#DATAFRAME FOR BEHAVIOUR ANALYSIS
View(Final_2021)
#create column by grouping videos by file_name and then labeling them with 0 if no flight present and 1 if flight present
Baboon_behaviour_data <- Final_2021 %>%
  group_by(file_name) %>% 
  mutate(flight_present = if_else(any(str_detect(Behaviour, "Flight")), 1, 0)) %>%
  ungroup()
View(Baboon_behaviour_data)

#there is still 6 missing videos that i cannot find 
na_rows <- Baboon_behaviour_data %>%
  filter(is.na(Camera.trap.site))
View(na_rows)

#Count how many file_name have flight and how many do not
Baboon_behaviour_data %>%
  group_by(flight_present) %>%
  summarize(count = n_distinct(file_name)) %>%
  ungroup()

#create new column where Walking_V, Staring, standing and staring, Scanning, Startling = Vigilant, Flight = Flight, Occluded = Occluded, and any other behaviour is Non_vigilant
Baboon_behaviour_data <- Baboon_behaviour_data %>%
  mutate(behaviour_class = case_when(
    Behaviour %in% c("Walking_V", "Staring", "Scanning","Stand_stare","Startling") ~ "Vigilant",
    Behaviour == "Flight" ~ "Flight",
    Behaviour == "Occluded" ~ "Occluded",
    TRUE ~ "Non_vigilant"
  ))

#calculate proportion time spent vigilant 
Baboon_behaviour_data <- Baboon_behaviour_data %>%
  group_by(file_name) %>%
  mutate(
    total_frames = n(),  # Count total frames per file_name
    vigilant_frames = sum(behaviour_class == "Vigilant", na.rm = TRUE),  # Count Vigilant frames
    occluded_frames = sum(behaviour_class == "Occluded", na.rm = TRUE),  # Count Occluded frames
    proportion_vigilant = vigilant_frames / (total_frames - occluded_frames)  # Compute proportion
  ) %>%
  ungroup() 

#calculate latency to flee - NEED TO DOUBLE CHECK THIS FORMULA for order of frames 
Baboon_behaviour_data <- Baboon_behaviour_data %>%
  group_by(file_name) %>%
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

View(Baboon_behaviour_data)
#convert frames until seconds = latency by dividing by 30 bc 1s = 30 frames
Baboon_behaviour_data <- Baboon_behaviour_data %>%
  mutate(latency_to_flee_s = rows_until_flight / 30)

#DATAFRAME FOR FLIGHT ANALYSIS

#Make dataframe with just videos where flight is present
Baboon_flight_data <- Baboon_behaviour_data %>%
  filter(flight_present == 1)
View(Baboon_flight_data)

#Calculate how many frames until flight - INCORRECT
#frames are actually ordered from last to first - but not for all vids?? 
Baboon_flight_data <- Baboon_flight_data %>%
  group_by(file_name) %>%
  mutate(
    first_row = first(row_number()),  # Get the first row number of each video
    rows_until_flight = if_else(Behaviour == "Flight" & row_number() == min(which(Behaviour == "Flight")), 
                                row_number() - first_row, NA_integer_)
  ) %>%
  ungroup() %>%
  group_by(file_name) %>%
  mutate(rows_until_flight = min(rows_until_flight, na.rm = TRUE)) %>%
  ungroup()


#convert frames until seconds = latency by dividing by 30 bc 1s = 30 frames
Baboon_flight_data <- Baboon_flight_data %>%
  mutate(latency_to_flee_s = rows_until_flight / 30)


#PREDATOR IDENTITY ANALYSIS

#PREDATOR IDENTITY VIGILANCE 
#calculate mean proportion of vigilance by predator cue
Baboon_vigilance_predatorcue <- Baboon_behaviour_data %>%
  group_by(Predator.cue) %>%
  summarise(mean_proportion_vigilant = mean(proportion_vigilant, na.rm = TRUE))
View(Baboon_vigilance_predatorcue)

#calculate 95% confidence intervals 
Baboon_vigilance_predatorcue <- Baboon_behaviour_data %>%
  group_by(Predator.cue) %>%
  summarise(
    mean_proportion_vigilant = mean(proportion_vigilant, na.rm = TRUE),  # ✅ Removed extra )
    sd_proportion_vigilant = sd(proportion_vigilant, na.rm = TRUE),
    n = n_distinct(file_name)  # ✅ Count unique file names
  ) %>%
  mutate(
    se = sd_proportion_vigilant / sqrt(n),  # Standard Error
    t_value = qt(0.975, df = n - 1),  # t-critical for 95% CI
    ci_95 = t_value * se  # Confidence Interval
  )

#filter data to remove No_sound group and reorder predator cues
Baboon_vigilance_predatorcue <- Baboon_vigilance_predatorcue %>%
  filter(Predator.cue != "No_sound") %>%
  mutate(Predator.cue = factor(Predator.cue, levels = c("Leopard", "Cheetah", "Lion","Wild_dog","Hyena", "Control")))  # Adjust Cue names as needed

#Bar graph for proportion of vigilance by predator cue
ggplot(Baboon_vigilance_predatorcue, aes(x = Predator.cue, y = mean_proportion_vigilant, fill = Predator.cue)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = mean_proportion_vigilant - ci_95, ymax = mean_proportion_vigilant + ci_95), 
                width = 0.2) +
  labs(title = "Mean proportion of time spent vigilant by predator cue",
       x = "Predator Cue",
       y = "Mean proportion of vigilance (seconds)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank()) 

#group by file_name and exclude those that fled immediately
Baboon_vigilance_predatorcue2 <- Baboon_behaviour_data %>%
  filter(latency_to_flee_s != 0) %>%  # Exclude files with latency_to_flee_s = 0
  group_by(Predator.cue, file_name) %>%
  summarise(mean_proportion_vigilant = mean(proportion_vigilant, na.rm = TRUE), .groups = "drop")


#correct NaN values to 0
Baboon_vigilance_predatorcue2$mean_proportion_vigilant[is.nan(Baboon_vigilance_predatorcue2$mean_proportion_vigilant)] <- 0

#filter data to remove No_sound group and reorder predator cues
Baboon_vigilance_predatorcue2 <- Baboon_vigilance_predatorcue2 %>%
  filter(Predator.cue != "No_sound") %>%
  mutate(Predator.cue = factor(Predator.cue, levels = c("Leopard", "Cheetah", "Lion","Wild_dog","Hyena", "Control")))  # Adjust Cue names as needed
View(Baboon_vigilance_predatorcue2)
#Strip plot for proportion of vigilance by predator cue
ggplot(Baboon_vigilance_predatorcue2, aes(x = Predator.cue, y = mean_proportion_vigilant, fill = Predator.cue, color = Predator.cue)) +
  geom_boxplot(alpha = 0.4, outlier.shape = NA) +  # Lighter box plot
  geom_jitter(width = 0.2, size = 1.2, alpha = 0.8) +  # Smaller points
  scale_colour_paletteer_d("nationalparkcolors::Acadia") +  # Color scheme for points
  scale_fill_paletteer_d("nationalparkcolors::Acadia") +  # Color scheme for box fill
  labs(title = "Proportion vigilant by Predator Cue",
       x = "Predator Cue",
       y = "Proportion vigilant") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank(),
        legend.position = "none")  

#PREDATOR IDENTITY FLIGHT
#Calculate mean latency to flight by predator cue
Baboon_flight_data_predatorcue <- Baboon_flight_data %>%
  group_by(Predator.cue) %>%
  summarize(mean_latency_to_flee = mean(latency_to_flee_s, na.rm = TRUE))
View(Baboon_flight_data_predatorcue)
View(Baboon_flight_data)

#calculate 95% confidence intervals 
Baboon_flight_data_predatorcue <- Baboon_flight_data %>%
  group_by(Predator.cue) %>%
  summarise(
    mean_latency_to_flee = mean(latency_to_flee_s, na.rm = TRUE),
    sd_latency_to_flee = sd(latency_to_flee_s, na.rm = TRUE),
    n = n_distinct(file_name)
  ) %>%
  mutate(
    se = sd_latency_to_flee / sqrt(n),  # Standard Error
    t_value = qt(0.975, df = n - 1),  # t-critical for 95% CI
    ci_95 = t_value * se  # Confidence Interval
  )

#Bar graph for latency to flight by predator cue
#filter data to remove No_sound group and reorder predator cues
Baboon_flight_data_predatorcue <- Baboon_flight_data_predatorcue %>%
  filter(Predator.cue != "No_sound") %>%
  mutate(Predator.cue = factor(Predator.cue, levels = c("Leopard", "Cheetah", "Lion","WD","Hyena", "Control")))  # Adjust Cue names as needed

ggplot(Baboon_flight_data_predatorcue, aes(x = Predator.cue, y = mean_latency_to_flee, fill = Predator.cue)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = mean_latency_to_flee - ci_95, ymax = mean_latency_to_flee + ci_95), 
                width = 0.2) +
  labs(title = "Mean Latency to Flee by Predator Cue",
       x = "Predator Cue",
       y = "Mean Latency to Flight (seconds)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank()) 

#Box + strip plot for latency to flee
#filter data to remove No_sound group and reorder predator cues
Baboon_flight_data_grouped <- Baboon_flight_data %>%
  group_by(file_name, Predator.cue) %>%
  summarise(
    mean_latency_to_flee = mean(latency_to_flee_s, na.rm = TRUE),
    Camera.trap.site = first(Camera.trap.site),  # Retain Camera.trap.site
    .groups = "drop"
  ) %>%
  filter(file_name != "2021_F07_07220010_Baboon.AVI")  #excluded outlier bc it was 27 seconds

View(Baboon_flight_data_grouped)
Baboon_flight_data_grouped <- Baboon_flight_data_grouped %>%
  filter(Predator.cue != "No_sound") %>%
  mutate(Predator.cue = factor(Predator.cue, levels = c("Leopard", "Cheetah", "Lion","Wild_dog","Hyena", "Control")))  # Adjust Cue names as needed

# Create boxplot + jitter plot (strip plot)
ggplot(Baboon_flight_data_grouped, aes(x = Predator.cue, y = mean_latency_to_flee, fill = Predator.cue, color = Predator.cue)) +
  geom_boxplot(alpha = 0.4, outlier.shape = NA) +  # Lighter box plot
  geom_jitter(width = 0.2, size = 1.2, alpha = 0.8) +  # Smaller points
  scale_colour_paletteer_d("nationalparkcolors::Acadia") +  # Color scheme for points
  scale_fill_paletteer_d("nationalparkcolors::Acadia") +  # Color scheme for box fill
  labs(title = "Latency to Flee by Predator Cue",
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
  labs(title = "Flight Response by Predator Cue",
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
  filter(!is.na(Habitat)) %>%  # Exclude NA values
  group_by(Habitat, file_name, Camera.trap.site) %>%
  summarise(mean_proportion_vigilant = mean(proportion_vigilant, na.rm = TRUE), .groups = "drop")
View(Baboon_vigilance_habitat)

#strip plot for proportion vigilance by habitat type
ggplot(Baboon_vigilance_habitat, aes(x = Habitat, y = mean_proportion_vigilant, fill = Habitat, color = Habitat)) +
  geom_boxplot(alpha = 0.4, outlier.shape = NA) +  # Lighter box plot
  geom_jitter(width = 0.2, size = 1.2, alpha = 0.8) +  # Smaller points
  scale_colour_paletteer_d("nationalparkcolors::Acadia") +  # Color scheme for points
  scale_fill_paletteer_d("nationalparkcolors::Acadia") +  # Color scheme for box fill
  labs(title = "Proportion of time vigilant by habitat type",
       x = "Habitat type",
       y = "Proportion of time vigilant") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank(),
        legend.position = "none") 


#HABITAT TYPE FLIGHT
#Calculate mean latency to flight by habitat type
Baboon_flight_data_habitat <- Baboon_flight_data %>%
  filter(!is.na(Habitat)) %>% 
  group_by(Habitat, file_name) %>%
  summarize(mean_latency_to_flee = mean(latency_to_flee_s, na.rm = TRUE), .groups = "drop") %>%  # Keep pipeline open
  filter(file_name != "2021_F07_07220010_Baboon.AVI") 

#Strip plot for latency to flight by habitat type
ggplot(Baboon_flight_data_habitat, aes(x = Habitat, y = mean_latency_to_flee, fill = Habitat, color = Habitat)) +
  geom_boxplot(alpha = 0.4, outlier.shape = NA) +  # Lighter box plot
  geom_jitter(width = 0.2, size = 1.2, alpha = 0.8) +  # Smaller points
  scale_colour_paletteer_d("nationalparkcolors::Acadia") +  # Color scheme for points
  scale_fill_paletteer_d("nationalparkcolors::Acadia") +  # Color scheme for box fill
  labs(title = "Latency to Flee by habitat type",
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
  labs(title = "Flight Response by Habitat Type",
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
  filter(!is.na(Focal.individual.sex)) %>% 
  group_by(file_name, Focal.individual.sex, Focal.individual.age) %>%
  summarise(mean_proportion_vigilant = mean(proportion_vigilant, na.rm = TRUE))
View(Baboon_vigilance_age_sex)

#make new column for age_sex_category
Baboon_vigilance_age_sex <- Baboon_vigilance_age_sex %>%
  mutate(Age_sex_Category = case_when(
    Focal.individual.sex == "J" & Focal.individual.age == "J" ~ "Juvenile",
    Focal.individual.sex == "F" & Focal.individual.age == "A" ~ "Female_Adult",
    Focal.individual.sex == "M" & Focal.individual.age == "A" ~ "Male_Adult",
    TRUE ~ NA_character_  # Default if nothing matches
  ))

#strip plot for proportion vigilance by age_sex_category
ggplot(Baboon_vigilance_age_sex, aes(x = Age_sex_Category, y = mean_proportion_vigilant, fill = Age_sex_Category, color = Age_sex_Category)) +
  geom_boxplot(alpha = 0.4, outlier.shape = NA) +  # Lighter box plot
  geom_jitter(width = 0.2, size = 1.2, alpha = 0.8) +  # Smaller points
  scale_colour_paletteer_d("nationalparkcolors::Acadia") +  # Color scheme for points
  scale_fill_paletteer_d("nationalparkcolors::Acadia") +  # Color scheme for box fill
  labs(title = "Proportion of time vigilant by Baboon age and sex class",
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

View(Baboon_flight_age_sex)
#Calculate mean latency to flight by age_sex_class
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
  labs(title = "Latency to Flee by Baboon age and sex class",
       x = "Baboon age and sex class",
       y = "Latency to Flee (seconds)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank(),
        legend.position = "none")  


#frequency of flight by age and sex class
flight_frequency_age_sex <- Baboon_behaviour_data %>%
  filter(!is.na(Age_sex_Category)) %>%
  group_by(file_name, Age_sex_Category) %>%  
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

