#Behavioural analysis 

#load packages
library(dplyr)

#FOR VIGILANCE ANALYSIS
#create column by grouping videos by file_name and then labeling them with 0 if no flight present,
#and 1 if flight present
Baboon_behaviour_data <- Final_2021 %>%
  group_by(file_name) %>% 
  mutate(flight_present = if_else(any(str_detect(Behaviour, "Flight")), 1, 0)) %>%
  ungroup()
View(Baboon_behaviour_data)

#Count how many file_name have flight and how many do not
Baboon_behaviour_data %>%
  group_by(flight_present) %>%
  summarize(count = n_distinct(file_name)) %>%
  ungroup()


#create new column where Walking_V, Staring, standing and staring, Scanning = Vigilant, Flight = Flight, Occluded = Occluded, and any other behaviour is Non_vigilant
Baboon_behaviour_data <- Baboon_behaviour_data %>%
  mutate(behaviour_class = case_when(
    Behaviour %in% c("Walking_V", "Staring", "Scanning","Stand_stare") ~ "Vigilant",
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

#reorganize data structure
#filter data to remove No_sound group and reorder predator cues
Baboon_vigilance_predatorcue <- Baboon_vigilance_predatorcue %>%
  filter(Predator.cue != "No_sound") %>%
  mutate(Predator.cue = factor(Predator.cue, levels = c("Leopard", "Cheetah", "Lion","Wild_dog","Hyena", "Control")))  # Adjust Cue names as needed

#Create bar graph for proportion of vigilance by predatorcue
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

#FOR FLIGHT ANALYSIS
#Make dataframe with just videos where flight is present
Baboon_flight_data <- Baboon_behaviour_data %>%
  filter(flight_present == 1)
View(Baboon_flight_data)

#Calculate how many frames until flight
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


#BAR GRAPH FOR FLIGHT BY PREDATOR CUE
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


#Make frame for frequency of flight
flight_frequency <- Baboon_behaviour_data %>%
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


#plot it

#filter data to remove No_sound group and reorder predator cues
flight_frequency <- flight_frequency %>%
  filter(Predator.cue != "No_sound") %>%
  mutate(Predator.cue = factor(Predator.cue, levels = c("Leopard", "Cheetah", "Lion","Wild_dog","Hyena", "Control")))  # Adjust Cue names as needed

ggplot(flight_frequency, aes(x = Predator.cue, y = proportion_flight, fill = Predator.cue)) +
  geom_bar(stat = "identity", position = "dodge") +  # Bar plot
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2, color = "black") +  # Add error bars
  labs(
    title = "Proportion of Flight Responses by Predator Cue",
    x = "Predator Cue",
    y = "Frequency Flight"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability




