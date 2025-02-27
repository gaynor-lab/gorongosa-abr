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


#create new column where Walking_V, Staring, Scanning = Vigilant, Flight = Flight, Occluded = Occluded, and any other behaviour is Non_vigilant
Baboon_behaviour_data <- Baboon_behaviour_data %>%
  mutate(behaviour_class = case_when(
    Behaviour %in% c("Walking_V", "Staring", "Scanning") ~ "Vigilant",
    Behaviour == "Flight" ~ "Flight",
    Behaviour == "Occluded" ~ "Occluded",
    TRUE ~ "Non_vigilant"
  ))


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


ggplot(Baboon_flight_data_predatorcue, aes(x = Predator.cue, y = mean_latency_to_flee, fill = Predator.cue)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Mean Latency to Flee by Predator Cue",
       x = "Predator Cue",
       y = "Mean Latency to Flight (seconds)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
