#Baboon Statistical analysis 

#PREDATOR IDENTITY

#Does predator cue have a significant effect compared to control

#for proportion vigilant

#create new column that categorizes predator cues as control or predator, and correct NaN to 0
Baboon_vigilance_predatorcue2 <- Baboon_vigilance_predatorcue2 %>%
  filter(Predator.cue != "No_sound") %>%  # Exclude rows where Predator.cue is "No_sound"
  mutate(Predator_category = ifelse(Predator.cue == "Control", "Control", "Predator"))

#assign control to reference level
Baboon_vigilance_predatorcue2$Predator_category <- as.factor(Baboon_vigilance_predatorcue2$Predator_category)
Baboon_vigilance_predatorcue2$Predator_category <- relevel(Baboon_vigilance_predatorcue2$Predator_category, ref = "Control")

#Binomial GLM
predcue_vigilance_glm <- glm(mean_proportion_vigilant ~ Predator_category, 
                     data = Baboon_vigilance_predatorcue2, 
                     family = binomial)
summary(predcue_vigilance_glm)

#Do predator cues have a significant effect compared to each other
predcue_vigilance_lm <- lm(mean_proportion_vigilant ~ Predator.cue, data = Baboon_vigilance_predatorcue2)
summary(predcue_vigilance_lm)

#for latency to flee

#set control as refrence level
Baboon_flight_data_grouped$Predator.cue <- relevel(Baboon_flight_data_grouped$Predator.cue, ref = "Control")

#Do predator cues have a significant effect compared to each other
predcue_latency_flee_lm <- lm(mean_latency_to_flee ~ Predator.cue, data = Baboon_flight_data_grouped)
summary(predcue_latency_flee_lm)

#for frequency of fleeing

#create binary dataframe
flee_binary <- Baboon_behaviour_data %>%
  filter(Predator.cue != "No_sound") %>%
  group_by(Predator.cue, file_name, Habitat, Age_sex_Category) %>%  
  summarise(flight_present = max(flight_present), .groups = "drop") %>%
  mutate(flight_binary = ifelse(flight_present == 1, 1, 0)) %>%
  select(Predator.cue, file_name, flight_binary, Habitat, Age_sex_Category) 

#set control as refrence level
predcue_flee_binary$Predator.cue <- as.factor(flee_binary$Predator.cue)
predcue_flee_binary$Predator.cue <- relevel(flee_binary$Predator.cue, ref = "Control")

#glm with binary family
predcue_frequency_flee_glm <- glm(flight_binary ~ Predator.cue, data = predcue_flee_binary, family = binomial)
summary(predcue_frequency_flee_glm)


#HABITAT TYPE 

#for vigilance
habitat_vigilance_lm <- lm(mean_proportion_vigilant ~ Habitat, data = Baboon_vigilance_habitat)
summary(habitat_vigilance_lm)

#relationship between habitat type and latency to flee
habitat_latency_flee_lm <- lm(mean_latency_to_flee ~ Habitat, data = Baboon_flight_data_habitat)
summary(habitat_latency_flee_lm)

#for frequency of fleeing
#glm with binary family
habitat_frequency_flee_glm <- glm(flight_binary ~ Habitat, data = flee_binary, family = binomial)
summary(habitat_frequency_flee_glm)

#PREY IDENTITY

#for vigilance
prey_vigilance_lm <- lm(mean_proportion_vigilant ~ Age_sex_Category, data = Baboon_vigilance_age_sex)
summary(prey_vigilance_lm)

#for latency to flee
prey_latency_flee_lm <- lm(mean_latency_to_flee ~ Age_sex_Category, data = Baboon_flight_age_sex)
summary(prey_latency_flee_lm)

#for frequency of fleeing
prey_frequency_flee_glm <- glm(flight_binary ~ Age_sex_Category, data = flee_binary, family = binomial)
summary(prey_frequency_flee_glm)









