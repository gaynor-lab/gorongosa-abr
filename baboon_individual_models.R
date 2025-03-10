#Baboon Statistical analysis

#load packages
install.packages("betareg")
install.packages("MuMIn")
library(lme4)
library(betareg)
library(MuMIn)

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
predcue_vigilance_glm <- glm(proportion_vigilant ~ Predator_category, 
                     data = Baboon_vigilance_predatorcue2, 
                     family = binomial)
summary(predcue_vigilance_glm)

#Do predator cues have a significant effect compared to each other
predcue_vigilance_lm <- lm(mean_proportion_vigilant ~ Predator.cue, data = Baboon_vigilance_predatorcue2)
summary(predcue_vigilance_lm)

Baboon_vigilance_predatorcue2$Predator.cue <- relevel(Baboon_vigilance_predatorcue2$Predator.cue, ref = "Control")
predcue_vigilance_glm <- glm(proportion_vigilant ~ Predator.cue, data = Baboon_vigilance_predatorcue2, family = binomial(link = "logit"))
summary(predcue_vigilance_glm)

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
  group_by(Predator.cue, file_name, Habitat, Age_sex_Category, Number.of.individuals) %>%  
  summarise(flight_present = max(flight_present), .groups = "drop") %>%
  mutate(flight_binary = ifelse(flight_present == 1, 1, 0)) %>%
  select(Predator.cue, file_name, flight_binary, Habitat, Age_sex_Category,Number.of.individuals) 

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
habitat_vigilance_glm <- glm(proportion_vigilant ~ Habitat, data = Baboon_vigilance_habitat, family = binomial(link = "logit"))
summary(habitat_vigilance_glm)

#relationship between habitat type and latency to flee
habitat_latency_flee_lm <- lm(mean_latency_to_flee ~ Habitat, data = Baboon_flight_data_habitat)
summary(habitat_latency_flee_lm)

#for frequency of fleeing
#glm with binary family
habitat_frequency_flee_glm <- glm(flight_binary ~ Habitat, data = flee_binary, family = binomial)
summary(habitat_frequency_flee_glm)

#PREY IDENTITY - age and sex class

#for vigilance
prey_vigilance_glm <- glm(proportion_vigilant ~ Age_sex_Category, data = Baboon_vigilance_age_sex, family = binomial(link = "logit"))
summary(prey_vigilance_glm)

#for latency to flee
prey_latency_flee_lm <- lm(mean_latency_to_flee ~ Age_sex_Category, data = Baboon_flight_age_sex)
summary(prey_latency_flee_lm)
#check assumptions- not met
par(mfrow = c(2, 2))
plot(prey_latency_flee_lm)


#for frequency of fleeing
prey_frequency_flee_glm <- glm(flight_binary ~ Age_sex_Category, data = flee_binary, family = binomial)
summary(prey_frequency_flee_glm)

#PREY IDENTITY - group size

#for vigilance
prey_vigilance_lm <- lm(proportion_vigilant ~ Number.of.individuals, data = Baboon_vigilance_age_sex)
summary(prey_vigilance_lm)
#check assumptions- not met
par(mfrow = c(2, 2))
plot(lm(proportion_vigilant ~ Number.of.individuals, data = Baboon_vigilance_age_sex))
#GLM
prey_count_glm <- glm(proportion_vigilant ~ Number.of.individuals, 
                 data = Baboon_vigilance_age_sex, 
                 family = binomial(link = "logit"))
summary(prey_count_glm)

#for latency to flee - cannot find variable
prey_latency_flee_lm <- lm(mean_latency_to_flee ~ Number.of.individuals, data = Baboon_flight_age_sex)
summary(prey_latency_flee_lm)

#for frequency of fleeing
prey_frequency_flee_glm <- glm(flight_binary ~ Number.of.individuals, data = flee_binary, family = binomial)
summary(prey_frequency_flee_glm)

#PREY IDENTITY - presence of offspring
#need to change presence of offspring either to be 0 (none) or 1 (yes) 
#for vigilance
prey_offspring_glm <- glm(proportion_vigilant ~ Presence.of.offspring, 
                      data = Baboon_vigilance_age_sex, 
                      family = binomial(link = "logit"))
summary(prey_offspring_glm)

#for latency to flee - cannot find variable
prey_latency_flee_lm <- lm(mean_latency_to_flee ~ Number.of.individuals, data = Baboon_flight_age_sex)
summary(prey_latency_flee_lm)

#for frequency of fleeing
prey_offspring_frequency_flee_glm <- glm(flight_binary ~ Presence.of.offspring, data = flee_binary, family = binomial)
summary(prey_frequency_flee_glm)


#interaction effect - proportion vigilant 
Baboon_behaviour_data_2 <- Baboon_behaviour_data %>%
  filter(Predator.cue != "No_sound") # Exclude rows where Predator.cue is "No_sound"

Baboon_behaviour_data_2$Predator.cue <- as.factor(Baboon_behaviour_data_2$Predator.cue)
Baboon_behaviour_data_2$Predator.cue <- relevel(Baboon_behaviour_data_2$Predator.cue, ref = "Control")  # Control as reference

vigilance_full_glm <- glmer(proportion_vigilant ~ Habitat + Age_sex_Category + Predator.cue, 
                          data = Baboon_behaviour_data_2, 
                          family = binomial(link = "logit"))
summary(vigilance_full_glm)
R.squaredGLMM(vigilance_full_glm)












