#load packages
library(glmmTMB)
library(dplyr)
library(MuMIn)
citation("MuMIn")

#PROPORTION OF TIME SPENT VIGILANT

#Join 2021 and 2024 vigilance stats
Baboon_vigilance_stats_both <- bind_rows(Baboon_vigilance_stats, Baboon_vigilance_stats_24)

#change Wild dog name to match in both datasets
Baboon_vigilance_stats_both <- Baboon_vigilance_stats_both %>%
  mutate(predator_cue = case_when(
    predator_cue %in% c("WD", "Wild_dog") ~ "Wild dog",
    TRUE ~ predator_cue  # Keep all other values as they are
  ))

#fix issue with spacing in predator cues
Baboon_vigilance_stats_both <- Baboon_vigilance_stats_both %>%
  mutate(predator_cue = str_trim(predator_cue))
table(Baboon_vigilance_stats_both$predator_cue, Baboon_vigilance_stats_both$year)

#Transform data for beta distribution using Smithson & Verkuilen transformation
#this is needed because beta distribution requires values to be 0<x<1 but in proportion_vigilance we have exact 0s and 1s
#this transformation compresses the scale of the data, taking values away from exactly 0 and 1
Baboon_vigilance_stats_both <- Baboon_vigilance_stats_both %>%
  mutate(proportion_vigilant_beta = (proportion_vigilant * (n() - 1) + 0.5) / n())

#Ensure reference levels are consisent across all models
#set 2021 as reference level
Baboon_vigilance_stats_both <- Baboon_vigilance_stats_both %>%
  mutate(year = factor(year, levels = c(2021, 2024)))

#set control as reference level for predator cue
Baboon_vigilance_stats_both <- Baboon_vigilance_stats_both %>%
  mutate(predator_cue = relevel(factor(predator_cue), ref = "Control"))


#set female adult as reference level
Baboon_vigilance_stats_both$age_sex_class <- factor(Baboon_vigilance_stats_both$age_sex_class)
Baboon_vigilance_stats_both$age_sex_class <- relevel(Baboon_vigilance_stats_both$age_sex_class, ref = "Female_Adult_no_offspring")

#set open habitat as reference level
Baboon_vigilance_stats_both$Habitat <- factor(Baboon_vigilance_stats_both$Habitat)
Baboon_vigilance_stats_both$Habitat <- relevel(Baboon_vigilance_stats_both$Habitat, ref = "Open")

#Global GLMM using beta distribution
Vigilance_global_model_both <- glmmTMB(proportion_vigilant_beta ~ predator_cue * year + Habitat + age_sex_class + group_number + (1|site),
                                     data = Baboon_vigilance_stats_both,
                                     family = beta_family(),
                                     na.action = na.fail) 

#generate model set
Vigilance_models_both <- dredge(Vigilance_global_model_both)

# Model averaging based on AIC
Vigilance_model_avg_both <- model.avg(Vigilance_models_both)

# Get model-averaged results
summary(Vigilance_model_avg_both)
print(Vigilance_model_avg_both)


#LATENCY TO FLEE

#Join 2021 and 2024 flight stats
Baboon_flight_stats_both <- bind_rows(Baboon_flight_stats, Baboon_flight_stats_24)
View(Baboon_flight_stats_both)

#set 2021 as refrence level
Baboon_flight_stats_both <- Baboon_flight_stats_both %>%
  mutate(year = factor(year, levels = c(2021, 2024)))

#change Wild dog name to match in both datasets
Baboon_flight_stats_both <- Baboon_flight_stats_both %>%
  mutate(predator_cue = case_when(
    predator_cue %in% c("WD", "Wild_dog") ~ "Wild dog",
    TRUE ~ predator_cue  # Keep all other values as they are
  ))

#fix spacing issue in predator names
Baboon_flight_stats_both <- Baboon_flight_stats_both %>%
  mutate(predator_cue = str_trim(predator_cue))
table(Baboon_flight_stats_both$predator_cue, Baboon_flight_stats_both$year)


#change Wild dog name to match in both datasets
Baboon_flight_stats_both <- Baboon_flight_stats_both %>%
  mutate(predator_cue = case_when(
    predator_cue %in% c("WD", "Wild_dog") ~ "Wild dog",
    TRUE ~ predator_cue  # Keep all other values as they are
  ))

#set control as reference level for predator_cue
Baboon_flight_stats_both <- Baboon_flight_stats_both %>%
  mutate(predator_cue = relevel(factor(predator_cue), ref = "Control"))


#set female adult as reference level
Baboon_flight_stats_both$age_sex_class <- factor(Baboon_flight_stats_both$age_sex_class)
Baboon_flight_stats_both$age_sex_class <- relevel(Baboon_flight_stats_both$age_sex_class, ref = "Female_Adult_no_offspring")

#set open habitat as reference level
Baboon_flight_stats_both$Habitat <- factor(Baboon_flight_stats_both$Habitat)
Baboon_flight_stats_both$Habitat <- relevel(Baboon_flight_stats_both$Habitat, ref = "Open")


#Global GLMM with gaussian (normal) distribution
Latency_global_model_both <- glmmTMB(log_latency_to_flee ~ predator_cue * year + Habitat + age_sex_class + group_number + (1|site),
                                data = Baboon_flight_stats_both,
                                family = gaussian(),
                                na.action = na.fail)

#generate model set
Latency_models_both <- dredge(Latency_global_model_both)

# Model averaging based on AIC
Latency_model_avg_both <- model.avg(Latency_models_both)

# Get model-averaged results
summary(Latency_model_avg_both)
print(Latency_model_avg_both)

#FLIGHT FREQUENCY

#Join 2021 and 2024 frequency stats
Baboon_frequency_stats_both <- bind_rows(Baboon_frequency_stats, Baboon_frequency_stats_24)
View(Baboon_frequency_stats_both)

#change Wild dog name to match in both datasets
Baboon_frequency_stats_both <- Baboon_frequency_stats_both %>%
  mutate(predator_cue = case_when(
    predator_cue %in% c("WD", "Wild_dog") ~ "Wild dog",
    TRUE ~ predator_cue  # Keep all other values as they are
  ))

#fix spacing issue
Baboon_frequency_stats_both <- Baboon_frequency_stats_both %>%
  mutate(predator_cue = str_trim(predator_cue))
table(Baboon_frequency_stats_both$predator_cue, Baboon_frequency_stats_both$year)


#set control as reference level for Predator.cue
Baboon_frequency_stats_both <- Baboon_frequency_stats_both %>%
  mutate(predator_cue = relevel(factor(predator_cue), ref = "Control"))

#set 2021 as refrence level
Baboon_frequency_stats_both <- Baboon_frequency_stats_both %>%
  mutate(year = factor(year, levels = c(2021, 2024)))


#set female adult as reference level
Baboon_frequency_stats_both$age_sex_class <- factor(Baboon_frequency_stats_both$age_sex_class)
Baboon_frequency_stats_both$age_sex_class <- relevel(Baboon_frequency_stats_both$age_sex_class, ref = "Female_Adult_no_offspring")

#set open habitat as reference level
Baboon_frequency_stats_both$Habitat <- factor(Baboon_frequency_stats_both$Habitat)
Baboon_frequency_stats_both$Habitat <- relevel(Baboon_frequency_stats_both$Habitat, ref = "Open")

#Global GLMM with binomial distribution
Frequency_global_model_both <- glmmTMB(flight_present ~ predator_cue * year + Habitat + age_sex_class + group_number + (1|site),
                                     data = Baboon_frequency_stats_both,
                                     family = binomial(),
                                     na.action = na.fail)

#generate model set
Frequency_models_both <- dredge(Frequency_global_model_both)

# Model averaging based on AIC
Frequency_model_avg_both <- model.avg(Frequency_models_both)

# Get model-averaged results
summary(Frequency_model_avg_both)
print(Frequency_model_avg_both)

