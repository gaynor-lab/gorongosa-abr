#Baboon multi-model for 2021

#load packages
library(glmmTMB)
library(dplyr)
library(MuMIn)

###FOR 2021 DATA

#PROPORTION VIGILANCE MODEL

#Transform data for beta distribution using Smithson & Verkuilen transformation
#this is needed because beta distribution requires values to be 0<x<1 but in proportion_vigilance we have exact 0s and 1s
#this transformation compresses the scale of the data, taking values away from exactly 0 and 1
Baboon_vigilance_stats <- Baboon_vigilance_stats %>%
  mutate(proportion_vigilant_beta = (proportion_vigilant * (n() - 1) + 0.5) / n())
View(Baboon_vigilance_stats)

#set control as reference level for Predator.cue
Baboon_vigilance_stats <- Baboon_vigilance_stats %>%
  mutate(Predator.cue = relevel(factor(Predator.cue), ref = "Control"))


#Global GLMM using beta distribution
Vigilance_global_model_beta <- glmmTMB(proportion_vigilant_beta ~ Predator.cue + Habitat + age_sex_class + Number.of.individuals + Presence_of_offspring + (1|Camera.trap.site),
                  data = Baboon_vigilance_stats,
                  family = beta_family(),
                  na.action = na.fail) 

#generate model set
Vigilance_models <- dredge(Vigilance_global_model_beta)

# Model averaging based on AIC
Vigilance_model_avg <- model.avg(Vigilance_models)

# Get model-averaged results
summary(Vigilance_model_avg)

# LATENCY TO FLEE MODEL

#set control as reference level for Predator.cue
Baboon_flight_stats <- Baboon_flight_stats %>%
  mutate(Predator.cue = relevel(factor(Predator.cue), ref = "Control"))

#Global GLMM with gaussian (normal) distribution
#use zero-inflated with log transformation to address right skew of the data
Latency_global_model <- glmmTMB(log_latency_to_flee ~ Predator.cue + Habitat + age_sex_class + Number.of.individuals + Presence_of_offspring + (1|Camera.trap.site),
                  data = Baboon_flight_stats,
                  ziformula = ~1,     # Zero-inflation formula
                  family = gaussian(),
                  na.action = na.fail)
                  
#generate model set
Latency_models <- dredge(Latency_global_model)

# Model averaging based on AIC
Latency_model_avg <- model.avg(Latency_models)

# Get model-averaged results
summary(Latency_model_avg)

#FLIGHT FREQUENCY MODEL

#set control as reference level for Predator.cue
Baboon_frequency_stats <- Baboon_frequency_stats %>%
  mutate(Predator.cue = relevel(factor(Predator.cue), ref = "Control"))

#Global GLMM with binomial distribution
Frequency_global_model <- glmmTMB(flight_present ~ Predator.cue + Habitat + age_sex_class + Number.of.individuals + Presence_of_offspring + (1|Camera.trap.site),
                                data = Baboon_frequency_stats,
                                family = binomial(),
                                na.action = na.fail)

#generate model set
Frequency_models <- dredge(Frequency_global_model)

# Model averaging based on AIC
Frequency_model_avg <- model.avg(Frequency_models)

# Get model-averaged results
summary(Frequency_model_avg)

###FOR 2024 DATA

#PROPORTION VIGILANCE MODEL

#Transform data for beta distribution using Smithson & Verkuilen transformation
#this is needed because beta distribution requires values to be 0<x<1 but in proportion_vigilance we have exact 0s and 1s
#this transformation compresses the scale of the data, taking values away from exactly 0 and 1
Baboon_vigilance_stats_24 <- Baboon_vigilance_stats_24 %>%
  mutate(proportion_vigilant_beta = (proportion_vigilant * (n() - 1) + 0.5) / n())
View(Baboon_vigilance_stats)

#set control as reference level for Predator.cue
Baboon_vigilance_stats_24 <- Baboon_vigilance_stats_24 %>%
  mutate(Predator.cue = relevel(factor(predator_cue), ref = "Control"))


#set no offspring as reference level
Baboon_vigilance_stats_24$offspring <- factor(Baboon_vigilance_stats_24$offspring,
                                                        levels = c(0, 1), 
                                                        labels = c("No", "Yes"))

Baboon_vigilance_stats_24$offspring <- relevel(Baboon_vigilance_stats_24$offspring, ref = "No")


#set female adult as reference level
Baboon_vigilance_stats_24$age_sex_class <- factor(Baboon_vigilance_stats_24$age_sex_class)
Baboon_vigilance_stats_24$age_sex_class <- relevel(Baboon_vigilance_stats_24$age_sex_class, ref = "Female_Adult")

#set open habitat as reference level
Baboon_vigilance_stats_24$Habitat <- factor(Baboon_vigilance_stats_24$Habitat)
Baboon_vigilance_stats_24$Habitat <- relevel(Baboon_vigilance_stats_24$Habitat, ref = "Open")

#Global GLMM using beta distribution
Vigilance_global_model_24 <- glmmTMB(proportion_vigilant_beta ~ predator_cue + Habitat + age_sex_class + (1|site),
                                       data = Baboon_vigilance_stats_24,
                                       family = beta_family(),
                                       na.action = na.fail) 

#generate model set
Vigilance_models_24 <- dredge(Vigilance_global_model_24)

# Model averaging based on AIC
Vigilance_model_avg_24 <- model.avg(Vigilance_models_24)

# Get model-averaged results
summary(Vigilance_model_avg_24)

# LATENCY TO FLEE MODEL

#set control as reference level for Predator.cue
Baboon_flight_stats_24 <- Baboon_flight_stats_24 %>%
  mutate(predator_cue = relevel(factor(predator_cue), ref = "Control"))

#set no offspring as reference level
Baboon_flight_stats_24$offspring <- factor(Baboon_flight_stats_24$offspring,
                                              levels = c(0, 1), 
                                              labels = c("No", "Yes"))

Baboon_flight_stats_24$offspring <- relevel(Baboon_flight_stats_24$offspring, ref = "No")


#set female adult as reference level
Baboon_flight_stats_24$age_sex_class <- factor(Baboon_flight_stats_24$age_sex_class)
Baboon_flight_stats_24$age_sex_class <- relevel(Baboon_flight_stats_24$age_sex_class, ref = "Female_Adult")

#set open habitat as reference level
Baboon_flight_stats_24$Habitat <- factor(Baboon_flight_stats_24$Habitat)
Baboon_flight_stats_24$Habitat <- relevel(Baboon_flight_stats_24$Habitat, ref = "Open")

#Global GLMM with gaussian (normal) distribution
#use zero-inflated with log transformation to address right skew of the data
Latency_global_model_24 <- glmmTMB(log_latency_to_flee ~ predator_cue + Habitat + age_sex_class + (1|site),
                                data = Baboon_flight_stats_24,
                                ziformula = ~1,     # Zero-inflation formula
                                family = gaussian(),
                                na.action = na.fail)

#generate model set
Latency_models_24 <- dredge(Latency_global_model_24)

# Model averaging based on AIC
Latency_model_avg_24 <- model.avg(Latency_models_24)

# Get model-averaged results
summary(Latency_model_avg_24)

#FLIGHT FREQUENCY MODEL

#set control as reference level for Predator.cue
Baboon_frequency_stats_24 <- Baboon_frequency_stats_24 %>%
  mutate(predator_cue = relevel(factor(predator_cue), ref = "Control"))

#set no offspring as reference level
Baboon_frequency_stats_24$offspring <- factor(Baboon_frequency_stats_24$offspring,
                                           levels = c(0, 1), 
                                           labels = c("No", "Yes"))

Baboon_frequency_stats_24$offspring <- relevel(Baboon_frequency_stats_24$offspring, ref = "No")


#set female adult as reference level
Baboon_frequency_stats_24$age_sex_class <- factor(Baboon_frequency_stats_24$age_sex_class)
Baboon_frequency_stats_24$age_sex_class <- relevel(Baboon_frequency_stats_24$age_sex_class, ref = "Female_Adult")

#set open habitat as reference level
Baboon_frequency_stats_24$Habitat <- factor(Baboon_frequency_stats_24$Habitat)
Baboon_frequency_stats_24$Habitat <- relevel(Baboon_frequency_stats_24$Habitat, ref = "Open")

#Global GLMM with binomial distribution
Frequency_global_model_24 <- glmmTMB(flight_present ~ predator_cue + Habitat + age_sex_class + (1|site),
                                  data = Baboon_frequency_stats_24,
                                  family = binomial(),
                                  na.action = na.fail)

#generate model set
Frequency_models_24 <- dredge(Frequency_global_model_24)

# Model averaging based on AIC
Frequency_model_avg_24 <- model.avg(Frequency_models_24)

# Get model-averaged results
summary(Frequency_model_avg_24)



