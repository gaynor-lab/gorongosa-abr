#Baboon multi-model

#load packages
install.packages("glmmTMB")
library(glmmTMB)
library(dplyr)
library(MuMIn)

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

#Global GLMM using gaussian distribution
Vigilance_global_model_gaussian <- glmmTMB(proportion_vigilant ~ Predator.cue + Habitat + age_sex_class + Number.of.individuals + Presence_of_offspring + (1|Camera.trap.site),
                                  data = Baboon_vigilance_stats,
                                  family = gaussian(),
                                  na.action = na.fail) 

#generate model set
Vigilance_models <- dredge(Vigilance_global_model_beta)
View(Vigilance_models)
# Model averaging based on AIC
Vigilance_model_avg <- model.avg(Vigilance_models)

# Get model-averaged results
summary(Vigilance_model_avg)

#LATENCY TO FLEE MODEL

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



