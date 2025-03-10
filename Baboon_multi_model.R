#Baboon multi-model

#load packages
install.packages("glmmTMB")
library(glmmTMB)
library(MuMIn)

#PROPORTION VIGILANCE MODEL
Vigilance_global_model_beta <- glmmTMB(proportion_vigilant ~ Predator.cue + Habitat + age_sex_class + Number.of.individuals + Presence_of_offspring + (1|Camera.trap.site),
                  data = Baboon_vigilance_stats,
                  family = beta_family()) #using uniform beta distribution

Vigilance_global_model_gaussian <- glmmTMB(proportion_vigilant ~ Predator.cue + Habitat + age_sex_class + Number.of.individuals + Presence_of_offspring + (1|Camera.trap.site),
                                  data = Baboon_vigilance_stats,
                                  family = gaussian(),
                                  na.action = na.fail) #using uniform beta distribution

#generate model set
Vigilance_models <- dredge(Vigilance_global_model_gaussian)

# Model averaging based on AIC
Vigilance_model_avg <- model.avg(Vigilance_models)

# Get model-averaged results
summary(Vigilance_model_avg)

#LATENCY TO FLEE MODEL

#use zero-inflated model due to right skew of data
#unsure about what family is best for latency to flee data or should I use
Latency_global_model <- glmmTMB(latency_to_flee ~ Predator.cue + Habitat + age_sex_class + Number.of.individuals + Presence_of_offspring + (1|Camera.trap.site),
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



