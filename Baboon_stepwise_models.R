#Baboon stepwise modelling for interaction effects

#PROPORTION OF TIME SPENT VIGILANT
Vigilance_full_model <- glmmTMB(proportion_vigilant_beta ~ Predator.cue + Habitat + age_sex_class + Number.of.individuals + Presence_of_offspring + (1|Camera.trap.site),
                                       data = Baboon_vigilance_stats,
                                       family = beta_family(),
                                       na.action = na.fail) 
# Stepwise model selection based on AIC
Vigilance_best_model <- step(Vigilance_full_model, direction = "backward")
summary(Vigilance_best_model)

#Test for interaction effects
Vigilance_interaction_model <- glmmTMB(proportion_vigilant_beta ~ Predator.cue * Habitat + 
                                         Number.of.individuals * Habitat + 
                                         (1|Camera.trap.site),
                                       data = Baboon_vigilance_stats, 
                                       family = beta_family())

summary(Vigilance_interaction_model)
AIC(Vigilance_best_model, Vigilance_interaction_model)
#interactions are important

#LATENCY TO FLEE
latency_full_model <- glmmTMB(log_latency_to_flee ~ Predator.cue + Habitat + age_sex_class + Number.of.individuals + Presence_of_offspring + (1|Camera.trap.site),
                              data = Baboon_flight_stats,
                              ziformula = ~1,     # Zero-inflation formula
                              family = gaussian(),
                              na.action = na.fail)

# Stepwise model selection based on AIC
latency_best_model <- step(latency_full_model, direction = "backward")
summary(latency_best_model)

#Test for interaction effects
latency_interaction_model <- glmmTMB(log_latency_to_flee ~ Predator.cue * Habitat + 
                                         Number.of.individuals * Habitat + 
                                         (1|Camera.trap.site),
                                       data = Baboon_flight_stats, 
                                       family = gaussian())

summary(latency_interaction_model)
AIC(latency_best_model, latency_interaction_model)
#interactions are important - AIC for interaction model is lower

#FREQUENCY OF FLIGHT
frequency_full_model <- glmmTMB(flight_present ~ Predator.cue + Habitat + age_sex_class + Number.of.individuals + Presence_of_offspring + (1|Camera.trap.site),
                                data = Baboon_frequency_stats,
                                family = binomial(),
                                na.action = na.fail)

# Stepwise model selection based on AIC
frequency_best_model <- step(frequency_full_model, direction = "backward")
summary(frequency_best_model)

#Test for interaction effects
frequency_interaction_model <- glmmTMB(flight_present ~ Predator.cue * Habitat + 
                                         Number.of.individuals * Habitat + 
                                         (1|Camera.trap.site),
                                       data = Baboon_frequency_stats, 
                                       family = binomial())

summary(frequency_interaction_model)
AIC(frequency_best_model, frequency_interaction_model)
#interactions are important - AIC for interaction model is lower
