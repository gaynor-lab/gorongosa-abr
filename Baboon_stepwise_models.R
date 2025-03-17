#Baboon stepwise modelling 

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