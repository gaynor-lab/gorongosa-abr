#PLOTTING MODEL PREDICTIONS


#PROPORTION TIME VIGILANT

#BY PREDATOR CUE
#Model for proportion vigilance
Vigilance_global_model_beta <- glmmTMB(proportion_vigilant_beta ~ Predator.cue + Habitat + age_sex_class + Number.of.individuals + Presence_of_offspring + (1|Camera.trap.site),
                                       data = Baboon_vigilance_stats,
                                       family = beta_family(),
                                       na.action = na.fail) 

# Create a grid for prediction
vigilance_pred_only <- expand.grid(
  Predator.cue = unique(Baboon_vigilance_stats$Predator.cue),  # Vary Predator.cue
  Habitat = "Open",    # Hold Habitat constant (reference or typical level)
  age_sex_class = factor("Female_Adult", levels = levels(Baboon_vigilance_stats$age_sex_class)),        # Hold age_sex_class constant
  Number.of.individuals = mean(Baboon_vigilance_stats$Number.of.individuals, na.rm = TRUE),  # Average group size
  Presence_of_offspring = factor("No", levels = levels(Baboon_vigilance_stats$Presence_of_offspring))      # Hold Presence_of_offspring constant 
)

# Get predictions on the response scale
vigilance_pred_only$predicted <- predict(Vigilance_global_model_beta, 
                               newdata = vigilance_pred_only, 
                               type = "response", 
                               re.form = NA)  # Excludes random effects

# Get predictions with standard errors
pred_with_se <- predict(Vigilance_global_model_beta, 
                        newdata = pred_data, 
                        type = "link",  # Get predictions on link scale for CIs
                        se.fit = TRUE,
                        re.form = NA)

# Add confidence intervals
vigilance_pred_only$se <- pred_with_se$se.fit
vigilance_pred_only$lower <- plogis(pred_with_se$fit - 1.96 * pred_with_se$se.fit)  # Convert back to response scale
vigilance_pred_only$upper <- plogis(pred_with_se$fit + 1.96 * pred_with_se$se.fit)

# Plot predictions
ggplot(vigilance_pred_only, aes(x = Predator.cue, y = predicted)) +
  geom_point(size = 3, color = "blue") +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, color = "black") +
  labs(
    title = "Predicted Proportion of Vigilance by Predator Cue",
    x = "Predator Cue",
    y = "Predicted Proportion Vigilant"
  ) +
  theme_minimal()

#BY PREY IDENTITY
vigilance_prey_only <- expand.grid(
  age_sex_class = unique(Baboon_vigilance_stats$age_sex_class),  # Hold age_sex_class constant
  Habitat = "Open",  # Vary Habitat
  Predator.cue = factor("Control", levels = levels(Baboon_vigilance_stats$Predator.cue)),  # Hold Predator.cue constant
  Number.of.individuals = mean(Baboon_vigilance_stats$Number.of.individuals, na.rm = TRUE),  # Average group size
  Presence_of_offspring = factor("No", levels = levels(Baboon_vigilance_stats$Presence_of_offspring))  # Hold Presence_of_offspring constant
)


# Get predictions on the response scale
vigilance_prey_only$predicted <- predict(Vigilance_global_model_beta, 
                                            newdata = vigilance_prey_only, 
                                            type = "response", 
                                            re.form = NA)  # Excludes random effects

# Get predictions with standard errors
vigilance_prey_with_se <- predict(Vigilance_global_model_beta, 
                                     newdata = vigilance_prey_only, 
                                     type = "link",  # Get predictions on link scale for CIs
                                     se.fit = TRUE,
                                     re.form = NA)

# Add confidence intervals
vigilance_prey_only$se <- vigilance_prey_with_se$se.fit
vigilance_prey_only$lower <- plogis(vigilance_prey_with_se$fit - 1.96 * vigilance_prey_with_se$se.fit)  # Convert back to response scale
vigilance_prey_only$upper <- plogis(vigilance_prey_with_se$fit + 1.96 * vigilance_prey_with_se$se.fit)

# Plot predictions
ggplot(vigilance_prey_only, aes(x = age_sex_class, y = predicted)) +
  geom_point(size = 3, color = "blue") +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, color = "black") +
  labs(
    title = "Predicted Proportion of Vigilance by Prey Age and Sex Class",
    x = "Age and Sex Class",
    y = "Predicted Proportion Vigilant"
  ) +
  theme_minimal()

#BY HABITAT TYPE

# Create a grid for prediction
vigilance_habitat_only <- expand.grid(
  Habitat = unique(Baboon_vigilance_stats$Habitat),  # Vary Habitat
  Predator.cue = factor("Control", levels = levels(Baboon_vigilance_stats$Predator.cue)),  # Hold Predator.cue constant
  age_sex_class = factor("Female_Adult", levels = levels(Baboon_vigilance_stats$age_sex_class)),  # Hold age_sex_class constant
  Number.of.individuals = mean(Baboon_vigilance_stats$Number.of.individuals, na.rm = TRUE),  # Average group size
  Presence_of_offspring = factor("No", levels = levels(Baboon_vigilance_stats$Presence_of_offspring))  # Hold Presence_of_offspring constant
)


# Get predictions on the response scale
vigilance_habitat_only$predicted <- predict(Vigilance_global_model_beta, 
                                         newdata = vigilance_habitat_only, 
                                         type = "response", 
                                         re.form = NA)  # Excludes random effects

# Get predictions with standard errors
vigilance_habitat_with_se <- predict(Vigilance_global_model_beta, 
                        newdata = vigilance_habitat_only, 
                        type = "link",  # Get predictions on link scale for CIs
                        se.fit = TRUE,
                        re.form = NA)

# Add confidence intervals
vigilance_habitat_only$se <- vigilance_habitat_with_se$se.fit
vigilance_habitat_only$lower <- plogis(vigilance_habitat_with_se$fit - 1.96 * vigilance_habitat_with_se$se.fit)  # Convert back to response scale
vigilance_habitat_only$upper <- plogis(vigilance_habitat_with_se$fit + 1.96 * vigilance_habitat_with_se$se.fit)

# Plot predictions
ggplot(vigilance_habitat_only, aes(x = Habitat, y = predicted)) +
  geom_point(size = 3, color = "blue") +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, color = "black") +
  labs(
    title = "Predicted Proportion of Vigilance by Habitat Type",
    x = "Habitat",
    y = "Predicted Proportion Vigilant"
  ) +
  theme_minimal()

#LATENCY TO FLEE

#convert to factors
Baboon_flight_stats$Presence_of_offspring <- factor(Baboon_flight_stats$Presence_of_offspring,
                                                       levels = c(0, 1), 
                                                       labels = c("No", "Yes"))

Baboon_flight_stats$age_sex_class <- factor(Baboon_flight_stats$age_sex_class)

Baboon_flight_stats$Predator.cue <- factor(Baboon_flight_stats$Predator.cue)

#Model for latency to flee
Latency_global_model <- glmmTMB(log_latency_to_flee ~ Predator.cue + Habitat + age_sex_class + Number.of.individuals + Presence_of_offspring + (1|Camera.trap.site),
                                data = Baboon_flight_stats,
                                ziformula = ~1,     # Zero-inflation formula
                                family = gaussian(),
                                na.action = na.fail)
#BY PREDATOR CUE

# Create a grid for prediction
latency_pred_only <- expand.grid(
  Predator.cue = unique(Baboon_flight_stats$Predator.cue),  # Vary Predator.cue
  Habitat = "Open",    # Hold Habitat constant (reference or typical level)
  age_sex_class = factor("Female_Adult", levels = levels(Baboon_flight_stats$age_sex_class)),        # Hold age_sex_class constant
  Number.of.individuals = mean(Baboon_flight_stats$Number.of.individuals, na.rm = TRUE),  # Average group size
  Presence_of_offspring = factor("No", levels = levels(Baboon_flight_stats$Presence_of_offspring))      # Hold Presence_of_offspring constant 
)

# Get predictions on the response scale
latency_pred_only$predicted <- predict(Latency_global_model, 
                                         newdata = latency_pred_only, 
                                         type = "response", 
                                         re.form = NA)  # Excludes random effects

# Get predictions with standard errors
latency_pred_with_se <- predict(Latency_global_model, 
                        newdata = latency_pred_only, 
                        type = "link",  # Get predictions on link scale for CIs
                        se.fit = TRUE,
                        re.form = NA)

# Add predictions on the response scale
latency_pred_only$predicted <- plogis(latency_pred_with_se$fit)  # Convert predicted values to response scale

# Add confidence intervals
latency_pred_only$se <- latency_pred_with_se$se.fit
latency_pred_only$lower <- plogis(latency_pred_with_se$fit - 1.96 * latency_pred_with_se$se.fit)  # Convert back to response scale
latency_pred_only$upper <- plogis(latency_pred_with_se$fit + 1.96 * latency_pred_with_se$se.fit)

# Plot predictions
ggplot(latency_pred_only, aes(x = Predator.cue, y = predicted)) +
  geom_point(size = 3, color = "blue") +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, color = "black") +
  labs(
    title = "Predicted Latency to flee by Predator Cue",
    x = "Predator Cue",
    y = "Predicted Latency to flee"
  ) +
  theme_minimal()

#BY PREY IDENTITY

latency_prey_only <- expand.grid(
  age_sex_class = unique(Baboon_flight_stats$age_sex_class),  # Hold age_sex_class constant
  Habitat = "Open",  # Vary Habitat
  Predator.cue = factor("Control", levels = levels(Baboon_flight_stats$Predator.cue)),  # Hold Predator.cue constant
  Number.of.individuals = mean(Baboon_flight_stats$Number.of.individuals, na.rm = TRUE),  # Average group size
  Presence_of_offspring = factor("No", levels = levels(Baboon_flight_stats$Presence_of_offspring))  # Hold Presence_of_offspring constant
)


# Get predictions on the response scale
latency_prey_only$predicted <- predict(Latency_global_model, 
                                         newdata = latency_prey_only, 
                                         type = "response", 
                                         re.form = NA)  # Excludes random effects

# Get predictions with standard errors
latency_prey_with_se <- predict(Latency_global_model, 
                                newdata = latency_prey_only, 
                                type = "link",  # Get predictions on link scale
                                se.fit = TRUE,
                                re.form = NA)

# Add predictions on the response scale
latency_prey_only$predicted <- plogis(latency_prey_with_se$fit)  # Convert predicted values to response scale

# Add confidence intervals
latency_prey_only$lower <- plogis(latency_prey_with_se$fit - 1.96 * latency_prey_with_se$se.fit)  # Lower bound
latency_prey_only$upper <- plogis(latency_prey_with_se$fit + 1.96 * latency_prey_with_se$se.fit)  # Upper bound

# Plot predictions
ggplot(latency_prey_only, aes(x = age_sex_class, y = predicted)) +
  geom_point(size = 3, color = "blue") +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, color = "black") +
  labs(
    title = "Predicted Latency to flee by Prey Age and Sex Class",
    x = "Age and Sex Class",
    y = "Latency to flee"
  ) +
  theme_minimal()

#BY HABITAT TYPE

# Create a grid for prediction
latency_habitat_only <- expand.grid(
  Habitat = unique(Baboon_flight_stats$Habitat),  # Vary Habitat
  Predator.cue = factor("Control", levels = levels(Baboon_flight_stats$Predator.cue)),  # Hold Predator.cue constant
  age_sex_class = factor("Female_Adult", levels = levels(Baboon_flight_stats$age_sex_class)),  # Hold age_sex_class constant
  Number.of.individuals = mean(Baboon_flight_stats$Number.of.individuals, na.rm = TRUE),  # Average group size
  Presence_of_offspring = factor("No", levels = levels(Baboon_flight_stats$Presence_of_offspring))  # Hold Presence_of_offspring constant
)


# Get predictions on the response scale
latency_habitat_only$predicted <- predict(Latency_global_model, 
                                            newdata = latency_habitat_only, 
                                            type = "response", 
                                            re.form = NA)  # Excludes random effects

# Get predictions with standard errors
latency_habitat_with_se <- predict(Latency_global_model, 
                                     newdata = latency_habitat_only, 
                                     type = "link",  # Get predictions on link scale for CIs
                                     se.fit = TRUE,
                                     re.form = NA)

# Add predictions on the response scale
latency_habitat_only$predicted <- plogis(latency_habitat_with_se$fit)  # Convert predicted values to response scale

# Add confidence intervals
latency_habitat_only$se <- latency_habitat_with_se$se.fit
latency_habitat_only$lower <- plogis(latency_habitat_with_se$fit - 1.96 * latency_habitat_with_se$se.fit)  # Convert back to response scale
latency_habitat_only$upper <- plogis(latency_habitat_with_se$fit + 1.96 * latency_habitat_with_se$se.fit)

# Plot predictions
ggplot(latency_habitat_only, aes(x = Habitat, y = predicted)) +
  geom_point(size = 3, color = "blue") +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, color = "black") +
  labs(
    title = "Predicted Latency to flee by Habitat Type",
    x = "Habitat",
    y = "Predicted Latency to flee"
  ) +
  theme_minimal()


#FREQUENCY OF FLIGHT

#convert to factors
Baboon_frequency_stats$Presence_of_offspring <- factor(Baboon_frequency_stats$Presence_of_offspring,
                                                    levels = c(0, 1), 
                                                    labels = c("No", "Yes"))

Baboon_frequency_stats$age_sex_class <- factor(Baboon_frequency_stats$age_sex_class)

Baboon_frequency_stats$Predator.cue <- factor(Baboon_frequency_stats$Predator.cue)

#Model for frequency of flight
Frequency_global_model <- glmmTMB(flight_present ~ Predator.cue + Habitat + age_sex_class + Number.of.individuals + Presence_of_offspring + (1|Camera.trap.site),
                                  data = Baboon_frequency_stats,
                                  family = binomial(),
                                  na.action = na.fail)
#BY PREDATOR CUE

# Create a grid for prediction
frequency_pred_only <- expand.grid(
  Predator.cue = unique(Baboon_frequency_stats$Predator.cue),  # Vary Predator.cue
  Habitat = "Open",    # Hold Habitat constant (reference or typical level)
  age_sex_class = factor("Female_Adult", levels = levels(Baboon_frequency_stats$age_sex_class)),        # Hold age_sex_class constant
  Number.of.individuals = mean(Baboon_frequency_stats$Number.of.individuals, na.rm = TRUE),  # Average group size
  Presence_of_offspring = factor("No", levels = levels(Baboon_frequency_stats$Presence_of_offspring))      # Hold Presence_of_offspring constant 
)

# Get predictions on the response scale
frequency_pred_only$predicted <- predict(Frequency_global_model, 
                                       newdata = frequency_pred_only, 
                                       type = "response", 
                                       re.form = NA)  # Excludes random effects

# Get predictions with standard errors
frequency_pred_with_se <- predict(Frequency_global_model, 
                                newdata = frequency_pred_only, 
                                type = "link",  # Get predictions on link scale for CIs
                                se.fit = TRUE,
                                re.form = NA)

# Add predictions on the response scale
frequency_pred_only$predicted <- plogis(frequency_pred_with_se$fit)  # Convert predicted values to response scale

# Add confidence intervals
frequency_pred_only$se <- frequency_pred_with_se$se.fit
frequency_pred_only$lower <- plogis(frequency_pred_with_se$fit - 1.96 * frequency_pred_with_se$se.fit)  # Convert back to response scale
frequency_pred_only$upper <- plogis(frequency_pred_with_se$fit + 1.96 * frequency_pred_with_se$se.fit)

# Plot predictions
ggplot(frequency_pred_only, aes(x = Predator.cue, y = predicted)) +
  geom_point(size = 3, color = "blue") +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, color = "black") +
  labs(
    title = "Predicted Frequency of Flight by Predator Cue",
    x = "Predator Cue",
    y = "Predicted Frequency of Flight"
  ) +
  theme_minimal()

#BY PREY IDENTITY

frequency_prey_only <- expand.grid(
  age_sex_class = unique(Baboon_frequency_stats$age_sex_class),  # Hold age_sex_class constant
  Habitat = "Open",  # Vary Habitat
  Predator.cue = factor("Control", levels = levels(Baboon_frequency_stats$Predator.cue)),  # Hold Predator.cue constant
  Number.of.individuals = mean(Baboon_frequency_stats$Number.of.individuals, na.rm = TRUE),  # Average group size
  Presence_of_offspring = factor("No", levels = levels(Baboon_frequency_stats$Presence_of_offspring))  # Hold Presence_of_offspring constant
)


# Get predictions on the response scale
frequency_prey_only$predicted <- predict(Frequency_global_model, 
                                       newdata = frequency_prey_only, 
                                       type = "response", 
                                       re.form = NA)  # Excludes random effects

# Get predictions with standard errors
frequency_prey_with_se <- predict(Frequency_global_model, 
                                newdata = frequency_prey_only, 
                                type = "link",  # Get predictions on link scale
                                se.fit = TRUE,
                                re.form = NA)

# Add predictions on the response scale
frequency_prey_only$predicted <- plogis(frequency_prey_with_se$fit)  # Convert predicted values to response scale

# Add confidence intervals
frequency_prey_only$lower <- plogis(frequency_prey_with_se$fit - 1.96 * frequency_prey_with_se$se.fit)  # Lower bound
frequency_prey_only$upper <- plogis(frequency_prey_with_se$fit + 1.96 * frequency_prey_with_se$se.fit)  # Upper bound

# Plot predictions
ggplot(frequency_prey_only, aes(x = age_sex_class, y = predicted)) +
  geom_point(size = 3, color = "blue") +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, color = "black") +
  labs(
    title = "Predicted Frequency of Flight by Prey Age and Sex Class",
    x = "Age and Sex Class",
    y = "Frequency of Flight"
  ) +
  theme_minimal()

#BY HABITAT TYPE

# Create a grid for prediction
frequency_habitat_only <- expand.grid(
  Habitat = unique(Baboon_frequency_stats$Habitat),  # Vary Habitat
  Predator.cue = factor("Control", levels = levels(Baboon_frequency_stats$Predator.cue)),  # Hold Predator.cue constant
  age_sex_class = factor("Female_Adult", levels = levels(Baboon_frequency_stats$age_sex_class)),  # Hold age_sex_class constant
  Number.of.individuals = mean(Baboon_frequency_stats$Number.of.individuals, na.rm = TRUE),  # Average group size
  Presence_of_offspring = factor("No", levels = levels(Baboon_frequency_stats$Presence_of_offspring))  # Hold Presence_of_offspring constant
)


# Get predictions on the response scale
frequency_habitat_only$predicted <- predict(Latency_global_model, 
                                          newdata = frequency_habitat_only, 
                                          type = "response", 
                                          re.form = NA)  # Excludes random effects

# Get predictions with standard errors
frequency_habitat_with_se <- predict(Frequency_global_model, 
                                   newdata = frequency_habitat_only, 
                                   type = "link",  # Get predictions on link scale for CIs
                                   se.fit = TRUE,
                                   re.form = NA)

# Add predictions on the response scale
frequency_habitat_only$predicted <- plogis(frequency_habitat_with_se$fit)  # Convert predicted values to response scale

# Add confidence intervals
frequency_habitat_only$se <- frequency_habitat_with_se$se.fit
frequency_habitat_only$lower <- plogis(frequency_habitat_with_se$fit - 1.96 * frequency_habitat_with_se$se.fit)  # Convert back to response scale
frequency_habitat_only$upper <- plogis(frequency_habitat_with_se$fit + 1.96 * frequency_habitat_with_se$se.fit)

# Plot predictions
ggplot(frequency_habitat_only, aes(x = Habitat, y = predicted)) +
  geom_point(size = 3, color = "blue") +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, color = "black") +
  labs(
    title = "Predicted Frequency of Flight by Habitat Type",
    x = "Habitat",
    y = "Predicted Frequency of Flight"
  ) +
  theme_minimal()


