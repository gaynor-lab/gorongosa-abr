#GRAPH MODEL PREDICTIONS
c("#FED789FF", "#023743FF", "#72874EFF", "#476F84FF", "#A4BED5FF", "#453947FF")

#PROPORTION VIGILANCE BY PREDATOR CUE
# Create a new dataset with combinations of explanatory variables
vigilance_pred_only <- expand.grid(
  predator_cue = unique(Baboon_vigilance_stats_both$predator_cue), # Vary predator cue
  Habitat = factor("Open", levels = levels(Baboon_vigilance_stats_both$Habitat)),  
  age_sex_class = factor("Female_Adult_no_offspring", levels = levels(Baboon_vigilance_stats_both$age_sex_class)),  
  group_number = mean(Baboon_vigilance_stats_both$group_number, na.rm = TRUE),
  offspring = factor("No", levels = levels(Baboon_vigilance_stats_both$offspring)),
  year = c(2021, 2024) 
)

View(vigilance_pred_only)
View(Baboon_vigilance_stats_both)
# Get predictions on the response scale
vigilance_pred_only$predicted <- predict(Vigilance_global_model_both, 
                                         newdata = vigilance_pred_only, 
                                         type = "response", 
                                         re.form = NA)  # Exclude random effects

# Get predictions with standard errors
pred_with_se <- predict(Vigilance_global_model_both, 
                        newdata = vigilance_pred_only, 
                        type = "link",  # Get predictions on link scale for CIs
                        se.fit = TRUE,
                        re.form = NA)

# Add confidence intervals on response scale
vigilance_pred_only$se <- pred_with_se$se.fit
vigilance_pred_only$lower <- plogis(pred_with_se$fit - 1.96 * pred_with_se$se.fit)  # Back-transform to response
vigilance_pred_only$upper <- plogis(pred_with_se$fit + 1.96 * pred_with_se$se.fit)

#reorder predator cues for graphing
vigilance_pred_only <- vigilance_pred_only%>%
  mutate(predator_cue = factor(predator_cue, levels = c("Leopard", "Cheetah", "Lion","Wild dog","Hyena", "Control"))) 

#plot for predicted vigilance by predator cue
predicted_vigilance_pred_plot <-
  ggplot(vigilance_pred_only, aes(x = predator_cue, y = predicted, color = factor(year))) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbar(aes(ymin = lower, ymax = upper), 
                width = 0.2, 
                position = position_dodge(width = 0.5)) +
  labs(
    x = "Predator Cue",
    y = "Predicted Proportion Vigilant",
    color = "Year"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("2021" = "#72874EFF", "2024" = "#476F84FF")) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom" 
  )

#PROPORTION VIGILANCE BY HABITAT
# Create a new dataset with combinations of explanatory variables
vigilance_habitat_only <- expand.grid(
  predator_cue = factor("Control", levels = levels(Baboon_vigilance_stats_both$predator_cue)),  
  Habitat = unique(Baboon_vigilance_stats_both$Habitat),
  age_sex_class = factor("Female_Adult_no_offspring", levels = levels(Baboon_vigilance_stats_both$age_sex_class)),
  group_number = mean(Baboon_vigilance_stats_both$group_number, na.rm = TRUE),  
  year = factor("2021", levels = c("2021", "2024")) 
)

# Get predictions on the response scale
vigilance_habitat_only$predicted <- predict(Vigilance_global_model_both, 
                                         newdata = vigilance_habitat_only, 
                                         type = "response", 
                                         re.form = NA)  # Exclude random effects
View(vigilance_habitat_only)
# Get predictions with standard errors
habitat_with_se <- predict(Vigilance_global_model_both, 
                        newdata = vigilance_habitat_only, 
                        type = "link",  # Get predictions on link scale for CIs
                        se.fit = TRUE,
                        re.form = NA)

# Add confidence intervals on response scale
vigilance_habitat_only$se <- habitat_with_se$se.fit
vigilance_habitat_only$lower <- plogis(habitat_with_se$fit - 1.96 * habitat_with_se$se.fit)  # Back-transform to response
vigilance_habitat_only$upper <- plogis(habitat_with_se$fit + 1.96 * habitat_with_se$se.fit)


#plot for predicted vigilance by habitat type
predicted_vigilance_habitat_plot <-
  ggplot(vigilance_habitat_only, aes(x = Habitat, y = predicted)) +
  geom_point(color = "#023743FF", position = position_dodge(width = 0.5), size = 3) +  # Set single color for points
  geom_errorbar(aes(ymin = lower, ymax = upper), 
                color = "#023743FF",  # Set single color for error bars
                width = 0.2, 
                position = position_dodge(width = 0.5)) +
  labs(
    x = "Habitat",
    y = "Predicted Proportion Vigilant"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#PROPORTION VIGILANCE BY AGE AND SEX CLASS
# Create a new dataset with combinations of explanatory variables
vigilance_prey_only <- expand.grid(
  predator_cue = factor("Control", levels = levels(Baboon_vigilance_stats_both$predator_cue)),  # Fixed parentheses
  Habitat = factor("Open", levels = levels(Baboon_vigilance_stats_both$Habitat)),
  age_sex_class = unique(Baboon_vigilance_stats_both$age_sex_class),
  group_number = mean(Baboon_vigilance_stats_both$group_number, na.rm = TRUE),
  offspring = factor("No", levels = levels(Baboon_vigilance_stats_both$offspring)),
  year = factor("2021", levels = c("2021", "2024"))
)

# Get predictions on the response scale
vigilance_prey_only$predicted <- predict(Vigilance_global_model_both, 
                                            newdata = vigilance_prey_only, 
                                            type = "response", 
                                            re.form = NA)  # Exclude random effects

# Get predictions with standard errors
prey_with_se <- predict(Vigilance_global_model_both, 
                           newdata = vigilance_prey_only, 
                           type = "link",  # Get predictions on link scale for CIs
                           se.fit = TRUE,
                           re.form = NA)

# Add confidence intervals on response scale
vigilance_prey_only$se <- prey_with_se$se.fit
vigilance_prey_only$lower <- plogis(prey_with_se$fit - 1.96 * prey_with_se$se.fit)  # Back-transform to response
vigilance_prey_only$upper <- plogis(prey_with_se$fit + 1.96 * prey_with_se$se.fit)

#change age sex class names for graphing
vigilance_prey_only <- vigilance_prey_only %>%
  mutate(age_sex_class = case_when(
    age_sex_class %in% c("Female_Adult_no_offspring") ~ "Female no offspring",
    age_sex_class %in% c("Female_Adult_with_offspring") ~ "Female with offspring",
    age_sex_class %in% c("Male_Adult") ~ "Male",
    TRUE ~ age_sex_class  # Keep all other values as they are
  ))

#plot for predicted vigilance by habitat type
predicted_vigilance_prey_plot <- 
  ggplot(vigilance_prey_only, aes(x = age_sex_class, y = predicted)) +
  geom_point(position = position_dodge(width = 0.5), size = 3, color = "#023743FF") +  # Set all points to black (or another color)
  geom_errorbar(aes(ymin = lower, ymax = upper), 
                width = 0.2, 
                position = position_dodge(width = 0.5)) +
  labs(
    x = "Age and Sex Class",
    y = "Predicted Proportion Vigilant"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))








#LATENCY BY PREDATOR CUE
latency_pred_only <- expand.grid(
  predator_cue = unique(Baboon_flight_stats_both$predator_cue), # Vary predator cue
  Habitat = factor("Open", levels = levels(Baboon_flight_stats_both$Habitat)),  
  age_sex_class = factor("Female_Adult_no_offspring", levels = levels(Baboon_flight_stats_both$age_sex_class)),  
  group_number = mean(Baboon_flight_stats_both$group_number, na.rm = TRUE),
  year = c(2021, 2024) 
)

# Get predictions on the response scale
latency_pred_only$predicted <- predict(Latency_global_model_both, 
                                         newdata = latency_pred_only, 
                                         type = "response", 
                                         re.form = NA)  # Exclude random effects

# Get predictions with standard errors
latency_with_se <- predict(Latency_global_model_both, 
                        newdata = latency_pred_only, 
                        type = "link",  # Get predictions on link scale for CIs
                        se.fit = TRUE,
                        re.form = NA)

# Add confidence intervals on response scale
latency_pred_only$se <- latency_with_se$se.fit
latency_pred_only$lower <- latency_with_se$fit - 1.96 * latency_with_se$se.fit
latency_pred_only$upper <- latency_with_se$fit + 1.96 * latency_with_se$se.fit

#reorder predator cues for graphing
latency_pred_only <- latency_pred_only%>%
  mutate(predator_cue = factor(predator_cue, levels = c("Leopard", "Cheetah", "Lion","Wild dog","Hyena", "Control"))) 

#plot for latency to flee by predator cue
predicted_latency_pred_plot <-
  ggplot(latency_pred_only, aes(x = predator_cue, y = predicted, color = factor(year))) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbar(aes(ymin = lower, ymax = upper), 
                width = 0.2, 
                position = position_dodge(width = 0.5)) +
  labs(
    x = "Predator Cue",
    y = "Predicted Latency to Flee",
    color = "Year"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("2021" = "#72874EFF", "2024" = "#476F84FF")) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom" 
  )

#LATENCY TO FLEE BY HABITAT
latency_habitat_only <- expand.grid(
  predator_cue = factor("Control", levels = levels(Baboon_flight_stats_both$predator_cue)),  
  Habitat = unique(Baboon_flight_stats_both$Habitat),
  age_sex_class = factor("Female_Adult_no_offspring", levels = levels(Baboon_flight_stats_both$age_sex_class)),
  group_number = mean(Baboon_flight_stats_both$group_number, na.rm = TRUE),  
  year = factor("2021", levels = c("2021", "2024")) 
)

# Get predictions on the response scale
latency_habitat_only$predicted <- predict(Latency_global_model_both, 
                                            newdata = latency_habitat_only, 
                                            type = "response", 
                                            re.form = NA)  # Exclude random effects
# Get predictions with standard errors
habitat_with_se <- predict(Latency_global_model_both, 
                           newdata = latency_habitat_only, 
                           type = "link",  # Get predictions on link scale for CIs
                           se.fit = TRUE,
                           re.form = NA)

# Add confidence intervals on response scale
latency_habitat_only$se <- habitat_with_se$se.fit
latency_habitat_only$lower <- habitat_with_se$fit - 1.96 * habitat_with_se$se.fit 
latency_habitat_only$upper <- habitat_with_se$fit + 1.96 * habitat_with_se$se.fit


#plot for predicted vigilance by habitat type
predicted_latency_habitat_plot <-
  ggplot(latency_habitat_only, aes(x = Habitat, y = predicted)) +
  geom_point(color = "#023743FF", position = position_dodge(width = 0.5), size = 3) +  # Set single color for points
  geom_errorbar(aes(ymin = lower, ymax = upper), 
                color = "#023743FF",  # Set single color for error bars
                width = 0.2, 
                position = position_dodge(width = 0.5)) +
  labs(
    x = "Habitat",
    y = "Predicted Latency to Flee"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#LATENCY TO FLEE BY AGE AND SEX CLASS
latency_prey_only <- expand.grid(
  predator_cue = factor("Control", levels = levels(Baboon_flight_stats_both$predator_cue)),  
  Habitat = factor("Open", levels = levels(Baboon_flight_stats_both$Habitat)),
  age_sex_class = unique(Baboon_flight_stats_both$age_sex_class),
  group_number = mean(Baboon_flight_stats_both$group_number, na.rm = TRUE),
  year = factor("2021", levels = c("2021", "2024"))
)

# Get predictions on the response scale
latency_prey_only$predicted <- predict(Latency_global_model_both, 
                                         newdata = latency_prey_only, 
                                         type = "response", 
                                         re.form = NA)  # Exclude random effects

# Get predictions with standard errors
prey_with_se <- predict(Latency_global_model_both, 
                        newdata = latency_prey_only, 
                        type = "link",  # Get predictions on link scale for CIs
                        se.fit = TRUE,
                        re.form = NA)

# Add confidence intervals on response scale
latency_prey_only$se <- prey_with_se$se.fit
latency_prey_only$lower <- prey_with_se$fit - 1.96 * prey_with_se$se.fit
latency_prey_only$upper <- prey_with_se$fit + 1.96 * prey_with_se$se.fit


#change age sex class names for graphing
latency_prey_only <- latency_prey_only %>%
  mutate(age_sex_class = case_when(
    age_sex_class %in% c("Female_Adult_no_offspring") ~ "Female no offspring",
    age_sex_class %in% c("Female_Adult_with_offspring") ~ "Female with offspring",
    age_sex_class %in% c("Male_Adult") ~ "Male",
    TRUE ~ age_sex_class  # Keep all other values as they are
  ))

#plot for latency to flee by age and sex class
predicted_latency_prey_plot <- 
  ggplot(latency_prey_only, aes(x = age_sex_class, y = predicted)) +
  geom_point(position = position_dodge(width = 0.5), size = 3, color = "#023743FF") +  # Set all points to black (or another color)
  geom_errorbar(aes(ymin = lower, ymax = upper), 
                width = 0.2, 
                position = position_dodge(width = 0.5)) +
  labs(
    x = "Age and Sex Class",
    y = "Predicted Latency to Flee"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))







#FREQUENCY BY PREDATOR CUE
frequency_pred_only <- expand.grid(
  predator_cue = unique(Baboon_frequency_stats_both$predator_cue), # Vary predator cue
  Habitat = factor("Open", levels = levels(Baboon_frequency_stats_both$Habitat)),  
  age_sex_class = factor("Female_Adult_no_offspring", levels = levels(Baboon_frequency_stats_both$age_sex_class)),  
  group_number = mean(Baboon_frequency_stats_both$group_number, na.rm = TRUE),
  year = c(2021, 2024) 
)

# Get predictions on the response scale
frequency_pred_only$predicted <- predict(Frequency_global_model_both, 
                                       newdata = frequency_pred_only, 
                                       type = "response", 
                                       re.form = NA)  # Exclude random effects

# Get predictions with standard errors
frequency_with_se <- predict(Frequency_global_model_both, 
                           newdata = frequency_pred_only, 
                           type = "link",  # Get predictions on link scale for CIs
                           se.fit = TRUE,
                           re.form = NA)

# Add confidence intervals on response scale
frequency_pred_only$se <- frequency_with_se$se.fit
frequency_pred_only$lower <- frequency_with_se$fit - 1.96 * frequency_with_se$se.fit
frequency_pred_only$upper <- frequency_with_se$fit + 1.96 * frequency_with_se$se.fit

#convert onto same scale
frequency_pred_only$lower_resp <- plogis(frequency_pred_only$lower)
frequency_pred_only$upper_resp <- plogis(frequency_pred_only$upper)

#reorder predator cues for graphing
frequency_pred_only <- frequency_pred_only%>%
  mutate(predator_cue = factor(predator_cue, levels = c("Leopard", "Cheetah", "Lion","Wild dog","Hyena", "Control"))) 

#plot frequency of flight by predator cue
predicted_frequency_pred_plot <-
  ggplot(frequency_pred_only, aes(x = predator_cue, y = predicted, color = factor(year))) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbar(aes(ymin = lower_resp, ymax = upper_resp), 
                width = 0.2, 
                position = position_dodge(width = 0.5)) +
  scale_color_manual(values = c("2021" = "#72874E", "2024" = "#476F84")) +
  labs(x = "Predator Cue", y = "Predicted Frequency of Flight", color = "Year") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#FLIGHT FREQUENCY BY HABITAT
frequency_habitat_only <- expand.grid(
  predator_cue = factor("Control", levels = levels(Baboon_frequency_stats_both$predator_cue)),  
  Habitat = unique(Baboon_frequency_stats_both$Habitat),
  age_sex_class = factor("Female_Adult_no_offspring", levels = levels(Baboon_frequency_stats_both$age_sex_class)),
  group_number = mean(Baboon_frequency_stats_both$group_number, na.rm = TRUE),  
  year = factor("2021", levels = c("2021", "2024")) 
)

# Get predictions on the response scale
frequency_habitat_only$predicted <- predict(Frequency_global_model_both, 
                                          newdata = frequency_habitat_only, 
                                          type = "response", 
                                          re.form = NA)  # Exclude random effects
# Get predictions with standard errors
habitat_with_se <- predict(Frequency_global_model_both, 
                           newdata = frequency_habitat_only, 
                           type = "link",  # Get predictions on link scale for CIs
                           se.fit = TRUE,
                           re.form = NA)

# Add confidence intervals on response scale
frequency_habitat_only$se <- habitat_with_se$se.fit
frequency_habitat_only$lower <- habitat_with_se$fit - 1.96 * habitat_with_se$se.fit 
frequency_habitat_only$upper <- habitat_with_se$fit + 1.96 * habitat_with_se$se.fit

#convert onto same scale
frequency_habitat_only$lower_resp <- plogis(frequency_habitat_only$lower)
frequency_habitat_only$upper_resp <- plogis(frequency_habitat_only$upper)


#plot for predicted frequency of flight by habitat type
predicted_frequency_habitat_plot <-
  ggplot(frequency_habitat_only, aes(x = Habitat, y = predicted)) +
  geom_point(color = "#023743FF", position = position_dodge(width = 0.5), size = 3) +  # Set single color for points
  geom_errorbar(aes(ymin = lower_resp, ymax = upper_resp), 
                color = "#023743FF",  # Set single color for error bars
                width = 0.2, 
                position = position_dodge(width = 0.5)) +
  labs(
    x = "Habitat",
    y = "Predicted Frequency of Flight"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#FREQUENCY OF FLIGHT BY AGE AND SEX CLASS
frequency_prey_only <- expand.grid(
  predator_cue = factor("Control", levels = levels(Baboon_frequency_stats_both$predator_cue)),  
  Habitat = factor("Open", levels = levels(Baboon_frequency_stats_both$Habitat)),
  age_sex_class = unique(Baboon_frequency_stats_both$age_sex_class),
  group_number = mean(Baboon_frequency_stats_both$group_number, na.rm = TRUE),
  year = factor("2021", levels = c("2021", "2024"))
)

# Get predictions on the response scale
frequency_prey_only$predicted <- predict(Frequency_global_model_both, 
                                       newdata = frequency_prey_only, 
                                       type = "response", 
                                       re.form = NA)  # Exclude random effects

# Get predictions with standard errors
prey_with_se <- predict(Frequency_global_model_both, 
                        newdata = frequency_prey_only, 
                        type = "link",  # Get predictions on link scale for CIs
                        se.fit = TRUE,
                        re.form = NA)

# Add confidence intervals on response scale
frequency_prey_only$se <- prey_with_se$se.fit
frequency_prey_only$lower <- prey_with_se$fit - 1.96 * prey_with_se$se.fit
frequency_prey_only$upper <- prey_with_se$fit + 1.96 * prey_with_se$se.fit

#convert onto same scale
frequency_prey_only$lower_resp <- plogis(frequency_prey_only$lower)
frequency_prey_only$upper_resp <- plogis(frequency_prey_only$upper)


#change age sex class names for graphing
frequency_prey_only <- frequency_prey_only %>%
  mutate(age_sex_class = case_when(
    age_sex_class %in% c("Female_Adult_no_offspring") ~ "Female no offspring",
    age_sex_class %in% c("Female_Adult_with_offspring") ~ "Female with offspring",
    age_sex_class %in% c("Male_Adult") ~ "Male",
    TRUE ~ age_sex_class  # Keep all other values as they are
  ))

#plot for latency to flee by age and sex class
predicted_frequency_prey_plot <- 
  ggplot(frequency_prey_only, aes(x = age_sex_class, y = predicted)) +
  geom_point(position = position_dodge(width = 0.5), size = 3, color = "#023743FF") +  # Set all points to black (or another color)
  geom_errorbar(aes(ymin = lower_resp, ymax = upper_resp), 
                width = 0.2, 
                position = position_dodge(width = 0.5)) +
  labs(
    x = "Age and Sex Class",
    y = "Predicted Frequency of Flight"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


















