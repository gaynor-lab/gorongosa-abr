#GRAPH MODEL PREDICTIONS
c("#FED789FF", "#023743FF", "#72874EFF", "#476F84FF", "#A4BED5FF", "#453947FF")

#PROPORTION VIGILANCE BY PREDATOR CUE
# Create a new dataset with combinations of explanatory variables
vigilance_pred_only <- expand.grid(
  predator_cue = unique(Baboon_vigilance_stats_both$predator_cue), # Vary predator cue
  Habitat = factor("Open", levels = levels(Baboon_vigilance_stats_both$Habitat)),  
  age_sex_class = factor("Female_Adult", levels = levels(Baboon_vigilance_stats_both$age_sex_class)),  
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
  scale_color_manual(values = c("2021" = "#72874EFF", "2024" = "#476F84FF"))

#PROPORTION VIGILANCE BY HABITAT
# Create a new dataset with combinations of explanatory variables
vigilance_habitat_only <- expand.grid(
  predator_cue = factor("Control", levels = levels(Baboon_vigilance_stats_both$predator_cue)),  # Fixed parentheses
  Habitat = unique(Baboon_vigilance_stats_both$Habitat),
  age_sex_class = factor("Female_Adult", levels = levels(Baboon_vigilance_stats_both$age_sex_class)),
  group_number = mean(Baboon_vigilance_stats_both$group_number, na.rm = TRUE),
  offspring = factor("No", levels = levels(Baboon_vigilance_stats_both$offspring)),
  year = c(2021, 2024)
)

# Get predictions on the response scale
vigilance_habitat_only$predicted <- predict(Vigilance_global_model_both, 
                                         newdata = vigilance_habitat_only, 
                                         type = "response", 
                                         re.form = NA)  # Exclude random effects

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
ggplot(vigilance_habitat_only, aes(x = Habitat, y = predicted, color = factor(year))) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbar(aes(ymin = lower, ymax = upper), 
                width = 0.2, 
                position = position_dodge(width = 0.5)) +
  labs(
    x = "Habitat",
    y = "Predicted Proportion Vigilant",
    color = "Year"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("2021" = "#72874EFF", "2024" = "#476F84FF"))


#PROPORTION VIGILANCE BY AGE AND SEX CLASS
# Create a new dataset with combinations of explanatory variables
vigilance_prey_only <- expand.grid(
  predator_cue = factor("Control", levels = levels(Baboon_vigilance_stats_both$predator_cue)),  # Fixed parentheses
  Habitat = factor("Open", levels = levels(Baboon_vigilance_stats_both$Habitat)),
  age_sex_class = unique(Baboon_vigilance_stats_both$age_sex_class),
  group_number = mean(Baboon_vigilance_stats_both$group_number, na.rm = TRUE),
  offspring = factor("No", levels = levels(Baboon_vigilance_stats_both$offspring)),
  year = c(2021, 2024)
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


#plot for predicted vigilance by habitat type
ggplot(vigilance_prey_only, aes(x = age_sex_class, y = predicted, color = factor(year))) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbar(aes(ymin = lower, ymax = upper), 
                width = 0.2, 
                position = position_dodge(width = 0.5)) +
  labs(
    x = "Age and Sex Class",
    y = "Predicted Proportion Vigilant",
    color = "Year"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("2021" = "#72874EFF", "2024" = "#476F84FF"))


















