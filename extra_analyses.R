#Random analyses


#T-test to test pre and post volume change in 2024

#T-test dataframes
ttest_vigilance <- Baboon_vigilance_data_24 %>%
  group_by(file_name) %>%
  slice(1) %>%
  select(file_name, proportion_vigilant, volume_change) %>%
  ungroup()

ttest_latency <- Baboon_flight_data_24 %>%
  group_by(file_name) %>%
  slice(1) %>%
  select(file_name, latency_to_flee_s, volume_change) %>%
  ungroup()

ttest_frequency <- Baboon_frequency_data_24 %>%
  group_by(file_name) %>%
  slice(1) %>%
  select(file_name, flight_present, volume_change) %>%
  ungroup()

#check normality for vigilance
shapiro.test(ttest_vigilance$proportion_vigilant[ttest_vigilance$volume_change == "pre"])
#non normally distributed
shapiro.test(ttest_vigilance$proportion_vigilant[ttest_vigilance$volume_change == "post"])
#not normally distributeed

#check normality for latency
shapiro.test(ttest_latency$latency_to_flee_s[ttest_latency$volume_change == "pre"])
#non normally distributed
shapiro.test(ttest_vigilance$proportion_vigilant[ttest_vigilance$volume_change == "post"])
#not normally distributeed

#does not fit assumptions for t-test so Mann-whitney U test
wilcox.test(proportion_vigilant ~ volume_change, data = ttest_vigilance)
#p=0.2672, no statistically significant difference :)

wilcox.test(latency_to_flee_s ~ volume_change, data = ttest_latency)
#p=0.1478, no statistically significant difference

wilcox.test(flight_present ~ volume_change, data = ttest_frequency)
#p=0.4112, no statistically significant difference

#Test differences in variances in 2021 vs 
library(car)
leveneTest(proportion_vigilant ~ year, data = Baboon_vigilance_stats_both)
leveneTest(latency_to_flee ~ year, data = Baboon_flight_stats_both)
leveneTest(flight_present ~ year, data = Baboon_frequency_stats_both)

var_2021 <- var(Baboon_frequency_stats_both$flight_present[Baboon_frequency_stats_both$year == 2021])
var_2024 <- var(Baboon_frequency_stats_both$flight_present[Baboon_frequency_stats_both$year == 2024])
var_2021
var_2024

#test habituation effect 2021

#vigilance dataframe
habituation_vigilance <- Baboon_vigilance_stats %>%
  select(file_name, proportion_vigilant) %>%
  mutate(month = sub(".*?_(\\d{2}).*", "\\1", file_name)) %>%
  mutate(day = sub(".*?_(\\d{2})(\\d{2}).*", "\\2", file_name)) %>%
  mutate(
    month = as.numeric(month),
    day = as.numeric(day)
  ) %>%
  mutate(week = case_when(
    month == 06 & day >= 1 & day <= 7 ~ 1,
    month == 06 & day >= 8 & day <= 14 ~ 2,
    month == 06 & day >= 15 & day <= 21 ~ 3,
    month == 06 & day >= 22 & day <= 30 ~ 4,
    month == 07 & day >= 1 & day <= 7 ~ 5,
    month == 07 & day >= 8 & day <= 14 ~ 6,
    month == 07 & day >= 15 & day <= 21 ~ 7,
    month == 07 & day >= 22 & day <= 31 ~ 8,
    month == 08 & day >= 1 & day <= 7 ~ 9,
    month == 08 & day >= 8 & day <= 14 ~ 10,
    month == 08 & day >= 15 & day <= 21 ~ 11,
    month == 08 & day >= 22 & day <= 30 ~ 12,
    TRUE ~ NA_real_
  )) %>%
  mutate(week = factor(week))

# Run ANOVA to test for significant effect between 'proportion_vigilant' and 'week'
anova_hab_vig_2021 <- aov(proportion_vigilant ~ week, data = habituation_vigilance)
summary(anova_hab_vig_2021)
#p = 0.00934

# Perform Tukey's HSD test
tukey_result <- TukeyHSD(anova_hab_vig_2021)
summary(tukey_result)
tukey_result$week

#latency dataframe
habituation_latency_21 <- Baboon_flight_stats %>%
  select(file_name, latency_to_flee) %>%
  mutate(month = sub(".*?_(\\d{2}).*", "\\1", file_name)) %>%
  mutate(day = sub(".*?_(\\d{2})(\\d{2}).*", "\\2", file_name)) %>%
  mutate(
    month = as.numeric(month),
    day = as.numeric(day)
  ) %>%
  mutate(week = case_when(
    month == 06 & day >= 1 & day <= 7 ~ 1,
    month == 06 & day >= 8 & day <= 14 ~ 2,
    month == 06 & day >= 15 & day <= 21 ~ 3,
    month == 06 & day >= 22 & day <= 30 ~ 4,
    month == 07 & day >= 1 & day <= 7 ~ 5,
    month == 07 & day >= 8 & day <= 14 ~ 6,
    month == 07 & day >= 15 & day <= 21 ~ 7,
    month == 07 & day >= 22 & day <= 31 ~ 8,
    month == 08 & day >= 1 & day <= 7 ~ 9,
    month == 08 & day >= 8 & day <= 14 ~ 10,
    month == 08 & day >= 15 & day <= 21 ~ 11,
    month == 08 & day >= 22 & day <= 30 ~ 12,
    TRUE ~ NA_real_
  )) %>%
  mutate(week = factor(week))

# Run ANOVA to test for significant effect between 'proportion_vigilant' and 'week'
anova_hab_lat_2021 <- aov(latency_to_flee ~ week, data = habituation_latency_21)
summary(anova_hab_lat_2021)
#not significant

#frequency dataframe
habituation_frequency_21 <- Baboon_frequency_stats %>%
  select(file_name, flight_present) %>%
  mutate(month = sub(".*?_(\\d{2}).*", "\\1", file_name)) %>%
  mutate(day = sub(".*?_(\\d{2})(\\d{2}).*", "\\2", file_name)) %>%
  mutate(
    month = as.numeric(month),
    day = as.numeric(day)
  ) %>%
  mutate(week = case_when(
    month == 06 & day >= 1 & day <= 7 ~ 1,
    month == 06 & day >= 8 & day <= 14 ~ 2,
    month == 06 & day >= 15 & day <= 21 ~ 3,
    month == 06 & day >= 22 & day <= 30 ~ 4,
    month == 07 & day >= 1 & day <= 7 ~ 5,
    month == 07 & day >= 8 & day <= 14 ~ 6,
    month == 07 & day >= 15 & day <= 21 ~ 7,
    month == 07 & day >= 22 & day <= 31 ~ 8,
    month == 08 & day >= 1 & day <= 7 ~ 9,
    month == 08 & day >= 8 & day <= 14 ~ 10,
    month == 08 & day >= 15 & day <= 21 ~ 11,
    month == 08 & day >= 22 & day <= 30 ~ 12,
    TRUE ~ NA_real_
  )) %>%
  mutate(week = factor(week))

# Run ANOVA to test for significant effect between 'proportion_vigilant' and 'week'
anova_hab_freq_2021 <- aov(flight_present ~ week, data = habituation_frequency_21)
summary(anova_hab_freq_2021)
#not significant



#test habituation effect 2024

#vigilance dataframe
habituation_vigilance_24 <- Baboon_vigilance_stats_24 %>%
  select(file_name, proportion_vigilant) %>%
  mutate(month = sub(".*?_(\\d{2}).*", "\\1", file_name)) %>%
  mutate(day = sub(".*?_(\\d{2})(\\d{2}).*", "\\2", file_name)) %>%
  mutate(
    month = as.numeric(month),
    day = as.numeric(day)
  ) %>%
  mutate(week = case_when(
    month == 06 & day >= 1 & day <= 7 ~ 1,
    month == 06 & day >= 8 & day <= 14 ~ 2,
    month == 06 & day >= 15 & day <= 21 ~ 3,
    month == 06 & day >= 22 & day <= 30 ~ 4,
    month == 07 & day >= 1 & day <= 7 ~ 5,
    month == 07 & day >= 8 & day <= 14 ~ 6,
    month == 07 & day >= 15 & day <= 21 ~ 7,
    month == 07 & day >= 22 & day <= 31 ~ 8,
    month == 08 & day >= 1 & day <= 7 ~ 9,
    month == 08 & day >= 8 & day <= 14 ~ 10,
    month == 08 & day >= 15 & day <= 21 ~ 11,
    month == 08 & day >= 22 & day <= 30 ~ 12,
    TRUE ~ NA_real_
  )) %>%
  mutate(week = factor(week))

# Run ANOVA to test for significant effect between 'proportion_vigilant' and 'week'
anova_hab_vig_2024 <- aov(proportion_vigilant ~ week, data = habituation_vigilance_24)
summary(anova_hab_vig_2024)
#not significant

# Perform Tukey's HSD test
tukey_result <- TukeyHSD(anova_hab_vig_2021)
summary(tukey_result)
tukey_result$week

#latency dataframe
habituation_latency_24 <- Baboon_flight_stats_24 %>%
  select(file_name, latency_to_flee) %>%
  mutate(month = sub(".*?_(\\d{2}).*", "\\1", file_name)) %>%
  mutate(day = sub(".*?_(\\d{2})(\\d{2}).*", "\\2", file_name)) %>%
  mutate(
    month = as.numeric(month),
    day = as.numeric(day)
  ) %>%
  mutate(week = case_when(
    month == 06 & day >= 1 & day <= 7 ~ 1,
    month == 06 & day >= 8 & day <= 14 ~ 2,
    month == 06 & day >= 15 & day <= 21 ~ 3,
    month == 06 & day >= 22 & day <= 30 ~ 4,
    month == 07 & day >= 1 & day <= 7 ~ 5,
    month == 07 & day >= 8 & day <= 14 ~ 6,
    month == 07 & day >= 15 & day <= 21 ~ 7,
    month == 07 & day >= 22 & day <= 31 ~ 8,
    month == 08 & day >= 1 & day <= 7 ~ 9,
    month == 08 & day >= 8 & day <= 14 ~ 10,
    month == 08 & day >= 15 & day <= 21 ~ 11,
    month == 08 & day >= 22 & day <= 30 ~ 12,
    TRUE ~ NA_real_
  )) %>%
  mutate(week = factor(week))


# Run ANOVA to test for significant effect between 'latency' and 'week'
anova_hab_lat_2024 <- aov(latency_to_flee ~ week, data = habituation_latency_24)
summary(anova_hab_lat_2024)
#not significant

#frequency dataframe
habituation_frequency_24 <- Baboon_frequency_stats_24 %>%
  select(file_name, flight_present) %>%
  mutate(month = sub(".*?_(\\d{2}).*", "\\1", file_name)) %>%
  mutate(day = sub(".*?_(\\d{2})(\\d{2}).*", "\\2", file_name)) %>%
  mutate(
    month = as.numeric(month),
    day = as.numeric(day)
  ) %>%
  mutate(week = case_when(
    month == 06 & day >= 1 & day <= 7 ~ 1,
    month == 06 & day >= 8 & day <= 14 ~ 2,
    month == 06 & day >= 15 & day <= 21 ~ 3,
    month == 06 & day >= 22 & day <= 30 ~ 4,
    month == 07 & day >= 1 & day <= 7 ~ 5,
    month == 07 & day >= 8 & day <= 14 ~ 6,
    month == 07 & day >= 15 & day <= 21 ~ 7,
    month == 07 & day >= 22 & day <= 31 ~ 8,
    month == 08 & day >= 1 & day <= 7 ~ 9,
    month == 08 & day >= 8 & day <= 14 ~ 10,
    month == 08 & day >= 15 & day <= 21 ~ 11,
    month == 08 & day >= 22 & day <= 30 ~ 12,
    TRUE ~ NA_real_
  )) %>%
  mutate(week = factor(week))

# Run ANOVA to test for significant effect between 'proportion_vigilant' and 'week'
anova_hab_freq_2024 <- aov(flight_present ~ week, data = habituation_frequency_24)
summary(anova_hab_freq_2024)
#not significant

# Perform Tukey's HSD test
tukey_result <- TukeyHSD(anova_hab_vig_2021)
summary(tukey_result)
tukey_result$week






