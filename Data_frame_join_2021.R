#2021 Joining dataframes (CVAT annotations and second watch)

#load packages
install.packages("xml2")
library(xml2)
library(dplyr)
library(ggplot2)
library(stringr)
library(tidyverse)

#Import second watch metadata in CSV format 
B_21_second <- read.csv("C:/Users/sophi/OneDrive/Desktop/gorongosa_baboon/gorongosa-abr/gorongosa-abr/Baboon_second_watch_2021_fixed.csv")

#make file name column in second watch data to join with file_name from CVAT annotations
B_21_second<- B_21_second %>%
  mutate(file_name = paste(Year, Camera.trap.site,video_name, sep = "_"))
View(B_21_second)

#Convert XML to dataframe
xml_file <- read_xml("C:/Users/sophi/OneDrive/Desktop/gorongosa_baboon/gorongosa-abr/gorongosa-abr/2021_annotations_april.xml")

# Extract all tasks (task = 1 video)
tasks <- xml_find_all(xml_file, ".//task")

# Create task and video ID
task_df <- map_df(tasks, ~{
  tibble(
    task_id = as.integer(xml_text(xml_find_first(.x, ".//id"))),
    task_name = xml_text(xml_find_first(.x, ".//name"))
  )
})

# Create annotations with task ID
task_df <- map_df(tasks, ~{
  tibble(
    task_id = as.integer(xml_text(xml_find_first(.x, ".//id"))),
    task_name = xml_text(xml_find_first(.x, ".//name"))
  )
})

# Extract all <track> nodes
tracks <- xml_find_all(xml_file, ".//track")

# Convert to data frame with desired columns
frame_df <- map_df(tracks, ~{
  task_id <- xml_attr(.x, "task_id")
  label <- xml_attr(.x, "label")
  
  # Extract <box> elements within each <track>
  boxes <- xml_find_all(.x, ".//box")
  
  map_df(boxes, ~tibble(
    task_id = as.integer(task_id),
    label = label,
    frame = as.integer(xml_attr(.x, "frame"))
  ))
})


# Join with file name
frame_df2 <- left_join(frame_df, task_df)
colnames(frame_df2)[colnames(frame_df2) == "task_name"] <- "file_name"
View(frame_df2)

# Join datasets based on filename
merged_df <- frame_df2 %>%
  left_join(B_21_second, by = "file_name")

view(merged_df)

#Clean dataset for unecessary coulumns and rename 

#remove unncessary columns
merged_clean_2021_1 <- merged_df %>% select(-X)
merged_clean_2021 <- merged_clean_2021_1 %>% select(- Notes..here.could.record.anything.unusual.and.also.notes.about.interspecies.interactions..)

#rename columns
colnames(merged_clean_2021)[colnames(merged_clean_2021) == "task_id"] <- "Task_ID"
colnames(merged_clean_2021)[colnames(merged_clean_2021) == "label"] <- "Behaviour"
colnames(merged_clean_2021)[colnames(merged_clean_2021) == "Sound.quality...Good..Poor..None"] <- "Sound_quality"
colnames(merged_clean_2021)[colnames(merged_clean_2021) == "Sound.delay..s."] <- "Sound_delay_s"
colnames(merged_clean_2021)[colnames(merged_clean_2021) == "Other.species.present..list.w.commas."] <- "Other_species_present"

#rename behaviour labels
Final_2021 <- merged_clean_2021 %>%
  mutate(Behaviour = case_when(
    Behaviour == "Walking (w/o vigilance)" ~ "Walking_NV",
    Behaviour == "Staring (not walking)" ~ "Staring",
    Behaviour == "Scanning (not walking)" ~ "Scanning",
    Behaviour == "Fleeing" ~ "Flight",
    Behaviour == "Social interactions" ~ "Social_interactions",
    Behaviour == "occluded" ~ "Occluded",
    Behaviour == "walking with vigilance" ~ "Walking_V",
    Behaviour == "startling" ~ "Startling",
    Behaviour == "standing and staring" ~ "Stand_stare",
    TRUE ~ Behaviour  # Keep all other values unchanged
  ))

#Fix leopard typo
Final_2021 <- Final_2021 %>%
  mutate(Predator.cue = str_replace(Predator.cue, "Leo\\[ard", "Leopard"))

#Fix cheetah typo
Final_2021 <- Final_2021 %>%
  mutate(Predator.cue = str_replace(Predator.cue, "Cheeetah", "Cheetah"))

#Fix control typo
Final_2021 <- Final_2021 %>%
  mutate(Predator.cue = str_replace(Predator.cue, "Control ", "Control"))

#Rename WD to Wild_dog
Final_2021 <- Final_2021 %>%
  mutate(Predator.cue = str_replace(Predator.cue, "WD", "Wild dog"))

#Rename NA predator cues to No_sound
Final_2021 <- Final_2021 %>%
  mutate(Predator.cue = if_else(is.na(Predator.cue), "No_sound", Predator.cue))

