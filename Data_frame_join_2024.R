#2024 Joining dataframes (CVAT annotations and second watch) 

#load packages
library(xml2)
library(dplyr)
library(ggplot2)
library(stringr)
library(tidyverse)

#Import second watch metadata in CSV format 
B_24_second <- read.csv("C:/Users/sophi/OneDrive/Desktop/gorongosa_baboon/gorongosa-abr/gorongosa-abr/Baboon_second_watch_FINAL_2024.csv")

#make file name column in second watch data to join with file_name from CVAT annotations
B_24_second<- B_24_second %>%
  mutate(file_name = paste(year, site,video_name, sep = "_"))
View(B_24_second)

#Import CVAT annotations in CVAT1.1 format to XML
Baboon_2024_vid_CVAT <- read_xml("C:/Users/sophi/OneDrive/Desktop/gorongosa-abr/gorongosa-abr/annotations_2024_sample.xml")

#Convert XML to dataframe
xml_file <- read_xml("C:/Users/sophi/OneDrive/Desktop/gorongosa-abr/gorongosa-abr/annotations_2024_sample.xml")

# Extract all tasks (task = 1 video)
tasks <- xml_find_all(xml_file, ".//task")

# Create task and video ID
task_df <- map_df(tasks, ~{
  tibble(
    task_id = as.integer(xml_text(xml_find_first(.x, ".//id"))),
    task_name = xml_text(xml_find_first(.x, ".//name"))
  )
})
head(task_df)

# Create annotations with task ID
task_df <- map_df(tasks, ~{
  tibble(
    task_id = as.integer(xml_text(xml_find_first(.x, ".//id"))),
    task_name = xml_text(xml_find_first(.x, ".//name"))
  )
})
head(task_df)

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

# View the resulting data frame
print(frame_df)

# Join with file name
frame_df2 <- left_join(frame_df, task_df)
colnames(frame_df2)[colnames(frame_df2) == "task_name"] <- "file_name"
View(frame_df2)

# Join datasets based on filename
Final_2024 <- frame_df2 %>%
  left_join(B_24_second, by = "file_name")

view(Final_2024)

#Clean dataset for unecessary coulumns and rename 

#rename columns
colnames(Final_2024)[colnames(Final_2024) == "task_id"] <- "Task_ID"
colnames(Final_2024)[colnames(Final_2024) == "label"] <- "Behaviour"





