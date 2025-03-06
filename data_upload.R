#set wd
setwd("C:/Users/sophi/OneDrive/Desktop/gorongosa-abr")
#Read the data in 
Baboon_trial_data <- read.csv("gt.txt", header = FALSE)
#check structure 
View(Baboon_trial_data)
#add column names from CVAT export data site
colnames(Baboon_trial_data) <- c("frame_id","track_id", "x", "y","w","h","not_ignored","class_id","visibility")

#Sampling for F07 2024 
set.seed(22)

# Generate 88 random numbers from 1 to 120
random_numbers <- sample(1:120, 88, replace = FALSE)
random_numbers
