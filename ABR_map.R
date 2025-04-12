#ABR site map
install.packages(c("readxl", "leaflet"))
library(readxl)
library(leaflet)
library(dplyr)

#read in data
ABR_data <- read.csv("ABR_data_3.csv")
View(ABR_data)

#plot
leaflet(ABR_data) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = ~Long,
    lat = ~Lat,
    label = ~paste("Site:", Site, "<br>", "Habitat:", Habitat, "<br>", "Videos number:", total),
    color = ~ifelse(Habitat == "Open", "blue", "green"),
    radius = ~sqrt(total) * 1.1,  # Scaling radius for better visualization
    fillOpacity = 0.7
  )



#try again
leaflet(ABR_data) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = ~Long,
    lat = ~Lat,
    label = ~paste("Site:", Site, "<br>", 
                   "Habitat:", Habitat, "<br>", 
                   "Videos number:", total, "<br>",
                   "Camera Site:", Site), ,
    fillColor = ~ifelse(Habitat == "Open", "dodgerblue", "forestgreen"),
    color = ~ifelse(Site %in% c("D09", "N03"), "purple", "none"),  
    radius = ~sqrt(total) * 1.1,
    fillOpacity = 0.7
  )

#try with labels
leaflet(ABR_data) %>%
  addProviderTiles("Esri.WorldTopoMap") %>%
  #addTiles() %>%
  addCircleMarkers(
    lng = ~Long,
    lat = ~Lat,
    fillColor = ~ifelse(Habitat == "Open", "blue", "green"),
    color = ~ifelse(Site %in% c("D09", "N03"), "red", "none"),
    weight = ~ifelse(Site %in% c("D09", "N03"), 2, 2),
    radius = ~sqrt(total) * 1.1,
    fillOpacity = 0.7
  ) %>%
  addLabelOnlyMarkers(
    lng = ~Long,
    lat = ~Lat,
    label = ~Site,
    labelOptions = labelOptions(
      noHide = TRUE,
      direction = "right",  # this pushes the label to the right
      textOnly = TRUE,
      offset = c(12, 0),    # fine-tunes x (horizontal), y (vertical) offset
      style = list(
        "color" = "black",
        "font-size" = "13px",
        "font-weight" = "bold",
        "background-color" = "none",
        "padding" = "none",
        "border-radius" = "none"
      )
    )
  ) %>%
  addLegend(
    position = "bottomright",
    colors = c("blue", "green"),
    labels = c("Open", "Closed"),
    title = "Habitat Type",
    opacity = 0.7
  )

