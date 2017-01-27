library(dplyr)
library(leaflet)
library(ggmap)

head(events)
coords <- events %>%
  select(device_id, longitude, latitude) %>%
  filter(is.na(device_id)==FALSE, longitude >=90, latitude>=24) %>%
  group_by(device_id,longitude,latitude) %>%
  summarise(nb_events=n())

coords <- merge(coords, phone_brand, by="device_id")
coords <- as.data.frame(coords)
coords_set <- sample_n(coords, 1000, replace = FALSE)
summary(coords_set$nb_events)

titles<-paste("ID:", coords_set$device_id,"<br>",
              "brand:",coords_set$phone_brand, "<br>",
              "model:",coords_set$device_model, "<br>",
              "events:", coords_set$nb_events)
leaflet(data=coords_set) %>%
  addTiles() %>%
  addCircleMarkers(~longitude, ~latitude, 
                   radius=~ifelse(nb_events > 1000,nb_events/100,sqrt(nb_events)),
                   clusterOptions = markerClusterOptions(),
                   popup=titles)

add<- mapply(FUN = function(lon, lat) revgeocode(c(lon, lat)),coords_set$longitude,
             coords_set$latitude)


