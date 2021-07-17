########################################################
## 3 - SIMPLE MAPS
########################################################
## Author: Abel Gelman
########################################################


library(utils)
library(tidyverse)
# Download the shapefile.
download.file("http://thematicmapping.org/downloads/TM_WORLD_BORDERS_SIMPL-0.3.zip" , 
              destfile="/Users/Abel/Documents/CartONG/R_webinar/R_training/world_shape_file.zip") # my destination folder

unzip("/Users/Abel/Documents/CartONG/R_webinar/R_training/world_shape_file.zip")


# Read this shape file with the rgdal library. 
library(rgdal)
my_spdf <- readOGR( 
  dsn= paste0(getwd()) , 
  layer="TM_WORLD_BORDERS_SIMPL-0.3",
  verbose=FALSE
)

# Basic plot of this shape file:
par(mar=c(10,4,4,2))
plot(my_spdf, col="#f2f2f2", bg="skyblue", lwd=0.25, border=0 )


# 'fortify' the data to get a dataframe format required by ggplot2
library(broom)
spdf_fortified <- tidy(my_spdf, region = "NAME") %>% 
  filter(id != "Antarctica")

# Plot it
library(ggplot2)
ggplot() +
  geom_polygon(data = spdf_fortified, aes( x = long, y = lat, group = group), fill="grey", color="white") +
  theme_void() 

library(rgdal)
library(broom)
library(ggplot2)
library(maps)

### Subset world shapefile with countries of interest
world <- readOGR( 
  dsn= paste0(getwd()) , 
  layer="TM_WORLD_BORDERS_SIMPL-0.3",
  verbose=FALSE)

world_fortified <- fortify(world) 

world_fortified_G <-  ggplot()+
  geom_polygon(data =world_fortified, 
               aes(x = long, y = lat, group = group),
               color = 'gray', fill = 'gray', size = .2)


# SELECT COUNTRIES OF INTEREST
drs19 <- unique(df19$Country) # vector with countries of interest

drs_cty <- subset(world, world$NAME %in% drs19) # subset shapefile

#### plot staitc map
library(rgdal)     # R wrapper around GDAL/OGR
library(ggplot2)   # for general plotting
library(ggmaps)    # for fortifying shapefiles

# convert shapefile  to a dataframe for use in ggplot2
drs_cty_fortified <- fortify(drs_cty)

# Now the shapefile can be plotted as either a geom_path or a geom_polygon.
# Paths handle clipping better. Polygons can be filled.
# You need the aesthetics long, lat, and group.
drs19map <- ggplot() +
  geom_polygon(data = world_fortified, 
               aes(x = long, y = lat, group = group),
               color = 'gray', fill = 'gray', size = .2)+
  geom_polygon(data = drs_cty_fortified, 
               aes(x = long, y = lat, group = group),
               color = '#0072BC', fill = '#0072BC', size = .2)+
  theme_minimal() 


print(drs19map) 



# change the projection and aesthetic.
library(mapproj)
map_projected_gilbert <- drs19map +
  coord_map("gilbert",
            ylim = c(75, -50))+
  theme_void()+
  labs(title = "2019 DRS Livelhoods Monitoring Program",
       subtitle = "Participating Countries",
       x ="", y="")

map_projected_gilbert

map_projected_gall <- drs19map +
  coord_map("gall", lat0 = 10,
            ylim = c(75, -50))+
  theme_void()+
  labs(title = "2019 DRS Livelhoods Monitoring Program",
       subtitle = "Participating Countries",
       x="",y="")

map_projected_gall


## interactive maps with leaflet

library(tidyverse)
library(leaflet)

leaflet(data = drs_cty) %>% 
  addTiles() %>%
  addPolygons(color = "#0072BC", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5,
              highlightOptions = highlightOptions(color = "grey", weight = 2,
                                                  bringToFront = TRUE)) %>% 
  addProviderTiles(providers$Esri.WorldImagery, group = "World Imagery") %>%
  addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
  addLayersControl(baseGroups = c("Toner Lite", "World Imagery"))



