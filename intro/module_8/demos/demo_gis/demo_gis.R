library(dplyr)   ## for manipulating data
library(ggplot2) ## for plotting data
library(sf)      ## for working with geospatial data
library(ggspatial) ## for basemaps and north arrows

################### READ DATA ##################################################

## read cleaned data
linelist <- readRDS("C:/Users/Spina/Downloads/intro_course/data/clean/backup/linelist_combined_20141201.rds")

## read in shapefile
shapefile <- read_sf(
  ## define path to file
  "C:/Users/Spina/Downloads/intro_course/data/shp/sle_adm3.shp"
)


######## FILTER DATA ###########################################################


## districts we are interested in
districts <- linelist %>%
  distinct(admin3pcod) %>%
  tidyr::drop_na() %>%
  pull()

## filter shapefile for districts of interest
shapefile <- shapefile %>%
  filter(admin3Pcod %in% districts)

######### Basic shape plot ####################################################

## open up a ggplot
shape_plot <- ggplot() +
  ## add the shapefile on top
  geom_sf(data = shapefile,
          # no fill
          fill = NA,
          # black borders
          colour = "black")


############# POINTS  ##########################################################

## take a random sample of GPS points (we dont have case lat/lon)
random_points <- sf::st_sample(shapefile, nrow(linelist))

## plot points
shape_plot +
  geom_sf(data = random_points)

############# CHOROPLETHS ####################################################

## get counts of cases by district
case_counts <- linelist %>%
  group_by(admin3pcod) %>%
  count() %>%
  rename(counts = n)



## add case counts to your dataset
shapefile <- left_join(shapefile, case_counts, by = c("admin3Pcod" = "admin3pcod"))


## plot choropleth
ggplot() +
  ## add the shapefile on top
  geom_sf(data = shapefile,
          # fill by case count
          aes(fill = counts),
          # black borders
          colour = "black")



############ BASE MAPS #########################################################



# get the bounding box for the shapefile
bounding_box <- shapefile %>%
  st_bbox()


# plot a base map including scale bar
basemap <- ggplot() +
  # change the bounding box to an sf object
  # this defines the area to download map tiles for
  geom_sf(data = st_as_sfc(bounding_box)) +
  # download map tiles and add to the plot
  annotation_map_tile(
    # define what map tiles to use
    type =  "cartolight",
    # define folder to store tile images
    cachedir = here::here("data", "map_tiles"),
    # define if should download tiles each time
    forcedownload = FALSE,
    # hide messages about download status and zoom
    progress = "none" )


basemap +
  ## add the shapefile on top
  geom_sf(data = shapefile,
          # no fill
          fill = NA,
          # black borders
          colour = "black") +
  geom_sf(data = random_points)
