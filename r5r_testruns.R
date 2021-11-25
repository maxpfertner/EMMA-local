# load packages

options(java.parameters = '-Xmx22G')

library(rJava)
library(r5r)
library(sf)
library(data.table)
library(ggplot2)
library(mapview)
library(RPostgreSQL)
library(postGIStools)
library(rpostgis)
library(geojsonR)
library(dplyr)
library(tmap)

mapviewOptions(platform = 'leafgl')



# Java Garbage collection
jgc <- function()
{
  gc()
  .jcall("java/lang/System", method = "gc")
}    


# setup r5r

path <- paste0(getwd(), "/data/filtered2")
r5r_core <- setup_r5(data_path = path, verbose = FALSE)
jgc()

# connect to database on netcup server

con <- dbConnect(PostgreSQL(),
                 dbname = "emma_db", user = "postgres",
                 host = "195.128.100.116",
                 #host = "localhost",
                 password = "internet1893"
)


# select destination and study area

weichselbaum <- data.frame(
  stringsAsFactors = FALSE,
  id = c("weichselbaum"),
  lon = c(11.276105),      #11.276105   11.565878 (TUM)
  lat = c(48.085196)       #48.085196   48.148837 (TUM)
) 

location <- st_as_sf(weichselbaum, coords = c("lon", "lat"), 
                     crs = 4326, agr = "constant")



census_grid_radius <- 40000 #meters
lon <- 11.276105
lat <- 48.085196

#query <- paste0("SELECT * FROM public.zensus100m_emm_ew WHERE ST_DWithin(geom, st_transform(st_setsrid(ST_MakePoint(", lon, ",", lat,"),4326),3857),", census_grid_radius, ");")

#census_points <- pgGetGeom(conn = con, query= query)

#query1km <- "SELECT * FROM zensusgrid_500m_emm;"
query1km <- paste0("SELECT * FROM zensusgrid_500m_emm WHERE ST_DWithin(geom, st_transform(st_setsrid(ST_MakePoint(", lon, ",", lat,"),4326),3857),", census_grid_radius, ");")
#query1km <- paste0("SELECT * FROM zensus100m_emm_ew WHERE ST_DWithin(geom, st_transform(st_setsrid(ST_MakePoint(", lon, ",", lat,"),4326),3857),", census_grid_radius, ");")
#query1km <- paste0("SELECT * FROM zensusgrid_100m_grid_muc WHERE ST_DWithin(geom, st_transform(st_setsrid(ST_MakePoint(", lon, ",", lat,"),4326),3857),", census_grid_radius, ");")



census_points_1km <- pgGetGeom(conn = con, query= query1km)
#census_points_df <- data.frame(census_points)

#create radius ring object for viz
ringquery <- paste0("SELECT ST_Buffer(st_transform(ST_GeomFromText('SRID=4326;POINT(",lon," ", lat, ")'), 3857),", census_grid_radius, ", 'quad_segs=16');")
radius_ring <- pgGetGeom(conn = con, query= ringquery, geom="st_buffer")

#map
tm_shape(census_points_1km) +
  tm_dots()





points_car <- st_as_sf(census_points_1km) %>% st_transform(4326) %>% st_centroid() #st_centroid only needed for 500m cells
points_car <- points_car %>% mutate(id=id)

# points <- st_as_sf(census_points) %>% st_transform(4326)
# points <- points %>% mutate(id=gitter_id_100m)

# CAR
# set inputs
#mode <- c("WALK", "TRANSIT")
mode <- "CAR"
max_walk_dist <- 1000
max_trip_duration <- 30
departure_datetime <- as.POSIXct("10-11-2021 07:00:00",
                                 format = "%d-%m-%Y %H:%M:%S")

# calculate a travel time matrix
car_time <- system.time(
ttm_car <- travel_time_matrix(r5r_core = r5r_core,
                              origins = points_car,
                              destinations = weichselbaum,
                              mode = mode,
                              departure_datetime = departure_datetime,
                              max_walk_dist = max_walk_dist,
                              max_trip_duration = max_trip_duration,
                              n_threads = 6,
                              verbose = F)
)

# time: nPOINTS= 68
# n_threads = 1 --> 204s
# n_threads = 7 --> 55s
# 12 --> 48s
# 36 --> 

ttm_car <- ttm_car %>% mutate(id=fromId)

mapping <- left_join(points_car, ttm_car, by = "id")
mapping <- mapping %>% mutate(tt_car = travel_time)

tmap_mode("view")
tm_shape(mapping) +
  tm_dots("tt_car",
              style="quantile",
              title="Travel time CAR") +
  tm_basemap(server = "https://tileserver.memomaps.de/tilegen/{z}/{x}/{y}.png")





# stop r5r and free RAM, then restart r5r
stop_r5()
r5r_core <- setup_r5(data_path = path, verbose = FALSE)
jgc()




# PT
# set inputs
mode <- c("WALK", "TRANSIT")
max_walk_dist <- 1500
max_trip_duration <- 60
departure_datetime <- as.POSIXct("10-11-2021 08:00:00",
                                 format = "%d-%m-%Y %H:%M:%S")

# calculate a travel time matrix
pt_time <- system.time(
ttm_pt <- travel_time_matrix(r5r_core = r5r_core,
                             origins = points_car,
                             destinations = weichselbaum,
                             mode = mode,
                             departure_datetime = departure_datetime,
                             time_window = 31,
                             percentiles = c(1, 5, 25, 50, 75, 95),
                             max_rides = 2,
                             max_walk_dist = max_walk_dist,
                             max_trip_duration = max_trip_duration,
                             n_threads = 7,
                             verbose = FALSE)
)
ttm_pt <- ttm_pt %>% mutate(id=fromId)
#mapping <- mapping %>% mutate(id=gitter_id_1km)
mapping <- left_join(mapping, ttm_pt, by = "id")
mapping <- mapping %>% mutate(tt_pt = travel_time_p001)

tm_shape(mapping) +
  tm_dots("tt_pt",
          style="quantile",
          title="Travel time PT") +
  tm_basemap(server = "https://tileserver.memomaps.de/tilegen/{z}/{x}/{y}.png")



# clean up
mapping <- mapping %>% select(id, einwohner = einwohner_sum, tt_pt, tt_car, geometry) %>% 
  mutate(tt_ratio = tt_pt/tt_car)

# mapview(mapping, zcol="tt_ratio", labels = T)
# mapview(mapping, zcol="tt_car")
# mapview(mapping, zcol="tt_pt")
# mapview(mapping, zcol = "id")

tm_shape(mapping) +
  tm_dots("tt_ratio",
          style="quantile",
          title="Travel time ratio") +
  tm_basemap(server = "https://tileserver.memomaps.de/tilegen/{z}/{x}/{y}.png")


# tmap_mode("view")
tm_shape(mapping) +
  tm_dots("tt_ratio",
          style="quantile",
          title="Travel time ratio") +
  tm_basemap(server = "https://tileserver.memomaps.de/tilegen/{z}/{x}/{y}.png")




# stop r5r and free RAM
stop_r5()
jgc()

# save result to PostGIS DB
# dbSendQuery(con, "DROP TABLE IF EXISTS mapping_100m_5km;")
# try(dbWriteTable(con, name = c("public", "mapping_100m_5km"), value = mapping))


# link to grid

#get grid
grid_query <- paste0("SELECT * FROM zensusgrid_500m_emm WHERE ST_DWithin(geom, st_transform(st_setsrid(ST_MakePoint(", lon, ",", lat,"),4326),3857),", census_grid_radius, ");")

grid <- pgGetGeom(conn = con, query= grid_query)

mapping2 <- mapping %>% st_drop_geometry() %>% as_data_frame()
mapping <- left_join(points_car, ttm_car, by = "id")

grid2 <- grid %>% st_as_sf()
grid3 <- left_join(grid2, mapping2, by = c("id" = "id"))

# # analysis
# 
# mapping %>%
#   group_by(tt_car) %>%
#   summarize(affected = sum(einwohner)) %>%
#   ggplot(aes(x=tt_car, y = affected)) +
#   geom_density(stat="identity")
# 
# mapping %>%
#   group_by(tt_pt) %>%
#   summarize(affected = sum(einwohner)) %>%
#   ggplot(aes(x=tt_pt, y = affected)) +
#   geom_density(stat="identity") +
#   scale_x_continuous()



#bivariate map
# see https://cran.r-project.org/web/packages/biscale/vignettes/biscale.html
#install.packages("biscale")
library(biscale)
library(cowplot)

data <- bi_class(grid3, x = einwohner.x, y = tt_ratio, style = "quantile", dim = 3)


map <- ggplot() +
  geom_sf(data = data, mapping = aes(fill = bi_class),color = "white", size = 0.1,  show.legend = F) +
  bi_scale_fill(pal = "DkViolet", dim = 3)+
  # labs(
  #   title = "Race and Income in St. Louis, MO",
  #   subtitle = "Dark Blue (DkBlue) Palette"
  # ) +
  bi_theme()

legend <- bi_legend(pal = "DkViolet",
                    dim = 3,
                    xlab = "Higher population ",
                    ylab = "Higher tt ratio ",
                    size = 8)

# combine map with legend
finalPlot <- ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, 0.2, .65, 0.2, 0.2)
finalPlot

# fix issue with Inf travel time ratio
grid3 <- grid3 %>% mutate(tt_ratio = ifelse(tt_ratio == Inf, NA, tt_ratio))
 
 
tmap_mode("view")
tm_shape(grid3) +
  tm_polygons("tt_pt",
          style="quantile",
          title="Travel time ratio",
          alpha = .5) +
  tm_basemap(server = "https://tileserver.memomaps.de/tilegen/{z}/{x}/{y}.png")

# saveRDS(grid3, file = "grid40km.rds")
saveRDS(mapping, file = paste0(getwd(), "/results/2021-11-25_Weichselbaum/mapping.rds"))
saveRDS(ttm_pt, file = paste0(getwd(), "/results/2021-11-25_Weichselbaum/ttm_pt.rds"))
saveRDS(ttm_car, file = paste0(getwd(), "/results/2021-11-25_Weichselbaum/ttm_car.rds"))
saveRDS(grid, file = paste0(getwd(), "/results/2021-11-25_Weichselbaum/grid.rds"))
saveRDS(grid2, file = paste0(getwd(), "/results/2021-11-25_Weichselbaum/grid2.rds"))
saveRDS(grid3, file = paste0(getwd(), "/results/2021-11-25_Weichselbaum/grid3.rds"))
saveRDS(weichselbaum, file = paste0(getwd(), "/results/2021-11-25_Weichselbaum/weichselbaum.rds"))
saveRDS(census_points_1km, file = paste0(getwd(), "/results/2021-11-25_Weichselbaum/censuspoints.rds"))
