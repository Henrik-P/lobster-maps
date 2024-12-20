# Prepare map layers for survey data lobster project


library(sf)
#library(dplyr)
library(ggplot2)
###################################################



# check layers in lobster geopackage
st_layers(
  "data/spatial/lobster maps.gpkg"
  )

# read different layers
study_area = st_read(
  dsn = "data/spatial/lobster maps.gpkg",
  layer = "survey study area Lysekil"
  )

west_coast = st_read(
  dsn = "data/spatial/lobster maps.gpkg",
  layer = "Swedish westcoast"
  )

kaavra = st_read(
  dsn = "data/spatial/lobster maps.gpkg",
  layer = "kaavra_MPA"
  )


# check and set crs
st_crs(study_area)
st_crs(west_coast)

# west_coast = st_transform(west_coast, 4326)
# st_crs(west_coast)

# simplify west coast
west_coast_simple = west_coast |>
  st_simplify(dTolerance = 500)

# crop map
bbox = c(
  xmin = 11, ymin = 58,
  xmax = 12, ymax = 58.5)

west_coast= st_crop(west_coast, bbox)



# create Kåvra polygon
# Only for demo; layer already saved in as layer)
# coordinates Kåvra MPA
# see email 2024-03-12 08:58

kaavra = data.table(
  lat_dm = c("58 20.437", "58 20.454", "58 20.422", "58 20.069", "58 19.785", "58 19.000"),
  lon_dm = c("11 21.341", "11 21.711", "11 22.216", "11 22.453", "11 22.738", "11 21.198")
)

# add point to close polygon
kaavra = rbind(kaavra, kaavra[1, ])
# format DD MM.MM
# convert to DD.dd

kaavra[ , `:=`(
  lat = as.numeric(substr(lat_dm, 1, 2)) + as.numeric(substr(lat_dm, 4, 9))/60,
  lon = as.numeric(substr(lon_dm, 1, 2)) + as.numeric(substr(lon_dm, 4, 9))/60
)]

kaavra_m = as.matrix(kaavra[ , .(lon, lat)])
kaavra_sf = st_sf(st_sfc(st_polygon(list(kaavra_m))))
st_crs(kaavra_sf) = 4326


# write
st_write(
  obj = kaavra_sf,
  dsn = "data/spatial/lobster maps.gpkg",
  layer = "kaavra_MPA"
  )

st_layers(
  "data/spatial/lobster maps.gpkg"
  )

###################



# lobster data

# individual captures 
capt = fread(
  "data/catch/lobster_individual_capture.csv",
  encoding = "UTF-8" # encoding set to UTF-8 when writing.
)

# select rows with coord and relevant columns
capt = capt[
  !is.na(lat_deg) & !is.na(lon_deg),
  .(
  station_id, data_source, site, lat_deg, lon_deg, year, date_haul, ind_id
  )]


# coerce to sf
capt_sf = st_as_sf(
  capt,
  coords = c("lon_deg", "lat_deg"),
  crs = 4326)

###########


# station


station = fread(
  "data/station/lobster_station.csv",
  encoding = "UTF-8" # encoding set to UTF-8 when writing.
)

# select rows with coord and relevant vars
station = station[
  !is.na(lat_deg) & !is.na(lon_deg),
  .(station_id, data_source, site, lat_deg, lon_deg, year, n_lob)]


# coerce to sf
station_sf = st_as_sf(
  station,
  coords = c("lon_deg", "lat_deg"),
  crs = 4326)

#############



# base map, study area
ggplot() +
geom_sf(
  data = land,
  fill = "grey70") +
  theme_classic()

# limits
ggplot() +
geom_sf(
  data = land,
  fill = "grey70") +
coord_sf(xlim = c(11.3, 11.45),
         ylim = c(58.25, 58.37))


# base map, land + Kåvra
ggplot() +
geom_sf(
  data = land_ll_simple,
  fill = "grey70") +
geom_sf(
  data = kaavra_sf,
  fill = "blue", alpha = 0.1) +
coord_sf(xlim = c(11.3, 11.4),
         ylim = c(58.3, 58.35))


# land and stations
ggplot() +
geom_sf(
  data = land_ll_simple,
  fill = "grey70") +
geom_sf(
  data = kaavra_sf,
  fill = "blue", alpha = 0.1) +
geom_sf(
  data = subset(station_sf, site == "Kåvra")[c(1, 100, 200, 400), ],
  color = "red") +
coord_sf(xlim = c(11.3, 11.4),
         ylim = c(58.3, 58.35))


# map point size to number of lobsters
ggplot() +
geom_sf(
  data = land_ll_simple,
  fill = "grey70") +
geom_sf(
  data = subset(station_sf, site == "Kåvra" & n_lob > 0)[c(1, 100, 200, 400), ],
  aes(size = n_lob),
  color = "red") +
coord_sf(xlim = c(11.3, 11.4),
         ylim = c(58.3, 58.35))

##########


# interactive map of station

# color palette
pal = colorNumeric(
  palette = "magma",
  domain = station_sf$n_lob)

previewColors(
  pal,
  values = sort(unique(station_sf$n_lob))
)


leaflet(station_sf) %>%
addProviderTiles('Esri.WorldGrayCanvas') %>%
addPolygons(
  data = kaavra_sf,
  weight = 0,
  fillOpacity = 0.2
  ) |>
  addCircleMarkers(
  data = subset(station_sf,
  site == "Kåvra" & year == 2023 & n_lob > 1),
  stroke = FALSE,
  fillOpacity = 0.4,
  color = ~pal(n_lob)) |>
  addLegend(
  "bottomright",
  pal = pal,
  values = ~n_lob,
  title = "Number of lobster per station"
)
# |> setView(lng = 11.37, 58.32, zoom = 5)

################################

# movement

# select individuals which have been captured >= 2
recap = capt[ , if(.N >= 2) .(year, date_haul, site, lat_deg, lon_deg), by = ind_id]

setorder(recap, ind_id, date_haul)

# test create "to variables"
# d = data.table(tag = rep(1:3, c(2, 3, 2)),
#                date = c(1:2, 1:3, 1:2),
#                lat = 1:7, lon = 7:1)
# 
# cols = c("date", "lat", "lon")
# to_cols = paste("to", cols, sep = "_")
# 
# d[ , (to_cols) := data.table::shift(.SD, 1, type = "lead"),
#    .SDcols = cols,
#    by = tag]

cols = c("date_haul", "lat_deg", "lon_deg")
to_cols = paste(cols, "next", sep = "_")

recap[ , (to_cols) := shift(.SD, 1, type = "lead"),
             .SDcols = cols,
             by = ind_id]

# remove last NA row by tag
recap = recap[ , .SD[-.N], by = ind_id]

ggplot() +
  geom_sf(data = land_ll_simple,) +
  geom_curve(
    data = recap[ind_id %in% sample(unique(ind_id), 4)],
    aes(x = lon_deg, y = lat_deg, xend = lon_deg_to, yend = lat_deg_to),
    arrow = arrow(length = unit(0.01, "npc"))) +
  coord_sf(xlim = c(11.3, 11.45),
           ylim = c(58.2, 58.4))
