# Prepare map layers for survey data lobster project


library(sf)
library(ggplot2)
library(tmap)
# library(leaflet)
###################################################



# check layers in lobster geopackage
st_layers(
  "data/spatial/lobster_layers.gpkg"
  )

  
# read different layers

# coast line from Sjökort
study_area_line = st_read(
  dsn = "data/spatial/lobster_layers.gpkg",
  layer = "study_area_LL_line"
  )

# coast polygon from Europe_coastline.shp 
west_coast = st_read(
  dsn = "data/spatial/lobster_layers.gpkg",
  layer = "west_coast"
  )

# OpenStreetMap data of the land polygons
west_coast_osm = st_read(
  dsn = "data/spatial/lobster_layers.gpkg",
  layer = "west_coast_osm"
)

# Kåvra MPA, manually created
kaavra = st_read(
  dsn = "data/spatial/lobster_layers.gpkg",
  layer = "kaavra_MPA"
  )


# check and set crs
st_crs(study_area_line)
st_crs(west_coast_osm)

st_crs(west_coast)
west_coast = st_transform(west_coast, 4326)
st_crs(west_coast)


# crop west coast
# check bounding box
st_bbox(west_coast_osm)

# create bounding box
bbox = c(
  xmin = 11, ymin = 58,
  xmax = 12, ymax = 58.5)

LL_osm= st_crop(west_coast_osm, bbox)

# simplify west coast
west_coast_simple = west_coast |>
  st_simplify(dTolerance = 500)

st_crs(west_coast_simple)


# create Kåvra polygon
# Note: Only for demo; layer already saved in as layer in GPKG, kaavra_MPA)
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


# write sf object to file
# here as a layer in a geopackage
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


# base map, west coast simple
# ggplot
ggplot() +
geom_sf(
  data = west_coast_simple,
  fill = "grey70") +
  theme_classic()

# tmap
tm_shape(shp = west_coast_simple) +
    tm_polygons() +
    tm_graticules(
      lines = FALSE
    )



# base map, Lysekil study area
# set limits
# ggplot
ggplot() +
geom_sf(
  data = LL_osm) +
coord_sf(
  xlim = c(11.3, 11.45),
  ylim = c(58.27, 58.37)) +
theme_classic()


# add layers
# study area + Kåvra
# ggplot
ggplot() +
geom_sf(
  data = LL_osm) +
geom_sf(
  data = kaavra,
  fill = "blue", alpha = 0.1) +
coord_sf(xlim = c(11.3, 11.4),
         ylim = c(58.3, 58.35)) +
theme_classic()

# tmap
tm_shape(
  shp = LL_osm,
  bbox = st_bbox(
    c(xmin = 11.3, xmax = 11.4, ymin = 58.3, ymax = 58.35), crs = 4326)) +
    tm_polygons() +

tm_shape(
  shp = kaavra) +
  tm_polygons(
    fill = "blue",
    fill_alpha = 0.1) +
  tm_graticules(
      lines = FALSE
    )




# study area + kåvra + stations
ggplot() +
geom_sf(
  data = LL_osm) +
geom_sf(
  data = kaavra,
  fill = "blue", alpha = 0.1) +
geom_sf(
  data = subset(station_sf, site == "Kåvra")[c(1, 100, 200, 400), ],
  color = "red") +
coord_sf(xlim = c(11.3, 11.4),
         ylim = c(58.3, 58.35)) +
theme_classic()


# map point size to number of lobsters
ggplot() +
geom_sf(
  data = LL_osm,
  fill = "grey70") +
geom_sf(
  data = kaavra,
  fill = "blue", alpha = 0.1) +
geom_sf(
  data = subset(station_sf, site == "Kåvra" & n_lob > 0)[c(1, 100, 200, 400), ],
  aes(size = n_lob),
  color = "red") +
coord_sf(xlim = c(11.3, 11.4),
         ylim = c(58.3, 58.35)) +
theme_classic()


m = tm_shape(
  shp = LL_osm,
  bbox = st_bbox(
    c(xmin = 11.3, xmax = 11.4, ymin = 58.3, ymax = 58.35), crs = 4326)) +
    tm_polygons() +

tm_shape(
  shp = kaavra) +
  tm_polygons(
    fill = "blue",
    fill_alpha = 0.1) +

tm_shape(
  shp = subset(station_sf, site == "Kåvra" & n_lob > 0)[c(1, 100, 200, 400), ]) +
  tm_symbols(
    fill = "red",
    size = "n_lob"
    ) +

tm_graticules(
  lines = FALSE
    )

m
##########

# raster

tm_shape(west_coast) +
 tm_polygons() +
tm_shape(land) +
  tm_raster("cover")



# interactive map
# tmap
tmap_mode("view")
m


m = tm_shape(shp = kaavra) + tm_polygons()

tmap_mode("plot")

# leaflef
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

# two different subsets of individuals
# 1. select individuals which have been captured >= 2
recap = capt[ ,
  if(.N >= 2) .(year, date_haul, site, lat_deg, lon_deg), by = ind_id]

setorder(recap, ind_id, date_haul)


# 2. select individuals which
# 1 captured >= 2
# 2 first site Kåvra
# 3 at least one non-SLU capt

# TODO missing site at first capture is wrong (wrong date of non-SLU catch?)
# TODO missing site at capture which must by SLU survey, according to date.

# sort by individual and date
setorder(capt, ind_id, date_haul)

recap = capt[ ,
  if(.N >= 2 & !is.na(first(site)) & first(site) == "Kåvra" & any(data_source != "slu_survey")) .(year, date_haul, site, lat_deg, lon_deg), by = ind_id]


cols = c("date_haul", "lat_deg", "lon_deg")
to_cols = paste(cols, "next", sep = "_")
to_cols
recap[ , (to_cols) := shift(.SD, 1, type = "lead"),
             .SDcols = cols,
             by = ind_id]

# remove last NA row by tag
recap = recap[ , .SD[-.N], by = ind_id]


# plot 4 lobsters
set.seed(3)
ggplot() +

geom_sf(data = west_coast_osm) +
coord_sf(
    xlim = c(11.2, 11.8),
    ylim = c(58.2, 58.55)) +
geom_curve(
    #data = recap[ind_id %in% sample(unique(ind_id), 20)],
    data = recap[lat_deg != lat_deg_next],
    aes(x = lon_deg, y = lat_deg, xend = lon_deg_next, yend = lat_deg_next),
    arrow = arrow(length = unit(0.01, "npc")),
    color = "blue") +
 
  theme_classic()




