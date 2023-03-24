

# R preamble --------------------------------------------------------------

library(DBI)
library(here)
library(plotly)
library(rjson) # plotly uses rjson formatted json
library(sf)
library(tidyverse)

# load custom functions
# doing it this way to mask the functions in the global environment
sys.source(
  here("R", "defaults.R"), # requires httr and jsonlite
  envir = attach(NULL, name = "defaults")
)

gpkg <- here("data", "chaco.gpkg")

dbc <- dbConnect(RSQLite::SQLite(), gpkg)

# data --------------------------------------------------------------------

four_corners <- read_sf(gpkg, "four-corners") |> rename("geometry" = geom)

watersheds <- read_sf(gpkg, "watersheds") |> rename("geometry" = geom)

inla_model <- readRDS(here("data", "inla-model.rds"))

profiles <- dbReadTable(dbc, "profiles") |> 
  as_tibble() |> 
  mutate(relative_rate = inla_model$summary.fitted.values$mean)

xy <- watersheds |> 
  st_union() |> 
  st_centroid() |> 
  st_coordinates() |> 
  c()

years <- seq(rng(origin), rng(terminus), by = 25)



missing_watersheds <- tibble(watershed = selection) |> 
  filter(!(watershed %in% unique(room_counts$watershed))) |> 
  mutate(year = min_year, rooms = 0)

room_counts <- room_counts |> 
  bind_rows(missing_watersheds) |> 
  group_by(watershed) |> 
  complete(
    year = years, 
    fill = list(rooms = 0)
  ) |> 
  ungroup() |> 
  distinct() |> 
  mutate(watershed = as.character(watershed)) |> 
  left_join(
    watersheds |> st_drop_geometry(), 
    by = "watershed"
  ) |> 
  mutate(
    density = rooms/area_km2,
    log_density = ifelse(density > 0, log(density), log(1e-5)),
    across(c(area_km2, log_density, density, rooms), ~round(.x, 3)),
    hover = paste0(
      "<b style='font-size:1.4em;'>", name, "</b><br>",
      "<b>ID:</b> ", watershed, "<br>",
      "<b>Basin:</b> ", basin, "<br>",
      "<b>Year:</b> ", year, "<br>",
      "<b>Count:</b> ", rooms, "<br>",
      "<b>Area (km<sup>2</sup>):</b> ", area_km2, "<br>",
      "<b>Density:</b> ", density, "<br>",
      "<b>Log density:</b> ", log_density
    )
  ) |> 
  select(watershed, year, log_density, hover) |> 
  rename("hydrologic_unit" = watershed)

# remove(min_year, max_year, years, watersheds, missing_watersheds)

sauce <- paste0(
  "https://raw.githubusercontent.com/kbvernon/",
  "hndsr-watersheds/main/data/watersheds.geojson"
)

p <- plot_ly() |> 
  add_trace(
    type = "choroplethmapbox",
    geojson = sauce,
    locations = room_counts$hydrologic_unit,
    featureidkey = "properties.hydrologic_unit",
    stroke = I("#fafafa"),
    span = I(0.4),
    frame = room_counts$year,
    z = room_counts$log_density,
    zmin = floor(min(room_counts$log_density)),
    zmax = ceiling(max(room_counts$log_density)),
    colorscale = "Viridis",
    text = room_counts$hover,
    hoverinfo = "text",
    marker = list(opacity = 0.5)
  ) |>
  layout(
    mapbox = list(
      style = "stamen-terrain",
      zoom = 5,
      center = list(lon = xy[1], lat = xy[2])
    )
  )

htmlwidgets::saveWidget(
  p,
  here("plotly-map-only.html"),
  selfcontained = FALSE,
  libdir = "plotly-map-lib"
)