
# R preamble --------------------------------------------------------------

library(DBI)
library(gganimate)
library(ggfx)
library(ggrepel)
library(here)
library(maps)
library(sf)
library(tidyverse)

# load custom functions
# doing it this way to mask the functions in the global environment
sys.source(
  here("R", "get_basemap.R"), # requires httr and jsonlite
  envir = attach(NULL, name = "basemap")
)

sys.source(
  here("R", "defaults.R"), # requires httr and jsonlite
  envir = attach(NULL, name = "defaults")
)

gpkg <- here("data", "chaco.gpkg")

dbc <- dbConnect(RSQLite::SQLite(), gpkg)

# data --------------------------------------------------------------------

project_area <- read_sf(gpkg, "project-area") |> rename("geometry" = geom)

watersheds <- read_sf(gpkg, "watersheds") |> rename("geometry" = geom)

greathouses <- dbReadTable(dbc, "great-houses") |> as_tibble()

four_corners <- read_sf(gpkg, "four-corners") |> rename("geometry" = geom)

profiles <- dbReadTable(dbc, "profiles") |> as_tibble()

# basemap -----------------------------------------------------------------

bb8 <- project_area |> st_buffer(90000) |> st_bbox()

dy <- bb8[["ymax"]] - bb8[["ymin"]]
dx <- dy/d(a)

mid_x <- (bb8[["xmax"]] + bb8[["xmin"]])/2

bb8[["xmax"]] <- mid_x + (dx/2)
bb8[["xmin"]] <- mid_x - (dx/2)

# shift the map down
bb8[["ymin"]] <- bb8[["ymin"]] + 30000 
bb8[["ymax"]] <- bb8[["ymax"]] + 30000

basemap <- get_basemap(
  bb8,
  map = "imagery", 
  size = c(d(w), d(h))
)

# watershed map -----------------------------------------------------------

cover <- st_sym_difference(
  bb8 |> st_as_sfc(),
  project_area
)

ggplot() +
  annotation_raster(
    basemap,
    bb8[["xmin"]], bb8[["xmax"]],
    bb8[["ymin"]], bb8[["ymax"]]
  ) +
  geom_sf(
    data = cover,
    color = "transparent",
    fill = alpha("white", 0.8)
  ) +
  geom_sf(
    data = watersheds,
    fill = "transparent",
    color = ghc(cinnamon2),
    linewidth = 0.2
  ) +
  geom_sf(
    data = project_area,
    color = ghc(cinnamon5),
    fill = "transparent",
    linewidth = 1.1
  ) +
  geom_sf(
    data = watersheds |> 
      filter(name %in% c("Gypsum Creek", "Kim-me-ni-oli Wash-Chaco River")),
    fill = ghc(cerulean3),
    color = ghc(cerulean5),
    linewidth = 1.1
  ) +
  coord_sf(
    xlim = bb8[c("xmin", "xmax")],
    ylim = bb8[c("ymin", "ymax")],
    crs = st_crs(watersheds)$epsg,
    datum = NA,
    expand = FALSE
  ) +
  theme_void()

ggsave(
  here("_misc", "unit-of-analysis.png"),
  width = 10/d(a),
  height = 10,
  dpi = 300
)

chaco_profile <- profiles |> 
  pivot_longer(
    c(farm_rooms, greathouse_rooms),
    names_to = "type",
    values_to = "count"
  ) |> 
  group_by(type) |> 
  mutate(relative_rate = count/((sum(count)/sum(area_km2))*area_km2)) |> 
  ungroup() |> 
  filter(watershed == "1408010606") |> 
  select(type, start, relative_rate) |>
  arrange(type, start) |> 
  (\(x)
   bind_rows(
     x |> mutate(start = lead(start)),
     x,
     .id = "sauce"
  ))() |>
  arrange(type, start)

type_labels <- tibble(
  x = 1400,
  y = c(0.63, 124),
  label = c("Farm", "Great House"),
  type = c("farm_rooms", "greathouse_rooms")
)

ggplot(chaco_profile) + 
  geom_ribbon(
    aes(start, ymin = 0, ymax = relative_rate, fill = type, color = type)
  ) +
  geom_text(
    data = type_labels,
    aes(x, y, label = label, color = type),
    hjust = 1,
    size = 13/.pt
  ) +
  facet_wrap(vars(type), nrow = 4, scale = "free_y") +
  scale_color_manual(
    name = NULL,
    values = ghc(cerulean4, cinnamon4)
  ) +
  scale_fill_manual(
    name = NULL,
    values = alpha(ghc(cerulean4, cinnamon4), 0.5)
  ) +
  labs(
    x = "Years (CE)",
    y = "Relative Population Rate"
  ) +
  scale_x_continuous(
    limits = c(rng(origin), rng(terminus)),
    expand = expansion()
  ) +
  theme(
    axis.ticks.y = element_blank(),
    axis.title = element_text(size = rel(1.1), lineheight = 1),
    legend.position = "none",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "white"),
    strip.background = element_blank(),
    strip.text = element_blank()
  )

ggsave(
  here("_misc", "chaco-profile.png"),
  width = 5,
  height = 4,
  dpi = 300
)

# chaco world -------------------------------------------------------------

greathouses_sf <- watersheds |> 
  st_centroid() |> 
  semi_join(
    greathouses |> group_by(watershed) |> summarize(),
    by = "watershed"
  )

chaco <- watersheds |> filter(name == "Kim-me-ni-oli Wash-Chaco River") 

innie <- st_sym_difference(
  chaco,
  chaco |> st_buffer(-2000)
)

chaco_xy <- chaco |> st_centroid() |> st_coordinates() |> c()
chaco_xy <- chaco_xy + c(65000, 18000)

bob <- ggplot() +
  annotation_raster(
    basemap,
    bb8[["xmin"]], bb8[["xmax"]],
    bb8[["ymin"]], bb8[["ymax"]]
  ) +
  geom_sf(
    data = cover,
    color = "transparent",
    fill = alpha("white", 0.7)
  ) +
  geom_sf(
    data = project_area,
    color = "black",
    fill = "transparent"
  ) +
  geom_sf(
    data = innie,
    fill = "white",
    color = "transparent",
    alpha = 0.8
  ) +
  geom_sf(
    data = chaco,
    fill = "transparent", 
    color = "black",
    linewidth = 0.35
  )

bob +
  geom_label(
    aes(chaco_xy[1], chaco_xy[2], label = "Chaco Canyon"),
    label.padding = unit(0.4, "lines"),
    size = 12/.pt
  ) + 
  coord_sf(
    xlim = bb8[c("xmin", "xmax")],
    ylim = bb8[c("ymin", "ymax")],
    crs = st_crs(watersheds)$epsg,
    datum = NA,
    expand = FALSE
  ) +
  theme_void()

ggsave(
  here("figures", "chaco-world.png"),
  width = 10/d(a),
  height = 10,
  dpi = 300
)

# chaco great houses ------------------------------------------------------

bob +
  with_outer_glow(
    geom_sf(
      data = greathouses_sf,
      shape = 21,
      fill = "white",
      color = "black",
      size = 2.8,
      stroke = 0.8
    ),
    colour = "white",
    sigma = 2,
    expand = 4
  ) +
  geom_label(
    aes(chaco_xy[1], chaco_xy[2], label = "Chaco Canyon"),
    label.padding = unit(0.5, "lines"),
    size = 12/.pt
  ) + 
  coord_sf(
    xlim = bb8[c("xmin", "xmax")],
    ylim = bb8[c("ymin", "ymax")],
    crs = st_crs(watersheds)$epsg,
    datum = NA,
    expand = FALSE
  ) +
  theme_void()

ggsave(
  here("figures", "chaco-world-great-houses.png"),
  width = 10/d(a),
  height = 10,
  dpi = 300
)

# disconnect! -------------------------------------------------------------

dbDisconnect(dbc)
