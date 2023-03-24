
##############################.
#     Table of Contents
# 01. Preamble
# 02. Data 
# 03. Relative Population Estimates
# 04. Overview Map
# 05. Disconnect from DB
##############################.

# preamble ----------------------------------------------------------------

library(DBI)
library(ggrepel)
library(here)
library(patchwork)
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

greathouses <- dbReadTable(dbc, "great-houses") |> 
  as_tibble() |> 
  mutate(huc4 = substr(watershed, 1, 4))

profiles <- dbReadTable(dbc, "profiles") |> as_tibble()

subregions <- watersheds |> 
  mutate(huc4 = substr(watershed, 1, 4)) |> 
  group_by(huc4) |> 
  summarize(subregion = unique(subregion)) |> 
  mutate(
    sr_area = st_area(geometry),
    sr_area = units::set_units(sr_area, km^2),
    sr_area = units::drop_units(sr_area)
  ) |> 
  ungroup() |> 
  st_cast() |> 
  st_make_valid()

# construction dates ------------------------------------------------------

ggplot(greathouses) + 
  geom_histogram(
    aes(start+25), # the interval is inclusive to the right, not left, so shift right
    binwidth = 25, 
    center = 12.5, 
    fill = alpha(ghc(cinnamon5), 0.4), 
    color = ghc(cinnamon5)
  ) + 
  annotate(
    "text",
    x = 1400,
    y = 58,
    label = paste0("N = ", nrow(greathouses)),
    hjust = 1, 
    vjust = 1,
    size = 14/.pt
  ) +
  labs(
    x = "Construction Dates (CE)", 
    y = "Count", 
    title = NULL
  ) + 
  scale_x_continuous(limits = c(700, 1400)) +
  scale_y_continuous(
    breaks = seq(0, 60, by = 20), 
    expand = expansion()
  ) +
  theme(
    panel.grid.minor = element_blank(), 
    panel.grid.major.x = element_blank()
  )

ggsave(
  here("figures", "construction-dates.png"),
  width = 7,
  height = 3.8,
  dpi = 300
)

# relative population estimates -------------------------------------------

relative_rate <- profiles |> 
  mutate(huc4 = substr(watershed, 1, 4)) |> 
  group_by(huc4, start) |> 
  summarize(
    area_km2 = sum(area_km2),
    farm_rooms = sum(farm_rooms, na.rm = TRUE)) |> 
  ungroup() |> 
  arrange(huc4, start) |> 
  left_join(subregions |> st_drop_geometry(), by = "huc4") |> 
  mutate(relative_rate = farm_rooms/((sum(farm_rooms)/sum(area_km2))*area_km2))

gh <- greathouses |> 
  left_join(relative_rate, by = c("huc4", "start")) |> 
  select(huc4, subregion, start, relative_rate) |> 
  group_by(subregion, start) |> 
  mutate(
    relative_rate = if (n() > 1) relative_rate + (0.06 * (row_number() - 1)) else relative_rate
  ) |> 
  ungroup()

xypoint <- gh |> 
  filter(subregion == "Little Colorado") |> 
  slice_min(start) |> 
  select(start, relative_rate, subregion) |> 
  mutate(x = start + 12.5, y = relative_rate)

xyline <- xypoint |> 
  slice(c(1,1,1)) |> 
  mutate(
    x = x + c(-15, 0, 0),
    y = y + c(2.4, 2.4, 0)
  )

xytext <- xyline |> 
  slice(1) |> 
  mutate(
    x = x - 5, 
    label = "Construction Date"
  )

subregion_labels <- subregions |> 
  st_drop_geometry() |> 
  select(subregion) |> 
  mutate(x = 1400, y = 5.2)

bob <- relative_rate |> 
  mutate(start = lead(start)) |> 
  group_by(subregion) |> 
  slice_head(n=-1) |> 
  ungroup() |> 
  bind_rows(relative_rate, .id = "source") |> 
  select(subregion, start, relative_rate, source) |> 
  arrange(subregion, start, source) |> 
  ggplot() + 
  geom_ribbon(
    aes(start, ymin = 0, ymax = relative_rate, fill = subregion, color = subregion)
  ) +
  geom_point(
    data = gh,
    aes(start + 12.5, relative_rate, color = subregion),
    alpha = 0.5,
    size = 2
  ) +
  facet_wrap(vars(subregion), nrow = 4) +
  scale_color_manual(
    name = NULL,
    values = ghc(cerulean5, cerulean3, cinnamon3, cinnamon5)
  ) +
  scale_fill_manual(
    name = NULL,
    values = alpha(ghc(cerulean5, cerulean3, cinnamon3, cinnamon5), 0.1)
  ) +
  geom_line(data = xyline, aes(x, y)) +
  geom_point(
    data = xypoint, 
    aes(x, y),
    shape = 21,
    fill = "white",
    color = "black",
    size = 4
  ) +
  geom_point(
    data = xypoint, 
    aes(x, y, color = subregion, fill = subregion),
    size = 2
  ) +
  geom_text(
    data = xytext,
    aes(x, y, label = label),
    hjust = 1,
    size = 12/.pt
  ) + 
  geom_text(
    data = subregion_labels,
    aes(x, y, label = subregion, color = subregion),
    hjust = 1,
    vjust = 0,
    size = 14/.pt
  ) +
  labs(
    x = "Years (CE)",
    y = "Relative Rate of Local Farmers\n(ratio of observed to expected room counts)"
  ) +
  scale_x_continuous(expand = expansion()) +
  scale_y_continuous(limits = c(0, 5.5), breaks = 0:5) +
  theme(
    axis.ticks.y = element_blank(),
    axis.title = element_text(size = rel(1.1), lineheight = 1),
    legend.position = "none",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.margin = margin(r = 10),
    strip.background = element_blank(),
    strip.text = element_blank()
  )

remove(relative_rate, gh, xypoint, xyline, xytext, subregion_labels)

# subregion map -----------------------------------------------------------

inner_glow <- st_sym_difference(
  subregions,
  subregions |> st_buffer(dist = -5000)
) |> 
  filter(subregion == subregion.1) |> 
  select(subregion) |> 
  st_cast("POLYGON")

bb8 <- subregions |> st_buffer(5000) |> st_bbox()

labels <- tibble(
  x = bb8[c("xmin", "xmin", "xmax", "xmax")] + c(2000, 2000, -2000, -2000),
  y = bb8[c("ymin", "ymax", "ymax", "ymin")] + c(3000, -3000, -3000, 3000),
  hjust = c(0, 0, 1, 1),
  vjust = c(0, 1, 1, 0),
  labels = c("AZ", "UT", "CO", "NM")
) |> st_as_sf(coords = c("x", "y"), crs = 26912)

greathouses_sf <- watersheds |> 
  st_centroid() |> 
  semi_join(
    greathouses |> group_by(watershed) |> summarize(huc4 = unique(huc4)),
    by = "watershed"
  )

chaco <- greathouses_sf |> filter(watershed == "1408010606")
chaco_xy <- chaco |> st_coordinates() |> c()

tom <- ggplot() +
  geom_sf(
    data = four_corners,
    fill = "transparent"
  ) +
  geom_sf_text(
    data = labels,
    aes(hjust = hjust, vjust = vjust, label = labels),
    size = 14/.pt
  ) +
  geom_sf(
    data = subregions,
    aes(fill = subregion),
    color = "transparent",
    alpha = 0.1
  ) +
  geom_sf(
    data = subregions,
    aes(color = subregion),
    fill = "transparent"
  ) +
  geom_sf(
    data = inner_glow,
    aes(fill = subregion),
    color = "transparent",
    alpha = 0.3
  ) +
  scale_color_manual(
    values = ghc(cerulean5, cerulean3, cinnamon3, cinnamon5)
  ) +
  scale_fill_manual(
    values = ghc(cerulean5, cerulean3, cinnamon3, cinnamon5), 
  ) +
  geom_sf(
    data = greathouses_sf,
    color = "white",
    fill = "transparent",
    shape = 21,
    size = 3,
    stroke = 1,
    alpha = 0.5
  ) +
  geom_sf(
    data = greathouses_sf,
    aes(fill = subregion),
    color = "transparent",
    alpha = 0.5,
    shape = 21,
    size = 2.3
  ) +
  geom_sf(
    data = greathouses_sf,
    aes(color = subregion),
    fill = "transparent",
    shape = 21,
    size = 2.3
  ) +
  geom_label_repel(
    aes(chaco_xy[1], chaco_xy[2], label = "Chaco Canyon"),
    nudge_x = 85000,
    nudge_y = 38000,
    label.padding = unit(0.3, "lines"),
    size = 12/.pt
  ) +
  geom_sf(
    data = chaco |> st_buffer(7000),
    fill = "white", 
    color = "black",
    linewidth = 0.5
  ) +
  geom_sf(
    data = chaco,
    aes(fill = subregion),
    color = "transparent",
    alpha = 0.5,
    shape = 21,
    size = 2.3
  ) +
  geom_sf(
    data = chaco,
    aes(color = subregion),
    fill = "transparent",
    shape = 21,
    size = 2.3
  ) + 
  scale_x_continuous(breaks = seq(0, 1500, by = 200)) +
  coord_sf(
    xlim = bb8[c("xmin", "xmax")],
    ylim = bb8[c("ymin", "ymax")], 
    expand = FALSE
  ) +
  theme_void() +
  theme(
    plot.margin = margin(l = 10),
    legend.position = "none"
  )


# combine figures ---------------------------------------------------------

bob + tom

ggsave(
  here("figures", "subregion-profiles.png"),
  width = 12,
  height = 7.5,
  dpi = 300
)

remove(inner_glow, bb8, greathouses_sf, chaco, chaco_xy, labels)

# disconnect! -------------------------------------------------------------

dbDisconnect(dbc)
