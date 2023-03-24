
# R preamble --------------------------------------------------------------

library(here)
library(maps)
library(sf)
library(tidyverse)
library(tigris)

# load custom functions
# doing it this way to mask the functions in the global environment
sys.source(
  here("R", "defaults.R"), # requires httr and jsonlite
  envir = attach(NULL, name = "defaults")
)

# data --------------------------------------------------------------------

exclude <- c("MP", "AK", "HI", "VI", "PR", "AS", "GU")

us_states <- states() |> 
  filter(!(STUSPS %in% exclude)) |> 
  st_union()

metros <- here("data", "uscities.csv") |>
  read_csv() |>
  st_as_sf(coords = c("lng", "lat"), crs = 4326) |>
  filter(!(state_id %in% exclude))


# map ---------------------------------------------------------------------

ggplot() +
  geom_sf(data = us_states, fill = "white") +
  geom_sf(
    data = metros, 
    color = ghc(cerulean5),
    size = 0.5,
    alpha = 0.2
  ) +
  coord_sf(crs = 5070, datum = NA, expand = FALSE) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.margin = margin()
  )

ggsave(
  here("figures", "us-cities.png"),
  width = 10,
  height = 10 * d(a),
  dpi = 300
)

