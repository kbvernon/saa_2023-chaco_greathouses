
get_basemap <- function(
    x,
    map = "physical",
    size = c(10500, 7000),
    dpi = 900,
    imageSR = 4326
){
  
  x <- sf::st_bbox(x)
  
  old_ratio <- (x[["xmax"]] - x[["xmin"]]) / (x[["ymax"]] - x[["ymin"]])
  new_ratio <- (size[[1]] / size[[2]])
  
  if (!all.equal(old_ratio, new_ratio)) {
    
    msg <- paste0(
      "Extent of image (size) differs from extent of x (bbox). ",
      "Map may be warped."
    )
    
    warning(msg, call. = FALSE)
    
  }
  
  map_name <- switch(
    map,
    "hillshade"= "Elevation/World_Hillshade",
    "dark"     = "Elevation/World_Hillshade_Dark",
    "natgeo"   = "NatGeo_World_Map",
    "usa"      = "USA_Topo_Maps",
    "imagery"  = "World_Imagery",
    "physical" = "World_Physical_Map",
    "shaded"   = "World_Shaded_Relief",
    "street"   = "World_Street_Map",
    "terrain"  = "World_Terrain_Base",
    "topo"     = "World_Topo_map",
    stope(paste0("The ", map, " map is not supported."), call. = FALSE)
  )
  
  path <- tempfile("bob-", fileext = ".png")
  
  httr2::request("http://services.arcgisonline.com/arcgis/rest/services/") |> 
    httr2::req_url_path_append(map_name) |> 
    httr2::req_url_path_append("MapServer") |> 
    httr2::req_url_path_append("export?") |> 
    httr2::req_url_query(
      bbox    = paste(x, collapse = ","),
      bboxSR  = st_crs(x)$epsg,
      imageSR = imageSR,
      f       = "image",
      format  = "png",
      size    = paste(size, collapse = ","),
      dpi     = dpi
    ) |> 
    httr2::req_perform(path = path)
  
  png::readPNG(path)
  
}