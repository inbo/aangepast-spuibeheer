static.map <- function(site){
  esri_ocean <- paste0('https://services.arcgisonline.com/arcgis/rest/services/',
                       'Ocean/World_Ocean_Base/MapServer/tile/${z}/${y}/${x}.jpeg')
  
  data <- read.csv("./data/metadata/coordinaten_ctd.csv", sep=";") %>%
    separate("X..Y", into = c("X", "Y"), sep = ",") %>%
    dplyr::mutate(X=as.numeric(X),
                  Y=as.numeric(Y)) %>%
    sf::st_as_sf(
      coords = c("Y","X"),
      crs = 4326
    )
  
  # Conditional filtering logic
  if (length(site) > 1 | site[1] != "all") {
    data <- data %>%
      dplyr::filter(site %in% !!site)
  }
  
  map <- ggplot(data) +
    annotation_map_tile(zoomin = -1, type=esri_ocean, cachedir = "./data/intern/") +
    geom_sf(aes(fill=site,colour=site), size=5) +
    geom_label_repel(
      aes(label = loc, geometry = geometry),
      size = 5,
      fontface = "bold",
      color = "gray50",
      stat = "sf_coordinates"
    ) +
    annotation_scale(location = "br", width_hint = 0.5, size=5) +
    annotation_north_arrow(location = "tl", which_north = "true",
                           style = north_arrow_fancy_orienteering, size=5) +
    theme(
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.background = element_blank(),
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.text = element_text(size = 12)
    )
  
  return(map)
}