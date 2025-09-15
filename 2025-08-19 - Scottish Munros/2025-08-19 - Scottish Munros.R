require(pacman)
p_load(tidytuesdayR, tidyverse, janitor, tidytext, extrafont, slider, here, TTR, sf, rnaturalearth, rnaturalearthdata, patchwork)

tuesdata <- tidytuesdayR::tt_load('2025-08-19')
scottish_munros <- tuesdata$scottish_munros


# Get UK countries (scotland, england, etc.) from Natural Earth
uk <- ne_countries(country = "United Kingdom", scale = "medium", returnclass = "sf")

# Transform UK shapefile to British National Grid
uk_bng <- st_transform(uk, crs = 27700)


# Get rivers and lakes
rivers <- ne_download(scale = "medium", type = "rivers_lake_centerlines", category = "physical", returnclass = "sf")
lakes <- ne_download(scale = "medium", type = "lakes", category = "physical", returnclass = "sf")
rivers_bng <- st_transform(rivers, crs = 27700)
lakes_bng <- st_transform(lakes, crs = 27700)

# Get major cities
cities <- ne_download(scale = "medium", type = "populated_places", category = "cultural", returnclass = "sf")
cities_bng <- st_transform(cities, crs = 27700)

# Make sf object with British National Grid CRS
df_sf <- st_as_sf(scottish_munros |> drop_na(xcoord, ycoord), coords = c("xcoord", "ycoord"), crs = 27700)

density <- ggplot() +
  # Lakes
  geom_sf(data = lakes_bng, fill = "lightblue", color = NA) +
  
  # Rivers
  geom_sf(data = rivers_bng, color = "blue", size = 0.5) +
  
  # Country borders
  geom_sf(data = uk_bng, fill = NA, color = "black", size = 0.7) +
  
  # 2D density heatmap (semi-transparent)
  stat_density_2d(
    data = df_sf,
    aes(x = st_coordinates(df_sf)[,1], y = st_coordinates(df_sf)[,2], fill = ..level.. * 1e9),
    geom = "polygon",
    alpha = 0.4,
    color = NA,
    h = 40000
  ) +
  scale_fill_viridis_c(option = "magma", name = "2D Kernel Density (x1e9)") +

  
  # Zoom to Scotland
  coord_sf(crs = 27700,
           xlim = c(0, 470000),
           ylim = c(530000, 1200000)) +
  
  # Add plot border
  theme_void(base_family = "Courier New") +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    panel.background = element_rect(fill = "white"),
    legend.position = "bottom"
  ) 


points <- ggplot() +
  # Lakes
  geom_sf(data = lakes_bng, fill = "lightblue", color = NA) +
  
  # Rivers
  geom_sf(data = rivers_bng, color = "blue", size = 0.5) +
  
  # Country borders
  geom_sf(data = uk_bng, fill = NA, color = "black", size = 0.7) +
  
  scale_fill_viridis_c(option = "magma", name = "Density") +
  
  # Points colored by height
  geom_sf(
    data = df_sf,
    aes(color = Height_m, size = Height_m),
    alpha = 0.8
  ) +
  scale_color_viridis_c(option = "plasma", name = "Height (m)") +
  scale_size_continuous(range = c(1,5), guide = "none") +
  
  # Zoom to Scotland
  coord_sf(crs = 27700,
           xlim = c(0, 470000),
           ylim = c(530000, 1200000)) +
  
  # Add plot border
  theme_void(base_family = "Courier New") +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    panel.background = element_rect(fill = "white"),
    legend.position = "bottom",
    legend.key.size = unit(1.5, "lines")
  ) 

points + density +
  plot_annotation(
    title = "Scottish Munros",
    subtitle = "Scottish Munros are largely clustered around three central points; the centre of each cluster has the tallest munros"
  ) &
  theme(text = element_text('Courier New'),
        plot.title = element_text(face = "bold", hjust = 0.5, size = 20),
        plot.subtitle = element_text(hjust = 0.5, size = 14))

ggsave(here("2025-08-19 - Scottish Munros/Scottish Munros.png"), plot = last_plot(), width = 15, height = 12, dpi = 300)
