require(pacman)
p_load(tidyverse, tidytuesdayR, sf, countrycode, ggmagnify)

tuesdata <- tidytuesdayR::tt_load(2024, week = 46)

countries <- tuesdata$countries
country_subdivisions <- tuesdata$country_subdivisions
former_countries <- tuesdata$former_countries

df <- countries |> 
  left_join(country_subdivisions, by = "alpha_2") |> 
  rename(country_name = name.x,
         subdivision_name = name.y) |> 
  group_by(alpha_3) |> 
  count(alpha_3)

world <- rnaturalearth::ne_countries(scale = 10) %>% 
  janitor::clean_names() |> 
  select(adm0_a3, admin) |> 
  left_join(df, by = c("adm0_a3" = "alpha_3")) |> 
  st_transform(crs = "ESRI:54030")

p <- ggplot(world) +
  geom_sf(aes(fill = n), colour = "white", linewidth = 0.2) +
  scale_fill_viridis_c(end = 0.9) +
  theme_void(base_family = "Courier New") +
  labs(
    title = "Global Distribution of ISO-Recognized Subdivisions",
    subtitle = "The United Kingdom and Slovenia lead in ISO-recognized subdivisions with 221 and 212 \nsubdivisions, respectfully, while 163 Countries have 20 or fewer ISO-recognized subdivisions",
    caption = "Source: ISO 3166-1 via the ISOcodes R package · Graphic: Cole Baril", 
    fill = "Number of Subdivisions"
  ) +
  
  theme(
    legend.position = "top",
    legend.title.position = "top",
    legend.title = element_text(hjust = 0.5, size = 12),
    legend.key.width = unit(2.5, "lines"),
    legend.key.height = unit(1, "lines"),
    legend.margin = margin(20, 0, 20, 0),
    plot.background = element_rect(fill = "grey99", color = NA),
    plot.title = element_text(face = "bold", size = 25),
    plot.subtitle = element_text(size = 15),
    plot.caption = element_text(hjust = 0.5, size = 12),
    plot.margin = margin(10, 10, 10, 10)
  ) +
  ggmagnify::geom_magnify(
    aes(from = admin == "Slovenia"), 
    to = c(-4.5e6, -2.5e6, 2e6, -0e6),  # Adjusted bounding box for Slovenia
    aspect = "fixed", 
    linewidth = 0.1
  ) +
  geom_text(
    data = NULL, 
    aes(x = -3.5e6, y = 1.8e6, label = "Slovenia: 212"), 
    family = "Courier New", 
    size = 3
  )

ggsave("ISO Codes.png", p, width = 12, height = 10, dpi = 300)








