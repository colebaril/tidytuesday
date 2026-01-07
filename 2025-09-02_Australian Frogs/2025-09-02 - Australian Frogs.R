require(pacman)
p_load(tidytuesdayR, tidyverse, janitor, tidytext, extrafont, slider, here, TTR, sf, rnaturalearth, rnaturalearthdata, patchwork, hexbin)

tuesdata <- tidytuesdayR::tt_load('2025-09-02')
frogs <- tuesdata$frogID_data
frog_names <- tuesdata$frog_names

frogs <- frogs |>
  clean_names() 
 

frogs_unique <- frogs %>%
  distinct(decimal_longitude, decimal_latitude, scientific_name)


aus_states <- ne_states(country = "australia", returnclass = "sf") |> 
  filter(name != "Macquarie Island")

ggplot(frogs_unique, aes(x = decimal_longitude, y = decimal_latitude)) +
  stat_summary_hex(
    aes(z = as.numeric(factor(scientific_name))),
    fun = function(x) length(unique(x)),
    bins = 50
  ) +
  geom_sf(data = aus_states, fill = NA, colour = "black", size = 0.5, inherit.aes = FALSE) +
  coord_sf() +
  scale_fill_viridis_c(option = "plasma") +
  labs(
    title = "Australian Frog Species Richness",
    subtitle = "Number of unique frog species recorded per hexagon across Australia in 2023
    ",
    caption = "Data: Australian Society of Herpetologists",
    fill = "Species Richness"
  ) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "grey99", color = NA),
    plot.title = element_text(face = "bold", size = 25, hjust = 0.5),
    plot.subtitle = element_text(size = 15, hjust = 0.5),
    plot.caption = element_text(size = 12),
    legend.position = "top",
    legend.key.width = unit(3, "lines"),
    legend.title = element_text(face = "bold", size = 15, hjust = 0.5),
    legend.title.position = "top",
    legend.text = element_text(size = 12),
    plot.margin = margin(10, 10, 10, 10)
  )

ggsave(here("2025-09-02 - Australian Frogs/Australian Frog Species Richness.png"), plot = last_plot(), width = 15, height = 12, dpi = 300)
