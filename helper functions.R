# Tidy Tuesday Helper Functions 

require(pacman)
p_load(tidyverse, palmerpenguins, extrafont, ggtext)

# Plot Theme 

theme_tidytuesday <- 
  theme_bw(base_size = 12, 
           base_family = "Courier New", 
           paper = "cornsilk", ink = "black"
           ) +
  theme(
  plot.title = element_text(face = "bold", size = 25, hjust = 0.5),
  plot.subtitle = element_text(size = 15, hjust = 0.5),
  plot.caption = element_text(size = 12),
  legend.position = "top",
  legend.key.width = unit(3, "lines"),
  legend.title = element_text(face = "bold", size = 15, hjust = 0.5),
  legend.title.position = "top",
  legend.text = element_text(size = 12)
  )

labs_tidytuesday <- labs(title = "test test",
                         subtitle = "test test test",
                         caption = "test")


penguins |> 
  ggplot(aes(x = sex, group = factor(year))) +
  geom_bar(aes(fill = factor(year))) +
  scale_fill_viridis_d("Year") +
  theme_tidytuesday +
  labs_tidytuesday +
  theme(legend.text.position = "top") 
        
