require(pacman)
p_load(tidytuesdayR, tidyverse, janitor, tidytext, extrafont, slider, here, TTR, patchwork)

tuesdata <- tidytuesdayR::tt_load('2025-08-26')
billboard <- tuesdata$billboard
topics <- tuesdata$topics

billboard |> 
  mutate(year = year(date)) |> 
  filter(year != "2025") |> 
  reframe(prop_explicit = mean(explicit),
          .by = year) |> 
  ggplot(aes(x = year, y = prop_explicit)) +
  geom_line() +
  geom_point() +
  labs(y = "Proportion Explicit",
       x = "Year",
       title = "Proportion of Explicit Billboard Hot 100 Number One Songs") +
  theme_bw(base_size = 15)

# Multiples 

billboard_10 <- billboard |> 
  mutate(year = year(date)) |> 
  filter(year != "2025") |> 
  mutate(decade = floor(year / 10) * 10) |> 
  reframe(total = n(),
          explicit = sum(explicit),
          prop_explicit = mean(explicit),
          .by = c("decade", "discogs_style", "artist_place_of_origin"))

top_contributors <- billboard_10 %>%
  group_by(decade) %>%
  slice_max(explicit, n = 3)

ggplot(billboard_10, aes(x = reorder(discogs_style, -prop_explicit), y = prop_explicit, fill = artist_place_of_origin)) +
  geom_col() +
  facet_wrap(~ decade, scales = "free_x") +
  coord_flip() +
  labs(
    title = "Explicit #1 Songs by Decade",
    y = "Proportion Explicit",
    x = "Genre (ordered by explicitness)"
  )
