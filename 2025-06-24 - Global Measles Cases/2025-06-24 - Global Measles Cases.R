require(pacman)
p_load(tidytuesdayR, tidyverse, janitor, tidytext, extrafont)

options(scipen = 999)

tuesdata <- tidytuesdayR::tt_load('2025-06-24')

cases_month <- tuesdata$cases_month
cases_year <- tuesdata$cases_year

# North America ----

cases_year |> 
  reframe(Measles = (sum(measles_total) / total_population) * 100000,
          Rubella = (sum(rubella_total) / total_population) * 100000,
          .by = c("country", "year")) |> 
  pivot_longer(!country & !year, names_to = "disease", values_to = "per_100") |> 
  filter(country %in% c("Canada", "United States of America", "Mexico")) |> 
  ggplot(aes(x = year, y = per_100, colour = disease)) +
  geom_point(size = 4) +
  geom_line(size = 2) +
  facet_wrap(~country, ncol = 1, nrow = 3, scales = "free") +
  scale_y_continuous(limits = c(0, 6)) +
  scale_colour_viridis_d("Virus", end = 0.8) + 
  labs(title = "From Elimination to Escalation: Canada’s Measles Comeback",
       x = "Year",
       y = "Cases per 100,000 population",
       caption = "Data: World Health Organisation") +
  theme_bw(base_size = 20, base_family = "Courier New") +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        strip.text = element_text(face = "bold", size = 14),
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "top",
        legend.title.position = "top",
        legend.title = element_text(hjust = 0.5),
        legend.key.width = unit(2.5, "lines"),
        legend.key.height = unit(1, "lines"),
        legend.background = element_rect(fill = "#f5f5f2"),
        plot.background = element_rect(fill = "#f5f5f2", color = "#f5f5f2"),
        panel.background = element_rect(fill = "#f5f5f2", color = "#f5f5f2"),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))

ggsave("measles_north_america.png", plot = last_plot(), width = 14, height = 10)


# Global ----


global <- cases_year |> 
  reframe(Measles = (sum(measles_total) / total_population) * 100000,
          .by = c("country", "year")) |> 
  pivot_longer(!country & !year, names_to = "disease", values_to = "per_100") |> 
  mutate(country_colour = case_when(country == "Canada" ~ "firebrick3",
                                    country == "United States of America" ~ "cadetblue",
                                    country == "Mexico" ~ "forestgreen",
                                    .default = "grey70"),
         alpha_value = if_else(country_colour == "grey70", 0.4, 1)  
         )  


ggplot() +
  geom_point(data = global |> filter(!country %in% c("Canada", "United States of America", "Mexico")),
             aes(x = year, y = per_100, col = country_colour, alpha = alpha_value, group = country),
             size = 2) +
  geom_line(data = global |> filter(!country %in% c("Canada", "United States of America", "Mexico")),
            aes(x = year, y = per_100, col = country_colour, alpha = alpha_value, group = country),
            size = 1) +
  
  geom_point(data = global |> filter(country %in% c("Canada", "United States of America", "Mexico")),
             aes(x = year, y = per_100, col = country_colour, alpha = alpha_value, group = country),
             size = 4) +
  geom_line(data = global |> filter(country %in% c("Canada", "United States of America", "Mexico")),
            aes(x = year, y = per_100, col = country_colour, alpha = alpha_value, group = country),
            size = 2) +
  
  scale_colour_identity("Country", guide = "legend", labels = c("United States", "Canada", "Mexico", "Other")) +
  scale_alpha_identity() +
  
  scale_y_continuous(trans = "log1p", breaks = c(0, 5, 50, 250, 500, 750, 1000, 1250)) +

  labs(title = "Canada’s Measles Surge in a Global Context",
       subtitle = "Canada's measles cases per 100,000 population among highest of 194 countries",
       x = "Year",
       y = "Cases per 100,000 population (log scale)",
       caption = "Data: World Health Organisation") +
  theme_bw(base_size = 20, base_family = "Courier New") +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "top",
        legend.title.position = "top",
        legend.title = element_text(hjust = 0.5),
        legend.key.width = unit(2.5, "lines"),
        legend.key.height = unit(1, "lines"),
        legend.background = element_rect(fill = "#f5f5f2"),
        plot.background = element_rect(fill = "#f5f5f2", color = "#f5f5f2"),
        panel.background = element_rect(fill = "#f5f5f2", color = "#f5f5f2"),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))

ggsave("measles_global.png", plot = last_plot(), width = 14, height = 10)
  



