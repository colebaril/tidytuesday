require(pacman)
p_load(tidytuesdayR, tidyverse, janitor, tidytext, extrafont, slider)

options(scipen = 999)

tuesdata <- tidytuesdayR::tt_load('2025-07-01')

weekly_gas_prices <- tuesdata$weekly_gas_prices



vol_df <- weekly_gas_prices |> 
  filter(grade == "all") |> 
  mutate(fuel = str_to_title(fuel)) |> 
  arrange(fuel, date) |> 

  mutate(
    log_return = log(price / lag(price)),
    volatility_52w = slide_dbl(log_return, sd, .before = 51, .complete = TRUE),
    .by = fuel
  ) |> 

  group_by(fuel) |> 
  mutate(volatility = sd(log_return, na.rm = TRUE)) |> 
  ungroup()

events <- tibble(
  event = c("2008 Financial Crisis", "2014 Oil Crash", "COVID-19 Pandemic"),
  start_date = as.Date(c("2008-09-01", "2014-06-01", "2020-03-01")),
  end_date = as.Date(c("2009-06-01", "2016-06-01", "2021-06-01"))
)

ggplot(vol_df, aes(x = date, y = volatility_52w, color = fuel)) +
  geom_line(na.rm = TRUE, size = 1) +
  scale_x_date(
    breaks = as.Date(c("1995-01-01", "2000-01-01", "2005-01-01", "2010-01-01", 
                       "2015-01-01", "2020-01-01", "2025-01-01")),
    date_labels = "%Y"
  ) +
  
  # Add shaded event periods
  geom_rect(data = events, inherit.aes = FALSE,
            aes(xmin = start_date, xmax = end_date, ymin = 0, ymax = Inf),
            fill = "grey80", alpha = 0.4) +
  
  # Add event labels (optional, adjust y position as needed)
  geom_text(
    data = events, inherit.aes = FALSE,
    aes(
      x = start_date + (end_date - start_date)/2,
      y = -0.005,  # Adjust lower if needed
      label = event
    ),
    angle = 0,  # Horizontal text
    vjust = 1, hjust = 0.5, size = 5, color = "black", fontface = "bold"
  ) +
  
  
  labs(
    title = "52-Week Rolling Volatility of Weekly U.S. Gas Prices",
    x = "Date",
    y = "Gasoline Price Volatility (Log Return SD)",
    caption = "Data: U.S. Energy Information Administration",
    color = "Fuel Type"
  ) +
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

ggsave("gasoline_rolling_volatility.png", plot = last_plot(), width = 16, height = 10)


