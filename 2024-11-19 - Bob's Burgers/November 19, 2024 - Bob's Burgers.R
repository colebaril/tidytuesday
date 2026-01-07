require(pacman)
p_load(tidyverse, tidytuesdayR, extrafont)

tuesdata <- tidytuesdayR::tt_load(2024, week = 47)

episode_metrics <- tuesdata$episode_metrics |> 
  



episode_metrics |> 
  ggplot(aes(x = episode, y = avg_length)) +
  geom_line(linewidth = 1) +
  facet_wrap(~season, labeller = label_both) +
  theme_bw(base_size = 14, base_family = "Courier New") +
  labs(
    x = "Episode Number",
    y = "Average Dialogue Length",
    color = "Sentence Type"
    # title = title_text,
    # subtitle = subtitle_text,
    # caption = caption_text
  ) +
  theme(
    plot.background = element_rect(fill = "grey99", color = NA),
    plot.title = element_text(face = "bold", size = 25),
    plot.subtitle = element_text(size = 15),
    plot.caption = element_text(hjust = 0.5, size = 12),
    plot.margin = margin(10, 10, 10, 10)
  )



