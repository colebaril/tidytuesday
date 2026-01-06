require(pacman)
p_load(spellbook, tidytuesdayR, tidyverse, janitor, tidytext, extrafont, here, 
       sf, rnaturalearth, rnaturalearthdata, patchwork, ggflags, gt, gtExtras, gridExtra,
       cowplot)

tuesdata <- tidytuesdayR::tt_load('2025-09-09')

country_lists <- tuesdata$country_lists
rank_by_year <- tuesdata$rank_by_year

world <- rnaturalearth::ne_countries(scale = 50) |> 
  filter(admin != "Antarctica") |> 
  select(admin, iso_a2) |> 
  left_join(rank_by_year, by = c("iso_a2" = "code"))


t <- world |> 
  st_drop_geometry() |> 
  filter(!year %in% c("2007", "2009")) |>
  select(iso_a2, country, visa_free_count, year) |> 
  arrange(year, .by = country) |> 
  reframe(visa_free_count_list = list(visa_free_count),
          .by = country) |> 
  drop_na(country)
  
gt_high <- world |> 
  st_drop_geometry() |> 
  filter(year %in% c("2025", "2024")) |>
  select(iso_a2, country, visa_free_count, year) |> 
  pivot_wider(names_from = year, values_from = visa_free_count) |> 
  relocate(`2025`, .before = `2024`) |> 
  mutate(change = `2025` - `2024`) |> 
  left_join(t, by = "country") |> 
  arrange(desc(`2025`)) |> 
  head(10) |> 
  gt() |> 
  fmt_flag(columns = iso_a2) |> 
  cols_add(dir = ifelse(`2024` < `2025`, "arrow-up", "arrow-down")) |> 
  fmt_icon(
    columns = dir,
    fill_color = c("arrow-up" = "green", "arrow-down" = "red")
  ) |> 
  cols_hide(c(`2025`, `2024`)) |>
  cols_move(dir, after = change) |> 
  gt_plt_sparkline(column = visa_free_count_list,
                   fig_dim = c(20, 30)) |> 
  cols_label(iso_a2 = "",
             dir = "",
             country = "Country",
             change = "Annual Change (2024-2025)",
             visa_free_count_list = "Visa Free Count over Time") |> 
  cols_align(columns = 5, align = "center") 

# Convert gt table to grob
tbl_high_grob <- gt::gtsave(gt_high, "temp.png",  vwidth = 1200,vheight = 400)  
tbl_high_img  <- png::readPNG("temp.png")
tbl_high_grob <- grid::rasterGrob(tbl_high_img, interpolate = TRUE)

world |> 
  filter(year == "2025") |> 
  ggplot() +
  geom_sf(aes(fill = visa_free_count), colour = "white", linewidth = 0.2) +
  annotation_custom(tbl_high_grob, xmin = -200, xmax = -100, ymin = -60, ymax = 60) +
  scale_fill_viridis_c(end = 0.9) +
  
  theme_void() +
  theme_parchment() +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        # axis.text = element_blank(),
        axis.ticks = element_blank()
        )

