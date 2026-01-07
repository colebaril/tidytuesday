require(pacman)
p_load(tidyverse, tidytuesdayR, extrafont)

tuesdata <- tidytuesdayR::tt_load(2024, week = 48)

cbp_resp <- tuesdata$cbp_resp
cbp_state <- tuesdata$cbp_state