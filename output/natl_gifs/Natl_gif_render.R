library(here)
library(tidyverse)
library(janitor)
library(gridExtra)
library(readr)
library(data.table)
library(openxlsx)


county_short <- readRDS(here("data/temp/county_short.RDS"))

# One of subcategories
county_cats <-  county_short %>% 
  filter(!is.na(large_category) & retain_total & large_category != "Total") %>% 
  group_by(year, fips, large_category, Population, fips_state) %>% 
  summarise(amount = sum(amount, na.rm = TRUE)) %>% 
  mutate(amount_pc = amount/Population)

library(usmap)
library(viridis)
library(gifski)
library(gganimate)
library(transformr)

anim_map <- function(df, cat){
  p <- df %>% 
    filter(year %in% seq(2002,2012,5) & will_category == cat) %>% 
    mutate(amount_pc = ifelse(amount_pc == 0, NA, log(amount_pc*1000))) %>% 
    plot_usmap(data = ., values = "amount_pc", regions = "counties", col = "gray90", linewidth = 0.01, exclude = c("HI", "AK")) + 
    scale_fill_viridis() +
    theme(panel.background = element_rect(color = "white", fill = "white"),
          plot.title = element_text(face = "bold"), legend.background=element_blank()) +
    labs(title = cat) +
    transition_states(year)
  return(p)
}

for(el in unique(county_cats$large_category)){
  print(el)
  anim_map(county_cats, el) %>% anim_save(here(paste0("output/natl_gifs/natl_", gsub(" ", "_", gsub("[()]", "", el)), ".gif")), .)
}


anim_map <- function(df, cat){
  p <- df %>% 
    filter(year %in% seq(2002,2012,5) & will_category == cat) %>% 
    mutate(amount_pc = ifelse(amount_pc == 0, NA, log(amount_pc*1000))) %>% 
    plot_usmap(data = ., values = "amount_pc", regions = "counties", col = "gray90", linewidth = 0.01, exclude = c("HI", "AK")) + 
    scale_fill_distiller(palette = "YlOrRd", name = "Log PC Expenditure", direction = 1, na.value = "gray90") +
    theme(panel.background = element_rect(color = "white", fill = "white"),
          plot.title = element_text(face = "bold", size = 14), plot.subtitle = element_text(face = "bold", size = 14), legend.background=element_blank()) +
    labs(title = cat, subtitle = "Year: {closest_state}") +
    transition_states(year)
  return(p)
}

for(el in c("total expenditure", "total direct expenditure", "direct expenditure", "general expenditure", "total current expenditure")){
 print(el)
 anim_map(county_totals, el) %>% anim_save(here(paste0("output/natl_gifs/natl_", gsub(" ", "_", gsub("[()]", "", el)), ".gif")), .)
}

tot_gif <- anim_map(county_totals, "total expenditure")

animate(tot_gif, width = 900, height = 800)
anim_save(here("output/natl_gifs/natl_total_expenditure_nolog_orange_yll_red.gif"))

# 
# anim_map_full <- function(df, cat){
#   p <- df %>% 
#     filter(will_category == cat) %>% 
#     mutate(amount_pc = ifelse(amount_pc == 0, NA, log(amount_pc*1000))) %>% 
#     plot_usmap(data = ., values = "amount_pc", regions = "counties", col = "gray90", linewidth = 0.01, exclude = c("HI", "AK")) + 
#     scale_fill_distiller(palette = "YlOrRd", name = "Log PC Expenditure", direction = 1, na.value = "gray90") +
#     theme(panel.background = element_rect(color = "white", fill = "white"),
#           plot.title = element_text(face = "bold"), legend.background=element_blank()) +
#     labs(title = cat, subtitle = "Year: {closest_state}") +
#     transition_states(year)
#   return(p)
# }
# 
# 
# tot_gif <- anim_map_full(county_totals, "total expenditure")
# 
# animate(tot_gif, width = 900, height = 800)
# anim_save(here("output/natl_gifs/natl_total_expenditure_orange_allyears.gif"))

  

