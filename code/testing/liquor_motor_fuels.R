# Scoping motor fuels taxes and liquor store revenues

library(here)
library(tidyverse)
library(janitor)
library(gridExtra)
library(readr)
library(data.table)
library(openxlsx)
library(usmap)
library(gets)
library(getspanel)
library(tidycensus)

data(fips_codes)
getstate <- fips_codes$state
names(getstate) <- fips_codes$state_code

pop <- readRDS(here('data/temp/pubexp_1970_2020.RDS')) %>% 
  select(fips, fips_state, pop, year) %>% 
  distinct %>% 
  group_by(fips_state, year) %>% 
  summarise(pop = sum(pop, na.rm = TRUE)) %>% 
  rename(state_code = fips_state)

# Motor fuels taxes: State level

fuels_tax_state <- read.csv(here("data/raw/StateData/StateData.csv")) %>% 
  tibble %>% 
  filter(Year4 >= 1977) %>% 
  select(State_Code, Year4, Motor_Fuels_Tax) %>% 
  rename(year = Year4) %>% 
  mutate(log_fuels_tax = log(Motor_Fuels_Tax),
         state_code = ifelse(nchar(as.character(State_Code)) == 1, paste0("0", as.character(State_Code)), as.character(State_Code)),
         state = getstate[state_code]) %>% 
  # Removes Samoa, Canal Zone, Guam, Puerto Rico
  filter(!(state_code %in% c("03", "07", "14", "43")))  %>% 
  left_join(., pop, by = c("state_code", "year")) %>% 
  mutate(motor_fuels_tax_pc = Motor_Fuels_Tax/pop,
         log_motor_fuels_tax_pc = log(motor_fuels_tax_pc))

pdf(here("code/testing/motor_fuel_taxes_overview.pdf"), onefile = TRUE)

k1 <-  fuels_tax_state %>%
  pivot_longer(cols = c(Motor_Fuels_Tax, log_fuels_tax)) %>%
  ggplot(aes(x = year, y = value, color = state)) +
  geom_line() +
  facet_wrap(~name, scales = "free") +
  labs(y = "Gross amount", x = "Year", title = "Total revenue earned from motor fuels taxes by state")

grid.arrange(k1)

k2 <- fuels_tax_state %>%
  pivot_longer(cols = c(log_motor_fuels_tax_pc, motor_fuels_tax_pc)) %>%
  ggplot(aes(x = year, y = value, color = state)) +
  geom_line() +
  facet_wrap(~name, scales = "free") +
  labs(y = "Gross amount per capita", x = "Year", title = "Total revenue earned from motor fuels taxes by state (PC)")

grid.arrange(k2)

mean_70_20 <- fuels_tax_state %>%
  group_by(state) %>%
  summarise(mean_fuels_tax = mean(log(Motor_Fuels_Tax))) %>%
  plot_usmap(data = ., values = "mean_fuels_tax", regions = "states", col = "gray90", linewidth = 0.01) +
  scale_fill_distiller(palette = "Purples", name = "", direction = 1, na.value = "gray90") +
  theme(panel.background = element_rect(color = "white", fill = "white"),
        plot.title = element_text(face = "bold"), legend.background=element_blank()) +
  labs(title = "Mean motor fuels tax revenue by state 1970-2020 (log)")

mean_pc_70_20 <- fuels_tax_state %>%
  group_by(state) %>%
  summarise(mean_fuels_tax_pc = mean(log(motor_fuels_tax_pc))) %>%
  plot_usmap(data = ., values = "mean_fuels_tax_pc", regions = "states", col = "gray90", linewidth = 0.01) +
  scale_fill_distiller(palette = "Greens", name = "", direction = 1, na.value = "gray90") +
  theme(panel.background = element_rect(color = "white", fill = "white"),
        plot.title = element_text(face = "bold"), legend.background=element_blank()) +
  labs(title = "Mean motor fuels tax revenue by state 1970-2020 (PC log)")

grid.arrange(grobs = list(mean_70_20, mean_pc_70_20))

# 
# breaks_fuels_tax <- fuels_tax_state %>%
#   isatpanel(
#     data = .,
#     formula = as.formula(log_fuels_tax ~ 1),
#     index = c("state", "year"),
#     effect = "twoways",
#     iis = TRUE,
#     fesis = TRUE,
#     t.pval = 0.01
#   )
# 
# saveRDS(breaks_fuels_tax, here("code/testing/breaks_fuel_tax.RDS"))
# # plot(breaks_fuels_tax)
# 
dev.off()



# Liquor Sales: 


# Not complete at the county level: only 2000 observations from 1970-2020 and 3000+ counties
# 
# temp_county %>% 
#   select(FIPS_Combined, Year4, Liquor_Stores_Revenue) %>% 
#   filter(Liquor_Stores_Revenue != 0)

# Try state level
liquor_rev_state <- read.csv(here("data/raw/StateData/StateData.csv")) %>% 
  tibble %>% 
  select(State_Code, Year4, Liquor_Stores_Revenue) %>% 
  rename(year = Year4) %>% 
  mutate(log_liquor_rev = log(Liquor_Stores_Revenue),
         state_code = ifelse(nchar(as.character(State_Code)) == 1, paste0("0", as.character(State_Code)), as.character(State_Code)),
         state = getstate[state_code]) %>% 
  # Removes Samoa, Canal Zone, Guam, Puerto Rico
  filter(!(state_code %in% c("03", "07", "14", "43")))  %>% 
  left_join(., pop, by = c("state_code", "year")) %>% 
  mutate(liquor_stores_rev_pc = Liquor_Stores_Revenue/pop,
         log_liquor_stores_rev_pc = log(liquor_stores_rev_pc))

pdf(here("code/testing/liquor_stores_rev_overview.pdf"), onefile = TRUE)

j1 <- liquor_rev_state %>%
  pivot_longer(cols = c(Liquor_Stores_Revenue, log_liquor_rev)) %>%
  ggplot(aes(x = year, y = value, color = state)) +
  geom_line() +
  facet_wrap(~name, scales = "free") +
  labs(y = "Gross amount", x = "Year", title = "Total liquor store revenue by state")

grid.arrange(j1)

j2 <- liquor_rev_state %>%
  pivot_longer(cols = c(log_liquor_stores_rev_pc, liquor_stores_rev_pc)) %>%
  ggplot(aes(x = year, y = value, color = state)) +
  geom_line() +
  facet_wrap(~name, scales = "free") +
  labs(y = "Gross amount per capita", x = "Year", title = "Total liquor store revenue by state (PC)") 

grid.arrange(j2)

mean_70_20 <- liquor_rev_state %>%
  group_by(state) %>%
  summarise(mean_liquor_rev = mean(log(Liquor_Stores_Revenue))) %>%
  plot_usmap(data = ., values = "mean_liquor_rev", regions = "states", col = "gray90", linewidth = 0.01) +
  scale_fill_distiller(palette = "Purples", name = "", direction = 1, na.value = "gray90") +
  theme(panel.background = element_rect(color = "white", fill = "white"),
        plot.title = element_text(face = "bold"), legend.background=element_blank()) +
  labs(title = "Mean liquor store revenue by state 1970-2020 (log)")

mean_pc_70_20 <- liquor_rev_state %>%
  group_by(state) %>%
  summarise(mean_liquor_rev_pc = mean(log(liquor_stores_rev_pc))) %>%
  plot_usmap(data = ., values = "mean_liquor_rev_pc", regions = "states", col = "gray90", linewidth = 0.01) +
  scale_fill_distiller(palette = "Greens", name = "", direction = 1, na.value = "gray90") +
  theme(panel.background = element_rect(color = "white", fill = "white"),
        plot.title = element_text(face = "bold"), legend.background=element_blank()) +
  labs(title = "Mean liquor store revenue by state 1970-2020 (PC log)")

grid.arrange(grobs = list(mean_70_20, mean_pc_70_20))

              
#               
# breaks_liquor_rev <- liquor_rev_state %>%
#   isatpanel(
#     data = .,
#     formula = as.formula(log_liquor_rev ~ 1),
#     index = c("state", "year"),
#     effect = "twoways",
#     iis = TRUE,
#     fesis = TRUE,
#     t.pval = 0.01
#   )
# 
# saveRDS(breaks_liquor_rev, here("code/testing/breaks_liquor.RDS"))

 
#plot(breaks_liquor_rev)
 
dev.off()
  
            
            