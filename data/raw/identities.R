##### Identities

temp_county <- read_csv(here("data/raw/willamette_county_data/CountyData.csv"))
t <- temp_county %>% select(Year4, GOVSid, Total_Expenditure:General_Debt_Interest) %>% 
  filter(if_all(Total_Expenditure:General_Debt_Interest, ~ !.x  %in% 0)) %>% slice(1)


t$Total_Expenditure == t$Total_IG_Expenditure + t$Direct_Expenditure

t$Total_Expenditure == t$Total_Current_Expend + t$Total_Capital_Outlays

t$Direct_Expenditure == t$Total_Current_Oper +t$Total_Capital_Outlays + t$Tot_Assist___Subsidies + t$Total_Insur_Trust_Ben + t$Total_Interest_on_Debt

t$Total_Capital_Outlays == t$Total_Construction + t$Total_Other_Capital_Outlays

