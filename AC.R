library(tidyverse)
library(tidymodels)
library(skimr) #used for skim() function, to skim dataframes
library(lubridate)
library(Amelia) #used for missmap() function, to graph missing values in a df


options(dplyr.width = Inf) # show all columns when printing to console
theme_set(theme_minimal()) 
set.seed(2021)

companies <- read_csv('companies.csv')
payments <- read_csv('payments.csv')
physicians <- read_csv('physicians.csv')


#Tidy the physicians data-set + Change types of columns in the data-sets

physicians <- physicians %>% 
  pivot_longer(
    cols = starts_with("License_State_"),
    names_to = "License_state_n",
    names_prefix = "License_State_",
    values_to = "Licensed_in",
    values_drop_na = TRUE) %>%
  select(-Province) %>% #All provinces are NA
  select(-Country) %>% #The only country is USA
  separate(
    Primary_Specialty,
    c('Specialty1', 'Specialty2', 'Specialty3'), 
    "\\|"
    ) %>% 
  mutate(
      State = factor(State, levels = unique(State)),
      City = factor(City, levels = unique(City)),
      Licensed_in = factor(Licensed_in, levels = unique(Licensed_in)),
      License_state_n = as.integer(License_state_n),
      Specialty1 = factor(Specialty1, levels = unique(union(union(Specialty1,Specialty2),Specialty3))),
      Specialty2 = factor(Specialty2, levels = unique(union(union(Specialty1,Specialty2),Specialty3))),
      Specialty3 = factor(Specialty3, levels = unique(union(union(Specialty1,Specialty2),Specialty3))),
      Name_Suffix = factor(Name_Suffix, levels = unique(Name_Suffix))
  )

#Tidy payments + adjustments
payments <- payments %>% 
  pivot_longer(
    cols = c(starts_with("Product_Code_"),
             starts_with("Product_Name_"),
             starts_with("Product_Category_"),
             starts_with("Product_Type_")),
    names_to = c(".value", "Product_n"),
    names_pattern = "([A-Za-z]+)_(\\d)",
    values_drop_na=TRUE
  ) %>% 
  mutate(
    Type = factor(Type, levels = unique(Type)),
    Category = factor(Category, levels = unique(Category)),
    Related_Product_Indicator = factor(Related_Product_Indicator, levels = unique(Related_Product_Indicator)),
    Product_n = as.integer(Product_n),
    Third_Party_Covered = factor(Third_Party_Covered, levels = unique(Third_Party_Covered)),
    Third_Party_Recipient = factor(Third_Party_Recipient, levels = unique(Third_Party_Recipient)),
    Charity = factor(Charity, levels = unique(Charity)),
    Ownership_Indicator = factor(Ownership_Indicator, levels = unique(Ownership_Indicator)),
    Country_of_Travel = factor(Country_of_Travel, levels = unique(Country_of_Travel)),
    Date = as.Date(Date, format = "%m/%d/%Y")
  )

#Adjustments to companies data-set
companies <- companies %>% 
  rename(
    Company_Name = Name,
    Company_State = State,
    Company_Country = Country
    ) %>% 
  mutate(
    Company_Country = factor(Company_Country, levels = unique(Company_Country)),
    Company_State = factor(Company_State, levels = unique(Company_State))
  )

#To finish