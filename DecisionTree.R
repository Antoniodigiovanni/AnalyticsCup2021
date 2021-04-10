library(tidyverse)
library(tidymodels)
library(skimr) #used for skim() function, to skim dataframes
library(lubridate)
library(Amelia) #used for missmap() function, to graph missing values in a df
library(stringr)
# library(rpart)
# library(rpart.plot)
library(party)

library(sparklyr)
library(ranger)


options(dplyr.width = Inf) # show all columns when printing to console
theme_set(theme_minimal()) 
set.seed(2021)



physicians <- read_csv('physicians.csv')

physicians <- physicians %>% filter(set == "train") %>% select(-set) 

split <- initial_split(physicians, prop = 0.8)
physician_train <- training(split)
physician_test <- testing(split)


physicians <- physicians %>%
  mutate(
    Licensed_States_number = as.integer(rowSums(!is.na(physicians %>% select(starts_with("License_State_"))))),
    number_of_specialties = as.integer(str_count(physicians$Primary_Specialty, "\\|") +1)
  ) %>%
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
    c('Specialty_1', 'Specialty_2', 'Specialty_3'), 
    "\\|"
  ) %>%
  pivot_longer(
    cols = starts_with("Specialty_"),
    names_to = "Specialty_n",
    names_prefix = "Specialty_",
    values_to = "Specialty_in",
    values_drop_na = TRUE) %>% 
  mutate(
    State = factor(State, levels = unique(State)),
    City = factor(City, levels = unique(City)),
    Licensed_in = factor(Licensed_in, levels = unique(Licensed_in)),
    License_state_n = as.integer(Licensed_States_number),
    
    Specialty_in = factor(Specialty_in, levels = unique(Specialty_in)),
    Specialty_n = as.integer(number_of_specialties),
    # Specialty1 = factor(Specialty1, levels = unique(union(union(Specialty1,Specialty2),Specialty3))) %>% na.omit(),
    # Specialty2 = factor(Specialty2, levels = unique(union(union(Specialty1,Specialty2),Specialty3))) %>% na.omit(),
    # Specialty3 = factor(Specialty3, levels = unique(union(union(Specialty1,Specialty2),Specialty3))) %>% na.omit(),
    Name_Suffix = factor(Name_Suffix, levels = unique(Name_Suffix))
  )
  
physicians <- physicians %>%  select(-First_Name, -Middle_Name, -Last_Name, -Name_Suffix, 
         -City, -Zipcode, -State, -Licensed_States_number, -number_of_specialties)

physician_train <- physicians %>% filter(physicians$id %in% physician_train$id)
physician_test <- physicians %>% filter(physicians$id %in% physician_test$id)


payments <- read_csv('payments.csv')
#filter out the payments to physicians of the test set
payments <- payments %>% filter(payments$Physician_ID %in% physicians$id)

Analytics <- payments %>%
  mutate(Form_of_Payment_or_Transfer_of_Value = factor(
    Form_of_Payment_or_Transfer_of_Value, levels = unique(Form_of_Payment_or_Transfer_of_Value)),
    Nature_of_Payment_or_Transfer_of_Value= factor(
      Nature_of_Payment_or_Transfer_of_Value, levels = unique(Nature_of_Payment_or_Transfer_of_Value))) %>%
  group_by(Physician_ID) %>%
  mutate(
    O_I = Ownership_Indicator, # any(Ownership_Indicator== "Yes"),
    Payment_Physician = sum(Total_Amount_of_Payment_USDollars)
    ) %>% ungroup() %>% 
  group_by(Physician_ID,Company_ID, Nature_of_Payment_or_Transfer_of_Value) %>%
  mutate(Percentage_of_Payment_of_Physician_by_Nature = sum(Total_Amount_of_Payment_USDollars) / Payment_Physician * 100) %>% 
  ungroup() %>%
  group_by(Physician_ID,Form_of_Payment_or_Transfer_of_Value) %>%
  mutate(Percentage_of_Payment_of_Physician_by_Form = sum(Total_Amount_of_Payment_USDollars) / Payment_Physician * 100) %>% 
  ungroup()%>%
  group_by(Physician_ID,Company_ID) %>% 
  mutate(Payment_between_Physician_and_Company = sum(Total_Amount_of_Payment_USDollars),
         # Percentage_of_Payment_by_Company_with_Their_Party_Recipient = Payment_between_Physician_and_Company / Payment_Physician * 100,
         # Percentage_of_Payment_by_Company = Payment_between_Physician_and_Company / Payment_Physician * 100,
        Percentage_of_Payment_by_Company = Payment_between_Physician_and_Company / Payment_Physician * 100
         ) %>%
  ungroup() %>%
  select(Physician_ID, O_I, Payment_Physician, 
         Company_ID, Percentage_of_Payment_by_Company,
         Nature_of_Payment_or_Transfer_of_Value, Percentage_of_Payment_of_Physician_by_Nature,
         Form_of_Payment_or_Transfer_of_Value,Percentage_of_Payment_of_Physician_by_Form) %>% distinct() %>%
  mutate(
    O_I = factor(O_I, unique(O_I)),
    Payment_Physician = cut(
      Payment_Physician, breaks = c(
        -Inf, 100, 200, 400, 750, 1000,
        1250, 1500, 2500, 5000, 7500,
        10000, 1e05, 1e06, 1e07, Inf)),
    Percentage_of_Payment_by_Company = cut(
      Percentage_of_Payment_by_Company,
      breaks = c(0, 2.5, 5, 10, 15, 20, 30, 40, 50, 60, 70, 80, 90, 100)
    ),
    Percentage_of_Payment_of_Physician_by_Nature = cut(
      Percentage_of_Payment_of_Physician_by_Nature,
      breaks = c(0, 2.5, 5, 10, 15, 20, 30, 40, 50, 60, 70, 80, 90, 100)
    ),
    Percentage_of_Payment_of_Physician_by_Form = cut(
      Percentage_of_Payment_of_Physician_by_Form,
      breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 95, 100)
    )
  )


companies <- read_csv('companies.csv')

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





## a simple test run

df_train <- physician_train  %>% 
  left_join(
    Analytics , 
    # %>% select(-Name, -City_of_Travel, -Contextual_Information, -Code, -Date)
    by=c("id" = "Physician_ID")) %>%
  mutate(
    O_I = factor(O_I, levels = unique(O_I))
  ) %>%
  left_join(
    companies %>% select(-Company_Name),
    by = c("Company_ID" = "Company_ID")
  )

df_test <- physician_test %>% 
  left_join(
    Analytics , 
    # %>% select(-Name, -City_of_Travel, -Contextual_Information, -Code, -Date)
    by=c("id" = "Physician_ID")) %>%
  mutate(
    O_I = factor(O_I, levels = unique(O_I))
  ) %>%
  left_join(
    companies %>% select(-Company_Name),
    by = c("Company_ID" = "Company_ID")
  )


remove(companies, payments, physicians, Analytics, split, physician_test, physician_train)
gc()

controls <- ctree_control(teststat = c("quad", "max"),
                          testtype = c("Bonferroni", "Univariate", "Teststatistic"),
                          mincriterion = 0.95,
                          stump = FALSE, maxsurrogate = 0,
                          mtry = 0, savesplitstats = TRUE)
tree <- ctree(formula = O_I~.,data = df_train[, !colnames(df_train) %in% c("id", "Company_ID")])

plot(tree)



df_test$model_prediction <- predict(tree, df_test[, !colnames(df_test) %in% c("id", "Company_ID", "O_I")])

result <- truth %>% left_join(
  df_test %>% group_by(id) %>%
    mutate(
      model_prediction = as.integer(as.logical(any(model_prediction == "TRUE")))
    ) %>%
    select(id, model_prediction) %>% distinct()
  ,
  by = c("id" = "id"))

result <- df_test %>% group_by(id) %>%
  mutate(
    O_I = any(O_I == "Yes"),
    model_prediction = any(model_prediction== "Yes")
  ) %>% ungroup() %>% select(id, O_I, model_prediction) %>% distinct() 


m_acc <- table(result$O_I, result$model_prediction)
BAC <- (m_acc[2,2]/(m_acc[2,2]+m_acc[2,1]) + m_acc[1,1]/(m_acc[1,1]+m_acc[1,2]))/2
BAC

