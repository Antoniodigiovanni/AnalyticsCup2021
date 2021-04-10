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
    Nature_of_Payment_or_Transfer_of_Value = factor(Nature_of_Payment_or_Transfer_of_Value, levels = unique(Nature_of_Payment_or_Transfer_of_Value)),
    Form_of_Payment_or_Transfer_of_Value = factor(Form_of_Payment_or_Transfer_of_Value, levels = unique(Form_of_Payment_or_Transfer_of_Value)),
    Category = factor(Category, levels = unique(Category)),
    Related_Product_Indicator = factor(Related_Product_Indicator, levels = unique(Related_Product_Indicator)),
    Product_n = as.integer(Product_n),
    Third_Party_Covered = factor(Third_Party_Covered, levels = unique(Third_Party_Covered)),
    Third_Party_Recipient = factor(Third_Party_Recipient, levels = unique(Third_Party_Recipient)),
    Charity = factor(Charity, levels = unique(Charity)),
    Ownership_Indicator = factor(Ownership_Indicator, levels = unique(Ownership_Indicator)),
    Country_of_Travel = factor(Country_of_Travel, levels = unique(Country_of_Travel)),
    Date = as.Date(Date, format = "%m/%d/%Y")
  ) %>% 
  mutate(
    Ownership_Indicator = case_when(
      Ownership_Indicator == 'Yes' ~ 1,
      Ownership_Indicator == 'No' ~ 0
    )
  ) %>% 
  relocate(Ownership_Indicator) 
  # group_by(Physician_ID) %>%
  # mutate(
  #   Mean_payment_to_physician = mean(Total_Amount_of_Payment_USDollars),
  # ) %>% 
  # ungroup()

#Test for NB Implementation
payments <- payments %>% select(
    Ownership_Indicator,
    Record_ID,
    Physician_ID,
    Company_ID,
    Total_Amount_of_Payment_USDollars,
    Number_of_Payments,
    Form_of_Payment_or_Transfer_of_Value,
    Nature_of_Payment_or_Transfer_of_Value,
    Third_Party_Recipient,
    Related_Product_Indicator
  ) 
payments <-payments %>%
  group_by(Physician_ID) %>%
  mutate(
    Payment_Physician = sum(Total_Amount_of_Payment_USDollars),
    Total_Number_of_Payments_Physician = sum(Number_of_Payments),
    mean_Payment_Physician = Payment_Physician/Total_Number_of_Payments_Physician
  ) %>% ungroup()%>%
  group_by(Physician_ID, Nature_of_Payment_or_Transfer_of_Value) %>%
  mutate(
    Payment_by_Nature = sum(Total_Amount_of_Payment_USDollars),
    Percent_Payment_by_Nature_Physician = Payment_by_Nature/Payment_Physician *100
  ) %>% ungroup() %>%
  group_by(Physician_ID, Form_of_Payment_or_Transfer_of_Value) %>%
  mutate(
    Payment_by_Form = sum(Total_Amount_of_Payment_USDollars),
    Percent_Payment_by_Form_Physician = Payment_by_Form/Payment_Physician *100
  ) %>% ungroup() %>%
  mutate( 
    Number_of_Payments = case_when(
      Number_of_Payments == 1 ~ "1",
      Number_of_Payments == 2 ~ "2",
      Number_of_Payments == 3 ~ "3",
      Number_of_Payments == 4 ~ "4",
      between(Number_of_Payments,5,6)~ "5-6",
      between(Number_of_Payments,7,11) ~ "7-11",
      between(Number_of_Payments,12,16) ~ "12-16",
      between(Number_of_Payments,17,20) ~ "17-20",
      TRUE ~ "20+"
      ),
    Total_Amount_of_Payment_USDollars = cut(
      Total_Amount_of_Payment_USDollars, 
      breaks = c(-Inf,10,15,20,30,40,50,60,70,80,90,100,500,Inf)),  #Correct discretization bins
    Payment_Physician = cut(
      Payment_Physician, breaks = c(
        -Inf, 500, 1000, 5000, 20000,
        40000, Inf)),
    mean_Payment_Physician = cut(
      mean_Payment_Physician, breaks = c(
        -Inf, 50, 100, 300, 1000, 5000,
        10000, Inf)),
    Percent_Payment_by_Nature_Physician = cut(
      Percent_Payment_by_Nature_Physician,
      breaks = c(0, 25,  50, 75, 100)
    ),
    Percent_Payment_by_Form_Physician = cut(
      Percent_Payment_by_Form_Physician,
      breaks = c(0, 25,  50, 75, 100))
    ) %>%
  mutate(
    Number_of_Payments = factor(Number_of_Payments, levels = unique(Number_of_Payments)),
    #Total_Amount_of_Payment_USDollars = factor(Total_Amount_of_Payment_USDollars, levels = unique(Total_Amount_of_Payment_USDollars))
  ) %>% select(-Total_Number_of_Payments_Physician, -Payment_by_Nature, -Payment_by_Form)
    

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

#COMMENT THE FOLLOWING PART WHEN DOING THE REAL IMPLEMENTATION
#THIS SNIPPET WILL CREATE A TEST DATAFRAME AS A SUBSET OF A TRAINING SET
#IN ORDER TO IMPROVE THE MODEL
t <- merge(physicians, payments, by.x = 'id', by.y = 'Physician_ID' )
t <- merge(t, companies, by = "Company_ID")
t <- t %>% 
  relocate(
    Ownership_Indicator,
    Record_ID,
    id
    ) %>% 
  select(
    -Middle_Name,
    -First_Name,
    -Last_Name,
    -Name_Suffix,
    -Zipcode,
    -Company_State,  #Evaluate if some attributes can be re-introduced in the df
    -State, #May be re-added
    -Company_Name, #May be re-added
    -License_state_n
  )
#Calculate the total number of licensed states for each physician



#Test and Train
physicians_train <- physicians %>% filter(set == 'train')
physicians_split <- initial_split(physicians_train)

train_p <- training(physicians_split)
train <- subset(t, t$id %in% train_p$id)

test_p <- testing(physicians_split)
test <- subset(t, t$id %in% test_p$id)

rm(physicians_train, physicians_split, train_p, test_p)
#Use payments instead of t use not merged df
# train_set <- subset(t, payments$Physician_ID %in% physicians_train$id)  
# 
# train_split <- initial_split(train_set)
# 
# 
# train <- training(train_split)
# test <- testing(train_split)

#rm(train_split, physicians_train, train_set)



#USE THIS FOR THE REAL IMPLEMENTATION (i.e. prediction of the true dataset)
#Subset Training and Test data
# physicians_train <- physicians %>% filter(set == "train")
# physicians_test <- physicians %>% filter(set == "test")
# 
# train <- subset(payments, payments$Physician_ID %in% physicians_train$id)
# test <- subset(payments, payments$Physician_ID %in% physicians_test$id)
# 
# rm(physicians_test, physicians_train)
  #TODO: Finish by implementing the write-to-csv for the predictions


gc()

#Naive Bayes Implementation
library(e1071)

nb <- naiveBayes(as.factor(Ownership_Indicator) ~., data = train[,c(-2,-3,-4, -5)], laplace = 1)
test$pred <- predict(nb, test, type = "class")


#down sample to physicians
result <- test %>% group_by(id) %>%
  mutate(
    Ownership_Indicator = any(Ownership_Indicator == "1"),
    model_prediction = any(pred== "1")
  ) %>% ungroup() %>% select(id, Ownership_Indicator, model_prediction) %>% distinct() 

#Visualization of Confusion Matrix
confMat <- table(true=result$Ownership_Indicator, prediction=result$model_prediction)
confMat

#Model Evaluation
sensitivity <- (confMat[2,2]/(confMat[2,2]+confMat[2,1]))
specificity <- (confMat[1,1]/(confMat[1,1]+confMat[1,2]))

BAC <- (specificity+sensitivity)/2
  #(confMat[2,2]/(confMat[2,2]+confMat[2,1]) + confMat[1,1]/(confMat[1,1]+confMat[1,2]))/2
BAC


