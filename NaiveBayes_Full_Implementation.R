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

### Pre-Processing ####
#Merge the Data-frames
physicians <- physicians %>% 
  mutate(Physician_ID = id) %>% 
  select(-id)
payments_merged <- left_join(
  payments,
  physicians)

#Adjustments to companies data-set
companies <- companies %>% 
  dplyr::rename(
    Company_Name = Name,
    Company_State = State,
    Company_Country = Country
  ) %>%  
  mutate(
    Company_Country = factor(Company_Country, levels = unique(Company_Country)),
    Company_State = factor(Company_State, levels = unique(Company_State))
  )

payments_merged <- left_join(
  payments_merged,
  companies
)


payments_merged <- payments_merged %>% 
  mutate(
    Nature_of_Payment_or_Transfer_of_Value = gsub(" ", "_", Nature_of_Payment_or_Transfer_of_Value),
    Form_of_Payment_or_Transfer_of_Value = gsub(" ", "_", Form_of_Payment_or_Transfer_of_Value),
    Third_Party_Recipient = gsub(" ", "_", Third_Party_Recipient),
    Product_Type_1 = gsub(" ", "_", Product_Type_1)
  )

#Tidy the physicians data-set + Change types of columns in the data-sets
payments_merged <- payments_merged %>%
  mutate(
    Number_of_licenses = rowSums(!is.na(payments_merged[,34:36])),
  ) %>% 
  select(
    -Name_Suffix,
    -First_Name,
    -Last_Name,
    -Middle_Name,
    -Zipcode,
    -Province, #It is empty
    -starts_with("License_State_")
  ) %>% 
  separate(
    Primary_Specialty,
    c('Specialty1', 'Specialty2', 'Specialty3'), 
    "\\|"
  ) %>%
  mutate(
    State = factor(State, levels = unique(State)),
    City = factor(City, levels = unique(City)),
    Specialty1 = factor(Specialty1, levels = unique(Specialty1)),
    Specialty2 = factor(Specialty2, levels = unique(Specialty2)),
    Specialty3 = factor(Specialty3, levels = unique(Specialty3)),
  )

payments_merged <- payments_merged %>%
  mutate(
    Number_of_related_products= rowSums(!is.na(payments_merged[,20:22])) #Number of products included in the transaction
  ) %>% select(
    #Most of the deleted columns are plenty of NAs
    -starts_with("Product_Code_"),
    -City_of_Travel,
    -State_of_Travel,
    -Country_of_Travel,
    -Third_Party_Covered,
    -Product_Type_2,
    -Product_Type_3, #Keeping Product_type_1 because it has a complete rate of 0.939
    -Product_Name_2,
    -Product_Name_3,
    -starts_with("Product_Category"),
    -Contextual_Information,
    -Charity #Half NA, half values, may think of re-introducing this
  ) %>% 
  mutate(
    Nature_of_Payment_or_Transfer_of_Value = factor(Nature_of_Payment_or_Transfer_of_Value, levels = unique(Nature_of_Payment_or_Transfer_of_Value)),
    Form_of_Payment_or_Transfer_of_Value = factor(Form_of_Payment_or_Transfer_of_Value, levels = unique(Form_of_Payment_or_Transfer_of_Value)),
    Related_Product_Indicator = factor(Related_Product_Indicator, levels = unique(Related_Product_Indicator)),
    Third_Party_Recipient = factor(Third_Party_Recipient, levels = unique(Third_Party_Recipient)),
    #Ownership_Indicator = factor(Ownership_Indicator, levels = unique(Ownership_Indicator)),
    Date = as.Date(Date, format = "%m/%d/%Y"),
    Transaction_Month = month(Date),
  ) %>% 
  # mutate(
  #   Ownership_Indicator = case_when(
  #     Ownership_Indicator == 'Yes' ~ 1,     #Caret complains with 0/1 classif.
  #     Ownership_Indicator == 'No' ~ 0
  #   )
  # ) %>% 
  relocate(Ownership_Indicator) %>% 
  mutate( 
    Number_of_Payments = case_when(      #Correct how the split is performed
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
    Number_of_Payments = factor(Number_of_Payments, levels = unique(Number_of_Payments)),
    Total_Amount_of_Payment_USDollars = cut(
      Total_Amount_of_Payment_USDollars, 
      breaks = c(0,10,15,20,30,40,50,60,70,80,90,100,Inf)), #Correct how the split is performed
    Number_of_Specialties = rowSums(!is.na(payments_merged[,18:20]))
  ) %>% 
  select(
    -Specialty2,
    -Specialty3
  ) %>% 
  mutate(
    #Number_of_related_products = factor(Number_of_related_products, levels = unique(Number_of_related_products)),
    #Transaction_Month = factor(Transaction_Month, levels = unique(Transaction_Month)),
    #Number_of_Specialties = factor(Number_of_Specialties, levels = unique(Number_of_Specialties)),
    #Number_of_licenses = factor(Number_of_licenses, levels = unique(Number_of_licenses)),
    Physician_Country = factor(Country, levels = unique(Company_Country)),
    Company_Name = factor(Company_Name, levels = unique(Company_Name)),
    Product_Type_1 = factor(Product_Type_1, levels = unique(Product_Type_1)),
    Product_Name_1 = factor(Product_Name_1, levels = unique(Product_Name_1)),
    Physician_Country= tolower(Physician_Country),
    Company_Country = tolower(Company_Country),
    Phys_and_comp_in_same_country = case_when(
      Company_Country == Physician_Country ~ 1,
      TRUE ~ 0),
    #Phys_and_comp_in_same_country = factor(Phys_and_comp_in_same_country, levels = c(0,1))
  ) %>% 
  select(
    -Country
  ) %>% 
  mutate(
    Number_of_licenses = factor(Number_of_licenses),
    Number_of_related_products = factor(Number_of_related_products),
    Number_of_Specialties = factor(Number_of_Specialties),
    Phys_and_comp_in_same_country = factor(Phys_and_comp_in_same_country),
    Transaction_Month = factor(Transaction_Month),
    Number_of_Payments = factor(Number_of_Payments)
  )

rm(payments,companies,physicians)

### Preparation of Train and Test data-sets ####

payments_merged <- payments_merged %>% 
  relocate(
    Ownership_Indicator,
    Record_ID,
    Physician_ID
  ) %>%
  select(
    -Company_State,  #Evaluate if some attributes can be re-introduced in the df
    -State, #May be re-added
    -Company_Name, #May be re-added
    -Product_Name_1,
    -City
  )

train <- payments_merged %>% filter(set == 'train')
test <- payments_merged %>% filter(set=='test')

train <- train %>% 
  select(
    -Specialty1,
    -set,
  ) %>% mutate(
    Ownership_Indicator = factor(Ownership_Indicator, levels = unique(Ownership_Indicator)))

test <- test %>% 
  select(
    -Specialty1,
    -set,
  ) %>% mutate(
    Ownership_Indicator = factor(Ownership_Indicator, levels = unique(Ownership_Indicator)),
  )  
train <- train %>% 
  select(
    -Date,
    -Physician_Country,
    -Product_Type_1,
    -Company_Country)

test <- test %>% 
  select(
    -Date,
    -Physician_Country,
    -Product_Type_1,
    -Company_Country) 

train <-drop_na(train) #There are only 3 instances with NA
test <- drop_na(test)

### Model Generation ####
library(bnclassify)

naive <- bnc(
  'nb',
  'Ownership_Indicator',
  train[,c(-2,-3,-4)],
  1)

cv(naive, 
   train[,c(-2,-3,-4)], 
   k=10, 
   dag = FALSE, 
   mean = TRUE)

###Predict on the test Set ####
TestSetPrediction <- predict(naive, test)
accuracy(TestSetPrediction, test$Ownership_Indicator) #It does not work sometimes, why?

Prediction <- cbind.data.frame(id=test$Physician_ID, Pred=as.factor(TestSetPrediction))
Prediction <- as.tibble(Prediction)

Prediction <- Prediction %>% 
  mutate(
    Pred = case_when(
      Pred=='No' ~ 0,
      Pred=='Yes'~ 1))

Prediction <- Prediction %>% 
  pivot_wider(
    values_from = Pred,
    values_fn = max,
    names_from = "Pred"
  )

cols <- c("0","1")
Prediction$prediction <- ((rowSums(Prediction[, cols] == 1, na.rm=T) > 0) * 1)
Submission <- Prediction %>% group_by(id,prediction) %>% distinct(id) #Check again if this is correct

write_csv(Submission, "CHAP.csv")


#rm(TestSetPrediction,payments_merged,naive,test)
