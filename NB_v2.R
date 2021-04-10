### Imports ####
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
  rename(
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
    Specialty1 = factor(Specialty1, levels = unique(union(union(Specialty1,Specialty2),Specialty3))),
    Specialty2 = factor(Specialty2, levels = unique(union(union(Specialty1,Specialty2),Specialty3))),
    Specialty3 = factor(Specialty3, levels = unique(union(union(Specialty1,Specialty2),Specialty3))),
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
    Ownership_Indicator = factor(Ownership_Indicator, levels = unique(Ownership_Indicator)),
    Date = as.Date(Date, format = "%m/%d/%Y"),
    Transaction_Month = month(Date),
    ) %>% 
  mutate(
    Ownership_Indicator = case_when(
      Ownership_Indicator == 'Yes' ~ 1,
      Ownership_Indicator == 'No' ~ 0
    )
  ) %>% 
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
    Number_of_related_products = factor(Number_of_related_products, levels = unique(Number_of_related_products)),
    Transaction_Month = factor(Transaction_Month, levels = unique(Transaction_Month)),
    Number_of_Specialties = factor(Number_of_Specialties, levels = unique(Number_of_Specialties)),
    Number_of_licenses = factor(Number_of_licenses, levels = unique(Number_of_licenses)),
    Physician_Country = factor(Country, levels = unique(Country)),
    Company_Name = factor(Company_Name, levels = unique(Company_Name)),
    Product_Type_1 = factor(Product_Type_1, levels = unique(Product_Type_1)),
    Product_Name_1 = factor(Product_Name_1, levels = unique(Product_Name_1)),
    Physician_Country= tolower(Physician_Country),
    Company_Country = tolower(Company_Country)
  ) %>% 
  select(
    -Country
    )

rm(payments,companies,physicians)

### Exploratory plots####

p1 <- payments_merged %>% 
  filter(Ownership_Indicator == 1) %>% 
  ggplot(aes(x=Form_of_Payment_or_Transfer_of_Value,fill=Ownership_Indicator)) +
  geom_histogram(stat = "count") +
  coord_flip()
p2 <- payments_merged %>% 
  filter(Ownership_Indicator==0) %>% 
  ggplot(aes(x=Form_of_Payment_or_Transfer_of_Value,fill=Ownership_Indicator)) +
  geom_histogram(stat = "count") +
  coord_flip()
library(grid)
grid.newpage()
grid.draw(rbind(ggplotGrob(p2),ggplotGrob(p1), size= "last"))
rm(p1,p2)

p1 <- payments_merged %>%
  filter(Ownership_Indicator==1) %>% 
  ggplot(aes(Total_Amount_of_Payment_USDollars, fill=Ownership_Indicator)) +
  geom_histogram(stat="count")
p2 <- payments_merged %>%
  filter(Ownership_Indicator==0) %>% 
  ggplot(aes(Total_Amount_of_Payment_USDollars, fill=Ownership_Indicator)) +
  geom_histogram(stat="count")
grid.newpage()
grid.draw(rbind(ggplotGrob(p2),ggplotGrob(p1), size= "last"))
rm(p1,p2)

p1 <- payments_merged %>%
  filter(Ownership_Indicator==1) %>% 
  ggplot(aes(Number_of_Specialties, fill=Ownership_Indicator)) +
  geom_histogram(stat="count")
p2 <- payments_merged %>%
  filter(Ownership_Indicator==0) %>% 
  ggplot(aes(Number_of_Specialties, fill=Ownership_Indicator)) +
  geom_histogram(stat="count")
grid.newpage()
grid.draw(rbind(ggplotGrob(p2),ggplotGrob(p1), size= "last"))
rm(p1,p2)

p1 <- payments_merged %>%
  filter(Ownership_Indicator==1) %>% 
  ggplot(aes(Number_of_licenses, fill=Ownership_Indicator)) +
  geom_histogram(stat="count")
p2 <- payments_merged %>%
  filter(Ownership_Indicator==0) %>% 
  ggplot(aes(Number_of_licenses, fill=Ownership_Indicator)) +
  geom_histogram(stat="count")
grid.newpage()
grid.draw(rbind(ggplotGrob(p2),ggplotGrob(p1), size= "last"))
rm(p1,p2)


### Modeling ####
#COMMENT THE FOLLOWING PART WHEN DOING THE REAL IMPLEMENTATION
#THIS SNIPPET WILL CREATE A TEST DATAFRAME AS A SUBSET OF A TRAINING SET
#IN ORDER TO IMPROVE THE MODEL
payments_merged <- payments_merged %>% 
  select(
    -Total_Amount_of_Payment_USDollars,
    -Number_of_licenses
  )


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
  )



#Test and Train
train_set_df <- payments_merged %>% filter(set == 'train')
df <- train_set_df %>% distinct(Physician_ID)

#train_split <- initial_split(train_set_df, strata = Ownership_Indicator)
train_split <- initial_split(df)

train <- training(train_split)
test <- testing(train_split)

train <- left_join(train, train_set_df, by = "Physician_ID")
test <- left_join(test, train_set_df, by = "Physician_ID")

rm(df,train_split, train_set_df)


#Naive Bayes Implementation
library(e1071)
library(fastNaiveBayes)
#nb <- fastNaiveBayes(train[,c(-2,-3,-4)], as.factor(Ownership_Indicator) ~., laplace = 1)
nb <- naiveBayes(as.factor(Ownership_Indicator) ~., data = train[,c(-2,-3,-4)], laplace = 1)
test$O_I_predicted <-predict(nb, test, type = "class")

#### Writing Results ####
result <- test %>% 
  distinct(Physician_ID) 

result <- result %>% left_join(
  test %>% group_by(Physician_ID) %>%
    mutate(
      O_I_predicted = as.integer(as.logical(any(O_I_predicted == 1)))
    ) %>%
    select(Physician_ID, O_I_predicted) %>% distinct(),
  by = c("Physician_ID" = "Physician_ID"))

result <- result %>% left_join(
  test %>% group_by(Physician_ID) %>% 
    mutate(Ownership_Indicator = as.integer(as.logical(any(Ownership_Indicator == 1)))
) %>% 
  select(Physician_ID, Ownership_Indicator) %>% distinct(),
by = c("Physician_ID" = "Physician_ID"))

confMat <- table(true=result$Ownership_Indicator, prediction=result$O_I_predicted)
confMat
sensitivity <- (confMat[2,2]/(confMat[2,2]+confMat[2,1]))
specificity <- (confMat[1,1]/(confMat[1,1]+confMat[1,2]))

BAC <- (specificity+sensitivity)/2
BAC


#### Actual Implementation, writing the results for the true test data-set ####
  ###       ###
 ### TODO ###
###     ###