### Imports ####
library(tidyverse)
library(tidymodels)
library(skimr) #used for skim() function, to skim dataframes
library(lubridate)
library(Amelia) #used for missmap() function, to graph missing values in a df
library(xgboost)
library(base)
library(rlist)
library(NMOF)
library(robustHD)

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
  )

payments_merged <- payments_merged %>%
  mutate(
    Number_of_related_products= rowSums(!is.na(payments_merged[,20:22])) #Number of products included in the transaction
  ) %>% select(
    #Most of the deleted columns are plenty of NAs
    -starts_with("Product_Code_"),
    -City_of_Travel,
    -State_of_Travel,
    -Product_Type_3, #Keeping Product_type_1 because it has a complete rate of 0.939
    -Product_Name_2,
    -Product_Name_3,
    -starts_with("Product_Category"),
    -Contextual_Information,
    -Charity #Half NA, half values, may think of re-introducing this
  ) %>% 
  mutate(
    Date = as.Date(Date, format = "%m/%d/%Y"),
    Transaction_Year = year(Date),
    Transaction_Month = month(Date),
  ) %>% 
  mutate(
    Ownership_Indicator = case_when(
      Ownership_Indicator == 'Yes'  ~1,
      Ownership_Indicator == 'No' ~ 0
    ) 
  ) %>% 
  relocate(Ownership_Indicator) %>% 
  select(
    -Specialty2,
    -Specialty3,
  ) 

# filter payments with set == train
payments_merged <- payments_merged[payments_merged$set == 'train',]
# add a 1 column in order to count the payments per physician
payments_merged <- cbind(payments_merged, 1)

# find out all physician ids
ids <- payments_merged %>% distinct(Physician_ID)
# divide set into random train set and test set
k <- 6
folds <- cut(seq(1,nrow(ids)),breaks=k,labels=FALSE)
numIndexes <- which(folds==1,arr.ind=TRUE)
randtestPhysicianIDs <- sample(1:nrow(ids), size = length(numIndexes))
train_ids <- ids[[1]][-randtestPhysicianIDs]
test_ids <- ids[[1]][randtestPhysicianIDs]

# find out which payments of physicians in the test set have
# an ownership indicator and delete them
ones <- which(payments_merged[which(payments_merged$Physician_ID %in% test_ids),] == 1)
payments_merged <- payments_merged[-ones,]

# filter which companies have a link to an Ownership Indicator
# and put all other companies into a "Dummy" class
max <- aggregate(payments_merged$Ownership_Indicator ~ payments_merged$Company_ID, payments_merged, sum)
company_ids <- max[which(max[,2] > 0),]
#companies <- payments_merged[payments_merged$Company_ID %in% company_ids[,1],c(3,4)]
payments_merged[!payments_merged$Company_ID %in% company_ids[,1],4] <- "Dummy"

# replace NAs
payments_merged$Third_Party_Covered[is.na(payments_merged$Third_Party_Covered)] <- "Dummy Specialty"
payments_merged$Specialty1[is.na(payments_merged$Specialty1)] <- "Dummy Specialty"
payments_merged$Related_Product_Indicator[is.na(payments_merged$Related_Product_Indicator)] <- "Dummy Product Indicator"
payments_merged$Product_Type_1[is.na(payments_merged$Product_Type_1)] <- "Dummy Product"
payments_merged$State[is.na(payments_merged$State)] <- "Dummy State"
payments_merged$Company_State[is.na(payments_merged$Company_State)] <- "Dummy Company_State"

# Design matrices
specialty <- model.matrix(~Specialty1-1,payments_merged)
product <- model.matrix(~Product_Type_1-1,payments_merged)
product_indicator <- model.matrix(~Related_Product_Indicator-1,payments_merged)
form <- model.matrix(~Form_of_Payment_or_Transfer_of_Value-1,payments_merged)
third_party <- model.matrix(~Third_Party_Recipient-1,payments_merged)
payment <- model.matrix(~Nature_of_Payment_or_Transfer_of_Value-1,payments_merged)
state <- model.matrix(~State-1,payments_merged)
com_state <- model.matrix(~Company_State-1,payments_merged)
company <- model.matrix(~payments_merged[,4]-1,payments_merged)

# count together attributes per physician ID
amount <- aggregate(payments_merged$Total_Amount_of_Payment_USDollars ~ payments_merged$Physician_ID, payments_merged, sum)
count <- aggregate(payments_merged[,length(payments_merged)] ~ payments_merged$Physician_ID, payments_merged, sum)
specialty <- aggregate(specialty ~ payments_merged$Physician_ID, payments_merged, sum)
product <- aggregate(product ~ payments_merged$Physician_ID, payments_merged, sum)
product_indicator <- aggregate(product_indicator ~ payments_merged$Physician_ID, payments_merged, sum)
third_party <- aggregate(third_party ~ payments_merged$Physician_ID, payments_merged, sum)
payment <- aggregate(payment ~ payments_merged$Physician_ID, payments_merged, sum)
state <- aggregate(state ~ payments_merged$Physician_ID, payments_merged, sum)
com_state <- aggregate(com_state ~ payments_merged$Physician_ID, payments_merged, sum)
O_I <- cbind(aggregate(payments_merged$Ownership_Indicator ~ payments_merged$Physician_ID, payments_merged, sum)[,1],sign(aggregate(payments_merged$Ownership_Indicator ~ payments_merged$Physician_ID, payments_merged, sum)[,2]))
form <- aggregate(form ~ payments_merged$Physician_ID, payments_merged, sum)
company <- aggregate(company ~ payments_merged$Physician_ID, payments_merged, sum)
number <- aggregate(payments_merged$Number_of_licenses ~ payments_merged$Physician_ID, payments_merged, mean)

numerics <- cbind(O_I, amount, count, specialty, product, product_indicator, third_party, state, number, com_state, form, company)
matrix_train <- data.matrix(numerics)

train_data <- matrix_train[matrix_train[,1] %in% train_ids,]
for (id in train_ids){
  if (O_I[O_I[,1]==id,2] > 0){
    train_data[train_data[,1]==id,2] <- 1
  }
}
train_labels <- train_data[,2]
training <- train_data[,-2]

test_data <- matrix_train[matrix_train[,1] %in% test_ids,]
for (id in test_ids){
  if (O_I[O_I[,1]==id,2] > 0){
    test_data[test_data[,1]==id,2] <- 1
  }
}
test_labels <- test_data[,2]
testing <- test_data[,-2]

# put our testing & training data into two seperates Dmatrixs objects
dtrain <- xgb.DMatrix(data = training, label= train_labels)
dtest <- xgb.DMatrix(data = testing, label= test_labels)

#test_function <- function(nround, eta, scale_pos_weight, gamma, split, max_max_depth){
  set.seed(8)
  model <- xgboost(data = dtrain, # the data
                   nround = 1500, # max number of boosting iterations
                   eta = 0.008,
                   scale_pos_weight = 10,
                   subsample = 0.93,
                   max_depth = 5,
                   objective = "binary:logistic")
  
  O_I_predicted1 <- predict(model, dtest)
  split <- 0.1
  O_I_predicted <- as.numeric(O_I_predicted1 > split)
  
  confMat <- base::table(true = test_labels, prediction=O_I_predicted)
  if ("1" %in% colnames(confMat)){
    if ("0" %in% colnames(confMat)){
      
      sensitivity <- (confMat[2,2]/(confMat[2,2]+confMat[2,1]))
      specificity <- (confMat[1,1]/(confMat[1,1]+confMat[1,2]))
      
      BAC <- (specificity+sensitivity)/2
      confMat
      BAC
  #    return(1/BAC)}
  #} else {
  #  return(1/0.000001)
  }
}

#levels <- list(nround = c(1500, 1400, 1450), eta = c(0.01,0.007,0.011,0.009),
 #              max_depth = c(5, 6, 4), split = c(0.1, 0.15, 0.009))

#comparison <- gridSearch(test_function, levels)

#1/comparison$minfun


# Ändern für Verbesserung
# Country_of_Travel dazugefügt
# nround = 1500, # max number of boosting iterations
# eta = 0.01,
# scale_pos_weight = 10,
# subsample = 0.93,
# max_depth = 5,




