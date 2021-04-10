### Imports ####
library(tidyverse)
library(tidymodels)
library(skimr) #used for skim() function, to skim dataframes
library(lubridate)
library(Amelia) #used for missmap() function, to graph missing values in a df
library(xgboost)
library(base)


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
  select(
    -Specialty2,
    -Specialty3,
  )

payments_merged1 <- na.omit(payments_merged)
payments_merged1 <- payments_merged1[payments_merged1$set == 'train',]
ids <- payments_merged1 %>% distinct(Physician_ID)


specialty <- model.matrix(~Specialty1-1,payments_merged1)
product <- model.matrix(~Product_Type_1-1,payments_merged1)
product_indicator <- model.matrix(~Related_Product_Indicator-1,payments_merged1)
form <- model.matrix(~Form_of_Payment_or_Transfer_of_Value-1,payments_merged1)
third_party <- model.matrix(~Third_Party_Recipient-1,payments_merged1)
payment <- model.matrix(~Nature_of_Payment_or_Transfer_of_Value-1,payments_merged1)
state <- model.matrix(~State-1,payments_merged1)
com_state <- model.matrix(~Company_State-1,payments_merged1)

numerics <- cbind(payments_merged1$Physician_ID, payments_merged1$Ownership_Indicator, payments_merged1$Company_ID,payments_merged1$Number_of_licenses, specialty, product, product_indicator, third_party, state, form, com_state)
matrix_train <- data.matrix(numerics)

# cross validation
k <- 5
SAVE_BACs <-c()
SAVE_confMat <- list()

#Create k equally size folds
folds <- cut(seq(1,nrow(ids)),breaks=k,labels=FALSE)
#Perform k fold cross validation
for(i in 1:k){
  #i = 1
  #Segement your data by fold using the which() function
  randtestPhysicianIDs <- which(folds==i,arr.ind=TRUE)
  train_ids <- ids[[1]][-randtestPhysicianIDs]
  test_ids <- ids[[1]][randtestPhysicianIDs]
  
  train <- matrix_train[matrix_train[,1] %in% train_ids,]
  train_data <- train[,-2]
  train_labels <- train[,2]
  
  test <- matrix_train[matrix_train[,1] %in% test_ids,]
  test_data <- test[,-2]
  test_labels <- test[,2]

# put our testing & training data into two seperates Dmatrixs objects
dtrain <- xgb.DMatrix(data = train_data, label= train_labels)
dtest <- xgb.DMatrix(data = test_data, label= test_labels)

model <- xgboost(data = dtrain, # the data   
                 nround = 20, # max number of boosting iterations
                 eta = 0.4,
                 subsample = 0.8,
                 scale_pos_weight = 540, # for unbalanced classes 
                 # scale_pos_weight is the ratio between the classes
                 # scale_pos_weight makes the train-logloss higher but the
                 # BAC higher (because of different weighting)
                 objective = "binary:logistic")

O_I_predicted <- predict(model, dtest)

O_I_predicted

O_I_predicted <- as.numeric(O_I_predicted > 0.02)

result <- cbind(O_I_predicted,test_labels)

confMat <- base::table(true=result[,2], prediction=result[,1])

sensitivity <- (confMat[2,2]/(confMat[2,2]+confMat[2,1]))
specificity <- (confMat[1,1]/(confMat[1,1]+confMat[1,2]))

BAC <- (specificity+sensitivity)/2
SAVE_BACs <- c(SAVE_BACs,BAC)
SAVE_confMat <- list.append(.data = SAVE_confMat, c(cross = confMat))
}

# folds <- cut(seq(1,nrow(ids)),breaks=k,labels=FALSE)
# numIndexes <- which(folds==1,arr.ind=TRUE)
# 
# randtestPhysicianIDs <- sample(1:nrow(ids), size = length(numIndexes))
# train_ids <- ids[[1]][-randtestPhysicianIDs]
# test_ids <- ids[[1]][randtestPhysicianIDs]
# 
# train <- matrix_train[matrix_train[,1] %in% train_ids,]
# train_data <- train[,-2]
# train_labels <- train[,2]
# 
# test <- matrix_train[matrix_train[,1] %in% test_ids,]
# test_data <- test[,-2]
# test_labels <- test[,2]
# 
# # put our testing & training data into two seperates Dmatrixs objects
# dtrain <- xgb.DMatrix(data = train_data, label= train_labels)
# dtest <- xgb.DMatrix(data = test_data, label= test_labels)
# 
# model <- xgboost(data = dtrain, # the data   
#                  nround = 24, # max number of boosting iterations 
#                  # try 16
#                  eta = 0.4,
#                  subsample = 0.8,
#                  lambda = 3,
#                  scale_pos_weight = 540, # for unbalanced classes 
#                  # scale_pos_weight is the ratio between the classes
#                  # scale_pos_weight makes the train-logloss higher but the
#                  # BAC higher (because of different weighting)
#                  objective = "binary:logistic")
# 
# O_I_predicted <- predict(model, dtest)
# 
# O_I_predicted <- as.numeric(O_I_predicted > 0.02)
# 
# result <- cbind(O_I_predicted,test_labels)
# 
# confMat <- base::table(true=result[,2], prediction=result[,1])
# confMat
# 
# sensitivity <- (confMat[2,2]/(confMat[2,2]+confMat[2,1]))
# specificity <- (confMat[1,1]/(confMat[1,1]+confMat[1,2]))
# 
# 
# BAC <- (specificity+sensitivity)/2
# SAVE_BACs <- c(SAVE_BACs,BAC)
# SAVE_confMat <- list.append(.data = SAVE_confMat, c(random = confMat))

SAVE_BACs
mean(as.matrix(SAVE_BACs))
#SAVE_confMat





