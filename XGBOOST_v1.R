# Clean Memory
#rm(list = ls()) 


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


specialty <- model.matrix(~Specialty1-1,payments_merged1)
product <- model.matrix(~Product_Type_1-1,payments_merged1)
product_indicator <- model.matrix(~Related_Product_Indicator-1,payments_merged1)
third_party <- model.matrix(~Form_of_Payment_or_Transfer_of_Value-1,payments_merged1)
form <- model.matrix(~Third_Party_Recipient-1,payments_merged1)
payment <- model.matrix(~Nature_of_Payment_or_Transfer_of_Value-1,payments_merged1)
state <- model.matrix(~State-1,payments_merged1)

numerics <- cbind(payments_merged1$Company_ID,payments_merged1$Number_of_licenses, specialty, payments_merged1$Physician_ID, product, product_indicator, third_party, state, form)
matrix_train <- data.matrix(numerics)

#numberOfTrainingSamples <- round(length(payments_merged1$Ownership_Indicator) * .75)

# cross validation
k <- 5
SAVE_BACs <-c()
SAVE_confMat <- list()


#Create k equally size folds
folds <- cut(seq(1,nrow(matrix_train)),breaks=k,labels=FALSE)
#Perform k fold cross validation
for(i in 1:k){
  #i = 1
  #Segement your data by fold using the which() function 
  testIndexes <- which(folds==i,arr.ind=TRUE)
  #Use the test and train data partitions however you desire...

train_data <- matrix_train[-testIndexes,]
train_labels <- payments_merged1$Ownership_Indicator[-testIndexes]

test_data <- matrix_train[testIndexes,]
test_labels <- payments_merged1$Ownership_Indicator[testIndexes]

# put our testing & training data into two seperates Dmatrixs objects
dtrain <- xgb.DMatrix(data = train_data, label= train_labels)
dtest <- xgb.DMatrix(data = test_data, label= test_labels)


# parameters <- list(max_depth = 8,  eta = 0.05,
#                    min_child_weight = 3,
#                    subsample = 0.8,
#                  objective = "binary:logistic")
model <- xgboost(data = dtrain, # the data   
                 nround = 10, # max number of boosting iterations
                 objective = "binary:logistic")  


O_I_predicted <- predict(model, dtest)

#err <- mean(as.numeric(O_I_predicted > 0.0223862) != test_labels)
#print(paste("test-error=", err))

O_I_predicted


O_I_predicted <- as.numeric(O_I_predicted > 0.023) #2) 82-84%

# Documentation
# 0.0234187: [1] 0.8710916 0.8895030 0.7642066 0.7265802 0.8329979
# 0.023: 0.8520992 0.8908379 0.7642066 0.7260687 0.9152534



#O_I_predicted <- as.numeric(O_I_predicted > 0.0223862)
# BAC is higher in the 1) model, but test_error is also higher?


result <- cbind(O_I_predicted,test_labels)

confMat <- base::table(true=result[,2], prediction=result[,1])
confMat

sensitivity <- (confMat[2,2]/(confMat[2,2]+confMat[2,1]))
specificity <- (confMat[1,1]/(confMat[1,1]+confMat[1,2]))

library(rlist)

BAC <- (specificity+sensitivity)/2
SAVE_BACs <- c(SAVE_BACs,BAC)
SAVE_confMat <- list.append(.data = SAVE_confMat, c(cross = confMat))

}


#plus one random test:

randtestIndexes <- sample(1:nrow(matrix_train), size = length(testIndexes), replace = TRUE)
#Use the test and train data partitions however you desire...
train_data <- matrix_train[-randtestIndexes,]
train_labels <- payments_merged1$Ownership_Indicator[-randtestIndexes]

test_data <- matrix_train[randtestIndexes,]
test_labels <- payments_merged1$Ownership_Indicator[randtestIndexes]

# put our testing & training data into two seperates Dmatrixs objects
dtrain <- xgb.DMatrix(data = train_data, label= train_labels)
dtest <- xgb.DMatrix(data = test_data, label= test_labels)

model <- xgboost(data = dtrain, # the data   
                 nround = 10, # max number of boosting iterations
                 objective = "binary:logistic")  

O_I_predicted <- predict(model, dtest)

O_I_predicted <- as.numeric(O_I_predicted > 0.023) #2) 82-84%


result <- cbind(O_I_predicted,test_labels)

confMat <- base::table(true=result[,2], prediction=result[,1])
confMat

sensitivity <- (confMat[2,2]/(confMat[2,2]+confMat[2,1]))
specificity <- (confMat[1,1]/(confMat[1,1]+confMat[1,2]))


BAC <- (specificity+sensitivity)/2
SAVE_BACs <- c(SAVE_BACs,BAC)
SAVE_confMat <- list.append(.data = SAVE_confMat, c(random = confMat))

SAVE_BACs
SAVE_confMat


# TODO: IDs are not added to the result
# Results of a 5 fold cross search validation and one additional random split




