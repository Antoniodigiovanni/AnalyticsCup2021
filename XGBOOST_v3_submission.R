### Imports ####
library(tidyverse)
library(tidymodels)
# library(skimr) #used for skim() function, to skim dataframes
# library(lubridate)
# library(Amelia) #used for missmap() function, to graph missing values in a df
library(xgboost)
library(base)
library(rlist)
library(Matrix)

options(dplyr.width = Inf) # show all columns when printing to console
theme_set(theme_minimal()) 
set.seed(2021)
########################################### Define functions ################################
 

## Pre-Processing
preprocessing <- function(
  physicians = NULL,
  payments = NULL,
  companies = NULL
){
  #Todo: check input is not na

  df <- physicians %>%
    select(
      -set,
      -Name_Suffix,
      -First_Name,
      -Last_Name,
      -Middle_Name,
      -Zipcode,
      -Province, #It is empty
      -State, # almost equal to lincensed_state_1
      -Country
      # -starts_with("License_State_")
    ) %>% 
    separate(
      Primary_Specialty,
      c('Specialty1', 'Specialty2', 'Specialty3'), 
      "\\|"
    ) %>%
   left_join(
    payments %>% 
      select(-ends_with("Travel"), -starts_with("Product_Name"),
             -starts_with("Product_C"),
             -Contextual_Information),
    by = c("id" = "Physician_ID")
  )  %>% left_join(
    companies %>% select(-Name) %>% 
      rename(
        Company_State = State,
        Company_Country = Country
      ),
    by = c("Company_ID" = "Company_ID")
  )
  

  
  #Tidy the physicians data-set + Change types of columns in the data-sets
  df <- df %>% 
    mutate(
      Number_of_licenses = 
        rowSums(!is.na(df %>% select(starts_with("License_State_")))
        )
    ) %>%
    mutate(
      Number_of_related_products= rowSums(!is.na(df %>% select(starts_with("Product_Type_")))) #Number of products included in the transaction
    ) %>%  
    mutate(
      Date = as.Date(Date, format = "%m/%d/%Y"),
      Transaction_Y = year(Date),
      Transaction_M = month(Date)
    ) %>% 
    mutate(
      Ownership_Indicator = case_when(
        Ownership_Indicator == 'Yes' ~ 1,
        Ownership_Indicator == 'No' ~ 0
      )
    ) %>% 
    relocate(Record_ID, id, Company_ID, Ownership_Indicator) 

  #handling NA values
  # nullcols <- colnames(df %>%select_if(~any(is.na(.))))  
  df <- df %>% mutate(across(where(~any(is.na(.))),
                             ~replace_na(., "DUMMY"))
  )
  
  # cast to factor
  states <- Reduce(union, list(df$License_State_1, df$License_State_2, 
                               df$License_State_3, df$License_State_4, 
                               df$License_State_5))
  
  specialties <- Reduce(union, list(df$Specialty1, df$Specialty2, 
                               df$Specialty3))
  types <- Reduce(union, list(df$Product_Type_1, df$Product_Type_2, 
                              df$Product_Type_))
  
  df <- df %>% mutate(
    License_State_1 = factor(License_State_1, levels = states),
    License_State_2 = factor(License_State_2, levels = states),
    License_State_3 = factor(License_State_3, levels = states),
    License_State_4 = factor(License_State_4, levels = states),
    License_State_5 = factor(License_State_5, levels = states),
    Specialty1 = factor(Specialty1, levels = specialties),
    Specialty2 = factor(Specialty2, levels = specialties),
    Specialty3 = factor(Specialty3, levels = specialties),
    Product_Type_1 = factor(Product_Type_1, levels = types),
    Product_Type_2 = factor(Product_Type_2, levels = types),
    Product_Type_3 = factor(Product_Type_3, levels = types)
    
    )%>%   mutate(
      across(where(is.character),
             ~factor(., levels = unique(.)))
    )
  return(df)
}

prepare_DMatrices <- function(
  df = NULL
){
  ## creating design matrices for attributes of  factor type 
  
  M_specialty_1 <- sparse.model.matrix(~Specialty_-1, df %>% mutate(Specialty_ = Specialty1))
  M_specialty_2 <- sparse.model.matrix(~Specialty_-1, df %>% mutate(Specialty_ = Specialty2))
  M_specialty_3 <- sparse.model.matrix(~Specialty_-1, df %>% mutate(Specialty_ = Specialty3))
  M_specialty <- M_specialty_1 + M_specialty_2 + M_specialty_3
  rm(list=ls(pattern="^M_specialty_"))
  
  # M_Product <- model.matrix(~Product_Type_1-1,df)
  M_Product_1 <- sparse.model.matrix(~Product_-1, df %>% mutate(Product_ = Product_Type_1))
  M_Product_2 <- sparse.model.matrix(~Product_-1, df %>% mutate(Product_ = Product_Type_2))
  M_Product_3 <- sparse.model.matrix(~Product_-1, df %>% mutate(Product_ = Product_Type_3))
  M_Product <- M_Product_1 + M_Product_2 + M_Product_3
  rm(list=ls(pattern="^M_Product_"))
  
  M_LStates_1 <- sparse.model.matrix(~LStates_-1, df %>% mutate(LStates_ = License_State_1))
  M_LStates_2 <- sparse.model.matrix(~LStates_-1, df %>% mutate(LStates_ = License_State_2))
  M_LStates_3 <- sparse.model.matrix(~LStates_-1, df %>% mutate(LStates_ = License_State_3))
  M_LStates_4 <- sparse.model.matrix(~LStates_-1, df %>% mutate(LStates_ = License_State_4))
  M_LStates_5 <- sparse.model.matrix(~LStates_-1, df %>% mutate(LStates_ = License_State_5))
  M_LStates <- M_LStates_1 + M_LStates_2 + M_LStates_3 + M_LStates_4 + M_LStates_5
  rm(list=ls(pattern="^M_LStates_"))
  
  
  M_Form <- sparse.model.matrix(~Form_of_Payment_or_Transfer_of_Value-1,df)
  M_Nature <- sparse.model.matrix(~Nature_of_Payment_or_Transfer_of_Value-1,df)
  M_Charity <- sparse.model.matrix(~Charity-1,df)
  M_ThirdParty_C <- sparse.model.matrix(~Third_Party_Covered-1,df)
  M_ThirdParty_R <- sparse.model.matrix(~Third_Party_Recipient-1,df)
  M_RelProInd <- sparse.model.matrix(~Related_Product_Indicator-1,df)
  M_ComState <- sparse.model.matrix(~Company_State-1,df)
  M_ComCountry <- sparse.model.matrix(~Company_Country-1,df)
  
  
  # a <- paste(ls(pattern = "^M"), sep =" ", collapse = ", ")  
  
  
  
  dMatrix <- cbind(
    df$id, 
    df$Ownership_Indicator,
    df$Total_Amount_of_Payment_USDollars,
    df$Number_of_licenses,
    df$Number_of_related_products,
    # M_Charity, 
    M_ComCountry, 
    # M_ComState, 
    M_Form, 
    M_Nature, 
    M_RelProInd, 
    # M_ThirdParty_C, 
    # M_ThirdParty_R,  
    M_Product, 
    M_specialty, 
    M_LStates 
  )
  rm(list=ls(pattern="^M_"))
  
  
  #cbind can not properly handle data.frame and dgCMatrix. 
  #After binding, the colnames of data.frame are missing 
  
  dMatrix@Dimnames[[2]][1:5] = c(
    "id", "Ownership_Indicator", "Total_Amount_of_Payment_USDollars", 
    "Number_of_licenses", "Number_of_related_products")
  
  # now remove Dummy
  colnames <- dMatrix@Dimnames[[2]]
  DUMMYpos <- grep("DUMMY$", colnames, perl=TRUE, value=FALSE)
  dMatrix <- dMatrix[, !colnames %in% colnames[DUMMYpos]]
  
  # final preparation
  ids <- dMatrix[,1]
  labels <- dMatrix[,2]
  dMatrix <- dMatrix[, c(-1, -2)]
  
  xgb.DMatrix <- xgb.DMatrix(data = dMatrix, label= labels)
  
  
  return(list(xgb.DMatrix, ids, labels))
  
}

## Predictor return the model
OwnershipInterestPredictor <- function(
  xgb.DMatrix = NULL,
  eta = 0.5,
  nround = 50,
  subsample = 0.8,
  scale_pos_weight = 700,
  max.depth = 10
){
  
  # sink("myfilename", append=FALSE, split=TRUE)  # for screen and log
  
  
  model <- xgboost(data = xgb.DMatrix, # the data
                   nround = nround, # max number of boosting iterations
                   eta = eta,
                   subsample = subsample,
                   scale_pos_weight = scale_pos_weight,
                   # for unbalanced classes 
                   # scale_pos_weight is the ratio between the classes
                   # scale_pos_weight makes the train-logloss higher but the
                   # BAC higher (because of different weighting)
                   objective = "binary:logistic",
                   max.depth = max.depth,
                   early_stopping_rounds = 4,
                   eval_metric = "logloss"
                   )
  
  return(model)
}


##predict
pridict_OwnershipInterest <- function(
  model = NULL,
  xgb.DMatrix = NULL
){
  O_I_predicted <- predict(model, xgb.DMatrix)
  return(O_I_predicted)
}

######################################Preparation ######################################

# Direct output to a log file and screen:
# sink(paste("logfile", format(Sys.time(), " %b_%d_%Y")), append=FALSE, split=TRUE)

## load data
companies <- read.csv('companies.csv', stringsAsFactors = F)
payments <- read.csv('payments.csv', stringsAsFactors = F)
physicians <- read.csv('physicians.csv', stringsAsFactors = F)

## preprocessing
df <- preprocessing(physicians = physicians, 
                    payments = payments,
                    companies = companies)


## separate the test and train data
test_id <- physicians %>% filter(set == "test") %>% distinct(id)
train_id <- physicians %>% filter(set == "train") %>% distinct(id)

df_train <- df %>% filter(df$id %in% train_id$id)
df_test <- df %>% filter(df$id %in% test_id$id)



##################################### k-fold cross validation #################################
k = 3
cv_ids<-train_id[sample(nrow(train_id)), ]

folds <- cut(seq(1, length(cv_ids)),breaks= k,labels=FALSE)

bestBAC <- 0
bestParams <- NULL
bestConfMat <- NULL
cv_params <- data.frame(
  eta =
    c(0.7, 0.75, 0.65),
  subsample = 
    c(0.95,0.85, 0.9),
  scale_pos_weight = 
    c(800, 850,450),
  max.depth = 
    c(6,6,70),
  threshold = 
    c(0.5, 0.5, 0.7)
)



for(i in 1:k){
  sink("log.branchpies.txt", append=TRUE, split=TRUE)
  
  testData <- cv_ids[folds==i]
  trainData <- cv_ids[folds!=i]
  cv_train <- df_train %>% filter(df_train$id %in% trainData)
  cv_test <- df_train %>% filter(df_train$id %in% testData)

  cv_result_train <- prepare_DMatrices(cv_train)
  cv_result_test <- prepare_DMatrices(cv_test)
  # rm(payments, physicians, companies, df)
  
  cv_DMatrix_train = cv_result_train[[1]]
  cv_DMatrix_test = cv_result_test[[1]]
  result_ids = cv_result_test[[2]]
  result_labels = cv_result_test[[3]]
  
  
  
  cv_model <- OwnershipInterestPredictor(
    xgb.DMatrix = cv_DMatrix_train,
    eta = cv_params$eta[i],
    nround = 85,
    subsample = cv_params$subsample[i],
    scale_pos_weight = cv_params$scale_pos_weight[i],
    max.depth = cv_params$max.depth[i]
  )
  
  

  cv_predicted <- pridict_OwnershipInterest(
    model = cv_model, xgb.DMatrix = cv_DMatrix_test)
  
  cv_predicted <- as.numeric(cv_predicted > cv_params$threshold[i])
  cv_result <- data.frame(id = result_ids, prediction = cv_predicted, truth = result_labels)
  
  cv_result <- cv_result %>% group_by(id) %>%
    mutate(
      prediction = as.numeric(any(prediction == 1)),
      truth = as.numeric(any(truth == 1))
    ) %>% ungroup() %>% distinct(id, prediction, truth)
  ConfMat <- table(truth = cv_result$truth, prediction = cv_result$prediction)
  ConfMat
  BAC <- (ConfMat[2,2]/(ConfMat[2,2]+ConfMat[2,1]) + ConfMat[1,1]/(ConfMat[1,1]+ConfMat[1,2]))/2
  if(BAC > bestBAC){
    bestBAC <- BAC
    bestConfMat <- ConfMat
    bestParams <- list("eta" = cv_params$eta[i],
                       "subsample" = cv_params$subsample[i],
                       "scale_pos_weight" = cv_params$scale_pos_weight[i], 
                       "max.depth" = cv_params$max.depth[i], 
                       "threshold" = cv_params$threshold[i])
  }
  cat(paste("\n","Starting iteration",i,"\n"))
  sink()
  print(BAC)
}

bestBAC
bestConfMat
bestParams

############################## train the final model with best params ################################

result_train <- prepare_DMatrices(df_train)
result_test <- prepare_DMatrices(df_test)
# rm(payments, physicians, companies, df)

xgb.DMatrix_train = result_train[[1]]
xgb.DMatrix_test = result_test[[1]]
test_matrix_ids = result_test[[2]]



model <- OwnershipInterestPredictor(
  xgb.DMatrix = xgb.DMatrix_train,
  eta = bestParams[['eta']],
  nround = 30,
  subsample = bestParams[['subsample']],
  scale_pos_weight = bestParams[['scale_pos_weight']],
  max.depth = bestParams[['max.depth']]
)


threshold = bestParams[['threshold']]

O_I_predicted <- pridict_OwnershipInterest(
  model = model, xgb.DMatrix = xgb.DMatrix_test)

O_I_predicted <- as.numeric(O_I_predicted > threshold)



resultPIDs <- data.frame(id = test_matrix_ids, prediction = O_I_predicted)


submission <- resultPIDs %>% group_by(id) %>%
  mutate(
    prediction = as.numeric(any(prediction == 1)),
    # model_prediction = any(model_prediction== "1")
  ) %>% ungroup() %>% distinct(id, prediction)


version = "V2"
write_csv(submission, paste("CHAP_", version, ".csv"))



