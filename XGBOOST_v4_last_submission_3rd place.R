### Imports ####
library(tidyverse)
library(tidymodels)
library(skimr) #used for skim() function, to skim dataframes
library(lubridate)
# library(Amelia) #used for missmap() function, to graph missing values in a df
library(xgboost)
library(base)
library(rlist)
library(Matrix)

options(dplyr.width = Inf) # show all columns when printing to console
theme_set(theme_minimal()) 
set.seed(2021)
########################################### Preprocessing ################################


## preprocessing ###
preprocessing <- function(
  physicians, payments, companies
){
  physicians <- physicians %>%
    select(
      -Name_Suffix,
      -First_Name,
      -Last_Name,
      -Middle_Name,
      -Zipcode,
      -Province, #It is empty
      -City,
      -State, # almost equal to lincensed_state_1
      -Country
      # -starts_with("License_State_")
    ) %>% 
    separate(
      Primary_Specialty,
      c('Specialty1', 'Specialty2', 'Specialty3'), 
      "\\|"
    )
    
    transactions <- payments %>% 
    select(-ends_with("Travel"), -starts_with("Product_Name"),
           -starts_with("Product_C"),
           -Contextual_Information, -Date)

  
    companies <- companies %>% select(-Name) %>% 
        rename(
          Company_State = State,
          Company_Country = Country
        )
  
    df <- transactions %>% 
      left_join(physicians, 
                by = c("Physician_ID" = "id")) %>%
      left_join(companies, 
                by = c("Company_ID" = "Company_ID"))
  
  #Tidy the physicians data-set + Change types of columns in the data-sets
  df <- df %>% 
    rename(id = Physician_ID) %>%
    mutate(
      Ownership_Indicator = case_when(
        Ownership_Indicator == 'Yes' ~ 1,
        Ownership_Indicator == 'No' ~ 0
      ),
      Company_ID = factor(Company_ID)
      # Number_of_related_products= rowSums(!is.na(df %>% select(starts_with("Product_Type_"))))
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
                              df$Product_Type_3))
  physicians <- physicians %>% mutate(
    License_State_1 = factor(License_State_1, levels = states),
    License_State_2 = factor(License_State_2, levels = states),
    License_State_3 = factor(License_State_3, levels = states),
    License_State_4 = factor(License_State_4, levels = states),
    License_State_5 = factor(License_State_5, levels = states),
    Specialty1 = factor(Specialty1, levels = specialties),
    Specialty2 = factor(Specialty2, levels = specialties),
    Specialty3 = factor(Specialty3, levels = specialties)
  )
  
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
  



aggregate_df <- function(df){

  df <- df %>% 
    group_by(id) %>%
    mutate(
      Payment_Physician = sum(Total_Amount_of_Payment_USDollars),
      NumberOfPayments_Physician = sum(Number_of_Payments),
      mean_Payment_Physician = Payment_Physician/NumberOfPayments_Physician,
      median_Payment_Physician = median(Total_Amount_of_Payment_USDollars) 
    ) %>% ungroup()
    
  nature <- df %>% 
    select(id, Nature_of_Payment_or_Transfer_of_Value, Total_Amount_of_Payment_USDollars, Payment_Physician) %>%
    group_by(id, Nature_of_Payment_or_Transfer_of_Value) %>%
    mutate(
      NatureOfPayment = paste("NATURE_", Nature_of_Payment_or_Transfer_of_Value),
      Sum_PaymentbyNature_Physician = sum(Total_Amount_of_Payment_USDollars),
      PercentPaymentbyNature_Physician = 
        Sum_PaymentbyNature_Physician/Payment_Physician * 100
    ) %>% ungroup() %>%
    distinct(id, NatureOfPayment, PercentPaymentbyNature_Physician) %>% 
    pivot_wider(
      names_from = NatureOfPayment, values_from = PercentPaymentbyNature_Physician
    ) %>% arrange(id)
    
  form <- df %>% 
    select(id, Form_of_Payment_or_Transfer_of_Value, Total_Amount_of_Payment_USDollars, Payment_Physician) %>%
    group_by(id, Form_of_Payment_or_Transfer_of_Value) %>%
    mutate(
      FormOfPayment = paste("FORM_", Form_of_Payment_or_Transfer_of_Value),
      PercentPaymentbyForm_Physician = 
        sum(Total_Amount_of_Payment_USDollars)/Payment_Physician * 100
    ) %>% ungroup() %>%
    distinct(id, FormOfPayment, PercentPaymentbyForm_Physician) %>% 
    pivot_wider(
      names_from = FormOfPayment, values_from = PercentPaymentbyForm_Physician
    ) %>% arrange(id)
  

  comp <- df %>% 
    select(id, Company_ID, Total_Amount_of_Payment_USDollars) %>%
    group_by(id, Company_ID) %>%
    mutate(
      Company = paste("COMP_", Company_ID),
      TransactionBetweenPhysicianCompany = sum(Total_Amount_of_Payment_USDollars)
    ) %>% ungroup() %>%
    distinct(id, Company, TransactionBetweenPhysicianCompany) %>% 
    pivot_wider(
      names_from = Company, values_from = TransactionBetweenPhysicianCompany
    ) %>% arrange(id) 
  
  
  # only use the Product_Type_1
  prod <- df %>% 
    select(id, Product_Type_1, Total_Amount_of_Payment_USDollars, Payment_Physician) %>%
    group_by(id, Product_Type_1) %>%
    mutate(
      Type = paste("Type_", Product_Type_1),
      PercentPaymentbyType_Physician = 
        sum(Total_Amount_of_Payment_USDollars)/Payment_Physician * 100
    ) %>% ungroup() %>% distinct(id, Product_Type_1, PercentPaymentbyType_Physician) %>%
    # pivot_longer(
    #   cols = starts_with("Product_Type"),
    #   names_to = "Type",
    #   values_to = "Product_Type",
    #   values_drop_na = T) %>%
    # distinct() %>% 
    pivot_wider(
      names_from = Product_Type_1, values_from = PercentPaymentbyType_Physician
    ) %>% arrange(id)
  
    states <- df %>% 
      select(id, starts_with("License_State_")) %>%
      pivot_longer(
        cols = starts_with("License_State_"),
        names_to = "LState_Indicator",
        values_to = "LState",
        values_drop_na = T) %>%
       mutate(
         LState_Indicator = LState,
        LState_Indicator = case_when(
          LState_Indicator == 'DUMMY' ~ 0,
          LState_Indicator != 'DUMMY' ~ 1
        )
      ) %>% filter(LState != "DUMMY") %>% distinct() %>%
      pivot_wider(
        names_from = LState, values_from = LState_Indicator
      ) %>% arrange(id)
    
    spec <- df %>% 
      select(id, starts_with("Specialty")) %>%
      pivot_longer(
        cols = starts_with("Specialty"),
        names_to = "Specialty_Indicator",
        values_to = "Specialty",
        values_drop_na = T) %>%
      mutate(
        Specialty_Indicator = Specialty,
        Specialty_Indicator = case_when(
          Specialty_Indicator == 'DUMMY' ~ 0,
          Specialty_Indicator != 'DUMMY' ~ 1
        )
      ) %>% filter(Specialty_Indicator != "DUMMY") %>% distinct() %>%
      pivot_wider(
        names_from = Specialty, values_from = Specialty_Indicator
      ) %>% arrange(id)
    
    
    OI <- df %>% group_by(id) %>%
      mutate(
        O_I = as.numeric(any(Ownership_Indicator == 1)),
        # model_prediction = any(model_prediction== "1")
      ) %>% ungroup() %>% distinct(id, O_I) %>% arrange(id)
    
    new_df <- cbind(OI,
                    nature %>% select(-id), 
                    form %>% select(-id), 
                    comp %>% select(-id),
                    prod %>% select(-id),
                    states %>% select(-id),
                    spec %>% select(-id)
                    )
    # remove dummy cols
    
    
    new_df <- new_df %>% select(-ends_with("DUMMY")) %>%
      mutate(across(where(~any(is.na(.))),
                    ~replace_na(., 0))
      )
    
 return(new_df)   
}
  






final_preparation <- function(train, test){
  traincols <- colnames(train) 
  testcols <- colnames(test) 
  missingcols <- traincols[!traincols %in% testcols ]

  
  m <- data.frame(matrix(0, ncol = length(missingcols), nrow = dim(test)[1]))
  colnames(m) <- missingcols
  test <- cbind(test, m) %>% relocate(all_of(traincols))
  
  traincols <- colnames(train) 
  testcols <- colnames(test) 
  missingcols <- testcols[! testcols %in% traincols ]
  m <- data.frame(matrix(0, ncol = length(missingcols), nrow = dim(train)[1]))
  colnames(m) <- missingcols
  train <- cbind(train, m) %>% relocate(all_of(testcols))
  
  train_mat <- data.matrix(train, rownames.force = NA)
  test_mat <- data.matrix(test, rownames.force = NA)
  return(list(train_mat, test_mat))
  
}



## Predictor return the model
CHAP_Predictor <- function(
  matrix = NULL,
  eta = 0.5,
  nround = 50,
  subsample = 0.8,
  gamma = 1,
  scale_pos_weight = 500,
  max.depth = 10
){
  
  # sink("myfilename", append=FALSE, split=TRUE)  # for screen and log
  
  ids <- matrix[,1]
  labels <- matrix[,2]
  matrix <- matrix[, c(-1, -2)]
  
  xgb.DMatrix <- xgb.DMatrix(data = matrix, label= labels)
  model <- xgboost(data = xgb.DMatrix, # the data
                   nround = nround, # max number of boosting iterations
                   eta = eta,
                   subsample = subsample,
                   gamma = gamma, 
                   scale_pos_weight = scale_pos_weight,
                   # for unbalanced classes 
                   # scale_pos_weight is the ratio between the classes
                   # scale_pos_weight makes the train-logloss higher but the
                   # BAC higher (because of different weighting)
                   objective = "binary:logistic",
                   max.depth = max.depth,
                   early_stopping_rounds = 5,
                   eval_metric = "logloss",
                   print_every_n = 10
  )
  
  return(model)
}


##predict
(
  matrix = NULL,
  eta = 0.5,
  nround = 50,
  subsample = 0.8,
  scale_pos_weight = 700,
  max.depth = 10
)

## load the data and preprocessing


companies <- read.csv('companies.csv', stringsAsFactors = F)
payments <- read.csv('payments.csv', stringsAsFactors = F)
physicians <- read.csv('physicians.csv', stringsAsFactors = F)

df <- preprocessing(companies = companies, payments = payments, physicians = physicians)

## separate the train and test dataset
test_id <- physicians %>% filter(set == "test") %>% distinct(id)
train_id <- physicians %>% filter(set == "train") %>% distinct(id)

df_train <- df %>% filter(df$id %in% train_id$id)
df_test <- df %>% filter(df$id %in% test_id$id)

train_aggregated_df <- aggregate_df(df_train)
test_aggregated_df <- aggregate_df(df_test)

# to make sure that both df having the same cols


# 
# train_aggregated_df <- train_aggregated_df %>% 
#   mutate(across(where(~any(is.na(.))),
#                 as.numeric)
#   )

# A <- as(train_aggregated_df, "sparseMatrix")  
# matCSC <- as(matrix(train_aggregated_df), "dgCMatrix")
# A <- as(train_aggregated_df, "sparseMatrix")














#########################################cv###############################################

k = 3
cv_ids<-train_id[sample(nrow(train_id)), ]

folds <- cut(seq(1, length(cv_ids)),breaks= k,labels=FALSE)

bestBAC <- 0
bestParams <- NULL
bestConfMat <- NULL
cv_params <- data.frame(
  eta =
    c(0.45, 0.45, 0.45),
  subsample =
    c(0.9,0.8, 0.7),
  scale_pos_weight =
    c(480, 480,480),
  max.depth =
    c(20, 20, 20),
  gamma =
    c(3, 3, 3)
)



for(i in 1:k){
  sink("log.branchpies.txt", append=TRUE, split=TRUE)

  testData <- cv_ids[folds==i]
  trainData <- cv_ids[folds!=i]
  cv_train <- train_aggregated_df %>% filter(train_aggregated_df$id %in% trainData)
  cv_test <- train_aggregated_df %>% filter(train_aggregated_df$id %in% testData)

  
  cv_mat <- final_preparation(train = cv_train, test = cv_test)
  cv_train_mat <- cv_mat[[1]]
  cv_test_mat <- cv_mat[[2]]
  

  cv_model <- CHAP_Predictor(
    matrix = cv_train_mat,
    eta = cv_params$eta[i],
    nround = 120,
    gamma = cv_params$gamma[i],
    subsample = cv_params$subsample[i],
    scale_pos_weight = cv_params$scale_pos_weight[i],
    max.depth = cv_params$max.depth[i]
  )
  
  
  
  
  cv_O_I_predicted <- Predict_CHAP(model = cv_model, matrix = cv_test_mat)
  
  cv_O_I_predicted <- as.numeric(cv_O_I_predicted > 0.5)
  
  
  
  cv_result <- data.frame(id = cv_test_mat[, 1], truth =  cv_test_mat[, 2], 
                           prediction = cv_O_I_predicted)
  
  
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
                       "gamma" = cv_params$gamma[i]
                       )
  }
  print(BAC)
  cat(paste("\n","Starting iteration",i,"\n"))
  sink()
}

bestConfMat
bestParams
bestBAC


#########################################Submission#######################################

mat <- final_preparation(train = train_aggregated_df, test = test_aggregated_df)
train_mat <- mat[[1]]
test_mat <- mat[[2]]

rm(mat, df, physicians, payments, companies)

model <- CHAP_Predictor(
  matrix = train_mat,
  eta = bestParams[['eta']],
  gamma = bestParams[['gamma']],
  nround = 150,
  subsample = bestParams[['subsample']],
  scale_pos_weight = bestParams[['scale_pos_weight']],
  max.depth = bestParams[['max.depth']]
)




O_I_predicted <- Predict_CHAP(model = model, matrix = test_mat)

O_I_predicted <- as.numeric(O_I_predicted > 0.5)



submission <- data.frame(id = test_mat[, 1], prediction = O_I_predicted)


version = "V4"
write_csv(submission, paste("CHAP_", version, ".csv"))



  # 
  # new_df$OI <- 0
  # new_df <- new_df %>% 
  #   select(
  #     -starts_with("Specialty"),
  #     -starts_with("License")
  #   ) 
  # 
  # new_df <- new_df %>% relocate(OI)
  # train <- new_df %>% filter(set=="train") %>% select(-set)
  # 
  # 
  
  # library(bnclassify)
  # naive <- bnc(
  #   'nb',
  #   'OI',
  #   new_df[,c(-1,-2)],
  #   1)  
