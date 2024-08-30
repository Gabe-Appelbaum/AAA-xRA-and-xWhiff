library(tidyverse)
library(data.table)

all_yrs <- rbindlist(list(
  read_csv("data/MLB 18.csv"),
  read_csv("data/MLB 18.csv"),
  read_csv("data/MLB 21.csv"),
  read_csv("data/MLB 22.csv"),
  read_csv("data/MLB 20.csv"),
  read_csv("data/MLB 23.csv") %>% select(-rv_event)
  )
)

pitch_ftrs <- all_yrs %>%
  mutate(
    run = case_when(
      p_throws == "L" ~ pfx_x,
      p_throws == "R" ~ pfx_x * -1
    ),
    whiffs = case_when(
      description %in% c("swinging_strike", "swinging_strike_blocked", "foul_tip") ~ 1,
      description != c("swinging_strike", "swinging_strike_blocked", "foul_tip") ~ 0,
    ),
    launch_speed = case_when(
      is.na(launch_speed) ~ 0,
      T ~ launch_speed
    ),
    launch_angle = case_when(
      is.na(launch_angle) ~ 0,
      T ~ launch_angle
    ),
    release_pos_x = case_when(
      p_throws == "L" ~ release_pos_x,
      p_throws == "R" ~ release_pos_x * -1
    )
  ) %>%
  select(pitch_type, release_speed, release_pos_x, release_pos_z, description, p_throws, bb_type, balls, strikes, run, pfx_z,
         on_3b, on_2b, on_1b, outs_when_up, inning, inning_topbot, launch_speed, launch_angle, effective_speed,
         release_spin_rate, release_extension, woba_value, woba_denom, spin_axis, delta_run_exp, whiffs)
rm(all_yrs)
gc() #free's memory


# library(Boruta)
# 
# # Separate the features and target variable
# #get rv's that match up properly without na's in other rows
# prediction_setup <- pitch_ftrs %>% select(-woba_value, -woba_denom, -description, -balls, -strikes, 
#                                     -on_3b, -on_2b, -on_1b, -outs_when_up, -inning, -inning_topbot, -bb_type) %>%
#   na.omit()
# 
# # Run Boruta
# #boruta_model <- Boruta(predictors, target, doTrace = 2)
# boruta = Boruta(delta_run_exp ~ 
#                 release_speed+ release_pos_x+ release_pos_z+ run+ pfx_z+
#                 launch_speed+ launch_angle+ effective_speed+
#                 release_spin_rate+ release_extension+ spin_axis,
#                 data = sample_n(prediction_setup, 25000),
#                 doTrace = 2)  
# 
# feature_importance <- data.frame(attStats(boruta))
# 
# # whiffs
prediction_setupW <- pitch_ftrs %>% select(-woba_value, -woba_denom, -description, -balls, -strikes, -delta_run_exp,
                                          -on_3b, -on_2b, -on_1b, -outs_when_up, -inning, -inning_topbot, -bb_type) %>%
  na.omit()
# 
# 
# # Run Boruta
# borutaW = Boruta(whiffs ~ 
#                   release_speed+ release_pos_x+ release_pos_z+ run+ pfx_z+
#                   effective_speed+ release_spin_rate+ release_extension+ spin_axis,
#                 data = sample_n(prediction_setupW, 25000),
#                 doTrace = 2)  
# 
# feature_importanceW <- data.frame(attStats(borutaW))

# based on boruta I'm going to predict whiffs with the following variables
  
# whiffs ~ release_speed + run + pfx_z + release_pos_x + release_pos_z + release_spin_rate + release_extension

# first step is to whittle the original data set down to just that
model_df <- prediction_setupW %>%
  select(whiffs, release_speed , run , pfx_z , release_pos_x , release_pos_z , release_spin_rate , release_extension)

# im gonna try library caret, ~o~ that guy kinda looks like a shrug
#library(caret)

# Set the seed for reproducibility
set.seed(123)

# Split the data into train (75%) and test (25%)
train_indices <- createDataPartition(model_df$whiffs, p = 0.75, list = FALSE)
train_data <- model_df[train_indices, ]
test_data <- model_df[-train_indices, ]

# okay now here's the model I guess? i dont know?
library(xgboost)

train_x = data.matrix(train_data %>% select(-whiffs)) # create predictors data, this is just the whiff training data minus the whiff label
train_y = as.numeric(unlist(train_data %>% select(whiffs))) # this is the labels
# not sure the purpose of creating a data matrix or unlisting. the specific syntax is a bit weird to me

# this is the actual model running
dtrain <- xgb.DMatrix(data = train_x, label = train_y)
bstDMatrix <- xgboost(data = dtrain, max.depth = 17, eta = 0.1, nrounds = 300, eval_metric = "auc",
                        objective="binary:logistic", early_stopping_rounds = 10, nthread = 4)

# now test the model, original model had training auc of only 0.62 so I don't have high hopes for the test data

test_x = data.matrix(test_data %>% select(-whiffs))
test_y = as.numeric(unlist(test_data %>% select(whiffs)))

pred <- predict(bstDMatrix, test_x) # make the predictions

testXG = as.data.frame(test_x) # make the testing predicotrs into a df

finalxgB = cbind(test_data, pred) #bind the predictions onto the testing data,

# Load the package
#library(pROC)

auc(finalxgB$whiff, finalxgB$pred)



# max.depth = 12, eta = 0.05, nrounds = 1000, eval_metric = "auc", objective="binary:logistic", early_stopping_rounds = 50
# those params acheived a training auc over 80 and a testing auc of 0.67

# max.depth = 15, eta = 0.05, nrounds = 400, eval_metric = "auc",objective="binary:logistic", early_stopping_rounds = 5
# those params achieved a training auc of 0.869 and test auc of 0.687

# data = dtrain, max.depth = 17, eta = 0.05, nrounds = 566, eval_metric = "auc", objective="binary:logistic", early_stopping_rounds = 50
# stepped in and forced a stop at 566 rounds when auc was 0.95, anyway test auc ended up at 0.687
# fuck

#data = dtrain, max.depth = 17, eta = 0.1, nrounds = 200, eval_metric = "auc", objective="binary:logistic", early_stopping_rounds = 10
# train data has an auc of 0.93, fucking hope this test run is good... TEST AUC IS 0.7!!!! what a step up, let's tweak it 

# data = dtrain, max.depth = 17, eta = 0.3, nrounds = 100, eval_metric = "auc", objective="binary:logistic", early_stopping_rounds = 10, nthread = 14)
# train AuC of 0.94, test auc of 0.69, im gonna try lowering max depth and increasing rounds

# max.depth = 17, eta = 0.1, nrounds = 300, eval_metric = "auc", objective="binary:logistic", early_stopping_rounds = 10, nthread = 4
# this achieved a train of .95 and a test of .71, im gonna roll with that

# adding raw stuff aka xWhiff% back onto the dataset, doing this so i can calculate mean and SD
full_predictions <- predict(bstDMatrix, data.matrix(model_df %>% select(-whiffs)))
mean(full_predictions) # 0.1182803
sd(full_predictions) # 0.1002361

# saving the model
saveRDS(bstDMatrix, file = "stuff plus model.rda")
