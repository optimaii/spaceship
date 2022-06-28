install.packages("magrittr") # package installations are only needed the first time you use it
install.packages("dplyr")    # alternative installation of the %>%

library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr)
library( tidyverse )
library( tidymodels )
library( modelr )
library( readr)
library(recipes)

# DATA PRE-PROCESSING
train <- read_csv( "train.csv" )
test <- read_csv('test.csv')

summary(train)
train <- train %>%
   mutate( HomePlanet = factor(HomePlanet),
           CryoSleep = factor( CryoSleep ),
           VIP = factor( VIP ),
           Transported = factor( Transported ))

test <- test %>%
  mutate( HomePlanet = factor(HomePlanet),
          CryoSleep = factor( CryoSleep ),
          VIP = factor( VIP ))

# MODEL
rec <- 
  recipe(train) %>%
  update_role(Transported, new_role = 'outcome') %>% 
  update_role(HomePlanet,CryoSleep,Age,VIP,RoomService,FoodCourt,ShoppingMall,Spa,VRDeck, new_role = 'predictor') %>% 
  step_normalize(all_numeric_predictors())

model <- logistic_reg()

wflow <-
  workflow() %>% 
  add_model(model) %>% 
  add_recipe(rec)

fitTrain <- fit(wflow,train)
glance(fitTrain)

train <- train %>% 
  add_predictions(fitTrain,var='predClass',type='class') %>% 
  add_predictions(fitTrain,var='predProb',type='prob')

train <- train %>%
   mutate( pred = predClass$.pred_class)

train$pred

# EVALUATE
conf_mat(train,Transported, pred)
metrics(train, Transported, pred)










