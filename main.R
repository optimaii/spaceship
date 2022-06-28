# https://www.kaggle.com/competitions/spaceship-titanic

library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr)
library( tidyverse )
library( tidymodels )
library( modelr )
library( readr)
library(recipes)
library(discrim)
library (klaR)
library ( kernlab)

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

# APPROACH 1
rec <- 
  recipe(train) %>%
  update_role(Transported, new_role = 'outcome') %>% 
  update_role(HomePlanet,
              CryoSleep,
              Age,
              VIP,
              RoomService,
              FoodCourt,
              ShoppingMall,
              Spa,
              VRDeck, 
              new_role = 'predictor') %>% 
  step_normalize(all_numeric_predictors()) %>% 
  step_dummy(all_nominal(),-all_outcomes()) %>% 
  step_cz(all_predictors())

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

conf_mat(train,Transported, pred)
metrics(train, Transported, pred)

# APPROACH 2 - todo 
train <- read_csv( "train.csv" )
train <- train %>%
  mutate( HomePlanet = factor(HomePlanet),
          CryoSleep = factor( CryoSleep ),
          VIP = factor( VIP ),
          Transported = factor( Transported ))

model2 <- 
  svm_poly( mode = "classification" ) %>%
  set_engine('kernlab')

wflow2 <- wflow %>% 
  update_model(model2)

fitTrain2 <- fit(wflow2,train)

train <- train %>% 
  add_predictions(fitTrain2,var='predClass2',type='class') %>% 
  mutate(pred2 = predClass2$.pred_class,
         predClass3= NULL)

train <- train %>%
  mutate( pred2 = predClass2$.pred_class)

conf_mat(train,Transported, pred2)
metrics(train, Transported, pred2)

# APPROACH 3 - todo 
train <- read_csv( "train.csv" )
train <- train %>%
  mutate( HomePlanet = factor(HomePlanet),
          CryoSleep = factor( CryoSleep ),
          VIP = factor( VIP ),
          Transported = factor( Transported ))

model3 <- mpl(hidden_units=32) %>% 
  set_engine('nnet') %>% 
  set_mode('classification')

wflowNeural <-
  workflow() %>% 
  add_model(model3) %>% 
  add_recipe(rec)

fitTrain2 <- fit(wflow2,train)

train <- train %>% 
  add_predictions(fitTrain2,var='predClass2',type='class') %>% 
  mutate(pred2 = predClass2$.pred_class,
         predClass3= NULL)

train <- train %>%
  mutate( pred2 = predClass2$.pred_class)

conf_mat(train,Transported, pred2)
metrics(train, Transported, pred2)

"Hey im Christopher"


