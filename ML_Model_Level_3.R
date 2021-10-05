library(tidyverse)
library(tidymodels)
library(themis)
library(readr)
library(finetune)
library(vip)
library(ggthemes)
set.seed(20175)
dd <- read.csv("https://raw.githubusercontent.com/fmathews11/College_Football_Stats/main/Allplayedgames.csv")


#Pre-Preprocessing

dd<- dd%>%
  filter(Result != 0.5)%>%
  mutate(Result = as.factor(Result))%>%
  mutate(Neutral_Location = as.factor(case_when(Neutral_Location == FALSE ~ 0, TRUE ~1)))%>%
  mutate(Team_FBS = as.factor(Team_FBS))%>%
  mutate(Opp_FBS = as.factor(Opp_FBS))%>%
  mutate(Home = as.factor(Home))

dd_split <- initial_split(dd)
dd_train <- training(dd_split)
dd_test <- testing(dd_split)

#LEAVING THIS HERE FOR NOW _ COME BACK TO IT LATER...

svm_spec <- svm_linear()%>%
  set_engine("LiblineaR")%>%
  set_mode("classification")

show_engines('svm_linear')

svm_recipe <- recipe(data = dd_train, formula = Result ~ Home + Neutral_Location + Team_FBS + Opp_FBS + ELO + Opp_ELO)%>%
  step_dummy(all_nominal_predictors())%>%
  step_normalize(all_numeric_predictors())

svm_recipe %>% prep()

svm_workflow <- workflow()%>%
  add_recipe(svm_recipe)%>%
  add_model(svm_spec)


svm_folds <- vfold_cv(dd_train,v = 10, strata = Result)


svm_fit <- fit_resamples(svm_workflow,resamples = svm_folds)

svm_fit%>% collect_Metrics()


cfbfastR::cfbd_calendar()


# KKNN MODEL

kknn_recipe <- 
  recipe(formula = Result ~ Home + Neutral_Location + Team_FBS + Opp_FBS + ELO + 
           Opp_ELO, data = dd_train) %>% 
  step_novel(all_nominal_predictors()) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_zv(all_predictors()) %>% 
  step_normalize(all_numeric_predictors()) 

kknn_spec <- 
  nearest_neighbor(neighbors = tune(), weight_func = tune()) %>% 
  set_mode("classification") %>% 
  set_engine("kknn") 

kknn_workflow <- 
  workflow() %>% 
  add_recipe(kknn_recipe) %>% 
  add_model(kknn_spec) 

knn_folds <- vfold_cv(dd_train,v = 10, strata = Result)


kknn_tune <-
  tune_race_anova(kknn_workflow, resamples = knn_folds, grid = 10,metrics = metric_set(roc_auc,accuracy),control = control_race(
    verbose = TRUE,allow_par = TRUE
  ) )

plot_race(kknn_tune)

kknn_tune%>%autoplot()

show_best()

finalize_workflow(kknn_workflow,select_best(kknn_tune))%>%
  last_fit(dd_split)%>%
  collect_metrics()

# NeuralNetwork

install.packages("keras")

nn_recipe <-recipe(formula = Result ~ Home + Neutral_Location + Team_FBS + Opp_FBS + ELO + 
                     Opp_ELO, data = dd_train) %>% 
  step_novel(all_nominal_predictors()) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_zv(all_predictors()) %>% 
  step_normalize(all_numeric_predictors())

nn_recipe%>%prep()

nn_spec <- mlp(hidden_units = tune(),
               penalty = tune(),
               dropout = 0,
               epochs = 500,
               activation = tune())%>%
  set_engine('keras')%>%
  set_mode('classification')

  nn_workflow <- workflow()%>%
    add_recipe(nn_recipe)%>%
    add_model(nn_spec)

  nn_folds <- vfold_cv(dd_train, times = 10,strata = Result)  

nn_tune <- tune_race_anova(nn_workflow,resamples = nn_folds,grid = 10,control = control_race(
  verbose = TRUE
),
metrics = metric_set(roc_auc,accuracy))  


#### XGBOOST

xgboost_recipe <- 
  recipe(formula = Result ~   Home + Neutral_Location +  Team_FBS +
           Opp_FBS + ELO + Opp_ELO, data = dd_train) %>% 
  step_novel(all_nominal(), -all_outcomes()) %>% 
  step_dummy(all_nominal(), -all_outcomes(), one_hot = TRUE) %>% 
  step_zv(all_predictors())%>%
  step_normalize(all_numeric_predictors())

juiced<-xgboost_recipe%>%prep()%>%juice()

num_cores <- parallel::detectCores()

xgboost_spec <- 
  boost_tree(trees = 2000, min_n = tune(), tree_depth = tune(), learn_rate = tune(), 
             loss_reduction = tune(), sample_size = tune(),stop_iter = 20) %>% 
  set_mode("classification") %>% 
  set_engine("xgboost",n.threads = num_cores,eval_metric = "auc") 

xgboost_workflow <- 
  workflow() %>% 
  add_recipe(xgboost_recipe) %>% 
  add_model(xgboost_spec) 


xgb_folds <- vfold_cv(data = dd_train,v = 10,strata = Result)

xgboost_tune <-
  tune_race_anova(xgboost_workflow, resamples = xgb_folds, grid = 10,control = control_race(
    verbose = TRUE,
    allow_par = TRUE),
    metrics = metric_set(roc_auc,accuracy))

xgboost_tune%>%show_best('accuracy')

xgb_best <- xgboost_tune%>%
  select_best(metric = 'accuracy')

final_xgb <- finalize_workflow(xgboost_workflow,xgb_best)

final_fit <- final_xgb%>%
  last_fit(dd_split)

final_fit%>%
  collect_metrics()

preds <- final_fit%>%
  collect_predictions()

caret::confusionMatrix(preds$.pred_class,preds$Result, positive = "1")

finalize_model(xgboost_spec,xgb_best)%>%
  set_engine('xgboost',importance = 'impurity')%>%
  fit(formula =  Result ~ .,data = juiced)%>%
  vip(num_features = 35)

raw_xgb <- final_fit%>%
  extract_fit_engine()

raw_xgb$evaluation_log%>%
  ggplot(aes(x = iter,y = training_auc))+
  geom_line(color='hotpink',size = 1.2)+
  labs(x = 'Boosting Iterations',
       y = 'Model AUC')+
  theme_hc()

xgb.plot.tree(model = raw_xgb,trees = 100)

xgb.ggplot.shap.summary(data = as.matrix(juiced[,-3]),model = raw_xgb)

fit_model <- fit(final_xgb,data = dd_train)

nextweeksgames <- read.csv('next_weeks_games.csv')

nextweeksgames <- nextweeksgames%>%
  mutate(Team_FBS = as.factor(Team_FBS))%>%
  mutate(Opp_FBS = as.factor(Opp_FBS))%>%
  mutate(Home = as.factor(Home))%>%
  mutate(Neutral_Location = as.factor(Neutral_Location))

predict_classprob.model_fit(final_xgb,nextweeksgames)

nextweeksgames$prediction = preds
