library(tidyverse)
library(tidymodels)
library(themis)
library(readr)
library(finetune)
library(vip)
library(ggthemes)
library(cfbfastR)
library(SHAPforxgboost)
 
Sys.setenv(CFBD_API_KEY = "ZmYrC0A2qVl/S+8M0W0ygbpyMtpTvGYn2lDZexO8WPP5ok0IKvxkYTJItmgNgBhh")

data <- read.csv('Level_One_With_ELO.csv',stringsAsFactors = TRUE)

NCAAF_L1_Teams <- read.csv('NCAAF_Team_List.csv')

#Matching and Pre-Processing
thisweeksgames <- read.csv('ThisWeeksGames.csv')
thisweeksgames$Team_FBS <- NCAAF_L1_Teams$FBS[match(thisweeksgames$Team,NCAAF_L1_Teams$Team)]
thisweeksgames$Opp_FBS <- NCAAF_L1_Teams$FBS[match(thisweeksgames$Opponent,NCAAF_L1_Teams$Team)]
thisweeksgames$OT <- 0
thisweeksgames$Neutral_Location <- ifelse(thisweeksgames$Neutral_Location == TRUE,1,0)
thisweeksgames$Neutral_Location <- as.factor(thisweeksgames$Neutral_Location)
thisweeksgames$OT <- as.factor(thisweeksgames$OT)
thisweeksgames$Team_FBS <- as.factor(thisweeksgames$Team_FBS)
thisweeksgames$Opp_FBS <- as.factor(thisweeksgames$Opp_FBS)


#Converting to proper data types. Pre-pre-processing
data <- data%>%
  filter(Result != 0.5)%>%
  mutate(Result = as.factor(Result))%>%
  mutate(OT = as.factor(case_when(OT == FALSE~0,TRUE ~1)))%>%
  mutate(Neutral_Location = as.factor(case_when(Neutral_Location == FALSE ~ 0, TRUE ~1)))%>%
  mutate(Team_FBS = as.factor(Team_FBS))%>%
  mutate(Opp_FBS = as.factor(Opp_FBS))
  mutate(Home = as.factor(Home))
  
data_split <- initial_split(data)
data_train <- training(data_split)
data_test <- testing(data_split)
  
## XGBoost Model ##

xgboost_recipe <- 
  recipe(formula = Result ~   Home + Neutral_Location + 
             OT + Team_FBS + Opp_FBS + ELO + Opp_ELO, data = data_train) %>% 
  step_novel(all_nominal(), -all_outcomes()) %>% 
  step_dummy(all_nominal(), -all_outcomes(), one_hot = TRUE) %>% 
  step_zv(all_predictors())%>%
    step_normalize(all_numeric_predictors())
  
juiced<-xgboost_recipe%>%prep()%>%juice()
  
num_cores <- parallel::detectCores()
  
xgboost_spec <- 
  boost_tree(trees = 500, min_n = tune(), tree_depth = tune(), learn_rate = tune(), 
               loss_reduction = tune(), sample_size = tune(),stop_iter = 50) %>% 
  set_mode("classification") %>% 
  set_engine("xgboost",n.threads = num_cores,eval_metric = "auc") 
  
xgboost_workflow <- 
    workflow() %>% 
    add_recipe(xgboost_recipe) %>% 
    add_model(xgboost_spec) 
  
set.seed(20109)
  
xgb_folds <- vfold_cv(data = data_train,v = 10,strata = Result)
  
xgboost_tune <-
    tune_grid(xgboost_workflow, resamples = xgb_folds, grid = 10,control = control_resamples(
      verbose = TRUE,
      allow_par = TRUE),
      metrics = metric_set(roc_auc))

autoplot(xgboost_tune)

#plot_race(xgboost_tune)
  
xgboost_tune%>%
  show_best()

xgboost_best <- select_best(xgboost_tune)


xgboost_final_wf <- finalize_workflow(xgboost_workflow,xgboost_best)


finalize_model(xgboost_spec,xgboost_best)%>%
  set_engine('xgboost',importance = 'impurity')%>%
  fit(formula =  Result ~ .,data = juiced)%>%
  vip(num_features = 35)

final_fit <- xgboost_final_wf%>%
  last_fit(data_split)

final_fit%>%
  collect_metrics()

preds <- final_fit%>%
  collect_predictions

caret::confusionMatrix(preds$.pred_class,preds$Result, positive = "1")

raw_xgb <- final_fit%>%
  extract_fit_engine()

raw_xgb$evaluation_log%>%
  ggplot(aes(x = iter,y = training_auc))+
  geom_line(color='hotpink',size = 1.2)+
  labs(x = 'Boosting Iterations',
       y = 'Model AUC')+
  theme_hc()






xgb.plot.tree(model = raw_xgb,trees = 1)


xgb.ggplot.shap.summary(data = as.matrix(juiced[,-4]),model = raw_xgb)



fit_workflow <- fit(xgboost_final_wf,data_train)


thisweeksgames <- thisweeksgames%>%
  na.omit()

thisweeksgames$team_predicted_win_probability <-predict(fit_workflow,thisweeksgames%>%na.omit(),type = 'prob')$.pred_0


thisweeksgames%>%
  select(Team,Opponent,Home,team_predicted_win_probability)%>%
  arrange(-team_predicted_win_probability)%>%
  View()

