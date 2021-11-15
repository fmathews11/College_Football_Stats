library(tidyverse)
library(tidymodels)
library(themis)
library(readr)
library(finetune)
library(vip)
library(ggthemes)
library(SHAPforxgboost)
set.seed(20175)
dd <- read.csv("https://raw.githubusercontent.com/fmathews11/College_Football_Stats/main/Allplayedgames.csv")


#Pre-Preprocessing

dd<- dd%>%
  filter(Result != 0.5)%>%
  mutate(Result = as.factor(Result))%>%
  mutate(Neutral_Location = as.factor(case_when(Neutral_Location == FALSE ~ 0, TRUE ~1)))%>%
  mutate(Team_FBS = as.factor(Team_FBS))%>%
  mutate(Opp_FBS = as.factor(Opp_FBS))%>%
  mutate(Home = as.factor(Home))%>%
  na.omit()

nextweeksgames <- read.csv('https://raw.githubusercontent.com/fmathews11/College_Football_Stats/main/next_weeks_games.csv')

nextweeksgames <- nextweeksgames%>%
  mutate(Team_FBS = as.factor(Team_FBS))%>%
  mutate(Opp_FBS = as.factor(Opp_FBS))%>%
  mutate(Home = as.factor(Home))%>%
  mutate(Neutral_Location = case_when(
    Neutral_Location == TRUE ~1,Neutral_Location == FALSE ~0
  ))%>%
  mutate(Neutral_Location = as.factor(Neutral_Location))



dd_split <- initial_split(dd,strata = Result)
dd_train <- training(dd_split)
dd_test <- testing(dd_split)

# GLMNET

glmnet_recipe <- 
  recipe(formula = Result ~ Home + Neutral_Location + ELO + Opp_ELO + Team_FBS + Opp_FBS, data = dd_train) %>% 
  step_novel(all_nominal(), -all_outcomes()) %>% 
  step_dummy(all_nominal(), -all_outcomes()) %>% 
  step_zv(all_predictors()) %>% 
  step_normalize(all_predictors(), -all_nominal()) 

juiced <- glmnet_recipe%>%prep()%>%juice()

glmnet_spec <- 
  logistic_reg(penalty = tune(), mixture = tune()) %>% 
  set_mode("classification") %>% 
  set_engine("glmnet") 

glmnet_workflow <- 
  workflow() %>% 
  add_recipe(glmnet_recipe) %>% 
  add_model(glmnet_spec) 

glmnet_grid <- tidyr::crossing(penalty = 10^seq(-6, -1, length.out = 20), mixture = c(0.05, 
                                                                                      0.2, 0.4, 0.6, 0.8, 1)) 

glm_folds <- vfold_cv(dd_train,v = 10,strata = Result)

glmnet_tune <- 
  tune_grid(glmnet_workflow, resamples = glm_folds, grid = glmnet_grid)


glmnet_tune%>%
  show_best('accuracy')

glmnet_best <- glmnet_tune%>%
  select_best('accuracy')

final_glmnet_wf <- finalize_workflow(glmnet_workflow,glmnet_best)

final_fit <- final_glmnet_wf%>%
  last_fit(dd_split)

final_fit%>%
  collect_metrics()

model_fit <- final_glmnet_wf%>%
  fit(dd_train)


nextweeksgames$Team_Win_Probability <- predict(model_fit,nextweeksgames,type = 'prob')$.pred_1

nextweeksgames%>%
  group_by(game_id)%>%
  arrange(-Team_Win_Probability)%>%
  select(Team,Opponent,Team_Win_Probability)%>%
  View()




### Spread

hist(dd_train$ELO)
hist(dd_train$Opp_ELO)
hist(dd_train$Spread)

xgboost_recipe <- 
  recipe(formula = Spread ~   Home + Neutral_Location +  Team_FBS +
           Opp_FBS + ELO + Opp_ELO, data = dd_train) %>% 
  step_novel(all_nominal_predictors()) %>% 
  step_dummy(all_nominal_predictors(), one_hot = TRUE) %>% 
  step_zv(all_predictors())%>%
  step_normalize(all_numeric_predictors())

juiced<-xgboost_recipe%>%prep()%>%juice()

num_cores <- parallel::detectCores()

xgboost_spec <- 
  boost_tree(trees = tune(), min_n = tune(), tree_depth = tune(), learn_rate = tune(), 
             loss_reduction = tune(), sample_size = tune(),stop_iter = 20) %>% 
  set_mode("regression") %>% 
  set_engine("xgboost",eval_metric = "rmse") 

xgboost_workflow <- 
  workflow() %>% 
  add_recipe(xgboost_recipe) %>% 
  add_model(xgboost_spec) 


xgb_folds <- vfold_cv(data = dd_train,v = 10,strata = Result)

doParallel::registerDoParallel(cores = num_cores)

xgboost_tune <-
  tune_race_anova(xgboost_workflow, resamples = xgb_folds, grid = 10,control = control_race(
    verbose = TRUE,
    allow_par = TRUE),
    metrics = metric_set(rmse,rsq))

xgboost_tune%>%
  collect_metrics()

xgboost_tune%>%
  show_best('rmse')

xgboost_tune%>%
  show_best('rsq')

plot_race(xgboost_tune)

xgboost_best <- xgboost_tune%>%
  select_best('rmse')

xgb_final_wf <- finalize_workflow(xgboost_workflow,xgboost_best)

final_fit <- xgb_final_wf%>%
  last_fit(dd_split)


final_fit%>%
  collect_metrics()

finalize_model(xgboost_spec,xgboost_best)%>%
  set_engine('xgboost',importance = 'impurity')%>%
  fit(formula =  Spread ~ .,data = juiced)%>%
  vip()

raw_xgb <- final_fit%>%
  extract_fit_engine()


raw_xgb$evaluation_log%>%
  ggplot(aes(x = iter,y = training_rmse))+
  geom_line(color='hotpink',size = 1.2)+
  labs(x = 'Boosting Iterations',
       y = 'Model RMSE')+
  theme_hc()

model_fit <- xgb_final_wf%>%
  fit(dd_train)

saveRDS(xgb_final_wf,'xgb_spread')

nextweeksgames$predicted_spread <- round(predict(model_fit,nextweeksgames)$.pred,digits = 0)

nextweeksgames <- nextweeksgames%>%
  arrange(-Team_Win_Probability)%>%
  mutate(Team_Win_Probability = round(Team_Win_Probability,digits = 2))%>%
  rename("Predicted_Win_Margin" = 'predicted_spread')

nextweeksgames%>%
  select(Team,Opponent,Team_Win_Probability,Predicted_Win_Margin)%>%View()
  



write.csv(nextweeksgames,'week8predictions')





