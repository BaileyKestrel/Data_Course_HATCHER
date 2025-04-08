library(tidyverse)
library(ggplot2)
library(janitor)
library(skimr)
library(easystats)
## Data/GradSchool_Admission.csv
## build a logical/logistic regression model and predict the admission of

dat <- read.csv('Data/GradSchool_Admissions.csv')

# logistic model b/c outcome (dependent variable) is logical
# (generalized linear model) assume predictor and outcome has linear regression

mod1 <- glm(data = dat,
           formula = as.logical(admit) ~ gre + gpa + rank,
           family = 'binomial')

dat$pred_1 <- predict(mod1, dat, type = 'response')


mod2 <- glm(data = dat,
            formula = as.logical(admit) ~ gre + gpa*rank,
            family = 'binomial')

compare_performance(mod1, mod2) %>% plot()


dat$pred_1 %>% summary()

dat %>% 
  mutate(outcome = case_when(pred_1 > 0.4 ~ 'Admit',
                             pred_1 >= 0.2 & pred_1 <= 0.4 ~ 'I do not know',
                             pred_1 < 0.2 ~ 'Not Admit')) %>% 
  mutate(accurate = case_when(admit == 1 & outcome == 'Admit' ~ TRUE,
                              admit == 0 & outcome == 'Not Admit' ~ TRUE,
                              TRUE ~ FALSE)) %>% 
  pluck('accurate') %>% 
  sum()/nrow(dat)






# automatically choose best model
library(MASS)
stepAIC()

# give the model all of the possibilities (*)
full_model <- glm(data = dat, formula = as.logical(admit) ~ gre*gpa*rank, 
                  family = 'binomial')
full_model$formula
summary(full_model)

# finds the smallest AIC and largest R^2
stepwise_mod <- stepAIC(full_model, direction = 'both')

# this shows our best model:
stepwise_mod$formula

best_model <- glm(data = dat,
                  formula = stepwise_mod$formula,
                  family = 'binomial')
# plot comparison of models
compare_performance(mod1, mod2, best_model) %>% plot()


dat$pred_2 <- predict(best_model, dat)
# why are there negative values in this logistic regression?
# need type = 'response' (turns everything to probability)
dat$pred_2 <- predict(best_model, dat, type = 'response')


dat %>% 
  mutate(outcome_2 = case_when(pred_2 > 0.4 ~ 'Admit',
                             pred_2 >= 0.2 & pred_2 <= 0.4 ~ 'I do not know',
                             pred_2 < 0.2 ~ 'Not Admit')) %>% 
  mutate(accurate = case_when(admit == 1 & outcome_2 == 'Admit' ~ TRUE,
                              admit == 0 & outcome_2 == 'Not Admit' ~ TRUE,
                              TRUE ~ FALSE)) %>% 
  pluck('accurate') %>% 
  sum()/nrow(dat)

# this model is slightly better than pred_1





library(caret)
createDataPartition()

# did a random separation in our data to have 80% in one group and 20% in another
# 80% go to training model (train_mod), 20% used to test the model (dat_test)
id <- createDataPartition(dat$admit, p = 0.8, list = F)
dat_train <- dat[id, ]
dim(dat_train) #320 6
dim(dat) # 400 6

dat_test <- dat[-id, ]

# only using dat_train which only use 80% of the data (from above)
train_mod <- glm(data = dat_train,
                  formula = stepwise_mod$formula,
                  family = 'binomial')

dat_test$pred <- predict(train_mod, dat_test, type = 'response')

# training the model, you only use a small chunk, but then test it with a larger
# chunk to check the validity. Need to have a good training model and a well rounded
# testing set. Using known data to predict unknown data... why do we separate two
# data sets?