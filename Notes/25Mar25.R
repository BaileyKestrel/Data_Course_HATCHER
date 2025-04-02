library(ggplot2)
library(tidyverse)

# glm is more versatile than lm 
mod <- glm(data = dat_2,
           formula = cm ~ sex)

car_insurance_price = age + gender + education + .....
t/f = 

lm() #for continuous data


## build model to predict cty (mpg in city) 
## as a function of displ(total volume) 
mpg #dataset in ggplot2

#plot first to see what data looks like
mpg %>% 
  ggplot(aes(x = displ, y = cty)) +
  geom_point()
#model if displacement affect miles per gallon (cty)
mod <- glm(data = mpg,
           formula = cty ~ displ) #dependent ~ independent(predictor)
summary(mod)

cty = 25.99 + (-2.63)*displ

mpg %>% 
  ggplot(aes(x = displ, y = cty)) +
  geom_point()+
  geom_smooth(method = 'glm', se = F)



str(mod)
mod$model
mod$formula
mod$coefficients
mod$fitted.values

cty = 25.99 + (-2.63)*displ
plot(mod$model$cty, mod$fitted.values)
cor.test(mod$model$cty, mod$fitted.values)

install.packages("easystats")
library(easystats)
report(mod)
performance(mod) # how good is the model
check_model(mod) # do any of my assumptions violate

#week 9 machine learning models explained (linear regression)... good for learing


mod <- glm(data = mpg,
           formula = cty ~ displ)
names(mpg)
mod2 <- glm(data = mpg,
           formula = cty ~ displ + year + manufacturer + model + trans + cyl + drv)
summary(mod2)
performance(mod2)



mod1 <- glm(data = mpg,
           formula = cty ~ displ)
summary(mod1)

mod2 <- glm(data = mpg,
           formula = cty ~ displ + cyl)
summary(mod2)

mod3 <- glm(data = mpg,
           formula = cty ~ displ * cyl)
summary(mod3)

mpg %>% 
  ggplot(aes(x = displ, y = cty, color = factor(cyl)))+
  geom_smooth(method = 'glm')


compare_models(mod1, mod2, mod3)
compare_performance(mod1, mod2, mod3)
compare_performance(mod1, mod2, mod3) %>% plot()


predict(mod1, mpg)
mod1$formula

plot(mod1$fitted.values, predict(mod1, mpg))

## stopped taking notes here....



