library(tidyverse)
library(ggplot2)
library(modelr)
library(easystats)
library(broom)
library(fitdistrplus)
library(performance)

#### 1. load the “/Data/mushroom_growth.csv” data set ####
dat <- read.csv("../../Data/mushroom_growth.csv")
glimpse(dat)

#### 2. create several plots exploring relationships between the response and predictors ####
## Response variable is GrowthRate; all others are predictor variables
## Continuous: GrowthRate, Light, Nitrogen, Temperature; categorical: Species, Humidity

# Light vs GrowthRate
ggplot(dat, aes(x = Light, y = GrowthRate)) +
  geom_point() +
  theme_minimal() +
  geom_smooth() +
  labs(title = "GrowthRate by Light", x = "Light")

ggplot(dat, aes(x = factor(Light), y = GrowthRate)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "GrowthRate by Light", x = "Light")


# Nitrogen vs GrowthRate
ggplot(dat, aes(x = Nitrogen, y = GrowthRate)) +
  geom_point() +
  theme_minimal() +
  geom_smooth() +
  labs(title = "GrowthRate by Nitrogen", x = "Nitrogen")

ggplot(dat, aes(x = factor(Nitrogen), y = GrowthRate)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "GrowthRate by Nitrogen", x = "Nitrogen")


# Temperature vs GrowthRate
ggplot(dat, aes(x = Temperature, y = GrowthRate)) +
  geom_point() +
  theme_minimal() +
  geom_smooth() +
  labs(title = "GrowthRate by Temperature", x = "Temperature")

ggplot(dat, aes(x = factor(Temperature), y = GrowthRate)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "GrowthRate by Temperature", x = "Temperature")


# Species vs GrowthRate
ggplot(dat, aes(x = Species, y = GrowthRate)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "GrowthRate by Species", x = "Species")


# Humidity vs GrowthRate
ggplot(dat, aes(x = Humidity, y = GrowthRate)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "GrowthRate by Humidity", x = "Humidity")


# Light vs GrowthRate, colored by Humidity
ggplot(dat, aes(x = Light, y = GrowthRate, color = Humidity)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  facet_wrap(~ Species) +
  labs(title = "GrowthRate vs Light by Humidity and Faceted by Species")


#### 3. define at least 4 models that explain the dependent variable “GrowthRate” ####
# dependent = GrowthRate; independent = Species, Light, Nitrogen, Humidity, Temperature

mod1 <- glm(GrowthRate ~ Light + Nitrogen, data = dat)
summary(mod1)

mod2 <- glm(GrowthRate ~ Light * Species + Humidity, data = dat)
summary(mod2)

mod3 <- glm(GrowthRate ~ Light * Humidity + Nitrogen, data = dat)
summary(mod3)

mod4 <- glm(GrowthRate ~ Light + Nitrogen * Humidity + Temperature + Species, data = dat)
summary(mod4)


#### 4. calculate the mean sq. error of each model ####
mean(mod1$residuals^2)
mean(mod2$residuals^2)
mean(mod3$residuals^2)
mean(mod4$residuals^2)

#### 5. select the best model you tried ####
AIC(mod1, mod2, mod3, mod4)

compare_models(mod1, mod2, mod3, mod4)
compare_performance(mod1, mod2, mod3, mod4)
plot(compare_performance(mod1, mod2, mod3, mod4))

best_model <- mod2

#### 6. add predictions based on new hypothetical values for the independent variables used in your model ####
unique(dat$Light)
unique(dat$Species)
unique(dat$Humidity)

dat$Species <- as.factor(dat$Species)
dat$Humidity <- as.factor(dat$Humidity)


# Make a new dataframe with the predictor values we want to assess
new_data <- expand.grid(
  Light = c(0, 10, 20),
  Species = levels(dat$Species),
  Humidity = levels(dat$Humidity)
)

new_data$Species <- factor(new_data$Species, levels = levels(dat$Species))
new_data$Humidity <- factor(new_data$Humidity, levels = levels(dat$Humidity))

new_data$PredictGrowth <- predict(mod2, newdata = new_data)

#### 7. plot these predictions alongside the real data ####

ggplot(dat, aes(x = Light, y = GrowthRate, color = Species)) +
  geom_point(alpha = 0.6) +
  geom_line(data = new_data, aes(x = Light, y = PredictGrowth, group = interaction(Species, Humidity), linetype = Humidity), size = 1) +
  facet_wrap(~ Species) +
  theme_minimal() +
  labs(title = "Predicted (Lines) vs Observed (Points) GrowthRate",
       y = "GrowthRate",
       x = "Light")




# load the data in
non_linear_dat <- read.csv("../../Data/non_linear_relationship.csv")
# view and plot the data
glimpse(non_linear_dat)
ggplot(data = non_linear_dat, aes(x = predictor, y = response)) +
  geom_point()

# Model
linearized_mod <- lm(response ~ predictor + I(predictor^2) + I(predictor^3), data = non_linear_dat)

ggplot(non_linear_dat, aes(x = predictor, y = response)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2) + I(x^3)) +
  theme_minimal()





