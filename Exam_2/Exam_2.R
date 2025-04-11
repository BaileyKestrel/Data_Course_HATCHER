# Bailey Hatcher BIOL3100 Exam2

# "unicef-u5mr.csv" contains a data set of UNICEF data regarding child mortality 
# rates for children under 5 years old. “U5MR” refers to ‘under-5 years old 
# mortality rate,’ and there is a time component to the data, which is also broken 
# down by country, region, and continent. The mortality rate values are expressed 
# as number of deaths (before age 5) per 1000 live births. 


library(tidyverse)
library(ggplot2)
library(janitor)
library(easystats)
# 1. read in the UNICEF data
dat <- read.csv("unicef-u5mr.csv")
str(dat)


# 2. tidy up the data
clean_dat <- dat %>% 
  pivot_longer(cols = starts_with("U5MR."),
               names_to = "date",
               values_to = "U5MR") %>% 
  mutate(date = str_remove(date, "U5MR\\.") %>% as.integer()) %>% 
  janitor::clean_names()


# 3. plot each country's U5MR over time
    # create a line plot (not a smooth trend line) for each country
    # facet by continent

plot_1 <- clean_dat %>% 
  ggplot(aes(x = date, y = u5mr, group = country_name)) +
  geom_line() +
  facet_wrap(~continent) +
  labs(x = "Year", y = "U5MR") +
  theme_minimal() +
  theme(strip.background = element_rect(fill = "gray80", color = "black"),
        strip.text = element_text(color = "black"),
        panel.border = element_rect(color = "black", fill = NA))

plot_1


# 4. save this plot as LASTNAME_Plot_1.png
ggsave("Hatcher_Plot_1.png", plot = plot_1, bg = "white")


# 5. Create another plot that shows the mean U5MR for all the countries within a 
# given continent at each year.
    # Another line plot (not smooth trendline)
    # Colored by continent

dat_mean <- clean_dat %>% 
  group_by(continent, date) %>% 
  summarise(mean_u5mr = mean(u5mr, na.rm = TRUE))

plot_2 <- dat_mean %>% 
  ggplot(aes(x = date, y = mean_u5mr, color = continent)) +
  geom_line(linewidth = 2) +
  labs(x = "Year",
       y = "Mean_U5MR",
       color = "Continent") +
  theme_minimal() +
  theme(panel.border = element_rect(color = "black", fill = NA))

plot_2

# 6. Save that plot as LASTNAME_Plot_2.png 
ggsave("HATCHER_Plot_2.png", plot = plot_2, bg = "white")


# 7. Create three models of U5MR 
# mod1 should account for only Year
mod1 <- glm(data = clean_dat, formula = u5mr ~ date)

# mod2 should account for Year and Continent
mod2 <- glm(data = clean_dat, formula = u5mr ~ date + continent)

# mod3 should account for Year, Continent, and their interaction term
mod3 <- glm(data = clean_dat, formula = u5mr ~ date * continent)


# 8. compare the three models with respect to their performance
library(performance)

compare_models(mod1, mod2, mod3)
compare_performance(mod1, mod2, mod3)
plot(compare_performance(mod1, mod2, mod3))
# based on the values and the plot from above, it is clear that mod3 is the best
# because it has the lowest RMSE, the highest AIC, AICc, and BIC weights, and the
# highest R2 value. The plot reinforces this claim that mod3 is the best because 
# each of the points are on the outside of the plot compared to mod1 and mod2. 
# mod3 is the best, then mod2, and mod1 is the worst.


# 9. plot the 3 models’ predictions
# make new columns for each model prediction
clean_dat$mod1_pred <- predict(mod1, clean_dat, type = 'response')
clean_dat$mod2_pred <- predict(mod2, clean_dat, type = 'response')
clean_dat$mod3_pred <- predict(mod3, clean_dat, type = 'response')

# tidy up the data frame again so all predictions are in one column
clean_dat_pred <- clean_dat %>% 
  select(continent, date, mod1_pred, mod2_pred, mod3_pred) %>% 
  pivot_longer(cols = starts_with("mod"),
               names_to = "model",
               values_to = "prediction") %>% 
  mutate(model = case_when(
    model == "mod1_pred" ~ "mod1",
    model == "mod2_pred" ~ "mod2", 
    model == "mod3_pred" ~ "mod3",
    TRUE ~ "error"))

# plot the predictions
clean_dat_pred %>% 
  ggplot(aes(x = date, y = prediction, color = continent)) +
  geom_line() +
  facet_wrap(~ model) +
  labs(
    title = "Model predictions",
    x = "Year",
    y = "Predicted U5MR") +
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "black", fill = NA),
    strip.background = element_rect(fill = "gray80", color = "black"),
    legend.key = element_rect(fill = "gray80", color = NA))


# 10. BONUS -  Using your preferred model, predict what the U5MR would be for 
# Ecuador in the year 2020. The real value for Ecuador for 2020 was 13 under-5 
# deaths per 1000 live births. How far off was your model prediction??? 

# create a small dataframe for Ecuador that mirrors clean_dat
ecuador_df <- data.frame(country_name = "Ecuador",
                         continent = "Americas",
                         date = 2020)

# make a prediction by plugging in ecuador_df to mod3
ecuador_df$mod3_pred <- predict(mod3, newdata = ecuador_df, type = 'response')

# this model predicted a U5MR for Ecuador in 2020 to be -10.58018. The model is
# off by 23.58018, meaning it is under predicting by a lot.
mod4 <- glm(data = clean_dat, formula = u5mr ~ date * continent + country_name)
ecuador_df$mod4_pred <- predict(mod4, newdata = ecuador_df, type = 'response')

# this new model mod4 has a much closer value to the real value by adding country_name

