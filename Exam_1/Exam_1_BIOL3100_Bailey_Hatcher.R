library(ggplot2)
library(tidyverse)
getwd()


# I. Read the cleaned_covid_data.csv file into an R data frame. (20 pts)
covid_data <- read_csv("Exam_1/cleaned_covid_data.csv")
str(covid_data)
head(covid_data)

# II. Subset the data set to just show states that begin with “A” and save this as an object called A_states. (20 pts)

A_states <- covid_data %>% 
  filter(grepl("^A", Province_State))

head(A_states)
tail(A_states)

# III. Create a plot of that subset showing Deaths over time, with a separate facet for each state. (20 pts)

A_states$Province_State %>% unique()
is.Date(A_states$Last_Update)

A_states %>% 
  ggplot(aes(x = Last_Update, 
             y = Deaths)) +
  facet_wrap(~Province_State, scales = "free") + # Keep scales “free” in each facet
  geom_point(size = 0.5) + # Create a scatterplot
  geom_smooth(method = "loess", se = FALSE) + # Add loess curves WITHOUT standard error shading
  labs(x = "Last Update", y = "Deaths") +
  ggtitle("Covid Deaths Over Time in A_states") +
  theme_bw() +
  theme() 




# IV. (Back to the full dataset) Find the “peak” of Case_Fatality_Ratio for each state and save this as a new data frame object called state_max_fatality_rate. (20 pts)
state_max_fatality_rate <- covid_data %>% 
  group_by(Province_State) %>% 
  summarise(Maximum_Fatality_Ratio = max(Case_Fatality_Ratio, na.rm = TRUE))

# Arrange the new data frame in descending order by Maximum_Fatality_Ratio
state_max_fatality_rate <- state_max_fatality_rate %>% 
  arrange(desc(Maximum_Fatality_Ratio))
  










# V. Use that new data frame from task IV to create another plot. (20 pts)
is.factor(state_max_fatality_rate$Province_State) #check if Province_State is factor

# x-axis arranged in descending order, just like the data frame (make it a factor to accomplish this)
state_max_fatality_rate <- state_max_fatality_rate %>% 
  mutate(Province_State = factor(Province_State, levels = Province_State))



state_max_fatality_rate %>% 
  ggplot(aes(x = Province_State, y = Maximum_Fatality_Ratio)) + # X-axis is Province_State, Y-axis is Maximum_Fatality_Ratio
  geom_col() + # bar plot
  theme(
    axis.text.x = element_text(angle = 90)) # X-axis labels turned to 90 deg to be readable












# VI. (BONUS 10 pts) Using the FULL data set, plot cumulative deaths for the entire US over time

# You’ll need to read ahead a bit and use the dplyr package functions group_by() and summarize() to accomplish this.


