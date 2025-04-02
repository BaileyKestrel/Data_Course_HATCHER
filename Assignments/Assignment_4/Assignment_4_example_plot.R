library(tidyverse)
library(ggplot2)

chickadee_data <- read_csv("../../../Desktop/MAPS_data_download/MAPS_BANDING_capture_data.csv")
str(chickadee_data)
unique(chickadee_data$SPEC)

chickadee_data %>%
  filter(!is.na(WEIGHT)) %>% 
  group_by(SPEC) %>%
  summarise(avg_weight = mean(WEIGHT))




chickadee_data %>% 
  filter(!is.na(WEIGHT)) %>%
  ggplot(aes(x = SPEC, y = WEIGHT)) +
  geom_boxplot(outlier.shape = NA) +
  labs(title = "Chickadee Weight by Species", x = "Species", y = "Weight (grams)") +
  theme_minimal()



boxplot.stats(chickadee_data$WEIGHT)$out
is.numeric(chickadee_data$WEIGHT)





# Compute Q1, Q3, and IQR for weight
chickadee_clean <- chickadee_data %>%
  group_by(SPEC) %>%
  mutate(
    Q1 = quantile(WEIGHT, 0.25, na.rm = TRUE),
    Q3 = quantile(WEIGHT, 0.75, na.rm = TRUE),
    IQR = Q3 - Q1
  ) %>%
  filter(WEIGHT >= (Q1 - 1.5 * IQR) & WEIGHT <= (Q3 + 1.5 * IQR)) %>%
  select(-Q1, -Q3, -IQR)  # Remove extra columns

# Plot without outliers
chickadee_weights <- ggplot(chickadee_clean, aes(x = SPEC, y = WEIGHT)) +
  geom_boxplot() +
  labs(title = "Chickadee Weight by Species (Outliers Removed)", 
       x = "Species", y = "Weight (grams)") +
  theme_minimal()

ggsave("Assignments/Assignment_4/chickadee_weights.pdf")




age_labels <- c(
  "0" = "Indeterminable",
  "4" = "Local (Young)",
  "2" = "Hatching-year",
  "1" = "After hatching-year",
  "5" = "Second-year",
  "6" = "After second-year",
  "7" = "Third-year",
  "8" = "After third-year"
)

ggplot(chickadee_data, aes(x = AGE, fill = SPEC)) +
  geom_histogram(binwidth = 1, position = "dodge", alpha = 0.7) +
  scale_x_continuous(breaks = as.numeric(names(age_labels)), labels = age_labels) +
  labs(title = "Age Distribution of Chickadee Species", x = "Age Category", y = "Count") +
  theme_minimal() 



