library(tidyverse)
library(ggplot2)
library(gganimate)
library(ggimage)
library(ggforce)

library(grid)
library(png)
library(scales)


# load in chickadee data
chickadee_data <- read_csv("../../MAPS_data_download/MAPS_BANDING_capture_data.csv")

chickadee_data$image <- paste0("Assignments/Ugly_Plot_Contest/Chickadee_Images/", chickadee_data$SPEC, ".jpeg")


chickadee_data <- chickadee_data %>%
  filter(!is.na(DATE), !is.na(WEIGHT)) %>%
  mutate(appear_order = row_number()) 



chickadee_data <- chickadee_data %>%
  mutate(fat_label = factor(case_when(
    F == 0 ~ "Skinny queen",  
    F == 1 ~ "just a dab",
    F == 2 ~ "a little fluff",
    F == 3 ~ "halfway to chubby",
    F == 4 ~ "well-padded",
    F == 5 ~ "borbular",
    F == 6 ~ "how can you even fly?"
  ), levels = c(
    "Skinny queen",  
    "just a dab",
    "a little fluff",
    "halfway to chubby",
    "well-padded",
    "borbular",
    "how can you even fly?"
  )))



background_img <- rasterGrob(readPNG("Assignments/Ugly_Plot_Contest/Chickadee_Images/my_chickadee_crop.png"), 
                     width = unit(1, "npc"), 
                     height = unit(1, "npc"))


p <- chickadee_data %>% 
  ggplot(aes(x = as.numeric(DATE), y = WEIGHT, color = SPEC, shape = SEX, size = fat_label)) +  # map 'size' to fat_label
  annotation_custom(background_img, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +  # Background image
  labs(x = "Date relative to 1992-05-01", 
       y = "What chickadee weighs 120 grams?",
       title = "Chickadee dee dee dee dee dee dee dee dee dee dee dee",
       size = "how chumky?",
       shape = "sex") +
  scale_y_reverse(labels = scales::scientific, breaks = seq(0, 130, by = 5)) +
  geom_point(alpha = 0.5) +
  scale_color_manual(values = c("BCCH" = "red", "CBCH" = "orange", "MOCH" = "green", "BOCH" = "blue", "CACH" = "yellow")) +
  facet_zoom(y = WEIGHT > 5 & WEIGHT < 15) +
  scale_size_manual(values = c("Skinny queen" = 1, 
                               "just a dab" = 2, 
                               "a little fluff" = 3, 
                               "halfway to chubby" = 4, 
                               "well-padded" = 5, 
                               "borbular" = 6, 
                               "how can you even fly?" = 7)) +
  theme(plot.margin = margin(3, 1, 2, 2, "cm"), text = element_text(size = 12)) +
  transition_reveal(along = appear_order) +
  theme(plot.background = element_rect(fill = "lawngreen"), 
        axis.text.y = element_text(color = "white", face = "bold", size = 10, angle = 245),
        axis.text.x = element_text(color = "yellow", size = 15, angle = 4),
        
        axis.title.x = element_text(color = "white", size = 30, angle = 362),
        axis.title.y = element_text(color = "olivedrab3", size = 20),
        legend.title = element_text(color = "magenta", angle = 363),
        legend.text = element_text(size = 15, color = "palegreen"),
        text = element_text(color = "red"),
        plot.title = element_text(size = 28, face = "bold", color = "magenta", angle = 1)) +
  coord_fixed(ratio = 1)  # Set aspect ratio to prevent squishing

# Render the animation
anim <- animate(p, nframes = 50, fps = 5, width = 600, height = 500)

anim

# save the animation
anim_save("Assignments/Ugly_Plot_Contest/chickadee_ugly_plot_Bailey_Hatcher.gif", animation = anim)





chickadee_data %>% 
  filter(SPEC == "CBCC") %>% 
  View()



