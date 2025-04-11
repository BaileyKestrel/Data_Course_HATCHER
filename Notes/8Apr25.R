library(tidyverse)

## glm() vs anova (function for anova is aov)
library(palmerpenguins)
mod_aov <- aov(data = penguins,
           formula = body_mass_g ~ species + sex + year)
summary(mod_aov)
# species and sex significant on body_mass_g, but year is not
# if you have more than one group you want to compare, use anova... tells you what is significant

# do the same thing for glm()... we are now using linear model ... gives you estimation
mod_glm <- glm(data = penguins,
           formula = body_mass_g ~ species + sex + year)
summary(mod_glm)


# testing set does not affect training set... 





## diving into R Markdown (.Rmd)
# spatial format to make everything nice and compiles into html





