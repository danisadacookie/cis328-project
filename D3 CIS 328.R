install.packages("tidyverse")
install.packages("ggpubr")
install.packages("rstatix")
install.packages("datarium")
library(tidyverse)
library(ggpubr)
library(rstatix)
library(datarium)
install.packages("rio")
library("rio")

penguins = import("Downloads/penguins.csv")
view(penguins)
summary(penguins)

# independent variable: species: Adelie, Chinstrap, Gentoo
# dependent variable: culmen depth

# check the assumptions of ANOVA
# check for outliers
penguins %>%
  group_by(Species) %>%
  identify_outliers(`Culmen Depth (mm)`) 
# One outliner, but not extreme outliers

# build linear model
model <- lm(`Culmen Depth (mm)` ~ Species, data = penguins)
# create a QQ plot of residuals to show the correlation between a given data 
# and the normal distribution. Points falling along the reference line 
# indicates normal distribution
ggqqplot(residuals(model))
# use the Shapiro Wilk test of normality 
shapiro_test(residuals(model))
# p value of the test is bigger than 0.05. The data is normally distributed

# Now let's check the homogeneity of variance assumption
# Levenes' test of homogeneity is widely used
penguins %>%
  levene_test(`Culmen Depth (mm)` ~ Species)

# The p value of the Levenes' test is non-significant, confirming homogeneity
# of variance.

# Compute the ANOVA
pg.aov <- penguins %>% anova_test(`Culmen Depth (mm)` ~ Species)
pg.aov

# p = 1.45e-81 <0.05. So there is significant difference among the three groups

# post-hoc tests
# We'll use the Tukey's test to know the specific groups between which the difference exists
pg.pwc <- penguins %>% tukey_hsd(`Culmen Depth (mm)` ~ Species)
pg.pwc
  
  # Adelie vs. Chinstrap
  #p = 8.97e- 1 = 0.897 (ns) → No significant difference between these two species.
  
  #Adelie vs. Gentoo
  #p = 5.82e-13 → Extremely significant difference.
  
  #Chinstrap vs. Gentoo
  #p = 5.82e-13 → Extremely significant difference.

penguins$Species <- as.factor(penguins$Species)
plot(penguins$Species,penguins$`Culmen Depth (mm)`,
     main = "Species vs. Culmen Depth",
     xlab = "Species",
     ylab = "Culmen Depth")



