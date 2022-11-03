# # Exercise 2. Distributions & Tests

# ### Problem 1a
# Create histograms showing the distribution of flipper lengths for each species and sex in the `palmerpenguins::penguins` dataset. Include a few short sentences offering a biological interpretation of these data.

# Load the data package
library(palmerpenguins)

# Examine the data structure
names(penguins)

# Plot by species and sex
penguins %>% 
  filter(!is.na(sex)) %>% 
  ggplot(aes(x=flipper_length_mm)) +
  theme_bw() +
  facet_grid( sex ~ species ) +
  geom_histogram()

# ### Problem 1b
# Test for differences in the flipper length among these species. Briefly explain why you chose to use the test you did. Interpret the results of the results of the test in biological terms and report them in your text as we described in class.

# Start by testing the assumptions of ANOVA

# sample sizes

penguins %>% 
  filter(!is.na(flipper_length_mm)) %>% 
  group_by(species) %>% 
  summarise(
    mean_body_mass_g = mean(body_mass_g),
    sample.size = n()
  )

# With unbalanced sample sizes, you should use a non-parametric test.
# But I'll walk through all the other assumption tests anyway.

# Equal variances?

library(car)
leveneTest(flipper_length_mm ~ species, data = penguins)
# NS

# Normal distribution of the data?
# Here's a "tidy" trick to run the Shapiro-Wilk test

penguins %>% 
  group_by(species) %>% 
  summarise(
    SW.test.p = shapiro.test(flipper_length_mm)$p.value,
    normality = shapiro.test(flipper_length_mm)$p.value > 0.05
  )
# The Gentoos have flipper lengths that are not normally distributed

# Log transformation?
penguins %>% 
  group_by(species) %>% 
  summarise(
    SW.test.p = shapiro.test(log10(flipper_length_mm))$p.value,
    normality = shapiro.test(log10(flipper_length_mm))$p.value > 0.05
  )
# Same results


# Outliers?

are.there.outliers <- function(x) {
  any(
    x < quantile(x, 0.25) - 1.5*IQR(x) |
      x > quantile(x, 0.75) + 1.5*IQR(x)
  )
}

penguins %>% 
  filter(!is.na(flipper_length_mm)) %>% 
  group_by(species) %>% 
  summarise(
    outliers = are.there.outliers(flipper_length_mm)
  )
# Yes, ther Adelies have some outliers

# If you did run ANOVA
aov.species <- aov(flipper_length_mm ~ species, data = penguins)
summary(aov.species)

# Post hoc
TukeyHSD(aov.species)

aov.sp.sex <- aov(flipper_length_mm ~ species + sex, data = penguins)
summary(aov.sp.sex)

# Post hoc
TukeyHSD(aov.sp.sex)

aov.spXsex <- aov(flipper_length_mm ~ species * sex, data = penguins)
car::Anova(aov.spXsex)

# Post hoc
TukeyHSD(aov.spXsex)

# Flipper lengths differed by both species and sex
# (two-way ANOVA, species F(2,327) = 784.5, p < 2x10^-16;
# sex F(1,327) = 122.1, p < 2x10^-16). Sexual dimorphism 
# also differed among species (species:sex F(2,327) = 5.144, p = 0.0063)


# KWRST is a better approach given these data.

grp <- paste(penguins$species, penguins$sex, sep = ".")
  
kruskal.test(flipper_length_mm ~ grp, data = penguins)

# Post hoc
library(dunn.test)
with(penguins,
  dunn.test(flipper_length_mm, grp)
)


# Flipper lengths differed by species and sex as a combined factor
# (Kruskal-Wallis test, $\chi$^2^~7~ = 268.8, p < 2x10^-16). 


# #### Challenge 1a
# Write a function to find the coefficient of variation for a numeric vector. (That is, the standard deviation divided by the mean.) Use your function to find the coefficient of variation for each of the numerical traits in `penguins` for each species. (You do not need to distinguish males and females.) This can be done using the "tidy" tools, like `summarize()`. Output a table of the coefficients for each trait and species combination.

coeffvar <- function (x) {
  x <- x[!is.na(x)]
  return(sd(x)/mean(x))
}

penguins %>% 
  filter(!is.na(flipper_length_mm)) %>% 
  group_by(species) %>% 
  summarize(
    mean_flipper_length_mm = mean(flipper_length_mm),
    CV_flipper_length_mm = coeffvar(flipper_length_mm),
  )


# #### Challenge 1b
# Apply the function to all the numerical traits in `penguins`. Represent the coefficients from the table produced in Challenge 1a or 1b as a barplot. Use `ggplot`. Tip: use the `pivot_longer()` function.

penguins %>% 
  group_by(species) %>% 
  summarize(
    CV_bill_length_mm = coeffvar(bill_length_mm),
    CV_bill_depth_mm = coeffvar(bill_depth_mm),
    CV_flipper_length_mm = coeffvar(flipper_length_mm),
    CV_body_mass_g = coeffvar(body_mass_g)
  ) %>% 
  pivot_longer(cols = -1) %>% 
  ggplot(aes(x = value, y = name, fill = species)) +
  geom_bar(stat = "identity", position = "dodge") 

# Make it look a little nicer!
penguins %>% 
  group_by(species) %>% 
  summarize(
    CV_bill_length_mm = coeffvar(bill_length_mm),
    CV_bill_depth_mm = coeffvar(bill_depth_mm),
    CV_flipper_length_mm = coeffvar(flipper_length_mm),
    CV_body_mass_g = coeffvar(body_mass_g)
  ) %>% 
  pivot_longer(cols = -1) %>% 
  mutate(name = gsub("_"," ",name)) %>% 
  mutate(name = sub(" mm$"," (mm)",name)) %>% 
  mutate(name = sub(" g$"," (g)",name)) %>% 
  ggplot(aes(x = value, y = name, fill = species)) +
  theme_bw() +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "coefficient of variation", y = "trait") +
  scale_fill_manual(values = c("lightblue","midnightblue","deepskyblue4"))

# Looks like variation is similar across species. 
# Body mass varies more than other traits.
# Adelie body mass showed the highest variation of all.
# It's interesting that bill length and dept had similar degrees of variation.


