# BI377 Fall 2024
# In-Class Code
# Week 3

# Load the data package
library(palmerpenguins)

# Does body mass differ by species?
ggplot(penguins, aes(x=body_mass_g)) +
  theme_bw() +
  facet_grid( species ~ . ) +
  geom_histogram()

# Let's test for the assumptions of ANOVA:
# - Independence of the samples
# - Equal sample sizes
# - Equal variance
# - Normal distribution of the data
# - Absence of outliers


# Equal sample sizes?

penguins %>%
  filter(!is.na(body_mass_g)) %>%
  group_by(species) %>%
  summarise(
    mean_body_mass_g = mean(body_mass_g),
    sample.size = n()
  )

(desc.stats <- penguins %>%
    filter(!is.na(body_mass_g)) %>%
    group_by(species) %>%
    summarise(
      mean_body_mass_g = mean(body_mass_g),
      sample.size = n()
    ))

desc.stats


# Equal variances?

# Create objects for the mass of each species
adelie.mass <- penguins$body_mass_g[which(penguins$species=="Adelie")]
adelie.mass <- adelie.mass[!is.na(adelie.mass)]
chin.mass <- penguins$body_mass_g[which(penguins$species=="Chinstrap")]
chin.mass <- chin.mass[!is.na(chin.mass)]
gentoo.mass <- penguins$body_mass_g[which(penguins$species=="Gentoo")]
gentoo.mass <- gentoo.mass[!is.na(gentoo.mass)]

# Test for differences in variance
var.test(chin.mass, adelie.mass)
var.test(gentoo.mass, adelie.mass)
var.test(gentoo.mass, chin.mass)
# Which contrasts are "significantly" different?

library(car)
leveneTest(body_mass_g ~ species, data = penguins)

# Normal distribution of the data?
shapiro.test(adelie.mass)
shapiro.test(chin.mass)
shapiro.test(gentoo.mass)

# Log-Normal distribution?
shapiro.test(log(adelie.mass))
shapiro.test(log(chin.mass))
shapiro.test(log(gentoo.mass))
# This suggests it would be better to run tests
# using the log-transformed data

# Absence of outliers?
# Using the Interquartile Range (IQR) method
boxplot(penguins$body_mass_g ~ penguins$species)

# Or...

are.there.outliers <- function(x) {
  any(x < quantile(x, 0.25) - 1.5*IQR(x)) |
  any(x > quantile(x, 0.75) + 1.5*IQR(x))
}

are.there.outliers(adelie.mass)
are.there.outliers(chin.mass)
are.there.outliers(gentoo.mass)

# Examine the metadata of those outliers
which.outliers <- function(x) {
  which(
    x < quantile(x, 0.25) - 1.5*IQR(x) |
    x > quantile(x, 0.75) + 1.5*IQR(x)
  )
}

which.outliers(chin.mass)

chinstrap <- penguins[which(penguins$species=="Chinstrap"), ]
chinstrap <- chinstrap[!is.na(chinstrap$body_mass_g), ]

(i <- which.outliers(chin.mass))

chinstrap[i, ]


# So there are violations of parametric assumptions
# For the sake of demonstration, let's try ANOVA anyway!

ggplot(penguins, aes(x=body_mass_g)) +
  theme_bw() +
  facet_grid( species ~ . ) +
  geom_histogram() +
  geom_vline(data = desc.stats, aes(xintercept = mean_body_mass_g),
             color = "darkred")

model.species <- aov(body_mass_g ~ species, data = penguins)
model.species
summary(model.species)

# Post hoc tests: Which species actually differ?

# Tukey's Honest Significant Difference
TukeyHSD(model.species)


# Two-way ANOVA by species and sex
penguins %>%
  filter(!is.na(sex)) %>%
  ggplot(aes(x=body_mass_g)) +
  theme_bw() +
  facet_grid( sex ~ species ) +
  geom_histogram()

model.sp.sex <- aov(body_mass_g ~ species + sex, data = penguins)
summary(model.sp.sex)

# Post hoc
TukeyHSD(model.sp.sex)

# Testing for an interaction of species and sex factors
# Is it possible that sex matters more in some species?
model.spXsex <- aov(
  body_mass_g ~ species + sex + species:sex,
  data = penguins
)
summary(model.spXsex)

model.spXsex <- aov(
  body_mass_g ~ species * sex,
  data = penguins
)
summary(model.spXsex)

# The aov() function uses Type I sum of squares.

# ANOVA with Type III SS can be done by the `Anova` function in the `car` package
car::Anova(model.spXsex, type = "III")

# Post hoc
TukeyHSD(model.spXsex)
# Think carefully about the meaning of different contrasts!

# Understand significant interaction terms, it can be helpful to
# plot the data in different ways.
# In the plots that follow, look at differences in the medians
# of each sex in each species.

boxplot(body_mass_g ~ species * sex, data = penguins)
boxplot(body_mass_g ~ sex * species, data = penguins)


# Non-parametric ANOVA: The Kruskal-Wallis Rank Sum Test
kruskal.test( body_mass_g ~ species,  data = penguins )

# Post hoc tests can be done by individual Wilcoxon Rank Sum Tests
wilcox.test(adelie.mass, chin.mass)
wilcox.test(gentoo.mass, chin.mass)
wilcox.test(adelie.mass, gentoo.mass)
# But if you're interested in all pariwise contrasts, you'd want to
# apply a correction for multiple tests.

# An alternative post hoc test for the WRST is Dunn's test, which
# works similarly to Tukey's HSD.
library(dunn.test)
dunn.test(penguins$body_mass_g, penguins$species, method = "bh")

# Or...
with(penguins,
     dunn.test(penguins$body_mass_g, penguins$species)
)


# Multi-factor designs are not possible with the KWRST
kw.sp.sex <- kruskal.test(body_mass_g ~ species + sex, data = penguins)

# But you can get around this by defining a new factor that
# combines species and sex
pen2 <- penguins %>%
  filter(!is.na(sex)) %>%
  mutate(sp.sex = paste(species, sex, sep = "."))

unique(pen2$sp.sex)

kruskal.test(body_mass_g ~ sp.sex, data = pen2)


# Permutation-based analysis of variance (permANOVA)
permKS( body_mass_g ~ species,  data = penguins )

# There is also no method for multi-factor permANOVA.
# But we can use the same work-around.
permKS( body_mass_g ~ sp.sex,  data = pen2 )
