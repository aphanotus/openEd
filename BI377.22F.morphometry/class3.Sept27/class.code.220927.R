# Sept 27 In class

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

penguins

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

shapiro.test(log(adelie.mass))
shapiro.test(log(chin.mass))
shapiro.test(log(gentoo.mass))



# Absence of outliers?

boxplot(penguins$body_mass_g ~ penguins$species)

# Or... 

are.there.outliers <- function(x) {
    any(x < quantile(x, 0.25) - 1.5*IQR(x)) 
    |
    any(x > quantile(x, 0.75) + 1.5*IQR(x))
}

are.there.outliers(adelie.mass)
are.there.outliers(chin.mass)
are.there.outliers(gentoo.mass)

which.outliers <- function(x) {
  which(
    x < quantile(x, 0.25) - 1.5*IQR(x) |
    x > quantile(x, 0.75) + 1.5*IQR(x)
  )
}

which.outliers(chin.mass)

i <- which.outliers(chin.mass)

penguins %>% 
  filter(species == "Chinstrap") %>% 
  slice(i)


# Let's try ANOVA anyway!

ggplot(penguins, aes(x=body_mass_g)) +
  theme_bw() +
  facet_grid( species ~ . ) +
  geom_histogram() +
  geom_vline(data = desc.stats, aes(xintercept = mean_body_mass_g),
             color = "darkred") 

aov.species <- aov(body_mass_g ~ species, data = penguins)
aov.species
summary(aov.species)

# Post hoc tests: Which species actually differ?

TukeyHSD(aov.species)


# Two-way ANOVA by species and sex

penguins %>% 
  filter(!is.na(sex)) %>% 
  ggplot(aes(x=body_mass_g)) +
  theme_bw() +
  facet_grid( species ~ sex ) +
  geom_histogram() 

aov.sp.sex <- aov(body_mass_g ~ species + sex, data = penguins)
summary(aov.sp.sex)

# Post hoc 
TukeyHSD(aov.sp.sex)


# Testing for an interaction of species and sex factors
# Is it possible that sex matters more in some species?
aov.spXsex <- aov(body_mass_g ~ species + sex + species:sex, data = penguins)
summary(aov.spXsex)

aov.spXsex <- aov(body_mass_g ~ species * sex, data = penguins)
summary(aov.spXsex)

car::Anova(aov.spXsex, type = "III")

# Post hoc 
TukeyHSD(aov.spXsex)

with(penguins, 
  boxplot(body_mass_g ~ species * sex)   
)

with(penguins, 
  boxplot(body_mass_g ~ sex * species)   
)


# Non-parametric ANOVA: The Kruskal-Wallis Rank Sum Test
kruskal.test(body_mass_g ~ species, data = penguins)

# Post hoc
wilcox.test(adelie.mass, chin.mass)
wilcox.test(gentoo.mass, chin.mass)
wilcox.test(adelie.mass, gentoo.mass)

library(dunn.test)

dunn.test(penguins$body_mass_g, penguins$species, method = "bh")

with(penguins, 
  dunn.test(body_mass_g, species)
)


# Multi-factor designs are not possible with the KWRST
kw.sp.sex <- kruskal.test(body_mass_g ~ species + sex, data = penguins)

# but...

penguins %>% 
  mutate(sp.sex = paste(species, sex, sep = "."))

pen2 <- penguins %>% 
  filter(!is.na(sex)) %>% 
  mutate(sp.sex = paste(species, sex, sep = "."))

kruskal.test(body_mass_g ~ sp.sex, data = pen2)
