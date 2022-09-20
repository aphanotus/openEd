# Sept 20 In class

# Load the data package
library(palmerpenguins)

# Examine the data strcuture
names(penguins)
colnames(penguins)

head(penguins)
tail(penguins)

dim(penguins)

# Examine a histogram of body sizes
hist(penguins$body_mass_g)

hist(penguins$body_mass_g, xlim = c(0,10000))

mean(penguins$body_mass_g)

mean(penguins$body_mass_g, na.rm = TRUE)

# Add vertical lines indicating the mean and median
abline(v = mean((penguins$body_mass_g), na.rm = TRUE), col = "darkred", lwd = 3)
abline(v = median((penguins$body_mass_g), na.rm = TRUE), col = "darkblue", lwd = 3)

# A function to find the 95% (2 standard deviation) confidence bounds
ci <- function (x) {
  x <- x[!is.na(x)]
  return(c(mean(x) + 2 * sd(x), mean(x) - 2 * sd(x) ))
}

ci(penguins$body_mass_g)

abline(v = ci(penguins$body_mass_g), col = "darkred", lwd = 2, lty = 3)

# Find out which species are present in the table.
unique(penguins$species)



# ggplot

library(ggplot2)
# or
library(tidyverse)

# A simple histogram
ggplot(penguins, aes(x = body_mass_g)) +
  theme_bw() +
  geom_histogram()

# By species
ggplot(penguins, aes(x=body_mass_g)) +
  theme_bw() +
  facet_wrap( . ~ species ) +
  geom_histogram()

# By species and sex
ggplot(penguins, aes(x=body_mass_g)) +
  theme_bw() +
  facet_grid( sex ~ species ) +
  geom_histogram()

library(tidyverse)

# Filter out individuals where sex is NA
penguins %>% 
  filter(!is.na(sex)) %>% 
  ggplot(aes(x=body_mass_g)) +
  theme_bw() +
  facet_grid( sex ~ species ) +
  geom_histogram()


# Tidyverse descriptive stats
# 
# For details on more "tidy" tools, check out...
# chrome-extension://efaidnbmnnnibpcajpcglclefindmkaj/https://www.rstudio.com/wp-content/uploads/2015/02/data-wrangling-cheatsheet.pdf

penguins %>% 
  filter(!is.na(body_mass_g))


penguins %>% 
  filter(!is.na(body_mass_g)) %>% 
  summarize(mean_body_mass_g = mean(body_mass_g))


penguins %>% 
  filter(!is.na(body_mass_g)) %>% 
  group_by(species) %>% 
  summarise(mean_body_mass_g = mean(body_mass_g))


penguins %>% 
  filter(!is.na(body_mass_g)) %>% 
  group_by(species) %>% 
  summarise(
    mean_body_mass_g = mean(body_mass_g),
    sd_body_mass_g = sd(body_mass_g),
    ci_body_mass_g = 2*sd(body_mass_g)
    )

desc.stats <- penguins %>% 
  filter(!is.na(body_mass_g)) %>% 
  group_by(species) %>% 
  summarise(
    mean_body_mass_g = mean(body_mass_g),
    sd_body_mass_g = sd(body_mass_g),
    ci_body_mass_g = 2*sd(body_mass_g)
    )


ggplot(penguins, aes(x=body_mass_g)) +
  theme_bw() +
  facet_wrap(.~species) +
  geom_histogram() +
  geom_vline(data = desc.stats, aes(xintercept = mean_body_mass_g),
             color = "darkred") 
  
  
ggplot(penguins, aes(x=body_mass_g)) +
  theme_bw() +
  facet_wrap(.~species) +
  geom_histogram() +
  geom_vline(data = desc.stats, aes(xintercept = mean_body_mass_g),
             color = "darkred") +
  geom_vline(data = desc.stats, aes(xintercept = mean_body_mass_g - ci_body_mass_g),
             color = "darkred", linetype = "dotted") +
  geom_vline(data = desc.stats, aes(xintercept = mean_body_mass_g + ci_body_mass_g),
             color = "darkred", linetype = "dotted") 

# Google search "ggplot linetype" for details on that formatting!


# Stats

# Create objects for the mass of each species
adelie.kg <- penguins$body_mass_g[which(penguins$species=="Adelie")]/1000
adelie.kg <- adelie.kg[!is.na(adelie.kg)]
chin.kg <- penguins$body_mass_g[which(penguins$species=="Chinstrap")]/1000
chin.kg <- chin.kg[!is.na(chin.kg)]
gentoo.kg <- penguins$body_mass_g[which(penguins$species=="Gentoo")]/1000
gentoo.kg <- gentoo.kg[!is.na(gentoo.kg)]

# Test for differences in means
t.test(chin.kg, adelie.kg) 
t.test(gentoo.kg, adelie.kg)
t.test(gentoo.kg, chin.kg)
# Which contrasts are "significantly" different?

# Test for differences in means, using a non-parametric test
wilcox.test(chin.kg, adelie.kg) 
wilcox.test(gentoo.kg, adelie.kg)
wilcox.test(gentoo.kg, chin.kg)
# Which contrasts are "significantly" different?
# How do the results of the Wilcoxon tests differ from the t-tests?

# Test for differences in variance
var.test(chin.kg, adelie.kg) # NS
var.test(gentoo.kg, adelie.kg) # NS
var.test(gentoo.kg, chin.kg)
# Which contrasts are "significantly" different?
# What exactly is different? How is this test different from above?


# Sampling

# Draw one value, at random, from the body mass column
sample(x = penguins$body_mass_g, size = 1)

# Again
sample(x = penguins$body_mass_g, size = 1)

# Again
sample(x = penguins$body_mass_g, size = 1)

# Get many at once
sample(x = penguins$body_mass_g, size = 10)


# Get many at once and compare that distribution to the 
# actual distribution
{
  # A trick to get multiple panels for the base R plots
  par(mfrow = c(2,1))
  
  x <- sample(x = penguins$body_mass_g, size = 100, replace = TRUE)

  hist(x)
  abline(v=mean(x, na.rm = TRUE), col = "darkred", lwd = 3)
  abline(v=median(x, na.rm = TRUE), col = "darkblue", lwd = 3)
  abline(v=ci(x), col = "darkred", lwd = 2, lty = 3)
  
  hist(penguins$body_mass_g)
  abline(v=mean((penguins$body_mass_g), na.rm = TRUE), col = "darkred", lwd = 3)
  abline(v=median((penguins$body_mass_g), na.rm = TRUE), col = "darkblue", lwd = 3)
  abline(v=ci(penguins$body_mass_g), col = "darkred", lwd = 2, lty = 3)
  
  # Reset the number of panels in the plot window
  par(mfrow = c(1,1))
}
# What differences are there? Why?

