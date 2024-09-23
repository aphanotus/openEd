# BI377 Fall 2024 
# In-Class Code
# Week 2

# Load the data package
library(palmerpenguins)

# Get help on an unfamiliar package
?palmerpenguins

# Get help on any function
?ggplot

# load data sets from a package
data(package = "palmerpenguins")

# Examine the data structure
is(penguins)
names(penguins)
colnames(penguins)

# Top and bottom of the data table
head(penguins)
tail(penguins)

# dimensions of the table, as the number of rows and columns 
dim(penguins)

# Examine a histogram of body sizes
hist(penguins$body_mass_g)

# Expand the axis range
hist(penguins$body_mass_g, xlim = c(0,10000))

# Find the mean value
mean(penguins$body_mass_g)
mean(penguins$body_mass_g, na.rm = TRUE)

# Add vertical lines indicating the mean and median
abline(v = mean((penguins$body_mass_g), na.rm = TRUE), col = "darkred", lwd = 3)
abline(v = median((penguins$body_mass_g), na.rm = TRUE), col = "darkblue", lwd = 3)

# Define a function to find the 95% (2 standard deviation) bounds
ci <- function (x) {       # Functions are defined using the reserved word `function`
                           # In parentheses, we define the arguments the function will take, in this example we're calling it `x`
  x <- x[!is.na(x)]        # This line filters NA values out of the input vector `x`
  return(                  # Within a function `return` defines the value to be outputted by the function
    c(mean(x) + 2 * sd(x), # Our output will be a vector, defined by `c`
      mean(x) - 2 * sd(x)  # The two output values will be the mean of `x` plus and minus 2 standard deviations 
    )
  )
} # End of function

# Run the function on the penguin body mass data
ci(penguins$body_mass_g)

# Use the `ci` to add vertical lines to our plot
# at plus and minus 2 standard deviations.
abline(v = ci(penguins$body_mass_g), col = "darkred", lwd = 2, lty = 3)


# Find out which species are present in the penguin dataset
unique(penguins$species)

# Let's make some plots!
library(ggplot2)

# A simple histogram
ggplot(penguins, aes(x = body_mass_g)) +
  theme_bw() +
  geom_histogram()

# By species
ggplot(penguins, aes(x=body_mass_g)) +
  theme_bw() +
  facet_wrap( . ~ species ) +
  geom_histogram()

ggplot(penguins, aes(x=body_mass_g)) +
  theme_bw() +
  facet_grid( species ~ . ) +
  geom_histogram()

# By species and sex
ggplot(penguins, aes(x=body_mass_g)) +
  theme_bw() +
  facet_grid( sex ~ species ) +
  geom_histogram()

library(dplyr) # A package for data manipulation
library(magrittr) # A package that allows "piping" of data between functions

# Filter out individuals where sex is NA
penguins %>% 
  filter(!is.na(sex)) %>% 
  ggplot(aes(x=body_mass_g)) +
  theme_bw() +
  facet_grid( sex ~ species ) +
  geom_histogram()


# Tidyverse descriptive stats
# For details on more "tidy" tools, check out...
# https://www.rstudio.com/wp-content/uploads/2015/02/data-wrangling-cheatsheet.pdf

# Find mean body size
penguins %>% 
  filter(!is.na(body_mass_g)) %>% 
  summarize(mean_body_mass_g = mean(body_mass_g))

# Find mean body size by species
# This would be tedious with base R, 
# but dplyr's `group_by` function makes it easy!
penguins %>% 
  filter(!is.na(body_mass_g)) %>% 
  group_by(species) %>% 
  summarise(mean_body_mass_g = mean(body_mass_g))

# The `summarise` function can generate multiple stats at once
penguins %>% 
  filter(!is.na(body_mass_g)) %>% 
  group_by(species) %>% 
  summarise(
    mean_body_mass_g = mean(body_mass_g),
    sd_body_mass_g = sd(body_mass_g),
    ci_body_mass_g = 2*sd(body_mass_g)
  )

# We can save this output into a new table
# (Actually, dplyr is saving this as a table-like
#  data structure called a "tibble".)
desc.stats <- penguins %>% 
  filter(!is.na(body_mass_g)) %>% 
  group_by(species) %>% 
  summarise(
    mean_body_mass_g = mean(body_mass_g),
    sd_body_mass_g = sd(body_mass_g),
    ci_body_mass_g = 2*sd(body_mass_g)
  )

desc.stats

# By saving those stats, we can call them up in a plot.
# For example when using `geom_vline` below.
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


# Test for differences in means, using a permutation test
# I just added the `perm `package to our server.
# If it doesn't load, run `install.packages("perm")`
library(perm)
permTS(chin.kg, adelie.kg)
?permTS

permTS(
  x = chin.kg, 
  y = adelie.kg,
  method = "exact.ce" # exact using complete enumeration
) 

permTS(
  x = chin.kg, 
  y = adelie.kg,
  method = "exact.mc" # exact using Monte Carlo
) 

# How to choose which kind of test is appropriate?
# Parametric (t.test), Non-parametric (wilcox.test), or Permutation (permTS)?
# Well, there are tests for the parametric assumptions...

# Test for differences in variance
var.test(chin.kg, adelie.kg) 
var.test(gentoo.kg, adelie.kg) 
var.test(gentoo.kg, chin.kg)
# Which contrasts are "significantly" different?
# What exactly is different? 
# How is this test different from a test of mean differences?


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
  
  x <- sample(x = penguins$body_mass_g, size = 50, replace = TRUE)
  
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
# Run that block of code a few times and see what changes.

