# BI377 Fall 2024
# In-Class Code
# Week 7

# FINAL CLASS VERSION

# Load some key packages of GMM tools
library(geomorph)
library(borealis)

########################
# Importing the data
########################

# Landmark data from Cyprindon pecosensis body shapes, with indication of
# Sex and Population from which fish were sampled (Marsh or Sinkhole).
# (Collyer et al. 2015 Heredity)
data(pupfish, package = "geomorph")

# GPA
pup.gpa <- align.procrustes(pupfish$coords)

# Add metadata into the GPA object in a way that's
# consistent with `borealis` shape data structures
pup.gpa$gdf$sex <- pupfish$Sex
pup.gpa$gdf$pop <- pupfish$Pop
pup.gpa$gdf$sex.pop <- paste(pupfish$Sex, pupfish$Pop)
pup.gpa$gdf$Csize <- pupfish$CS

# Before modeling it can be helpful to look at
# variation in the shape data using PCA. This can serve
# as a reality check for the results of modeling.
pup.pca <- gm.prcomp(pup.gpa$gdf$coords)

shape.space(
  pup.pca, group = pup.gpa$gdf$sex.pop,
  convex.hulls = TRUE
)

shape.space(
  pup.pca, group = pup.gpa$gdf$sex.pop,
  convex.hulls = TRUE,
  backtransform.examples = TRUE, ref.shape = pup.gpa$consensus,
  bt.margin.factor = 2, shape.method = "TPS"
)

# Shape comparisons by Procrustes ANOVA with permutation
?procD.lm



# Set the number of iterations
# Defining this value here is optional, since you
# can also set the value directly in the function later.
i <- 9999

# Since size is a strong influence on shape for most organisms,
# it is useful to start with a model where size (as `log(Csize`)
# is the only predictive factor.
size.model <- procD.lm(
  coords ~ log(Csize),
  data = pup.gpa$gdf,
  iter = i, print.progress = TRUE
)

# It's helpful to give your model objects descriptive names.

# The procD.lm model object itself doesn't tell us much.
size.model

# We use the `anova` function to generate an ANOVA table.
anova(size.model)

# the `summary` function can generate the same output.
summary(size.model)

# How to interpret the results of each line in the table:
# - `Df` is the degrees of freedom.
# - `Rsq` (R^2) is the proportion of overall variance attributed to that factor.
#   The R^2 value for all factors (plus the residuals) will sum to 1.
# - `F` is the test F statistic.
# - `Z` is the "effect size". Unlike F or p, the Z-scores are comparable
#   across groups and data sets.
# - Pr(>F) is effectively the p-value

# **Notes on effect size**
# For univariate ANOVA, there are various measures of effect size.
# R^2 is typically one that is used since it can be interpreted as
# "the amount of variance accounted by each factor" -- or left
# unaccounted for by the model and instead appearing in the residuals.
# However, it has been argued (e.g. https://www.youtube.com/watch?v=VWrbYxkCWYY&t=2942s)
# that this interpretation doesn't extend to multivariate analysis.
# Instead `Z` can be considered a better metric of effect size.
# It is a standardized value, calculated from the F statistic and its dispersion.
# Larger values of `Z` indicate a stronger effect for one factor, compared to another.
# Amazingly, Z-scores are comparable across groups and data sets.

# More complex models
size.sex.model <- procD.lm(
  coords ~ log(Csize) + sex,
  data = pup.gpa$gdf,
  iter = i, print.progress = TRUE
)
anova(size.sex.model)

size.pop.model <- procD.lm(
  coords ~ log(Csize) + pop,
  data = pup.gpa$gdf,
  iter = i, print.progress = TRUE
)
anova(size.pop.model)


# A model with 3 factors: size, population, and sex
# Since the simpler models suggest that `pop` has a larger effect size
# it makes sense (given Type I sums of squares) that it goes before `sex`.
size.pop.sex.model <- procD.lm(
  coords ~ log(Csize) + pop + sex,
  data = pup.gpa$gdf,
  iter = i, print.progress = TRUE
)
anova(size.pop.sex.model)

# Examine the interaction between sex and population
# Biologically speaking, this model is testing whether the way that
# shape differs by sex might be different in each population.
size.popXsex.model <- procD.lm(
  coords ~ log(Csize) + pop * sex, # same as  `coords ~ log(Csize) + pop + sex pop:sex`
  data = pup.gpa$gdf,
  iter = i, print.progress = TRUE
)
anova(size.popXsex.model)

# Compare the advanced models to the "null" model
anova(size.pop.sex.model, size.popXsex.model)

# **Note on model complexity**
# We can combine any number of terms in a model
# Although we should be cautious, since highly complex models are
# prone to "over-fitting" where the model seems to predict the data
# well, but this is only an artifact.


# Comparing shape allometries

# Do populations have unique allometries or a common allometry?

pop.unique.model <- procD.lm(
  coords ~ log(Csize) * pop,
  data = pup.gpa$gdf,
  iter = i, print.progress = TRUE
)

# The `geomorph` package has a function `plotAllometry` to
# visualize the relationship of shape (as PC1) to size
plotAllometry(
  fit = pop.unique.model,
  size = log(pup.gpa$gdf$Csize),
  col = pup.gpa$gdf$pop,
  xlab = "log centroid size"
)

# An allometry plot using `borealis::scaling.plot`
scaling.plot(
  x = log(pup.gpa$gdf$Csize),
  y = pup.pca$x[,1],
  group = pup.gpa$gdf$pop,
  group.title = "population",
  xlab = "log10 centroid size",
  ylab = "shape PC1",
  include.legend = TRUE,
  groups.trendlines = TRUE,
  fixed.aspect = FALSE
)

# To actually test the hypothesis of unique population allomwtries,
# we need to define a null model, without the interaction.
pop.model <- procD.lm(
  coords ~ log(Csize) + pop,
  data = pup.gpa$gdf, iter = i
)

# Comapre them
anova(pop.model, pop.unique.model)


# Some new bumblebee wing data
# Download from https://github.com/aphanotus/openEd/blob/main/BI377.morphometry/class7.Oct28/worker.forewings.Rda
# Then upload to your bi377.colby.edu space
load("worker.forewings.Rda", verbose = TRUE)

# Reflection
x <- align.reflect(worker.forewings, top.pt = 2, left.pt = 19)

# GPA
wing.gpa <- align.procrustes(x)

# PCA
wing.pca <- gm.prcomp(wing.gpa$gdf$coords)

# Viz
shape.space(
  wing.pca,
  group = wing.gpa$gdf$species,
  group.title = 'species',
  convex.hulls = TRUE, include.legend = FALSE
)

# How can we represent evolutionary relationships in space shape?
# We'll come back to that.


# Pairwise comparisons

# Recall how we did this for univariate data.
pen.model <- aov(body_mass_g ~ species, data = palmerpenguins::penguins)

summary(pen.model)

TukeyHSD(pen.model)
# Which of these contrasts is significant?

boxplot(body_mass_g ~ species, data = palmerpenguins::penguins)



# Let's look at how we can make pairwise comparisons
# for models of shape data
table(wing.gpa$gdf$species)

bees.sp.model <- procD.lm(
  coords ~ log(Csize) + species,
  data=wing.gpa$gdf,
  iter=i, print.progress = TRUE
)
anova(bees.sp.model)

# We need to build a null model, one that lacks the factor we're interested in.
# In this case, that's `species`. So our null model will include only centroid size.
bees.size.model <- procD.lm(
  coords ~ log(Csize),
  data=wing.gpa$gdf,
  iter=i, print.progress = TRUE
)

# The function `pairwise` examines the contrasts among each level
bees.sp.pw <- pairwise(
  fit = bees.sp.model,
  fit.null = bees.size.model,
  groups = wing.gpa$gdf$species,
  print.progress = TRUE
)
summary(bees.sp.pw)
# How do you interpret the results?

# `pairwise` requires a null model, and it will guess if you don’t specify one!
# You can check it's assumptions using `reveal.model.designs`, but it is
# good practice to explicitly define the null model.

reveal.model.designs(bees.sp.model)

# **Closing notes**
# - Modeling allows you to account for confounding factors, before examining a factor-of-interest
# - Interaction terms can be examined too, as `A * B` or `A + B + A:B`
# - The sequential order of factors in the model matters (this is Type I ANOVA)
