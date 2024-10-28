# BI377 Fall 2024
# In-Class Code
# Week 7

# Student version

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
pup.gpa <-








# Add metadata into the GPA object in a way that's
# consistent with `borealis` shape data structures
pup.gpa$gdf$sex <- pupfish$Sex
pup.gpa$gdf$pop <- pupfish$Pop
pup.gpa$gdf$sex.pop <- paste(pupfish$Sex, pupfish$Pop)
pup.gpa$gdf$Csize <- pupfish$CS




# Check the PCA
pup.pca <-


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

i <-  # number of iterations




size.model <- procD.lm(
  ~
  data =
  iter = i, print.progress = TRUE
)





# Model testing
anova(size.model)




# How to interpret the results of each line in the table:
# - Df is the degrees of freedom
# - Rsq (R^2) is the proportion of overall variance attributed to that factor
# - F is the test statistic
# - Z is the "effect size"
# - Pr(>F) is effectively the p-value





# More complex models










# Compare the advanced models to the null size model







# Comparing shape allometries

# Do populations have unique allometries or a common allometry?

pop.unique.model <- procD.lm(
  ~
  data =
  iter = i, print.progress = TRUE
)

plotAllometry(
  fit =
  size =
  col = pup.gpa$gdf$pop,
  xlab = "log10 centroid size"
)





# An allometry plot using `borealis::scaling.plot`
scaling.plot(
  x = log10(pup.gpa$gdf$Csize),
  y = pup.pca$x[,1],
  group = pup.gpa$gdf$pop,
  group.title = "population",
  xlab = "log10 centroid size",
  ylab = "shape PC1",
  include.legend = TRUE,
  groups.trendlines = TRUE,
  fixed.aspect = FALSE
)



# Define a null model, without the interaction
pop.model <- procD.lm(
  ~
  data = pup.gpa$gdf, iter = i
)






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

# Create a simple ANOVA model of body mass by species,
# using the Palmer pengiuns data








# Let's look at how we can make pairwise comparisons
# for models of shape data

# Check the sample sizes by species

# Create a `procD.lm` model of worker wing shape by size and species





# Then create a null model

# The function `pairwise` examines the contrasts among each level
bees.sp.pw <- pairwise(
  fit =
  fit.null =
  groups =
)
summary(bees.sp.pw)
# How do you interpret the results?



# `pairwise` requires a null model, and it will guess if you don’t specify one!
# You can check it's assumptions using `reveal.model.designs`, but it is
# good practice to explicitly define the null model.

reveal.model.designs(bees.sp.model)
