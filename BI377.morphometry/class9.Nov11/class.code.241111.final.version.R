# BI377 Fall 2024
# In-Class Code
# Week 9

# FINAL VERSION

# Please, restart R!
# Behind the scenes, some packages have been updated, necessitating a restart.
.rs.restartR()

########################
# Colorimetric Analysis
#######################

# load libraries
{
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(tibble)
  library(magrittr)
  library(ggplot2)
  library(ggpubr)
  library(jpeg)
  library(colordistance)
}


# Optionally, clear all objects from R's memory
# rm(list = ls(all = TRUE))

# Clean up R's memory
gc()

# As sample data, we'll use some images that come with the `colordistance` package

# Get the full paths and filenames to the sample images
(path.A <- system.file("extdata/Heliconius/Heliconius_A", package="colordistance"))
(image.files <- list.files(
  path = path.A,
  pattern = "jpeg"
))

(image.files <- paste(path.A, image.files, sep = "/"))

(path.B <- system.file("extdata/Heliconius/Heliconius_B", package="colordistance"))
(image.files <- c(
  image.files,
  paste(
    path.B,
    list.files(
      path = path.B,
      pattern = "jpeg"
    ),
    sep = "/"
  )
))

# So, `image.files` is a vector of explicit file names
# pointing to all 8 image files on the server
image.files

# Load the images
img <- lapply(image.files, readJPEG)

# Display some images, just to see them
plot(1:2, type='n'); rasterImage(img[[1]],1,1,2,2)
plot(1:2, type='n'); rasterImage(img[[2]],1,1,2,2)


# You can use a loop to view them all

# First set the number of images to display
max.i <- length(img)
# Group the following code in braces, to run as a group
{
  # The command below splits the plot window into panels
  par(mfrow=c(ceiling(sqrt(max.i)),ceiling(sqrt(max.i))))
  # Loop
  for (i in 1:max.i) {
    # Plot the image
    plot(1:2, type='n', main = paste0("Heliconius_0",i))
    rasterImage(img[[i]],1,1,2,2)
  }
  # Re-set the plot window to a single panel
  par(mfrow=c(1,1))
}


# Display a histogram of pixel colors using the `colordistance`
# function `plotPixels`
plotPixels(image.files[1])

# We need to filter out the background pixels, which are white in this case

# Set bounds on the background pixel values
lower.bound <- c(0.8, 0.8, 0.8)
upper.bound <- c(1,   1,   1  )

# Use `colordistance::loadImage` to import the image file in the format that
# colordistance functions will need
Heliconius1 <- loadImage(
  path = image.files[1],
  lower = lower.bound,
  upper = upper.bound
)

# Display another histogram of pixel colors
plotPixels(Heliconius1)



# Examine the data

is(Heliconius1)

names(Heliconius1)

Heliconius1$path

dim(Heliconius1$original.rgb)

# What's the number of pixels in the image?
prod(dim(Heliconius1$original.rgb)[1:2])

head(Heliconius1$filtered.rgb.2d)

# This is the number of pixels passing the filter
dim(Heliconius1$filtered.rgb.2d)

# What portion of the pixels pass the background filter?
dim(Heliconius1$filtered.rgb.2d)[1] / prod(dim(Heliconius1$original.rgb)[1:2])

# Look at some of the raw pixel values
Heliconius1$original.rgb[1:10,1:8,1]

# This is a white corner!

# Here's the edge of the specimen
Heliconius1$original.rgb[51:60,30:48,1]


# Plot the RGB values of the pixels in the image
plotPixels(Heliconius1)

# Let's compare this to some of the other images

# Load all of the Heliconius images
# Another challenge: Use `lapply` and `loadImage`, but include the
# same arguements to the `loadImage` function
Heliconius <- lapply(
  image.files,
  function (x) {
    loadImage(
      path = x,
      lower = lower.bound,
      upper = upper.bound
    )
  }
)

is(Heliconius)

length(Heliconius)


# Let's look at the first 3 images and their pixel distributions

# First set the number of images to display
max.i <- 3
# Group the following code in braces, to run as a group
{
  # The command below splits the plot window into panels
  par(mfrow=c(max.i,2))
  # Loop
  for (i in 1:max.i) {
    # Plot the image itself
    plot(1:2, type='n', main = paste0("Heliconius_0",i))
    rasterImage(img[[i]],1,1,2,2)
    # Plot the pixel distribution
    plotPixels(Heliconius[[i]])
  }
  # Re-set the plot window to a single panel
  par(mfrow=c(1,1))
}


# Load the images, but this time as binned data
hist.Heliconius <- lapply(
  image.files,
  function (x) {
    getImageHist(
      image = x,
      bins=c(2, 2, 2),
      lower = lower.bound,
      upper = upper.bound,
      plotting = FALSE
    )
  }
)

# Explore the data

is(hist.Heliconius[[1]])

hist.Heliconius[[1]]


# Let's define a function to look at the color distributions
color.barplot <- function(x, n) {
  if (missing(n)) { n <- c(1:length(x)) }
  if (length(n)>1) { par(mfrow=c(ceiling(sqrt(length(n))), ceiling(sqrt(length(n))))) }
  lapply(n, function(i) {
    x.i <- x[[i]]
    barplot(x.i$Pct, col = rgb(x.i[,1:3]), main = names(x)[i], ylim = c(0,1))
  })
  par(mfrow=c(1,1))
}

# Examine the histogram for the first specimen
?color.barplot

color.barplot(hist.Heliconius, n = 1)


# Histograms for all specimens
color.barplot(hist.Heliconius)


# 3D!
?plotClusters

plotClusters(cluster.list = hist.Heliconius[[1]])

# 3D histograms of the first 3 images
plotClusters(cluster.list = hist.Heliconius, p = 1:3)




# What is the distance between each color distribution?
# Let's calculate a color distance matrix (CDM) using the
# `colordistance` function `getColorDistanceMatrix`
?getColorDistanceMatrix

CDM <- getColorDistanceMatrix(
  cluster.list = hist.Heliconius,
  method = "emd",
  plotting = FALSE
)

# Note: An alternative approach is the function `getHistList` which
# takes a vector of file names (with a path) and iterates `getImageHist`
# to produce a list of color histograms

CDM

?heatmapColorDistance

heatmapColorDistance(
  clusterList_or_matrixObject = CDM,
  col = inferno(1000)
)

# Compare this with the images themselves
{
  max.i <- length(img)
  par(mfrow=c(ceiling(sqrt(max.i)),ceiling(sqrt(max.i))))
  for (i in 1:max.i) {
    # Plot the image
    plot(1:2, type='n', main = paste0("Heliconius_0",i))
    rasterImage(img[[i]],1,1,2,2)
  }
  par(mfrow=c(1,1))
}



# Using k-means instead of binned histograms
?getKMeanColors

# Testing one image first
getKMeanColors(
  path = image.files[1],
  n = 10,
  lower = lower.bound,
  upper = upper.bound
)

# The argument `n` sets the number of bins.
# Choose a number that has bins that contain a decent number
# of pixels and aren't too redundant in their color

getKMeanColors(
  path = image.files[1],
  n = 6,
  lower = lower.bound,
  upper = upper.bound
)
# I think n=6 is a good value

# Load the "binned" color data for all images
Heliconius.K6 <- lapply(
  image.files,
  function (x) {
    getKMeanColors(
      path = x,
      n = 6,
      lower = lower.bound,
      upper = upper.bound,
      plotting = FALSE
    )
  }
)

# Use `extractClusters` to define "bins" based on the k-means clusters
?extractClusters
Heliconius.k.clusters <- extractClusters(Heliconius.K6)

Heliconius.k.clusters

# Calculate the color distance matrix using the k-means clsuters
CDM.k <- getColorDistanceMatrix(
  cluster.list = Heliconius.k.clusters,
  method = "emd",
  plotting = FALSE
)

heatmapColorDistance(CDM.k, col = inferno(2^8))



########################
# Residual Plots
########################

data("penguins", package = "palmerpenguins")

head(penguins)

# Try a simple model of bill length using body mass as the predictor
m.bill.by.mass <- lm(
  bill_length_mm ~ body_mass_g,
  data = penguins
)

anova(m.bill.by.mass)

# Residual plot
plot(m.bill.by.mass, which = 1)

# Test the residuals for normality
shapiro.test(m.bill.by.mass$residuals)

# The fact that the p-value is very small implies this model
# does not describe variation in the data very well.

# Perhaps another metadata factor accounts for variation?
# Look at the whether `species` is evently distributed
# among the residuals
plot(m.bill.by.mass, which = 1, col = penguins$species)

# No! This suggests a model including `species` as a predictive
# factor would do a better job of describing variation in bill length

# Try a more complex model that incorporates species
m.bill.by.mass.species <- lm(
  bill_length_mm ~ body_mass_g + species,
  data = penguins
)

anova(m.bill.by.mass.species)

plot(m.bill.by.mass.species, which = 1)

shapiro.test(m.bill.by.mass.species$residuals)
# The fact that residuals are normally distributed
# suggests this model does an okay job of describing
# variation

# Just for the sake of it, we can check how other metadata
# factors are distributed around the residuals
plot(m.bill.by.mass.species, which = 1, col = penguins$year)
plot(m.bill.by.mass.species, which = 1, col = penguins$sex)
plot(m.bill.by.mass.species, which = 1, col = penguins$island)
# The "green" `island` category is the only factor that looks
# biased in this model. So if we wanted to try a more refined
# model, we migh try adding that factor.


########################
# Residual Plots for Shape Data
########################

library(geomorph)
library(borealis)

data(package = "geomorph")

# As an example dataset, we'll use the
# Head shapes of larval salamanders in `larvalMorph`
data("larvalMorph", package = "geomorph")

names(larvalMorph)

landmark.plot(larvalMorph$headcoords)

# This dataset also includes sliding semilandmarks
head(larvalMorph$head.sliders)

# During GPA, semilandmarks can be adjusted (by "sliding")
# using the `curves` argument
gpa.sali <- align.procrustes(
  A = larvalMorph$headcoords,
  curves = larvalMorph$head.sliders
)

# Start by modeling head shape as a function of centroid size
m.sali.by.size <- procD.lm(
  f1 = coords ~ log(Csize),
  iter = 9999,
  data = gpa.sali$gdf,
  print.progress = TRUE
)

anova(m.sali.by.size)
# Size is a significant predictive factor, but only
# accounts for about 9% of shape variation.

# We can look at a residual plot by passing the `procD.lm` model
# to the `plot` function
plot(m.sali.by.size)

# These residuals look pretty biased, with a lot of
# them in the lower left

# Try running a Normality test...
shapiro.test(m.sali.by.size$residuals)
# It doesn't work!

dim(m.sali.by.size$residuals)
# This is a 2D shape matrix, because Procrustes residuals
# are multi-dimensional!

# Tests exist for multidimenmsional normality, but there are
# good reasons to think shape residuals shouldn't necessarily be
# normally distributed.

# But what we should worry about is whether the model residuals
# should any bias regarding other metadata factors.
# We can explore this by looking at PCA on the model residuals.

pca.sali.residuals <- gm.prcomp(m.sali.by.size$residuals)

# Here, let's label the residuals by herbicide treatment group
# (one of the other metadata factors included in this dataset)
shape.space(
  pca.sali.residuals,
  group = larvalMorph$treatment,
  convex.hulls = TRUE
)
# It looks like these treatments might be biased to different
# areas of PC1 and PC2. -- And PC1 makes up a lot (56%) of
# variance in the residuals

# This suggests that a model including `treatment` might do a
# better job of describing the larval salamander head shapes

m.sali.by.size.treatment <- procD.lm(
  f1 = coords ~ log(Csize) + larvalMorph$treatment,
  iter = 9999,
  data = gpa.sali$gdf,
  print.progress = TRUE
)

anova(m.sali.by.size.treatment)

# The ANOVA table show that, yes, not only is `treatment` a
# predictive factor of shape, but it's a strong predictor.
# It's effect size (Z = 5.5) is much greater than size (3.4).

# This suggests that an even better model would include `treatment`
# before `log(Csize)` since this is a Type I ANOVA where the order
# of factors can be important.
