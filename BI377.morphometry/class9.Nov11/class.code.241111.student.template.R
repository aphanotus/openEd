# BI377 Fall 2024
# In-Class Code
# Week 9

# Student template

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

# Load the images
# Here's a quick challenge: Use `lapply` and `readJPEG`
img <-

# Display some images, just to see them
plot(1:2, type='n')
rasterImage(img[[1]],1,1,2,2)


# You can use a loop to view them all

# First set the number of images to display
max.i <-
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
lower.bound <-
upper.bound <-

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








# Look at some of the raw pixel values
Heliconius1$original.rgb









# What portion of the pixels pass the background filter?
dim(Heliconius1$filtered.rgb.2d)[1] / prod(dim(Heliconius1$original.rgb)[1:2])

# Plot the RGB values of the pixels in the image
plotPixels(Heliconius1)







# Let's compare this to some of the other images

# Load all of the Heliconius images
# Another challenge: Use `lapply` and `loadImage`, but include the
# same arguements to the `loadImage` function
Heliconius <- lapply(

)


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


# Histograms for all specimens



# 3D!
?plotClusters




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
  clusterList_or_matrixObject = CDM
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








# Load color "binned" color data for all images
Heliconius.K6 <- lapply(
  image.files,
  function (x) {
    getKMeanColors(
      path = x,
      n = ,
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
#######################

data("penguins", package = "palmerpenguins")







# Try a simple model of bill length using body mass as the predictor
m.bill.by.mass <-







# Try a slightly more involved model







# A shape example

library(geomorph)
library(borealis)

data("larvalMorph", package = "geomorph")

# Explore the data










# GPA
gpa.sali <- align.procrustes(
  A = larvalMorph$headcoords,
  curves = larvalMorph$head.sliders
)

# Define a model of head shape based on size
m.sali.by.size <-
