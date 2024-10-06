# BI377 Fall 2024 
# In-Class Code
# Week 5

# Student Version


# Clean-up
?rm



# Restart R
.rs.restartR()


# Update the package 
# (Only run this on your own Rstudio installation, not on bi377.colby.edu)
detach("package:borealis", unload = TRUE)
devtools::install_github("aphanotus/borealis")


# Load some key packages of GMM tools
library(geomorph)
library(borealis)

########################
# Importing the data
########################


# Convert raw landmark coordinates into the tps file format

?create.tps

file.choose()

create.tps(
  input.filename = "bi377.demo.borealis.v.fervidus.forewings.class.csv",
  output.filename = ,
  id.factors = ,
  include.scale = ,
  invert.scale = )

# Repeat for the hindwings









# Import TPS data into R

?read.tps

xy.forewings <- read.tps(
  file = ".tps", 
  keep.original.ids = TRUE
)

# Repeat for the hindwings



# Explore the data structure of `xy.forewings`









# View the shape data

plot(xy.forewings$coords[,,1])


?landmark.plot









# Viewing shapes with landmark connections

fw.links <- matrix(c(1,2, 1,5, 5,4, 4,3, 3,2, 5,6, 6,7, 7,8, 8,9, 
                     9,4, 3,11, 11,12, 11,10, 9,10, 10,14, 14,15, 15,16, 
                     16,18, 18,20, 16,17, 17,8, 12,13, 13,19, 14,13, 18,19, 
                     2,12),
                   ncol = 2, byrow = TRUE)

(fw.links <- as.matrix(read.csv("fw.links.csv")))

landmark.plot(xy.forewings, links = fw.links)



# Define links for hindwings
hw.links <- matrix(c(1:5,2:6), ncol = 2, byrow = FALSE)


landmark.plot( )


########################
# Forewings
########################

# Procrustes alignment for forewing shapes

?align.procrustes





landmark.plot()

# Explore the GPA data structure










# Reflect specimens

landmark.plot(xy.forewings, links = fw.links, specimen.number = c(1,25,182,185))

?align.reflect

xy.forewings <- align.reflect(
  A = xy.forewings, 
  top.pt = , 
  left.pt = , 
  links = fw.links, 
  show.plot = 
)

landmark.plot(xy.hindwings, links = hw.links, specimen.number = c(   ,   ))

# Repeat with the hindwings







# Procrustes alignment, again







landmark.plot(gpa.fw$consensus, links = fw.links)


# Data curation

landmark.plot(xy.forewings, links = fw.links, specimen.number = c(   ,   ))

# Work with your neighbor and find a computational solution to correct this issue! 





# Documenting changes

?add.provenance

xy.forewings <- add.provenance(
  xy.forewings,
  name="error.correction",
  title = "Corrected error in the placement of landmarks ",
  text = "" )

names(xy.forewings$provenance)
cat(xy.forewings$provenance$error.correction)



# What to do if landmarks are missing?
?estimate.missing.landmarks

xy.forewings <- estimate.missing.landmarks(xy.forewings)

landmark.plot(xy.forewings, links = fw.links, specimen.number = c(61,62))


# Procrustes alignment, again, again



# Examine the data structure







########################
# Hindwings
########################

# Procrustes alignment for the hindwings

















########################
# Procrustes distances
########################

# Compare the Procrustes distances between specimens before...
landmark.plot(xy.forewings, specimen.number = 1:2, links = fw.links)

# ...and after GPA.
landmark.plot(gpa.fw, specimen.number = 1:2, links = fw.links)


?procrustes.distance

procrustes.distance(   ,   )

procrustes.distance(   ,   )


# Need to multiply centroid size (Csize) to the GPA-aligned coordinates 
# to make them comparable to the original XY coordinates.

# Can we do this in a systematic way, for all specimens?
# For n specimens there are n*(n-1)/2 unique pairwise combinations!
gpa.fw$specimen.number


# Below is code to examine all pairwise Procrustes distances 
# between specimens, before and after GPA.
library(magrittr)
library(tidyr)
library(stringr)
library(ggplot2)
{
  pairwise.procD.xy <- vector(mode = "numeric")
  pairwise.procD.gpa <- vector(mode = "numeric")
  pairwise.procD.specimen <- vector(mode = "character")
  pairwise.procD.digitizer <- vector(mode = "character")
  
  for (i in 1:(gpa.fw$specimen.number-1)) {
    for (j in (i+1):gpa.fw$specimen.number) {
      # cat(i,"\t",j,"\n")
      x <- procrustes.distance(xy.forewings$coords[,,i], xy.forewings$coords[,,j])
      pairwise.procD.xy <- c(pairwise.procD.xy, x)
      x <- procrustes.distance(gpa.fw$gdf$coords[,,i]*gpa.fw$gdf$Csize[i], gpa.fw$gdf$coords[,,j]*gpa.fw$gdf$Csize[j])
      pairwise.procD.gpa <- c(pairwise.procD.gpa, x)
      pairwise.procD.specimen <- c(pairwise.procD.specimen, str_split_fixed(xy.forewings$metadata$specimen.id[i],"__",2)[1,1])
      pairwise.procD.digitizer <- c(pairwise.procD.digitizer, str_split_fixed(xy.forewings$metadata$specimen.id[i],"__",2)[1,2])
    }
  }
}

# Organize the vectors into a data frame
df <- data.frame(
  specimen = pairwise.procD.specimen,
  digitizer = pairwise.procD.digitizer,
  procD.xy = pairwise.procD.xy,
  procD.gpa = pairwise.procD.gpa
)

# Plot histograms, for the distances before and after GPA ("stage") 
# Color-code the data by specimen
df %>% 
  pivot_longer(cols = 3:4, names_to = "stage") %>% 
  ggplot(aes(x=value, fill = specimen)) +
  theme_bw() +
  facet_grid(stage ~ ., scales = "free_y") +
  geom_histogram()

# Color-code the data by digitizer
df %>% 
  pivot_longer(cols = 3:4, names_to = "stage") %>% 
  ggplot(aes(x=value, fill = digitizer)) +
  theme_bw() +
  facet_grid(stage ~ ., scales = "free_y") +
  geom_histogram()


# Use a color-blind friendly palette
library(viridis)
df %>% 
  pivot_longer(cols = 3:4, names_to = "stage") %>% 
  ggplot(aes(x=value, fill = digitizer)) +
  theme_bw() +
  facet_grid(stage ~ ., scales = "free_y") +
  geom_histogram() +
  scale_fill_viridis(discrete = TRUE)


