# BI377 Fall 2024
# In-Class Code
# Week 6

# FINAL CLASS VERSION

# Please, restart R!
# Behind the scenes, some packages have been updated, necessitating a restart.
.rs.restartR()

# Load some key packages of GMM tools
library(geomorph)
library(borealis)
library(stringr)

########################
# Importing the data
########################

# Landmark data from Cyprindon pecosensis body shapes, with indication of
# Sex and Population from which fish were sampled (Marsh or Sinkhole).
# (Collyer et al. 2015 Heredity)
data(pupfish, package = "geomorph")

# Explore the data
# e.g.

landmark.plot(pupfish, links = "chull")

names(pupfish)

pupfish$Sex
pupfish$Pop

# GPA
gpa.pupfish <- align.procrustes(pupfish)

# PCA
pca.pupfish <- gm.prcomp(gpa.pupfish$gdf$coords)

# Explore the PCA object that's created
# e.g.
names(pca.pupfish)

# Calculate the proportion of variance described by each PC axis
(pca.pupfish$sdev^2)/(sum(pca.pupfish$sdev^2))

# There's also a convenient function to do this
?pcvar
pcvar(pca.pupfish)

# Scree plot: plot that information over all dimensions
barplot((pca.pupfish$sdev^2)/(sum(pca.pupfish$sdev^2)))
abline(h=0.05, col = "darkred")

# There's also a convenient function to do this
?scree.plot
scree.plot(pca.pupfish)

# Limit the number of dimensions that are included
scree.plot(pca.pupfish, 10)

# Plot the PC coordinate positions of each specimen
?shape.space

shape.space(pca.pupfish)

# Color points by group
shape.space(pca.pupfish, group = pupfish$Sex)
shape.space(pca.pupfish, group = pupfish$Pop)

# Combine the factors for sex and population
sex.pop <- paste(pupfish$Sex,pupfish$Pop)
shape.space(pca.pupfish, group = sex.pop)

# Outline areas of shape space occupied by particular groups using convex hulls
shape.space(pca.pupfish, group = sex.pop, convex.hulls = TRUE)

# Add a legend
shape.space(
  pca.pupfish, group = sex.pop, convex.hulls = TRUE,
  include.legend = TRUE, group.title = "sex & pop."
)

# Look into other dimensions of the shape space
shape.space(
  pca.pupfish, group = sex.pop, convex.hulls = TRUE,
  include.legend = TRUE, group.title = "sex & pop.",
  axis1 = 1, axis2 = 3
)

# Include "back-transform" shapes to exemplify the extreme shapes along each axis
shape.space(
  pca.pupfish, group = sex.pop, convex.hulls = TRUE,
  include.legend = TRUE, group.title = "sex & pop.",
  backtransform.examples = TRUE,
  ref.shape = gpa.pupfish$consensus
)

# Magnify shape differences in those example shape plots
# This function call also uses a different `shape.method` argument
shape.space(
  pca.pupfish, group = sex.pop, convex.hulls = TRUE,
  include.legend = TRUE, group.title = "sex & pop.",
  backtransform.examples = TRUE,
  bt.shape.mag = 2,
  shape.method = "TPS", # default is "points"
  ref.shape = gpa.pupfish$consensus
)


#####
# B. borealis v. B. fervidus?
#####

# Borrowing code from last week!
# If you already have the GPA objects in your R environment, then
# you may not need to re-run these lines
fw.links <- matrix(
  c(1,2, 1,5, 5,4, 4,3, 3,2, 5,6, 6,7, 7,8, 8,9,
    9,4, 3,11, 11,12, 11,10, 9,10, 10,14, 14,15, 15,16,
    16,18, 18,20, 16,17, 17,8, 12,13, 13,19, 14,13, 18,19,
    2,12),
  ncol = 2, byrow = TRUE
)
# Or
# (fw.links <- as.matrix(read.csv("fw.links.csv")))
hw.links <- matrix(c(1:5,2:6), ncol = 2, byrow = FALSE)

# It was necessary to curate some of the specimen IDs in the raw data.
# Therefore, please upload them again from
# https://github.com/aphanotus/openEd/tree/main/BI377.morphometry/class5.Oct7
# and repeat the steps below.

create.tps(
  input.filename = "bi377.demo.borealis.v.fervidus.forewings.class.csv",
  output.filename = "bi377.demo.borealis.v.fervidus.forewings.class.tps",
  id.factors = c("digitizer"),
  include.scale = TRUE,
  invert.scale = TRUE)

create.tps(
  input.filename = "bi377.demo.borealis.v.fervidus.hindwings.class.csv",
  output.filename = "bi377.demo.borealis.v.fervidus.hindwings.class.tps",
  id.factors = c("digitizer"),
  include.scale = TRUE,
  invert.scale = TRUE)

xy.forewings <- read.tps("bi377.demo.borealis.v.fervidus.forewings.class.tps", keep.original.ids = TRUE)
xy.hindwings <- read.tps("bi377.demo.borealis.v.fervidus.hindwings.class.tps", keep.original.ids = TRUE)
xy.forewings <- align.reflect(xy.forewings, top.pt = 2, left.pt = 19, links = fw.links, show.plot = 15)
xy.hindwings <- align.reflect(xy.hindwings, top.pt = 1, left.pt = 6, links = hw.links, show.plot = 15)

# Data curation
tmp <- xy.forewings$coords[18,,c(11,60)]
xy.forewings$coords[18,,c(11,60)] <- xy.forewings$coords[19,,c(11,60)]
xy.forewings$coords[19,,c(11,60)] <- tmp
xy.forewings <- add.provenance(
  xy.forewings,
  name="error.correction",
  title = "Corrected error in the placement of landmarks",
  text = "Specimens 11 and 60 had landmarks 18 and 19 swapped. This was manually corrected." )

# GPA
gpa.fw <- align.procrustes(xy.forewings)
gpa.hw <- align.procrustes(xy.hindwings)

# Add in the previously blinded metadata
# https://raw.githubusercontent.com/aphanotus/openEd/refs/heads/main/BI377.morphometry/class6.Oct21/bi377.demo.borealis.v.fervidus.metadata.csv
blinded.metadata <- read.csv("bi377.demo.borealis.v.fervidus.metadata.csv")
blinded.metadata

# This is a simple table listing the species name for each specimen in our dataset

# We need to match this information to the repeated specimens in shape data,
# which appear in different orders!
# The code below uses some R tricks to make that happen

# The current `specimen.id` factor includes the actualy specimen ID and the digitizer
# Let's break that up using `stringr::str_split_fixed``
x <- str_split_fixed(gpa.fw$gdf$specimen.id,"__",2)[,1]

# Then use `match` to find the row indices where each specimen ID appears
i <- match(x,blinded.metadata$specimen_ID)

# Check that there are no NAs
# If you run this code from a clean instance of R, it should return FALSE
any(is.na(i))

# Use the `i` index to assign values for `species` to the metadata in `gpa.fw`
gpa.fw$gdf$species <- blinded.metadata$species[i]

# Overwrite `specimen.id` with the IDs without digitizer initials
gpa.fw$gdf$specimen.id <- x

# Repeat those steps for the hindwing data
x <- str_split_fixed(gpa.hw$gdf$specimen.id,"__",2)[,1]
i <- match(x,blinded.metadata$specimen_ID)
any(is.na(i))
gpa.hw$gdf$species <- blinded.metadata$species[i]
gpa.hw$gdf$specimen.id <- x


# From here [Exercise 6](https://hackmd.io/@ColbyBI377/exercise6) Problem 2
# asks you to perform and interpret PCA!
