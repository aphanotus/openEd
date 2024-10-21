# BI377 Fall 2024 
# In-Class Code
# Week 6

# Student version

# Please, restart R!
.rs.restartR()

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

# Explore the data







# GPA



# PCA
?gm.prcomp



# Explore the PCA object that's created






# Calculate the proportion of variance described by each PC axis
# Recall that variance is standard deviation squared, and that variance is additive.  








# Scree plot






# Plot the PC coordinate positions of each specimen
?shape.space




# Color points by group





# Combine the factors for sex and population





# Outline areas of shape space occupied by particular groups using convex hulls







# Look into other dimensions of the shape space





# Include "back-transform" shapes to exemplify the axial extremes






# Magnify the differences






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

# PCA
pca.fw <- gm.prcomp(gpa.fw$gdf$coords)
pca.hw <- gm.prcomp(gpa.hw$gdf$coords)


# Exercise 6 will test the question of whether the Bombus wing datasets
# are consistent with the presence of one or two species

# Blinded metadata!
blinded.metadata <- read.csv("bi377.demo.borealis.v.fervidus.metadata.csv")
blinded.metadata

# Isolate the Specimen IDs from digitizers' initials 
x <- str_split_fixed(gpa.fw$gdf$specimen.id,"__",2)[,1]

# Match specimen IDs in the main dataset (`x`) with those in the `blinded.metadata`
i <- match(x,blinded.metadata$specimen_ID)

# Check that everything has a match (that there are no NAs) 
any(is.na(i))

# Create a new element of ther `gdf` list component with species information
gpa.fw$gdf$species <- blinded.metadata$species[i]

# Replace the specimen IDs with the shorter version
gpa.fw$gdf$specimen.id <- x


# Shape space plots



