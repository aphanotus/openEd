# Exercise 5. PCA

# Problem 1
# Use the code from last week (October 18) to repeat the data curation and GPA on the `bi377.demo.borealisv.v.fervidus` forewing and hindwing datasets. If you already have the GPA objects for both datasets, then great!

# Borrowing code from last week!
# If you already have the GPA objects in your R environment, then
# you may not need to re-run these lines
fw.links <- matrix(c(1,2, 1,5, 5,4, 4,3, 3,2, 5,6, 6,7, 7,8, 8,9,
                     9,4, 3,11, 11,12, 11,10, 9,10, 10,14, 14,15, 15,16,
                     16,18, 18,20, 16,17, 17,8, 12,13, 13,19, 14,13, 18,19,
                     2,12),
                   ncol = 2, byrow = TRUE)
(fw.links <- as.matrix(read.csv("fw.links.csv")))
hw.links <- matrix(c(1:5,2:6), ncol = 2, byrow = FALSE)

xy.fw <- read.tps("bi377.demo.borealis.v.fervidus.forewings.class.tps", keep.original.ids = TRUE)
xy.hw <- read.tps("bi377.demo.borealis.v.fervidus.hindwings.class.tps", keep.original.ids = TRUE)
xy.fw <- align.reflect(xy.fw, top.pt = 2, left.pt = 19, links = fw.links, show.plot = 15)
xy.hw <- align.reflect(xy.hw, top.pt = 1, left.pt = 6, links = hw.links, show.plot = 15)
# Data curation
xy.fw$coords[7:10,,61] <- xy.fw$coords[8:11,,61]
xy.fw$coords[11,,61] <- NA
xy.fw <- add.provenance(
  xy.fw,
  name="error.correction",
  title = "Corrected error in the placement of landmarks 7-11",
  text = "The digitizer placed landmark 7 twice and shifted the position of landmarks 7-10. No data exist for landmark 11, which was set to NA." )
xy.fw <- estimate.missing.landmarks(xy.fw)
gpa.fw <- align.procrustes(xy.fw)
tmp <- xy.hw$coords[2,,45]
xy.hw$coords[2,,45] <- xy.hw$coords[3,,45]
xy.hw$coords[3,,45] <- tmp
xy.hw <- add.provenance(
  xy.hw,
  name="error.correction",
  title = "Corrected error in the placement of landmarks 2&3",
  text = "The digitizer swapped landmarks 2 and 3. This error has been corrected" )
gpa.hw <- align.procrustes(xy.hw)

# Show the names of data provenance entries on your forewing and hindwing data objects, after GPA.

names(gpa.fw$provenance)
names(gpa.hw$provenance)

# Challenge 1
# Save these data objects to a file.

save(gpa.fw, fw.links, gpa.hw, hw.links,
     file = "bi377.demo.borealisv.v.fervidus.gpa.rda")

load("bi377.demo.borealisv.v.fervidus.gpa.rda", verbose = TRUE)


# ### Problem 2
# Perform PCA on the two GPA-aligned wing datasets.
# Examine the distribution of variance associated with each principle component axis using the functions `pcvar` and `screeplot`, for each dataset

pca.fw <- gm.prcomp(gpa.fw$gdf$coords)
pca.hw <- gm.prcomp(gpa.hw$gdf$coords)

pcvar(pca.fw)
pcvar(pca.hw)

scree.plot(pca.fw)
scree.plot(pca.hw)

# ### Problem 3
# Import the metadata into R.
blinded.metadata <- read.csv("bi377.demo.borealis.v.fervidus.metadata.csv")
blinded.metadata

species <- rep(blinded.metadata$species,8)
caste <- rep(blinded.metadata$caste,8)
sp.caste <- paste(species,caste)

# Forewings
shape.space(pca.fw, group = species, convex.hulls = TRUE,
            include.legend = TRUE, group.title = "species")

shape.space(pca.fw, group = species, convex.hulls = TRUE,
            include.legend = TRUE, group.title = "species",
            axis1 = 3, axis2 = 5)

shape.space(pca.fw, group = caste, convex.hulls = TRUE,
            include.legend = TRUE, group.title = "caste")

shape.space(pca.fw, group = caste, convex.hulls = TRUE,
            include.legend = TRUE, group.title = "caste",
            axis1 = 1, axis2 = 3)

shape.space(pca.fw, group = sp.caste, convex.hulls = TRUE,
            include.legend = TRUE, group.title = "species & caste",
            axis1 = 1, axis2 = 3)

shape.space(pca.fw, group = gpa.fw$gdf$digitizer, convex.hulls = TRUE,
            include.legend = TRUE, group.title = "digitizer")

shape.space(pca.fw, group = gpa.fw$gdf$digitizer, convex.hulls = TRUE,
            include.legend = TRUE, group.title = "digitizer",
            axis1 = 1, axis2 = 3)

# Hindwings
shape.space(pca.hw, group = species, convex.hulls = TRUE,
            include.legend = TRUE, group.title = "species")

shape.space(pca.hw, group = species, convex.hulls = TRUE,
            include.legend = TRUE, group.title = "species",
            axis1 = 1, axis2 = 3)

shape.space(pca.hw, group = caste, convex.hulls = TRUE,
            include.legend = TRUE, group.title = "caste")

shape.space(pca.hw, group = caste, convex.hulls = TRUE,
            include.legend = TRUE, group.title = "caste",
            axis1 = 1, axis2 = 3)

shape.space(pca.hw, group = sp.caste, convex.hulls = TRUE,
            include.legend = TRUE, group.title = "species & caste")

shape.space(pca.hw, group = gpa.hw$gdf$digitizer, convex.hulls = TRUE,
            include.legend = TRUE, group.title = "digitizer")

shape.space(pca.hw, group = gpa.hw$gdf$digitizer, convex.hulls = TRUE,
            include.legend = TRUE, group.title = "digitizer",
            axis1 = 1, axis2 = 3)
