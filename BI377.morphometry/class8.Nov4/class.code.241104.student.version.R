# BI377 Fall 2024
# In-Class Code
# Week 8

# Student Version

# Load some key packages of GMM tools
library(geomorph)
library(borealis)

########################
# Modularity tests
########################

data(pupfish)

landmark.plot(pupfish, links = "chull")

# Perform GPA

pup.gpa <-


# Define a hypothesis
# as a vector with the same length of the numnber of landmarks

modularity.hypothesis1 <-



?modularity.test

mt1 <- modularity.test(
  A =
  partition.gp =
  iter =
  print.progress = TRUE
)





# Try a different hypothesis









########################
# Disparity tests
########################

# First, get a look at the morphospace
pup.pca <- gm.prcomp(pup.gpa$gdf$coords)
sex.pop <- paste(pupfish$Sex,pupfish$Pop)

shape.space(
  pup.pca,
  group = sex.pop, convex.hulls = TRUE,
  include.legend = TRUE, group.title = "sex & pop."
)

?morphol.disparity

# Overall disparity: Morphological disparity for the entire data set
morphol.disparity(
  f1 =
  groups =
  data =
)


# Morphological disparity among and between groups
morphol.disparity(
  f1 =
  groups =
  data =
)

# Overall disparity, accounting for the influence of size
morphol.disparity(
  f1 =
  groups =
  data =
)

# Morphological disparity among and between groups, accounting for the influence of size
morphol.disparity(
  f1 =
  groups =
  data =
)


########################
# Phylogeny
########################


# Phylogenetic context

data("Bombus.tree")
Bombus.tree

# Plot it



# Explore the data





# Sub-setting
which(Bombus.forewings$metadata$caste == "W")

i <- which(Bombus.forewings$metadata$caste == "W")

?subsetgmm

w <-



# GPA
gpa.w <-
# Remove the 2 extreme outliers

# PCA - Look at the shape space
pca.w <-
?shape.space






# If we want to compare species shapes,
# can we account for evolutionary relationships?

# To do this, we need one shape to represent each species.

# Organize shapes by species
?coords.subset

coords.by.species <- coords.subset(
  gpa.w$gdf$coords,
  group = as.character(gpa.w$gdf$species)
)






# Get mean worker shapes by species

?mshape
?lapply

mshape.by.species <-






# Convert the mean shapes into a "plain" 3-D array data type

species.names <-



# "Data Munging"
# We need to turn the `mshape.by.species` object from a list into an array
is(mshape.by.species)

mshape.by.species <- array(
  unlist(mshape.by.species),
  c(gpa.w$landmark.number, 2, length(species.names))
)

dimnames(mshape.by.species) <- list(NULL,NULL,species.names)

is(mshape.by.species)


# Explore the data object some more










plot(Bombus.tree)

# Prune the tree
# Make a copy to modify
btree <- Bombus.tree

# tip labels
btree$tip.label

# Remove the genus abbreviations
?str_split_fixed
btree$tip.label <-

# truncate to the first 5 characters
?strtrim
btree$tip.label <-

# Keep tips that match the shape data abbreviations
library(phytools)

i <- unlist(lapply(species.names, function (x) { grep(paste0("^",x),btree$tip.label) }))

btree <- keep.tip(btree, i)
plot(btree)

# Quality control
species.names %in% btree$tip.label

btree$tip.label
species.names

btree$tip.label[1] <- "ferv"







species.names %in% btree$tip.label


plot(btree)

# PCA
PCA <-

plot(PCA, main = "PCA (without phylogeny)", )
text(x = PCA$x[,1], y = PCA$x[,2], labels = rownames(PCA$x), adj = c(-0.2,0))


# PCA with the phylogeny
PCA.w.phylo <-

plot(PCA.w.phylo, phylo = TRUE, main = "PCA with phylogeny")



# Phylogenetically-aligned PCA
# “...provides an ordination that aligns phenotypic data with
# phylogenetic signal, by maximizing variation in directions that
# describe phylogenetic signal, while simultaneously preserving the
# Euclidean distances among observations in the data space.”

PaCA <-

plot(PaCA, phylo = TRUE, main = "Phylogenetically-aligned PCA")

# Ancestral states
PaCA$ancestors

?arrayspecs

landmark.plot(
  arrayspecs(PaCA$ancestor, p = 20, k = 2),
  specimen.number = 4,
  links = fw.links
)

?plotRefToTarget




# Phylogenetic Generalized Least Squares Regression (PGLS)

?procD.pgls

?geomorph.data.frame

species.gdf <- geomorph.data.frame(


)

i <- 9999
# Does shape vary by size, after correcting for the influence of relatedness?
pgls.size <-
