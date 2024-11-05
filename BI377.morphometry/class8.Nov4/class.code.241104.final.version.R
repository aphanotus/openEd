# BI377 Fall 2024
# In-Class Code
# Week 8

# FINAL VERSION

# Load some key packages of GMM tools
library(geomorph)
library(borealis)

# We'll use the pupfish dataset to demonstrate tests for modularity and disparity
data(pupfish)
pup.gpa <- align.procrustes(pupfish)

# Plot a reminder of what the fish look like
landmark.plot(pupfish, links = "chull")

########################
# Modularity tests
########################

# Define a modularity hypothesis
# as a vector with the same length of the number of landmarks

# How many landmarks are there?
number.of.landmarks <- dim(pup.gpa$gdf$coords)[1]

modularity.hypothesis1 <- rep("body", number.of.landmarks)

i <-c(10,49:56)
modularity.hypothesis1[i] <- "eye"

modularity.hypothesis1

?modularity.test

mt1 <- modularity.test(
  A = pup.gpa$gdf$coords,
  partition.gp = modularity.hypothesis1,
  iter = 9999,
  print.progress = TRUE
)

mt1

plot(mt1)

# Try a different hypothesis

# For example...
landmark.plot(pupfish, links = "chull")

modularity.hypothesis2 <- rep("body", dim(pup.gpa$gdf$coords)[1])

i <- c(39:47)
modularity.hypothesis2[i] <- "operculum"

modularity.hypothesis2

mt2 <- modularity.test(
  A = pup.gpa$gdf$coords,
  partition.gp = modularity.hypothesis2,
  iter = 9999,
  print.progress = TRUE
)

mt2

plot(mt2)


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
  f1 = coords ~ 1,
  groups = NULL,
  data = pup.gpa$gdf
)
# Outputs overall Procrustes variance

# Morphological disparity among and between groups
morphol.disparity(
  f1 = coords ~ 1,
  groups = ~ sex.pop,
  data = pup.gpa$gdf
)
# Outputs:
# - Procrustes variance of each group
# - Pairwise differences between groups
# - P-values associated with pairwise differences

# Overall disparity, accounting for the influence of size
morphol.disparity(
  f1 = coords ~ Csize,
  groups = NULL,
  data = pup.gpa$gdf
)

# Morphological disparity among and between groups, accounting for the influence of size
morphol.disparity(
  f1 = coords ~ Csize,
  groups = ~ sex.pop,
  data = pup.gpa$gdf
)

# The best approach would be to first test for a significant
# influence of size on shape, using `procD.lm`, e.g.

size.model <- procD.lm(
  f1 = coords ~ Csize,
  data = pup.gpa$gdf
)
anova(size.model)

# Based on that result, decide whether your test for differences in
# disparity among groups should account for size or not



########################
# Phylogenetic PCA
########################

# To demonstrate how to incorporate phylogenetic information, let's
# turn to a different dataset: the bumblebee wing shapes in `borealis`

data(Bombus.forewings, package = "borealis")

names(Bombus.forewings)
head(Bombus.forewings$metadata)
unique(Bombus.forewings$metadata$caste)

# Since this dataset includes wings from workers, males and queens,
# let's filter it to include only worker wing shapes.

# Sub-setting
which(Bombus.forewings$metadata$caste == "W")

# Save the worker wing index numbers in `i`
i <- which(Bombus.forewings$metadata$caste == "W")

# Since shape data structures are complex, we'll use a function for subsetting
?subsetgmm

w <- subsetgmm(Bombus.forewings, specimens = i)

# Redefine the matrix of lines connecting landmarks, just
# to make it look more like a real bumble bee wing
fw.links <- matrix(
  c(1,2, 1,5, 5,4, 4,3, 3,2, 5,6, 6,7, 7,8, 8,9,
    9,4, 3,11, 11,12, 11,10, 9,10, 10,14, 14,15, 15,16,
    16,18, 18,20, 16,17, 17,8, 12,13, 13,19, 14,13, 18,19,
    2,12),
  ncol = 2, byrow = TRUE
)

landmark.plot(w, links = fw.links)

# Reflect the shapes so they're all oriented the same way
# (anterior up; proximal left)
w <- align.reflect(w, top.pt = 2, left.pt = 19)

# GPA
gpa.w <- align.procrustes(w, outlier.analysis = TRUE)
# Remove the 2 extreme outliers

# PCA - Look at the shape space
pca.w <- gm.prcomp(gpa.w$gdf$coords)
?shape.space
shape.space(
  pca.w,
  group = gpa.w$gdf$species, convex.hulls = TRUE
)

# Great!

# Now an obvious question is, are wing shapes different by species?
# It looks like this is the case, but...
# Can we account for evolutionary relationships?
# Can we give less weight to species that are closely related
# and impact more to species that are distantly related?

# To do this, we need one shape to represent each species.

# Unfortunately, there isn't currently a simple way to do this.
# The code that follows does this with annotations explaining what
# each step accomplishes. However, to aid you in reproducing this workflow
# critical steps are annotated with "**Critical step**"

# Group and label shapes by species
?coords.subset

# **Critical step**
coords.by.species <- coords.subset(
  gpa.w$gdf$coords,
  group = as.character(gpa.w$gdf$species)
)

# Get mean worker shapes by species

# The function `mshape` calculates mean shapes
?mshape

# `lapply` is a powerful and versatile function.
# It applies a function you name to each element of a list
?lapply

# **Critical step**
mshape.by.species <- lapply(coords.by.species, mshape)

# Note that the object `mshape.by.species` is list.
# Each of its elements is a single shape, named for a species
is(mshape.by.species)
names(mshape.by.species)
dim(mshape.by.species$bimac)
mshape.by.species$bimac

# We can plot those shapes. For example, this is the
# mean shape for B. bimaculatus wings.
landmark.plot(mshape.by.species$bimac, links = fw.links)

# However, later functions will require an array, not a list.
# Therefore, we need to convert `mshape.by.species` into an array.
# There isn't a simple way to do this, so we need to do what
# some coders call "data munging", the somewhat inelegant conversion
# of data into a different format.


# Convert the mean shapes into a "plain" 3-D array data type

# Start by saving the species names from the shape object
# as a seperate vector
species.names <- names(mshape.by.species)

species.names

# Again, we're starting with `mshape.by.species` as a list
is(mshape.by.species)

# Next, we redefine `mshape.by.species` as an array by first "unlisting" it
# (converting it to a vector using `unlist`). The `dim` argument below
# resets the dimensions of the array be:
# - dimension 1: the number of landmarks,
# - dimension 2: 2 (for the 2D landmark coordinates, X and Y),
# - dimension 3: the number of specimens
# **Critical step**
mshape.by.species <- array(
  data = unlist(mshape.by.species),
  dim = c(gpa.w$landmark.number, 2, length(species.names))
)

# Re-apply the species names to label the specimens
# in dimension 3 of the array
# **Critical step**
dimnames(mshape.by.species) <- list(NULL,NULL,species.names)

# Now we can confirm that `mshape.by.species` is an array.
is(mshape.by.species)

# Optionally, explore the data object some more
dim(mshape.by.species)

dimnames(mshape.by.species)

mshape.by.species[,,3]

landmark.plot(mshape.by.species[,,3], links = fw.links)

# Great!

# Now that we have a mean shape representing each species, we can
# look at the phylogeny for these species.

# There is a tree of bumble bee species in the `borealis` package
# **Critical step**
data(Bombus.tree, package = "borealis")

# Some info on the phylogeny
?Bombus.tree

# Plot the tree
plot(Bombus.tree)

# To make this tree work for us we need to do two things:
# - Remove species for which we don't have shape data
# - Change the species names to match the abbreviations in the shape data

# First, let's make a copy to modify
# **Critical step**
btree <- Bombus.tree

# Tip labels are stored in the `tip.label` element of tree data structures
btree$tip.label

# Remove the genus abbreviations using `str_split_fixed` from
# the tidyverse package `stringr`
library(stringr)
?str_split_fixed

# We'll "split" the species names in the tree at the space character
# taking the second piece.

# **Critical step**
btree$tip.label <- str_split_fixed(btree$tip.label," ",2)[,2]

# Next, truncate to names to the first 5 characters using `strtrim`
?strtrim

# **Critical step**
btree$tip.label <- strtrim(btree$tip.labe,5)

# Now we'll use a function from the phylogenetics package `phytools`
# to trim the tree now to match the species we have in our shape data.
library(phytools)
?keep.tip

# The line below uses `lapply` and `grep` to search (that's what grep does)
# to identify the positions in `btree$tip.label` that match the character strings
# in `species.names`, which has the species abbreviations from `mshape.by.species`.
?grep

# **Critical step**
i <- unlist(lapply(species.names, function (x) { grep(paste0("^",x),btree$tip.label) }))

# **Critical step**
btree <- keep.tip(btree, i)
plot(btree)

# Quality control -- This asks whether each `species.names` appears
# as a `tip.label` exactly
species.names %in% btree$tip.label

# Not all of them do!

# We can review the two sets of names we're trying to match up.
btree$tip.label
species.names

# At this point the simplest solution is to manually assign
# values to `btree$tip.label` to make them match perfectly!

# **Critical step**
btree$tip.label[1] <- "ferv"
btree$tip.label[2] <- "bor"
btree$tip.label[3] <- "tern"
btree$tip.label[4] <- "imp"
btree$tip.label[6] <- "vag"

# Another look at the tree
plot(btree)

# Repeat the quality control...
species.names %in% btree$tip.label

# They all match! Now we can move ahead combining phylogenetic information
# with our analysis of bumble bee wing shape

# First, let's look at PCA without any phylogenetic information.
# This will be different than out last PCA, because here we only have
# one wing shape representing each species.

# PCA
PCA <- gm.prcomp(mshape.by.species)

plot(PCA, main = "PCA (without phylogeny)")
text(x = PCA$x[,1], y = PCA$x[,2], labels = rownames(PCA$x), adj = c(-0.2,0))

# PCA with the phylogeny
PCA.w.phylo <- gm.prcomp(mshape.by.species, phy = btree)

plot(PCA.w.phylo, phylo = TRUE, main = "PCA with phylogeny")

# Phylogenetically-aligned PCA
# "...provides an ordination that aligns phenotypic data with
# phylogenetic signal, by maximizing variation in directions that
# describe phylogenetic signal, while simultaneously preserving the
# Euclidean distances among observations in the data space."
# ([Collyer & Adams 2020](https://doi.org/10.1111/2041-210X.13515))

PaCA <- gm.prcomp(mshape.by.species, phy = btree, align.to.phy = TRUE)

plot(PaCA, phylo = TRUE, main = "Phylogenetically-aligned PCA")

# Phylogenetically-aligned PCA will typically be the best complement
# to regular (non-phylogenetic) PCA.



########################
# Ancestral states
########################

# Predicted ancestral states are saved in PCA data structures made
# with phylogenetic information. For example

PaCA$ancestors

# In this table each row contains GPA-aligned shape coordinates for
# an ancestral node in the phylogeny (one of the gray dots in the
# phylogeny that's superimposed on the shape space).
# Each column in the table is X and Y for a landmark. This is a
# different format of shape data than we've seen. So it must be
# converted into the 3-D array style of the `coords` shape data
# we've seen used by many functions previously.
# The `geomorph` function `arrayspecs` can do this if we tell it
# the number of landmarks (`p = 20`) and dimensions (`k = 2`).

landmark.plot(
  arrayspecs(PaCA$ancestor, p = 20, k = 2),
  specimen.number = 4,
  links = fw.links
)

# This is the predicted wing shape of the common ancestor of
# species in the subgenus *Pyrobombus*.

# We can also use another function from the `geomorph` package,
# `plotRefToTarget` to compare how similar this wing shape is
# to the overall mean shape. The difference is subtle, so the
# code below amplifies the difference using `mag = 5`.

plotRefToTarget(
  M1 = arrayspecs(PaCA$ancestor, p = 20, k = 2)[,,4],
  M2 = mshape(gpa.w$gdf$coords),
  links = fw.links,
  mag = 5
)



########################
# PGLS
########################

# Phylogenetic Generalized Least Squares Regression (PGLS)
# is a weighed version of ANOVA that will account for relatedness.
# Similar to the phylogenetic PCA, it requires only one shape to
# represent each taxon (tip) in the phylogeny.
?procD.pgls

# The `geomorph` function `procD.pgls` will require the shape
# and phylogenetic information be input as a special data type,
# a `geomorph.data.frame`.
?geomorph.data.frame

species.gdf <- geomorph.data.frame(
  coords = mshape.by.species,
  Csize = c(by(gpa.w$gdf$Csize, gpa.w$gdf$species, mean)),
  tree = btree
)

# Let's put it all together and test the question,
# "Does shape vary by size, after correcting for relatedness?"
pgls.size <- procD.pgls(
  f1 = coords ~ log(Csize),
  phy = tree,
  data = species.gdf,
  iter = 9999
)

anova(pgls.size)

# The output is an ANOVA table. But how should be interpret results?
# Remember `f1` formula here is testing the influence of size
# (`~ log(Csize)`) on shape (`coords`). So the null hypothesis is that
# there is no difference in shape according to size. Because this is
# PGLS, the pseudoreplication effect that relatedness has is being
# accounted for by weighting differences based on branch lengths in
# the tree. So, while it might be counter-intuitive, you can't use PGLS
# to test for shape differences by species! It just gives you a more
# fair way to testing the influence of other factors on shape.
# So in this example, we can conclude that, yes, size does appear to
# influence bumble bee wing shapes, even after accounting for the
# relatedness of species.

# Right now, there is no implementation of a post-hoc contrast test
# for PGLS. However, Dean Adams and Michael Collyer are working on it.
# (https://doi.org/10.1111/2041-210X.14438)





########################
# Randomly assign groups
########################

# In case you're curious how I've used R to randomly assign groups,
# I thought I'd share that code.

class.roster <- c(
  "Annie", "Anya", "Caitlin", "Cole", "Eliza",
  "Imoni", "Jack", "James", "Margaret", "MJ",
  "Randall", "Riddhavee", "Samuel", "Tim", "Will"
)

# Set the group size
# For example...
group.size <- 3
# or
(group.size <- floor(length(class.roster)/2))

# The function `set.seed` initializes random number generation.
set.seed(123)

# Randomly re-shuffle the class roster
x <- sample(class.roster)

# Divide the re-shuffled vector of names by the group size

# Use the modulo operator (`%%`) to find the starting index numbers of each group
for (i in which((1:length(class.roster) %% group.size) == 0)) {
  # Print out group membership
  cat("Group:",i/group.size,"\t",paste(x[(i-(group.size-1)):i], collapse = ", "))
  # If there's an odd number, add the last person to the last group
  # This logic test asks if `i` is at its last value
  # and if there should be an odd person out, based on group size
  if ((i==floor(length(class.roster)/group.size)*group.size) & (length(class.roster) %% group.size) == 1) {
    cat(",",x[length(class.roster)])
  }
  cat("\n")
}

# The end!
