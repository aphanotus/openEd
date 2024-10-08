# BI377 Fall 2024 
# In-Class Code
# Week 5

# Class Version

# Clean-up
# Remove objects from memory with `rm()`
?rm

# View a list of objects in memory with `ls()`
ls()

# Combine these functions to remove all objects!
rm(list = ls())
ls()

# Restart R from within R
.rs.restartR()

# Load some key packages of GMM tools
library(geomorph)
library(borealis)


########################
# Importing the data
########################

# Convert raw landmark coordinates into the tps file format

# R operates on files from a particular folder (also called a directory).
# This is called the "working directory".
# To check where in the directory structure it's currently working use `getwd()`

getwd()

# You can set the working directory using `setwd()` or ...
# Navigate to the folder you'd like to work from in the "Files" tab in Rstudio
# and select "Set As Working Directory" from commands available under the gear icon.

# For long or complicated file names, you can have R pull their exact name.

file.choose()

# Convert CSV format raw XY data into the TPS format required by GMM tools.

?create.tps

create.tps(
  input.filename = "bi377.demo.borealis.v.fervidus.forewings.class.csv",
  output.filename = "bi377.demo.borealis.v.fervidus.forewings.class.tps",
  id.factors = c("digitizer"),
  include.scale = TRUE,
  invert.scale = TRUE)

# Repeat for the hindwings

create.tps(
  input.filename = "bi377.demo.borealis.v.fervidus.hindwings.class.csv",
  output.filename = "bi377.demo.borealis.v.fervidus.hindwings.class.tps",
  id.factors = c("digitizer"),
  include.scale = TRUE,
  invert.scale = TRUE)


# Import TPS data into R

?read.tps

xy.forewings <- read.tps(
  file = "bi377.demo.borealis.v.fervidus.forewings.class.tps",
  keep.original.ids = TRUE
)

# Repeat for the hindwings

xy.hindwings <- read.tps(
  file = "bi377.demo.borealis.v.fervidus.hindwings.class.tps",
  keep.original.ids = TRUE
)

# Explore the data structure of `xy.forewings`

is(xy.forewings)

names(xy.forewings)

dim(xy.forewings$coords)
# This is an array with 3 dimensions
# Dimension 1 is the number of landmarks (20 in this case)
# Dimension 2 is the number of coordinate dimensions (2 in this case: X and Y)
# Dimension 3 is the number of specimens in the dataset (192 in this case)

# You can use sub-setting on the `coords` element.
# Let's look at the first specimen
xy.forewings$coords[,,1]

# The metadata is a data frame (one of R's table style data structures)
is(xy.forewings$metadata)
dim(xy.forewings$metadata)
head(xy.forewings$metadata)


# View the shape data

# A classic R plot doesn't represent shapes well. It stretchs the vertical
# and horizontal dimensions and just places dots for each landmark.
plot(xy.forewings$coords[,,1])

# A better tool is `landmark.plot`
?landmark.plot

# It numbers landmarks and maintains the aspect ratio.
landmark.plot(xy.forewings)

# The function is also robust and can accept shape data from
# different levels of the data structure.
landmark.plot(xy.forewings$coords)
landmark.plot(xy.forewings$coords[,,1])

# You can indicate different specimen numbers (or names)
landmark.plot(xy.forewings, specimen.number = 7)

# Adding "links", lines that connect certain landmarks, can help
# make anatomy more obvious.
# This matrix will help define the veins between the bumblebee
# forewing landmarks that we digitized.
fw.links <- matrix(c(1,2, 1,5, 5,4, 4,3, 3,2, 5,6, 6,7, 7,8, 8,9,
                     9,4, 3,11, 11,12, 11,10, 9,10, 10,14, 14,15, 15,16,
                     16,18, 18,20, 16,17, 17,8, 12,13, 13,19, 14,13, 18,19,
                     2,12),
                   ncol = 2, byrow = TRUE)

# If you uploaded the CSV file with this matrix, you can also import it.
(fw.links <- as.matrix(read.csv("fw.links.csv")))

# Use the `links` argument to `landmark.plot`
landmark.plot(xy.forewings, links = fw.links)

# Define links for hindwings
hw.links <- matrix(c(1:5,2:6), ncol = 2, byrow = FALSE)

landmark.plot(xy.hindwings, links =  hw.links)


########################
# Forewing GPA
########################

# Procrustes alignment for forewing shapes

?align.procrustes

# This function can be run in its most straight forward way with just
# the XY raw data object as input
gpa.fw <- align.procrustes(xy.forewings)
# The plot it generates shows landmarks from each specimen as gray dots.
# The consensus shape is indicated by large black dots.
# If the alignment is working well, then the black dots will be centered on
# clusters of gray dots and few if any gray dots will be on their own.
# So, this result is not a great fit!

# The issue can be revealed by looking at some of the specimens
landmark.plot(
  xy.forewings, links = fw.links,
  specimen.number = c(1,25,182,185)
)
# Some of them exist in all different possible orientations!
# GPA will translate, scale and rotate shapes, but it doesn't flip them!

# Reflect specimens

# We can use another tool to reflect (or flip) shapes.

?align.reflect

# `align.reflect` takes as input an XY shape object.
# You can specify if any of the landmarks should be at the top, bottom,
# left or right sides. It can also "show" one of the specimens at the end
# and will accept `links` too.

xy.forewings <- align.reflect(
  A = xy.forewings,
  top.pt = 2,
  left.pt = 19,
  links = fw.links,
  show.plot = 25
)

# Looking at the same four specimens, they
# are now all in the same orientation.
landmark.plot(
  xy.forewings, links = fw.links,
  specimen.number = c(1,25,182,185)
)

# These changes are all recorded automatically in the `provenance`
# element of the data structure.

names(xy.forewings$provenance)

cat(xy.forewings$provenance$align.reflect)

# After making these changes, you can try GPA again

# Procrustes alignment, again
# The `align.procrustes` function has an optional mode for `outlier.analysis`.
gpa.fw <- align.procrustes(xy.forewings, outlier.analysis = TRUE)
# The first plot is the same: superimposing black landmark consensus points
# on top of gray landmarks from individual specimens.
# In the console window, it says "Press any key to continue."
# Then the plot changes to a plot of Procrustes distance from the mean
# for each specimen. Potential outliers are highlighted in red and named.
# If there are specimens far from the average, it suggests these may be
# something other than biological variations, and they may require some curation.
# In the console window, it says "Press any key to continue."
# The next plot shows "deformation grids" of the 4 most deviant shapes.
# In this case, we see that the first two are very odd.
# The console says "Remove how many of the most extreme outliers?
# ('Enter' or '0' for none.)" -- In this case say 0, and we'll investigate more.

# Data curation

# The last plot of outlier analysis indicated that specimens 11 and 60
# were strong outliers. Let's use `landmark.plot` to get a better look at
# them and compare them to the specimens immediately preceeding them.

landmark.plot(
  xy.forewings, links = fw.links,
  specimen.number = c(10,11, 59,60)
)

# It looks like landmarks 18 and 19 are swapped in both specimens!

# How to fix this?
# I class, I asked you to work with your neighbor to find a
# computational solution to correct this issue.
# Incidentally, this is a great use of generative AI! ChatGPT, Gemini, etc.
# are good at suggesting solutions to similar problems. And this is an
# acceptable use of AI in this course. Just be sure you know what the code
# is doing!
# For example, ask ChatGTP...
# "I'm coding in R. I have a matrix with 2 columns and 20 rows. I need to
# swap the values from rows 18 and 19. How do I do that?"

# It may help to look at the values
xy.forewings$coords[18:19,,11]

# Define a temporary object to hold one of the rows
(tmp <- xy.forewings$coords[18,,11])

# Then overwrite that row with the other one
xy.forewings$coords[18,,11] <- xy.forewings$coords[19,,11]

# Then write over the second row with the temp object
xy.forewings$coords[19,,11] <- tmp

# Confirm it's done.
xy.forewings$coords[18:19,,11]

# Do the same for specimen 60
tmp <- xy.forewings$coords[18,,60]
xy.forewings$coords[18,,60] <- xy.forewings$coords[19,,60]
xy.forewings$coords[19,,60] <- tmp

# Check the same landmark plots to confirm the problem is fixed.
landmark.plot(
  xy.forewings, links = fw.links,
  specimen.number = c(10,11, 59,60)
)
# No corssing lines!


# Documenting changes

# Since we made that data curation manually, there's no data provenance
# to document it. But we can add that ourselves.

?add.provenance

xy.forewings <- add.provenance(
  xy.forewings,
  name="error.correction",
  title = "Corrected error in the placement of landmarks",
  text = "Specimens 11 and 60 had landmarks 18 and 19 swapped. This was manually corrected." )

names(xy.forewings$provenance)
cat(xy.forewings$provenance$error.correction)


# What to do if landmarks are missing?

# As an example, let's just set one landmark to NAs
xy.forewings$coords[,,192]
xy.forewings$coords[20,,192] <- NA

xy.forewings$coords[,,192]

# This will cause an error for most functions looking for shape data.
landmark.plot(xy.forewings, links = fw.links, specimen.number = c(191,192))

# The function `estimate.missing.landmarks` will impute missing (NA) values
# from the median position of that landmark in all other specimens
?estimate.missing.landmarks

xy.forewings <- estimate.missing.landmarks(xy.forewings)

xy.forewings$coords[,,192]

landmark.plot(xy.forewings, links = fw.links, specimen.number = c(191,192))

# GPA needs to be repeated after any data curation steps
# In other words, we want to use the final, curated data for GPA.

# Procrustes alignment, again, again
gpa.fw <- align.procrustes(xy.forewings, outlier.analysis = TRUE)

# In this case, the "outliers" are not extreme, and likely represent
# real biological variation. They should all be retained.

# Examine the data structure

names(gpa.fw)

# The concensus (or median) shape
landmark.plot(gpa.fw$consensus, links = fw.links)

# Metadata is present in an element called `gdf`
# This includes the specimen IDs, digitizer, and centroid size (Csize)
# as well as `coords`, which is then GPA-aligned XY coordinates.
names(gpa.fw$gdf)

dim(gpa.fw$gdf$coords)

gpa.fw$gdf$coords[,,1]

# `landmark.plot` can also plot shapes from a GPA object
landmark.plot(gpa.fw, links = fw.links)

########################
# Hindwings
########################

# Here's a streamlined workflow for the hindwings

# Reflect shapes to be consistent
landmark.plot(xy.hindwings, links = hw.links, specimen.number = c(1,15))

xy.hindwings <- align.reflect(
  A = xy.hindwings,
  top.pt = 1, left.pt = 6,
  links = hw.links,
  show.plot = 15
)

landmark.plot(xy.hindwings, links = hw.links, specimen.number = c(1,15))

# GPA
gpa.hw <- align.procrustes(xy.hindwings, outlier.analysis = TRUE)

# Looks good, so I didn't remove any specimens.


########################
# We'll examine Procrustes distances next time!
