#####
# GPA with jointed structures
#####

library(tidyverse)
library(viridis)
library(borealis)

names(mantis)

dim(mantis$coords)

names(mantis$provenance)

cat(mantis$provenance$create.tps)

# Define mantis.lines
{
  x <- 1:16
  mantis.lines <- matrix(c(x[-length(x)],x[-1]), ncol = 2)
  mantis.lines[10,] <- c(10,1)
  mantis.lines[15,] <- c(15,6)
  mantis.lines <- rbind(mantis.lines,
                        matrix(c(5,11, 6,11, 13,16, 14,16), ncol = 2, byrow = TRUE))
}

landmark.plot(mantis, specimen.number = 1:4, links = mantis.lines)

# angle.pts.1, art.pt (the vertex), and angle.pts.2 define the angle to enforce
# rot.pts defines the landmarks to actually rotate
mantis2 <- align.angle(mantis,
                       art.pt = 11,
                       angle.pts.1 = 1:10,
                       angle.pts.2 = 12:15,
                       rot.pts = 12:16)

landmark.plot(mantis2, specimen.number = 1:4, links = mantis.lines)

# By default the average of all specimens defines the reference angle
# However reference.specimen can specify one of more specimens to use.
mantis3 <- align.angle(mantis,
                       art.pt = 11,
                       angle.pts.1 = 1:10,
                       angle.pts.2 = 12:15,
                       rot.pts = 12:16,
                       reference.specimen = c(1,7) )

landmark.plot(mantis3, specimen.number = 1:4, links = mantis.lines)


# Below is code to examine all pairwise Procrustes distances 
# between specimens, before and after GPA.
{
  pw.procD.mantis <- vector(mode = "numeric")
  pw.procD.mantis2 <- vector(mode = "numeric")
  pw.procD.mantis3 <- vector(mode = "numeric")
  
  for (i in 1:(mantis$specimen.number-1)) {
    for (j in (i+1):mantis$specimen.number) {
      x <- procrustes.distance(mantis$coords[,,i], mantis$coords[,,j])
      pw.procD.mantis <- c(pw.procD.mantis, x)
      x <- procrustes.distance(mantis2$coords[,,i], mantis2$coords[,,j])
      pw.procD.mantis2 <- c(pw.procD.mantis2, x)
      x <- procrustes.distance(mantis3$coords[,,i], mantis3$coords[,,j])
      pw.procD.mantis3 <- c(pw.procD.mantis3, x)
    }
  }
}

# Organize the vectors in a data frame
df <- data.frame(
  mantis = pw.procD.mantis,
  mantis2 = pw.procD.mantis2,
  mantis3 = pw.procD.mantis3
)

plot.xy.mantis <- df %>% 
  pivot_longer(cols = 1:3, names_to = "stage") %>% 
  ggplot(aes(x=value)) +
  theme_bw() +
  facet_grid(stage ~ ., scales = "free_y") +
  geom_histogram()
plot.xy.mantis


procrustes.jackknife(mantis)
procrustes.jackknife(mantis2)
procrustes.jackknife(mantis3)

#####
# PCA
#####

data(pupfish, package = "geomorph")

landmark.plot(pupfish, links = "chull")

names(pupfish)

pupfish$Sex
pupfish$Pop

gpa.pupfish <- align.procrustes(pupfish)
pca.pupfish <- gm.prcomp(gpa.pupfish$gdf$coords)

names(pca.pupfish)

(pca.pupfish$sdev^2)/(sum(pca.pupfish$sdev^2))

pcvar(pca.pupfish)

# Scree plot
barplot((pca.pupfish$sdev^2)/(sum(pca.pupfish$sdev^2)))
abline(h=0.05, col = "darkred")

?shape.space

install.packages("ggplotify")

shape.space(pca.pupfish)

shape.space(pca.pupfish, group = pupfish$Sex)
shape.space(pca.pupfish, group = pupfish$Pop)

sex.pop <- paste(pupfish$Sex,pupfish$Pop)
shape.space(pca.pupfish, group = sex.pop)

shape.space(pca.pupfish, group = sex.pop, convex.hulls = TRUE)

shape.space(pca.pupfish, group = sex.pop, convex.hulls = TRUE, 
            include.legend = TRUE, group.title = "sex & pop.")

shape.space(pca.pupfish, group = sex.pop, convex.hulls = TRUE, 
            include.legend = TRUE, group.title = "sex & pop.",
            axis1 = 1, axis2 = 3)

shape.space(pca.pupfish, group = sex.pop, convex.hulls = TRUE, 
            include.legend = TRUE, group.title = "sex & pop.",
            backtransform.examples = TRUE,
            ref.shape = gpa.pupfish$consensus)

#####
# B. borealis v. B. fervidus?
#####

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

# Blinded metadata!
blinded.metadata <- read.csv("bi377.demo.borealis.v.fervidus.metadata.csv")
blinded.metadata


# Forewing PCA
pca.fw <- gm.prcomp(gpa.fw$gdf$coords)
pcvar(pca.fw)

shape.space(pca.fw, group = blinded.metadata$species, convex.hulls = TRUE, 
            include.legend = TRUE, group.title = "species")

species <- rep(blinded.metadata$species,8)

shape.space(pca.fw, group = species, convex.hulls = TRUE, 
            include.legend = TRUE, group.title = "species")

shape.space(pca.fw, group = species, convex.hulls = TRUE, 
            include.legend = TRUE, group.title = "species",
            axis1 = 1, axis2 = 3)

shape.space(pca.fw, group = species, convex.hulls = TRUE, 
            include.legend = TRUE, group.title = "species",
            backtransform.examples = TRUE,
            ref.shape = gpa.fw$consensus)



