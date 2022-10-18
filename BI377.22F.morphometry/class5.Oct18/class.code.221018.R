library(tidyverse)

devtools::install_github("aphanotus/borealis")

library(borealis)


setwd("~/Documents/5.teaching/openEd/BI377.22F.morphometry/class5.Oct18")

# Convert raw landmark coordinates into the tps file format

?create.tps

file.choose()

create.tps(
  input.filename = "bi377.demo.borealis.v.fervidus.forewings.class.csv",
  output.filename = "bi377.demo.borealis.v.fervidus.forewings.class.tps",
  id.factors = c('digitizer'),
  include.scale = TRUE,
  invert.scale = TRUE)

create.tps(
  input.filename = "bi377.demo.borealis.v.fervidus.hindwings.class.csv",
  output.filename = "bi377.demo.borealis.v.fervidus.hindwings.class.tps",
  id.factors = c('digitizer'),
  include.scale = TRUE,
  invert.scale = TRUE)

# Import tps data into R

?read.tps

xy.fw <- read.tps("bi377.demo.borealis.v.fervidus.forewings.class.tps", keep.original.ids = TRUE)
xy.hw <- read.tps("bi377.demo.borealis.v.fervidus.hindwings.class.tps", keep.original.ids = TRUE)

names(xy.fw) 
names(xy.fw$provenance)

cat(xy.fw$provenance$read.tps)

# View the shape data

plot(xy.fw$coords[,,1])

landmark.plot(xy.fw)

landmark.plot(xy.fw, specimen.number = 1)
landmark.plot(xy.fw$coords)
landmark.plot(xy.fw$coords[,,1])

landmark.plot(xy.fw, specimen.number = 2)

# Viewing shapes with landmark connections

fw.links <- matrix(c(1,2, 1,5, 5,4, 4,3, 3,2, 5,6, 6,7, 7,8, 8,9, 9,4, 3,11, 11,12, 11,10, 9,10, 10,14, 14,15, 15,16, 16,18, 18,20, 16,17, 17,8, 12,13, 13,19, 14,13, 18,19, 2,12),
                   ncol = 2, byrow = TRUE)

write.csv(fw.links, "fw.links.csv", row.names = FALSE, quote = FALSE)

(fw.links <- as.matrix(read.csv("fw.links.csv")))

landmark.plot(xy.fw, links = fw.links)

landmark.plot(xy.fw, links = fw.links, specimen.number = 1:4)

hw.links <- matrix(c(1:5,2:6), ncol = 2, byrow = FALSE)

landmark.plot(xy.hw, links = hw.links, specimen.number = 1:9)


# Procrustes alignment

gpa.fw <- align.procrustes(xy.fw)

gpa.fw <- align.procrustes(xy.fw, outlier.analysis = TRUE)

landmark.plot(xy.fw, links = fw.links, specimen.number = c(1,15))


# Reflect specimens

xy.fw <- align.reflect(xy.fw, top.pt = 2, left.pt = 19, links = fw.links, show.plot = 15)

landmark.plot(xy.hw, links = hw.links, specimen.number = c(1,15))

xy.hw <- align.reflect(xy.hw, top.pt = 1, left.pt = 6, links = hw.links, show.plot = 15)


# Procrustes alignment, again

gpa.fw <- align.procrustes(xy.fw)

gpa.fw <- align.procrustes(xy.fw, outlier.analysis = TRUE)

# Data curation

landmark.plot(xy.fw, links = fw.links, specimen.number = c(61,62))

xy.fw$coords[7:10,,61] <- xy.fw$coords[8:11,,61]
xy.fw$coords[11,,61] <- NA

plot(xy.fw$coords[,,61], type = "n")
text(xy.fw$coords[,1,61],xy.fw$coords[,2,61], 1:xy.fw$landmark.number)

xy.fw <- add.provenance(
  xy.fw,
  name="error.correction",
  title = "Corrected error in the placement of landmarks 7-11",
  text = "The digitizer placed landmark 7 twice and shifted the position of landmarks 7-10. No data exist for landmark 11, which was set to NA." )

names(xy.fw$provenance)
cat(xy.fw$provenance$error.correction)

?estimate.missing.landmarks

xy.fw <- estimate.missing.landmarks(xy.fw)

landmark.plot(xy.fw, links = fw.links, specimen.number = c(61,62))


# Procrustes alignment, again, again

gpa.fw <- align.procrustes(xy.fw, outlier.analysis = TRUE)

names(gpa.fw$provenance)

cat(gpa.fw$provenance$estimate.missing.landmarks)



# Procrustes alignment for the hindwings

gpa.hw <- align.procrustes(xy.hw, outlier.analysis = TRUE)

landmark.plot(xy.hw, links = hw.links, specimen.number = c(45,46))

tmp <- xy.hw$coords[2,,45]
xy.hw$coords[2,,45] <- xy.hw$coords[3,,45]
xy.hw$coords[3,,45] <- tmp

landmark.plot(xy.hw, links = hw.links, specimen.number = c(45,46))

xy.hw <- add.provenance(
  xy.hw,
  name="error.correction",
  title = "Corrected error in the placement of landmarks 2&3",
  text = "The digitizer swapped landmarks 2 and 3. This error has been corrected" )

gpa.hw <- align.procrustes(xy.hw)


# Examine the data structure

names(gpa.fw)

names(gpa.fw$gdf)
head(gpa.fw$gdf$specimen.id)
head(gpa.fw$gdf$Csize)

gpa.fw$censensus

landmark.plot(gpa.fw$censensus, links = fw.links)

head(gpa.fw$outliers)


# Procrustes distances

landmark.plot(xy.fw, specimen.number = 1:2, links = fw.links)
landmark.plot(gpa.fw, specimen.number = 1:2, links = fw.links)

procrustes.distance(xy.fw$coords[,,1], xy.fw$coords[,,2])
procrustes.distance(gpa.fw$gdf$coords[,,1], gpa.fw$gdf$coords[,,2])
procrustes.distance(gpa.fw$gdf$coords[,,1]*gpa.fw$gdf$Csize[1], gpa.fw$gdf$coords[,,2]*gpa.fw$gdf$Csize[2])

gpa.fw$specimen.number

# For 112 specimens there are 6216 unique pairwise contrasts

pairwise.procD.xy <- vector(mode = "numeric")
pairwise.procD.gpa <- vector(mode = "numeric")
pairwise.procD.specimen <- vector(mode = "character")
pairwise.procD.digitizer <- vector(mode = "character")

for (i in 1:(gpa.fw$specimen.number-1)) {
  for (j in (i+1):gpa.fw$specimen.number) {
    # cat(i,"\t",j,"\n")
    x <- procrustes.distance(xy.fw$coords[,,i], xy.fw$coords[,,j])
    pairwise.procD.xy <- c(pairwise.procD.xy, x)
    x <- procrustes.distance(gpa.fw$gdf$coords[,,i]*gpa.fw$gdf$Csize[i], gpa.fw$gdf$coords[,,j]*gpa.fw$gdf$Csize[j])
    pairwise.procD.gpa <- c(pairwise.procD.gpa, x)
    pairwise.procD.specimen <- c(pairwise.procD.specimen, str_split_fixed(xy.fw$metadata$specimen.id[i],"__",2)[1,1])
    pairwise.procD.digitizer <- c(pairwise.procD.digitizer, str_split_fixed(xy.fw$metadata$specimen.id[i],"__",2)[1,2])
  }
}

df <- data.frame(
  specimen = pairwise.procD.specimen,
  digitizer = pairwise.procD.digitizer,
  procD.xy = pairwise.procD.xy,
  procD.gpa = pairwise.procD.gpa
)

df %>% 
  pivot_longer(cols = 3:4, names_to = "stage") %>% 
  ggplot(aes(x=value, fill = specimen)) +
  theme_bw() +
  facet_grid(stage ~ ., scales = "free_y") +
  geom_histogram()

df %>% 
  pivot_longer(cols = 3:4, names_to = "stage") %>% 
  ggplot(aes(x=value, fill = digitizer)) +
  theme_bw() +
  facet_grid(stage ~ ., scales = "free_y") +
  geom_histogram()

library(viridis)

df %>% 
  pivot_longer(cols = 3:4, names_to = "stage") %>% 
  ggplot(aes(x=value, fill = digitizer)) +
  theme_bw() +
  facet_grid(stage ~ ., scales = "free_y") +
  geom_histogram() +
  scale_fill_viridis(discrete = TRUE)


