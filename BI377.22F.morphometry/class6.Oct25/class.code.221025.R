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




