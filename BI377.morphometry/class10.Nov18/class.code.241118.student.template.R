# BI377 Fall 2024
# In-Class Code
# Week 10

# Student Template

########################
# Jointed alignment
########################

# load libraries
library(dplyr)
library(magrittr)
library(ggplot2)
library(borealis)

data("mantis", package = "borealis")

# Explore the dataset



# Define a matrix to outline the anatomy
{
  x <- 1:16
  mantis.lines <- matrix(c(x[-length(x)],x[-1]), ncol = 2)
  mantis.lines[10,] <- c(10,1)
  mantis.lines[15,] <- c(15,6)
  mantis.lines <- rbind(mantis.lines,
                        matrix(c(5,11, 6,11, 13,16, 14,16), ncol = 2, byrow = TRUE))
}

# Plot four specimens with the outlines


?align.angle

# angle.pts.1, art.pt (the vertex), and angle.pts.2 define the angle to enforce
# rot.pts defines the landmarks to actually rotate
mantis2 <- align.angle(
  mantis,
  art.pt = 11,
  angle.pts.1 = 1:10,
  angle.pts.2 = 12:15,
  rot.pts = 12:16
)

# Plot the same specimens again, after angle alignment



# By default the average of all specimens defines the reference angle
# However reference.specimen can specify one or more specimens to use.
mantis3 <- align.angle(
  mantis,
  art.pt = 11,
  angle.pts.1 = 1:10,
  angle.pts.2 = 12:15,
  rot.pts = 12:16,
  reference.specimen = c(1,7)
)

# Plot the same specimens again, after angle alignment



# Below is code to examine all pairwise Procrustes distances
# between specimens, before and after GPA.
{
  {
    # Define vectors to hold the Procrustes distances ("procD")
    pw.procD.mantis <- vector(mode = "numeric")
    pw.procD.mantis2 <- vector(mode = "numeric")
    pw.procD.mantis3 <- vector(mode = "numeric")

    # Nested loops to scan through all pairwise combinations of specimens
    # for the three different alignment versions of the dataset
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

  # Repeat the process after generalized Procrustes alignment
  {
    pw.procD.mantis.gpa <- vector(mode = "numeric")
    pw.procD.mantis2.gpa <- vector(mode = "numeric")
    pw.procD.mantis3.gpa <- vector(mode = "numeric")

    # Run GPA, silently
    mantis.gpa <- align.procrustes(mantis, print.progress = FALSE)
    mantis2.gpa <- align.procrustes(mantis2, print.progress = FALSE)
    mantis3.gpa <- align.procrustes(mantis3, print.progress = FALSE)

    for (i in 1:(mantis$specimen.number-1)) {
      for (j in (i+1):mantis$specimen.number) {
        x <- procrustes.distance(mantis.gpa$gdf$coords[,,i], mantis.gpa$gdf$coords[,,j])
        pw.procD.mantis.gpa <- c(pw.procD.mantis.gpa, x)
        x <- procrustes.distance(mantis2.gpa$gdf$coords[,,i], mantis2.gpa$gdf$coords[,,j])
        pw.procD.mantis2.gpa <- c(pw.procD.mantis2.gpa, x)
        x <- procrustes.distance(mantis3.gpa$gdf$coords[,,i], mantis3.gpa$gdf$coords[,,j])
        pw.procD.mantis3.gpa <- c(pw.procD.mantis3.gpa, x)
      }
    }
  }

  # Organize the vectors into a data frame
  df1 <- data.frame(
    mantis = pw.procD.mantis,
    mantis2 = pw.procD.mantis2,
    mantis3 = pw.procD.mantis3
  ) %>%
    pivot_longer(cols = 1:3, names_to = "stage")

  # Another dataframe for the GPA-aligned distances
  df2 <- data.frame(
    mantis = pw.procD.mantis.gpa,
    mantis2 = pw.procD.mantis2.gpa,
    mantis3 = pw.procD.mantis3.gpa
  ) %>%
    pivot_longer(cols = 1:3, names_to = "stage")

  # Combine the two data frames into one
  df <- cbind(df1, df2[,-1])
  colnames(df)[2:3] <- c(" before GPA","after GPA")

  # Pivot longer
  df <- df %>%
    pivot_longer(cols = 2:3, names_to = "GPA")

  # Find the median Procrustes distance for each group
  procD.medians <- df  %>%
    group_by(stage, GPA) %>%
    summarise(median = median(value))

  # Create a faceted plot
  plot.xy.mantis <- df %>%
    ggplot(aes(x=value)) +
    theme_bw() +
    facet_grid(stage ~ GPA, scales = "free") +
    geom_histogram() +
    geom_vline(
      data = procD.medians, aes(xintercept = median),
      color = "darkred", size = 1) +
    xlab("pairwise Procrustes distance")
}

# Show the plot
plot.xy.mantis

# Which landmarks are most variable?
?procrustes.jackknife
