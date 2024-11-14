# BI377 Fall 2024
# In-Class Code
# Week 9

# SUPPLEMENTAL

# Please, restart R!
# Behind the scenes, some packages have been updated, necessitating a restart.
.rs.restartR()

########################
# Outline-based Morphometrics
########################

# https://momx.github.io/Momocs/
# devtools::install_github("MomX/Momocs")

library(Momocs)

# Familiar data sets
coo_plot(pupfish$coords[,,1])

pupfish$coords[,,1] %T>%
  coo_plot() %>% 
  efourier(3) %>% 
  efourier_i() %T>% 
  coo_draw(
    coo = ., 
    border="darkred",
    lwd = 2
  )

# Image data
# https://hackmd.io/_uploads/rJ9bg2ff1x.jpg
img_plot("~/Desktop/teacup.jpg")

img <- import_jpg("~/Desktop/teacup.jpg") %T>% 
  coo_plot()

img$teacup %T>% 
  coo_plot() %>% 
  efourier(12) %>% 
  efourier_i() %T>% 
  coo_draw(
    coo = ., 
    border="darkred",
    lwd = 2
  )


data("hearts", package = "Momocs")
?hearts
# We thank the fellows of the Ecology Department of the French Institute of 
# Pondicherry that drawn the hearts, that then have been smoothed, scaled, 
# centered, and downsampled to 80 coordinates per outline.

is(hearts)
length(hearts)
names(hearts)
hearts

# coordinate data
is(hearts[[1]])
names(hearts[[1]])
hearts[[1]]

plot(hearts[[1]]$remi3)

# not sure what this is!
is(hearts[[2]])
names(hearts[[2]])
hearts[[2]]

# Metadata! (author names)
is(hearts[[3]])
names(hearts[[3]])
hearts[[3]]

# No more
is(hearts[[4]])

plot(hearts[1])

panel(hearts)
panel(hearts, fac="aut", names="aut")

# Some cool functions in Momocs
stack(hearts)
stack(hearts, border = hearts$aut)

h.gpa <- fgProcrustes(hearts)

h.gpa %>% stack()    

h.gpa %>% 
  coo_slide(ldk = 2) %>%
  stack(border = hearts$aut)


# Elliptical Fourier Analysis

# See how EFA harmonics approach a single shape
coo_plot(hearts$coo$remi3)
coo_plot(h.gpa$coo$remi3)
coo_plot(h.gpa$coo$remi3, lwd = 5)


# Increasing harmonics
max.i <- 7
colors.i <- viridis::viridis(max.i)
for (i in 1:max.i) {
  h.gpa$coo$remi3 %>% 
    efourier(
      x = ., 
      nb.h = i
    ) %>% 
    efourier_i() %>% 
    coo_draw(
      coo = ., 
      border=colors.i[i],
      lwd = 2
    )
  if (i < max.i) { 
    cat(i,"- Press ENTER to continue.") 
    invisible(readline())
  }
}

# High parameterization (40)
coo_plot(h.gpa$coo$remi3, lwd = 5)
h.gpa$coo$remi3 %>% 
  efourier(
    x = ., 
    nb.h = 40 # the maximum!
  ) %>% 
  efourier_i() %>% 
  coo_draw(
    coo = ., 
    border="darkred",
    lwd = 2
  )

# Harmonic power
?harm_pow

h.gpa$coo$remi3 %>% 
  efourier(nb.h = 15) %>% 
  harm_pow() %>% 
  cumsum() %T>% 
  plot(
    type='o',
    xlab='Harmonic rank',
    ylab='Cumulated harmonic power'
  ) %>%
  median() %>% 
  abline(h=., col = "darkred")

abline(v=7, col = "darkred")

# EFA with 7 harmonics
h.efa <- h.gpa %>% 
  coo_slide(ldk = 2) %>%
  efourier(
    nb.h = 7,
    norm = FALSE
  )
# The `norm` argument can results in bad EFA fits for outlines
# that are circular or strongly bilaterally symmetrical.


is(h.efa)
names(h.efa)
h.efa[1]


# Plot the values of the harmonic coefficients
h.efa %>% 
  rm_harm() %>% 
  boxplot()



# Harmonic contribution to shape
hcontrib(h.efa)


# Pairwise comparison of shapes
h.efa %>% 
  MSHAPES(~aut) %>% 
  plot_MSHAPES()


# PCA simply uses the harmonic coefficients
h.pca <- h.efa %>% 
  PCA()

is(h.pca)
names(h.pca)

# Metadata
names(h.pca$fac)
h.pca$fac$aut

h.pca %>% plot_PCA()

h.pca %>% plot_PCA(
  f = ~ aut,
  chullfilled = TRUE
)

h.pca %>% 
  KMEANS(8)


# Shape variation along PC axes
PCcontrib(
  PCA = h.pca,
  nax = 1:5
)


# Linear Discriminant Analysis (LDA)
h.lda <- LDA(
  x = h.pca,
  fac = ~aut
)

is(h.lda)
names(h.lda)

# LDA plot
h.lda %>% 
  plot_LDA(
    chullfilled = TRUE
  )

# Cross-validation heatmap
plot_CV(h.lda)

# Plots a cross-correlation table
plot_CV2(h.lda)



# The workflow - All in one!
hearts %T>%                    # A toy dataset
  stack() %>%                  # Take a family picture of raw outlines
  fgProcrustes() %>%           # Full generalized Procrustes alignment
  coo_slide(ldk = 2) %T>%      # Redefine a robust 1st point between the cheeks
  stack() %>%                  # Another picture of aligned outlines
  efourier(7, norm=FALSE) %>%  # Elliptical Fourier Transforms
  PCA() %T>%                   # Principal Component Analysis
  plot_PCA(~aut) %>%           # A PC1:2 plot
  LDA(~aut) %>%                # Linear Discriminant Analysis
  plot_CV()                    # And the confusion matrix after leave one out cross validation


# Modeling

# We can test for a difference between subsets of shapes using multivariate 
# analysis of variance (MANOVA) on PC scores.

MANOVA(
  x = h.pca,
  fac = ~ aut
)

MANOVA_PW(
  x = h.pca,
  fac = ~ aut
)


# Test for shape allometry?

Csize <- coo_centsize(hearts)

plot(x = Csize, y = h.pca$x[,1])
size.model <- lm(h.pca$x[,1] ~ Csize)
abline(size.model)
summary(size.model)

# `MANOVA` cannot handle continuous predictors!
MANOVA(
  x = h.pca,
  fac = ~ Csize
)

# But `manova` can!
h.size.model <- manova(
  h.pca$x ~ Csize
)

summary(h.size.model)

# Residual plot (for only the first residual dimension!)
plot(
  x = Csize,
  y = h.size.model$residuals[,1],
  col = h.pca$aut
)

# Modularity and Disparity?
# There are no convenient functions to implement these tests with
# the outline and EFA data types handled by `Momocs`. However,
# you could, in principle, write your own code to work the math!
# (Or use data munging to force these data into `geomorph`'s tools.)







# Exercise 9 Challenge

hearts %T>% 
  panel() %>%
  fgProcrustes() %>%
  coo_slide(ldk = 2) %>%
  efourier(7, norm=FALSE) %>% 
  PCA() %>%
  LDA(~aut) %>%
  plot_CV2() 


