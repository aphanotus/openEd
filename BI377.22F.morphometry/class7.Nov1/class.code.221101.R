library(geomorph)
library(borealis)

# List data available in a package
data(package="geomorph")

# GPA
pup.gpa <- align.procrustes(pupfish$coords)

# Add metadata into the GPA object in a way that's
# consistent with borealis
pup.gpa$gdf$sex <- pupfish$Sex
pup.gpa$gdf$pop <- pupfish$Pop
pup.gpa$gdf$Csize <- pupfish$CS

# Shape comparisons by Procrustes ANOVA with permutation
i <- 9999 # number of iterations
size.model <- procD.lm(coords ~ log(Csize), data=pup.gpa$gdf, iter=i)
size.model

# Linear Model fit with lm.rrpp
#
# Number of observations: 54
# Number of dependent variables: 112
# Data space dimensions: 53
# Sums of Squares and Cross-products: Type I
# Number of permutations: 10000
# Call: procD.lm(f1 = coords ~ log(Csize), iter = i, data = pup.gdf)

# Model testing
anova(size.model)

# Analysis of Variance, using Residual Randomization
# Permutation procedure: Randomization of null model residuals
# Number of permutations: 10000
# Estimation method: Ordinary Least Squares
# Sums of Squares and Cross-products: Type I
# Effect sizes (Z) based on F distributions
#
#            Df       SS        MS     Rsq      F      Z Pr(>F)
# log(Csize)  1 0.014001 0.0140012 0.24886 17.228 4.2655  1e-04 ***
# Residuals  52 0.042261 0.0008127 0.75114
# Total      53 0.056262
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Call: procD.lm(f1 = coords ~ log(Csize), iter = i, data = pup.gpa$gdf)

# - Rsq (R2) is the proportion of overall varianace attributed to that factor
# - Z is the “effect size”
# - Pr(>F) is effectively the p-value

# More complex models
sex.model <- procD.lm(coords ~ log(Csize) + sex, data=pup.gpa$gdf,
                      iter=i)
anova(sex.model)

#            Df       SS        MS     Rsq      F      Z Pr(>F)
# log(Csize)  1 0.014001 0.0140012 0.24886 20.605 4.4879  1e-04 ***
# sex         1 0.007607 0.0076068 0.13520 11.195 4.2191  1e-04 ***
# Residuals  51 0.034654 0.0006795 0.61594
# Total      53 0.056262

sex.pop.model <- procD.lm(coords ~ log(Csize) + sex + pop,
                          data=pup.gpa$gdf, iter=i)
anova(sex.pop.model)

#            Df       SS        MS     Rsq      F      Z Pr(>F)
# log(Csize)  1 0.014001 0.0140012 0.24886 25.926 4.7684  1e-04 ***
# sex         1 0.007607 0.0076068 0.13520 14.086 4.5703  1e-04 ***
# pop         1 0.007652 0.0076520 0.13601 14.169 4.7492  1e-04 ***
# Residuals  50 0.027002 0.0005400 0.47993
# Total      53 0.056262

# Compare the advanced models to the null size model
anova(size.model, sex.model, sex.pop.model)

# Analysis of Variance, using Residual Randomization
# Permutation procedure: Randomization of null model residuals
# Number of permutations: 10000
# Estimation method: Ordinary Least Squares
# Effect sizes (Z) based on F distributions
#
#                                 ResDf Df      RSS        SS        MS     Rsq      F      Z     P Pr(>F)
# coords ~ log(Csize) (Null)         52  1 0.042261                     0.00000
# coords ~ log(Csize) + sex          51  1 0.034654 0.0076068 0.0076068 0.13520 11.195 4.2191 1e-04
# coords ~ log(Csize) + sex + pop    50  2 0.027002 0.0152588 0.0076294

anova(sex.model, sex.pop.model)

#                                 ResDf Df      RSS        SS        MS     Rsq      F      Z     P Pr(>F)
# coords ~ log(Csize) + sex (Null)    51  1 0.034654                   0.00000
# coords ~ log(Csize) + sex + pop     50  1 0.027002 0.007652 0.007652 0.13601 14.169 4.7492 1e-04
# Total                               53    0.056262

# Comparing shape allometries
# Do populations have unique allometries or a common allometry?
pop.unique.model <- procD.lm(coords ~ log(Csize) * pop,
                             data=pup.gpa$gdf, iter=i)

plotAllometry(fit = pop.unique.model, size = pup.gpa$gdf$Csize,
              col = pup.gpa$gdf$pop, xlab = "log10 centroid size")

# Define a null model, without the interaction
pop.model <- procD.lm(coords ~ log(Csize) + pop,
                          data=pup.gpa$gdf, iter=i)

anova(pop.model, pop.unique.model)
#                                  ResDf Df      RSS        SS        MS      Rsq      F     Z      P Pr(>F)
# coords ~ log(Csize) + pop (Null)    51  1 0.032025                     0.000000
# coords ~ log(Csize) * pop           50  1 0.029590 0.0024353 0.0024353 0.043284 4.1151 2.714 0.0024
# Total                               53    0.056262


# More with the bumblebee wing example data
wing.gpa <- align.procrustes(Bombus.forewings, outlier.analysis = TRUE)
# Remove the one extreme outlier

# PCA
wing.pca <- gm.prcomp(wing.gpa$gdf$coords)
shape.space(wing.pca, group = wing.gpa$gdf$species,
            group.title = 'species',
            convex.hulls = TRUE, include.legend = TRUE)
# How can we represent evolutionary relationships in space shape?
# We'll come back to that.


# Pairwise comparisons
# Recall how we did this for univariate data.
x <- which(ChickWeight$Time == max(ChickWeight$Time))
chx <- ChickWeight[x,]
boxplot(chx$weight ~ chx$Diet)

chx.model <- aov(chx$weight ~ chx$Diet)

summary(chx.model)
#             Df Sum Sq Mean Sq F value  Pr(>F)
# chx$Diet     3  57164   19055   4.655 0.00686 **
# Residuals   41 167839    4094

TukeyHSD(chx.model)

#          diff        lwr       upr     p adj
# 2-1  36.95000  -32.11064 106.01064 0.4868095
# 3-1  92.55000   23.48936 161.61064 0.0046959 **
# 4-1  60.80556  -10.57710 132.18821 0.1192661
# 3-2  55.60000  -21.01591 132.21591 0.2263918
# 4-2  23.85556  -54.85981 102.57092 0.8486781
# 4-3 -31.74444 -110.45981  46.97092 0.7036249


# Let's look at how we can make pairwise comparisons
# for models of shape data

# First, how to subset shape data

wing.gpa <- align.procrustes(Bombus.forewings, outlier.analysis = TRUE)
# Remove the one extreme outlier

c(with(wing.gpa$gdf, by(species, species, length)))
# bimac   imp  tern terri   vag
#    14    12    14     1     1

# Let's remove the species with only 1 observation

common.sp <- subsetgmm(wing.gpa, specimens = (wing.gpa$gdf$species %in% c("bimac","imp","tern")) )

common.sp.model <- procD.lm(coords ~ log(Csize) + species,
                            data=common.sp$gdf, iter=i)
anova(common.sp.model)
#            Df        SS         MS     Rsq      F      Z Pr(>F)
# log(Csize)  1 0.0021213 0.00212126 0.11109 6.2505 4.5844  1e-04 ***
# species     2 0.0047559 0.00237795 0.24907 7.0069 6.2645  1e-04 ***
# Residuals  36 0.0122175 0.00033937 0.63984
# Total      39 0.0190947

# We need to build a null nodel, one that lacks the factor we're interested in.
# In this case, that's `species`. So our null model will just include centroid size.
common.size.model <- procD.lm(coords ~ log(Csize),
                              data=common.sp$gdf, iter=i)

# The function pairwise() examines the contrasts among each level
common.sp.pw <- pairwise(fit = common.sp.model,
                         fit.null = common.size.model,
                         groups = common.sp$gdf$species)
summary(common.sp.pw)
#                     d  UCL (95%)        Z Pr > d
# bimac:imp  0.02273410 0.01087468 5.309004  1e-04
# bimac:tern 0.02303207 0.01623952 3.424081  3e-04
# imp:tern   0.02805745 0.01781987 3.992952  1e-04

# Each species has different wing shapes from the others!

# `pairwise` requires a null model, and it will guess if you don’t specify one!
# You can check it's assumptions using `reveal.model.designs`, but it is
# good practice to explicitly define ther null model.

reveal.model.designs(common.sp.model)
#                Reduced                  Full
# log(Csize)           1            log(Csize)
# species     log(Csize)  log(Csize) + species <- Null/Full inherent in pairwise

# Some closing notes:
# - Modeling allows you to account for confounding factors, before examining a factor-of-interest
# - The sequential order of factors in the model matters (Type I ANOVA)
# - Interaction terms can be examined too, as `A * B` or `A + B + A:B`
