library(borealis)


load("Bombus.wing.update.rda", verbose = TRUE)



landmark.plot(Bombus.forewings, links = links.forewing)



Bombus.forewings$specimen.number


# Look at the metadata
sort(with(Bombus.forewings$metadata, by(species, species, length)))



sort(with(Bombus.forewings$metadata, by(caste, caste, length)))


# Sub-setting
which(Bombus.forewings$metadata$caste == "W")



i <- which(Bombus.forewings$metadata$caste == "W")

worker.fw <- subsetgmm(Bombus.forewings, specimens = i)

# GPA
gpa.wfw <- align.procrustes(worker.fw, outlier.analysis = TRUE)




# Modularity testing
landmark.plot(gpa.wfw, links = links.forewing)




modularity.hypothesis1 <- rep("distal", gpa.wfw$landmark.number)

modularity.hypothesis1[13:17] <- "proximal"

modularity.hypothesis1

?modularity.test

mt1 <- modularity.test(gpa.wfw$gdf$coords, modularity.hypothesis1, CI=TRUE, iter=99 )
mt1




modularity.hypothesis2 <- rep("proximal", gpa.wfw$landmark.number)
modularity.hypothesis2[c(1:9,17)] <- "distal"

mt2 <- modularity.test(gpa.wfw$gdf$coords, modularity.hypothesis2, CI=TRUE, iter=99 )

mt2



# PCA
pca.wfw <- gm.prcomp(gpa.wfw$gdf$coords)

scree.plot(pca.wfw)

shape.space(x = pca.wfw,
            group = gpa.wfw$gdf$species, 
            include.legend = TRUE, group.title = 'species',
            convex.hulls = TRUE)



shape.space(x = pca.wfw,
            group = gpa.wfw$gdf$species, 
            include.legend = TRUE, group.title = 'species',
            convex.hulls = TRUE,
            backtransform.examples = TRUE,
            ref.shape = gpa.wfw$consensus,
            shape.method = "TPS",
            bt.shape.mag = 3)


# Disparity tests

?morphol.disparity

# Overall disparity
morphol.disparity(coords ~ 1, groups = NULL, data = gpa.wfw$gdf)

# Morphological disparity among and between groups
morphol.disparity(coords ~ 1, groups = ~ species, data = gpa.wfw$gdf)


# Overall disparity, accounting for the influence of size
morphol.disparity(coords ~ log(Csize), groups = NULL, data = gpa.wfw$gdf)

# Morphological disparity among and between groups, accounting for the influence of size
morphol.disparity(coords ~ log(Csize), groups = ~ species, data = gpa.wfw$gdf)




# Modeling
m.size <- procD.lm(coords ~ log(Csize), data = gpa.wfw$gdf)
anova(m.size)



m.size.sp <- procD.lm(coords ~ log(Csize) + species, data = gpa.wfw$gdf)
anova(m.size.sp)



pw.m.size.sp <- pairwise(
  fit = m.size.sp,
  fit.null = m.size,
  groups = gpa.wfw$gdf$species
)

summary(pw.m.size.sp)



# Phylogenetic context

Bombus.tree

plot(Bombus.tree)



# Organize shapes by species

?coords.subset

fw.coords.by.species <- coords.subset(
  gpa.wfw$gdf$coords, 
  group = as.character(gpa.wfw$gdf$species)
)

typeof(fw.coords.by.species)

names(fw.coords.by.species)


dim(fw.coords.by.species$bimaculatus)

# Get mean worker shapes by species

?mshape

fw.mshape.by.species <- lapply(fw.coords.by.species, mshape)

typeof(fw.mshape.by.species)

names(fw.mshape.by.species)


dim(fw.mshape.by.species$bimaculatus)

fw.mshape.by.species$bimaculatus

landmark.plot(fw.mshape.by.species$bimaculatus, links = links.forewing)

# Convert the mean shapes into a "plain" 3-D array data type

species.names <- names(fw.mshape.by.species)

# "Data Munging"
fw.mshape.by.species <- array(
  unlist(fw.mshape.by.species),
  c(gpa.wfw$landmark.number, 2, length(species.names))
)

dimnames(fw.mshape.by.species) <- list(NULL,NULL,species.names)


typeof(fw.mshape.by.species)

dim(fw.mshape.by.species)

dimnames(fw.mshape.by.species)

fw.mshape.by.species[,,3]

landmark.plot(fw.mshape.by.species[,,3], links = links.forewing)

plot(Bombus.tree)

# Prune the tree
# Make a copy to modify
btree <- Bombus.tree
btree$tip.label <- str_split_fixed(btree$tip.label," ",2)[,2] 

btree$tip.label

plot(btree)

# Quality control
species.names %in% btree$tip.label


library(phytools)

btree <- keep.tip(btree, species.names)

plot(btree)


# PCA with the phylogeny
PCA.w.phylo <- gm.prcomp(fw.mshape.by.species, phy = btree)


plot(PCA.w.phylo, phylo = TRUE, main = "PCA with phylogeny")



PCA <- gm.prcomp(fw.mshape.by.species)


plot(PCA, main = "PCA with phylogeny")



# Phylogenetically-aligned PCA
# “...provides an ordination that aligns phenotypic data with 
# phylogenetic signal, by maximizing variation in directions that 
# describe phylogenetic signal, while simultaneously preserving the 
# Euclidean distances among observations in the data space.”

PaCA <- gm.prcomp(fw.mshape.by.species, phy = btree, align.to.phy = TRUE)


plot(PaCA, phylo = TRUE, main = "Phylogenetically-aligned PCA")


# PGLS

species.gdf <- geomorph.data.frame(
  coords = fw.mshape.by.species,
  Csize = c(by(gpa.wfw$gdf$Csize, gpa.wfw$gdf$species, mean)),
  tree = btree
)

i <- 999
# Does shape vary by size, after correcting for the influence of relatedness? 
pgls.size <- procD.pgls(coords ~ log(Csize), phy = tree, data = species.gdf, iter = i)

anova(pgls.size)


# No pairwise implementation yet!






?bilat.symmetry




