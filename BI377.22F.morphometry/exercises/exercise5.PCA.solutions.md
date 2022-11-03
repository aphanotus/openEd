# Exercise 5. PCA - Solutions

###### tags: `exercise solutions`



Problems as listed in [Exercise 5](https://hackmd.io/@ColbyBI377/exercise5_pca) appear below indented. 

### Problem 1

>  Show the names of data provenance entries on your forewing and hindwing data objects, after GPA.

```R
names(gpa.fw$provenance)
```

```
[1] "create.tps"                 "read.tps"                   "align.reflect"             
[4] "error.correction"           "estimate.missing.landmarks" "align.procrustes"          
[7] "listed.gdf"                
```

```R
names(gpa.hw$provenance)
```

```
[1] "create.tps"       "read.tps"         "align.reflect"    "error.correction" "align.procrustes"
[6] "listed.gdf"      
```

#### Challenge 1

> Save these data objects and their link matrices together in one file in your working directory. Then load them back into R's environment and make it verbose. Show your code. (Hint: Google how to do this "in R".)

Saving objects in R can be a good way to save your progress on a large project or when the generation of an object take a long time. The `save` function can taker any number of objects of any type. It should be saved as a file with the extension `.rda` or `.Rdata`.

```R
save(gpa.fw, fw.links, gpa.hw, hw.links,
     file = "bi377.demo.borealisv.v.fervidus.gpa.rda")

load("bi377.demo.borealisv.v.fervidus.gpa.rda", verbose = TRUE)
```

```
Loading objects:
  gpa.fw
  fw.links
  gpa.hw
  hw.links
```

When loading R data files with `load`, it's not immediately obvious what data objects have been loaded into the local environment. By setting the argument `verbose = TRUE`,  you will see a list of the objects in the order they were named in the original `save` call.

### Problem 2

> Perform PCA on the two GPA-aligned wing datasets. Examine the distribution of variance associated with each principal component axis using the functions `pcvar` and `scree.plot`, for each dataset.

```R
pca.fw <- gm.prcomp(gpa.fw$gdf$coords)
pca.hw <- gm.prcomp(gpa.hw$gdf$coords)

pcvar(pca.fw)
```

```
     PC1      PC2      PC3      PC4      PC5      PC6      PC7      PC8      PC9 
"22.19%" "12.61%" "10.76%"  "7.97%"  "6.59%"  "5.01%"   "4.2%"  "3.44%"  "3.39%" 
    PC10     PC11     PC12     PC13     PC14     PC15     PC16     PC17     PC18 
 "2.67%"  "2.52%"  "2.32%"  "2.01%"   "1.7%"  "1.46%"  "1.37%"  "1.19%"  "1.14%" 
    PC19     PC20     PC21     PC22     PC23     PC24     PC25     PC26     PC27 
 "0.88%"  "0.84%"  "0.75%"  "0.63%"  "0.59%"   "0.5%"  "0.46%"  "0.45%"  "0.38%" 
    PC28     PC29     PC30     PC31     PC32     PC33     PC34     PC35     PC36 
 "0.37%"  "0.32%"  "0.27%"  "0.24%"   "0.2%"  "0.17%"  "0.15%"  "0.14%"  "0.12%" 
```

```R
pcvar(pca.hw)
```

```
     PC1      PC2      PC3      PC4      PC5      PC6      PC7      PC8 
"24.15%" "23.31%" "15.46%" "11.72%"  "9.81%"  "6.76%"  "5.36%"  "3.42%" 
```

```R
scree.plot(pca.fw)
```

![](https://i.imgur.com/kvDK5Ys.png)

```R
scree.plot(pca.hw)
```

![](https://i.imgur.com/ESb37kh.png)


#### Challenge 2

> What does it mean if the first principal component axis has by far the most variance? What does it mean if variance is spread across multiple axes? How does this influence your interpretation of the forewing and hindwing data?

A principal component axis compasses variance across several of the initial dimensions of the data. If one (or a few) the first PC axis accounts for most of the variance, that indicates the data are highly correlated. Change in one variable is accompanied by change in the other variables in a consistent way across the specimens. In contrast, if variance is spread across several of the first PC axes, that means that their is less consistency in the ways in which specimens vary. Thinking of shape data, a dominant PC1 would imply coordinated shape changes across the dataset. As one aspect of shape changes, other aspects change in a concerted way. If variance is spread among many PCs, instead it means that shapes vary among one another in different ways, with certain shape differences appearing in different combinations. 

### Problem 3

> If you have not already done so, download the previously-blinded [metadata](https://github.com/aphanotus/openEd/blob/main/BI377.22F.morphometry/class6.Oct25/bi377.demo.borealis.v.fervidus.metadata.csv) for the shape datasets. Import the metadata into R. There's one snag. The table lists each specimen once, while all 8 of us digitized these specimens! So, you'll need to repeat e.g. the species information 8 times. Create a vector like this, `species <- rep(blinded.metadata$species,8)`

If you excluded any specimens during GPA, you would also need to omit the same index number(s) from the `species` object you create too!

> Next, create shape space plots for each wing dataset and highlight species using convex hulls. Experiment with the variations of the plot we explored in class. For example, looking into higher dimensions than only the first two. Do you think we are correct in assigning these specimens to two different species?  Do the forewing and hindwing data concur?

For example...

```R
blinded.metadata <- read.csv("bi377.demo.borealis.v.fervidus.metadata.csv")
blinded.metadata

species <- rep(blinded.metadata$species,8)
caste <- rep(blinded.metadata$caste,8)
sp.caste <- paste(species,caste)

# Forewings
shape.space(pca.fw, group = species, convex.hulls = TRUE, 
            include.legend = TRUE, group.title = "species")

shape.space(pca.fw, group = species, convex.hulls = TRUE, 
            include.legend = TRUE, group.title = "species",
            axis1 = 1, axis2 = 3)

shape.space(pca.fw, group = caste, convex.hulls = TRUE, 
            include.legend = TRUE, group.title = "caste")

shape.space(pca.fw, group = caste, convex.hulls = TRUE, 
            include.legend = TRUE, group.title = "caste",
            axis1 = 1, axis2 = 3)

shape.space(pca.fw, group = sp.caste, convex.hulls = TRUE, 
            include.legend = TRUE, group.title = "species & caste")

# Hindwings
shape.space(pca.hw, group = species, convex.hulls = TRUE, 
            include.legend = TRUE, group.title = "species")

shape.space(pca.hw, group = species, convex.hulls = TRUE, 
            include.legend = TRUE, group.title = "species",
            axis1 = 1, axis2 = 3)

shape.space(pca.hw, group = caste, convex.hulls = TRUE, 
            include.legend = TRUE, group.title = "caste")

shape.space(pca.hw, group = caste, convex.hulls = TRUE, 
            include.legend = TRUE, group.title = "caste",
            axis1 = 1, axis2 = 3)

shape.space(pca.hw, group = sp.caste, convex.hulls = TRUE, 
            include.legend = TRUE, group.title = "species & caste")
```

It appears that the specimens overlap in shape space. In other words, their wings do not appear to be to different according to the species assignments they were given in the field.  

#### Challenge 3

> Examine whether digitizer (that is, the person who digitized each shape) distinguishes shapes in the dataset. Do this by creating another set of plots using convex hulls to highlight digitizer.

`digitizer` is one of the metadata factors that has been included with our bumble bee wing data from he beginning. 

```R
# Forewings
shape.space(pca.fw, group = gpa.fw$gdf$digitizer, convex.hulls = TRUE, 
            include.legend = TRUE, group.title = "digitizer")

shape.space(pca.fw, group = gpa.fw$gdf$digitizer, convex.hulls = TRUE, 
            include.legend = TRUE, group.title = "digitizer",
            axis1 = 1, axis2 = 3)

# Hindwings
shape.space(pca.hw, group = gpa.hw$gdf$digitizer, convex.hulls = TRUE, 
            include.legend = TRUE, group.title = "digitizer")

shape.space(pca.hw, group = gpa.hw$gdf$digitizer, convex.hulls = TRUE, 
            include.legend = TRUE, group.title = "digitizer",
            axis1 = 1, axis2 = 3)
```

While there's some variation, there's mostly overlap in the areas of shape shape occupied by each digitizer. None of us appears to have marked up specimens in a way that is wildly different. 



---

## Quick Links

- [BI377 Moodle](https://moodle.colby.edu/course/view.php?id=25474)
- [BI377 HackMD](https://hackmd.io/@ColbyBI377/landingpage)
- [BI377 Rstudio VM](https://bi377.colby.edu/)

