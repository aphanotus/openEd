# Exercise 4. Procrustes distance and alignment

###### tags: `exercise`

## Instructions

- Before attempting this exercise, please walk through the [code from class on October 18](https://github.com/aphanotus/openEd/blob/main/BI377.22F.morphometry/class2.Sept20/class.code.221018.R).
- Below *Problems* are meant to be fairly easy. If you have trouble with these, contact me immediately. *Challenges* will require you to find resources or answers on your own. (Hint: Try googling "how to ... in R".)
- Create a HackMD note to share your answers to these prompts. Copy the prompts, add your code and explanatory text and images. Use markdown formatting to distinguish the prompts from the code and explanatory text of your answers.
- Please complete this assignment individually. Discussing the problems with others is acceptable. Key element of code may be borrowed from others, but please credit the source (be it a website or a person). The accompanying text should be your own. 
- We''ll let submissions a bit differently wit this exercise. Please download the markdown text from HackMD, and submit the markdown file with your answers on Moodle, before Monday, 10pm.

---

The `borealis` package comes with several build-in datasets. The object `Bombus.forewings` contains a  dataset of bumblebee forewing shapes digitized using the same landmarks you recently used. The same matrix of "links" will work to highlight the anatomy of the wing. Many functions, including `align.procrustes` will also accept the `links` argument.

To complete Problem 3 and Challenge 3 you will need to update the `borealis ` package. If you are working on the BI377 Rstudio server, run the following.

```R
detach("package:borealis", unload = TRUE)
library(borealis)
```

If you are working on your own machine, you will also need to download the package's source files.

```R
detach("package:borealis", unload = TRUE)
if (require(devtools)) { devtools::install_github("aphanotus/borealis") }
library(borealis)
```



### Problem 1

How many specimens are present in the `Bombus.forewings` dataset? What species are included? 

#### Challenge 1

And how many specimens represent each species? (Hint: You could simply count each, but that would be tedious. The goal here is a computational solution. Try looking into the use of the R function `by`. )

### Problem 2

Perform generalized Procrustes alignment of the `Bombus.forewings` dataset, using the methods and tools we discussed. Be aware that some data curation may be necessary. Use your discretion and check whether it is necessary to omit or reflect specimens, or to correct landmark errors made during digitization. Be sure to document any manual corrections in the data provenance. 

In your response to these questions, include an image of the aligned landmarks (the plot with gray and black dots generated by the `align.procrustes` function) and the data provenance (in markdown format) for the GPA step and any steps of data curation. 

### Problem 3

Use the function `procrustes.distance` to compare the shapes of the first 3 specimens in the `Bombus.forewings` dataset, after Procrustes alignment. Which two specimens are most similar?

#### Challenge 3

We discussed some of the considerations in choosing landmarks. The function `procrustes.jackknife` will test how variation in the location of each landmark affects the outcome of Procrustes alignment. It does this by one-by-one removing landmarks from the dataset and repeating GPA. After each iteration, the median pairwise Procrustes distance among all specimens is determined. The results are output as a table and a plot. (You can include an argument to `links` in this function too.)

Run this function on your curated coordinate data from `Bombus.forewings` (not on the GPA-aligned data). Include the resulting image in your answer. Which landmarks are more variable? What explanations exist for these results?

---

## Quick Links

- [BI377 Moodle](https://moodle.colby.edu/course/view.php?id=25474)
- [BI377 HackMD](https://hackmd.io/@ColbyBI377/landingpage)
- [BI377 Rstudio VM](https://bi377.colby.edu/)

