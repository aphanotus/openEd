# Exercise 5. PCA

###### tags: `exercise`

## Instructions

- Before attempting this exercise, please walk through the [code from class on October 25](https://github.com/aphanotus/openEd/blob/main/BI377.22F.morphometry/class6.Oct25/class.code.221025.R).
- Below *Problems* are meant to be fairly easy. If you have trouble with these, contact me immediately. *Challenges* will require you to find resources or answers on your own. (Hint: Try googling "how to ... in R".)
- Create a HackMD note to share your answers to these prompts. Copy the prompts, add your code and explanatory text and images. Use markdown formatting to distinguish the prompts from the code and explanatory text of your answers.
- Please complete this assignment individually. Discussing the problems with others is acceptable. Key element of code may be borrowed from others, but please credit the source (be it a website or a person). The accompanying text should be your own. 
- Please download the markdown text from HackMD, and submit the markdown file with your answers on Moodle, before Monday, 10pm.

---

A number of small changes have been made to the `borealis` package. It is advisable to update it.

```R
detach("package:borealis", unload = TRUE)
if (require(devtools)) { devtools::install_github("aphanotus/borealis") }
library(borealis)
```

### Problem 1

Use the code from last week (October 18) to repeat the data curation and GPA on the `bi377.demo.borealisv.v.fervidus` forewing and hindwing datasets. If you already have the GPA objects for both datasets, then great!

Show the names of data provenance entries on your forewing and hindwing data objects, after GPA.

#### Challenge 1

Save these data objects and their link matrices together in one file in your working directory. Then load them back into R's environment and make it verbose. Show your code. (Hint: Google how to do this "in R".)

### Problem 2

Perform PCA on the two GPA-aligned wing datasets. Examine the distribution of variance associated with each principal component axis using the functions `pcvar` and `scree.plot`, for each dataset.

#### Challenge 2

What does it mean if the first principal component axis has by far the most variance? What does it mean if variance is spread across multiple axes? How does this influence your interpretation of the forewing and hindwing data?

### Problem 3

If you have not already done so, download the previously-blinded [metadata](https://github.com/aphanotus/openEd/blob/main/BI377.22F.morphometry/class6.Oct25/bi377.demo.borealis.v.fervidus.metadata.csv) for the shape datasets. Import the metadata into R. There's one snag. The table lists each specimen once, while all 8 of us digitized these specimens! So, you'll need to repeat e.g. the species information 8 times. Create a vector like this, `species <- rep(blinded.metadata$species,8)`

Next, create shape space plots for each wing dataset and highlight species using convex hulls. Experiment with the variations of the plot we explored in class. For example, looking into higher dimensions than only the first two. Do you think we are correct in assigning these specimens to two different species?  Do the forewing and hindwing data concur?

#### Challenge 3

Examine whether digitizer (that is, the person who digitized each shape) distinguishes shapes in the dataset. Do this by creating another set of plots using convex hulls to highlight digitizer.

---

## Quick Links

- [BI377 Moodle](https://moodle.colby.edu/course/view.php?id=25474)
- [BI377 HackMD](https://hackmd.io/@ColbyBI377/landingpage)
- [BI377 Rstudio VM](https://bi377.colby.edu/)

