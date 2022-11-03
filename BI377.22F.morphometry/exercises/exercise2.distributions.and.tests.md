# Exercise 2. Distributions & Tests

###### tags: `exercise`

## Instructions

- Before attempting this exercise, please walk through the [code from class on September 20](https://github.com/aphanotus/openEd/blob/main/BI377.22F.morphometry/class2.Sept20/class.code.220920.R) and [September 27](https://raw.githubusercontent.com/aphanotus/openEd/main/BI377.22F.morphometry/class3.Sept27/class.code.220927.R). You may also want to refer back to the [R tutorial](https://hackmd.io/@ColbyBI377/Rtutorial).
- Remember, you (and probably should) use the BI377 Rstudio virtual machine at https://bi377.colby.edu/
- Below *Problems* are meant to be fairly easy. If you have trouble with these, contact me immediately. *Challenges* will require you to find resources or answers on your own. (Hint: Try googling "how to ... in R".)
- Create a HackMD note to share your answers to these prompts. Copy the prompts, add your code and explanatory text and images. Use markdown formatting to distinguish the prompts from the code and explanatory text of your answers.
- Please complete this assignment individually. Discussing the problems with others is acceptable. Key element of code may be borrowed from others, but please credit the source (be it a website or a person). The accompanying text should be your own. 
- Submit your answers by sharing the note with Dave before Monday, 10pm.

---

### Problem 1a

Create histograms showing the distribution of flipper lengths for each species and sex in the `palmerpenguins::penguins` dataset. Include a few short sentences offering a biological interpretation of these data.

### Problem 1b

Test for differences in the flipper length among these species. Briefly explain why you chose to use the test you did. Interpret the results of the results of the test in biological terms and report them in your text as we described in class.

#### Challenge 1a

Write a function to find the coefficient of variation for a numeric vector. (That is, the standard deviation divided by the mean.) Use your function to find the coefficient of variation for flipper length in each species. (You do not need to distinguish males and females.) This can be done using the "tidy" tools, like `summarize()`. Output a table of the coefficients for each trait and species combination.

#### Challenge 1b

Apply the function to all the numerical traits in `penguins`. Represent the coefficients from the table produced in Challenge 1a or 1b as a barplot. Use `ggplot`. Tip: use the `pivot_longer()` function. Write a short biological interpretation of these results.



---

## Quick Links

- [BI377 Moodle](https://moodle.colby.edu/course/view.php?id=25474)
- [BI377 HackMD](https://hackmd.io/@ColbyBI377/landingpage)
- [BI377 Rstudio VM](https://bi377.colby.edu/)