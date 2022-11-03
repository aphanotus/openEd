# Exercise 2. Distributions & Tests - Solutions

###### tags: `exercise solutions`

Problems as listed in [Exercise 2](https://hackmd.io/@ColbyBI377/exercise2_Htests) appear below indented. 

As a general part of this exercise, your answers should demonstrate some use of markdown formatting, such as headings, code fences, embedded images and different text formats. 

### Problem 1a

> Create histograms showing the distribution of flipper lengths for each species and sex in the `palmerpenguins::penguins` dataset. Include a few short sentences offering a biological interpretation of these data.

```R
penguins %>% 
  filter(!is.na(sex)) %>% 
  ggplot(aes(x=flipper_length_mm)) +
  theme_bw() +
  facet_grid( sex ~ species ) +
  geom_histogram()
```

![](https://i.imgur.com/BfLnYV9.png)

### Problem 1b

> Test for differences in the flipper length among these species. Briefly explain why you chose to use the test you did. Interpret the results of the results of the test in biological terms and report them in your text as we described in class.

In terms of explaining why you chose your test, you should talk about the assumptions of ANOVA. These data are not a good fit for ANOVA, and ideally you would show code testing some of all of those assumptions. 

```R
# sample sizes
penguins %>% 
  filter(!is.na(flipper_length_mm)) %>% 
  group_by(species) %>% 
  summarise(
    mean_body_mass_g = mean(body_mass_g),
    sample.size = n()
  )
```

```
  species   mean_body_mass_g sample.size
  <fct>                <dbl>       <int>
1 Adelie               3701.         151
2 Chinstrap            3733.          68
3 Gentoo               5076.         123
```

With unbalanced sample sizes, you should use a non-parametric test. But I'll walk through all the other assumption tests anyway.

```R
# Equal variances?

library(car)
leveneTest(flipper_length_mm ~ species, data = penguins)
```

```
Levene's Test for Homogeneity of Variance (center = median)
       Df F value Pr(>F)
group   2  0.3306 0.7188
      339   
```

The null hypothesis, that each species has equal variance in flipper length, is not rejected. So based on this criterion, ANOVA would be appropriate.

```R
# Normal distribution of the data?
# Here's a "tidy" trick to run the Shapiro-Wilk test

penguins %>% 
  group_by(species) %>% 
  summarise(
    SW.test.p = shapiro.test(flipper_length_mm)$p.value,
    normality = shapiro.test(flipper_length_mm)$p.value > 0.05
  )
```

```
  species   SW.test.p normality
  <fct>         <dbl> <lgl>    
1 Adelie      0.720   TRUE     
2 Chinstrap   0.811   TRUE     
3 Gentoo      0.00162 FALSE 
```

The Gentoos have flipper lengths that are not normally distributed. 

What if the data are log-transformed?

```R
penguins %>% 
  group_by(species) %>% 
  summarise(
    SW.test.p = shapiro.test(log10(flipper_length_mm))$p.value,
    normality = shapiro.test(log10(flipper_length_mm))$p.value > 0.05
  )
```
Same results.

```R
# Outliers?
are.there.outliers <- function(x) {
  any(
    x < quantile(x, 0.25) - 1.5*IQR(x) |
      x > quantile(x, 0.75) + 1.5*IQR(x)
  )
}

penguins %>% 
  filter(!is.na(flipper_length_mm)) %>% 
  group_by(species) %>% 
  summarise(
    outliers = are.there.outliers(flipper_length_mm)
  )
```

```
  species   outliers
  <fct>     <lgl>   
1 Adelie    TRUE    
2 Chinstrap FALSE   
3 Gentoo    FALSE  
```

Yes, the Adelies have some outliers.

Because of the violations of ANOVA assumptions, it will be better to use the Kruskal-Wallis rank sum test.

```R
kruskal.test(flipper_length_mm ~ species, data = penguins)
```

```
Kruskal-Wallis rank sum test

data:  flipper_length_mm by species
Kruskal-Wallis chi-squared = 244.89, df = 2, p-value < 2.2e-16
```

```R
library(dunn.test)
with(penguins,
  dunn.test(flipper_length_mm, species, method = "bh")
)
```

```
                  Comparison of flipper_length_mm by species                   
                             (Benjamini-Hochberg)                              
Col Mean-|
Row Mean |     Adelie   Chinstra
---------+----------------------
Chinstra |  -3.629336
         |    0.0001*
         |
  Gentoo |  -15.47661  -8.931938
         |    0.0000*    0.0000*

alpha = 0.05
Reject Ho if p <= alpha/2
```

To finish this question, you should describe the result as you would in the text of a paper, with parenthetical citation of the test results.

*Flipper lengths differed by species (Kruskal-Wallis rank sum test, chi^2^~2~ = 244.9, p < 2.2x10^-16^). Each species was significantly different from the others (Dunn's test with FDR correction, p < 0.0001).*

#### Challenge 1a

>  Write a function to find the coefficient of variation for a numeric vector. (That is, the standard deviation divided by the mean.) Use your function to find the coefficient of variation for flipper length in each species. (You do not need to distinguish males and females.) This can be done using the "tidy" tools, like `summarize()`. Output a table of the coefficients for each trait and species combination.

```R
coeffvar <- function (x) {
  x <- x[!is.na(x)]
  return(sd(x)/mean(x))
}

penguins %>% 
  filter(!is.na(flipper_length_mm)) %>% 
  group_by(species) %>% 
  summarize(
    mean_flipper_length_mm = mean(flipper_length_mm),
    CV_flipper_length_mm = coeffvar(flipper_length_mm),
  )
```

```
  species   mean_flipper_length_mm CV_flipper_length_mm
  <fct>                      <dbl>                <dbl>
1 Adelie                      190.               0.0344
2 Chinstrap                   196.               0.0364
3 Gentoo                      217.               0.0299
```

#### Challenge 1b

> Apply the function to all the numerical traits in `penguins`. Represent the coefficients from the table produced in Challenge 1a or 1b as a barplot. Use `ggplot`. Tip: use the `pivot_longer()` function. Write a short biological interpretation of these results.

The key here is the correct use of `pivot_longer`, which can be a difficult tool to use, but it is a huge time saver when applied correctly. My hope is that by working this problem out on your own, you will learn to use it better than if I simply demonstrated it!

Here's code that will produce an acceptable outcome.

```R
penguins %>% 
  group_by(species) %>% 
  summarize(
    CV_bill_length_mm = coeffvar(bill_length_mm),
    CV_bill_depth_mm = coeffvar(bill_depth_mm),
    CV_flipper_length_mm = coeffvar(flipper_length_mm),
    CV_body_mass_g = coeffvar(body_mass_g)
  ) %>% 
  pivot_longer(cols = -1) %>% 
  ggplot(aes(x = name, y = value, fill = species)) +
  geom_bar(stat = "identity", position = "dodge") 
```

![](https://i.imgur.com/K4BizR2.png)

Below is some slightly more complex code that applies a theme and changes the bar colors. It swaps the x and y axes so that the names of the traits appear on the side and don't run together. Finally, the names are adjusted (using `mutate` and `sub`) to make them more human-readable.

```R
penguins %>% 
  group_by(species) %>% 
  summarize(
    CV_bill_length_mm = coeffvar(bill_length_mm),
    CV_bill_depth_mm = coeffvar(bill_depth_mm),
    CV_flipper_length_mm = coeffvar(flipper_length_mm),
    CV_body_mass_g = coeffvar(body_mass_g)
  ) %>% 
  pivot_longer(cols = -1) %>% 
  mutate(name = gsub("_"," ",name)) %>% 
  mutate(name = sub(" mm$"," (mm)",name)) %>% 
  mutate(name = sub(" g$"," (g)",name)) %>% 
  ggplot(aes(x = value, y = name, fill = species)) +
  theme_bw() +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "coefficient of variation", y = "trait") +
  scale_fill_manual(values = c("lightblue","midnightblue","deepskyblue4"))
```

![](https://i.imgur.com/2BTS5aM.png)

Here's how I would interpret these findings.

*Examining coefficients of variation for body mass, flipper length and the length and depth of the bill, variation was similar across all three species. Interestingly, bill length and depth had very similar degrees of variation. Body mass varied more than the other traits, and Adelie body mass showed the highest variation of all.*

---

## Quick Links

- [BI377 Moodle](https://moodle.colby.edu/course/view.php?id=25474)
- [BI377 HackMD](https://hackmd.io/@ColbyBI377/landingpage)
- [BI377 Rstudio VM](https://bi377.colby.edu/)

