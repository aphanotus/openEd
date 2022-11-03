# Exercise 1. Markdown & R

###### tags: `exercise solutions`

Problems as listed in [Exercise 1](https://hackmd.io/@ColbyBI377/exercise1_R) appear below indented. 

As a general part of this exercise, your answers should demonstrate some use of markdown formatting, such as headings, code fences, embedded images and different text formats. 

### Problem 1

> Using the `ChickWeights` dataset, create boxplots showing log10 weights only on the first, tenth, and last days of the experiment.

```R
data("ChickWeight")
colnames(ChickWeight) <- c("weight","days","individual","diet")
```

There are several ways to select specific values for the  `days` column. The function `which` is useful, and it can be used one day at a time. 

```R
day0  <- which(ChickWeight$days == 0)
day10 <- which(ChickWeight$days == 10)
day21 <- which(ChickWeight$days == max(ChickWeight$days))
select.days <- c(day0, day10, day21)
```

Or you could use the boollean operator for OR, which in R is the vertical line or pipe character `|` as in this example.

```R
select.days <- which(
  ChickWeight$days == 0 |
  ChickWeight$days == 10 |
  ChickWeight$days == max(ChickWeight$days)
)
```

You could also use the `%in%` operator.

```R
select.days <- which(ChickWeight$days %in% c(0,10,21))
```

However you've chosen to identify the rows of interest, the vector of row numbers can then be used to subset the `ChickWeight` date frame into one with only the rows for days we want.

```R
select.chx <- ChickWeight[select.days,]
```

After that you can use the parred-down data frame to make a plot.

```R
boxplot(log10(select.chx$weight) ~ select.chx$days,
        names=c("day 0","day 10","day 21"),
        xlab="", ylab="log10 chick weight",
        main="problem 1")
```
![](https://i.imgur.com/8Xz2lco.png)

By the way, if you interpreted "the first, tenth, and last days of the experiment" to mean days 1, 10 and 21 -- and found there are no data for day 1, there's no penalty for that. The question should have been more clear! 

#### Challenge 1a

> Have the figure display results for each diet separately. 

Separate plots for another factor, like diet, can most easily be done by using in a `ggplot` with the `facet_wrap` feature. The initial data subsetting could be done as in the example above, but this solution uses a `%in%` logical operation. 

```R
data("ChickWeight")
colnames(ChickWeight) <- c("weight","days","individual","diet")

i <- which(ChickWeight$days %in% c(0,10,21))

plot1a <- ggplot(ChickWeight[i,], aes(x=as.factor(days), y=log10(weight))) + 
  facet_wrap(.~diet) +
  geom_boxplot() +
  ggtitle("Chick weights by diet") +
  xlab("days") +
  ylab("log10 weight") 
plot1a
```
![](https://i.imgur.com/P4lB0GO.png)


#### Challenge 1b

> With the figure features above, communicate both the individual values and the distribution of those values to a viewer. 

The key here is to overlay points using `geom_jitter` on top of violin plots, which show the distribution of values. [See the tutorial for more details.](https://hackmd.io/@aphanotus/Rtutorial#Violin-plots)

```R
data("ChickWeight")
colnames(ChickWeight) <- c("weight","days","individual","diet")

i <- which(ChickWeight$days %in% c(0,10,21))

plot1b <- ggplot(ChickWeight[i,], aes(x=as.factor(days), y=log10(weight))) + 
  geom_violin(alpha = 0.65) +  
  geom_jitter(position = position_jitter(width = 0.2, height = 0), alpha = 0.5) +
  ggtitle("Chick weights by diet") +
  xlab("days") +
  ylab("log10 weight") 
plot1b
```
![](https://i.imgur.com/4NUhFoM.png)


---

### Problem 2

> Randomly generate two vectors of 100 values each. Plot them against one another and draw a linear trend line through the group of points. 

```R
x <- rnorm(n = 100)
y <- runif(n = 100)
plot(x,y)
abline(lm(y~x))
```
![](https://i.imgur.com/VkXGHF6.png)


#### Challenge 2a

> Find the slope of the line you've drawn above.

Slope is already provided by the `lm` function we invoke as part of the `abline` call.

```R
lm(y~x)
```
```
Call:
lm(formula = y ~ x)

Coefficients:
(Intercept)            x  
    0.47651     -0.03493  
```

The coefficent for `x` in the table above is the slope of the line.

#### Challenge 2b

> Add text to the plot, showing the slope.

A Google search should turn up that `text` is the function to add text to a base R plot. This could be done very simply, like this.

```R
text(x=2, y=1, "slope = -0.03493")
```
![](https://i.imgur.com/aguSLLw.png)

Or could get fancy and store the slope value in a variable.

```R
m <- lm(y~x)
slope <- m$coefficients[2]
text(2,1, paste("slope =",slope))
```

You could also achieve the same result using `ggplot`, although the function will require the `x` and `y` values first be placed in a matrix-like data structure called a data frame.

```R
ggplot(df, aes(x = x, y = y)) +
  theme_bw() +
  geom_point(alpha = 0.85) +
  stat_smooth(method = lm, se = FALSE) +
  annotate("text", x = 2, y = 1, label = paste("slope =",slope))
```
![](https://i.imgur.com/sfOikbF.png)


---

## Quick Links

- [BI377 Moodle](https://moodle.colby.edu/course/view.php?id=25474)
- [BI377 HackMD](https://hackmd.io/@ColbyBI377/landingpage)
- [BI377 Rstudio VM](https://bi377.colby.edu/)

