# Test cross problem: Solution using a discrete model simulation
# Dave Angelini, Colby College, BI279, Spring 2021

# The genotypes of F1 individuals who will be mating to produce the F2.
# There are many F1's but they have an equal ratio of these two genotypes.
genotypes <- c("Ww","ww")

# A function to convert a string with multiple characters (a genotype)
# into a vector of single characters (alleles)
vstr <- function (x) { substring(x, seq(1, nchar(x), 1), seq(1, nchar(x), 1)) }

# A function to model allelic segregation in a cross of two parents
cross <- function (female, male, offspring.number = 1, order.hets = TRUE) {
  # Split genotypes into alleles
  female <- vstr(female)
  male <- vstr(male)
  # If parents are both homozygous with the same alleles then, just output that.
  if (all(c(female,male) == female[1])) {
    F1 <- rep(paste0(female, collapse = ""),offspring.number)
  } else { # Otherwise, sample alleles from each parent
    F1 <- unlist(
      lapply(1:offspring.number, function(x) {
        paste0(sample(female,1),sample(male,1))
      } ) )
    if (order.hets) { # Ensure hets are listed with dominant (capital) alleles first
      F1 <- unlist(
        lapply(F1, function(x) {
          x <- vstr(x)
          x <- x[order(x, decreasing = TRUE)]
          x <- paste0(x, collapse = "")
        } ) )
    }
  } # End the else-statement
  return(F1)
} # End of cross function

# Set the random seed to allow reproducibility
set.seed(123)

# Simulate many matings
matings <- 1000
offspring.per.cross <- 1000
F2 <- unlist(
  lapply(1:matings, function(x) {
    cross(female = sample(genotypes,1),
          male = sample(genotypes,1),
          offspring.number = offspring.per.cross)
  })
)

length(F2)
unique(F2)

# Count the number of each genotype
F2.genotypes <- c(by(as.factor(F2), F2, length))

# Plot the number of each genotype
barplot(F2.genotypes)

# Count the number of each phenotype
F2.phenotypes <- F2.genotypes[1]
F2.phenotypes[2] <- F2.genotypes["Ww"] + F2.genotypes["WW"]
names(F2.phenotypes) <- c("wrinkled (ww)","round (W_)")
F2.phenotypes

# Find the fraction (over 16) of each phenotype
F2.ratio <- paste0(round((F2.phenotypes / sum(F2.phenotypes))*16,2)," / 16")

# Plot the number of each phenotype
p <- barplot(F2.phenotypes, cex.names = 2)

# Overlay the fraction of each phenotype
text(p, F2.phenotypes*(2/3), labels=F2.ratio, cex=2, adj=0.5)

# Add some of the simulation details to the bottom of the plot
title(sub = paste0(
  "Based on ",matings," simulated matings of ", paste0(genotypes, collapse = ", "),
  " each producing ",offspring.per.cross," offspring."), adj = 1)

# Save the plot
png("~/Downloads/test.cross.problem.png", width = 600, height = 750)

# Chi-squared test
chisq.test(F2.phenotypes, p = c(9,7)/16)
# X-squared = 35.052, df = 1, p-value = 3.211e-09


