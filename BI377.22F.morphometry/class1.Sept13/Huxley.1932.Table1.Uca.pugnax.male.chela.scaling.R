library(tidyverse)
library(ggpubr)

filename <- "~/Documents/5.teaching/openEd/BI377.22F.morphometry/class1.Sept13/Huxley.1932.Table1.Uca.pugnax.male.chela.scaling.csv"

uca <- read.csv(filename)
uca

scaling.coef <- lm(log10(chela.mg) ~ log10(body.mg), data = uca)$coefficients[2]

uca.plot1 <- uca %>%
  ggplot(aes(body.mg, chela.mg)) +
  theme_classic() +
  geom_point(alpha = 0.85) +
  labs(x = "body mass (mg)", y = "claw mass (mg)") +
  xlim(c(0,2250)) + ylim(c(0,2250)) +
  coord_fixed()

uca.plot2 <- uca %>%
  ggplot(aes(log10(body.mg), log10(chela.mg))) +
  theme_classic() +
  geom_point(alpha = 0.85) +
  labs(x = "body mass (log10 mg)", y = "claw mass (log10 mg)") +
  geom_abline(intercept = 0, slope = 1, color = "gray65") +
  geom_smooth(method = lm, formula = (y ~ x), se = FALSE, color = "darkred", alpha = 0.85) +
  xlim(c(1,3.5)) + ylim(c(1,3.5)) +
  coord_fixed()

uca.plots <- ggarrange(uca.plot1, uca.plot2, ncol = 2)
uca.plots

ggsave("~/Documents/5.teaching/openEd/BI377.22F.morphometry/class1.Sept13/uca.plots.png",
       uca.plots, width = 6, height = 3, scale = 1)
