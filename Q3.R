##Q3: Are there any differences between mitonuclear combination' adaptation to admixture
#i.e. does tX adaptation differ to mX adaptation
#Factors - Production, block, pop, mX vs tX f2 -> fx. (before - after)
library(dplyr)
data_q3 <- data %>%
  filter(genotype == "mX" | genotype =="tX")

data_q3$genotype <- factor(data_q3$genotype)
data_q3$stage <- factor(data_q3$stage)
data_q3$block <- factor(data_q3$block)
data_q3$pop <- factor(data_q3$pop)

data_q3$popID <- interaction(data_q3$genotype, data_q3$pop)
data_q3$popID <- factor(data_q3$popID)

library(glmmTMB)

data_q3$stage <- relevel(data_q3$stage, "f2")

q3_nb <- glmmTMB(
  production ~ genotype * stage + block,
  family = nbinom2(link="log"),
  data = data_q3
)

summary(q3_nb)

#Plot

ggplot(data_q3, aes(stage, production, colour = genotype, group = genotype)) +
  stat_summary(fun = mean, geom = "point", size = 3) +
  stat_summary(fun = mean, geom = "line", linewidth = 1) +
  
  scale_colour_manual(values = c(
    "mX" = "blue",
    "tX" = "red"
  )) +
  
  scale_x_discrete(labels = c(
    "f2" = "Before admixture\nadaptation",
    "fx" = "After admixture\nadaptation"
  )) +
  
  labs(
    x = "",
    y = "Productivity",
    colour = "Genotype",
    title = "Comparison of adaptation effects\non Mitonuclear combination"
  ) +
  
  theme_classic()