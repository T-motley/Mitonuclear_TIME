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
  family = nbinom2,
  data = data_q3
)

summary(q3_nb)

#Plot
summary_data <- data_q3 %>%
  group_by(genotype, stage) %>%
  summarise(
    mean_prod = mean(production),
    se = sd(production) / sqrt(n()),
    .groups = "drop"
  )

q3_p <- ggplot(summary_data, aes(stage, mean_prod, colour = genotype, group = genotype)) +
  stat_summary(fun = mean, geom = "point", size = 3) +
  stat_summary(fun = mean, geom = "line", linewidth = 1) +
  
  scale_colour_manual(values = c(
    "mX" = "blue",
    "tX" = "red"
  )) +
  geom_errorbar(aes(ymin = mean_prod - se, ymax = mean_prod + se), width = 0.1) +
  scale_x_discrete(labels = c(
    "f2" = "Before admixture\nadaptation",
    "fx" = "After admixture\nadaptation"
  )) +
  
  labs(
    x = "",
    y = " Mean Offspring Productivity",
    colour = "Genotype",
    title = "Comparison of adaptation effects on Mitonuclear combination"
  ) +
  
  theme_classic()