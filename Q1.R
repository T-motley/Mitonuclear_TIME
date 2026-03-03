#Q1: Does admixture affect production (F2 mismatched vs fx matched)
library(dplyr)

#Create separate Dataframe

data_q1 <- data %>%
  dplyr::filter((stage == "f2" & status == "mismatched") |
            (stage == "fx" & status == "matched"))

data_q1$group <- NA_character_
data_q1$group[data_q1$stage == "fx" & data_q1$status == "matched"] <- "fx_matched"
data_q1$group[data_q1$stage == "f2" & data_q1$status == "mismatched"] <- "f2_mismatched"
data_q1$group <- factor(data_q1$group, levels = c("fx_matched","f2_mismatched"))

data_q1$pop   <- factor(data_q1$pop)
data_q1$block <- factor(data_q1$block)

#Linear Mixed Model (Gaussian)

library(lmerTest)
q1_lm <- lmerTest::lmer(data = data_q1, production ~ group + block + (1|pop))
summary(q1_lm)
anova(q1_lm)

#Checking assumptions for LMM (Gaussian)
res1 <- resid(q1_lm)
qqnorm(res1) ; qqline(res1)

#poisson model
library(glmmTMB)
q1_pois <- glmmTMB(data = data_q1, production ~ group + block + (1|pop), family= poisson())
summary(q1_pois)

#negative binomial model
library(glmmTMB)

q1_nb <- glmmTMB(production ~ group + block + (1|pop),
                 data = data_q1,
                 family = nbinom2)

summary(q1_nb)

anova(q1_nb)

p_val <- summary(q1_nb)$coefficients$cond["groupf2_mismatched", "Pr(>|z|)"]

#Likelihood ratio test for best fit between pois and nb
anova(q1_pois,q1_nb, test = "LRT")

#Plot For comparison 
###

library(ggplot2)

y_max1 <- max(data_q1$production)

q1_p <- ggplot(data_q1, aes(x = group, y = production, fill = group)) +
  
  geom_boxplot(width = 0.6, alpha = 0.8, outlier.size = 1.5) +
  
  # Fix labels
  scale_x_discrete(labels = c(
    "fx_matched" = "Mitonuclear\nMatched",
    "f2_mismatched" = "Mitonuclear\nMismatched"
  )) +
  
  scale_fill_manual(values = c(
    "fx_matched" = "#4E79A7",
    "f2_mismatched" = "#E15759"
  )) +
  
  # y-axis ticks
  scale_y_continuous(
    breaks = seq(0, 140, by = 20),
    expand = expansion(mult = c(0.02, 0.12))
  ) +
  
  # Significance bracket
  annotate("segment", x = 1, xend = 2,
           y = y_max1 * 1.05, yend = y_max1 * 1.05) +
  annotate("segment", x = 1, xend = 1,
           y = y_max1 * 1.02, yend = y_max1 * 1.05) +
  annotate("segment", x = 2, xend = 2,
           y = y_max1 * 1.02, yend = y_max1 * 1.05) +
  
  annotate("text",
           x = 1.5,
           y = y_max1 * 1.08,
           label = stars,
           size = 6) +
  
  labs(x = NULL,
       y = "Offspring Productivity",
       title = "Offspring Productivity between Matched and mismatched mitonuclear before adaptation") +
  
  theme_classic(base_size = 14) +
  theme(legend.position = "none")

q1_p