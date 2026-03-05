#Q2. Does evolution restore / alter production toward matched population after admixture
library(dplyr)
library(lmerTest)

#Separate Data-frame to Only Fx stage
data_q2 <- data %>%
  filter(stage == "fx")

data_q2$pop   <- factor(data_q2$pop)
data_q2$block <- factor(data_q2$block)

#Model
q2_lm <- lmer(data = data_q2, production ~ status + block + (1|pop))

summary(q2_lm)
anova(q2_lm)

#Checking assumptions for LMM (Gaussian)
res2 <- resid(q2_lm)
qqnorm(res2) ; qqline(res2)

#Poisson Model
library(glmmTMB)

q2_pois <- glmmTMB(production ~ status + block + (1|pop),
                   data = data_q2,
                   family = poisson())

#negative binomial model
library(glmmTMB)

q2_nb <- glmmTMB(production ~ status + block + (1|pop),
                 data = data_q2,
                 family = nbinom2)

summary(q2_nb)

p_val2 <- summary(q2_nb)$coefficients$cond["statusmismatched", "Pr(>|z|)"]

#LRT
anova(q2_pois,q2_nb, test = "LRT")

#Plot for comparison 
library(ggplot2)

stars2 <- ifelse(p_val2 < 0.001, "***",
                 ifelse(p_val2 < 0.01, "**",
                        ifelse(p_val2 < 0.05, "*", "ns")))

y_max2 <- max(data_q2$production)

q2_p <- ggplot(data_q2, aes(x = status, y = production, fill = status)) +
  
  geom_boxplot(width = 0.6, alpha = 0.8, outlier.size = 1.5) +
 
   scale_x_discrete(labels = c(
    "matched" = "Mitonuclear\nMatched",
    "mismatched" = "Mitonuclear\nMixed"
  )) +
  scale_fill_manual(values = c(
    "matched" = "darkgreen",
    "mismatched" = "orange"
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
           label = stars2,
           size = 6) +
  
  labs(x = NULL,
       y = "Offspring Productivity",
       title = "Offspring productivity comparison between Matched and\nMixed mitonuclear combinations after adaptation") +
  
  theme_classic(base_size = 14) +
  theme(legend.position = "none")

q2_p

#Is there difference between F2 mixed and FX mixed?

q25_nb <- glmmTMB(production ~ group_label + block + (1|pop),
                 data = data_fig,
                 family = nbinom2)

summary(q25_nb)

p_val3 <- summary(q25_nb)$coefficients$cond["group_labelF2 mismatched", "Pr(>|z|)"]