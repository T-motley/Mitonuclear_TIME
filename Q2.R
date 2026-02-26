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


#negative binomial model
library(glmmTMB)

q2_nb <- glmmTMB(production ~ status + block + (1|pop),
                 data = data_q2,
                 family = nbinom2)

summary(q2_nb)