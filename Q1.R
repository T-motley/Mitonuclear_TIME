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


#negative binomial model
library(glmmTMB)

q1_nb <- glmmTMB(production ~ group + block + (1|pop),
                 data = data_q1,
                 family = nbinom2)

summary(q1_nb)