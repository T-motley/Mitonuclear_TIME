#Q1: Does admixture affect production (F2 mismatched vs fx matched)
library(dplyr)

#Create seperate Dataframe

data_q1 <- data %>%
  dplyr::filter((stage == "f2" & status == "mismatched") |
            (stage == "fx" & status == "matched"))

data_q1$group <- NA_character_
data_q1$group[data_q1$stage == "fx" & data_q1$status == "matched"] <- "fx_matched"
data_q1$group[data_q1$stage == "f2" & data_q1$status == "mismatched"] <- "f2_mismatched"
data_q1$group <- factor(data_q1$group, levels = c("fx_matched","f2_mismatched"))

data_q1$pop   <- factor(data_q1$pop)
data_q1$block <- factor(data_q1$block)

#Model

library(lmerTest)
q1_lm <- lmerTest::lmer(data = data_q1, production ~ group + block + (1|pop))
summary(q1_lm)
anova(q1_lm)