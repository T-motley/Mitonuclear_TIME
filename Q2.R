#Q2. Does evolution restore / alter production toward matched population after admixture
library(dplyr)
library(lmerTest)

#Seperate Dataframe to Only Fx stage
data_q2 <- data %>%
  filter(stage == "fx")

data_q2$pop   <- factor(data_q2$pop)
data_q2$block <- factor(data_q2$block)

#Mixed Model
q2_lm <- lmer(data = data_q2, production ~ status + block + (1|pop))

summary(q2_lm)
anova(q2_lm)