#Box Plot
library(ggplot2)
library(ggpubr)
library(dplyr)

#Combined Data from both Questions
data_fig <- data %>%
  filter( (stage == "f2" & status == "mismatched") |  # Q1
            (stage == "fx" & status %in% c("matched", "mismatched")) ) %>%  
  mutate(group_label = case_when(
    stage == "fx" & status == "matched" ~ "Fx matched",
    stage == "fx" & status == "mismatched" ~ "Fx mismatched", 
    stage == "f2" & status == "mismatched" ~ "F2 mismatched"),
    group_label = factor(group_label, 
                         levels = c("Fx matched", "F2 mismatched", "Fx mismatched")))


# Plot 1: Boxplot with model-based means and significance
p1 <- ggplot(data_fig, aes(x = group_label, y = production, fill = group_label)) +
  geom_boxplot(alpha = 0.7, width = 0.7) +
  stat_summary(fun = mean, geom = "point", size = 3, color = "black") +
  stat_summary(fun.data = mean_se, geom = "errorbar", 
               width = 0.2, size = 0.8, color = "black") +
  scale_fill_manual(values = c("Fx matched" = "green", 
                               "F2 mismatched" = "red", 
                               "Fx mismatched" = "orange")) +
  labs(x = "Mitonuclear status and evolutionary stage",
       y = "Offspring production per female",
       title = "Admixture and evolutionary effects on production") +
  theme_bw(base_size= 12) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))

#add significance brackets(Q1: Fx matched vs f2 mismatched, Q2: Fx mismatched vs Fx Matched)
p1 <- p1 + 
  stat_compare_means(comparisons = list(c("Fx matched", "F2 mismatched")),
                     label = "p.signif", method = "t.test") +
  stat_compare_means(comparisons = list(c("Fx matched", "Fx mismatched")),
                     label = "p.signif", method = "t.test", 
                     label.y = 85)  
