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


# Plot 1: combined Boxplot 

ggplot(data_fig,
       aes(x = group_label,
           y = production,
           fill = group_label)) +
  
  geom_boxplot(width = 0.6,
               alpha = 0.85,
               outlier.size = 1.3) +
  
  scale_x_discrete(labels = c(
    "Fx matched" = "Fx\nMatched",
    "F2 mismatched" = "F2\nMixed (Before)",
    "Fx mismatched" = "Fx\nMixed (After)"
  )) +
  
  scale_fill_manual(values = c(
    "Fx matched" = "darkgreen",
    "F2 mismatched" = "red",
    "Fx mismatched" = "orange"
  )) +
  
  scale_y_continuous(
    breaks = seq(0, 140, by = 20),
    expand = expansion(mult = c(0.02, 0.12))
  ) +
  
  labs(x = NULL,
       y = "Offspring Productivity",
       title = "Productivity of Mitonuclear Combinations Before and After Adaptation") +
  
  theme_classic(base_size = 14) +
  theme(legend.position = "none")