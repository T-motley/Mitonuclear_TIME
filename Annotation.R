#Annotation of Data
data <- main_production_assay

data <- na.omit(data)

#Create matched vs mismatched column 
data$status <- NA_character_
data$status[data$genotype %in% c("mM","tT")] <- "matched"
data$status[data$genotype %in% c("mX","tX")] <- "mismatched"
data$status <- factor(data$status, levels = c("matched", "mismatched"))

#Create stage (F2 vs Fx) Column 
data$stage <- NA_character_
data$stage <- ifelse(data$pop %in% 21:26, "f2","fx")
data$stage <- factor(data$stage, levels = c("fx","f2"))

#Plot Information 
stars <- ifelse(p_val < 0.001, "***",
                ifelse(p_val < 0.01, "**",
                       ifelse(p_val < 0.05, "*", "ns")))

stars2 <- ifelse(p_val2 < 0.001, "***",
                ifelse(p_val2 < 0.01, "**",
                       ifelse(p_val2 < 0.05, "*", "ns")))

stars3 <- ifelse(p_val3 < 0.001, "***",
                 ifelse(p_val3 < 0.01, "**",
                        ifelse(p_val3 < 0.05, "*", "ns")))
