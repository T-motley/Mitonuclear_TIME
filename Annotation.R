#Annotation of Data
data <- main_production_assay

#Create matched vs mismatched column 
data$status <- NA_character_
data$status[data$genotype %in% c("mM","tT")] <- "matched"
data$status[data$genotype %in% c("mX","tX")] <- "mismatched"
data$status <- factor(data$status, levels = c("matched", "mismatched"))

#Create stage (F2 vs Fx) Column 
data$stage <- NA_character_
data$stage <- ifelse(data$pop %in% 21:26, "f2","fx")
data$stage <- factor(data$stage, levels = c("fx","f2"))

