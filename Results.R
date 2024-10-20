# ----------------------------------------------------------------------------------------------------
# Sourcing
# ----------------------------------------------------------------------------------------------------
setwd("/Users/svenvanholtencharria/Documents/Econometrics Erasmus/Year 5/Thesis/Sven Code/")
library(dplyr)
library(tibble)
library(tidyverse)
library(patchwork)
library(ggplot2)
methods = c("binom","unif","adaptive","score","control" )
path = c("Results/Simulation 3 - homo/")

# ----------------------------------------------------------------------------------------------------
# Simulation 2
# ----------------------------------------------------------------------------------------------------
### Select filenames
sim2 <- list.files(path) %>%
  .[grepl(".csv", .)] %>%
  substr(., 1, nchar(.) - 4)


# ----------------------------------------------------------------------------------------------------
# BIAS
# ----------------------------------------------------------------------------------------------------
bias_m = data.frame(matrix(ncol = 5, nrow = 15, dimnames= list(sim2, methods)))
for (i in 1:length(sim2)) {
  file_name = sim2[i]
  temp = read.csv(paste(path,file_name, ".csv" , sep=""), row.names = 1)
  bias_m[i,] = temp[1,]
}

bias_low = bias_m %>%
  filter(grepl("low",rownames(.))) %>%
  rownames_to_column(var = "R2") %>%
  mutate(R2 = gsub("_.*", "", R2)) %>%
  pivot_longer(cols = -R2, names_to = "Method", values_to = "Value")

bias_medium = bias_m %>%
  filter(grepl("medium",rownames(.))) %>%
  rownames_to_column(var = "R2") %>%
  mutate(R2 = gsub("_.*", "", R2)) %>%
  pivot_longer(cols = -R2, names_to = "Method", values_to = "Value")

bias_high = bias_m %>%
  filter(grepl("high",rownames(.))) %>%
  rownames_to_column(var = "R2") %>%
  mutate(R2 = gsub("_.*", "", R2)) %>%
  pivot_longer(cols = -R2, names_to = "Method", values_to = "Value")

lim_bias = c(min(bias_m), max(bias_m))

plot_bias_low <- ggplot(bias_low, aes(x = R2, y = Value, color = Method, shape = Method, group = Method)) +
  geom_line(show.legend = FALSE) + 
  geom_point() +
  labs(x = bquote("R"^2), y = "Estimated Bias") + 
  theme_classic() + 
  scale_color_brewer(palette = "Dark2") +
  scale_shape_manual(values = c(1, 2, 3, 4, 5)) + 
  geom_line(aes(y = 0), color = "grey", linetype = "longdash") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5, face = "bold", size = 18), text = element_text(family = "serif"),  axis.title.y = element_text(size = 15), axis.title.x = element_text(size = 15)) +
  ylim(lim_bias) +
  ggtitle("Low precision")

plot_bias_medium <- ggplot(bias_medium, aes(x = R2, y = Value, color = Method, shape = Method, group = Method)) +
  geom_line(show.legend = FALSE) + 
  geom_point() +
  labs(title = "Values by R^{2}", x = bquote("R"^2), y = "Estimated Bias") + 
  theme_classic() + 
  scale_color_brewer(palette = "Dark2") +
  scale_shape_manual(values = c(1, 2, 3, 4, 5)) + 
  geom_line(aes(y = 0), color = "grey", linetype = "longdash") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5, face = "bold", size = 18), text = element_text(family = "serif"),  axis.title.y = element_text(size = 15), axis.title.x = element_text(size = 15)) +
  ylim(lim_bias) +
  ggtitle("Medium precision")

plot_bias_high <- ggplot(bias_high, aes(x = R2, y = Value, color = Method, shape = Method, group = Method)) +
  geom_line(show.legend = FALSE) + 
  geom_point() +
  labs(title = "Values by R^{2}", x = bquote("R"^2), y = "Estimated Bias") + 
  theme_classic() +
  scale_color_brewer(palette = "Dark2") +
  scale_shape_manual(values = c(1, 2, 3, 4, 5)) + 
  geom_line(aes(y = 0), color = "grey", linetype = "longdash") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5, face = "bold", size = 18), text = element_text(family = "serif"),  axis.title.y = element_text(size = 15), axis.title.x = element_text(size = 15)) +
  ylim(lim_bias) +
  ggtitle("High precision")

combined_bias <- plot_bias_low + plot_bias_medium + plot_bias_high + plot_layout(ncol = 3)

combined_bias



# ----------------------------------------------------------------------------------------------------
# BIAS / SE RATIO
# ----------------------------------------------------------------------------------------------------
ratio_m = data.frame(matrix(ncol = 5, nrow = 15, dimnames= list(sim2, methods)))
for (i in 1:length(sim2)) {
  file_name = sim2[i]
  temp = read.csv(paste(path,file_name, ".csv" , sep=""), row.names = 1)
  ratio_m[i,] = temp[1,] / temp[2,]
}

ratio_low = ratio_m %>%
  filter(grepl("low",rownames(.))) %>%
  rownames_to_column(var = "R2") %>%
  mutate(R2 = gsub("_.*", "", R2)) %>%
  pivot_longer(cols = -R2, names_to = "Method", values_to = "Value")

ratio_medium = ratio_m %>%
  filter(grepl("medium",rownames(.))) %>%
  rownames_to_column(var = "R2") %>%
  mutate(R2 = gsub("_.*", "", R2)) %>%
  pivot_longer(cols = -R2, names_to = "Method", values_to = "Value")

ratio_high = ratio_m %>%
  filter(grepl("high",rownames(.))) %>%
  rownames_to_column(var = "R2") %>%
  mutate(R2 = gsub("_.*", "", R2)) %>%
  pivot_longer(cols = -R2, names_to = "Method", values_to = "Value")

lim_ratio = c(min(ratio_m), max(ratio_m))

plot_ratio_low <- ggplot(ratio_low, aes(x = R2, y = Value, color = Method, shape = Method, group = Method)) +
  geom_line(show.legend = FALSE) + 
  geom_point() +
  labs(x = bquote("R"^2), y = "Ratio") + 
  theme_classic() + 
  scale_color_brewer(palette = "Dark2") +
  scale_shape_manual(values = c(1, 2, 3, 4, 5)) + 
  geom_line(aes(y = 0), color = "grey", linetype = "longdash") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5, face = "bold", size = 18), text = element_text(family = "serif"),  axis.title.y = element_text(size = 15), axis.title.x = element_text(size = 15)) +
  ylim(lim_ratio) +
  ggtitle("Low precision")

plot_ratio_medium <- ggplot(ratio_medium, aes(x = R2, y = Value, color = Method, shape = Method, group = Method)) +
  geom_line(show.legend = FALSE) + 
  geom_point() +
  labs(x = bquote("R"^2), y = "Ratio") + 
  theme_classic() + 
  scale_color_brewer(palette = "Dark2") +
  scale_shape_manual(values = c(1, 2, 3, 4, 5)) + 
  geom_line(aes(y = 0), color = "grey", linetype = "longdash") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5, face = "bold", size = 18), text = element_text(family = "serif"),  axis.title.y = element_text(size = 15), axis.title.x = element_text(size = 15)) +
  ylim(lim_ratio) +
  ggtitle("Medium precision")

plot_ratio_high <- ggplot(ratio_high, aes(x = R2, y = Value, color = Method, shape = Method, group = Method)) +
  geom_line(show.legend = FALSE) + 
  geom_point() +
  labs(x = bquote("R"^2), y = "Ratio") + 
  theme_classic() +
  scale_color_brewer(palette = "Dark2") +
  geom_line(aes(y = 0), color = "grey", linetype = "longdash") +
  scale_shape_manual(values = c(1, 2, 3, 4, 5)) + 
  theme(legend.position = "none",plot.title = element_text(hjust = 0.5, face = "bold", size = 18), text = element_text(family = "serif"),  axis.title.y = element_text(size = 15), axis.title.x = element_text(size = 15)) +
  ylim(lim_ratio) +
  ggtitle("High precision")

combined_ratio <- plot_ratio_low + plot_ratio_medium + plot_ratio_high + plot_layout(ncol = 3)





# ----------------------------------------------------------------------------------------------------
# COVERAGE
# ----------------------------------------------------------------------------------------------------
coverage_m = data.frame(matrix(ncol = 5, nrow = 15, dimnames= list(sim2, methods)))
for (i in 1:length(sim2)) {
  file_name = sim2[i]
  temp = read.csv(paste(path,file_name, ".csv" , sep=""), row.names = 1)
  coverage_m[i,] = temp[3,]
}

coverage_low = coverage_m %>%
  filter(grepl("low",rownames(.))) %>%
  rownames_to_column(var = "R2") %>%
  mutate(R2 = gsub("_.*", "", R2)) %>%
  pivot_longer(cols = -R2, names_to = "Method", values_to = "Value")

coverage_medium = coverage_m %>%
  filter(grepl("medium",rownames(.))) %>%
  rownames_to_column(var = "R2") %>%
  mutate(R2 = gsub("_.*", "", R2)) %>%
  pivot_longer(cols = -R2, names_to = "Method", values_to = "Value")

coverage_high = coverage_m %>%
  filter(grepl("high",rownames(.))) %>%
  rownames_to_column(var = "R2") %>%
  mutate(R2 = gsub("_.*", "", R2)) %>%
  pivot_longer(cols = -R2, names_to = "Method", values_to = "Value")

# lim_coverage = c(0.05, max(coverage_m))
lim_coverage = c(min(coverage_m), max(coverage_m))

plot_coverage_low <- ggplot(coverage_low, aes(x = R2, y = Value, color = Method, shape = Method, group = Method)) +
  geom_line(show.legend = FALSE) + 
  geom_point() +
  labs(x = bquote("R"^2), y = "Rejection Probability") + 
  theme_classic() + 
  scale_color_brewer(palette = "Dark2") +
  scale_shape_manual(values = c(1, 2, 3, 4, 5)) + 
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5, face = "bold", size = 18), text = element_text(family = "serif"),  axis.title.y = element_text(size = 15), axis.title.x = element_text(size = 15)) +
  geom_line(aes(y = 0.05), color = "grey", linetype = "longdash") +
  ylim(lim_coverage) +
  ggtitle("Low precision")

plot_coverage_medium <- ggplot(coverage_medium, aes(x = R2, y = Value, color = Method, shape = Method, group = Method)) +
  geom_line(show.legend = FALSE) + 
  geom_point() +
  labs(x = bquote("R"^2), y = "Rejection Probability") + 
  theme_classic() + 
  scale_color_brewer(palette = "Dark2") +
  scale_shape_manual(values = c(1, 2, 3, 4, 5)) + 
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5, face = "bold", size = 18), text = element_text(family = "serif"),  axis.title.y = element_text(size = 15), axis.title.x = element_text(size = 15)) +
  geom_line(aes(y = 0.05), color = "grey", linetype = "longdash") +
  ylim(lim_coverage) +
  ggtitle("Medium precision")

plot_coverage_high <- ggplot(coverage_high, aes(x = R2, y = Value, color = Method, shape = Method, group = Method)) +
  geom_line(show.legend = FALSE) + 
  geom_point() +
  labs(x = bquote("R"^2), y = "Rejection Probability") + 
  theme_classic() +
  scale_color_brewer(palette = "Dark2") +
  scale_shape_manual(values = c(1, 2, 3, 4, 5)) + 
  theme(legend.position = "none",plot.title = element_text(hjust = 0.5, face = "bold", size = 18), text = element_text(family = "serif"),  axis.title.y = element_text(size = 15), axis.title.x = element_text(size = 15)) +
  geom_line(aes(y = 0.05), color = "grey", linetype = "longdash") +
  ylim(lim_coverage) +
  ggtitle("High precision")

combined_coverage <- plot_coverage_low + plot_coverage_medium + plot_coverage_high + plot_layout(ncol = 3)

combined_coverage



# ----------------------------------------------------------------------------------------------------
# RELEVANT CONTROLS
# ----------------------------------------------------------------------------------------------------
relevant_m = data.frame(matrix(ncol = 5, nrow = 15, dimnames= list(sim2, methods)))
for (i in 1:length(sim2)) {
  file_name = sim2[i]
  temp = read.csv(paste(path,file_name, ".csv" , sep=""), row.names = 1)
  relevant_m[i,] = temp[4,]
}

relevant_low = relevant_m %>%
  filter(grepl("low",rownames(.))) %>%
  rownames_to_column(var = "R2") %>%
  mutate(R2 = gsub("_.*", "", R2)) %>%
  pivot_longer(cols = -R2, names_to = "Method", values_to = "Value")

relevant_medium = relevant_m %>%
  filter(grepl("medium",rownames(.))) %>%
  rownames_to_column(var = "R2") %>%
  mutate(R2 = gsub("_.*", "", R2)) %>%
  pivot_longer(cols = -R2, names_to = "Method", values_to = "Value")

relevant_high = relevant_m %>%
  filter(grepl("high",rownames(.))) %>%
  rownames_to_column(var = "R2") %>%
  mutate(R2 = gsub("_.*", "", R2)) %>%
  pivot_longer(cols = -R2, names_to = "Method", values_to = "Value")

lim_relevant = c(min(relevant_m), max(relevant_m))

plot_relevant_low <- ggplot(relevant_low, aes(x = R2, y = Value, color = Method, shape = Method, group = Method)) +
  geom_line(show.legend = FALSE) + 
  geom_point() +
  labs(x = bquote("R"^2), y = "# Selected Relevant Controls") + 
  theme_classic() + 
  scale_color_brewer(palette = "Dark2") +
  scale_shape_manual(values = c(1, 2, 3, 4, 5)) + 
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5, face = "bold", size = 18), text = element_text(family = "serif"),  axis.title.y = element_text(size = 15), axis.title.x = element_text(size = 15)) +
  ylim(lim_relevant) +
  ggtitle("Low precision")

plot_relevant_medium <- ggplot(relevant_medium, aes(x = R2, y = Value, color = Method, shape = Method, group = Method)) +
  geom_line(show.legend = FALSE) + 
  geom_point() +
  labs(x = bquote("R"^2), y = "# Selected Relevant Controls") + 
  theme_classic() + 
  scale_color_brewer(palette = "Dark2") +
  scale_shape_manual(values = c(1, 2, 3, 4, 5)) + 
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5, face = "bold", size = 18), text = element_text(family = "serif"),  axis.title.y = element_text(size = 15), axis.title.x = element_text(size = 15)) +
  ylim(lim_relevant) +
  ggtitle("Medium precision")

plot_relevant_high <- ggplot(relevant_high, aes(x = R2, y = Value, color = Method, shape = Method, group = Method)) +
  geom_line(show.legend = FALSE) + 
  geom_point() +
  labs(x = bquote("R"^2), y = "# Selected Relevant Controls") + 
  theme_classic() +
  scale_color_brewer(palette = "Dark2") +
  scale_shape_manual(values = c(1, 2, 3, 4, 5)) + 
  theme(legend.position = "none",plot.title = element_text(hjust = 0.5, face = "bold", size = 18), text = element_text(family = "serif"),  axis.title.y = element_text(size = 15), axis.title.x = element_text(size = 15)) +
  ylim(lim_relevant) +
  ggtitle("High precision")

combined_relevant <- plot_relevant_low + plot_relevant_medium + plot_relevant_high + plot_layout(ncol = 3)

combined_relevant




# ----------------------------------------------------------------------------------------------------
# TOTAL CONTROLS
# ----------------------------------------------------------------------------------------------------
total_m = data.frame(matrix(ncol = 5, nrow = 15, dimnames= list(sim2, methods)))
for (i in 1:length(sim2)) {
  file_name = sim2[i]
  temp = read.csv(paste(path,file_name, ".csv" , sep=""), row.names = 1)
  total_m[i,] = temp[5,]
}

total_low = total_m %>%
  filter(grepl("low",rownames(.))) %>%
  rownames_to_column(var = "R2") %>%
  mutate(R2 = gsub("_.*", "", R2)) %>%
  pivot_longer(cols = -R2, names_to = "Method", values_to = "Value")

total_medium = total_m %>%
  filter(grepl("medium",rownames(.))) %>%
  rownames_to_column(var = "R2") %>%
  mutate(R2 = gsub("_.*", "", R2)) %>%
  pivot_longer(cols = -R2, names_to = "Method", values_to = "Value")

total_high = total_m %>%
  filter(grepl("high",rownames(.))) %>%
  rownames_to_column(var = "R2") %>%
  mutate(R2 = gsub("_.*", "", R2)) %>%
  pivot_longer(cols = -R2, names_to = "Method", values_to = "Value")

lim_total = c(min(total_m), max(total_m))

plot_total_low <- ggplot(total_low, aes(x = R2, y = Value, color = Method, shape = Method, group = Method)) +
  geom_line(show.legend = FALSE) + 
  geom_point() +
  labs(x = bquote("R"^2), y = "# Selected Total Controls") + 
  theme_classic() + 
  scale_color_brewer(palette = "Dark2") +
  scale_shape_manual(values = c(1, 2, 3, 4, 5)) + 
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5, face = "bold", size = 18), text = element_text(family = "serif"),  axis.title.y = element_text(size = 15), axis.title.x = element_text(size = 15)) +
  geom_line(aes(y = 10), color = "grey", linetype = "longdash") +
  ylim(lim_total) +
  ggtitle("Low precision")

plot_total_medium <- ggplot(total_medium, aes(x = R2, y = Value, color = Method, shape = Method, group = Method)) +
  geom_line(show.legend = FALSE) + 
  geom_point() +
  labs(x = bquote("R"^2), y = "# Selected Total Controls") + 
  theme_classic() + 
  scale_color_brewer(palette = "Dark2") +
  scale_shape_manual(values = c(1, 2, 3, 4, 5)) + 
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5, face = "bold", size = 18), text = element_text(family = "serif"),  axis.title.y = element_text(size = 15), axis.title.x = element_text(size = 15)) +
  geom_line(aes(y = 10), color = "grey", linetype = "longdash") +
  ylim(lim_total) +
  ggtitle("Medium precision")

plot_total_high <- ggplot(total_high, aes(x = R2, y = Value, color = Method, shape = Method, group = Method)) +
  geom_line(show.legend = FALSE) + 
  geom_point() +
  labs(x = bquote("R"^2), y = "# Selected Total Controls") + 
  theme_classic() +
  scale_color_brewer(palette = "Dark2") +
  scale_shape_manual(values = c(1, 2, 3, 4, 5)) + 
  theme(legend.position = "none",plot.title = element_text(hjust = 0.5, face = "bold", size = 18), text = element_text(family = "serif"),  axis.title.y = element_text(size = 15), axis.title.x = element_text(size = 15)) +
  geom_line(aes(y = 10), color = "grey", linetype = "longdash") +
  ylim(lim_total) +
  ggtitle("High precision")

combined_total <- plot_total_low + plot_total_medium + plot_total_high + plot_layout(ncol = 3)





# ----------------------------------------------------------------------------------------------------
# PRINTING
# ----------------------------------------------------------------------------------------------------
print(combined_bias)
print(combined_ratio)
print(combined_coverage)
print(combined_relevant)
print(combined_total)
