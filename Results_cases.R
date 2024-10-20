# ----------------------------------------------------------------------------------------------------
# Sourcing
# ----------------------------------------------------------------------------------------------------
setwd("/Users/svenvanholtencharria/Documents/Econometrics Erasmus/Year 5/Thesis/Sven Code/")
library(dplyr)
library(tibble)
library(tidyverse)
library(ggplot2)
methods = c("control","controlAmel" ,"binom","unif","adaptive","score")
path = c("Results/Simulation 3 - cases/")


# ----------------------------------------------------------------------------------------------------
# Filenames
# ----------------------------------------------------------------------------------------------------
### Select filenames
sim2 <- list.files(path) %>%
  .[grepl(".csv", .)] %>%
  substr(., 1, nchar(.) - 4)





# ----------------------------------------------------------------------------------------------------
# BIAS
# ----------------------------------------------------------------------------------------------------
bias_mat = data.frame(matrix(ncol = 6, nrow = 9, dimnames= list(sim2, methods)))
for (i in 1:length(sim2)) {
  file_name = sim2[i]
  temp = read.csv(paste(path,file_name, ".csv" , sep=""), row.names = 1)
  bias_mat[i,] = temp[1,]
}

bias_v = bias_mat %>%
  filter(grepl("violent",rownames(.))) %>%
  rownames_to_column(var = "size") %>%
  mutate(size = as.numeric(sub(".*_([0-9]+)", "\\1",size))) %>%
  pivot_longer(cols = -size, names_to = "Method", values_to = "Value")

bias_p = bias_mat %>%
  filter(grepl("property",rownames(.))) %>%
  rownames_to_column(var = "size") %>%
  mutate(size = as.numeric(sub(".*_([0-9]+)", "\\1",size))) %>%
  arrange(size) %>%
  pivot_longer(cols = -size, names_to = "Method", values_to = "Value")

bias_m = bias_mat %>%
  filter(grepl("murder",rownames(.))) %>%
  rownames_to_column(var = "size") %>%
  mutate(size = as.numeric(sub(".*_([0-9]+)", "\\1",size))) %>%
  arrange(size) %>%
  pivot_longer(cols = -size, names_to = "Method", values_to = "Value")

lim_bias = c(min(bias_mat), max(bias_mat))

plot_bias_v <- ggplot(bias_v, aes(x = size, y = Value, color = Method, shape = Method, group = Method)) +
  geom_line(show.legend = FALSE) + 
  geom_point() +
  labs(x = "# Controls", y = "Bias") + 
  theme_classic() + 
  scale_color_brewer(palette = "Dark2") +
  scale_shape_manual(values = c(1, 2, 3, 4, 5, 6)) + 
  #geom_line(aes(y = 0), color = "grey", linetype = "longdash") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5, face = "bold")) +
  #ylim(lim_bias) +
  ggtitle("Violence")

plot_bias_p <- ggplot(bias_p, aes(x = size, y = Value, color = Method, shape = Method, group = Method)) +
  geom_line(show.legend = FALSE) + 
  geom_point() +
  labs(x = "# Controls", y = "Bias") + 
  theme_classic() + 
  scale_color_brewer(palette = "Dark2") +
  scale_shape_manual(values = c(1, 2, 3, 4, 5, 6)) + 
  #geom_line(aes(y = 0), color = "grey", linetype = "longdash") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5, face = "bold")) +
  #ylim(lim_bias) +
  ggtitle("Property")

plot_bias_m <- ggplot(bias_m, aes(x = size, y = Value, color = Method, shape = Method, group = Method)) +
  geom_line(show.legend = FALSE) + 
  geom_point() +
  labs(x = "# Controls", y = "Bias") + 
  theme_classic() +
  scale_color_brewer(palette = "Dark2") +
  scale_shape_manual(values = c(1, 2, 3, 4, 5,6)) + 
  #geom_line(aes(y = 0), color = "grey", linetype = "longdash") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  #ylim(lim_bias) +
  ggtitle("Murder")

combined_bias <- plot_bias_v + plot_bias_p + plot_bias_m + plot_layout(ncol = 3)






# ----------------------------------------------------------------------------------------------------
# relevant
# ----------------------------------------------------------------------------------------------------
relevant_mat = data.frame(matrix(ncol = 6, nrow = 9, dimnames= list(sim2, methods)))
for (i in 1:length(sim2)) {
  file_name = sim2[i]
  temp = read.csv(paste(path,file_name, ".csv" , sep=""), row.names = 1)
  relevant_mat[i,] = temp[2,]
}

relevant_v = relevant_mat %>%
  filter(grepl("violent",rownames(.))) %>%
  rownames_to_column(var = "size") %>%
  mutate(size = as.numeric(sub(".*_([0-9]+)", "\\1",size))) %>%
  pivot_longer(cols = -size, names_to = "Method", values_to = "Value")

relevant_p = relevant_mat %>%
  filter(grepl("property",rownames(.))) %>%
  rownames_to_column(var = "size") %>%
  mutate(size = as.numeric(sub(".*_([0-9]+)", "\\1",size))) %>%
  arrange(size) %>%
  pivot_longer(cols = -size, names_to = "Method", values_to = "Value")

relevant_m = relevant_mat %>%
  filter(grepl("murder",rownames(.))) %>%
  rownames_to_column(var = "size") %>%
  mutate(size = as.numeric(sub(".*_([0-9]+)", "\\1",size))) %>%
  arrange(size) %>%
  pivot_longer(cols = -size, names_to = "Method", values_to = "Value")

lim_relevant = c(min(relevant_mat), max(relevant_mat))

plot_relevant_v <- ggplot(relevant_v, aes(x = size, y = Value, color = Method, shape = Method, group = Method)) +
  geom_line(show.legend = FALSE) + 
  geom_point() +
  labs(x = "# Controls", y = "relevant") + 
  theme_classic() + 
  scale_color_brewer(palette = "Dark2") +
  scale_shape_manual(values = c(1, 2, 3, 4, 5, 6)) + 
  #geom_line(aes(y = 0), color = "grey", linetype = "longdash") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5, face = "bold")) +
  #ylim(lim_relevant) +
  ggtitle("Violence")

plot_relevant_p <- ggplot(relevant_p, aes(x = size, y = Value, color = Method, shape = Method, group = Method)) +
  geom_line(show.legend = FALSE) + 
  geom_point() +
  labs(x = "# Controls", y = "relevant") + 
  theme_classic() + 
  scale_color_brewer(palette = "Dark2") +
  scale_shape_manual(values = c(1, 2, 3, 4, 5, 6)) + 
  #geom_line(aes(y = 0), color = "grey", linetype = "longdash") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5, face = "bold")) +
  #ylim(lim_relevant) +
  ggtitle("Property")

plot_relevant_m <- ggplot(relevant_m, aes(x = size, y = Value, color = Method, shape = Method, group = Method)) +
  geom_line(show.legend = FALSE) + 
  geom_point() +
  labs(x = "# Controls", y = "relevant") + 
  theme_classic() +
  scale_color_brewer(palette = "Dark2") +
  scale_shape_manual(values = c(1, 2, 3, 4, 5,6)) + 
  #geom_line(aes(y = 0), color = "grey", linetype = "longdash") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  #ylim(lim_relevant) +
  ggtitle("Murder")

combined_relevant <- plot_relevant_v + plot_relevant_p + plot_relevant_m + plot_layout(ncol = 3)






# ----------------------------------------------------------------------------------------------------
# relevant
# ----------------------------------------------------------------------------------------------------
relevant_mat = data.frame(matrix(ncol = 6, nrow = 9, dimnames= list(sim2, methods)))
for (i in 1:length(sim2)) {
  file_name = sim2[i]
  temp = read.csv(paste(path,file_name, ".csv" , sep=""), row.names = 1)
  relevant_mat[i,] = temp[3,]
}

relevant_v = relevant_mat %>%
  filter(grepl("violent",rownames(.))) %>%
  rownames_to_column(var = "size") %>%
  mutate(size = as.numeric(sub(".*_([0-9]+)", "\\1",size))) %>%
  pivot_longer(cols = -size, names_to = "Method", values_to = "Value")

relevant_p = relevant_mat %>%
  filter(grepl("property",rownames(.))) %>%
  rownames_to_column(var = "size") %>%
  mutate(size = as.numeric(sub(".*_([0-9]+)", "\\1",size))) %>%
  arrange(size) %>%
  pivot_longer(cols = -size, names_to = "Method", values_to = "Value")

relevant_m = relevant_mat %>%
  filter(grepl("murder",rownames(.))) %>%
  rownames_to_column(var = "size") %>%
  mutate(size = as.numeric(sub(".*_([0-9]+)", "\\1",size))) %>%
  arrange(size) %>%
  pivot_longer(cols = -size, names_to = "Method", values_to = "Value")

lim_relevant = c(min(relevant_mat), max(relevant_mat))

plot_relevant_v <- ggplot(relevant_v, aes(x = size, y = Value, color = Method, shape = Method, group = Method)) +
  geom_line(show.legend = FALSE) + 
  geom_point() +
  labs(x = "# Controls", y = "relevant") + 
  theme_classic() + 
  scale_color_brewer(palette = "Dark2") +
  scale_shape_manual(values = c(1, 2, 3, 4, 5, 6)) + 
  #geom_line(aes(y = 0), color = "grey", linetype = "longdash") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5, face = "bold")) +
  #ylim(lim_relevant) +
  ggtitle("Violence")

plot_relevant_p <- ggplot(relevant_p, aes(x = size, y = Value, color = Method, shape = Method, group = Method)) +
  geom_line(show.legend = FALSE) + 
  geom_point() +
  labs(x = "# Controls", y = "relevant") + 
  theme_classic() + 
  scale_color_brewer(palette = "Dark2") +
  scale_shape_manual(values = c(1, 2, 3, 4, 5, 6)) + 
  #geom_line(aes(y = 0), color = "grey", linetype = "longdash") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5, face = "bold")) +
  #ylim(lim_relevant) +
  ggtitle("Property")

plot_relevant_m <- ggplot(relevant_m, aes(x = size, y = Value, color = Method, shape = Method, group = Method)) +
  geom_line(show.legend = FALSE) + 
  geom_point() +
  labs(x = "# Controls", y = "relevant") + 
  theme_classic() +
  scale_color_brewer(palette = "Dark2") +
  scale_shape_manual(values = c(1, 2, 3, 4, 5,6)) + 
  #geom_line(aes(y = 0), color = "grey", linetype = "longdash") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  #ylim(lim_relevant) +
  ggtitle("Murder")

combined_relevant <- plot_relevant_v + plot_relevant_p + plot_relevant_m + plot_layout(ncol = 3)






# ----------------------------------------------------------------------------------------------------
# Relevant
# ----------------------------------------------------------------------------------------------------
relevant_mat = data.frame(matrix(ncol = 6, nrow = 9, dimnames= list(sim2, methods)))
for (i in 1:length(sim2)) {
  file_name = sim2[i]
  temp = read.csv(paste(path,file_name, ".csv" , sep=""), row.names = 1)
  relevant_mat[i,] = temp[4,]
}

relevant_v = relevant_mat %>%
  filter(grepl("violent",rownames(.))) %>%
  rownames_to_column(var = "size") %>%
  mutate(size = as.numeric(sub(".*_([0-9]+)", "\\1",size))) %>%
  pivot_longer(cols = -size, names_to = "Method", values_to = "Value")

relevant_p = relevant_mat %>%
  filter(grepl("property",rownames(.))) %>%
  rownames_to_column(var = "size") %>%
  mutate(size = as.numeric(sub(".*_([0-9]+)", "\\1",size))) %>%
  arrange(size) %>%
  pivot_longer(cols = -size, names_to = "Method", values_to = "Value")

relevant_m = relevant_mat %>%
  filter(grepl("murder",rownames(.))) %>%
  rownames_to_column(var = "size") %>%
  mutate(size = as.numeric(sub(".*_([0-9]+)", "\\1",size))) %>%
  arrange(size) %>%
  pivot_longer(cols = -size, names_to = "Method", values_to = "Value")

lim_relevant = c(min(relevant_mat), max(relevant_mat))

plot_relevant_v <- ggplot(relevant_v, aes(x = size, y = Value, color = Method, shape = Method, group = Method)) +
  geom_line(show.legend = FALSE) + 
  geom_point() +
  labs(x = "# Controls", y = "relevant") + 
  theme_classic() + 
  scale_color_brewer(palette = "Dark2") +
  scale_shape_manual(values = c(1, 2, 3, 4, 5, 6)) + 
  #geom_line(aes(y = 0), color = "grey", linetype = "longdash") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5, face = "bold")) +
  #ylim(lim_relevant) +
  ggtitle("Violence")

plot_relevant_p <- ggplot(relevant_p, aes(x = size, y = Value, color = Method, shape = Method, group = Method)) +
  geom_line(show.legend = FALSE) + 
  geom_point() +
  labs(x = "# Controls", y = "relevant") + 
  theme_classic() + 
  scale_color_brewer(palette = "Dark2") +
  scale_shape_manual(values = c(1, 2, 3, 4, 5, 6)) + 
  #geom_line(aes(y = 0), color = "grey", linetype = "longdash") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5, face = "bold")) +
  #ylim(lim_relevant) +
  ggtitle("Property")

plot_relevant_m <- ggplot(relevant_m, aes(x = size, y = Value, color = Method, shape = Method, group = Method)) +
  geom_line(show.legend = FALSE) + 
  geom_point() +
  labs(x = "# Controls", y = "relevant") + 
  theme_classic() +
  scale_color_brewer(palette = "Dark2") +
  scale_shape_manual(values = c(1, 2, 3, 4, 5,6)) + 
  #geom_line(aes(y = 0), color = "grey", linetype = "longdash") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  #ylim(lim_relevant) +
  ggtitle("Murder")

combined_relevant <- plot_relevant_v + plot_relevant_p + plot_relevant_m + plot_layout(ncol = 3)






# ----------------------------------------------------------------------------------------------------
# Total
# ----------------------------------------------------------------------------------------------------
total_mat = data.frame(matrix(ncol = 6, nrow = 9, dimnames= list(sim2, methods)))
for (i in 1:length(sim2)) {
  file_name = sim2[i]
  temp = read.csv(paste(path,file_name, ".csv" , sep=""), row.names = 1)
  total_mat[i,] = temp[5,]
}

total_v = total_mat %>%
  filter(grepl("violent",rownames(.))) %>%
  rownames_to_column(var = "size") %>%
  mutate(size = as.numeric(sub(".*_([0-9]+)", "\\1",size))) %>%
  pivot_longer(cols = -size, names_to = "Method", values_to = "Value")

total_p = total_mat %>%
  filter(grepl("property",rownames(.))) %>%
  rownames_to_column(var = "size") %>%
  mutate(size = as.numeric(sub(".*_([0-9]+)", "\\1",size))) %>%
  arrange(size) %>%
  pivot_longer(cols = -size, names_to = "Method", values_to = "Value")

total_m = total_mat %>%
  filter(grepl("murder",rownames(.))) %>%
  rownames_to_column(var = "size") %>%
  mutate(size = as.numeric(sub(".*_([0-9]+)", "\\1",size))) %>%
  arrange(size) %>%
  pivot_longer(cols = -size, names_to = "Method", values_to = "Value")

lim_total = c(min(total_mat), max(total_mat))

plot_total_v <- ggplot(total_v, aes(x = size, y = Value, color = Method, shape = Method, group = Method)) +
  geom_line(show.legend = FALSE) + 
  geom_point() +
  labs(x = "# Controls", y = "total") + 
  theme_classic() + 
  scale_color_brewer(palette = "Dark2") +
  scale_shape_manual(values = c(1, 2, 3, 4, 5, 6)) + 
  #geom_line(aes(y = 0), color = "grey", linetype = "longdash") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5, face = "bold")) +
  #ylim(lim_total) +
  ggtitle("Violence")

plot_total_p <- ggplot(total_p, aes(x = size, y = Value, color = Method, shape = Method, group = Method)) +
  geom_line(show.legend = FALSE) + 
  geom_point() +
  labs(x = "# Controls", y = "total") + 
  theme_classic() + 
  scale_color_brewer(palette = "Dark2") +
  scale_shape_manual(values = c(1, 2, 3, 4, 5, 6)) + 
  #geom_line(aes(y = 0), color = "grey", linetype = "longdash") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5, face = "bold")) +
  #ylim(lim_total) +
  ggtitle("Property")

plot_total_m <- ggplot(total_m, aes(x = size, y = Value, color = Method, shape = Method, group = Method)) +
  geom_line(show.legend = FALSE) + 
  geom_point() +
  labs(x = "# Controls", y = "total") + 
  theme_classic() +
  scale_color_brewer(palette = "Dark2") +
  scale_shape_manual(values = c(1, 2, 3, 4, 5,6)) + 
  #geom_line(aes(y = 0), color = "grey", linetype = "longdash") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  #ylim(lim_total) +
  ggtitle("Murder")

combined_total <- plot_total_v + plot_total_p + plot_total_m + plot_layout(ncol = 3)





# ----------------------------------------------------------------------------------------------------
# PRINTING
# ----------------------------------------------------------------------------------------------------
print(combined_bias)
print(combined_ratio)
print(combined_coverage)
print(combined_relevant)
print(combined_total)

