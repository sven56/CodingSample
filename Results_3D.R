# ----------------------------------------------------------------------------------------------------
# Sourcing
# ----------------------------------------------------------------------------------------------------
setwd("/Users/svenvanholtencharria/Documents/Econometrics Erasmus/Year 5/Thesis/Sven Code/")
library(dplyr)
library(tibble)
library(tidyverse)
library(ggplot2)
library(plotly)
library(magick)
library(R.matlab)
methods = c("control","binom","score","adaptive", "unif")
path = c("Results/Simulation 4 - hetero/")
mapping <- c(`0` = 1, `0.2` = 2, `0.4` = 3, `0.6` = 4, `0.8` = 5)
errors = "hetero"

# ----------------------------------------------------------------------------------------------------
# Simulation 4
# ----------------------------------------------------------------------------------------------------
### Select filenames
names <- list.files(path) %>%
  .[grepl(".csv", .)] %>%
  substr(., 1, nchar(.) - 4)



# ----------------------------------------------------------------------------------------------------
# matCreate: creates a 3D matrix, based on column (method) of accuracy matrix
# ----------------------------------------------------------------------------------------------------
matCreate <- function(methodVec) {
  # init matrix
  mat <- matrix(nrow = 5, ncol = 5)

  #over all rows
  for (r in 1:nrow(methodVec) ) {
    # Extract coords
    coords = rownames(methodVec)[r]%>%
      str_extract_all(., "\\d+(\\.\\d+)?")

    # Parse coords into matrix coordinates
    R21 = mapping[coords[[1]][1]]
    R22 = mapping[coords[[1]][2]]

    # Fill into matrix
    mat[R21,R22] = methodVec[[1]][r]
  }
  return(mat)
}





# ----------------------------------------------------------------------------------------------------
# Source data into matrices
# ----------------------------------------------------------------------------------------------------
bias_m = data.frame(matrix(ncol = length(methods), nrow = 75, dimnames= list(names, methods)))
coverage_m = data.frame(matrix(ncol = length(methods), nrow = 75, dimnames= list(names, methods)))
relevant_m = data.frame(matrix(ncol = length(methods), nrow = 75, dimnames= list(names, methods)))
total_m = data.frame(matrix(ncol = length(methods), nrow = 75, dimnames= list(names, methods)))
for (i in 1:length(names)) {
  file_name = names[i]
  temp = read.csv(paste(path,file_name, ".csv" , sep=""), row.names = 1)
  bias_m[i,] = temp[1,methods]
  coverage_m[i,] = temp[3,methods]
  relevant_m[i,] = temp[4,methods]
  total_m[i,] = temp[5,methods]
}



# createPlot <- function(z, mat) {
#   # Init stuff
#   x = y = c(0, 0.2, 0.4, 0.6, 0.8)
#   camera <- list(
#     eye = list(x = -1.25, y = -1.25, z = 3)  # Adjust these values to change the camera angle
#   )
# 
#   z_lim = c(min(abs(unlist(mat[-1]))), max(abs(unlist(mat[-1]))))
# 
#   output_plot = plot_ly() %>%
#     add_surface(
#       x = ~x,
#       y = ~y,
#       z = ~abs(z), #absolute values
#       colorscale = 'Portland',  # Choose an appropriate colorscale
#       showscale = FALSE,
#       contours = list(
#         x = list(show = TRUE, color = "black", width = 1),
#         y = list(show = TRUE, color = "black", width = 1),
#         z = list(show = FALSE, color = "black", width = 1)
#       )  ) %>%
#     layout(
#       title = title,
#       scene = list(
#         xaxis = list(title = "Second Stage R2"),
#         yaxis = list(title = "First Stage R2"),
#         zaxis = list(title="", range = z_lim),
#         camera = camera
#       )
# 
#     )
# 
#   return(output_plot)
# }





# ----------------------------------------------------------------------------------------------------
# Low Precision
# ----------------------------------------------------------------------------------------------------
bias_low <- bias_m %>%
  filter(grepl("low",rownames(.))) %>%
  list( "control" = matCreate(.[1]),
    "binom" = matCreate(.[2]),
    "score" = matCreate(.[3]),
    "adaptive" = matCreate(.[4]),
    "unif" = matCreate(.[5]))

# createPlot(bias_low$control, bias_low)
# createPlot(bias_low$binom, bias_low)
# createPlot(bias_low$score, bias_low)

# createPlot(bias_low$control, bias_low) %>%
#   save_image(file = paste("plots/Sim 4 - ",errors,"/",errors,"_low_bias_control.png", sep =""), scale = 2 ) %>%
#   image_read(.) %>%
#   image_trim(.) %>%
#   image_write(., paste("plots/Sim 4 - ",errors,"/",errors,"_low_bias_control.png", sep =""))
# 
# createPlot(bias_low$binom, bias_low) %>%
#   save_image(file = paste("plots/Sim 4 - ",errors,"/",errors,"_low_bias_binom.png", sep =""), scale = 2 ) %>%
#   image_read(.) %>%
#   image_trim(.) %>%
#   image_write(., paste("plots/Sim 4 - ",errors,"/",errors,"_low_bias_binom.png", sep =""))
# 
# createPlot(bias_low$score, bias_low) %>%
#   save_image(file = paste("plots/Sim 4 - ",errors,"/",errors,"_low_bias_score.png", sep =""), scale = 2 ) %>%
#   image_read(.) %>%
#   image_trim(.) %>%
#   image_write(., paste("plots/Sim 4 - ",errors,"/",errors,"_low_bias_score.png", sep =""))
# 
# createPlot(bias_low$adaptive, bias_low) %>%
#   save_image(file = paste("plots/Sim 4 - ",errors,"/",errors,"_low_bias_adaptive.png", sep =""), scale = 2 ) %>%
#   image_read(.) %>%
#   image_trim(.) %>%
#   image_write(., paste("plots/Sim 4 - ",errors,"/",errors,"_low_bias_adaptive.png", sep =""))
# 
# createPlot(bias_low$unif, bias_low) %>%
#   save_image(file = paste("plots/Sim 4 - ",errors,"/",errors,"_low_bias_unif.png", sep =""), scale = 2 ) %>%
#   image_read(.) %>%
#   image_trim(.) %>%
#   image_write(., paste("plots/Sim 4 - ",errors,"/",errors,"_low_bias_unif.png", sep =""))
# 

coverage_low <- coverage_m %>%
  filter(grepl("low",rownames(.))) %>%
  list( "control" = matCreate(.[1]),
        "binom" = matCreate(.[2]),
        "score" = matCreate(.[3]),
        "adaptive" = matCreate(.[4]),
        "unif" = matCreate(.[5]))

# createPlot(coverage_low$control)
# createPlot(coverage_low$binom)
# createPlot(coverage_low$score)
# 
# createPlot(coverage_low$control, coverage_low) %>%
#   save_image(file = paste("plots/Sim 4 - ",errors,"/",errors,"_low_coverage_control.png", sep =""), scale = 2 ) %>%
#   image_read(.) %>%
#   image_trim(.) %>%
#   image_write(., paste("plots/Sim 4 - ",errors,"/",errors,"_low_coverage_control.png", sep =""))
# 
# createPlot(coverage_low$binom, coverage_low) %>%
#   save_image(file = paste("plots/Sim 4 - ",errors,"/",errors,"_low_coverage_binom.png", sep =""), scale = 2 ) %>%
#   image_read(.) %>%
#   image_trim(.) %>%
#   image_write(., paste("plots/Sim 4 - ",errors,"/",errors,"_low_coverage_binom.png", sep =""))
# 
# createPlot(coverage_low$score, coverage_low) %>%
#   save_image(file = paste("plots/Sim 4 - ",errors,"/",errors,"_low_coverage_score.png", sep =""), scale = 2 ) %>%
#   image_read(.) %>%
#   image_trim(.) %>%
#   image_write(., paste("plots/Sim 4 - ",errors,"/",errors,"_low_coverage_score.png", sep =""))
# 
# createPlot(coverage_low$adaptive, coverage_low) %>%
#   save_image(file = paste("plots/Sim 4 - ",errors,"/",errors,"_low_coverage_adaptive.png", sep =""), scale = 2 ) %>%
#   image_read(.) %>%
#   image_trim(.) %>%
#   image_write(., paste("plots/Sim 4 - ",errors,"/",errors,"_low_coverage_adaptive.png", sep =""))
# 
# createPlot(coverage_low$unif, coverage_low) %>%
#   save_image(file = paste("plots/Sim 4 - ",errors,"/",errors,"_low_coverage_unif.png", sep =""), scale = 2 ) %>%
#   image_read(.) %>%
#   image_trim(.) %>%
#   image_write(., paste("plots/Sim 4 - ",errors,"/",errors,"_low_coverage_unif.png", sep =""))
# 


relevant_low <- relevant_m %>%
  filter(grepl("low",rownames(.))) %>%
  list( "control" = matCreate(.[1]),
        "binom" = matCreate(.[2]),
        "score" = matCreate(.[3]),
        "adaptive" = matCreate(.[4]),
        "unif" = matCreate(.[5]))

# createPlot(relevant_low$control)
# createPlot(relevant_low$binom)
# createPlot(relevant_low$score)
# 
# createPlot(relevant_low$control, relevant_low) %>%
#   save_image(file = paste("plots/Sim 4 - ",errors,"/",errors,"_low_relevant_control.png", sep =""), scale = 2 ) %>%
#   image_read(.) %>%
#   image_trim(.) %>%
#   image_write(., paste("plots/Sim 4 - ",errors,"/",errors,"_low_relevant_control.png", sep =""))
# 
# createPlot(relevant_low$binom, relevant_low) %>%
#   save_image(file = paste("plots/Sim 4 - ",errors,"/",errors,"_low_relevant_binom.png", sep =""), scale = 2 ) %>%
#   image_read(.) %>%
#   image_trim(.) %>%
#   image_write(., paste("plots/Sim 4 - ",errors,"/",errors,"_low_relevant_binom.png", sep =""))
# 
# createPlot(relevant_low$score, relevant_low) %>%
#   save_image(file = paste("plots/Sim 4 - ",errors,"/",errors,"_low_relevant_score.png", sep =""), scale = 2 ) %>%
#   image_read(.) %>%
#   image_trim(.) %>%
#   image_write(., paste("plots/Sim 4 - ",errors,"/",errors,"_low_relevant_score.png", sep =""))
# 
# createPlot(relevant_low$adaptive, relevant_low) %>%
#   save_image(file = paste("plots/Sim 4 - ",errors,"/",errors,"_low_relevant_adaptive.png", sep =""), scale = 2 ) %>%
#   image_read(.) %>%
#   image_trim(.) %>%
#   image_write(., paste("plots/Sim 4 - ",errors,"/",errors,"_low_relevant_adaptive.png", sep =""))
# 
# createPlot(relevant_low$unif, relevant_low) %>%
#   save_image(file = paste("plots/Sim 4 - ",errors,"/",errors,"_low_relevant_unif.png", sep =""), scale = 2 ) %>%
#   image_read(.) %>%
#   image_trim(.) %>%
#   image_write(., paste("plots/Sim 4 - ",errors,"/",errors,"_low_relevant_unif.png", sep =""))
# 
# 

total_low <- total_m %>%
  filter(grepl("low",rownames(.))) %>%
  list( "control" = matCreate(.[1]),
        "binom" = matCreate(.[2]),
        "score" = matCreate(.[3]),
        "adaptive" = matCreate(.[4]),
        "unif" = matCreate(.[5]))

# createPlot(total_low$control)
# createPlot(total_low$binom)
# createPlot(total_low$score)
# 
# createPlot(total_low$control, total_low) %>%
#   save_image(file = paste("plots/Sim 4 - ",errors,"/",errors,"_low_total_control.png", sep =""), scale = 2 ) %>%
#   image_read(.) %>%
#   image_trim(.) %>%
#   image_write(., paste("plots/Sim 4 - ",errors,"/",errors,"_low_total_control.png", sep =""))
# 
# createPlot(total_low$binom, total_low) %>%
#   save_image(file = paste("plots/Sim 4 - ",errors,"/",errors,"_low_total_binom.png", sep =""), scale = 2 ) %>%
#   image_read(.) %>%
#   image_trim(.) %>%
#   image_write(., paste("plots/Sim 4 - ",errors,"/",errors,"_low_total_binom.png", sep =""))
# 
# createPlot(total_low$score, total_low) %>%
#   save_image(file = paste("plots/Sim 4 - ",errors,"/",errors,"_low_total_score.png", sep =""), scale = 2 ) %>%
#   image_read(.) %>%
#   image_trim(.) %>%
#   image_write(., paste("plots/Sim 4 - ",errors,"/",errors,"_low_total_score.png", sep =""))
# 
# createPlot(total_low$adaptive, total_low) %>%
#   save_image(file = paste("plots/Sim 4 - ",errors,"/",errors,"_low_total_adaptive.png", sep =""), scale = 2 ) %>%
#   image_read(.) %>%
#   image_trim(.) %>%
#   image_write(., paste("plots/Sim 4 - ",errors,"/",errors,"_low_total_adaptive.png", sep =""))
# 
# createPlot(total_low$unif, total_low) %>%
#   save_image(file = paste("plots/Sim 4 - ",errors,"/",errors,"_low_relevant_unif.png", sep =""), scale = 2 ) %>%
#   image_read(.) %>%
#   image_trim(.) %>%
#   image_write(., paste("plots/Sim 4 - ",errors,"/",errors,"_low_relevant_unif.png", sep =""))
# 


# ----------------------------------------------------------------------------------------------------
# Medium Precision
# ----------------------------------------------------------------------------------------------------
bias_medium <- bias_m %>%
  filter(grepl("medium",rownames(.))) %>%
  list( "control" = matCreate(.[1]),
        "binom" = matCreate(.[2]),
        "score" = matCreate(.[3]),
        "adaptive" = matCreate(.[4]),
        "unif" = matCreate(.[5]))

# createPlot(bias_medium$control)
# createPlot(bias_medium$binom)
# createPlot(bias_medium$score)
# 
# createPlot(bias_medium$control, bias_medium) %>%
#   save_image(file = paste("plots/Sim 4 - ",errors,"/",errors,"_medium_bias_control.png", sep =""), scale = 2 ) %>%
#   image_read(.) %>%
#   image_trim(.) %>%
#   image_write(., paste("plots/Sim 4 - ",errors,"/",errors,"_medium_bias_control.png", sep =""))
# 
# createPlot(bias_medium$binom, bias_medium) %>%
#   save_image(file = paste("plots/Sim 4 - ",errors,"/",errors,"_medium_bias_binom.png", sep =""), scale = 2 ) %>%
#   image_read(.) %>%
#   image_trim(.) %>%
#   image_write(., paste("plots/Sim 4 - ",errors,"/",errors,"_medium_bias_binom.png", sep =""))
# 
# createPlot(bias_medium$score, bias_medium) %>%
#   save_image(file = paste("plots/Sim 4 - ",errors,"/",errors,"_medium_bias_score.png", sep =""), scale = 2 ) %>%
#   image_read(.) %>%
#   image_trim(.) %>%
#   image_write(., paste("plots/Sim 4 - ",errors,"/",errors,"_medium_bias_score.png", sep =""))
# 
# createPlot(bias_medium$adaptive, bias_medium) %>%
#   save_image(file = paste("plots/Sim 4 - ",errors,"/",errors,"_medium_bias_adaptive.png", sep =""), scale = 2 ) %>%
#   image_read(.) %>%
#   image_trim(.) %>%
#   image_write(., paste("plots/Sim 4 - ",errors,"/",errors,"_medium_bias_adaptive.png", sep =""))
# 
# createPlot(bias_medium$unif, bias_medium) %>%
#   save_image(file = paste("plots/Sim 4 - ",errors,"/",errors,"_medium_bias_unif.png", sep =""), scale = 2 ) %>%
#   image_read(.) %>%
#   image_trim(.) %>%
#   image_write(., paste("plots/Sim 4 - ",errors,"/",errors,"_medium_bias_unif.png", sep =""))


coverage_medium <- coverage_m %>%
  filter(grepl("medium",rownames(.))) %>%
  list( "control" = matCreate(.[1]),
        "binom" = matCreate(.[2]),
        "score" = matCreate(.[3]),
        "adaptive" = matCreate(.[4]),
        "unif" = matCreate(.[5]))

# createPlot(coverage_medium$control)
# createPlot(coverage_medium$binom)
# createPlot(coverage_medium$score)
# 
# createPlot(coverage_medium$control, coverage_medium) %>%
#   save_image(file = paste("plots/Sim 4 - ",errors,"/",errors,"_medium_coverage_control.png", sep =""), scale = 2 ) %>%
#   image_read(.) %>%
#   image_trim(.) %>%
#   image_write(., paste("plots/Sim 4 - ",errors,"/",errors,"_medium_coverage_control.png", sep =""))
# 
# createPlot(coverage_medium$binom, coverage_medium) %>%
#   save_image(file = paste("plots/Sim 4 - ",errors,"/",errors,"_medium_coverage_binom.png", sep =""), scale = 2 ) %>%
#   image_read(.) %>%
#   image_trim(.) %>%
#   image_write(., paste("plots/Sim 4 - ",errors,"/",errors,"_medium_coverage_binom.png", sep =""))
# 
# createPlot(coverage_medium$score, coverage_medium) %>%
#   save_image(file = paste("plots/Sim 4 - ",errors,"/",errors,"_medium_coverage_score.png", sep =""), scale = 2 ) %>%
#   image_read(.) %>%
#   image_trim(.) %>%
#   image_write(., paste("plots/Sim 4 - ",errors,"/",errors,"_medium_coverage_score.png", sep =""))
# 
# createPlot(coverage_medium$adaptive, coverage_medium) %>%
#   save_image(file = paste("plots/Sim 4 - ",errors,"/",errors,"_medium_coverage_adaptive.png", sep =""), scale = 2 ) %>%
#   image_read(.) %>%
#   image_trim(.) %>%
#   image_write(., paste("plots/Sim 4 - ",errors,"/",errors,"_medium_coverage_adaptive.png", sep =""))
# 
# createPlot(coverage_medium$unif, coverage_medium) %>%
#   save_image(file = paste("plots/Sim 4 - ",errors,"/",errors,"_medium_coverage_unif.png", sep =""), scale = 2 ) %>%
#   image_read(.) %>%
#   image_trim(.) %>%
#   image_write(., paste("plots/Sim 4 - ",errors,"/",errors,"_medium_coverage_unif.png", sep =""))
# 


relevant_medium <- relevant_m %>%
  filter(grepl("medium",rownames(.))) %>%
  list( "control" = matCreate(.[1]),
        "binom" = matCreate(.[2]),
        "score" = matCreate(.[3]),
        "adaptive" = matCreate(.[4]),
        "unif" = matCreate(.[5]))

# createPlot(relevant_medium$control)
# createPlot(relevant_medium$binom)
# createPlot(relevant_medium$score)
# 
# createPlot(relevant_medium$control, relevant_medium) %>%
#   save_image(file = paste("plots/Sim 4 - ",errors,"/",errors,"_medium_relevant_control.png", sep =""), scale = 2 ) %>%
#   image_read(.) %>%
#   image_trim(.) %>%
#   image_write(., paste("plots/Sim 4 - ",errors,"/",errors,"_medium_relevant_control.png", sep =""))
# 
# createPlot(relevant_medium$binom, relevant_medium) %>%
#   save_image(file = paste("plots/Sim 4 - ",errors,"/",errors,"_medium_relevant_binom.png", sep =""), scale = 2 ) %>%
#   image_read(.) %>%
#   image_trim(.) %>%
#   image_write(., paste("plots/Sim 4 - ",errors,"/",errors,"_medium_relevant_binom.png", sep =""))
# 
# createPlot(relevant_medium$score, relevant_medium) %>%
#   save_image(file = paste("plots/Sim 4 - ",errors,"/",errors,"_medium_relevant_score.png", sep =""), scale = 2 ) %>%
#   image_read(.) %>%
#   image_trim(.) %>%
#   image_write(., paste("plots/Sim 4 - ",errors,"/",errors,"_medium_relevant_score.png", sep =""))
# 
# createPlot(relevant_medium$adaptive, relevant_medium) %>%
#   save_image(file = paste("plots/Sim 4 - ",errors,"/",errors,"_medium_relevant_adaptive.png", sep =""), scale = 2 ) %>%
#   image_read(.) %>%
#   image_trim(.) %>%
#   image_write(., paste("plots/Sim 4 - ",errors,"/",errors,"_medium_relevant_adaptive.png", sep =""))
# 
# createPlot(relevant_medium$unif, relevant_medium) %>%
#   save_image(file = paste("plots/Sim 4 - ",errors,"/",errors,"_medium_relevant_unif.png", sep =""), scale = 2 ) %>%
#   image_read(.) %>%
#   image_trim(.) %>%
#   image_write(., paste("plots/Sim 4 - ",errors,"/",errors,"_medium_relevant_unif.png", sep =""))
# 
# 

total_medium <- total_m %>%
  filter(grepl("medium",rownames(.))) %>%
  list( "control" = matCreate(.[1]),
        "binom" = matCreate(.[2]),
        "score" = matCreate(.[3]),
        "adaptive" = matCreate(.[4]),
        "unif" = matCreate(.[5]))

# createPlot(total_medium$control)
# createPlot(total_medium$binom)
# createPlot(total_medium$score)
# 
# createPlot(total_medium$control, total_medium) %>%
#   save_image(file = paste("plots/Sim 4 - ",errors,"/",errors,"_medium_total_control.png", sep =""), scale = 2 ) %>%
#   image_read(.) %>%
#   image_trim(.) %>%
#   image_write(., paste("plots/Sim 4 - ",errors,"/",errors,"_medium_total_control.png", sep =""))
# 
# createPlot(total_medium$binom, total_medium) %>%
#   save_image(file = paste("plots/Sim 4 - ",errors,"/",errors,"_medium_total_binom.png", sep =""), scale = 2 ) %>%
#   image_read(.) %>%
#   image_trim(.) %>%
#   image_write(., paste("plots/Sim 4 - ",errors,"/",errors,"_medium_total_binom.png", sep =""))
# 
# createPlot(total_medium$score, total_medium) %>%
#   save_image(file = paste("plots/Sim 4 - ",errors,"/",errors,"_medium_total_score.png", sep =""), scale = 2 ) %>%
#   image_read(.) %>%
#   image_trim(.) %>%
#   image_write(., paste("plots/Sim 4 - ",errors,"/",errors,"_medium_total_score.png", sep =""))
# 
# createPlot(total_medium$adaptive, total_medium) %>%
#   save_image(file = paste("plots/Sim 4 - ",errors,"/",errors,"_medium_total_adaptive.png", sep =""), scale = 2 ) %>%
#   image_read(.) %>%
#   image_trim(.) %>%
#   image_write(., paste("plots/Sim 4 - ",errors,"/",errors,"_medium_total_adaptive.png", sep =""))
# 
# createPlot(total_medium$unif, total_medium) %>%
#   save_image(file = paste("plots/Sim 4 - ",errors,"/",errors,"_medium_relevant_unif.png", sep =""), scale = 2 ) %>%
#   image_read(.) %>%
#   image_trim(.) %>%
#   image_write(., paste("plots/Sim 4 - ",errors,"/",errors,"_medium_relevant_unif.png", sep =""))
# 
# 



# ----------------------------------------------------------------------------------------------------
# High Precision
# ----------------------------------------------------------------------------------------------------
bias_high <- bias_m %>%
  filter(grepl("high",rownames(.))) %>%
  list( "control" = matCreate(.[1]),
        "binom" = matCreate(.[2]),
        "score" = matCreate(.[3]),
        "adaptive" = matCreate(.[4]),
        "unif" = matCreate(.[5]))

# createPlot(bias_high$control)
# createPlot(bias_high$binom)
# createPlot(bias_high$score)
# 
# createPlot(bias_high$control, bias_high) %>%
#   save_image(file = paste("plots/Sim 4 - ",errors,"/",errors,"_high_bias_control.png", sep =""), scale = 2 ) %>%
#   image_read(.) %>%
#   image_trim(.) %>%
#   image_write(., paste("plots/Sim 4 - ",errors,"/",errors,"_high_bias_control.png", sep =""))
# 
# createPlot(bias_high$binom, bias_high) %>%
#   save_image(file = paste("plots/Sim 4 - ",errors,"/",errors,"_high_bias_binom.png", sep =""), scale = 2 ) %>%
#   image_read(.) %>%
#   image_trim(.) %>%
#   image_write(., paste("plots/Sim 4 - ",errors,"/",errors,"_high_bias_binom.png", sep =""))
# 
# createPlot(bias_high$score, bias_high) %>%
#   save_image(file = paste("plots/Sim 4 - ",errors,"/",errors,"_high_bias_score.png", sep =""), scale = 2 ) %>%
#   image_read(.) %>%
#   image_trim(.) %>%
#   image_write(., paste("plots/Sim 4 - ",errors,"/",errors,"_high_bias_score.png", sep =""))
# 
# createPlot(bias_high$adaptive, bias_high) %>%
#   save_image(file = paste("plots/Sim 4 - ",errors,"/",errors,"_high_bias_adaptive.png", sep =""), scale = 2 ) %>%
#   image_read(.) %>%
#   image_trim(.) %>%
#   image_write(., paste("plots/Sim 4 - ",errors,"/",errors,"_high_bias_adaptive.png", sep =""))
# 
# createPlot(bias_high$unif, bias_high) %>%
#   save_image(file = paste("plots/Sim 4 - ",errors,"/",errors,"_high_bias_unif.png", sep =""), scale = 2 ) %>%
#   image_read(.) %>%
#   image_trim(.) %>%
#   image_write(., paste("plots/Sim 4 - ",errors,"/",errors,"_high_bias_unif.png", sep =""))



coverage_high <- coverage_m %>%
  filter(grepl("high",rownames(.))) %>%
  list( "control" = matCreate(.[1]),
        "binom" = matCreate(.[2]),
        "score" = matCreate(.[3]),
        "adaptive" = matCreate(.[4]),
        "unif" = matCreate(.[5]))

# createPlot(coverage_high$control)
# createPlot(coverage_high$binom)
# createPlot(coverage_high$score)
# 
# createPlot(coverage_high$control, coverage_high) %>%
#   save_image(file = paste("plots/Sim 4 - ",errors,"/",errors,"_high_coverage_control.png", sep =""), scale = 2 ) %>%
#   image_read(.) %>%
#   image_trim(.) %>%
#   image_write(., paste("plots/Sim 4 - ",errors,"/",errors,"_high_coverage_control.png", sep =""))
# 
# createPlot(coverage_high$binom, coverage_high) %>%
#   save_image(file = paste("plots/Sim 4 - ",errors,"/",errors,"_high_coverage_binom.png", sep =""), scale = 2 ) %>%
#   image_read(.) %>%
#   image_trim(.) %>%
#   image_write(., paste("plots/Sim 4 - ",errors,"/",errors,"_high_coverage_binom.png", sep =""))
# 
# createPlot(coverage_high$score, coverage_high) %>%
#   save_image(file = paste("plots/Sim 4 - ",errors,"/",errors,"_high_coverage_score.png", sep =""), scale = 2 ) %>%
#   image_read(.) %>%
#   image_trim(.) %>%
#   image_write(., paste("plots/Sim 4 - ",errors,"/",errors,"_high_coverage_score.png", sep =""))
# 
# createPlot(coverage_high$adaptive, coverage_high) %>%
#   save_image(file = paste("plots/Sim 4 - ",errors,"/",errors,"_high_coverage_adaptive.png", sep =""), scale = 2 ) %>%
#   image_read(.) %>%
#   image_trim(.) %>%
#   image_write(., paste("plots/Sim 4 - ",errors,"/",errors,"_high_coverage_adaptive.png", sep =""))
# 
# createPlot(coverage_high$unif, coverage_high) %>%
#   save_image(file = paste("plots/Sim 4 - ",errors,"/",errors,"_high_coverage_unif.png", sep =""), scale = 2 ) %>%
#   image_read(.) %>%
#   image_trim(.) %>%
#   image_write(., paste("plots/Sim 4 - ",errors,"/",errors,"_high_coverage_unif.png", sep =""))
# 


relevant_high <- relevant_m %>%
  filter(grepl("high",rownames(.))) %>%
  list( "control" = matCreate(.[1]),
        "binom" = matCreate(.[2]),
        "score" = matCreate(.[3]),
        "adaptive" = matCreate(.[4]),
        "unif" = matCreate(.[5]))

# createPlot(relevant_high$control)
# createPlot(relevant_high$binom)
# createPlot(relevant_high$score)
# 
# createPlot(relevant_high$control, relevant_high) %>%
#   save_image(file = paste("plots/Sim 4 - ",errors,"/",errors,"_high_relevant_control.png", sep =""), scale = 2 ) %>%
#   image_read(.) %>%
#   image_trim(.) %>%
#   image_write(., paste("plots/Sim 4 - ",errors,"/",errors,"_high_relevant_control.png", sep =""))
# 
# createPlot(relevant_high$binom, relevant_high) %>%
#   save_image(file = paste("plots/Sim 4 - ",errors,"/",errors,"_high_relevant_binom.png", sep =""), scale = 2 ) %>%
#   image_read(.) %>%
#   image_trim(.) %>%
#   image_write(., paste("plots/Sim 4 - ",errors,"/",errors,"_high_relevant_binom.png", sep =""))
# 
# createPlot(relevant_high$score, relevant_high) %>%
#   save_image(file = paste("plots/Sim 4 - ",errors,"/",errors,"_high_relevant_score.png", sep =""), scale = 2 ) %>%
#   image_read(.) %>%
#   image_trim(.) %>%
#   image_write(., paste("plots/Sim 4 - ",errors,"/",errors,"_high_relevant_score.png", sep =""))
# 
# createPlot(relevant_high$adaptive, relevant_high) %>%
#   save_image(file = paste("plots/Sim 4 - ",errors,"/",errors,"_high_relevant_adaptive.png", sep =""), scale = 2 ) %>%
#   image_read(.) %>%
#   image_trim(.) %>%
#   image_write(., paste("plots/Sim 4 - ",errors,"/",errors,"_high_relevant_adaptive.png", sep =""))
# 
# createPlot(relevant_high$unif, relevant_high) %>%
#   save_image(file = paste("plots/Sim 4 - ",errors,"/",errors,"_high_relevant_unif.png", sep =""), scale = 2 ) %>%
#   image_read(.) %>%
#   image_trim(.) %>%
#   image_write(., paste("plots/Sim 4 - ",errors,"/",errors,"_high_relevant_unif.png", sep =""))
# 


total_high <- total_m %>%
  filter(grepl("high",rownames(.))) %>%
  list( "control" = matCreate(.[1]),
        "binom" = matCreate(.[2]),
        "score" = matCreate(.[3]),
        "adaptive" = matCreate(.[4]),
        "unif" = matCreate(.[5]))

# createPlot(total_high$control)
# createPlot(total_high$binom)
# createPlot(total_high$score)
# 
# createPlot(total_high$control, total_high) %>%
#   save_image(file = paste("plots/Sim 4 - ",errors,"/",errors,"_high_total_control.png", sep =""), scale = 2 ) %>%
#   image_read(.) %>%
#   image_trim(.) %>%
#   image_write(., paste("plots/Sim 4 - ",errors,"/",errors,"_high_total_control.png", sep =""))
# 
# createPlot(total_high$binom, total_high) %>%
#   save_image(file = paste("plots/Sim 4 - ",errors,"/",errors,"_high_total_binom.png", sep =""), scale = 2 ) %>%
#   image_read(.) %>%
#   image_trim(.) %>%
#   image_write(., paste("plots/Sim 4 - ",errors,"/",errors,"_high_total_binom.png", sep =""))
# 
# createPlot(total_high$score, total_high) %>%
#   save_image(file = paste("plots/Sim 4 - ",errors,"/",errors,"_high_total_score.png", sep =""), scale = 2 ) %>%
#   image_read(.) %>%
#   image_trim(.) %>%
#   image_write(., paste("plots/Sim 4 - ",errors,"/",errors,"_high_total_score.png", sep =""))
# 
# createPlot(total_high$adaptive, total_high) %>%
#   save_image(file = paste("plots/Sim 4 - ",errors,"/",errors,"_high_total_adaptive.png", sep =""), scale = 2 ) %>%
#   image_read(.) %>%
#   image_trim(.) %>%
#   image_write(., paste("plots/Sim 4 - ",errors,"/",errors,"_high_total_adaptive.png", sep =""))
# 
# createPlot(total_high$unif, total_high) %>%
#   save_image(file = paste("plots/Sim 4 - ",errors,"/",errors,"_high_relevant_unif.png", sep =""), scale = 2 ) %>%
#   image_read(.) %>%
#   image_trim(.) %>%
#   image_write(., paste("plots/Sim 4 - ",errors,"/",errors,"_high_relevant_unif.png", sep =""))
# 
# 
# 

