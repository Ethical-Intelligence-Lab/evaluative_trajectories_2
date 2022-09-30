# Julian De Freitas and Pechthida Kim
# Lifelines 
# Experiment 3: Customer Journeys 

# This code is structured as follows: 
# Define a bunch of functions (imported from Lifelines_Generate_Plots), then call them. 
# Skip to 'main script' section for the flow of calls.  

# Clear workspace
rm(list = ls()) 

# Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #set working directory to current directory

#import libraries
if (!require(pacman)) {install.packages("pacman")}
pacman::p_load('rlang', 'dplyr', 'matrixStats', 'ggplot2', 'ggpubr', 
               'calculus', 'mosaic', 'mosaicCalc', 'magrittr', 'filesstrings')

# Read in Lifelines_Generate_Plots.R  
source2 <- function(file, start, end, ...) {
  file.lines <- scan(file, what = character(), skip = start - 1, nlines = end - start + 1, sep ='\n')
  file.lines.collapsed <- paste(file.lines, collapse ='\n')
  source(textConnection(file.lines.collapsed), ...)
}

# Set working directory 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #set working directoy to current directory
setwd("..") #go one directory up
source2("Lifelines_Generate_Plots.R", 0, 545) #only get the defined functions and standardized features data frame  
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #go back to current directory


## -------------------------------------------------------------------------------------------------------------
                                                # MAIN SCRIPT
## -------------------------------------------------------------------------------------------------------------

## (1) Define and Save Standardized Features Data Frame 
df <- z_scorer(features)
write.csv(df, 'featuresZ.csv')


## (2) Plot the functions
plot_experiment_figures <- TRUE


# Plot individual plots 
if(plot_experiment_figures == TRUE) {
  my_comp_equations <- create_comp_equations()
  for(i in 1:length(my_comp_equations)) {
    png(file = paste0(i,"_comprehension_plots.png", ""))
    sapply(my_comp_equations[i], plotter, "Customer Touchpoint", "Stress", c(start_age, end_age), c(0, end_y_axis))
    dev.off()
  }
  my_comp_equations_2 <- create_comp_equations_2()
  for(i in 1:length(my_comp_equations_2)) {
    png(file = paste0(i,"_comprehension_plots_2.png", ""))
    sapply(my_comp_equations_2[i], plotter, "Customer Touchpoint", "Stress", c(start_age, end_age), c(0, end_y_axis))
    dev.off()
  }
  my_equations <- create_equations()
  for(i in 1:length(my_equations)) {
    png(file = paste0(i,"_experimental_plots.png", ""))
    sapply(my_equations[i], plotter, "Customer Touchpoint", "Happiness", c(start_age, end_age), c(0, end_y_axis))
    dev.off()
  }
  dir.create("customer_journey_plots")
  plot_individuals <- c(list.files(pattern = ".png"))
  file.move(plot_individuals, "customer_journey_plots", overwrite = TRUE)
  file.move('featuresZ.csv', "customer_journey_plots", overwrite = TRUE)
}


# Plot array of plots (9 on each page)
if(plot_experiment_figures == TRUE) {
  plot_array <- c("comprehension", "experiments", "d1", "d2")
  for(plot in plot_array) {
    pdf(file = paste0(plot, "_plots.pdf", ""))
    par(mfrow = c(3,3))
    if(plot == 'comprehension') {
      sapply(create_comp_equations(), plotter, "Customer Touchpoint", "Stress", c(start_age, end_age), c(0, end_y_axis))
    }
    else if(plot == 'experiments') {
      sapply(create_equations(), plotter, "Customer Touchpoint", "Happiness", c(start_age, end_age), c(0, end_y_axis))
    }
    else if(plot == 'd1') {
      sapply(create_D1(), plotter, "", "", c(start_age, end_age), c(-10,10))
    }
    else if(plot == 'd2') {
      sapply(create_D2(), plotter, "", "", c(start_age, end_age), c(-2,2))
    }
    dev.off()
  }
  files <- c("comprehension_plots.pdf", "experiments_plots.pdf", "d1_plots.pdf", "d2_plots.pdf")
  file.move(files, "customer_journey_plots", overwrite = TRUE)
}


# Combine axis labels using ggpubr::ggarrange() 
if(plot_experiment_figures == TRUE) {
  pdf(file = "grid_plots.pdf", 16, 5.5)
  
  par(mar = c(2, 2, 2, 2))
  par(mfrow = c(3,9), omi=c(0.5,0.6,0,0)) 
  
  sapply(create_equations(), plotter, "", "", c(start_age, end_age), c(0, end_y_axis)) 
  mtext("Happiness", side = 2, outer = TRUE, cex = 2.5, line = 1.5, font=1)
  mtext("Customer Touchpoint", side = 1, outer = TRUE, cex = 2.5, line = 2, font=1)
  
  dev.off()
  
  files <- list.files(pattern = c("(.pdf|.png)")) 
  file.move(files, "customer_journey_plots", overwrite = TRUE)
}



## END -------------------------------------------------------------------------------------------------------------------

