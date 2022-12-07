# Julian De Freitas and Pechthida Kim
# This code is structured as follows: 
# Define a bunch of functions (imported from Lifelines_Generate_Plots), then call them. 
# Skip to 'main script' section for the flow of calls.  

# Clear workspace
# rm(list = ls()) 


## Plot functions 
plotter <- function(equation, x_label, y_label, x_range, y_range) {
    #dev.new(width = 8, height = 6, noRStudioGD = TRUE)
    my_plot <- plot(equation, lwd = 30, xlim = x_range, ylim = y_range, main = "",
                    xlab = x_label, ylab = y_label, col = "firebrick3", cex.lab = 1.5, cex.axis = 1.5)

    return(my_plot)
}


# Read in Lifelines_Generate_Plots.R  
source2 <- function(file, start, end, ...) {
  file.lines <- scan(file, what = character(), skip = start - 1, nlines = end - start + 1, sep ='\n')
  file.lines.collapsed <- paste(file.lines, collapse ='\n')
  source(textConnection(file.lines.collapsed), ...)
}

# Set working directory 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #set working directoy to current directory
setwd("..") #go one directory up
source2("../tools/Lifelines_Generate_Plots.R", 0, 545) #only get the defined functions and standardized features data frame
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #go back to current directory


## -------------------------------------------------------------------------------------------------------------
                                                # MAIN SCRIPT
## -------------------------------------------------------------------------------------------------------------

# Z-score
# The diff. features have different scales, so we standardize them to same z scale, and save as featuresZ
z_scorer <- function(data) {
    df <- (data - rowMeans(data)) / (rowSds(as.matrix(data)))[row(data)] ##calculating Z score
    is.nan.data.frame <- function(x) ##replacing NaN values with 0 (zero), part 1
        do.call(cbind, lapply(x, is.nan))

    df[is.nan(df)] <- 0 ##replacing NaN values with 0 (zero), part 2

    return(df)
}

## (2) Plot the functions
plot_experiment_figures <- TRUE


# Plot individual plots 
if(plot_experiment_figures == TRUE) {
  my_comp_equations <- create_comp_equations()
  for(i in 1:length(my_comp_equations)) {
    png(file = paste0(i,"_comprehension_plots.png", ""))
    sapply(my_comp_equations[i], plotter, "Time", "Stress", c(start_age, end_age), c(0, end_y_axis))
    dev.off()
  }
  my_comp_equations_2 <- create_comp_equations_2()
  for(i in 1:length(my_comp_equations_2)) {
    png(file = paste0(i,"_comprehension_plots_2.png", ""))
    sapply(my_comp_equations_2[i], plotter, "Time", "Stress", c(start_age, end_age), c(0, end_y_axis))
    dev.off()
  }
  my_equations <- create_equations()
  for(i in 1:length(my_equations)) {
    png(file = paste0(i,"_experimental_plots.png", ""))
    sapply(my_equations[i], plotter, "Time", "Perceived Performance", c(start_age, end_age), c(0, end_y_axis))
    dev.off()
  }
  dir.create("interview_performance_plots")
  plot_individuals <- c(list.files(pattern = ".png"))
  file.move(plot_individuals, "interview_performance_plots", overwrite = TRUE)
}


# Plot array of plots (9 on each page)
if(plot_experiment_figures == TRUE) {
  plot_array <- c("comprehension", "experiments", "d1", "d2")
  for(plot in plot_array) {
    pdf(file = paste0(plot, "_plots.pdf", ""))
    par(mfrow = c(3,3))
    if(plot == 'comprehension') {
      sapply(create_comp_equations(), plotter, "Time", "Stress", c(start_age, end_age), c(0, end_y_axis))
    }
    else if(plot == 'experiments') {
      sapply(create_equations(), plotter, "Time", "Perceived Performance", c(start_age, end_age), c(0, end_y_axis))
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
  file.move(files, "interview_performance_plots", overwrite = TRUE)
}


# Combine axis labels using ggpubr::ggarrange() 
if(plot_experiment_figures == TRUE) {
  pdf(file = "grid_plots.pdf", 16, 5.5)
  
  par(mar = c(2, 2, 2, 2))
  par(mfrow = c(3,9), omi=c(0.5,0.6,0,0)) 
  
  sapply(create_equations(), plotter, "", "", c(start_age, end_age), c(0, end_y_axis)) 
  mtext("Perceived Performance", side = 2, outer = TRUE, cex = 2.5, line = 1.5, font=1)
  mtext("Time", side = 1, outer = TRUE, cex = 2.5, line = 2, font=1)
  
  dev.off()
  
  files <- list.files(pattern = c("(.pdf|.png)")) 
  file.move(files, "interview_performance_plots", overwrite = TRUE)
}



## END -------------------------------------------------------------------------------------------------------------------

