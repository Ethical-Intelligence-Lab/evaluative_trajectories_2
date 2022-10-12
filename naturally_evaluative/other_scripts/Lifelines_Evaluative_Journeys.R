# Julian De Freitas and Pechthida Kim
# Lifelines 
# Experiment 4: Evaluative Journeys 

# This code is structured as follows: 
# Define a bunch of functions (imported from Lifelines_Generate_Plots), then call them. 
# Skip to 'main script' section for the flow of calls.  

# Clear workspace
rm(list = ls()) 

# Read in Lifelines_Generate_Plots.R  
source2 <- function(file, start, end, ...) {
  file.lines <- scan(file, what = character(), skip = start - 1, nlines = end - start + 1, sep ='\n')
  file.lines.collapsed <- paste(file.lines, collapse ='\n')
  source(textConnection(file.lines.collapsed), ...)
}

# Set working directory 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #set working directory to current directory
setwd("..") #go one directory up
source2("Lifelines_Generate_Plots.R", 0, 545) #only get the defined functions and standardized features data frame  
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #go back to current directory


## -------------------------------------------------------------------------------------------------------------
                                                # DEFINE FUNCTIONS 
## -------------------------------------------------------------------------------------------------------------


e4_plotter <- function(equation, x_label, y_label, x_range, y_range, x_breaks, x_ticks) {
    "
    What: Refer to plotter() function; adds x-axis tick labels 
    Input: equations to plot, x-axis label, y-axis label, x-axis limits, y-axis limits, 
            x-axis tick breaks, x-axis tick labels  
    Output: A new plotting function 
    "
    
    # Plot equations and nullifies x-axis tick marks; refer to original plotter() function 
    plot(equation, lwd = 7, xlim = x_range, ylim = y_range, main = "", 
         xlab = x_label, ylab = y_label, col = "firebrick3", cex.lab = 1.5, cex.axis = 1.5, 
         xaxt = "n")
    
    # Add tick mark labels on the x-axis 
    axis(1, at = x_breaks, labels = x_ticks, cex.axis = 1.5)   
    
    return(e4_plotter)
}


bitcoin_2021 <- function(x) {
    "
    What: Create equation for simplified graph of bitcoin hashrate in 2021  
    Input: x 
    Output: Equation for simplified graph of bitcoin hashrate in 2021 
    "
    
    # Create piecewise equation 
    (start_age <= x & x < (end_age*3/8))*(50 + (0.5*x)) + 
      ((end_age*3/8) <= x & x < (end_age/2))*(88.853-1.11157^x) + 
      ((end_age/2) <= x & x <= end_age)*(70-1.059194^(-x+108)) 
}


## -------------------------------------------------------------------------------------------------------------
                                                # MAIN SCRIPT
## -------------------------------------------------------------------------------------------------------------


## (1) Define and Save Standardized Features Data Frame 
df <- z_scorer(features)
write.csv(df, 'featuresZ.csv')


## (2) Plot the functions
plot_experiment_figures <- TRUE 
non_eval_cond <- c("non_eval", "Date", "PHash/s") 
eval_cond <- c("eval", "Date", "PHash/s") 

x_tick_breaks <- c(0, 20, 40, 60, 80)
x_tick_labels <- c("Jan '21", "Apr '21", "Jul '21", "Oct '21", "Jan '22")


# Plot individual plots 
for(lab in 1:2) { 
  
  # Set conditions 
  if(lab == 1) { 
    plot_labels <- non_eval_cond} #if non-evaluative condition, then name and label the plots accordingly 
  if(lab == 2) {
    plot_labels <- eval_cond} #ditto for the evaluative condition 

  # Create comprehension and experiment plots based on condition type 
  if(plot_experiment_figures == TRUE) {
    my_comp_equations <- create_comp_equations() 
    for(i in 1:length(my_comp_equations)) {
      png(file = paste0(i, "_comp_", plot_labels[1], ".png"))
      par(mar = c(5, 6, 5, 2)) # par(mar = c(5.1, 4.1, 4.1, 2.1) + 1) 
      sapply(my_comp_equations[i], e4_plotter, plot_labels[2], plot_labels[3], 
             c(start_age, end_age), c(0, end_y_axis), x_tick_breaks, x_tick_labels)
      dev.off()
    }
    my_comp_equations_2 <- create_comp_equations_2()
    for(i in 1:length(my_comp_equations_2)) {
      png(file = paste0(i, "_comp_2_", plot_labels[1], ".png"))
      par(mar = c(5, 6, 5, 2))
      sapply(my_comp_equations_2[i], e4_plotter, plot_labels[2], plot_labels[3], 
             c(start_age, end_age), c(0, end_y_axis), x_tick_breaks, x_tick_labels)
      dev.off()
    }
    my_equations <- create_equations()
    for(i in 1:length(my_equations)) {
      png(file = paste0(i, "_exp_", plot_labels[1], ".png"))
      par(mar = c(5, 6, 5, 2))
      sapply(my_equations[i], e4_plotter, plot_labels[2], plot_labels[3], 
             c(start_age, end_age), c(0, end_y_axis), x_tick_breaks, x_tick_labels)
      dev.off()
    }
    
    # Bitcoin 2021 
    png(file = paste0("bitcoin", "_exp_", plot_labels[1], ".png"))
    par(mar = c(5, 6, 5, 2))
    e4_plotter(bitcoin_2021, plot_labels[2], plot_labels[3], 
               c(start_age, end_age), c(0, end_y_axis), x_tick_breaks, x_tick_labels)
    dev.off()
    
    # Compile array of plots (9 on each page)
    plot_array <- c("comprehension", "comprehension_2", "experiments")
    for(plot in plot_array) {
      pdf(file = paste0(plot, "_", plot_labels[1], ".pdf"))
      par(mfrow = c(3,3))
      if(plot == 'comprehension') {
        par(mar = c(5, 6, 5, 3)) # par(mar = c(6.375, 6, 5.125, 2.625))
        sapply(create_comp_equations(), e4_plotter, plot_labels[2], plot_labels[3], 
               c(start_age, end_age), c(0, end_y_axis), x_tick_breaks, x_tick_labels)
      }
      else if(plot == 'comprehension_2') {
        par(mar = c(5, 6, 5, 3))
        sapply(create_comp_equations_2(), e4_plotter, plot_labels[2], plot_labels[3], 
               c(start_age, end_age), c(0, end_y_axis), x_tick_breaks, x_tick_labels)
      }
      else if(plot == 'experiments') {
        par(mar = c(5, 6, 5, 3))
        sapply(create_equations(), e4_plotter, plot_labels[2], plot_labels[3], 
               c(start_age, end_age), c(0, end_y_axis), x_tick_breaks, x_tick_labels)
      }
      dev.off()
    }
  }
  
  # Move files
  if(lab == 1) {
    dir.create("non_eval_plots") #non-evaluative folder
    non_eval_files <- list.files(pattern = "non_eval.png|non_eval.pdf")
    move_files(files = non_eval_files, destination = "non_eval_plots", overwrite = TRUE)
  }
  
  if(lab == 2) {
    dir.create("eval_plots") #evaluative folder
    eval_files <- list.files(pattern = "eval.png|eval.pdf")
    move_files(files = eval_files, destination = "eval_plots", overwrite = TRUE)
  }
}


## END -------------------------------------------------------------------------------------------------------------------

















































## CODE GRAVEYARD -------------------------------------------------------------------------------------------------------------------


# ## -------------------------------------------------------------------------------------------------------------
# # DEFINE VARIABLES
# ## -------------------------------------------------------------------------------------------------------------
# 
# ## Shrink the functions 
# shrink_by <- 0.3 #shrink all functions so that they can fit to a y-axis of (0, 30) and retain their original "lifelines" shape
# shrink_y_axis <- (shrink_by * end_y_axis)
# 
# 
# ## -------------------------------------------------------------------------------------------------------------
# # DEFINE FUNCTIONS
# ## -------------------------------------------------------------------------------------------------------------
# 
# # Create simple functions we present during initial comprehension checks
# shrink_comp_equations <- function() {
#   check1 <- function(x) {shrink_by * (x)} 
#   check2 <- function(x) {shrink_by * (50-50*(cos((x-20)*0.079)))}
#   check3 <- function(x) {shrink_by * (100/(1+(exp(1))^-(x-70)))}
#   check_equations <- c(check1, check2, check3)
#   
#   return(check_equations)
# }
# 
# # Create more functions that we can present during the second round of 
# # comprehension checks if people fail the first
# shrink_comp_equations_2 <- function() {
#   check4 <- function(x) {shrink_by * (80-x)} 
#   check5 <- function(x) {shrink_by * (50+30*(cos((x-20)*0.079)))}
#   check6 <- function(x) {shrink_by * (100/(1+(exp(1))^(x-70)))}
#   check_equations_2 <- c(check4, check5, check6)
#   
#   return(check_equations_2)
# }
# 
# # Define main graph functions 
# shrink_equations <- function() {
#   linear_rise <- function(x) {shrink_by * (1.25*x)}
#   linear_fall <- function(x) {shrink_by * (100-1.25*x)}
#   linear_low <- function(x) {shrink_by * (0*x+0)}
#   linear_middle <- function(x) {shrink_by * (0*x+50)}
#   linear_high <- function(x) {shrink_by * (0*x+100)}
#   exp_rise_convex <- function(x) {shrink_by * (1.0595^x-1)}
#   exp_fall_convex <- function(x) {shrink_by * (1.0595^(-x+80)-1)}
#   exp_rise_concave <- function(x) {shrink_by * (100-1.0595^(-x+80)+1)}
#   exp_fall_concave <- function(x) {shrink_by * (100-1.0595^x+1)}
#   sin_fr_full <- function(x) {shrink_by * (50+50*(cos(x*0.079)))}
#   sin_fr_partial <- function(x) {shrink_by * (50-50*(sin(x*0.05889)))}
#   sin_rf_full <- function(x) {shrink_by * (50-50*(cos(x*0.079)))}
#   sin_rf_partial <- function(x) {shrink_by * (50+50*(sin(x*0.05889)))}
#   sin_rfr_full <- function(x) {shrink_by * (50-50*(cos(x*0.1185)))}
#   sin_rfr_partial <- function(x) {shrink_by * (50+50*(sin(x*0.0982)))}
#   sin_frf_full <- function(x) {shrink_by * (50+50*(cos(x*0.1185)))}
#   sin_frf_partial <- function(x) {shrink_by * (50-50*(sin(x*0.0982)))}
#   sin_frfr <- function(x) {shrink_by * (50-50*(sin(x*0.1375)))}
#   sin_rfrf <- function(x) {shrink_by * (50+50*(sin(x*0.1375)))} 
#   logistic_rise <- function(x) {shrink_by * (100/(1+(exp(1))^-(x-40)))}
#   logistic_fall <- function(x) {shrink_by * (100-100/(1+(exp(1))^-(x-40)))}
#   positive_change_full <- function(x) {
#     shrink_by * ((x < end_age*3/4)*((-5/3)*(x-60)) +
#                    (end_age*3/4 <= x & x <= end_age)*(100+5*(x-80)))
#   }
#   positive_change_partial <- function(x) {
#     shrink_by * ((x < end_age*3/4)*((-5/3)*(x-60)) +
#                    (end_age*3/4 <= x & x <= end_age)*(50+2.5*(x-80)))
#   }
#   negative_change_full <- function(x) {
#     shrink_by * ((x < end_age*3/4)*(5/3)*x +
#                    (end_age*3/4 <= x & x <= end_age)*(-5*(x-80)))
#   }
#   negative_change_partial <- function(x) {
#     shrink_by * ((x < end_age*3/4)*(5/3)*x +
#                    (end_age*3/4 <= x & x <= end_age)*(-2.5*(x-100)))
#   }
#   linear_rise_sharp_fall <- function(x) {
#     shrink_by * ((x < end_age/2)*(1.25*x) +
#                    (end_age/2 < x & x <= end_age)*(0*x+0))
#   }
#   linear_rise_sharp_fall_exp_rise <- function(x) {
#     shrink_by * ((x < end_age/2)*(1.25*x) +
#                    (end_age/2 < x & x <= end_age)*(1.0606^x-10))
#   }
#   
#   equations <- c(linear_rise, linear_fall, linear_low, linear_middle, linear_high, 
#                  exp_rise_convex, exp_fall_convex, exp_rise_concave, exp_fall_concave, 
#                  sin_fr_full, sin_fr_partial, sin_rf_full, sin_rf_partial, 
#                  sin_rfr_full, sin_rfr_partial, sin_frf_full, sin_frf_partial, 
#                  sin_frfr, sin_rfrf, logistic_rise, logistic_fall, 
#                  positive_change_full, positive_change_partial, negative_change_full, negative_change_partial, 
#                  linear_rise_sharp_fall, linear_rise_sharp_fall_exp_rise)
#   
#   return(equations)
# }






# Multiply function 
# shrink_by <- function(a) { #shrink all functions so that they can fit to a y-axis of (0, 30) and retain their original "lifelines" shape
#   force(a)
#   force(0.3)
#   function(x) {0.3 * a(x)} 
# }

