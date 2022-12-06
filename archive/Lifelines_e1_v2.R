#Julian De Freitas and Pechthida Kim
#Lifelines

if (!require(pacman)) {install.packages("pacman")}
pacman::p_load('rlang', 'dplyr', 'matrixStats', 'ggplot2',
               'calculus', 'mosaic', 'mosaicCalc', 'inflection', 'magrittr')

## ---------------------------------------------------------------------------------------------------------
## ABBREVIATIONS ---------------------------------------------------------------------------------------------------------
## ---------------------------------------------------------------------------------------------------------

##e.g., 
##FRFR = Fall, Rise, Fall, Rise

## -------------------------------------------------------------------------------------------------------------------
## DEFINE ALL FUNCTIONS -------------------------------------------------------------------------------------------
## -------------------------------------------------------------------------------------------------------------

#define feature array
feature_names <- c("max", "min", "number_peaks", "number_valleys", "number_extrema", "integral", "d1_sum_unweight", "d1_sum_weight_prime",
                   "d1_sum_weight_asc", "d1_sum_weight_des", "d1_sum_weight_end", "d1_avg_unweight", "d1_avg_weight_prime",
                   "d1_avg_weight_asc", "d1_avg_weight_des", "d1_avg_weight_end", "d2_sum_unweight", "d2_sum_weight_prime",
                   "d2_sum_weight_asc", "d2_sum_weight_des", "d2_sum_weight_end", "d2_avg_unweight", "d2_avg_weight_prime",
                   "d2_avg_weight_asc", "d2_avg_weight_des", "d2_avg_weight_end")

plot_names <- c("linear_rise", "linear_fall", 
                "linear_low", "linear_middle", "linear_high", 
                "exp_rise_convex", "exp_fall_convex", "exp_rise_concave", "exp_fall_concave", 
                "sin_fr_full", "sin_fr_partial", "sin_rf_full", "sin_rf_partial", 
                "sin_rfr_full", "sin_rfr_partial", 
                "sin_frf_full", "sin_frf_partial", 
                "logistic_rise", "logistic_fall", 
                "sin_frfr", "sin_rfrf", 
                "positive_change_full", "positive_change_partial", 
                "negative_change_full", "negative_change_partial", 
                "linear_rise_sharp_fall", "linear_rise_sharp_fall_exp_rise")

features <- array(0,dim=c(length(plot_names), length(feature_names))) #averaging across vignettes
colnames(features) <- feature_names
rownames(features) <- plot_names

#create comprehension functions
create_comprehension_plots <- function() {
  check1 <- function(x) {x}
  check2 <- function(x) {50-50*(cos((x-20)*0.079))}
  check3 <- function(x) {100/(1+(exp(1))^-(x-70))}
  myFuncs <- c(check1, check2, check3)
  lapply(X = myFuncs, FUN = single_plot_function)
}

#define equations and create experimental functions
create_equations <- function() {
  linear_rise <- function(x) {1.25*x}
  linear_fall <- function(x) {100-1.25*x}
  linear_low <- function(x) {0*x+0}
  linear_middle <- function(x) {0*x+50}
  linear_high <- function(x) {0*x+100}
  exp_rise_convex <- function(x) {1.0595^x-1}
  exp_fall_convex <- function(x) {1.0595^(-x+80)-1}
  exp_rise_concave <- function(x) {100-1.0595^(-x+80)+1}
  exp_fall_concave <- function(x) {100-1.0595^x+1}
  sin_fr_full <- function(x) {50+50*(cos(x*0.079))}
  sin_fr_partial <- function(x) {50-50*(sin(x*0.05889))}
  sin_rf_full <- function(x) {50-50*(cos(x*0.079))}
  sin_rf_partial <- function(x) {50+50*(sin(x*0.05889))}
  sin_rfr_full <- function(x) {50-50*(cos(x*0.1185))}
  sin_rfr_partial <- function(x) {50+50*(sin(x*0.0982))}
  sin_frf_full <- function(x) {50+50*(cos(x*0.1185))}
  sin_frf_partial <- function(x) {50-50*(sin(x*0.0982))}
  logistic_rise <- function(x) {100/(1+(exp(1))^-(x-40))}
  logistic_fall <- function(x) {100-100/(1+(exp(1))^-(x-40))}
  sin_frfr <- function(x) {50-50*(sin(x*0.1375))}
  sin_rfrf <- function(x) {50+50*(sin(x*0.1375))}
  positive_change_full <- function(x) {
    (x < 60)*((-5/3)*(x-60)) +
      (60 <= x & x <= 80)*(100+5*(x-80))
  }
  positive_change_partial <- function(x) {
    (x < 60)*((-5/3)*(x-60)) +
      (60 <= x & x <= 80)*(50+2.5*(x-80))
  }
  negative_change_full <- function(x) {
    (x < 60)*(5/3)*x +
      (60 <= x & x <= 80)*(-5*(x-80))
  }
  negative_change_partial <- function(x) {
    (x < 60)*(5/3)*x +
      (60 <= x & x <= 80)*(-2.5*(x-100))
  }
  linear_rise_sharp_fall <- function(x) {
    (x < 40)*(1.25*x) +
      (40 < x & x <= 80)*(0*x+0)
  }
  linear_rise_sharp_fall_exp_rise <- function(x) {
    (x < 40)*(1.25*x) +
      (40 < x & x <= 80)*(1.0606^x-10)
  }
  equations <- c(linear_rise, linear_fall, linear_low, linear_middle, linear_high, 
                 exp_rise_convex, exp_fall_convex, exp_rise_concave, exp_fall_concave, 
                 sin_fr_full, sin_fr_partial, sin_rf_full, sin_rf_partial, 
                 sin_rfr_full, sin_rfr_partial, sin_frf_full, sin_frf_partial, 
                 logistic_rise, logistic_fall, sin_frfr, sin_rfrf, 
                 positive_change_full, positive_change_partial, negative_change_full, negative_change_partial, 
                 linear_rise_sharp_fall, linear_rise_sharp_fall_exp_rise)
  return(equations)
}

#plot first derivatives
my_plot_D1 <- function(equation) {
  dev.new(width = 8, height = 6, noRStudioGD = TRUE)
  plot(equation, lwd = 7, xlim = c(0,80), ylim = c(-10,10), main = "", 
       xlab = "", ylab = "", col = "firebrick3", cex.lab = 1.5)
  return(my_plot_D1)
}

#create first derivative functions
#the first derivatives of the linear (1-5) and piecewise (22-27) equations below 
#have been written into functions so that they can be plotted and computed.
create_D1 <- function() {
  linear_rise <- function(x) {1.25+0*x}
  linear_fall <- function(x) {-1.25+0*x}
  linear_low <- function(x) {0+0*x}
  linear_middle <- function(x) {0+0*x}
  linear_high <- function(x) {0+0*x}
  exp_rise_convex <- D(1.0595^x-1 ~ x)
  exp_fall_convex <- D(1.0595^(-x+80)-1 ~ x)
  exp_rise_concave <- D(100-1.0595^(-x+80)+1 ~ x)
  exp_fall_concave <- D(100-1.0595^x+1 ~ x)
  sin_fr_full <- D(50+50*(cos(x*0.079)) ~ x)
  sin_fr_partial <- D(50-50*(sin(x*0.05889)) ~ x)
  sin_rf_full <- D(50-50*(cos(x*0.079)) ~ x)
  sin_rf_partial <- D(50+50*(sin(x*0.05889)) ~ x)
  sin_rfr_full <- D(50-50*(cos(x*0.1185)) ~ x)
  sin_rfr_partial <- D(50+50*(sin(x*0.0982)) ~ x)
  sin_frf_full <- D(50+50*(cos(x*0.1185)) ~ x)
  sin_frf_partial <- D(50-50*(sin(x*0.0982)) ~ x)
  logistic_rise <- D(100/(1+(exp(1))^-(x-40)) ~ x)
  logistic_fall <- D(100-100/(1+(exp(1))^-(x-40)) ~ x)
  sin_frfr <- D(50-50*(sin(x*0.1375)) ~ x)
  sin_rfrf <- D(50+50*(sin(x*0.1375)) ~ x)
  positive_change_full <- function(x) {
    (x < 60)*(0*x-(5/3)) + (60 <= x & x <= 80)*(0*x+5)
  }
  positive_change_partial <- function(x) {
    (x < 60)*(0*x-(5/3)) + (60 <= x & x <= 80)*(0*x+2.5)
  }
  negative_change_full <- function(x) {
    (x < 60)*(0*x+(5/3)) + (60 <= x & x <= 80)*(0*x-5)
  }
  negative_change_partial <- function(x) {
    (x < 60)*(0*x+(5/3)) + (60 <= x & x <= 80)*(0*x-2.5)
  }
  linear_rise_sharp_fall <- function(x) {
    (x < 40)*(0*x+1.25) + (40 < x & x <= 80)*(0*x+0)
  }
  linear_rise_sharp_fall_exp_rise <- function(x) {
    (x < 40)*(0*x+1.25) + (40 < x & x <= 80)*(1.0606^x * log(1.0606))
  }
  first_derivatives <- c(linear_rise, linear_fall, linear_low, linear_middle, linear_high, 
                         exp_rise_convex, exp_fall_convex, exp_rise_concave, exp_fall_concave, 
                         sin_fr_full, sin_fr_partial, sin_rf_full, sin_rf_partial, 
                         sin_rfr_full, sin_rfr_partial, sin_frf_full, sin_frf_partial, 
                         logistic_rise, logistic_fall, sin_frfr, sin_rfrf, 
                         positive_change_full, positive_change_partial, negative_change_full, negative_change_partial, 
                         linear_rise_sharp_fall, linear_rise_sharp_fall_exp_rise)
  return(first_derivatives)
} 

#plot second derivatives
my_plot_D2 <- function(equation) {
  dev.new(width = 8, height = 6, noRStudioGD = TRUE)
  plot(equation, lwd = 7, xlim = c(0,80), ylim = c(-2,2), main = "", 
       xlab = "", ylab = "", col = "firebrick3", cex.lab = 1.5)
  return(my_plot_D2)
}

#create second derivative functions
#the second derivatives of the linear (1-5) and piecewise (22-27) equations below 
#have been written into functions so that they can be plotted and computed.
create_D2 <- function() {
  linear_rise <- function(x) {0+0*x}
  linear_fall <- function(x) {0+0*x}
  linear_low <- function(x) {0+0*x}
  linear_middle <- function(x) {0+0*x}
  linear_high <- function(x) {0+0*x}
  exp_rise_convex <- D(1.0595^x-1 ~ x&x)
  exp_fall_convex <- D(1.0595^(-x+80)-1 ~ x&x)
  exp_rise_concave <- D(100-1.0595^(-x+80)+1 ~ x&x)
  exp_fall_concave <- D(100-1.0595^x+1 ~ x&x)
  sin_fr_full <- D(50+50*(cos(x*0.079)) ~ x & x)
  sin_fr_partial <- D(50-50*(sin(x*0.05889)) ~ x&x)
  sin_rf_full <- D(50-50*(cos(x*0.079)) ~ x & x)
  sin_rf_partial <- D(50+50*(sin(x*0.05889)) ~ x&x)
  sin_rfr_full <- D(50-50*(cos(x*0.1185)) ~ x&x)
  sin_rfr_partial <- D(50+50*(sin(x*0.0982)) ~ x&x)
  sin_frf_full <- D(50+50*(cos(x*0.1185)) ~ x&x)
  sin_frf_partial <- D(50-50*(sin(x*0.0982)) ~ x&x)
  logistic_rise <- D(100/(1+(exp(1))^-(x-40)) ~ x & x)
  logistic_fall <- D(100-100/(1+(exp(1))^-(x-40)) ~ x & x)
  sin_frfr <- D(50-50*(sin(x*0.1375)) ~ x&x)
  sin_rfrf <- D(50+50*(sin(x*0.1375)) ~ x&x)
  positive_change_full <- function(x) {0+0*x}
  positive_change_partial <- function(x) {0+0*x}
  negative_change_full <- function(x) {0+0*x}
  negative_change_partial <- function(x) {0+0*x}
  linear_rise_sharp_fall <- function(x) {0+0*x}
  linear_rise_sharp_fall_exp_rise <- function(x) {
    (x < 40)*(0*x+0) + (40 < x & x <= 80)*(1.0606^x * log(1.0606) * log(1.0606))
  }
  second_derivatives <- c(linear_rise, linear_fall, linear_low, linear_middle, linear_high, 
                          exp_rise_convex, exp_fall_convex, exp_rise_concave, exp_fall_concave, 
                          sin_fr_full, sin_fr_partial, sin_rf_full, sin_rf_partial, 
                          sin_rfr_full, sin_rfr_partial, sin_frf_full, sin_frf_partial, 
                          logistic_rise, logistic_fall, sin_frfr, sin_rfrf, 
                          positive_change_full, positive_change_partial, negative_change_full, negative_change_partial, 
                          linear_rise_sharp_fall, linear_rise_sharp_fall_exp_rise)
  return(second_derivatives)
} 

#find minima
get_min <- function(equation) {
  answer <- optimize(equation, interval = c(0,80))
  return(answer$objective[1])
}

#find maxima (only works for functions 1-26; makes mistake on function 27)
get_max <- function(equation) {
  answer <- optimize(equation, interval = c(0,80), maximum = TRUE)
  return(answer$objective[1])
}

#find maximum separately for function 27
get_max_2 <- function(equation) {
  answer <- optimize(function(x) {
    (x < 40)*(1.25*x) + (40 < x & x <= 80)*(1.0606^x-10)}, 
    interval = c(40,80), maximum = TRUE)
  return(answer$objective[1])
}

#find integral
get_integral <- function(equation) {
  answer <- integrate(equation, 0, 80)
  return(answer$value)
}

#find the unweighted sum of the first derivative, which is equal to subtracting
#the values of the original function (i.e., integrating f'(x) from 0 to 80 = f(80)-f(0)).
#(note: these sum and average functions only work for the first derivatives of functions 1-25, 
#not for the discontinuous piecewise functions 26 & 27).
my_unweight_sum <- function(equation) {
  a <- equation(80)-equation(0)
  return(a)
}

#find (customized) sum separately for piecewise functions 26 & 27
my_unweight_sum_D1_2 <- function() {   
  linear_rise_sharp_fall <- integrate(function(x) {
    (x <= 40)*(0*x+1.25) + (40 < x & x <= 80)*(0*x+0)}, 0, 80)
  linear_rise_sharp_fall_exp_rise <- integrate(function(x) {
    (x <= 40)*(0*x+1.25) + (40 < x & x <= 80)*(1.0606^x * log(1.0606))}, 0, 80)
  equations <- c(linear_rise_sharp_fall, linear_rise_sharp_fall_exp_rise)
  return(equations)
}

#find sum of first derivative with weighted "prime" (ages 18-30)
my_prime_sum <- function(equation) {
  a <- equation(18)-equation(0)
  b <- 2*(equation(30)-equation(18))
  c <- equation(80)-equation(30)
  d <- a+b+c
  return(d)
}

#find (customized) sum separately for piecewise functions 26 & 27
my_prime_sum_D1_2 <- function() {   
  linear_rise_sharp_fall <- integrate(function(x) {
    (x <= 18)*(0*x+1.25) + (18 < x & x <= 30)*(0*x+1.25)*2 + 
      (30 < x & x <= 40)*(0*x+1.25) + (40 < x & x <= 80)*(0*x+0)}, 0, 80)
  linear_rise_sharp_fall_exp_rise <- integrate(function(x) {
    (x <= 18)*(0*x+1.25) + (18 < x & x <= 30)*(0*x+1.25)*2 + 
      (30 < x & x <= 40)*(0*x+1.25) + (40 < x & x <= 80)*(1.0606^x * log(1.0606))}, 0, 80)
  equations <- c(linear_rise_sharp_fall, linear_rise_sharp_fall_exp_rise)
  lapply(equations, eval)
}

#find sum of first derivative with ascending weights (0.25, 0.5, 0.75, and 1)
my_ascending_sum <- function(equation) {
  a <- 0.25*(equation(20)-equation(0))
  b <- 0.5*(equation(40)-equation(20))
  c <- 0.75*(equation(60)-equation(40))
  d <- 1*(equation(80)-equation(60))
  e <- a+b+c+d
  e
  return(e)
}

#find (customized) sum separately for piecewise functions 26 & 27
my_ascending_sum_D1_2 <- function() {   
  linear_rise_sharp_fall <- integrate(function(x) {
    (x <= 20)*(0*x+1.25)*0.25 + (20 < x & x <= 40)*(0*x+1.25)*0.5 + 
      (40 < x & x <= 60)*(0*x+0)*0.75 + (60 < x & x <= 80)*(0*x+0)*1}, 0, 80)
  linear_rise_sharp_fall_exp_rise <- integrate(function(x) {
    (x <= 20)*(0*x+1.25)*0.25 + (20 < x & x <= 40)*(0*x+1.25)*0.5 + 
      (40 < x & x <= 60)*(1.0606^x * log(1.0606))*0.75 + (60 < x & x <= 80)*(1.0606^x * log(1.0606))*1}, 0, 80)
  equations <- c(linear_rise_sharp_fall, linear_rise_sharp_fall_exp_rise)
  lapply(equations, eval)
}

#find sum of first derivative with descending weights (1, 0.75, 0.5, and 0.25)
my_descending_sum <- function(equation) {
  a <- 1*(equation(20)-equation(0))
  b <- 0.75*(equation(40)-equation(20))
  c <- 0.5*(equation(60)-equation(40))
  d <- 0.25*(equation(80)-equation(60))
  e <- a+b+c+d
  return(e)
}

#find (customized) sum separately for piecewise functions 26 & 27
my_descending_sum_D1_2 <- function() {   
  linear_rise_sharp_fall <- integrate(function(x) {
    (x <= 20)*(0*x+1.25)*1 + (20 < x & x <= 40)*(0*x+1.25)*0.75 + 
      (40 < x & x <= 60)*(0*x+0)*0.5 + (60 < x & x <= 80)*(0*x+0)*0.25}, 0, 80)
  linear_rise_sharp_fall_exp_rise <- integrate(function(x) {
    (x <= 20)*(0*x+1.25)*1 + (20 < x & x <= 40)*(0*x+1.25)*0.75 + 
      (40 < x & x <= 60)*(1.0606^x * log(1.0606))*0.5 + (60 < x & x <= 80)*(1.0606^x * log(1.0606))*0.25}, 0, 80)
  equations <- c(linear_rise_sharp_fall, linear_rise_sharp_fall_exp_rise)
  lapply(equations, eval)
}

#find sum of first derivative with weighted "end" (ages 60-80)
my_end_sum <- function(equation) {
  a <- equation(60)-equation(0)
  b <- 2*(equation(80)-equation(60))
  c <- a+b
  return(c)
}

#find (customized) sum separately for piecewise functions 26 & 27
my_end_sum_D1_2 <- function() {   
  linear_rise_sharp_fall <- integrate(function(x) {
    (x <= 40)*(0*x+1.25) + (40 < x & x <= 60)*(0*x+0) + 
      (60 < x & x <= 80)*(0*x+0)*2}, 0, 80)
  linear_rise_sharp_fall_exp_rise <- integrate(function(x) {
    (x <= 40)*(0*x+1.25) + (40 < x & x <= 60)*(1.0606^x * log(1.0606)) + 
      (60 < x & x <= 80)*(1.0606^x * log(1.0606))*2}, 0, 80)
  equations <- c(linear_rise_sharp_fall, linear_rise_sharp_fall_exp_rise)
  lapply(equations, eval)
}

#find averages of unweighted and weighted sums by dividing the columns "d1_sum_unweight" 
#through "d1_sum_weight_end" by 80, which is the full range of x values (see main script).

#-------------------------------------------------------------------------------------------------------------------------------------------------

#define equations to find sums and averages of second derivative

#for equations 1-21, subtracting the values of the first derivative and averaging them is the same as
#integrating (finding the area of) and averaging the second derivative. Equations 22-27 are piecewise 
#functions, and because R cannot properly compute their area, they will be considered separately below.
#the linear equations (1-5) have been manually rewritten from constants into functions 
#so that they can be computed with the rest.
create_equations_2 <- function() {
  linear_rise <- function(x) {1.25+0*x} 
  linear_fall <- function(x) {-1.25+0*x}
  linear_low <- function(x) {0+0*x}
  linear_middle <- function(x) {0+0*x}
  linear_high <- function(x) {0+0*x}
  exp_rise_convex <- D(1.0595^x-1 ~ x)
  exp_fall_convex <- D(1.0595^(-x+80)-1 ~ x)
  exp_rise_concave <- D(100-1.0595^(-x+80)+1 ~ x)
  exp_fall_concave <- D(100-1.0595^x+1 ~ x)
  sin_fr_full <- D(50+50*(cos(x*0.079)) ~ x)
  sin_fr_partial <- D(50-50*(sin(x*0.05889)) ~ x)
  sin_rf_full <- D(50-50*(cos(x*0.079)) ~ x)
  sin_rf_partial <- D(50+50*(sin(x*0.05889)) ~ x)
  sin_rfr_full <- D(50-50*(cos(x*0.1185)) ~ x)
  sin_rfr_partial <- D(50+50*(sin(x*0.0982)) ~ x)
  sin_frf_full <- D(50+50*(cos(x*0.1185)) ~ x)
  sin_frf_partial <- D(50-50*(sin(x*0.0982)) ~ x)
  logistic_rise <- D(100/(1+(exp(1))^-(x-40)) ~ x)
  logistic_fall <- D(100-100/(1+(exp(1))^-(x-40)) ~ x)
  sin_frfr <- D(50-50*(sin(x*0.1375)) ~ x)
  sin_rfrf <- D(50+50*(sin(x*0.1375)) ~ x)
  equations_2 <- c(linear_rise, linear_fall, linear_low, linear_middle, linear_high, 
                   exp_rise_convex, exp_fall_convex, exp_rise_concave, exp_fall_concave, 
                   sin_fr_full, sin_fr_partial, sin_rf_full, sin_rf_partial, 
                   sin_rfr_full, sin_rfr_partial, sin_frf_full, sin_frf_partial, 
                   logistic_rise, logistic_fall, sin_frfr, sin_rfrf)
  return(equations_2)
} 

#finding sums and averages of the second derivatives of functions 1-21 uses
#the same defined formulas as the first derivatives above ("my_unweight_sum"  
#through "my_end_sum"), but with "equations_2" instead of "equations" (see main script).

#the equations below will only be used for piecewise functions 22-27, 
#which have been manually rewritten as the second derivatives of their original functions
#so that they can be integrated.

#find (customized) unweighted sum separately for piecewise functions 22-27
my_unweight_sum_D2_2 <- function() {   
  positive_change_full <- integrate(function(x) {0*x+0}, 0, 80)
  positive_change_partial <- integrate(function(x) {0*x+0}, 0, 80)
  negative_change_full <- integrate(function(x) {0*x+0}, 0, 80)
  negative_change_partial <- integrate(function(x) {0*x+0}, 0, 80)
  linear_rise_sharp_fall <- integrate(function(x) {0*x+0}, 0, 80)
  linear_rise_sharp_fall_exp_rise <- integrate(function(x) {
    (x < 40)*(0*x+0) + (40 < x & x <= 80)*(1.0606^x * log(1.0606) * log(1.0606))}, 0, 80)
  equations <- c(positive_change_full, positive_change_partial, negative_change_full, negative_change_partial, 
                 linear_rise_sharp_fall, linear_rise_sharp_fall_exp_rise)
  lapply(equations, eval)
}

my_prime_sum_D2_2 <- function() {   
  positive_change_full <- integrate(function(x) {0*x+0}, 0, 80)
  positive_change_partial <- integrate(function(x) {0*x+0}, 0, 80)
  negative_change_full <- integrate(function(x) {0*x+0}, 0, 80)
  negative_change_partial <- integrate(function(x) {0*x+0}, 0, 80)
  linear_rise_sharp_fall <- integrate(function(x) {0*x+0}, 0, 80)
  linear_rise_sharp_fall_exp_rise <- integrate(function(x) {
    (x < 40)*(0*x+0) + (40 < x & x <= 80)*(1.0606^x * log(1.0606) * log(1.0606))}, 0, 80)
  equations <- c(positive_change_full, positive_change_partial, negative_change_full, negative_change_partial, 
                 linear_rise_sharp_fall, linear_rise_sharp_fall_exp_rise)
  lapply(equations, eval)
}

my_ascending_sum_D2_2 <- function() {
  positive_change_full <- integrate(function(x) {0*x+0}, 0, 80)
  positive_change_partial <- integrate(function(x) {0*x+0}, 0, 80)
  negative_change_full <- integrate(function(x) {0*x+0}, 0, 80)
  negative_change_partial <- integrate(function(x) {0*x+0}, 0, 80)
  linear_rise_sharp_fall <- integrate(function(x) {0*x+0}, 0, 80)
  linear_rise_sharp_fall_exp_rise <- integrate(function(x) {
    (x < 20)*(0*x+0) + (40 < x & x <= 60)*(1.0606^x * log(1.0606) * log(1.0606))*0.75 + 
      (60 < x & x <= 80)*(1.0606^x * log(1.0606) * log(1.0606))*1}, 0, 80)
  equations <- c(positive_change_full, positive_change_partial, negative_change_full, negative_change_partial, 
                 linear_rise_sharp_fall, linear_rise_sharp_fall_exp_rise)
  lapply(equations, eval)
}   

my_descending_sum_D2_2 <- function() {
  positive_change_full <- integrate(function(x) {0*x+0}, 0, 80)
  positive_change_partial <- integrate(function(x) {0*x+0}, 0, 80)
  negative_change_full <- integrate(function(x) {0*x+0}, 0, 80)
  negative_change_partial <- integrate(function(x) {0*x+0}, 0, 80)
  linear_rise_sharp_fall <- integrate(function(x) {0*x+0}, 0, 80)
  linear_rise_sharp_fall_exp_rise <- integrate(function(x) {
    (x < 20)*(0*x+0) + (40 < x & x <= 60)*(1.0606^x * log(1.0606) * log(1.0606))*0.5 + 
      (60 < x & x <= 80)*(1.0606^x * log(1.0606) * log(1.0606))*0.25}, 0, 80)
  equations <- c(positive_change_full, positive_change_partial, negative_change_full, negative_change_partial, 
                 linear_rise_sharp_fall, linear_rise_sharp_fall_exp_rise)
  lapply(equations, eval)
}   

my_end_sum_D2_2 <- function() {
  positive_change_full <- integrate(function(x) {0*x+0}, 0, 80)
  positive_change_partial <- integrate(function(x) {0*x+0}, 0, 80)
  negative_change_full <- integrate(function(x) {0*x+0}, 0, 80)
  negative_change_partial <- integrate(function(x) {0*x+0}, 0, 80)
  linear_rise_sharp_fall <- integrate(function(x) {0*x+0}, 0, 80)
  linear_rise_sharp_fall_exp_rise <- integrate(function(x) {
    (x < 20)*(0*x+0) + (40 < x & x <= 60)*(1.0606^x * log(1.0606) * log(1.0606)) + 
      (60 < x & x <= 80)*(1.0606^x * log(1.0606) * log(1.0606))*2}, 0, 80)
  equations <- c(positive_change_full, positive_change_partial, negative_change_full, negative_change_partial, 
                 linear_rise_sharp_fall, linear_rise_sharp_fall_exp_rise)
  lapply(equations, eval)
}   

#find averages of unweighted and weighted sums by dividing the columns 
#above by 80 (see main script).

##plotting functions
#plot comprehension and experimental functions
plotter <- function(equation, x_label, y_label, y_range) {
  dev.new(width = 8, height = 6, noRStudioGD = TRUE)
  plot(equation, lwd = 7, xlim = c(0,80), ylim = c(0,100), main = "", 
       xlab = "Age", ylab = "Fulfillment", col = "firebrick3", cex.lab = 1.5, cex.axis = 1.5)  
  
}

single_plot_function <- function(equation) {
  dev.new(width = 8, height = 6, noRStudioGD = TRUE)
  plot(equation, lwd = 7, xlim = c(0,80), ylim = c(0,100), main = "", 
       xlab = "Age", ylab = "Fulfillment", col = "firebrick3", cex.lab = 1.5, cex.axis = 1.5)
  return(single_plot_function)
}

my_plot_function_2 <- function(equation) {
  plot(equation, lwd = 7, xlim = c(0,80), ylim = c(-10,10), main = "", 
       xlab = "", ylab = "", col = "firebrick3", cex.lab = 1.5)
  return(my_plot_function_2)
}

my_plot_function_3 <- function(equation) {
  plot(equation, lwd = 7, xlim = c(0,80), ylim = c(-2,2), main = "", 
       xlab = "", ylab = "", col = "firebrick3", cex.lab = 1.5)
  return(my_plot_function_3)
}


#-------------------------------------------------------------------------------------------------------------------------------------------------

# **** MAIN **** #

#create equations
equations <- create_equations()
equations_2 <- create_equations_2()

# PL0T STUFF
create_comprehension_plots()
create_experimental_plots <- sapply(create_equations(), single_plot_function)
create_D1_plots <- sapply(create_D1(), my_plot_D1)
create_D2_plots <- sapply(create_D2(), my_plot_D2)



#get features
features[ ,"min"] <- sapply(equations, get_min)
features[ ,"max"] <- c(sapply(equations[1:26], get_max), get_max_2())
features[ ,"integral"] <- sapply(equations, get_integral)

features[ ,"d1_sum_unweight"] <- c(sapply(equations[1:25], my_unweight_sum), unlist(my_unweight_sum_D1_2()[seq(1, length(my_unweight_sum_D1_2()), 5)]))
features[ ,"d1_sum_weight_prime"] <- c(sapply(equations[1:25], my_prime_sum), unlist(my_prime_sum_D1_2()[seq(1, length(my_prime_sum_D1_2()), 5)]))
features[ ,"d1_sum_weight_asc"] <- c(sapply(equations[1:25], my_ascending_sum), unlist(my_ascending_sum_D1_2()[seq(1, length(my_ascending_sum_D1_2()), 5)]))
features[ ,"d1_sum_weight_des"] <- c(sapply(equations[1:25], my_descending_sum), unlist(my_descending_sum_D1_2()[seq(1, length(my_descending_sum_D1_2()), 5)]))
features[ ,"d1_sum_weight_end"] <- c(sapply(equations[1:25], my_end_sum), unlist(my_end_sum_D1_2()[seq(1, length(my_end_sum_D1_2()), 5)]))
features[ ,"d1_avg_unweight"] <- features[ ,"d1_sum_unweight"]/80
features[ ,"d1_avg_weight_prime"] <- features[ ,"d1_sum_weight_prime"]/80
features[ ,"d1_avg_weight_asc"] <- features[ ,"d1_sum_weight_asc"]/80
features[ ,"d1_avg_weight_des"] <- features[ ,"d1_sum_weight_des"]/80
features[ ,"d1_avg_weight_end"] <- features[ ,"d1_sum_weight_end"]/80

features[ ,"d2_sum_unweight"] <- c(sapply(equations_2, my_unweight_sum), unlist(my_unweight_sum_D2_2()[seq(1, length(my_unweight_sum_D2_2()), 5)]))
features[ ,"d2_sum_weight_prime"] <- c(sapply(equations_2, my_prime_sum), unlist(my_prime_sum_D2_2()[seq(1, length(my_prime_sum_D2_2()), 5)]))
features[ ,"d2_sum_weight_asc"] <- c(sapply(equations_2, my_ascending_sum), unlist(my_ascending_sum_D2_2()[seq(1, length(my_ascending_sum_D2_2()), 5)]))
features[ ,"d2_sum_weight_des"] <- c(sapply(equations_2, my_descending_sum), unlist(my_descending_sum_D2_2()[seq(1, length(my_descending_sum_D2_2()), 5)]))
features[ ,"d2_sum_weight_end"] <- c(sapply(equations_2, my_end_sum), unlist(my_end_sum_D2_2()[seq(1, length(my_end_sum_D2_2()), 5)]))
features[ ,"d2_avg_unweight"] <- features[ ,"d2_sum_unweight"]/80
features[ ,"d2_avg_weight_prime"] <- features[ ,"d2_sum_weight_prime"]/80
features[ ,"d2_avg_weight_asc"] <- features[ ,"d2_sum_weight_asc"]/80
features[ ,"d2_avg_weight_des"] <- features[ ,"d2_sum_weight_des"]/80
features[ ,"d2_avg_weight_end"] <- features[ ,"d2_sum_weight_end"]/80

##Note: r can't differentiate between local maxima and minima (using infl(), it counts both as just extrema 
##at the inflection points). When possible, it also wants to count extrema at the endpoints of the graph, 
##which are not technically inflection points and in our case do not indicate changes in direction ("notches), 
##so the data below have been manually generated. (Note: "Number of minima" will be written as "valleys" in order
##to avoid confusion with the actual minimum values above. Likewise, "number of maxima" will be changed to "peaks.")

features[ ,"number_valleys"] <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 1, 1, 1, 0, 0, 2, 1, 1, 1, 0, 0, 0, 0)
features[ ,"number_peaks"] <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 0, 0, 1, 2, 0, 0, 1, 1, 1, 1)
features[ ,"number_extrema"] <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 2, 0, 0, 3, 3, 1, 1, 1, 1, 1, 1)

#standardize features
##standardize the difficult-to-interpret values of the unweighted and weighted sums and averages in the 'features' array
##into z-scores and create a new, standardized dataframe called 'featuresZ'.

featuresN <- features[ ,6:26] ##getting the columns 6-26 from the 'features' array
df <- (featuresN-rowMeans(featuresN))/(rowSds(as.matrix(featuresN)))[row(featuresN)] ##calculating Z score
is.nan.data.frame <- function(x) ##replacing NaN values with 0 (zero), part 1
  do.call(cbind, lapply(x, is.nan))
df[is.nan(df)] <- 0 ##replacing NaN values with 0 (zero), part 2
sub_features <- features[ ,1:5] #extracting columns 1-5 from 'features'
df <- cbind(df, sub_features) ##merging 'df' and 'sub_features'
col_order <- c("max", "min", "number_peaks", "number_valleys", "number_extrema", "integral", "d1_sum_unweight", "d1_sum_weight_prime",
                 "d1_sum_weight_asc", "d1_sum_weight_des", "d1_sum_weight_end", "d1_avg_unweight", "d1_avg_weight_prime",
                 "d1_avg_weight_asc", "d1_avg_weight_des", "d1_avg_weight_end", "d2_sum_unweight", "d2_sum_weight_prime",
                 "d2_sum_weight_asc", "d2_sum_weight_des", "d2_sum_weight_end", "d2_avg_unweight", "d2_avg_weight_prime",
                 "d2_avg_weight_asc", "d2_avg_weight_des", "d2_avg_weight_end")
featuresZ <- df[, col_order] ##creating the final dataframe, 'featuresZ'
View(featuresZ)
write.csv(featuresZ, 'featuresZ.csv')

#save as pdf files

pdf(file="comprehension_plots.pdf")
par(mfrow = c(3,3))
my_plot_function <- function(equation) {
  plot(equation, lwd = 7, xlim = c(0,80), ylim = c(0,100), main = "", 
       xlab = "Age", ylab = "Fulfillment", col = "firebrick3", cex.lab = 1.5, cex.axis = 1.5)
  return(my_plot_function)
}
comp_plots <- function() {
  check1 <- function(x) {x}
  check2 <- function(x) {50-50*(cos((x-20)*0.079))}
  check3 <- function(x) {100/(1+(exp(1))^-(x-70))}
  checks <- c(check1, check2, check3)
  return(checks)
}
comprehension_plots <- sapply(comp_plots(), my_plot_function)
dev.off()

pdf(file="experimental_plots.pdf")
par(mfrow = c(3,3))
experimental_plots <- sapply(create_equations(), my_plot_function)
dev.off()

pdf(file="d1_plots.pdf")
par(mfrow = c(3,3))
D1_plots <- sapply(create_D1(), my_plot_function_2)
dev.off()

pdf(file="d2_plots.pdf")
par(mfrow = c(3,3))
D2_plots <- sapply(create_D2(), my_plot_function_3)
dev.off()










## -------------------------------------------------------------------------------------------------------------------
##POTENTIAL SECOND STUDY ---------------------------------------------------------------------------------------------
## -------------------------------------------------------------------------------------------------------------------

## -------------------------------------------------------------------------------------------------------------------
## DEFINE ALL FUNCTIONS ----------------------------------------------------------------------------------------------
## -------------------------------------------------------------------------------------------------------------------

#define feature array
feature_names_2 <- c("max", "min", "number_peaks", "number_valleys", "number_extrema", "integral", 
                     "d1_sum_unweight", "d1_sum_weight_prime", "d1_sum_weight_asc", "d1_sum_weight_des", 
                     "d1_avg_unweight", "d1_avg_weight_prime", "d1_avg_weight_asc", "d1_avg_weight_des", 
                     "d2_sum_unweight", "d2_sum_weight_prime", "d2_sum_weight_asc", "d2_sum_weight_des", 
                     "d2_avg_unweight", "d2_avg_weight_prime", "d2_avg_weight_asc", "d2_avg_weight_des")

plot_names_2 <- c("linear_cut_rise", "linear_cut_fall", 
                  "exponential_cut_rise", "exponential_cut_fall")

features_2 <- array(0,dim=c(length(plot_names_2), length(feature_names_2)))
colnames(features_2) <- feature_names_2
rownames(features_2) <- plot_names_2

#create cutoff plots
my_plot_cutoffs <- function(equation) {
  dev.new(width = 8, height = 6, noRStudioGD = TRUE)
  plot(equation,0,40, lwd = 7, xlim = c(0,80), ylim = c(0,100), main = "", 
       xlab = "Age", ylab = "Fulfillment", col = "firebrick3", cex.lab = 1.5, cex.axis = 1.5)
  return(my_plot_cutoffs)
}

cutoff_functions <- function() {
  linear_cut_rise <- function(x) {1.25*x}
  linear_cut_fall <- function(x) {100-1.25*x}
  exp_cut_rise <- function(x) {1.0595^x-1}
  exp_cut_fall <- function(x) {1.0595^(-x+80)-1}
  equations <- c(linear_cut_rise, linear_cut_fall, exp_cut_rise, exp_cut_fall)
  return(equations)
}

#create first derivative plots
my_plot_D1_cutoffs <- function(equation) {
  dev.new(width = 8, height = 6, noRStudioGD = TRUE)
  plot(equation,0,40, lwd = 7, xlim = c(0,80), ylim = c(-10,10), main = "", 
       xlab = "", ylab = "", col = "firebrick3", cex.lab = 1.5)
  return(my_plot_D1_cutoffs)
}

D1_cutoff_functions <- function() {
  linear_cut_rise <- function(x) {1.25+0*x}
  linear_cut_fall <- function(x) {-1.25+0*x}
  exp_cut_rise <- D(1.0595^x-1 ~ x)
  exp_cut_fall <- D(1.0595^(-x+80)-1 ~ x)
  equations <- c(linear_cut_rise, linear_cut_fall, exp_cut_rise, exp_cut_fall)
  return(equations)
}

#create second derivative plots
my_plot_D2_cutoffs <- function(equation) {
  dev.new(width = 8, height = 6, noRStudioGD = TRUE)
  plot(equation,0,40, lwd = 7, xlim = c(0,80), ylim = c(-2,2), main = "", 
       xlab = "", ylab = "", col = "firebrick3", cex.lab = 1.5)
  return(my_plot_D2_cutoffs)
}

D2_cutoff_functions <- function() {
  linear_cut_rise <- function(x) {0+0*x}
  linear_cut_fall <- function(x) {0+0*x}
  exp_cut_rise <- D(1.0595^x-1 ~ x&x)
  exp_cut_fall <- D(1.0595^(-x+80)-1 ~ x&x)
  equations <- c(linear_cut_rise, linear_cut_fall, exp_cut_rise, exp_cut_fall)
  return(equations)
}

#find minima
get_min <- function(equation) {
  answer <- optimize(equation, interval = c(0,40))
  return(answer$objective[1])
}

#find maxima
get_max <- function(equation) {
  answer <- optimize(equation, interval = c(0,40), maximum = TRUE)
  return(answer$objective[1])
}

#find integral
get_integral <- function(equation) {
  answer <- integrate(equation, 0, 40)
  return(answer$value)
}

#find unweighted sum of first derivative, i.e., subtracting the values 
#of the original function
my_unweight_sum_cutoffs <- function(equation) {
  a <- equation(40)-equation(0)
  return(a)
}

#find sum of first derivative with weighted "prime" (ages 18-30)
my_prime_sum_cutoffs <- function(equation) {
  a <- equation(18)-equation(0)
  b <- 2*(equation(30)-equation(18))
  c <- equation(40)-equation(30)
  d <- a+b+c
  return(d)
}

#find sum of first derivative with ascending weights (0.25, 0.5)
my_ascending_sum_cutoffs <- function(equation) {
  a <- 0.25*(equation(20)-equation(0))
  b <- 0.5*(equation(40)-equation(20))
  c <- a+b
  return(c)
}

#find sum of first derivative with descending weights (1, 0.75)
my_descending_sum_cutoffs <- function(equation) {
  a <- 1*(equation(20)-equation(0))
  b <- 0.75*(equation(40)-equation(20))
  c <- a+b
  return(c)
}

#find averages of unweighted and weighted sums by dividing the columns "d2_sum_unweight" 
#through "d2_sum_weight_end" by 40, which is the full range of x values (see main script).

#-------------------------------------------------------------------------------------------------------------------------------------------------

#finding sums and averages of the second derivatives uses the same defined formulas as 
#the first derivatives above ("my_unweight_sum_cutoffs" through "my_end_sum_cutoffs"), 
#but with "early_cutoffs_2" instead of "early_cutoffs" (see main script).
#note: integrating f''(x) from 0 to 40 is equal to f'(40)-f'(0), meaning we can use
#'D1_cutoff_functions' to compute the sums and averages of the second derivative.

## -------------------------------------------------------------------------------------------------------------------
## MAIN SCRIPT -------------------------------------------------------------------------------------------------------
## -------------------------------------------------------------------------------------------------------------------

#plot functions
create_early_cutoffs <- sapply(cutoff_functions(), my_plot_cutoffs)
create_D1_cutoffs <- sapply(D1_cutoff_functions(), my_plot_D1_cutoffs)
create_D2_cutoffs <- sapply(D2_cutoff_functions(), my_plot_D2_cutoffs)

#create equations
early_cutoffs <- cutoff_functions()
early_cutoffs_2 <- D1_cutoff_functions()

#get features
features_2[ ,"min"] <- sapply(early_cutoffs, get_min)
features_2[ ,"max"] <- sapply(early_cutoffs, get_max)
features_2[ ,"integral"] <- sapply(early_cutoffs, get_integral)

features_2[ ,"d1_sum_unweight"] <- sapply(early_cutoffs, my_unweight_sum_cutoffs)
features_2[ ,"d1_sum_weight_prime"] <- sapply(early_cutoffs, my_prime_sum_cutoffs)
features_2[ ,"d1_sum_weight_asc"] <- sapply(early_cutoffs, my_ascending_sum_cutoffs)
features_2[ ,"d1_sum_weight_des"] <- sapply(early_cutoffs, my_descending_sum_cutoffs)
features_2[ ,"d1_avg_unweight"] <- features_2[ ,"d1_sum_unweight"]/80
features_2[ ,"d1_avg_weight_prime"] <- features_2[ ,"d1_sum_weight_prime"]/80
features_2[ ,"d1_avg_weight_asc"] <- features_2[ ,"d1_sum_weight_asc"]/80
features_2[ ,"d1_avg_weight_des"] <- features_2[ ,"d1_sum_weight_des"]/80

features_2[ ,"d2_sum_unweight"] <- sapply(early_cutoffs_2, my_unweight_sum_cutoffs)
features_2[ ,"d2_sum_weight_prime"] <- sapply(early_cutoffs_2, my_prime_sum_cutoffs)
features_2[ ,"d2_sum_weight_asc"] <- sapply(early_cutoffs_2, my_ascending_sum_cutoffs)
features_2[ ,"d2_sum_weight_des"] <- sapply(early_cutoffs_2, my_descending_sum_cutoffs)
features_2[ ,"d2_avg_unweight"] <- features_2[ ,"d2_sum_unweight"]/80
features_2[ ,"d2_avg_weight_prime"] <- features_2[ ,"d2_sum_weight_prime"]/80
features_2[ ,"d2_avg_weight_asc"] <- features_2[ ,"d2_sum_weight_asc"]/80
features_2[ ,"d2_avg_weight_des"] <- features_2[ ,"d2_sum_weight_des"]/80

##Note: r can't differentiate between local maxima and minima (using infl(), it counts both as just extrema 
##at the inflection points). When possible, it also wants to count extrema at the endpoints of the graph, 
##which are not technically inflection points and in our case do not indicate changes in direction ("notches), 
##so the data below have been manually generated. (Note: "Number of minima" will be written as "valleys" in order
##to avoid confusion with the actual minimum values above. Likewise, "number of maxima" will be changed to "peaks.")

features_2[ ,"number_valleys"] <- c(0, 0, 0, 0)
features_2[ ,"number_peaks"] <- c(0, 0, 0, 0)
features_2[ ,"number_extrema"] <- c(0, 0, 0, 0)

##standardize the difficult-to-interpret values of the unweighted and weighted sums and averages in the 'features_2' array
##into z-scores and create a new, standardized array called 'features_2Z'.

features_2N <- features_2[, 6:22] ##getting the columns 6-22 from the 'features_2' array
df2 <- (features_2N-rowMeans(features_2N))/(rowSds(as.matrix(features_2N)))[row(features_2N)] ##calculating Z score
sub_features_2 <- features_2[, 1:5] #extracting columns 1-5 from 'features_2'
df2 <- cbind(df2, sub_features_2) ##merging 'df2' and 'sub_features_2'
column_order_2 <- c("max", "min", "number_peaks", "number_valleys", "number_extrema", "integral", 
                  "d1_sum_unweight", "d1_sum_weight_prime", "d1_sum_weight_asc", "d1_sum_weight_des", 
                  "d1_avg_unweight", "d1_avg_weight_prime", "d1_avg_weight_asc", "d1_avg_weight_des", 
                  "d2_sum_unweight", "d2_sum_weight_prime", "d2_sum_weight_asc", "d2_sum_weight_des", 
                  "d2_avg_unweight", "d2_avg_weight_prime", "d2_avg_weight_asc", "d2_avg_weight_des")
features_2Z <- df2[, column_order_2] ##creating the final dataframe, 'features_2Z'
View(features_2Z)
write.csv(features_2Z, 'features_2Z.csv')

#save as pdf files

pdf(file="early_cutoff_plots.pdf")
par(mfrow = c(3,3))
my_plot_cutoffs_1 <- function(equation) {
  plot(equation,0,40, lwd = 7, xlim = c(0,80), ylim = c(0,100), main = "", 
       xlab = "Age", ylab = "Fulfillment", col = "firebrick3", cex.lab = 1.5, cex.axis = 1.5)
  return(my_plot_cutoffs_1)
}
early_cutoff_plots <- sapply(cutoff_functions(), my_plot_cutoffs_1)
dev.off()

pdf(file="early_cutoff_d1_plots.pdf")
par(mfrow = c(3,3))
my_plot_cutoffs_2 <- function(equation) {
  plot(equation,0,40, lwd = 7, xlim = c(0,80), ylim = c(-10,10), main = "", 
       xlab = "", ylab = "", col = "firebrick3", cex.lab = 1.5)
  return(my_plot_cutoffs_2)
}
D1_cutoff_plots <- sapply(D1_cutoff_functions(), my_plot_cutoffs_2)
dev.off()

pdf(file="early_cutoff_d2_plots.pdf")
par(mfrow = c(3,3))
my_plot_cutoffs_3 <- function(equation) {
  plot(equation,0,40, lwd = 7, xlim = c(0,80), ylim = c(-2,2), main = "", 
       xlab = "", ylab = "", col = "firebrick3", cex.lab = 1.5)
  return(my_plot_cutoffs_3)
}
D2_cutoff_plots <- sapply(D2_cutoff_functions(), my_plot_cutoffs_3)
dev.off()

## ---------------------------------------------------------------------------------------------------------
##PLOTS ----------------------------------------------------------------------------------------------------
## ---------------------------------------------------------------------------------------------------------

##Linear ---------------------------------------------------------------------------------------------------------

##1. Linear Rise (Creation Story) ---------------------------------------------------------------------------------------------------------

linear_rise <- function(x) {1.25*x} ##function
linear_rise_plot <- plot(linear_rise, lwd = 3, xlim = c(0,80), ylim = c(0,100), main = "Linear Rise", 
        xlab = "Age", ylab = "Meaning", col = "dodgerblue2", cex.lab = 1.5) ##plot function
mysubtitle = "(Creation Story)"
mtext(line = 0.5, mysubtitle)
lifelinesZ[1, ]

linear_rise_D1 <- D(1.25*x ~ x) ##first derivative/slope
linear_rise_D1 ##because this is a constant, we'll have to rewrite it as a function.
linear_rise_D1_fxn <- function(x) {0*x+1.25}
linear_rise_D1_plot <- plot(linear_rise_D1_fxn, lwd = 3, xlim = c(0,80), ylim = c(-10,10), 
                                main = "First Derivative of Linear Rise", col = "dodgerblue2", cex.lab = 1.5, cex.lab = 1.5) ##plot of first derivative
mysubtitle = "(Creation Story)"
mtext(line = 0.5, mysubtitle)

linear_rise_D2 <- D(1.25*x ~ x & x) ##second derivative
linear_rise_D2 ##again, this is a constant, so we will rewrite it as a function.
linear_rise_D2_fxn <- function(x) {0*x+0}
linear_rise_D2_plot <- plot(linear_rise_D2_fxn, lwd = 3, xlim = c(0,80), ylim = c(-2,2), 
                            main = "Second Derivative of Linear Rise", col = "dodgerblue2", cex.lab = 1.5, cex.lab = 1.5) ##plot of second derivative
mysubtitle = "(Creation Story)"
mtext(line = 0.5, mysubtitle)

linear_rise_area <- integrate(linear_rise, 0, 80) ##area
linear_rise_area ##4000

linear_rise_min <- optimize(linear_rise, interval = c(0,80)) ##minimum
linear_rise_min$objective ##0
linear_rise_max <- optimize(linear_rise, interval = c(0,80), maximum = TRUE) ##maximum
linear_rise_max$objective ##100

##D1 unweighted sum
lr_D1_sum_unweight <- integrate(linear_rise_D1_fxn, 0, 80)
lr_D1_sum_unweight ##100

##D1 weighted prime: finding sum of various integrals (ages 0-18, 18-30, 30-80)
integrate(linear_rise_D1_fxn, 0, 18) ##22.5
integrate(linear_rise_D1_fxn, 18, 30) ##15
15*2 ##weighing the prime (ages 18-30) by 2 = 30
integrate(linear_rise_D1_fxn, 30, 80) ##62.5
lr_D1_sum_weight_prime <- 22.5+30+62.5
lr_D1_sum_weight_prime ##115

##D1 ascending weights: finding sum of various integrals (using weights of 0.25, 0.5, 0.75, and 1)
integrate(linear_rise_D1_fxn, 0, 20) ##25
25*0.25 ##6.25
integrate(linear_rise_D1_fxn, 20, 40) ##25
25*0.5 ##12.5
integrate(linear_rise_D1_fxn, 40, 60) ##25
25*0.75 ##18.75
integrate(linear_rise_D1_fxn, 60, 80) ##25
25*1 ##25
lr_D1_sum_weight_asc <- 6.25+12.5+18.75+25
lr_D1_sum_weight_asc ##62.5

##D1 descending weights: finding sum of various integrals (using weights of 1, 0.75, 0.5, and 0.25)
integrate(linear_rise_D1_fxn, 0, 25) ##25
25*1 ##25
(integrate(linear_rise_D1_fxn, 20, 40)) ##25
25*0.75 ##18.75
integrate(linear_rise_D1_fxn, 40, 60) ##25
25*0.5 ##12.5
integrate(linear_rise_D1_fxn, 60, 80) ##25
25*0.25 ##6.25
lr_D1_sum_weight_des <- 25+18.75+12.5+6.25
lr_D1_sum_weight_des ##62.5

##D1 weighted end of life: finding sum of various integrals (ages 0-60, 60-80)
integrate(linear_rise_D1_fxn, 0, 60) ##75
integrate(linear_rise_D1_fxn, 60, 80) ##25
25*2 ##weighing the end of life (ages 60-80) by 2 = 50
lr_d1_sum_weight_end <- 75+50
lr_d1_sum_weight_end ##125

##D1 unweighted and weighted averages (sum/80)
lr_D1_avg_unweight <- 100/80
lr_D1_avg_unweight ##1.25
lr_D1_avg_weight_prime <- 115/80
lr_D1_avg_weight_prime ##1.4375
lr_D1_avg_weight_asc <- 62.5/80
lr_D1_avg_weight_asc ##0.78125
lr_D1_avg_weight_des <- 62.5/80
lr_D1_avg_weight_des ##0.78125
lr_D1_avg_weight_end <- 125/80
lr_D1_avg_weight_end ##1.5625

##D2: because the second derivative is zero, all the values below will also be zero
lr_d2_sum_unweight <- 0
lr_d2_sum_weight_prime <- 0
lr_d2_sum_weight_asc <- 0
lr_d2_sum_weight_des <- 0
lr_d2_sum_weight_end <- 0
lr_d2_avg_unweight <- 0
lr_d2_avg_weight_prime <- 0
lr_d2_avg_weight_asc <- 0
lr_d2_avg_weight_des <- 0
lr_d2_avg_weight_end <- 0

##number_minima: 0
##number_maxima: 0
##number_extrema: 0

##2. Linear Fall ---------------------------------------------------------------------------------------------------------

linear_fall <- function(x) {100-1.25*x} ##function
linear_fall_plot <- plot(linear_fall, lwd = 3, xlim = c(0,80), ylim = c(0,100), main = "Linear Fall", 
                            xlab = "Age", ylab = "Meaning", col = "dodgerblue2", cex.lab = 1.5, cex.lab = 1.5) ##plot function
lifelinesZ[2, ]

linear_fall_D1 <- D(100-1.25*x ~ x) ##first derivative/slope
linear_fall_D1 ##rewrite as function
linear_fall_D1_fxn <- function(x) {0*x-1.25}
linear_fall_D1_plot <- plot(linear_fall_D1_fxn, lwd = 3, xlim = c(0,80), ylim = c(-10,10), 
                            main = "First Derivative of Linear Fall", col = "dodgerblue2", cex.lab = 1.5, cex.lab = 1.5) ##plot of first derivative

linear_fall_D2 <- D(100-1.25*x ~ x & x) ##second derivative
linear_fall_D2 ##rewrite as function
linear_fall_D2_fxn <- function(x) {0*x+0}
linear_fall_D2_plot <- plot(linear_fall_D2_fxn, lwd = 3, xlim = c(0,80), ylim = c(-2,2), 
                            main = "Second Derivative of Linear Fall", col = "dodgerblue2", cex.lab = 1.5, cex.lab = 1.5) ##plot of second derivative

linear_fall_area <- integrate(linear_fall, 0, 80) ##area
linear_fall_area ##4000

linear_fall_min <- optimize(linear_fall, interval = c(0,80)) ##minimum
linear_fall_min$objective ##0
linear_fall_max <- optimize(linear_fall, interval = c(0,80), maximum = TRUE) ##maximum
linear_fall_max$objective ##100

##D1 unweighted sum
lf_D1_sum_unweight <- integrate(linear_fall_D1_fxn, 0, 80)
lf_D1_sum_unweight ##-100

##D1 weighted prime: finding sum of various integrals (ages 0-18, 18-30, 30-80)
integrate(linear_fall_D1_fxn, 0, 18) ##-22.5
integrate(linear_fall_D1_fxn, 18, 30) ##-15
-15*2 ##weighing the prime (ages 18-30) by 2 = -30
integrate(linear_fall_D1_fxn, 30, 80) ##-62.5
lf_D1_sum_weight_prime <- -22.5-30-62.5
lf_D1_sum_weight_prime ##-115

##D1 ascending weights: finding sum of various integrals (using weights of 0.25, 0.5, 0.75, and 1)
integrate(linear_fall_D1_fxn, 0, 20) ##-25
-25*0.25 ##-6.25
(integrate(linear_fall_D1_fxn, 20, 40)) ##-25
-25*0.5 ##-12.5
integrate(linear_fall_D1_fxn, 40, 60) ##-25
-25*0.75 ##-18.75
integrate(linear_fall_D1_fxn, 60, 80) ##-25
-25*1 ##-25
lf_D1_sum_weight_asc <- -6.25-12.5-18.75-25
lf_D1_sum_weight_asc ##-62.5

##D1 descending weights: finding sum of various integrals (using weights of 1, 0.75, 0.5, and 0.25)
integrate(linear_fall_D1_fxn, 0, 20) ##-25
-25*1 ##-25
(integrate(linear_fall_D1_fxn, 20, 40)) ##-25
-25*0.75 ##-18.75
integrate(linear_fall_D1_fxn, 40, 60) ##-25
-25*0.5 ##-12.5
integrate(linear_fall_D1_fxn, 60, 80) ##-25
-25*0.25 ##-6.25
lf_D1_sum_weight_des <- -25-18.75-12.5-6.25
lf_D1_sum_weight_des ##-62.5

##D1 weighted end of life: finding sum of various integrals (ages 0-60, 60-80)
integrate(linear_fall_D1_fxn, 0, 60) ##-75
integrate(linear_fall_D1_fxn, 60, 80) ##-25
-25*2 ##weighing the end of life (ages 60-80) by 2 = -50
lf_d1_sum_weight_end <- -75-50
lf_d1_sum_weight_end ##-125

##D1 unweighted and weighted averages (sum/80)
lf_D1_avg_unweight <- -100/80
lf_D1_avg_unweight ##-1.25
lf_D1_avg_weight_prime <- -115/80
lf_D1_avg_weight_prime ##-1.4375
lf_D1_avg_weight_asc <- -62.5/80
lf_D1_avg_weight_asc ##-0.78125
lf_D1_avg_weight_des <- -62.5/80
lf_D1_avg_weight_des ##-0.78125
lf_d1_avg_weight_end <- -125/80
lf_d1_avg_weight_end ##-1.5625

##D2: because the second derivative is zero, all the values below will also be zero
lf_d2_sum_unweight <- 0
lf_d2_sum_weight_prime <- 0
lf_d2_sum_weight_asc <- 0
lf_d2_sum_weight_des <- 0
lf_d2_sum_weight_end <- 0
lf_d2_avg_unweight <- 0
lf_d2_avg_weight_prime <- 0
lf_d2_avg_weight_asc <- 0
lf_d2_avg_weight_des <- 0
lf_d2_avg_weight_end <- 0

##manually generated minima, maxima, and total
##number_minima: 0
##number_maxima: 0
##number_extrema: 0

##3. Linear Low (at y = 0) ---------------------------------------------------------------------------------------------------------

linear_low <- function(x) {0*x+0} ##function
linear_low_plot <- plot(linear_low, lwd = 3, xlim = c(0,80), ylim = c(0,100), main = "Linear Low", 
                            xlab = "Age", ylab = "Meaning", col = "dodgerblue2", cex.lab = 1.5, cex.lab = 1.5) ##plot function
lifelinesZ[3, ]

linear_low_D1 <- D(0*x+0 ~ x) ##first derivative/slope
linear_low_D1 ##rewrite as function
linear_low_D1_fxn <- function(x) {0*x+0}
linear_low_D1_plot <- plot(linear_low_D1_fxn, lwd = 3, xlim = c(0,80), ylim = c(-10,10), 
                            main = "First Derivative of Linear Low", col = "dodgerblue2", cex.lab = 1.5, cex.lab = 1.5) ##plot of first derivative

linear_low_D2 <- D(0*x+0 ~ x & x) ##second derivative
linear_low_D2 ##rewrite as function
linear_low_D2_fxn <- function(x) {0*x+0}
linear_low_D2_plot <- plot(linear_low_D2_fxn, lwd = 3, xlim = c(0,80), ylim = c(-2,2), 
                            main = "Second Derivative of Linear Low", col = "dodgerblue2", cex.lab = 1.5, cex.lab = 1.5) ##plot of second derivative

linear_low_area <- integrate(linear_low, 0, 80) ##area
linear_low_area ##0

linear_low_min <- optimize(linear_low, interval = c(0,80)) ##minimum
linear_low_min$objective ##0
linear_low_max <- optimize(linear_low, interval = c(0,80), maximum = TRUE) ##maximum
linear_low_max$objective ##0

##D1: because the first derivative is zero, all the values below will also be zero
ll_D1_sum_unweight <- 0
ll_D1_sum_weight_prime <- 0
ll_D1_sum_weight_asc <- 0
ll_D1_sum_weight_des <- 0
ll_D1_sum_weight_end <- 0
ll_D1_avg_unweight <- 0
ll_D1_avg_weight_prime <- 0
ll_D1_avg_weight_asc <- 0
ll_D1_avg_weight_des <- 0
ll_D1_avg_weight_end <- 0

##D2: because the second derivative is zero, all the values below will also be zero
ll_d2_sum_unweight <- 0
ll_d2_sum_weight_prime <- 0
ll_d2_sum_weight_asc <- 0
ll_d2_sum_weight_des <- 0
ll_d2_sum_weight_end <- 0
ll_d2_avg_unweight <- 0
ll_d2_avg_weight_prime <- 0
ll_d2_avg_weight_asc <- 0
ll_d2_avg_weight_des <- 0
ll_d2_avg_weight_end <- 0

##manually generated minima, maxima, and total
##number_minima: 0
##number_maxima: 0
##number_extrema: 0

##4. Linear Middle (at y = 50) (Which Way is Up?) ---------------------------------------------------------------------------------------------------------

linear_middle <- function(x) {0*x+50} ##function
linear_middle_plot <- plot(linear_middle, lwd = 3, xlim = c(0,80), ylim = c(0,100), main = "Linear Middle", 
                           xlab = "Age", ylab = "Meaning", col = "dodgerblue2", cex.lab = 1.5, cex.lab = 1.5) ##plot function
mysubtitle = "(Which Way is Up?)"
mtext(line = 0.5, mysubtitle)
lifelinesZ[4, ]

linear_middle_D1 <- D(0*x+50 ~ x) ##first derivative/slope
linear_middle_D1 ##rewrite as function
linear_middle_D1_fxn <- function(x) {0*x+0}
linear_middle_D1_plot <- plot(linear_middle_D1_fxn, lwd = 3, xlim = c(0,80), ylim = c(-10,10), 
                           main = "First Derivative of Linear Middle", col = "dodgerblue2", cex.lab = 1.5, cex.lab = 1.5) ##plot of first derivative
mysubtitle = "(Which Way is Up?)"
mtext(line = 0.5, mysubtitle)

linear_middle_D2 <- D(0*x+50 ~ x & x) ##second derivative
linear_middle_D2 ##rewrite as function
linear_middle_D2_fxn <- function(x) {0*x+0}
linear_middle_D2_plot <- plot(linear_middle_D2_fxn, lwd = 3, xlim = c(0,80), ylim = c(-2,2), 
                           main = "Second Derivative of Linear Middle", col = "dodgerblue2", cex.lab = 1.5, cex.lab = 1.5) ##plot of second derivative
mysubtitle = "(Which Way is Up?)"
mtext(line = 0.5, mysubtitle)

linear_middle_area <- integrate(linear_middle, 0, 80) ##area
linear_middle_area ##4000

linear_middle_min <- optimize(linear_middle, interval = c(0,80)) ##minimum
linear_middle_min$objective ##50 
linear_middle_max <- optimize(linear_middle, interval = c(0,80), maximum = TRUE) ##maximum
linear_middle_max$objective ##50

##D1: because the first derivative is zero, all the values below will also be zero
lm_D1_sum_unweight <- 0
lm_D1_sum_weight_prime <- 0
lm_D1_sum_weight_asc <- 0
lm_D1_sum_weight_des <- 0
lm_D1_sum_weight_end <- 0
lm_D1_avg_unweight <- 0
lm_D1_avg_weight_prime <- 0
lm_D1_avg_weight_asc <- 0
lm_D1_avg_weight_des <- 0
lm_D1_avg_weight_end <- 0

##D2: because the second derivative is zero, all the values below will also be zero
lm_d2_sum_unweight <- 0
lm_d2_sum_weight_prime <- 0
lm_d2_sum_weight_asc <- 0
lm_d2_sum_weight_des <- 0
lm_d2_sum_weight_end <- 0
lm_d2_avg_unweight <- 0
lm_d2_avg_weight_prime <- 0
lm_d2_avg_weight_asc <- 0
lm_d2_avg_weight_des <- 0
lm_d2_avg_weight_end <- 0

##manually generated minima, maxima, and total
##number_minima: 0
##number_maxima: 0
##number_extrema: 0

##5. Linear High (at y = 100) ---------------------------------------------------------------------------------------------------------

linear_high <- function(x) {0*x+100} ##function
linear_high_plot <- plot(linear_high, lwd = 3, xlim = c(0,80), ylim = c(0,100), main = "Linear High", 
                              xlab = "Age", ylab = "Meaning", col = "dodgerblue2", cex.lab = 1.5) ##plot function
lifelinesZ[5, ]

linear_high_D1 <- D(0*x+100 ~ x) ##first derivative/slope
linear_high_D1 ##rewrite as function
linear_high_D1_fxn <- function(x) {0*x+0}
linear_high_D1_plot <- plot(linear_high_D1_fxn, lwd = 3, xlim = c(0,80), ylim = c(-10,10), 
                           main = "First Derivative of Linear High", col = "dodgerblue2", cex.lab = 1.5) ##plot of first derivative

linear_high_D2 <- D(0*x+100 ~ x & x) ##second derivative
linear_high_D2 ##rewrite as function
linear_high_D2_fxn <- function(x) {0*x+0}
linear_high_D2_plot <- plot(linear_high_D2_fxn, lwd = 3, xlim = c(0,80), ylim = c(-2,2), 
                           main = "Second Derivative of Linear High", col = "dodgerblue2", cex.lab = 1.5) ##plot of second derivative

linear_high_area <- integrate(linear_high, 0, 80) ##area
linear_high_area ##8000

linear_high_min <- optimize(linear_high, interval = c(0,80)) ##minimum
linear_high_min$objective ##100
linear_high_max <- optimize(linear_high, interval = c(0,80), maximum = TRUE) ##maximum
linear_high_max$objective ##100

##D1: because the first derivative is zero, all the values below will also be zero
lh_D1_sum_unweight <- 0
lh_D1_sum_weight_prime <- 0
lh_D1_sum_weight_asc <- 0
lh_D1_sum_weight_des <- 0
lh_D1_sum_weight_end <- 0
lh_D1_avg_unweight <- 0
lh_D1_avg_weight_prime <- 0
lh_D1_avg_weight_asc <- 0
lh_D1_avg_weight_des <- 0
lh_D1_avg_weight_end <- 0

##D2: because the second derivative is zero, all the values below will also be zero
lh_d2_sum_unweight <- 0
lh_d2_sum_weight_prime <- 0
lh_d2_sum_weight_asc <- 0
lh_d2_sum_weight_des <- 0
lh_d2_sum_weight_end <- 0
lh_d2_avg_unweight <- 0
lh_d2_avg_weight_prime <- 0
lh_d2_avg_weight_asc <- 0
lh_d2_avg_weight_des <- 0
lh_d2_avg_weight_end <- 0

##manually generated minima, maxima, and total
##number_minima: 0
##number_maxima: 0
##number_extrema: 0

##Exponential ---------------------------------------------------------------------------------------------------------

##6. Exponential Rise (Convex) ---------------------------------------------------------------------------------------------------------

exp_rise_convex <- function(x) {1.0595^x-1} ##function
exp_rise_convex_plot <- plot(exp_rise_convex, lwd = 3, xlim = c(0,80), ylim = c(0,100), main = "Exponential Rise (Convex)", 
                            xlab = "Age", ylab = "Meaning", col = "dodgerblue2", cex.lab = 1.5) ##plot function
lifelinesZ[6, ]

exp_rise_convex_D1 <- D(1.0595^x-1 ~ x) ##first derivative
exp_rise_convex_D1
exp_rise_convex_D1_plot <- plot(exp_rise_convex_D1, lwd = 3, xlim = c(0,80), ylim = c(-10,10), 
                                main = "First Derivative of Exponential Rise (Convex)", col = "dodgerblue2", cex.lab = 1.5) ##plot of first derivative
exp_rise_convex_D2 <- D(1.0595^x-1 ~ x & x) ##second derivative
exp_rise_convex_D2
exp_rise_convex_D2_plot <- plot(exp_rise_convex_D2, lwd = 3, xlim = c(0,80), ylim = c(-2,2), 
                           main = "Second Derivative of Exponential Rise (Convex)", col = "dodgerblue2", cex.lab = 1.5) ##plot of second derivative

exp_rise_convex_area <- integrate(exp_rise_convex, 0, 80) ##area
exp_rise_convex_area ##1665.367

exp_rise_convex_min <- optimize(exp_rise_convex, interval = c(0,80)) ##minimum
exp_rise_convex_min$objective ##0
exp_rise_convex_max <- optimize(exp_rise_convex, interval = c(0,80), maximum = TRUE) ##maximum
exp_rise_convex_max$objective ##100

##D1 unweighted sum
erc_D1_sum_unweight <- integrate(exp_rise_convex_D1, 0, 80)
erc_D1_sum_unweight ##100.8772

##D1 weighted prime: finding sum of various integrals (ages 0-18, 18-30, 30-80)
integrate(exp_rise_convex_D1, 0, 18) ##1.830201
integrate(exp_rise_convex_D1, 18, 30) ##2.832568
2.832568*2 ##weighing the prime (ages 18-30) by 2 = 5.665136
integrate(exp_rise_convex_D1, 30, 80) ##96.2144
erc_D1_sum_weight_prime <- 1.830201+5.665136+96.2144
erc_D1_sum_weight_prime ##103.7097

##D1 ascending weights: finding sum of various integrals (using weights of 0.25, 0.5, 0.75, and 1)
integrate(exp_rise_convex_D1, 0, 20) ##2.177015
2.177015*0.25 ##0.5442537
integrate(exp_rise_convex_D1, 20, 40) ##6.916408
6.916408*0.5 ##3.458204
integrate(exp_rise_convex_D1, 40, 60) ##21.97353
21.97353*0.75 ##16.48015
integrate(exp_rise_convex_D1, 60, 80) ##69.81022
69.81022*1 ##69.81022
erc_D1_sum_weight_asc <- 0.5442537+3.458204+16.48015+69.81022
erc_D1_sum_weight_asc ##90.29283

##D1 descending weights: finding sum of various integrals (using weights of 1, 0.75, 0.5, and 0.25)
integrate(exp_rise_convex_D1, 0, 20) ##2.177015
2.177015*1 ##2.177015
integrate(exp_rise_convex_D1, 20, 40) ##6.916408
6.916408*0.75 ##5.187306
integrate(exp_rise_convex_D1, 40, 60) ##21.97353
21.97353*0.5 ##10.98677
integrate(exp_rise_convex_D1, 60, 80) ##69.81022
69.81022*0.25 ##17.45256
erc_D1_sum_weight_des <- 2.177015+5.187306+10.98677+17.45256
erc_D1_sum_weight_des ##35.80365

##D1 weighted end of life: finding sum of various integrals (ages 0-60, 60-80)
integrate(exp_rise_convex_D1, 0, 60) ##31.06695
integrate(exp_rise_convex_D1, 60, 80) ##69.81022
69.81022*2 ##weighing the end of life (ages 60-80) by 2 = 139.6204
erc_D1_sum_weight_end <- 31.06695+139.6204
erc_D1_sum_weight_end ##170.6873

##D1 unweighted and weighted averages (sum/80)
erc_D1_avg_unweight <- 100.8772/80
erc_D1_avg_unweight ##1.260965
erc_D1_avg_weight_prime <- 103.7097/80
erc_D1_avg_weight_prime ##1.296371
erc_D1_avg_weight_asc <- 90.29283/80
erc_D1_avg_weight_asc ##1.12866
erc_D1_avg_weight_des <- 35.80365/80
erc_D1_avg_weight_des ##0.4475456
erc_D1_avg_weight_end <- 170.6873/80
erc_D1_avg_weight_end ##2.133591

##D2 unweighted sum
erc_d2_sum_unweight <- integrate(exp_rise_convex_D2, 0, 80)
erc_d2_sum_unweight ##5.830408

##D2 weighted prime: finding sum of various integrals (ages 0-18, 18-30, 30-80)
integrate(exp_rise_convex_D2, 0, 18) ##0.1057803
integrate(exp_rise_convex_D2, 18, 30) ##0.1637142
0.1637142*2 ##weighing the prime (ages 18-30) by 2 = 0.3274284
integrate(exp_rise_convex_D2, 30, 80) ##5.560913
erc_d2_sum_weight_prime <- 0.1057803+0.3274284+5.560913
erc_d2_sum_weight_prime ##5.994122

##D2 ascending weights: finding sum of various integrals (using weights of 0.25, 0.5, 0.75, and 1)
integrate(exp_rise_convex_D2, 0, 20) ##0.1258251
0.1258251*0.25 ##0.03145627
integrate(exp_rise_convex_D2, 20, 40) ##0.3997483
0.3997483*0.5 ##0.1998742
integrate(exp_rise_convex_D2, 40, 60) ##1.270006
1.270006*0.75 ##0.9525045
integrate(exp_rise_convex_D2, 60, 80) ##4.034828
4.034828*1 ##4.034828
erc_d2_sum_weight_asc <- 0.03145627+0.1998742+0.9525045+4.034828
erc_d2_sum_weight_asc ##5.218663

##D2 descending weights: finding sum of various integrals (using weights of 1, 0.75, 0.5, and 0.25)
integrate(exp_rise_convex_D2, 0, 20) ##0.1258251
0.1258251*1 ##0.1258251
integrate(exp_rise_convex_D2, 20, 40) ##0.3997483
0.3997483*0.75 ##0.2998112
integrate(exp_rise_convex_D2, 40, 60) ##1.270006
1.270006*0.5 ##0.635003
integrate(exp_rise_convex_D2, 60, 80) ##4.034828
4.034828*0.25 ##1.008707
erc_d2_sum_weight_des <- 0.1258251+0.2998112+0.635003+1.008707
erc_d2_sum_weight_des ##2.069346

##D2 weighted end of life: finding sum of various integrals (ages 0-60, 60-80)
integrate(exp_rise_convex_D2, 0, 60) ##1.79558
integrate(exp_rise_convex_D2, 60, 80) ##4.034828
4.034828*2 ##weighing the end of life (ages 60-80) by 2 = 8.069656
erc_d2_sum_weight_end <- 1.79558+8.069656
erc_d2_sum_weight_end ##9.865236

##D2 unweighted and weighted averages (sum/80)
erc_d2_avg_unweight <- 5.830408/80
erc_d2_avg_unweight ##0.0728801
erc_d2_avg_weight_prime <- 5.994122/80
erc_d2_avg_weight_prime ##0.07492652
erc_d2_avg_weight_asc <- 5.218663/80
erc_d2_avg_weight_asc ##0.06523329
erc_d2_avg_weight_des <- 2.069346/80
erc_d2_avg_weight_des ##0.02586682
erc_d2_avg_weight_end <- 9.865236/80
erc_d2_avg_weight_end ##0.1233154

##manually generated minima, maxima, and total
##number_minima: 0
##number_maxima: 0
##number_extrema: 0

##7. Exponential Fall (Convex) ---------------------------------------------------------------------------------------------------------

exp_fall_convex <- function(x) {1.0595^(-x+80)-1} ##function
exp_fall_convex_plot <- plot(exp_fall_convex, lwd = 3, xlim = c(0,80), ylim = c(0,100), main = "Exponential Fall (Convex)", 
                                xlab = "Age", ylab = "Meaning", col = "dodgerblue2", cex.lab = 1.5) ##plot function
lifelinesZ[7, ]

exp_fall_convex_D1 <- D(1.0595^(-x+80)-1 ~ x) ##first derivative
exp_fall_convex_D1
exp_fall_convex_D1_plot <- plot(exp_fall_convex_D1, lwd = 3, xlim = c(0,80), ylim = c(-10,10), 
                                main = "First Derivative of Exponential Fall (Convex)", col = "dodgerblue2", cex.lab = 1.5) ##plot of first derivative
exp_fall_convex_D2 <- D(1.0595^(-x+80)-1 ~ x & x) ##second derivative
exp_fall_convex_D2
exp_fall_convex_D2_plot <- plot(exp_fall_convex_D2, lwd = 3, xlim = c(0,80), ylim = c(-2,2), 
                           main = "Second Derivative of Exponential Fall (Convex)", col = "dodgerblue2", cex.lab = 1.5) ##plot of second derivative

exp_fall_convex_area <- integrate(exp_fall_convex, 0, 80) ##area
exp_fall_convex_area ##1665.367

exp_fall_convex_min <- optimize(exp_fall_convex, interval = c(0,80)) ##minimum
exp_fall_convex_min$objective ##0  
exp_fall_convex_max <- optimize(exp_fall_convex, interval = c(0,80), maximum = TRUE) ##maximum
exp_fall_convex_max$objective ##100

##D1 unweighted sum
efc_D1_sum_unweight <- integrate(exp_fall_convex_D1, 0, 80)
efc_D1_sum_unweight ##-100.8772

##D1 weighted prime: finding sum of various integrals (ages 0-18, 18-30, 30-80)
integrate(exp_fall_convex_D1, 0, 18) ##-65.88073
integrate(exp_fall_convex_D1, 18, 30) ##-18.00574
-18.00574*2 ##weighing the prime (ages 18-30) by 2 = -36.01148
integrate(exp_fall_convex_D1, 30, 80) ##-16.9907
efc_D1_sum_weight_prime <- -65.88073-36.01148-16.9907
efc_D1_sum_weight_prime ##-118.8829

##D1 ascending weights: finding sum of various integrals (using weights of 0.25, 0.5, 0.75, and 1)
integrate(exp_fall_convex_D1, 0, 20) ##-69.81022
-69.81022*0.25 ##-17.45256
integrate(exp_fall_convex_D1, 20, 40) ##-21.97353
-21.97353*0.5 ##-10.98677
integrate(exp_fall_convex_D1, 40, 60) ##-6.916408
-6.916408*0.75 ##-5.187306
integrate(exp_fall_convex_D1, 60, 80) ##-2.177015
-2.177015*1 ##-2.177015
efc_D1_sum_weight_asc <- -17.45256-10.98677-5.187306-2.177015
efc_D1_sum_weight_asc ##-35.80365

##D1 descending weights: finding sum of various integrals (using weights of 1, 0.75, 0.5, and 0.25)
integrate(exp_fall_convex_D1, 0, 20) ##-69.81022
-69.81022*1 ##-69.81022
integrate(exp_fall_convex_D1, 20, 40) ##-21.97353
-21.97353*0.75 ##-16.48015
integrate(exp_fall_convex_D1, 40, 60) ##-6.916408
-6.916408*0.5 ##-3.458204
integrate(exp_fall_convex_D1, 60, 80) ##-2.177015
-2.177015*0.25 ##-0.5442537
efc_D1_sum_weight_des <- -69.81022-16.48015-3.458204-0.5442537
efc_D1_sum_weight_des ##-90.29283

##D1 weighted end of life: finding sum of various integrals (ages 0-60, 60-80)
integrate(exp_fall_convex_D1, 0, 60) ##-98.70016
integrate(exp_fall_convex_D1, 60, 80) ##-2.177015
-2.177015*2 ##weighing the end of life (ages 60-80) by 2 = -4.35403
efc_D1_sum_weight_end <- -98.70016-4.35403
efc_D1_sum_weight_end ##-103.0542

##D1 unweighted and weighted averages (sum/80)
efc_D1_avg_unweight <- -100.8772/80
efc_D1_avg_unweight ##-1.260965
efc_D1_avg_weight_prime <- -118.8829/80
efc_D1_avg_weight_prime ##-1.486036
efc_D1_avg_weight_asc <- -35.80365/80
efc_D1_avg_weight_asc ##-0.4475456
efc_D1_avg_weight_des <- -90.29283/80
efc_D1_avg_weight_des ##-1.12866
efc_d1_avg_weight_end <- 103.0542/80
efc_d1_avg_weight_end ##1.288177

##D2 unweighted sum
efc_d2_sum_unweight <- integrate(exp_fall_convex_D2, 0, 80)
efc_d2_sum_unweight ##5.830408

##D2 weighted prime: finding sum of various integrals (ages 0-18, 18-30, 30-80)
integrate(exp_fall_convex_D2, 0, 18) ##3.807715
integrate(exp_fall_convex_D2, 18, 30) ##1.04068
1.04068*2 ##weighing the prime (ages 18-30) by 2 = 2.08136
integrate(exp_fall_convex_D2, 30, 80) ##0.9820131
efc_d2_sum_weight_prime <- 3.807715+2.08136+0.9820131
efc_d2_sum_weight_prime ##6.871088

##D2 ascending weights: finding sum of various integrals (using weights of 0.25, 0.5, 0.75, and 1)
integrate(exp_fall_convex_D2, 0, 20) ##4.034828
4.034828*0.25 ##1.008707
integrate(exp_fall_convex_D2, 20, 40) ##1.270006
1.270006*0.5 ##0.635003
integrate(exp_fall_convex_D2, 40, 60) ##0.3997483
0.3997483*0.75 ##0.2998112
integrate(exp_fall_convex_D2, 60, 80) ##0.1258251
0.1258251*1 ##0.1258251
efc_d2_sum_weight_asc <- 1.008707+0.635003+0.2998112+0.1258251
efc_d2_sum_weight_asc ##2.069346

##D2 descending weights: finding sum of various integrals (using weights of 1, 0.75, 0.5, and 0.25)
integrate(exp_fall_convex_D2, 0, 20) ##4.034828
4.034828*1 ##4.034828
integrate(exp_fall_convex_D2, 20, 40) ##1.270006
1.270006*0.75 ##0.9525045
integrate(exp_fall_convex_D2, 40, 60) ##0.3997483
0.3997483*0.5 ##0.1998742
integrate(exp_fall_convex_D2, 60, 80) ##0.1258251
0.1258251*0.25 ##0.03145627
efc_d2_sum_weight_des <- 4.034828+0.9525045+0.1998742+0.03145627
efc_d2_sum_weight_des ##5.218663

##D2 weighted end of life: finding sum of various integrals (ages 0-60, 60-80)
integrate(exp_fall_convex_D2, 0, 60) ##5.704583
integrate(exp_fall_convex_D2, 60, 80) ##0.1258251
0.1258251*2 ##weighing the end of life (ages 60-80) by 2 = 0.2516502
efc_d2_sum_weight_end <- 5.704583+0.2516502
efc_d2_sum_weight_end ##5.956233

##D2 unweighted and weighted averages (sum/80)
efc_d2_avg_unweight <- 5.830408/80
efc_d2_avg_unweight ##0.0728801
efc_d2_avg_weight_prime <- 6.871088/80
efc_d2_avg_weight_prime ##0.0858886
efc_d2_avg_weight_asc <- 2.069346/80
efc_d2_avg_weight_asc ##0.02586682
efc_d2_avg_weight_des <- 5.218663/80
efc_d2_avg_weight_des ##0.06523329
efc_d2_avg_weight_end <- 5.956233/80
efc_d2_avg_weight_end ##0.07445291

##manually generated minima, maxima, and total
##number_minima: 0
##number_maxima: 0
##number_extrema: 0

##8. Exponential Rise (Concave) ---------------------------------------------------------------------------------------------------------

exp_rise_concave <- function(x) {100-1.0595^(-x+80)+1} ##function
exp_rise_concave_plot <- plot(exp_rise_concave, lwd = 3, xlim = c(0,80), ylim = c(0,100), main = "Exponential Rise (Concave)", 
                            xlab = "Age", ylab = "Meaning", col = "dodgerblue2", cex.lab = 1.5) ##plot function
lifelinesZ[8, ]

exp_rise_concave_D1 <- D(100-1.0595^(-x+80)+1 ~ x) ##first derivative
exp_rise_concave_D1
exp_rise_concave_D1_plot <- plot(exp_rise_concave_D1, lwd = 3, xlim = c(0,80), ylim = c(-10,10), 
                                    main = "First Derivative of Exponential Rise (Concave)", col = "dodgerblue2", cex.lab = 1.5)
exp_rise_concave_D2 <- D(100-1.0595^(-x+80)+1 ~ x & x) ##second derivative
exp_rise_concave_D2
exp_rise_concave_D2_plot <- plot(exp_rise_concave_D2, lwd = 3, xlim = c(0,80), ylim = c(-2,2), 
                                    main = "Second Derivative of Exponential Rise (Concave)", col = "dodgerblue2", cex.lab = 1.5)

exp_rise_concave_area <- integrate(exp_rise_concave, 0, 80) ##area
exp_rise_concave_area ##6334.633

exp_rise_concave_min <- optimize(exp_rise_concave, interval = c(0,80)) ##minimum
exp_rise_concave_min$objective ##0
exp_rise_concave_max <- optimize(exp_rise_concave, interval = c(0,80), maximum = TRUE) ##maximum
exp_rise_concave_max$objective ##100

##D1 unweighted sum
ercc_d1_sum_unweight <- integrate(exp_rise_concave_D1, 0, 80)
ercc_d1_sum_unweight ##100.8772

##D1 weighted prime: finding sum of various integrals (ages 0-18, 18-30, 30-80)
integrate(exp_rise_concave_D1, 0, 18) ##65.88073
integrate(exp_rise_concave_D1, 18, 30) ##18.00574
18.00574*2 ##weighing the prime (ages 18-30) by 2 = 36.01148
integrate(exp_rise_concave_D1, 30, 80) ##16.9907
ercc_d1_sum_weight_prime <- 65.88073+36.01148+16.9907
ercc_d1_sum_weight_prime ##118.8829

##D1 ascending weights: finding sum of various integrals (using weights of 0.25, 0.5, 0.75, and 1)
integrate(exp_rise_concave_D1, 0, 20) ##69.81022
69.81022*0.25 ##17.45256
integrate(exp_rise_concave_D1, 20, 40) ##21.97353
21.97353*0.5 ##10.98677
integrate(exp_rise_concave_D1, 40, 60) ##6.916408
6.916408*0.75 ##5.187306
integrate(exp_rise_concave_D1, 60, 80) ##
2.177015*1 ##2.177015
ercc_d1_sum_weight_asc <- 17.45256+10.98677+5.187306+2.177015
ercc_d1_sum_weight_asc ##35.80365

##D1 descending weights: finding sum of various integrals (using weights of 1, 0.75, 0.5, and 0.25)
integrate(exp_rise_concave_D1, 0, 20) ##69.81022
69.81022*1 ##69.81022
integrate(exp_rise_concave_D1, 20, 40) ##21.97353
21.97353*0.75 ##16.48015
integrate(exp_rise_concave_D1, 40, 60) ##6.916408
6.916408*0.5 ##3.458204
integrate(exp_rise_concave_D1, 60, 80) ##2.177015
2.177015*0.25 ##0.5442537
ercc_d1_sum_weight_des <- 69.81022+16.48015+3.458204+0.5442537
ercc_d1_sum_weight_des ##90.29283

##D1 weighted end of life: finding sum of various integrals (ages 0-60, 60-80)
integrate(exp_rise_concave_D1, 0, 60) ##98.70016
integrate(exp_rise_concave_D1, 60, 80) ##2.177015
2.177015*2 ##weighing the end of life (ages 60-80) by 2 = 4.35403
ercc_d1_sum_weight_end <- 98.70016+4.35403
ercc_d1_sum_weight_end ##103.0542

##D1 unweighted and weighted averages (sum/80)
ercc_d1_avg_unweight <- 100.8772/80
ercc_d1_avg_unweight ##1.260965
ercc_d1_avg_weight_prime <- 118.8829/80
ercc_d1_avg_weight_prime ##1.486036
ercc_d1_avg_weight_asc <- 35.80365/80
ercc_d1_avg_weight_asc ##0.4475456
ercc_d1_avg_weight_des <- 90.29283/80
ercc_d1_avg_weight_des ##1.12866
ercc_d1_avg_weight_end <- 103.0542/80
ercc_d1_avg_weight_end ##1.288177

##D2 unweighted sum
ercc_d2_sum_unweight <- integrate(exp_rise_concave_D2, 0, 80)
ercc_d2_sum_unweight ##-5.830408

##D2 weighted prime: finding sum of various integrals (ages 0-18, 18-30, 30-80)
integrate(exp_rise_concave_D2, 0, 18) ##-3.807715
integrate(exp_rise_concave_D2, 18, 30) ##-1.04068
-1.04068*2 ##weighing the prime (ages 18-30) by 2 = -2.08136
integrate(exp_rise_concave_D2, 30, 80) ##-0.9820131
ercc_d2_sum_weight_prime <- -3.807715-2.08136-0.9820131
ercc_d2_sum_weight_prime ##-6.871088

##D2 ascending weights: finding sum of various integrals (using weights of 0.25, 0.5, 0.75, and 1)
integrate(exp_rise_concave_D2, 0, 20) ##-4.034828
-4.034828*0.25 ##-1.008707
integrate(exp_rise_concave_D2, 20, 40) ##-1.270006
-1.270006*0.5 ##-0.635003
integrate(exp_rise_concave_D2, 40, 60) ##-0.3997483
-0.3997483*0.75 ##-0.2998112
integrate(exp_rise_concave_D2, 60, 80) ##-0.1258251
-0.1258251*1 ##-0.1258251
ercc_d2_sum_weight_asc <- -1.008707-0.635003-0.2998112-0.1258251
ercc_d2_sum_weight_asc ##-2.069346

##D2 descending weights: finding sum of various integrals (using weights of 1, 0.75, 0.5, and 0.25)
integrate(exp_rise_concave_D2, 0, 20) ##-4.034828
-4.034828*1 ##-4.034828
integrate(exp_rise_concave_D2, 20, 40) ##-1.270006
-1.270006*0.75 ##-0.9525045
integrate(exp_rise_concave_D2, 40, 60) ##-0.3997483
-0.3997483*0.5 ##-0.1998742
integrate(exp_rise_concave_D2, 60, 80) ##-0.1258251
-0.1258251*0.25 ##-0.03145627
ercc_d2_sum_weight_des <- -4.034828-0.9525045-0.1998742-0.03145627
ercc_d2_sum_weight_des ##-5.218663

##D2 weighted end of life: finding sum of various integrals (ages 0-60, 60-80)
integrate(exp_rise_concave_D2, 0, 60) ##-5.704583
integrate(exp_rise_concave_D2, 60, 80) ##-0.1258251
-0.1258251*2 ##weighing the end of life (ages 60-80) by 2 = -0.2516502
ercc_d2_sum_weight_end <- -5.704583-0.2516502
ercc_d2_sum_weight_end ##-5.956233

##D2 unweighted and weighted averages (sum/80)
ercc_d2_avg_unweight <- -5.830408/80
ercc_d2_avg_unweight ##-0.0728801
ercc_d2_avg_weight_prime <- -6.871088/80
ercc_d2_avg_weight_prime ##-0.0858886
ercc_d2_avg_weight_asc <- -2.069346/80
ercc_d2_avg_weight_asc ##-0.02586682
ercc_d2_avg_weight_des <- -5.218663/80
ercc_d2_avg_weight_des ##-0.06523329
ercc_d2_avg_weight_end <- -5.956233/80
ercc_d2_avg_weight_end ##-0.07445291

##manually generated minima, maxima, and total
##number_minima: 0
##number_maxima: 0
##number_extrema: 0

##9. Exponential Fall (Concave) (From Bad to Worse) ---------------------------------------------------------------------------------------------------------

exp_fall_concave <- function(x) {100-1.0595^x+1} ##function
exp_fall_concave_plot <- plot(exp_fall_concave, lwd = 3, xlim = c(0,80), ylim = c(0,100), main = "Exponential Fall (Concave)", 
                                 xlab = "Age", ylab = "Meaning", col = "dodgerblue2", cex.lab = 1.5) ##plot function
mysubtitle = "(From Bad to Worse)"
mtext(line = 0.5, mysubtitle)
lifelinesZ[9, ]

exp_fall_concave_D1 <- D(100-1.0595^x+1 ~ x) ##first derivative
exp_fall_concave_D1
exp_fall_concave_D1_plot <- plot(exp_fall_concave_D1, lwd = 3, xlim = c(0,80), ylim = c(-10,10), 
                                    main = "First Derivative of Exponential Fall (Concave)", col = "dodgerblue2", cex.lab = 1.5)
mysubtitle = "(From Bad to Worse)"
mtext(line = 0.5, mysubtitle)

exp_fall_concave_D2 <- D(100-1.0595^x+1 ~ x & x) ##second derivative
exp_fall_concave_D2
exp_fall_concave_D2_plot <- plot(exp_fall_concave_D2, lwd = 3, xlim = c(0,80), ylim = c(-2,2), 
                                    main = "Second Derivative of Exponential Fall (Concave)", col = "dodgerblue2", cex.lab = 1.5)
mysubtitle = "(From Bad to Worse)"
mtext(line = 0.5, mysubtitle)

exp_fall_concave_area <- integrate(exp_fall_concave, 0, 80) ##area
exp_fall_concave_area ##6334.633

exp_fall_concave_min <- optimize(exp_fall_concave, interval = c(0,80)) ##minimum
exp_fall_concave_min$objective ##0
exp_fall_concave_max <- optimize(exp_fall_concave, interval = c(0,80), maximum = TRUE) ##maximum
exp_fall_concave_max$objective ##100

##D1 unweighted sum
efcc_d1_sum_unweight <- integrate(exp_fall_concave_D1, 0, 80)
efcc_d1_sum_unweight ##-100.8772

##D1 weighted prime: finding sum of various integrals (ages 0-18, 18-30, 30-80)
integrate(exp_fall_concave_D1, 0, 18) ##-1.830201
integrate(exp_fall_concave_D1, 18, 30) ##-2.832568
-2.832568*2 ##weighing the prime (ages 18-30) by 2 = -5.665136
integrate(exp_fall_concave_D1, 30, 80) ##-96.2144
efcc_d1_sum_weight_prime <- -1.830201-5.665136-96.2144
efcc_d1_sum_weight_prime ##-103.7097

##D1 ascending weights: finding sum of various integrals (using weights of 0.25, 0.5, 0.75, and 1)
integrate(exp_fall_concave_D1, 0, 20) ##-2.177015
-2.177015*0.25 ##-0.5442537
integrate(exp_fall_concave_D1, 20, 40) ##-6.916408
-6.916408*0.5 ##-3.458204
integrate(exp_fall_concave_D1, 40, 60) ##-21.97353
-21.97353*0.75 ##-16.48015
integrate(exp_fall_concave_D1, 60, 80) ##-69.81022
-69.81022*1 ##-69.81022
efcc_d1_sum_weight_asc <- -0.5442537-3.458204-16.48015-69.81022
efcc_d1_sum_weight_asc ##-90.29283

##D1 descending weights: finding sum of various integrals (using weights of 1, 0.75, 0.5, and 0.25)
integrate(exp_fall_concave_D1, 0, 20) ##-2.177015
-2.177015*1 ##-2.177015
integrate(exp_fall_concave_D1, 20, 40) ##-6.916408
-6.916408*0.75 ##-5.187306
integrate(exp_fall_concave_D1, 40, 60) ##-21.97353
-21.97353*0.5 ##-10.98677
integrate(exp_fall_concave_D1, 60, 80) ##-69.81022
-69.81022*0.25 ##-17.45256
efcc_d1_sum_weight_des <- -2.177015-5.187306-10.98677-17.45256
efcc_d1_sum_weight_des ##-35.80365

##D1 weighted end of life: finding sum of various integrals (ages 0-60, 60-80)
integrate(exp_fall_concave_D1, 0, 60) ##-31.06695
integrate(exp_fall_concave_D1, 60, 80) ##-69.81022
-69.81022*2 ##weighing the end of life (ages 60-80) by 2 = -139.6204
efcc_d1_sum_weight_end <- -31.06695-139.6204
efcc_d1_sum_weight_end ##-170.6873

##D1 unweighted and weighted averages (sum/80)
efcc_d1_avg_unweight <- -100.8772/80
efcc_d1_avg_unweight ##-1.260965
efcc_d1_avg_weight_prime <- -103.7097/80
efcc_d1_avg_weight_prime ##-1.296371
efcc_d1_avg_weight_asc <- -90.29283/80
efcc_d1_avg_weight_asc ##-1.12866
efcc_d1_avg_weight_des <- -35.80365/80
efcc_d1_avg_weight_des ##-0.4475456
efcc_d1_avg_weight_end <- -170.6873/80
efcc_d1_avg_weight_end ##-2.133591

##D2 unweighted sum
efcc_d2_sum_unweight <- integrate(exp_fall_concave_D2, 0, 80)
efcc_d2_sum_unweight ##-5.830408

##D2 weighted prime: finding sum of various integrals (ages 0-18, 18-30, 30-80)
integrate(exp_fall_concave_D2, 0, 18) ##-0.1057803
integrate(exp_fall_concave_D2, 18, 30) ##-0.1637142
-0.1637142*2 ##weighing the prime (ages 18-30) by 2 = -0.3274284
integrate(exp_fall_concave_D2, 30, 80) ##-5.560913
efcc_d2_sum_weight_prime <- -0.1057803-0.3274284-5.560913
efcc_d2_sum_weight_prime ##-5.994122

##D2 ascending weights: finding sum of various integrals (using weights of 0.25, 0.5, 0.75, and 1)
integrate(exp_fall_concave_D2, 0, 20) ##-0.1258251
-0.1258251*0.25 ##-0.03145627
integrate(exp_fall_concave_D2, 20, 40) ##-0.3997483
-0.3997483*0.5 ##-0.1998742
integrate(exp_fall_concave_D2, 40, 60) ##-1.270006
-1.270006*0.75 ##-0.9525045
integrate(exp_fall_concave_D2, 60, 80) ##-4.034828
-4.034828*1 ##-4.034828
efcc_d2_sum_weight_asc <- -0.03145627-0.1998742-0.9525045-4.034828
efcc_d2_sum_weight_asc ##-5.218663

##D2 descending weights: finding sum of various integrals (using weights of 1, 0.75, 0.5, and 0.25)
integrate(exp_fall_concave_D2, 0, 20) ##-0.1258251
-0.1258251*1 ##-0.1258251
integrate(exp_fall_concave_D2, 20, 40) ##-0.3997483
-0.3997483*0.75 ##-0.2998112
integrate(exp_fall_concave_D2, 40, 60) ##-1.270006
-1.270006*0.5 ##-0.635003
integrate(exp_fall_concave_D2, 60, 80) ##-4.034828
-4.034828*0.25 ##-1.008707
efcc_d2_sum_weight_des <- -0.1258251-0.2998112-0.635003-1.008707
efcc_d2_sum_weight_des ##-2.069346

##D2 weighted end of life: finding sum of various integrals (ages 0-60, 60-80)
integrate(exp_fall_concave_D2, 0, 60) ##-1.79558
integrate(exp_fall_concave_D2, 60, 80) ##-4.034828
-4.034828*2 ##weighing the end of life (ages 60-80) by 2 = -8.069656
ercc_d2_sum_weight_end <- -1.79558-8.069656
ercc_d2_sum_weight_end ##-9.865236

##D2 unweighted and weighted averages (sum/80)
efcc_d2_avg_unweight <- -5.830408/80
efcc_d2_avg_unweight ##-0.0728801
efcc_d2_avg_weight_prime <- -5.994122/80
efcc_d2_avg_weight_prime ##-0.07492652
efcc_d2_avg_weight_asc <- -5.218663/80
efcc_d2_avg_weight_asc ##-0.06523329
efcc_d2_avg_weight_des <- -2.069346/80
efcc_d2_avg_weight_des ##-0.02586682
ercc_d2_avg_weight_end <- -9.865236/80
ercc_d2_avg_weight_end ##-0.1233154

##manually generated minima, maxima, and total
##number_minima: 0
##number_maxima: 0
##number_extrema: 0

##Sinusoidal - Fall, Rise (Man in Hole) ---------------------------------------------------------------------------------------------------------

##10. Sinusoidal Fall, Rise (Full) ---------------------------------------------------------------------------------------------------------

sin_fr_full <- function(x) {50+50*(cos(x*0.079))} ##function
sin_fr_full_plot <- plot(sin_fr_full, lwd = 3, xlim = c(1,79), ylim = c(0,100), main = "Sinusoidal Fall, Rise (Full)", 
                                 xlab = "Age", ylab = "Meaning", col = "dodgerblue2", cex.lab = 1.5) ##plot function
mysubtitle = "(Man in Hole)"
mtext(line = 0.5, mysubtitle)
lifelinesZ[10, ]

sin_fr_full_D1 <- D(50+50*(cos(x*0.079)) ~ x) ##first derivative
sin_fr_full_D1
sin_fr_full_D1_plot <- plot(sin_fr_full_D1, lwd = 3, xlim = c(0,80), ylim = c(-10,10), 
                               main = "First Derivative of Sinusoidal FR (Full)", col = "dodgerblue2", cex.lab = 1.5)
mysubtitle = "(Man in Hole)"
mtext(line = 0.5, mysubtitle)

sin_fr_full_D2 <- D(50+50*(cos(x*0.079)) ~ x & x) ##second derivative
sin_fr_full_D2
sin_fr_full_D2_plot <- plot(sin_fr_full_D2, lwd = 3, xlim = c(0,80), ylim = c(-2,2), 
                               main = "Second Derivative of Sinusoidal FR (Full)", col = "dodgerblue2", cex.lab = 1.5)
mysubtitle = "(Man in Hole)"
mtext(line = 0.5, mysubtitle)

sin_fr_full_area <- integrate(sin_fr_full, 0, 80) ##area
sin_fr_full_area ##4023.295

sin_fr_full_min <- optimize(sin_fr_full, interval = c(0,80)) ##minimum
sin_fr_full_min$objective ##0 
sin_fr_full_max <- optimize(sin_fr_full, interval = c(0,80), maximum = TRUE) ##maximum
sin_fr_full_max$objective ##100

##D1 unweighted sum
sfr_full_d1_sum_unweight <- integrate(sin_fr_full_D1, 0, 80)
sfr_full_d1_sum_unweight ##-0.03387921

##D1 weighted prime: finding sum of various integrals (ages 0-18, 18-30, 30-80)
integrate(sin_fr_full_D1, 0, 18) ##-42.58761
integrate(sin_fr_full_D1, 18, 30) ##-43.25245
-43.25245*2 ##weighing the prime (ages 18-30) by 2 = -86.5049
integrate(sin_fr_full_D1, 30, 80) ##85.80617
sfr_full_d1_sum_weight_prime <- -42.58761-86.5049+85.80617
sfr_full_d1_sum_weight_prime ##-43.28634

##D1 ascending weights: finding sum of various integrals (using weights of 0.25, 0.5, 0.75, and 1)
integrate(sin_fr_full_D1, 0, 20) ##-50.46018
-50.46018*0.25 ##-12.61505
integrate(sin_fr_full_D1, 20, 40) ##-49.53135
-49.53135*0.5 ##-24.76568
integrate(sin_fr_full_D1, 40, 60) ##51.37191
51.37191*0.75 ##38.52893
integrate(sin_fr_full_D1, 60, 80) ##48.58575
48.58575*1 ##48.58575
sfr_full_d1_sum_weight_asc <- -12.61505-24.76568+38.52893+48.58575
sfr_full_d1_sum_weight_asc ##49.73395

##D1 descending weights: finding sum of various integrals (using weights of 1, 0.75, 0.5, and 0.25)
integrate(sin_fr_full_D1, 0, 20) ##-50.46018
-50.46018*1 ##-50.46018
integrate(sin_fr_full_D1, 20, 40) ##-49.53135
-49.53135*0.75 ##-37.14851
integrate(sin_fr_full_D1, 40, 60) ##51.37191
51.37191*0.5 ##25.68595
integrate(sin_fr_full_D1, 60, 80) ##48.58575
48.58575*0.25 ##12.14644
sfr_full_d1_sum_weight_des <- -50.46018-37.14851+25.68595+12.14644
sfr_full_d1_sum_weight_des ##-49.7763

##D1 weighted end of life: finding sum of various integrals (ages 0-60, 60-80)
integrate(sin_fr_full_D1, 0, 60) ##-48.61962
integrate(sin_fr_full_D1, 60, 80) ##48.58575
48.58575*2 ##weighing the end of life (ages 60-80) by 2 = 97.1715
sfr_full_d1_sum_weight_end <- -48.61962+97.1715
sfr_full_d1_sum_weight_end ##48.55188

##D1 unweighted and weighted averages (sum/80)
sfr_full_d1_avg_unweight <- -0.03387921/80
sfr_full_d1_avg_unweight ##-0.0004234901
sfr_full_d1_avg_weight_prime <- -43.28634/80
sfr_full_d1_avg_weight_prime ##-0.5410793
sfr_full_d1_avg_weight_asc <- 49.73395/80
sfr_full_d1_avg_weight_asc ##0.6216744
sfr_full_d1_avg_weight_des <- -49.7763/80
sfr_full_d1_avg_weight_des ##-0.6222037
sfr_full_d1_avg_weight_end <- 48.55188/80
sfr_full_d1_avg_weight_end ##0.6068985

##D2 unweighted sum
sfr_full_d2_sum_unweight <- integrate(sin_fr_full_D2, 0, 80)
sfr_full_d2_sum_unweight ##-0.1453852

##D2 weighted prime: finding sum of various integrals (ages 0-18, 18-30, 30-80)
integrate(sin_fr_full_D2, 0, 18) ##-3.906353
integrate(sin_fr_full_D2, 18, 30) ##1.152106
1.152106*2 ##weighing the prime (ages 18-30) by 2 = 2.304212
integrate(sin_fr_full_D2, 30, 80) ##2.608862
sfr_full_d2_sum_weight_prime <- -3.906353+2.304212+2.608862
sfr_full_d2_sum_weight_prime ##1.006721

##D2 ascending weights: finding sum of various integrals (using weights of 0.25, 0.5, 0.75, and 1)
integrate(sin_fr_full_D2, 0, 20) ##-3.949833
-3.949833*0.25 ##-0.9874582
integrate(sin_fr_full_D2, 20, 40) ##4.022538
4.022538*0.5 ##2.011269
integrate(sin_fr_full_D2, 40, 60) ##3.87579
3.87579*0.75 ##2.906842
integrate(sin_fr_full_D2, 60, 80) ##-4.09388
-4.09388*1 ##-4.09388
sfr_full_d2_sum_weight_asc <- -0.9874582+2.011269+2.906842-4.09388
sfr_full_d2_sum_weight_asc ##-0.1632272

##D2 descending weights: finding sum of various integrals (using weights of 1, 0.75, 0.5, and 0.25)
integrate(sin_fr_full_D2, 0, 20) ##-3.949833
-3.949833*1 ##-3.949833
integrate(sin_fr_full_D2, 20, 40) ##4.022538
4.022538*0.75 ##3.016903
integrate(sin_fr_full_D2, 40, 60) ##3.87579
3.87579*0.5 ##1.937895
integrate(sin_fr_full_D2, 60, 80) ##-4.09388
-4.09388*0.25 ##-1.02347
sfr_full_d2_sum_weight_des <- -3.949833+3.016903+1.937895-1.02347
sfr_full_d2_sum_weight_des ##-0.018505

##D2 weighted end of life: finding sum of various integrals (ages 0-60, 60-80)
integrate(sin_fr_full_D2, 0, 60) ##3.948494
integrate(sin_fr_full_D2, 60, 80) ##-4.09388
-4.09388*2 ##weighing the end of life (ages 60-80) by 2 = -8.18776
sfr_full_d2_sum_weight_end <- 3.948494-8.18776
sfr_full_d2_sum_weight_end ##-4.239266

##D2 unweighted and weighted averages (sum/80)
sfr_full_d2_avg_unweight <- -0.1453852/80
sfr_full_d2_avg_unweight ##-0.001817315
sfr_full_d2_avg_weight_prime <- 1.006721/80
sfr_full_d2_avg_weight_prime ##0.01258401
sfr_full_d2_avg_weight_asc <- -0.1632272/80
sfr_full_d2_avg_weight_asc ##-0.00204034
sfr_full_d2_avg_weight_des <- -0.018505/80
sfr_full_d2_avg_weight_des ##-0.0002313125
sfr_full_d2_avg_weight_end <- -4.239266/80
sfr_full_d2_avg_weight_end ##-0.05299082

##manually generated minima, maxima, and total
##number_minima: 1
##number_maxima: 0
##number_extrema: 1

##11. Sinusoidal Fall, Rise (Partial) ---------------------------------------------------------------------------------------------------------

##We thought that it was important to test if it would matter to participants whether a sinusoidal lifeline starts at the middle
##of the graph while keeping the same number of fluctuations (peaks and valleys) and the same endpoint (rising to 100 or falling to 0.)
##One way to do this was to "shrink" a full graph to make a "partial" one and keep all features intact except for the starting point.

sin_fr_partial <- function(x) {50-50*(sin(x*0.05889))} ##function
sin_fr_partial_plot <- plot(sin_fr_partial, lwd = 3, xlim = c(0,80), ylim = c(0,100), main = "Sinusoidal Fall, Rise (Partial)", 
                            xlab = "Age", ylab = "Meaning", col = "dodgerblue2", cex.lab = 1.5) ##plot function
lifelinesZ[11, ]

sin_fr_partial_D1 <- D(50-50*(sin(x*0.05889)) ~ x) ##first derivative
sin_fr_partial_D1
sin_fr_partial_D1_plot <- plot(sin_fr_partial_D1, lwd = 3, xlim = c(0,80), ylim = c(-10,10), 
                               main = "First Derivative of Sinusoidal FR (Partial)", col = "dodgerblue2", cex.lab = 1.5)
sin_fr_partial_D2 <- D(50-50*(sin(x*0.05889)) ~ x & x) ##second derivative
sin_fr_partial_D2
sin_fr_partial_D2_plot <- plot(sin_fr_partial_D2, lwd = 3, xlim = c(0,80), ylim = c(-2,2), 
                               main = "Second Derivative of Sinusoidal FR (Partial)", col = "dodgerblue2", cex.lab = 1.5)

sin_fr_partial_area <- integrate(sin_fr_partial, 0, 80) ##area
sin_fr_partial_area ##3149.95

sin_fr_partial_min <- optimize(sin_fr_partial, interval = c(0,80)) ##minimum
sin_fr_partial_min$objective ##0
sin_fr_partial_max <- optimize(sin_fr_partial, interval = c(0,80), maximum = TRUE) ##maximum
sin_fr_partial_max$objective ##100

##D1 unweighted sum
sfr_partial_d1_sum_unweight <- integrate(sin_fr_partial_D1, 0, 80)
sfr_partial_d1_sum_unweight ##49.99996

##D1 weighted prime: finding sum of various integrals (ages 0-18, 18-30, 30-80)
integrate(sin_fr_partial_D1, 0, 18) ##-43.61826
integrate(sin_fr_partial_D1, 18, 30) ##-5.425345
-5.425345*2 ##weighing the prime (ages 18-30) by 2 = -10.85069
integrate(sin_fr_partial_D1, 30, 80) ##99.04357
sfr_partial_d1_sum_weight_prime <- -43.61826-10.85069+99.04357
sfr_partial_d1_sum_weight_prime ##44.57462

##D1 ascending weights: finding sum of various integrals (using weights of 0.25, 0.5, 0.75, and 1)
integrate(sin_fr_partial_D1, 0, 20) ##-46.18829
-46.18829*0.25 ##-11.54707
integrate(sin_fr_partial_D1, 20, 40) ##10.81194
10.81194*0.5 ##5.40597
integrate(sin_fr_partial_D1, 40, 60) ##54.46932
54.46932*0.75 ##40.85199
integrate(sin_fr_partial_D1, 60, 80) ##30.90699
30.90699*1 ##30.90699
sfr_partial_d1_sum_weight_asc <- -11.54707+5.40597+40.85199+30.90699
sfr_partial_d1_sum_weight_asc ##65.61788

##D1 descending weights: finding sum of various integrals (using weights of 1, 0.75, 0.5, and 0.25)
integrate(sin_fr_partial_D1, 0, 20) ##-46.18829
-46.18829*1 ##-46.18829
integrate(sin_fr_partial_D1, 20, 40) ##10.81194
10.81194*0.75 ##8.108955
integrate(sin_fr_partial_D1, 40, 60) ##54.46932
54.46932*0.5 ##27.23466
integrate(sin_fr_partial_D1, 60, 80) ##30.90699
30.90699*0.25 ##7.726748
sfr_partial_d1_sum_weight_des <- -46.18829+8.108955+27.23466+7.726748
sfr_partial_d1_sum_weight_des ##-3.117927

##D1 weighted end of life: finding sum of various integrals (ages 0-60, 60-80)
integrate(sin_fr_partial_D1, 0, 60) ##19.09297
integrate(sin_fr_partial_D1, 60, 80) ##30.90699
30.90699*2 ##weighing the end of life (ages 60-80) by 2 = 61.81398
sfr_partial_d1_sum_weight_end <- 19.09297+61.81398
sfr_partial_d1_sum_weight_end ##80.90695

##D1 unweighted and weighted averages (sum/80)
sfr_partial_d1_avg_unweight <- 49.99996/80
sfr_partial_d1_avg_unweight ##0.6249995
sfr_partial_d1_avg_weight_prime <- 44.57462/80
sfr_partial_d1_avg_weight_prime ##0.5571828
sfr_partial_d1_avg_weight_asc <- 65.61788/80
sfr_partial_d1_avg_weight_asc ##0.8202235
sfr_partial_d1_avg_weight_des <- -3.117927/80
sfr_partial_d1_avg_weight_des ##-0.03897409
sfr_partial_d1_avg_weight_end <- 80.90695/80
sfr_partial_d1_avg_weight_end ##1.011337

##D2 unweighted sum
sfr_partial_d2_sum_unweight <- integrate(sin_fr_partial_D2, 0, 80)
sfr_partial_d2_sum_unweight ##2.948001

##D2 weighted prime: finding sum of various integrals (ages 0-18, 18-30, 30-80)
integrate(sin_fr_partial_D2, 0, 18) ##1.505068
integrate(sin_fr_partial_D2, 18, 30) ##2.012588
2.012588*2 ##weighing the prime (ages 18-30) by 2 = 4.025176
integrate(sin_fr_partial_D2, 30, 80) ##-0.5696548
sfr_partial_d2_sum_weight_prime <- 1.505068+4.025176-0.5696548
sfr_partial_d2_sum_weight_prime ##4.960589

##D2 ascending weights: finding sum of various integrals (using weights of 0.25, 0.5, 0.75, and 1)
integrate(sin_fr_partial_D2, 0, 20) ##1.81688
1.81688*0.25 ##0.45422
integrate(sin_fr_partial_D2, 20, 40) ##3.208458
3.208458*0.5 ##1.604229
integrate(sin_fr_partial_D2, 40, 60) ##0.6405292
0.6405292*0.75 ##0.4803969
integrate(sin_fr_partial_D2, 60, 80) ##-2.717866
-2.717866*1 ##-2.717866
sfr_partial_d2_sum_weight_asc <- 0.45422+1.604229+0.4803969-2.717866
sfr_partial_d2_sum_weight_asc ##-0.1790201

##D2 descending weights: finding sum of various integrals (using weights of 1, 0.75, 0.5, and 0.25)
integrate(sin_fr_partial_D2, 0, 20) ##1.81688
1.81688*1 ##1.81688
integrate(sin_fr_partial_D2, 20, 40) ##3.208458
3.208458*0.75 ##2.406343
integrate(sin_fr_partial_D2, 40, 60) ##0.6405292
0.6405292*0.5 ##0.3202646
integrate(sin_fr_partial_D2, 60, 80) ##-2.717866
-2.717866*0.25 ##-0.6794665
sfr_partial_d2_sum_weight_des <- 1.81688+2.406343+0.3202646-0.6794665
sfr_partial_d2_sum_weight_des ##3.864021

##D2 weighted end of life: finding sum of various integrals (ages 0-60, 60-80)
integrate(sin_fr_partial_D2, 0, 60) ##5.665867
integrate(sin_fr_partial_D2, 60, 80) ##-2.717866
-2.717866*2 ##weighing the end of life (ages 60-80) by 2 = -5.435732
sfr_partial_d2_sum_weight_end <- 5.665867-5.435732
sfr_partial_d2_sum_weight_end ##0.230135

##D2 unweighted and weighted averages (sum/80)
sfr_partial_d2_avg_unweight <- 2.948001/80
sfr_partial_d2_avg_unweight ##0.03685001
sfr_partial_d2_avg_weight_prime <- 4.960589/80
sfr_partial_d2_avg_weight_prime ##0.06200736
sfr_partial_d2_avg_weight_asc <- -0.1790201/80
sfr_partial_d2_avg_weight_asc ##-0.002237751
sfr_partial_d2_avg_weight_des <- 3.864021/80
sfr_partial_d2_avg_weight_des ##0.04830026
sfr_partial_d2_avg_weight_end <- 0.230135/80
sfr_partial_d2_avg_weight_end ##0.002876687

##manually generated minima, maxima, and total
##number_minima: 1
##number_maxima: 0
##number_extrema: 1

##Sinusoidal - Rise, Fall (Icarus) ---------------------------------------------------------------------------------------------------------

##12. Sinusoidal Rise, Fall (Full) ---------------------------------------------------------------------------------------------------------

sin_rf_full <- function(x) {50-50*(cos(x*0.079))} ##function
sin_rf_full_plot <- plot(sin_rf_full, lwd = 3, xlim = c(0,80), ylim = c(0,100), main = "Sinusoidal Rise, Fall (Full)", 
                               xlab = "Age", ylab = "Meaning", col = "dodgerblue2", cex.lab = 1.5) ##plot function
lifelinesZ[12, ]

sin_rf_full_D1 <- D(50-50*(cos(x*0.079)) ~ x) ##first derivative
sin_rf_full_D1
sin_rf_full_D1_plot <- plot(sin_rf_full_D1, lwd = 3, xlim = c(0,80), ylim = c(-10,10), 
                                  main = "First Derivative of Sinusoidal RF (Full)", col = "dodgerblue2", cex.lab = 1.5)
sin_rf_full_D2 <- D(50-50*(cos(x*0.079)) ~ x & x) ##second derivative
sin_rf_full_D2
sin_rf_full_D2_plot <- plot(sin_rf_full_D2, lwd = 3, xlim = c(0,80), ylim = c(-2,2), 
                                  main = "Second Derivative of Sinusoidal RF (Full)", col = "dodgerblue2", cex.lab = 1.5)

sin_rf_full_area <- integrate(sin_rf_full, 0, 80) ##area
sin_rf_full_area ##3976.705

sin_rf_full_min <- optimize(sin_rf_full, interval = c(0,80)) ##minimum
sin_rf_full_min$objective ##0
sin_rf_full_max <- optimize(sin_rf_full, interval = c(0,80), maximum = TRUE) ##maximum
sin_rf_full_max$objective ##100

##D1 unweighted sum
srf_full_d1_sum_unweight <- integrate(sin_rf_full_D1, 0, 80)
srf_full_d1_sum_unweight ##0.03387921

##D1 weighted prime: finding sum of various integrals (ages 0-18, 18-30, 30-80)
integrate(sin_rf_full_D1, 0, 18) ##42.58761
integrate(sin_rf_full_D1, 18, 30) ##43.25245
43.25245*2 ##weighing the prime (ages 18-30) by 2 = 86.5049
integrate(sin_rf_full_D1, 30, 80) ##-85.80617
srf_full_d1_sum_weight_prime <- 42.58761+86.5049-85.80617
srf_full_d1_sum_weight_prime ##43.28634

##D1 ascending weights: finding sum of various integrals (using weights of 0.25, 0.5, 0.75, and 1)
integrate(sin_rf_full_D1, 0, 20) ##50.46018
50.46018*0.25 ##12.61505
integrate(sin_rf_full_D1, 20, 40) ##49.53135
49.53135*0.5 ##24.76568
integrate(sin_rf_full_D1, 40, 60) ##-51.37191
-51.37191*0.75 ##-38.52893
integrate(sin_rf_full_D1, 60, 80) ##-48.58575
-48.58575*1 ##-48.58575
srf_full_d1_sum_weight_asc <- 12.61505+24.76568-38.52893-48.58575
srf_full_d1_sum_weight_asc ##-49.73395

##D1 descending weights: finding sum of various integrals (using weights of 1, 0.75, 0.5, and 0.25)
integrate(sin_rf_full_D1, 0, 20) ##50.46018
50.46018*1 ##50.46018
integrate(sin_rf_full_D1, 20, 40) ##49.53135
49.53135*0.75 ##37.14851
integrate(sin_rf_full_D1, 40, 60) ##-51.37191
-51.37191*0.5 ##-25.68595
integrate(sin_rf_full_D1, 60, 80) ##-48.58575
-48.58575*0.25 ##-12.14644
srf_full_d1_sum_weight_des <- 50.46018+37.14851-25.68595-12.14644
srf_full_d1_sum_weight_des ##49.7763

##D1 weighted end of life: finding sum of various integrals (ages 0-60, 60-80)
integrate(sin_rf_full_D1, 0, 60) ##48.61962
integrate(sin_rf_full_D1, 60, 80) ##-48.58575
-48.58575*2 ##weighing the end of life (ages 60-80) by 2 = -97.1715
srf_full_d1_sum_weight_end <- 48.61962-97.1715
srf_full_d1_sum_weight_end ##-48.55188

##D1 unweighted and weighted averages (sum/80)
srf_full_d1_avg_unweight <- 0.03387921/80
srf_full_d1_avg_unweight ##0.0004234901
srf_full_d1_avg_weight_prime <- 43.28634/80
srf_full_d1_avg_weight_prime ##0.5410793
srf_full_d1_avg_weight_asc <- -49.73395/80
srf_full_d1_avg_weight_asc ##-0.6216744
srf_full_d1_avg_weight_des <- 49.7763/80
srf_full_d1_avg_weight_des ##0.6222037
srf_full_d1_avg_weight_end <- -48.55188/80
srf_full_d1_avg_weight_end ##-0.6068985

##D2 unweighted sum
srf_full_d2_sum_unweight <- integrate(sin_rf_full_D2, 0, 80)
srf_full_d2_sum_unweight ##0.1453852

##D2 weighted prime: finding sum of various integrals (ages 0-18, 18-30, 30-80)
integrate(sin_rf_full_D2, 0, 18) ##3.906353
integrate(sin_rf_full_D2, 18, 30) ##-1.152106
-1.152106*2 ##weighing the prime (ages 18-30) by 2 = -2.304212
integrate(sin_rf_full_D2, 30, 80) ##-2.608862
srf_full_d2_sum_weight_prime <- 3.906353-2.304212-2.608862
srf_full_d2_sum_weight_prime ##-1.006721

##D2 ascending weights: finding sum of various integrals (using weights of 0.25, 0.5, 0.75, and 1)
integrate(sin_rf_full_D2, 0, 20) ##3.949833
3.949833*0.25 ##0.9874582
integrate(sin_rf_full_D2, 20, 40) ##-4.022538
-4.022538*0.5 ##-2.011269
integrate(sin_rf_full_D2, 40, 60) ##-3.87579
-3.87579*0.75 ##-2.906842
integrate(sin_rf_full_D2, 60, 80) ##4.09388
4.09388*1 ##4.09388
srf_full_d2_sum_weight_asc <- 0.9874582-2.011269-2.906842+4.09388
srf_full_d2_sum_weight_asc ##0.1632272

##D2 descending weights: finding sum of various integrals (using weights of 1, 0.75, 0.5, and 0.25)
integrate(sin_rf_full_D2, 0, 20) ##3.949833
3.949833*1 ##3.949833
integrate(sin_rf_full_D2, 20, 40) ##-4.022538
-4.022538*0.75 ##-3.016903
integrate(sin_rf_full_D2, 40, 60) ##-3.87579
-3.87579*0.5 ##-1.937895
integrate(sin_rf_full_D2, 60, 80) ##4.09388
4.09388*0.25 ##1.02347
srf_full_d2_sum_weight_des <- 3.949833-3.016903-1.937895+1.02347
srf_full_d2_sum_weight_des ##0.018505

##D2 weighted end of life: finding sum of various integrals (ages 0-60, 60-80)
integrate(sin_rf_full_D2, 0, 60) ##-3.948494
integrate(sin_rf_full_D2, 60, 80) ##4.09388
4.09388*2 ##weighing the end of life (ages 60-80) by 2 = 8.18776
srf_full_d2_sum_weight_end <- -3.948494+8.18776
srf_full_d2_sum_weight_end ##4.239266

##D2 unweighted and weighted averages (sum/80)
srf_full_d2_avg_unweight <- 0.1453852/80
srf_full_d2_avg_unweight ##0.001817315
srf_full_d2_avg_weight_prime <- -1.006721/80
srf_full_d2_avg_weight_prime ##-0.01258401
srf_full_d2_avg_weight_asc <- 0.1632272/80
srf_full_d2_avg_weight_asc ##0.00204034
srf_full_d2_avg_weight_des <- 0.018505/80
srf_full_d2_avg_weight_des ##0.0002313125
srf_full_d2_avg_weight_end <- 4.239266/80
srf_full_d2_avg_weight_end ##0.05299082

##manually generated minima, maxima, and total
##number_minima: 0
##number_maxima: 1
##number_extrema: 1

##13. Sinusoidal Rise, Fall (Partial) ---------------------------------------------------------------------------------------------------------

##(Note from ##11) 
##We thought that it was important to test if it would matter to participants whether a sinusoidal lifeline starts at the middle
##of the graph while keeping the same number of fluctuations (peaks and valleys) and the same endpoint (rising to 100 or falling to 0.)
##One way to do this was to "shrink" a full graph to make a "partial" one and keep all features intact except for the starting point.

sin_rf_partial <- function(x) {50+50*(sin(x*0.05889))} ##function
sin_rf_partial_plot <- plot(sin_rf_partial, lwd = 3, xlim = c(0,80), ylim = c(0,100), main = "Sinusoidal Rise, Fall (Partial)", 
                            xlab = "Age", ylab = "Meaning", col = "dodgerblue2", cex.lab = 1.5) ##plot function
lifelinesZ[13, ]

sin_rf_partial_D1 <- D(50+50*(sin(x*0.05889)) ~ x) ##first derivative
sin_rf_partial_D1
sin_rf_partial_D1_plot <- plot(sin_rf_partial_D1, lwd = 3, xlim = c(0,80), ylim = c(-10,10), 
                               main = "First Derivative of Sinusoidal RF (Partial)", col = "dodgerblue2", cex.lab = 1.5)
sin_rf_partial_D2 <- D(50+50*(sin(x*0.05889)) ~ x & x) ##second derivative
sin_rf_partial_D2
sin_rf_partial_D2_plot <- plot(sin_rf_partial_D2, lwd = 3, xlim = c(0,80), ylim = c(-2,2), 
                               main = "Second Derivative of Sinusoidal RF (Partial)", col = "dodgerblue2", cex.lab = 1.5)

sin_rf_partial_area <- integrate(sin_rf_partial, 0, 80) ##area
sin_rf_partial_area ##4850.05

sin_rf_partial_min <- optimize(sin_rf_partial, interval = c(0,80)) ##minimum
sin_rf_partial_min$objective ##0 
sin_rf_partial_max <- optimize(sin_rf_partial, interval = c(0,80), maximum = TRUE) ##maximum
sin_rf_partial_max$objective ##100  

##D1 unweighted sum
srf_partial_d1_sum_unweight <- integrate(sin_rf_partial_D1, 0, 80)
srf_partial_d1_sum_unweight ##-49.99996

##D1 weighted prime: finding sum of various integrals (ages 0-18, 18-30, 30-80)
integrate(sin_rf_partial_D1, 0, 18) ##43.61826
integrate(sin_rf_partial_D1, 18, 30) ##5.425345
5.425345*2 ##weighing the prime (ages 18-30) by 2 = 10.85069
integrate(sin_rf_partial_D1, 30, 80) ##-99.04357
srf_partial_d1_sum_weight_prime <- 43.61826+10.85069-99.04357
srf_partial_d1_sum_weight_prime ##-44.57462

##D1 ascending weights: finding sum of various integrals (using weights of 0.25, 0.5, 0.75, and 1)
integrate(sin_rf_partial_D1, 0, 20) ##46.18829
46.18829*0.25 ##11.54707
integrate(sin_rf_partial_D1, 20, 40) ##-10.81194
-10.81194*0.5 ##-5.40597
integrate(sin_rf_partial_D1, 40, 60) ##-54.46932
-54.46932*0.75 ##-40.85199
integrate(sin_rf_partial_D1, 60, 80) ##-30.90699
-30.90699*1 ##-30.90699
srf_partial_d1_sum_weight_asc <- 11.54707-5.40597-40.85199-30.90699
srf_partial_d1_sum_weight_asc ##-65.61788

##D1 descending weights: finding sum of various integrals (using weights of 1, 0.75, 0.5, and 0.25)
integrate(sin_rf_partial_D1, 0, 20) ##46.18829
46.18829*1 ##46.18829
integrate(sin_rf_partial_D1, 20, 40) ##-10.81194
-10.81194*0.75 ##-8.108955
integrate(sin_rf_partial_D1, 40, 60) ##-54.46932
-54.46932*0.5 ##-27.23466
integrate(sin_rf_partial_D1, 60, 80) ##-30.90699
-30.90699*0.25 ##-7.726748
srf_partial_d1_sum_weight_des <- 46.18829-8.108955-27.23466-7.726748
srf_partial_d1_sum_weight_des ##3.117927

##D1 weighted end of life: finding sum of various integrals (ages 0-60, 60-80)
integrate(sin_rf_partial_D1, 0, 60) ##-19.09297
integrate(sin_rf_partial_D1, 60, 80) ##-30.90699
-30.90699*2 ##weighing the end of life (ages 60-80) by 2 = -61.81398
srf_partial_d1_sum_weight_end <- -19.09297-61.81398
srf_partial_d1_sum_weight_end ##-80.90695

##D1 unweighted and weighted averages (sum/80)
srf_partial_d1_avg_unweight <- -49.99996/80
srf_partial_d1_avg_unweight ##-0.6249995
srf_partial_d1_avg_weight_prime <- -44.57462/80
srf_partial_d1_avg_weight_prime ##-0.5571828
srf_partial_d1_avg_weight_asc <- -65.61788/80
srf_partial_d1_avg_weight_asc ##-0.8202235
srf_partial_d1_avg_weight_des <- 3.117927/80
srf_partial_d1_avg_weight_des ##0.03897409
srf_partial_d1_avg_weight_end <- -80.90695/80
srf_partial_d1_avg_weight_end ##-1.011337

##D2 unweighted sum
srf_partial_d2_sum_unweight <- integrate(sin_rf_partial_D2, 0, 80)
srf_partial_d2_sum_unweight ##-2.948001

##D2 weighted prime: finding sum of various integrals (ages 0-18, 18-30, 30-80)
integrate(sin_rf_partial_D2, 0, 18) ##-1.505068
integrate(sin_rf_partial_D2, 18, 30) ##-2.012588
-2.012588*2 ##weighing the prime (ages 18-30) by 2 = -4.025176
integrate(sin_rf_partial_D2, 30, 80) ##0.5696548
srf_partial_d2_sum_weight_prime <- -1.505068-4.025176+0.5696548
srf_partial_d2_sum_weight_prime ##-4.960589

##D2 ascending weights: finding sum of various integrals (using weights of 0.25, 0.5, 0.75, and 1)
integrate(sin_rf_partial_D2, 0, 20) ##-1.81688
-1.81688*0.25 ##-0.45422
integrate(sin_rf_partial_D2, 20, 40) ##-3.208458
-3.208458*0.5 ##-1.604229
integrate(sin_rf_partial_D2, 40, 60) ##-0.6405292
-0.6405292*0.75 ##-0.4803969
integrate(sin_rf_partial_D2, 60, 80) ##2.717866
2.717866*1 ##2.717866
srf_partial_d2_sum_weight_asc <- -0.45422-1.604229-0.4803969+2.717866
srf_partial_d2_sum_weight_asc ##0.1790201

##D2 descending weights: finding sum of various integrals (using weights of 1, 0.75, 0.5, and 0.25)
integrate(sin_rf_partial_D2, 0, 20) ##-1.81688
-1.81688*1 ##-1.81688
integrate(sin_rf_partial_D2, 20, 40) ##-3.208458
-3.208458*0.75 ##-2.406343
integrate(sin_rf_partial_D2, 40, 60) ##-0.6405292
-0.6405292*0.5 ##-0.3202646
integrate(sin_rf_partial_D2, 60, 80) ##2.717866
2.717866*0.25 ##0.6794665
srf_partial_d2_sum_weight_des <- -1.81688-2.406343-0.3202646+0.6794665
srf_partial_d2_sum_weight_des ##-3.864021

##D2 weighted end of life: finding sum of various integrals (ages 0-60, 60-80)
integrate(sin_rf_partial_D2, 0, 60) ##-5.665867
integrate(sin_rf_partial_D2, 60, 80) ##2.717866
2.717866*2 ##weighing the end of life (ages 60-80) by 2 = 5.435732
srf_partial_d2_sum_weight_end <- -5.665867+5.435732
srf_partial_d2_sum_weight_end ##-0.230135

##D2 unweighted and weighted averages (sum/80)
srf_partial_d2_avg_unweight <- -2.948001/80
srf_partial_d2_avg_unweight ##-0.03685001
srf_partial_d2_avg_weight_prime <- -4.960589/80
srf_partial_d2_avg_weight_prime ##-0.06200736
srf_partial_d2_avg_weight_asc <- 0.1790201/80
srf_partial_d2_avg_weight_asc ##0.002237751
srf_partial_d2_avg_weight_des <- -3.864021/80
srf_partial_d2_avg_weight_des ##-0.04830026
srf_partial_d2_avg_weight_end <- -0.230135/80
srf_partial_d2_avg_weight_end ##-0.002876687

##manually generated minima, maxima, and total
##number_minima: 0
##number_maxima: 1
##number_extrema: 1

##Sinusoidal - Rise, Fall, Rise (Boy Meets Girl) ---------------------------------------------------------------------------------------------------------

##14. Sinusoidal Rise, Fall, Rise (Full) ---------------------------------------------------------------------------------------------------------

sin_rfr_full <- function(x) {50-50*(cos(x*0.1185))} ##function
sin_rfr_full_plot <- plot(sin_rfr_full, lwd = 3, xlim = c(0,80), ylim = c(0,100), main = "Sinusoidal Rise, Fall, Rise (Full)", 
                               xlab = "Age", ylab = "Meaning", col = "dodgerblue2", cex.lab = 1.5) ##plot function
mysubtitle = "(Boy Meets Girl)"
mtext(line = 0.5, mysubtitle)
lifelinesZ[14, ]

sin_rfr_full_D1 <- D(50-50*(cos(x*0.1185)) ~ x) ##first derivative
sin_rfr_full_D1
sin_rfr_full_D1_plot <- plot(sin_rfr_full_D1, lwd = 3, xlim = c(0,80), ylim = c(-10,10), 
                                  main = "First Derivative of Sinusoidal RFR (Full)", col = "dodgerblue2", cex.lab = 1.5)
mysubtitle = "(Boy Meets Girl)"
mtext(line = 0.5, mysubtitle)

sin_rfr_full_D2 <- D(50-50*(cos(x*0.1185)) ~ x & x) ##second derivative
sin_rfr_full_D2
sin_rfr_full_D2_plot <- plot(sin_rfr_full_D2, lwd = 3, xlim = c(0,80), ylim = c(-2,2), 
                                  main = "Second Derivative of Sinusoidal RFR (Full)", col = "dodgerblue2", cex.lab = 1.5)
mysubtitle = "(Boy Meets Girl)"
mtext(line = 0.5, mysubtitle)

sin_rfr_full_area <- integrate(sin_rfr_full, 0, 80) ##area
sin_rfr_full_area ##4023.289

sin_rfr_full_min <- optimize(sin_rfr_full, interval = c(0,80)) ##minimum
sin_rfr_full_min$objective ## 0
sin_rfr_full_max <- optimize(sin_rfr_full, interval = c(0,80), maximum = TRUE) ##maximum
sin_rfr_full_max$objective ## 100

##D1 unweighted sum
srfr_full_d1_sum_unweight <- integrate(sin_rfr_full_D1, 0, 80)
srfr_full_d1_sum_unweight ##99.92378

##D1 weighted prime: finding sum of various integrals (ages 0-18, 18-30, 30-80)
integrate(sin_rfr_full_D1, 0, 18) ##76.6526
integrate(sin_rfr_full_D1, 18, 30) ##19.13527
19.13527*2 ##weighing the prime (ages 18-30) by 2 = 38.27054
integrate(sin_rfr_full_D1, 30, 80) ##4.135917
srfr_full_d1_sum_weight_prime <- 76.6526+38.27054+4.135917
srfr_full_d1_sum_weight_prime ##119.0591

##D1 ascending weights: finding sum of various integrals (using weights of 0.25, 0.5, 0.75, and 1)
integrate(sin_rfr_full_D1, 0, 20) ##85.84005
85.84005*0.25 ##21.46001
integrate(sin_rfr_full_D1, 20, 40) ##-37.22043
-37.22043*0.5 ##-18.61022
integrate(sin_rfr_full_D1, 40, 60) ##-32.48077
-32.48077*0.75 ##-24.36058
integrate(sin_rfr_full_D1, 60, 80) ##83.78493
83.78493*1 ##83.78493
srfr_full_d1_sum_weight_asc <- 21.46001-18.61022-24.36058+83.78493
srfr_full_d1_sum_weight_asc ##62.27414

##D1 descending weights: finding sum of various integrals (using weights of 1, 0.75, 0.5, and 0.25)
integrate(sin_rfr_full_D1, 0, 20) ##85.84005
85.84005*1 ##85.84005
integrate(sin_rfr_full_D1, 20, 40) ##-37.22043
-37.22043*0.75 ##-27.91532
integrate(sin_rfr_full_D1, 40, 60) ##-32.48077
-32.48077*0.5 ##-16.24038
integrate(sin_rfr_full_D1, 60, 80) ##83.78493
83.78493*0.25 ##20.94623
srfr_full_d1_sum_weight_des <- 85.84005-27.91532-16.24038+20.94623
srfr_full_d1_sum_weight_des ##62.63058

##D1 weighted end of life: finding sum of various integrals (ages 0-60, 60-80)
integrate(sin_rfr_full_D1, 0, 60) ##16.13886
integrate(sin_rfr_full_D1, 60, 80) ##83.78493
83.78493*2 ##weighing the end of life (ages 60-80) by 2 = 167.5699 
srfr_full_d1_sum_weight_end <- 16.13886+167.5699
srfr_full_d1_sum_weight_end ##183.7088

##D1 unweighted and weighted averages (sum/80)
srfr_full_d1_avg_unweight <- 99.92378/80
srfr_full_d1_avg_unweight ##1.249047
srfr_full_d1_avg_weight_prime <- 119.0591/80
srfr_full_d1_avg_weight_prime ##1.488239
srfr_full_d1_avg_weight_asc <- 62.27414/80
srfr_full_d1_avg_weight_asc ##0.7784268
srfr_full_d1_avg_weight_des <- 62.63058/80
srfr_full_d1_avg_weight_des ##0.7828823
srfr_full_d1_avg_weight_end <- 183.7088/80
srfr_full_d1_avg_weight_end ##2.29636

##D2 unweighted sum
srfr_full_d2_sum_unweight <- integrate(sin_rfr_full_D2, 0, 80)
srfr_full_d2_sum_unweight ##-0.3270243

##D2 weighted prime: finding sum of various integrals (ages 0-18, 18-30, 30-80)
integrate(sin_rfr_full_D2, 0, 18) ##5.013039
integrate(sin_rfr_full_D2, 18, 30) ##-7.393301
-7.393301*2 ##weighing the prime (ages 18-30) by 2 = -14.7866
integrate(sin_rfr_full_D2, 30, 80) ##2.053238
srfr_full_d2_sum_weight_prime <- 5.013039-14.7866+2.053238
srfr_full_d2_sum_weight_prime ##-7.720323

##D2 ascending weights: finding sum of various integrals (using weights of 0.25, 0.5, 0.75, and 1)
integrate(sin_rfr_full_D2, 0, 20) ##4.131371
4.131371*0.25 ##1.032843
integrate(sin_rfr_full_D2, 20, 40) ##-10.05411
-10.05411*0.5 ##-5.027055
integrate(sin_rfr_full_D2, 40, 60) ##10.28223
10.28223*0.75 ##7.711673
integrate(sin_rfr_full_D2, 60, 80) ##-4.686509
-4.686509*1 ##-4.686509
srfr_full_d2_sum_weight_asc <- 1.032843-5.027055+7.711673-4.686509
srfr_full_d2_sum_weight_asc ##-0.969048

##D2 descending weights: finding sum of various integrals (using weights of 1, 0.75, 0.5, and 0.25)
integrate(sin_rfr_full_D2, 0, 20) ##4.131371
4.131371*1 ##4.131371
integrate(sin_rfr_full_D2, 20, 40) ##-10.05411
-10.05411*0.75 ##-7.540582
integrate(sin_rfr_full_D2, 40, 60) ##10.28223
10.28223*0.5 ##5.141115
integrate(sin_rfr_full_D2, 60, 80) ##-4.686509
-4.686509*0.25 ##-1.171627
srfr_full_d2_sum_weight_des <- 4.131371-7.540582+5.141115-1.171627
srfr_full_d2_sum_weight_des ##0.560277

##D2 weighted end of life: finding sum of various integrals (ages 0-60, 60-80)
integrate(sin_rfr_full_D2, 0, 60) ##4.359484
integrate(sin_rfr_full_D2, 60, 80) ##-4.686509
-4.686509*2 ##weighing the end of life (ages 60-80) by 2 = -9.373018 
srfr_full_d2_sum_weight_end <- 4.359484-9.373018
srfr_full_d2_sum_weight_end ##-5.013534

##D2 unweighted and weighted averages (sum/80)
srfr_full_d2_avg_unweight <- -0.3270243/80
srfr_full_d2_avg_unweight ##-0.004087804
srfr_full_d2_avg_weight_prime <- -7.720323/80
srfr_full_d2_avg_weight_prime ##-0.09650404
srfr_full_d2_avg_weight_asc <- -0.969048/80
srfr_full_d2_avg_weight_asc ##-0.0121131
srfr_full_d2_avg_weight_des <- 0.560277/80
srfr_full_d2_avg_weight_des ##0.007003462
srfr_full_d2_avg_weight_end <- -5.013534/80
srfr_full_d2_avg_weight_end ##-0.06266917

##manually generated minima, maxima, and total
##number_minima: 1
##number_maxima: 1
##number_extrema: 2

##15. Sinusoidal Rise, Fall, Rise (Partial) ---------------------------------------------------------------------------------------------------------

##(Note from ##11) 
##We thought that it was important to test if it would matter to participants whether a sinusoidal lifeline starts at the middle
##of the graph while keeping the same number of fluctuations (peaks and valleys) and the same endpoint (rising to 100 or falling to 0.)
##One way to do this was to "shrink" a full graph to make a "partial" one and keep all features intact except for the starting point.

sin_rfr_partial <- function(x) {50+50*(sin(x*0.0982))} ##function
sin_rfr_partial_plot <- plot(sin_rfr_partial, lwd = 3, xlim = c(0,80), ylim = c(0,100), main = "Sinusoidal Rise, Fall, Rise (Partial)", 
                             xlab = "Age", ylab = "Meaning", col = "dodgerblue2", cex.lab = 1.5) ##plot function
lifelinesZ[15, ]

sin_rfr_partial_D1 <- D(50+50*(sin(x*0.0982)) ~ x) ##first derivative
sin_rfr_partial_D1
sin_rfr_partial_D1_plot <- plot(sin_rfr_partial_D1, lwd = 3, xlim = c(0,80), ylim = c(-10,10), 
                                main = "First Derivative of Sinusoidal RFR (Partial)", col = "dodgerblue2", cex.lab = 1.5)
sin_rfr_partial_D2 <- D(50+50*(sin(x*0.0982)) ~ x & x) ##second derivative
sin_rfr_partial_D2
sin_rfr_partial_D2_plot <- plot(sin_rfr_partial_D2, lwd = 3, xlim = c(0,80), ylim = c(-2,2), 
                                main = "Second Derivative of Sinusoidal RFR (Partial)", col = "dodgerblue2", cex.lab = 1.5)

sin_rfr_partial_area <- integrate(sin_rfr_partial, 0, 80) ##area
sin_rfr_partial_area ##4510.193

sin_rfr_partial_min <- optimize(sin_rfr_partial, interval = c(0,80)) ##minimum
sin_rfr_partial_min$objective ##0  
sin_rfr_partial_max <- optimize(sin_rfr_partial, interval = c(0,80), maximum = TRUE) ##maximum
sin_rfr_partial_max$objective ##100

##D1 unweighted sum
srfr_partial_d1_sum_unweight <- integrate(sin_rfr_partial_D1, 0, 80)
srfr_partial_d1_sum_unweight ##49.9999

##D1 weighted prime: finding sum of various integrals (ages 0-18, 18-30, 30-80)
integrate(sin_rfr_partial_D1, 0, 18) ##49.03483
integrate(sin_rfr_partial_D1, 18, 30) ##-39.31743
-39.31743*2 ##weighing the prime (ages 18-30) by 2 = -78.63486
integrate(sin_rfr_partial_D1, 30, 80) ##40.2825
srfr_partial_d1_sum_weight_prime <- 49.03483-78.63486+40.2825
srfr_partial_d1_sum_weight_prime ##10.68247

##D1 ascending weights: finding sum of various integrals (using weights of 0.25, 0.5, 0.75, and 1)
integrate(sin_rfr_partial_D1, 0, 20) ##46.18432
46.18432*0.25 ##11.54608
integrate(sin_rfr_partial_D1, 20, 40) ##-81.57532
-81.57532*0.5 ##-40.78766
integrate(sin_rfr_partial_D1, 40, 60) ##16.32678
16.32678*0.75 ##12.24508
integrate(sin_rfr_partial_D1, 60, 80) ##69.06412
69.06412*1 ##69.06412
srfr_partial_d1_sum_weight_asc <- 11.54608-40.78766+12.24508+69.06412
srfr_partial_d1_sum_weight_asc ##52.06762

##D1 descending weights: finding sum of various integrals (using weights of 1, 0.75, 0.5, and 0.25)
integrate(sin_rfr_partial_D1, 0, 20) ##46.18432
46.18432*1 ##46.18432
integrate(sin_rfr_partial_D1, 20, 40) ##-81.57532
-81.57532*0.75 ##-61.18149
integrate(sin_rfr_partial_D1, 40, 60) ##16.32678
16.32678*0.5 ##8.16339
integrate(sin_rfr_partial_D1, 60, 80) ##69.06412
69.06412*0.25 ##17.26603
srfr_partial_d1_sum_weight_des <- 46.18432-61.18149+8.16339+17.26603
srfr_partial_d1_sum_weight_des ##10.43225

##D1 weighted end of life: finding sum of various integrals (ages 0-60, 60-80)
integrate(sin_rfr_partial_D1, 0, 60) ##-19.06422
integrate(sin_rfr_partial_D1, 60, 80) ##69.06412
69.06412*2 ##weighing the end of life (ages 60-80) by 2 = 138.1282
srfr_partial_d1_sum_weight_end <- -19.06422+138.1282
srfr_partial_d1_sum_weight_end ##119.064

##D1 unweighted and weighted averages (sum/80)
srfr_partial_d1_avg_unweight <- 49.9999/80
srfr_partial_d1_avg_unweight ##0.6249987
srfr_partial_d1_avg_weight_prime <- 10.68247/80
srfr_partial_d1_avg_weight_prime ##0.1335309
srfr_partial_d1_avg_weight_asc <- 52.06762/80
srfr_partial_d1_avg_weight_asc ##0.6508452
srfr_partial_d1_avg_weight_des <- 10.43225/80
srfr_partial_d1_avg_weight_des ##0.1304031
srfr_partial_d1_avg_weight_end <- 119.064/80
srfr_partial_d1_avg_weight_end ##1.4883

##D2 unweighted sum
srfr_partial_d2_sum_unweight <- integrate(sin_rfr_partial_D2, 0, 80)
srfr_partial_d2_sum_unweight ##-4.91991

##D2 weighted prime: finding sum of various integrals (ages 0-18, 18-30, 30-80)
integrate(sin_rfr_partial_D2, 0, 18) ##-5.87008
integrate(sin_rfr_partial_D2, 18, 30) ##-3.856299 
-3.856299 *2 ##weighing the prime (ages 18-30) by 2 = -7.712598
integrate(sin_rfr_partial_D2, 30, 80) ##4.806469
srfr_partial_d2_sum_weight_prime <- -5.87008-7.712598+4.806469
srfr_partial_d2_sum_weight_prime ##-8.776209

##D2 ascending weights: finding sum of various integrals (using weights of 0.25, 0.5, 0.75, and 1)
integrate(sin_rfr_partial_D2, 0, 20) ##-6.791264
-6.791264*0.25 ##-1.697816
integrate(sin_rfr_partial_D2, 20, 40) ##-1.587124
-1.587124*0.5 ##-0.793562
integrate(sin_rfr_partial_D2, 40, 60) ##8.007476
8.007476*0.75 ##6.005607
integrate(sin_rfr_partial_D2, 60, 80) ##-4.548998
-4.548998*1 ##-4.548998
srfr_partial_d2_sum_weight_asc <- -1.697816-0.793562+6.005607-4.548998
srfr_partial_d2_sum_weight_asc ##-1.034769

##D2 descending weights: finding sum of various integrals (using weights of 1, 0.75, 0.5, and 0.25)
integrate(sin_rfr_partial_D2, 0, 20) ##-6.791264
-6.791264*1 ##-6.791264
integrate(sin_rfr_partial_D2, 20, 40) ##-1.587124
-1.587124*0.75 ##-1.190343
integrate(sin_rfr_partial_D2, 40, 60) ##8.007476
8.007476*0.5 ##4.003738
integrate(sin_rfr_partial_D2, 60, 80) ##-4.548998
-4.548998*0.25 ##-1.13725
srfr_partial_d2_sum_weight_des <- -6.791264-1.190343+4.003738-1.13725
srfr_partial_d2_sum_weight_des ##-5.115119

##D2 weighted end of life: finding sum of various integrals (ages 0-60, 60-80)
integrate(sin_rfr_partial_D2, 0, 60) ##-0.3709123
integrate(sin_rfr_partial_D2, 60, 80) ##-4.548998
-4.548998*2 ##weighing the end of life (ages 60-80) by 2 = -9.097996
srfr_partial_d2_sum_weight_end <- -0.3709123-9.097996
srfr_partial_d2_sum_weight_end ##-9.468908

##D2 unweighted and weighted averages (sum/80)
srfr_partial_d2_avg_unweight <- -4.91991/80
srfr_partial_d2_avg_unweight ##-0.06149887
srfr_partial_d2_avg_weight_prime <- -8.776209/80
srfr_partial_d2_avg_weight_prime ##-0.1097026
srfr_partial_d2_avg_weight_asc <- -1.034769/80
srfr_partial_d2_avg_weight_asc ##-0.01293461
srfr_partial_d2_avg_weight_des <- -5.115119/80
srfr_partial_d2_avg_weight_des ##-0.06393899
srfr_partial_d2_avg_weight_end <- -9.468908/80
srfr_partial_d2_avg_weight_end ##-0.1183614

##manually generated minima, maxima, and total
##number_minima: 1
##number_maxima: 1
##number_extrema: 2

##Sinusoidal - Fall, Rise, Fall (Oedipus) ---------------------------------------------------------------------------------------------------------

##16. Sinusoidal Fall, Rise, Fall (Full) ---------------------------------------------------------------------------------------------------------

sin_frf_full <- function(x) {50+50*(cos(x*0.1185))} ##function
sin_frf_full_plot <- plot(sin_frf_full, lwd = 3, xlim = c(0,80), ylim = c(0,100), main = "Sinusoidal Fall, Rise, Fall (Full)", 
                                xlab = "Age", ylab = "Meaning", col = "dodgerblue2", cex.lab = 1.5) ##plot function
lifelinesZ[16, ]

sin_frf_full_D1 <- D(50+50*(cos(x*0.1185)) ~ x) ##first derivative
sin_frf_full_D1
sin_frf_full_D1_plot <- plot(sin_frf_full_D1, lwd = 3, xlim = c(0,80), ylim = c(-10,10), 
                                   main = "First Derivative of Sinusoidal FRF (Full)", col = "dodgerblue2", cex.lab = 1.5)
sin_frf_full_D2 <- D(50+50*(cos(x*0.1185)) ~ x & x) ##second derivative
sin_frf_full_D2
sin_frf_full_D2_plot <- plot(sin_frf_full_D2, lwd = 3, xlim = c(0,80), ylim = c(-2,2), 
                                   main = "Second Derivative of Sinusoidal FRF (Full)", col = "dodgerblue2", cex.lab = 1.5)

sin_frf_full_area <- integrate(sin_frf_full, 0, 80) ##area
sin_frf_full_area ##3976.711

sin_frf_full_min <- optimize(sin_frf_full, interval = c(0,80)) ##minimum
sin_frf_full_min$objective ##0  
sin_frf_full_max <- optimize(sin_frf_full, interval = c(0,80), maximum = TRUE) ##maximum
sin_frf_full_max$objective ##100

##D1 unweighted sum
sfrf_full_d1_sum_unweight <- integrate(sin_frf_full_D1, 0, 80)
sfrf_full_d1_sum_unweight ##-99.92378

##D1 weighted prime: finding sum of various integrals (ages 0-18, 18-30, 30-80)
integrate(sin_frf_full_D1, 0, 18) ##-76.6526
integrate(sin_frf_full_D1, 18, 30) ##-19.13527
-19.13527*2 ##weighing the prime (ages 18-30) by 2 = -38.27054
integrate(sin_frf_full_D1, 30, 80) ##-4.135917
sfrf_full_d1_sum_weight_prime <- -76.6526-38.27054-4.135917
sfrf_full_d1_sum_weight_prime ##-119.0591

##D1 ascending weights: finding sum of various integrals (using weights of 0.25, 0.5, 0.75, and 1)
integrate(sin_frf_full_D1, 0, 20) ##-85.84005 
-85.84005*0.25 ##-21.46001
integrate(sin_frf_full_D1, 20, 40) ##37.22043
37.22043*0.5 ##18.61022
integrate(sin_frf_full_D1, 40, 60) ##32.48077
32.48077*0.75 ##24.36058
integrate(sin_frf_full_D1, 60, 80) ##-83.78493
-83.78493*1 ##-83.78493
sfrf_full_d1_sum_weight_asc <- -21.46001+18.61022+24.36058-83.78493
sfrf_full_d1_sum_weight_asc ##-62.27414

##D1 descending weights: finding sum of various integrals (using weights of 1, 0.75, 0.5, and 0.25)
integrate(sin_frf_full_D1, 0, 20) ##-85.84005 
-85.84005*1 ##-85.84005
integrate(sin_frf_full_D1, 20, 40) ##37.22043
37.22043*0.75 ##27.91532
integrate(sin_frf_full_D1, 40, 60) ##32.48077
32.48077*0.5 ##16.24038
integrate(sin_frf_full_D1, 60, 80) ##-83.78493
-83.78493*0.25 ##-20.94623
sfrf_full_d1_sum_weight_des <- -85.84005+27.91532+16.24038-20.94623
sfrf_full_d1_sum_weight_des ##-62.63058

##D1 weighted end of life: finding sum of various integrals (ages 0-60, 60-80)
integrate(sin_frf_full_D1, 0, 60) ##-16.13886
integrate(sin_frf_full_D1, 60, 80) ##-83.78493
-83.78493*2 ##weighing the end of life (ages 60-80) by 2 = -167.5699
sfrf_full_d1_sum_weight_end <- -16.13886-167.5699
sfrf_full_d1_sum_weight_end ##-183.7088

##D1 unweighted and weighted averages (sum/80)
sfrf_full_d1_avg_unweight <- -99.92378/80
sfrf_full_d1_avg_unweight ##-1.249047
sfrf_full_d1_avg_weight_prime <- -119.0591/80
sfrf_full_d1_avg_weight_prime ##-1.488239
sfrf_full_d1_avg_weight_asc <- -62.27414/80
sfrf_full_d1_avg_weight_asc ##-0.7784268
sfrf_full_d1_avg_weight_des <- -62.63058/80
sfrf_full_d1_avg_weight_des ##-0.7828823
sfrf_full_d1_avg_weight_end <- -183.7088/80
sfrf_full_d1_avg_weight_end ##-2.29636

##D2 unweighted sum
sfrf_full_d2_sum_unweight <- integrate(sin_frf_full_D2, 0, 80)
sfrf_full_d2_sum_unweight ##0.3270243

##D2 weighted prime: finding sum of various integrals (ages 0-18, 18-30, 30-80)
integrate(sin_frf_full_D2, 0, 18) ##-5.013039
integrate(sin_frf_full_D2, 18, 30) ##7.393301
7.393301*2 ##weighing the prime (ages 18-30) by 2 = 14.7866
integrate(sin_frf_full_D2, 30, 80) ##-2.053238
sfrf_full_d2_sum_weight_prime <- -5.013039+14.7866-2.053238
sfrf_full_d2_sum_weight_prime ##7.720323

##D2 ascending weights: finding sum of various integrals (using weights of 0.25, 0.5, 0.75, and 1)
integrate(sin_frf_full_D2, 0, 20) ##-4.131371
-4.131371*0.25 ##-1.032843
integrate(sin_frf_full_D2, 20, 40) ##10.05411
10.05411*0.5 ##5.027055
integrate(sin_frf_full_D2, 40, 60) ##-10.28223
-10.28223*0.75 ##-7.711673
integrate(sin_frf_full_D2, 60, 80) ##4.686509
4.686509*1 ##4.686509
sfrf_full_d2_sum_weight_asc <- -1.032843+5.027055-7.711673+4.686509
sfrf_full_d2_sum_weight_asc ##0.969048

##D2 descending weights: finding sum of various integrals (using weights of 1, 0.75, 0.5, and 0.25)
integrate(sin_frf_full_D2, 0, 20) ##-4.131371
-4.131371*1 ##-4.131371
integrate(sin_frf_full_D2, 20, 40) ##10.05411
10.05411*0.75 ##7.540582
integrate(sin_frf_full_D2, 40, 60) ##-10.28223
-10.28223*0.5 ##-5.141115
integrate(sin_frf_full_D2, 60, 80) ##4.686509
4.686509*0.25 ##1.171627
sfrf_full_d2_sum_weight_des <- -4.131371+7.540582-5.141115+1.171627
sfrf_full_d2_sum_weight_des ##-0.560277

##D2 weighted end of life: finding sum of various integrals (ages 0-60, 60-80)
integrate(sin_frf_full_D2, 0, 60) ##-4.359484
integrate(sin_frf_full_D2, 60, 80) ##4.686509
4.686509*2 ##weighing the end of life (ages 60-80) by 2 = 9.373018
sfrf_full_d2_sum_weight_end <- -4.359484+9.373018
sfrf_full_d2_sum_weight_end ##5.013534

##D2 unweighted and weighted averages (sum/80)
sfrf_full_d2_avg_unweight <- 0.3270243/80
sfrf_full_d2_avg_unweight ##0.004087804
sfrf_full_d2_avg_weight_prime <- 7.720323/80
sfrf_full_d2_avg_weight_prime ##0.09650404
sfrf_full_d2_avg_weight_asc <- 0.969048/80
sfrf_full_d2_avg_weight_asc ##0.0121131
sfrf_full_d2_avg_weight_des <- -0.560277/80
sfrf_full_d2_avg_weight_des ##-0.007003462
sfrf_full_d2_avg_weight_end <- 5.013534/80
sfrf_full_d2_avg_weight_end ##0.06266917

##manually generated minima, maxima, and total
##number_minima: 1
##number_maxima: 1
##number_extrema: 2

##17. Sinusoidal Fall, Rise, Fall (Partial) ---------------------------------------------------------------------------------------------------------

##(Note from ##11) 
##We thought that it was important to test if it would matter to participants whether a sinusoidal lifeline starts at the middle
##of the graph while keeping the same number of fluctuations (peaks and valleys) and the same endpoint (rising to 100 or falling to 0.)
##One way to do this was to "shrink" a full graph to make a "partial" one and keep all features intact except for the starting point.

sin_frf_partial <- function(x) {50-50*(sin(x*0.0982))} ##function
sin_frf_partial_plot <- plot(sin_frf_partial, lwd = 3, xlim = c(0,80), ylim = c(0,100), main = "Sinusoidal Fall, Rise, Fall (Partial)", 
                             xlab = "Age", ylab = "Meaning", col = "dodgerblue2", cex.lab = 1.5) ##plot function
lifelinesZ[17, ]

sin_frf_partial_D1 <- D(50-50*(sin(x*0.0982)) ~ x) ##first derivative
sin_frf_partial_D1
sin_frf_partial_D1_plot <- plot(sin_frf_partial_D1, lwd = 3, xlim = c(0,80), ylim = c(-10,10), 
                                main = "First Derivative of Sinusoidal FRF (Partial)", col = "dodgerblue2", cex.lab = 1.5)
sin_frf_partial_D2 <- D(50-50*(sin(x*0.0982)) ~ x & x) ##second derivative
sin_frf_partial_D2
sin_frf_partial_D2_plot <- plot(sin_frf_partial_D2, lwd = 3, xlim = c(0,80), ylim = c(-2,2), 
                                main = "Second Derivative of Sinusoidal FRF (Partial)", col = "dodgerblue2", cex.lab = 1.5)

sin_frf_partial_area <- integrate(sin_frf_partial, 0, 80) ##area
sin_frf_partial_area ##3489.807

sin_frf_partial_min <- optimize(sin_frf_partial, interval = c(0,80)) ##minimum
sin_frf_partial_min$objective ##0 
sin_frf_partial_max <- optimize(sin_frf_partial, interval = c(0,80), maximum = TRUE) ##maximum
sin_frf_partial_max$objective ##100 

##D1 unweighted sum
sfrf_partial_d1_sum_unweight <- integrate(sin_frf_partial_D1, 0, 80)
sfrf_partial_d1_sum_unweight ##-49.9999

##D1 weighted prime: finding sum of various integrals (ages 0-18, 18-30, 30-80)
integrate(sin_frf_partial_D1, 0, 18) ##-49.03483
integrate(sin_frf_partial_D1, 18, 30) ##39.31743
39.31743*2 ##weighing the prime (ages 18-30) by 2 = 78.63486
integrate(sin_frf_partial_D1, 30, 80) ##-40.2825
sfrf_partial_d1_sum_weight_prime <- -49.03483+78.63486-40.2825
sfrf_partial_d1_sum_weight_prime ##-10.68247

##D1 ascending weights: finding sum of various integrals (using weights of 0.25, 0.5, 0.75, and 1)
integrate(sin_frf_partial_D1, 0, 20) ##-46.18432
-46.18432*0.25 ##-11.54608
integrate(sin_frf_partial_D1, 20, 40) ##81.57532
81.57532*0.5 ##40.78766
integrate(sin_frf_partial_D1, 40, 60) ##-16.32678
-16.32678*0.75 ##-12.24508
integrate(sin_frf_partial_D1, 60, 80) ##-69.06412
-69.06412*1 ##-69.06412
sfrf_partial_d1_sum_weight_asc <- -11.54608+40.78766-12.24508-69.06412
sfrf_partial_d1_sum_weight_asc ##-52.06762

##D1 descending weights: finding sum of various integrals (using weights of 1, 0.75, 0.5, and 0.25)
integrate(sin_frf_partial_D1, 0, 20) ##-46.18432
-46.18432*1 ##-46.18432
integrate(sin_frf_partial_D1, 20, 40) ##81.57532
81.57532*0.75 ##61.18149
integrate(sin_frf_partial_D1, 40, 60) ##-16.32678
-16.32678*0.5 ##-8.16339
integrate(sin_frf_partial_D1, 60, 80) ##-69.06412
-69.06412*0.25 ##-17.26603
sfrf_partial_d1_sum_weight_des <- -46.18432+61.18149-8.16339-17.26603
sfrf_partial_d1_sum_weight_des ##-10.43225

##D1 weighted end of life: finding sum of various integrals (ages 0-60, 60-80)
integrate(sin_frf_partial_D1, 0, 60) ##19.06422
integrate(sin_frf_partial_D1, 60, 80) ##-69.06412
-69.06412*2 ##weighing the end of life (ages 60-80) by 2 = -138.1282
sfrf_partial_d1_sum_weight_end <- 19.06422-138.1282
sfrf_partial_d1_sum_weight_end ##-119.064

##D1 unweighted and weighted averages (sum/80)
sfrf_partial_d1_avg_unweight <- -49.9999/80
sfrf_partial_d1_avg_unweight ##-0.6249987
sfrf_partial_d1_avg_weight_prime <- -10.68247/80
sfrf_partial_d1_avg_weight_prime ##-0.1335309
sfrf_partial_d1_avg_weight_asc <- -52.06762/80
sfrf_partial_d1_avg_weight_asc ##-0.6508452
sfrf_partial_d1_avg_weight_des <- -10.43225/80
sfrf_partial_d1_avg_weight_des ##-0.1304031
sfrf_partial_d1_avg_weight_end <- -119.064/80
sfrf_partial_d1_avg_weight_end ##-1.4883

##D2 unweighted sum
sfrf_partial_d2_sum_unweight <- integrate(sin_frf_partial_D2, 0, 80)
sfrf_partial_d2_sum_unweight ##4.91991

##D2 weighted prime: finding sum of various integrals (ages 0-18, 18-30, 30-80)
integrate(sin_frf_partial_D2, 0, 18) ##5.87008
integrate(sin_frf_partial_D2, 18, 30) ##3.856299
3.856299*2 ##weighing the prime (ages 18-30) by 2 = 7.712598
integrate(sin_frf_partial_D2, 30, 80) ##-4.806469
sfrf_partial_d2_sum_weight_prime <- 5.87008+7.712598-4.806469
sfrf_partial_d2_sum_weight_prime ##8.776209

##D2 ascending weights: finding sum of various integrals (using weights of 0.25, 0.5, 0.75, and 1)
integrate(sin_frf_partial_D2, 0, 20) ##6.791264
6.791264*0.25 ##1.697816
integrate(sin_frf_partial_D2, 20, 40) ##1.587124
1.587124*0.5 ##0.793562
integrate(sin_frf_partial_D2, 40, 60) ##-8.007476
-8.007476*0.75 ##-6.005607
integrate(sin_frf_partial_D2, 60, 80) ##4.548998
4.548998*1 ##4.548998
sfrf_partial_d2_sum_weight_asc <- 1.697816+0.793562-6.005607+4.548998
sfrf_partial_d2_sum_weight_asc ##1.034769

##D2 descending weights: finding sum of various integrals (using weights of 1, 0.75, 0.5, and 0.25)
integrate(sin_frf_partial_D2, 0, 20) ##6.791264
6.791264*1 ##6.791264
integrate(sin_frf_partial_D2, 20, 40) ##1.587124
1.587124*0.75 ##1.190343
integrate(sin_frf_partial_D2, 40, 60) ##-8.007476
-8.007476*0.5 ##-4.003738
integrate(sin_frf_partial_D2, 60, 80) ##4.548998
4.548998*0.25 ##1.13725
sfrf_partial_d2_sum_weight_des <- 6.791264+1.190343-4.003738+1.13725
sfrf_partial_d2_sum_weight_des ##5.115119

##D2 weighted end of life: finding sum of various integrals (ages 0-60, 60-80)
integrate(sin_frf_partial_D2, 0, 60) ##0.3709123
integrate(sin_frf_partial_D2, 60, 80) ##4.548998
4.548998*2 ##weighing the end of life (ages 60-80) by 2 = 9.097996
sfrf_partial_d2_sum_weight_end <- 0.3709123+9.097996
sfrf_partial_d2_sum_weight_end ##9.468908

##D2 unweighted and weighted averages (sum/80)
sfrf_partial_d2_avg_unweight <- 4.91991/80
sfrf_partial_d2_avg_unweight ##0.06149887
sfrf_partial_d2_avg_weight_prime <- 8.776209/80
sfrf_partial_d2_avg_weight_prime ##0.1097026
sfrf_partial_d2_avg_weight_asc <- 1.034769/80
sfrf_partial_d2_avg_weight_asc ##0.01293461
sfrf_partial_d2_avg_weight_des <- 5.115119/80
sfrf_partial_d2_avg_weight_des ##0.06393899
sfrf_partial_d2_avg_weight_end <- 9.468908/80
sfrf_partial_d2_avg_weight_end ##0.1183614

##manually generated minima, maxima, and total
##number_minima: 1
##number_maxima: 1
##number_extrema: 2

##Logistic - Sharp Rise & Sharp Fall ---------------------------------------------------------------------------------------------------------

##18. Logistic Rise ---------------------------------------------------------------------------------------------------------

logistic_rise <- function(x) {100/(1+(exp(1))^-(x-40))} ##function
logistic_rise_plot <- plot(logistic_rise, lwd = 3, xlim = c(0,80), ylim = c(0,100), main = "Logistic Rise", 
                                xlab = "Age", ylab = "Meaning", col = "dodgerblue2", cex.lab = 1.5) ##plot function
lifelinesZ[18, ]

logistic_rise_D1 <- D(100/(1+(exp(1))^-(x-40)) ~ x) ##first derivative
logistic_rise_D1
logistic_rise_D1_plot <- plot(logistic_rise_D1, lwd = 3, xlim = c(0,80), ylim = c(-10,10), 
                                   main = "First Derivative of Logistic Rise", col = "dodgerblue2", cex.lab = 1.5)
logistic_rise_D2 <- D(100/(1+(exp(1))^-(x-40)) ~ x & x) ##second derivative
logistic_rise_D2
logistic_rise_D2_plot <- plot(logistic_rise_D2, lwd = 3, xlim = c(0,80), ylim = c(-2,2), 
                                   main = "Second Derivative of Logistic Rise", col = "dodgerblue2", cex.lab = 1.5)

logistic_rise_area <- integrate(logistic_rise, 0, 80) ##area
logistic_rise_area ##4000

logistic_rise_min <- optimize(logistic_rise, interval = c(0,80)) ##minimum
logistic_rise_min$objective ##0 
logistic_rise_max <- optimize(logistic_rise, interval = c(0,80), maximum = TRUE) ##maximum
logistic_rise_max$objective ##100 

##D1 unweighted sum
lr_d1_sum_unweight <- integrate(logistic_rise_D1, 0, 80)
lr_d1_sum_unweight ##100

##D1 weighted prime: finding sum of various integrals (ages 0-18, 18-30, 30-80)
integrate(logistic_rise_D1, 0, 18) ##2.789468e-08
integrate(logistic_rise_D1, 18, 30) ##0.004539759
0.004539759*2 ##weighing the prime (ages 18-30) by 2 = 0.009079518
integrate(logistic_rise_D1, 30, 80) ##99.99546
lr_d1_sum_weight_prime <- 2.789468e-08+0.009079518+99.99546
lr_d1_sum_weight_prime ##100.0045

##D1 ascending weights: finding sum of various integrals (using weights of 0.25, 0.5, 0.75, and 1)
integrate(logistic_rise_D1, 0, 20) ##2.061154e-07
2.061154e-07*0.25 ##5.152885e-08
integrate(logistic_rise_D1, 20, 40) ##50
50*0.5 ##25
integrate(logistic_rise_D1, 40, 60) ##50
50*0.75 ##37.5
integrate(logistic_rise_D1, 60, 80) ##2.061154e-07
2.061154e-07*1 ##2.061154e-07
lr_d1_sum_weight_asc <- 5.152885e-08+25+37.5+2.061154e-07
lr_d1_sum_weight_asc ##62.5

##D1 descending weights: finding sum of various integrals (using weights of 1, 0.75, 0.5, and 0.25)
integrate(logistic_rise_D1, 0, 20) ##2.061154e-07
2.061154e-07*1 ##2.061154e-07
integrate(logistic_rise_D1, 20, 40) ##50
50*0.75 ##37.5
integrate(logistic_rise_D1, 40, 60) ##50
50*0.5 ##25
integrate(logistic_rise_D1, 60, 80) ##2.061154e-07
2.061154e-07*0.25 ##5.152885e-08
lr_d1_sum_weight_des <- 2.061154e-07+37.5+25+5.152885e-08
lr_d1_sum_weight_des ##62.5

##D1 weighted end of life: finding sum of various integrals (ages 0-60, 60-80)
integrate(logistic_rise_D1, 0, 60) ##100
integrate(logistic_rise_D1, 60, 80) ##2.061154e-07
2.061154e-07*2 ##weighing the end of life (ages 60-80) by 2 = 4.122308e-07
lr_d1_sum_weight_end <- 100+4.122308e-07
lr_d1_sum_weight_end ##100

##D1 unweighted and weighted averages (sum/80)
lr_d1_avg_unweight <- 100/80
lr_d1_avg_unweight ##1.25
lr_d1_avg_weight_prime <- 100.0045/80
lr_d1_avg_weight_prime ##1.250056
lr_d1_avg_weight_asc <- 62.5/80
lr_d1_avg_weight_asc ##0.78125
lr_d1_avg_weight_des <- 62.5/80
lr_d1_avg_weight_des ##0.78125
lr_d1_avg_weight_end <- 100/80
lr_d1_avg_weight_end ##1.25

##D2 unweighted sum
lr_d2_sum_unweight <- integrate(logistic_rise_D2, 0, 80)
lr_d2_sum_unweight ##-3.28049e-16

##D2 weighted prime: finding sum of various integrals (ages 0-18, 18-30, 30-80)
integrate(logistic_rise_D2, 0, 18) ##2.789468e-08
integrate(logistic_rise_D2, 18, 30) ##0.004539553
0.004539553*2 ##weighing the prime (ages 18-30) by 2 = 0.009079106
integrate(logistic_rise_D2, 30, 80) ##-0.004539581
lr_d2_sum_weight_prime <- 2.789468e-08+0.009079106-0.004539581
lr_d2_sum_weight_prime ##0.004539553

##D2 ascending weights: finding sum of various integrals (using weights of 0.25, 0.5, 0.75, and 1)
integrate(logistic_rise_D2, 0, 20) ##2.061154e-07
2.061154e-07*0.25 ##5.152885e-08
integrate(logistic_rise_D2, 20, 40) ##25
25*0.5 ##12.5
integrate(logistic_rise_D2, 40, 60) ##-25
-25*0.75 ##-18.75
integrate(logistic_rise_D2, 60, 80) ##-2.061154e-07
-2.061154e-07*1 ##-2.061154e-07
lr_d2_sum_weight_asc <- 5.152885e-08+12.5-18.75-2.061154e-07
lr_d2_sum_weight_asc ##-6.25

##D2 descending weights: finding sum of various integrals (using weights of 1, 0.75, 0.5, and 0.25)
integrate(logistic_rise_D2, 0, 20) ##2.061154e-07
2.061154e-07*1 ##2.061154e-07
integrate(logistic_rise_D2, 20, 40) ##25
25*0.75 ##18.75
integrate(logistic_rise_D2, 40, 60) ##-25
-25*0.5 ##-12.5
integrate(logistic_rise_D2, 60, 80) ##-2.061154e-07
-2.061154e-07*0.25 ##-5.152885e-08
lr_d2_sum_weight_des <- -2.061154e-07+18.75-12.5-5.152885e-08
lr_d2_sum_weight_des ##6.25

##D2 weighted end of life: finding sum of various integrals (ages 0-60, 60-80)
integrate(logistic_rise_D2, 0, 60) ##2.061153e-07
integrate(logistic_rise_D2, 60, 80) ##-2.061154e-07
-2.061154e-07*2 ##weighing the end of life (ages 60-80) by 2 = -4.122308e-07
lr_d2_sum_weight_end <- 2.061153e-07-4.122308e-07
lr_d2_sum_weight_end ##-2.061155e-07

##D2 unweighted and weighted averages (sum/80)
lr_d2_avg_unweight <- -3.28049e-16/80
lr_d2_avg_unweight ##-4.100613e-18
lr_d2_avg_weight_prime <- 0.004539553/80
lr_d2_avg_weight_prime ##5.674441e-05
lr_d2_avg_weight_asc <- -6.25/80
lr_d2_avg_weight_asc ##-0.078125
lr_d2_avg_weight_des <- 6.25/80
lr_d2_avg_weight_des ##0.078125
lr_d2_avg_weight_end <- -2.061155e-07/80
lr_d2_avg_weight_end ##-2.576444e-09

##manually generated minima, maxima, and total
##number_minima: 0
##number_maxima: 0
##number_extrema: 0

##19. Logistic Fall ---------------------------------------------------------------------------------------------------------

logistic_fall <- function(x) {100-100/(1+(exp(1))^-(x-40))} ##function
logistic_fall_plot <- plot(logistic_fall, lwd = 3, xlim = c(0,80), ylim = c(0,100), main = "Logistic Fall", 
                              xlab = "Age", ylab = "Meaning", col = "dodgerblue2", cex.lab = 1.5) ##plot function
lifelinesZ[19, ]

logistic_fall_D1 <- D(100-100/(1+(exp(1))^-(x-40)) ~ x) ##first derivative
logistic_fall_D1
logistic_fall_D1_plot <- plot(logistic_fall_D1, lwd = 3, xlim = c(0,80), ylim = c(-10,10), 
                                 main = "First Derivative of Logistic Fall", col = "dodgerblue2", cex.lab = 1.5)
logistic_fall_D2 <- D(100-100/(1+(exp(1))^-(x-40)) ~ x & x) ##second derivative
logistic_fall_D2
logistic_fall_D2_plot <- plot(logistic_fall_D2, lwd = 3, xlim = c(0,80), ylim = c(-2,2), 
                                 main = "Second Derivative of Logistic Fall", col = "dodgerblue2", cex.lab = 1.5)

logistic_fall_area <- integrate(logistic_fall, 0, 80) ##area
logistic_fall_area ##4000

logistic_fall_min <- optimize(logistic_fall, interval = c(0,80)) ##minimum
logistic_fall_min$objective ##0 
logistic_fall_max <- optimize(logistic_fall, interval = c(0,80), maximum = TRUE) ##maximum
logistic_fall_max$objective ##100 

##D1 unweighted sum
lf_d1_sum_unweight <- integrate(logistic_fall_D1, 0, 80)
lf_d1_sum_unweight ##-100

##D1 weighted prime: finding sum of various integrals (ages 0-18, 18-30, 30-80)
integrate(logistic_fall_D1, 0, 18) ##-2.789468e-08
integrate(logistic_fall_D1, 18, 30) ##-0.004539759
-0.004539759*2 ##weighing the prime (ages 18-30) by 2 = -0.009079518
integrate(logistic_fall_D1, 30, 80) ##-99.99546
lf_d1_sum_weight_prime <- -2.789468e-08-0.009079518-99.99546
lf_d1_sum_weight_prime ##-100.0045

##D1 ascending weights: finding sum of various integrals (using weights of 0.25, 0.5, 0.75, and 1)
integrate(logistic_fall_D1, 0, 20) ##-2.061154e-07
-2.061154e-07*0.25 ##-5.152885e-08
integrate(logistic_fall_D1, 20, 40) ##-50
-50*0.5 ##-25
integrate(logistic_fall_D1, 40, 60) ##-50
-50*0.75 ##-37.5
integrate(logistic_fall_D1, 60, 80) ##-2.061154e-07
-2.061154e-07*1 ##-2.061154e-07
lf_d1_sum_weight_asc <- -5.152885e-08-25-37.5-2.061154e-07
lf_d1_sum_weight_asc ##-62.5

##D1 descending weights: finding sum of various integrals (using weights of 1, 0.75, 0.5, and 0.25)
integrate(logistic_fall_D1, 0, 20) ##-2.061154e-07
-2.061154e-07*1 ##-2.061154e-07
integrate(logistic_fall_D1, 20, 40) ##-50
-50*0.75 ##-37.5
integrate(logistic_fall_D1, 40, 60) ##-50
-50*0.5 ##-25
integrate(logistic_fall_D1, 60, 80) ##-2.061154e-07
-2.061154e-07*0.25 ##-5.152885e-08
lf_d1_sum_weight_des <- -2.061154e-07-37.5-25-5.152885e-08
lf_d1_sum_weight_des ##-62.5

##D1 weighted end of life: finding sum of various integrals (ages 0-60, 60-80)
integrate(logistic_fall_D1, 0, 60) ##-100
integrate(logistic_fall_D1, 60, 80) ##-2.061154e-07
-2.061154e-07*2 ##weighing the end of life (ages 60-80) by 2 = 
lf_d1_sum_weight_end <- -100-4.122308e-07
lf_d1_sum_weight_end ##-100

##D1 unweighted and weighted averages (sum/80)
lf_d1_avg_unweight <- -100/80
lf_d1_avg_unweight ##-1.25
lf_d1_avg_weight_prime <- -100.0045/80
lf_d1_avg_weight_prime ##-1.250056
lf_d1_avg_weight_asc <- -62.5/80
lf_d1_avg_weight_asc ##-0.78125
lf_d1_avg_weight_des <- -62.5/80
lf_d1_avg_weight_des ##-0.78125
lf_d1_avg_weight_end <- -100/80
lf_d1_avg_weight_end ##-1.25

##D2 unweighted sum
lf_d2_sum_unweight <- integrate(logistic_fall_D2, 0, 80)
lf_d2_sum_unweight ##3.28049e-16

##D2 weighted prime: finding sum of various integrals (ages 0-18, 18-30, 30-80)
integrate(logistic_fall_D2, 0, 18) ##-2.789468e-08
integrate(logistic_fall_D2, 18, 30) ##-0.004539553
-0.004539553*2 ##weighing the prime (ages 18-30) by 2 = -0.009079106
integrate(logistic_fall_D2, 30, 80) ##0.004539581
lf_d2_sum_weight_prime <- -2.789468e-08+-0.009079106+0.004539581
lf_d2_sum_weight_prime ##-0.004539553

##D2 ascending weights: finding sum of various integrals (using weights of 0.25, 0.5, 0.75, and 1)
integrate(logistic_fall_D2, 0, 20) ##-2.061154e-07
-2.061154e-07*0.25 ##-5.152885e-08
integrate(logistic_fall_D2, 20, 40) ##-25
-25*0.5 ##-12.5
integrate(logistic_fall_D2, 40, 60) ##25
25*0.75 ##18.75
integrate(logistic_fall_D2, 60, 80) ##-2.061154e-07
-2.061154e-07*1 ##-2.061154e-07
lf_d2_sum_weight_asc <- -5.152885e-08-12.5+18.75+-2.061154e-07
lf_d2_sum_weight_asc ##6.25

##D2 descending weights: finding sum of various integrals (using weights of 1, 0.75, 0.5, and 0.25)
integrate(logistic_fall_D2, 0, 20) ##-2.061154e-07
-2.061154e-07*1 ##-2.061154e-07
integrate(logistic_fall_D2, 20, 40) ##-25
-25*0.75 ##-18.75
integrate(logistic_fall_D2, 40, 60) ##25
25*0.5 ##12.5
integrate(logistic_fall_D2, 60, 80) ##-2.061154e-07
-2.061154e-07*0.25 ##-5.152885e-08
lf_d2_sum_weight_des <- -2.061154e-07-18.75+12.5-5.152885e-08
lf_d2_sum_weight_des ##-6.25

##D2 weighted end of life: finding sum of various integrals (ages 0-60, 60-80)
integrate(logistic_fall_D2, 0, 60) ##-2.061153e-07
integrate(logistic_fall_D2, 60, 80) ##2.061153e-07
2.061153e-07*2 ##weighing the end of life (ages 60-80) by 2 = 4.122306e-07
lf_d2_sum_weight_end <- -2.061153e-07+4.122306e-07
lf_d2_sum_weight_end ##2.061153e-07

##D2 unweighted and weighted averages (sum/80)
lf_d2_avg_unweight <- 3.28049e-16/80
lf_d2_avg_unweight ##4.100613e-18
lf_d2_avg_weight_prime <- -0.004539553/80
lf_d2_avg_weight_prime ##-5.674441e-05
lf_d2_avg_weight_asc <- 6.25/80
lf_d2_avg_weight_asc ##0.078125
lf_d2_avg_weight_des <- -6.25/80
lf_d2_avg_weight_des ##-0.078125
lf_d2_avg_weight_end <- 2.061153e-07/80
lf_d2_avg_weight_end ##2.576441e-09

##manually generated minima, maxima, and total
##number_minima: 0
##number_maxima: 0
##number_extrema: 0

## Multiple Peaks - Fall, Rise, Fall, Rise & Rise, Fall, Rise, Fall ---------------------------------------------------------------------------------------------------------

##20. Sinusoidal Fall, Rise, Fall, Rise (FRFR) ---------------------------------------------------------------------------------------------------------

sin_frfr <- function(x) {50-50*(sin(x*0.1375))} ##function
sin_frfr_plot <- plot(sin_frfr, lwd = 3, xlim = c(0,80), ylim = c(0,100), main = "Sinusoidal Fall, Rise, Fall, Rise", 
                              xlab = "Age", ylab = "Meaning", col = "dodgerblue2", cex.lab = 1.5) ##plot function
lifelinesZ[20, ]

sin_frfr_D1 <- D(50-50*(sin(x*0.1375)) ~ x) ##first derivative
sin_frfr_D1
sin_frfr_D1_plot <- plot(sin_frfr_D1, lwd = 3, xlim = c(0,80), ylim = c(-10,10), 
                                 main = "First Derivative of Sinusoidal FRFR", col = "dodgerblue2", cex.lab = 1.5)
sin_frfr_D2 <- D(50-50*(sin(x*0.1375)) ~ x & x) ##second derivative
sin_frfr_D2
sin_frfr_D2_plot <- plot(sin_frfr_D2, lwd = 3, xlim = c(0,80), ylim = c(-2,2), 
                                 main = "Second Derivative of Sinusoidal FRFR", col = "dodgerblue2", cex.lab = 1.5)

sin_frfr_area <- integrate(sin_frfr, 0, 80) ##area
sin_frfr_area ##3637.973

sin_frfr_min <- optimize(sin_frfr, interval = c(0,80)) ##minimum
sin_frfr_min$objective ##0 
sin_frfr_max <- optimize(sin_frfr, interval = c(0,80), maximum = TRUE) ##maximum
sin_frfr_max$objective ##100

##D1 unweighted sum
sfrfr_d1_sum_unweight <- integrate(sin_frfr_D1, 0, 80)
sfrfr_d1_sum_unweight ##49.99951

##D1 weighted prime: finding sum of various integrals (ages 0-18, 18-30, 30-80)
integrate(sin_frfr_D1, 0, 18) ##-30.91558
integrate(sin_frfr_D1, 18, 30) ##72.53511
72.53511*2 ##weighing the prime (ages 18-30) by 2 = 145.0702
integrate(sin_frfr_D1, 30, 80) ##8.379985
sfrfr_d1_sum_weight_prime <- -30.91558+145.0702+8.379985
sfrfr_d1_sum_weight_prime ##122.5346

##D1 ascending weights: finding sum of various integrals (using weights of 0.25, 0.5, 0.75, and 1)
integrate(sin_frfr_D1, 0, 20) ##-19.08305
-19.08305*0.25 ##-4.770763
integrate(sin_frfr_D1, 20, 40) ##54.36007
54.36007*0.5 ##27.18004
integrate(sin_frfr_D1, 40, 60) ##-81.40723
-81.40723*0.75 ##-61.05542
integrate(sin_frfr_D1, 60, 80) ##96.12972
96.12972*1 ##96.12972
sfrfr_d1_sum_weight_asc <- -4.770763+27.18004-61.05542+96.12972
sfrfr_d1_sum_weight_asc ##57.48358

##D1 descending weights: finding sum of various integrals (using weights of 1, 0.75, 0.5, and 0.25)
integrate(sin_frfr_D1, 0, 20) ##-19.08305
-19.08305*1 ##-19.08305
integrate(sin_frfr_D1, 20, 40) ##54.36007
54.36007*0.75 ##40.77005
integrate(sin_frfr_D1, 40, 60) ##-81.40723
-81.40723*0.5 ##-40.70361
integrate(sin_frfr_D1, 60, 80) ##96.12972
96.12972*0.25 ##24.03243
sfrfr_d1_sum_weight_des <- -19.08305+40.77005-40.70361+24.03243
sfrfr_d1_sum_weight_des ##5.01582

##D1 weighted end of life: finding sum of various integrals (ages 0-60, 60-80)
integrate(sin_frfr_D1, 0, 60) ##-46.13021
integrate(sin_frfr_D1, 60, 80) ##96.12972
96.12972*2 ##weighing the end of life (ages 60-80) by 2 = 192.2594
sfrfr_d1_sum_weight_end <- -46.13021+192.2594
sfrfr_d1_sum_weight_end ##146.1292

##D1 unweighted and weighted averages (sum/80)
sfrfr_d1_avg_unweight <- 49.99951/80
sfrfr_d1_avg_unweight ##0.6249939
sfrfr_d1_avg_weight_prime <- 122.5346/80
sfrfr_d1_avg_weight_prime ##1.531683
sfrfr_d1_avg_weight_asc <- 57.48358/80
sfrfr_d1_avg_weight_asc ##0.7185448
sfrfr_d1_avg_weight_des <- 5.01582/80
sfrfr_d1_avg_weight_des ##0.06269775
sfrfr_d1_avg_weight_end <- 146.1292/80
sfrfr_d1_avg_weight_end ##1.826615

##D2 unweighted sum
sfrfr_d2_sum_unweight <- integrate(sin_frfr_D2, 0, 80)
sfrfr_d2_sum_unweight ##6.844573

##D2 weighted prime: finding sum of various integrals (ages 0-18, 18-30, 30-80)
integrate(sin_frfr_D2, 0, 18) ##12.27829
integrate(sin_frfr_D2, 18, 30) ##-1.593237
-1.593237*2 ##weighing the prime (ages 18-30) by 2 = -3.186474
integrate(sin_frfr_D2, 30, 80) ##-3.84048
sfrfr_d2_sum_weight_prime <- 12.27829-3.186474-3.84048
sfrfr_d2_sum_weight_prime ##5.251336

##D2 ascending weights: finding sum of various integrals (using weights of 0.25, 0.5, 0.75, and 1)
integrate(sin_frfr_D2, 0, 20) ##13.22958
13.22958*0.25 ##3.307395
integrate(sin_frfr_D2, 20, 40) ##-11.22668
-11.22668*0.5 ##-5.61334
integrate(sin_frfr_D2, 40, 60) ##7.524122
7.524122*0.75 ##5.643092
integrate(sin_frfr_D2, 60, 80) ##-2.682444
-2.682444*1 ##-2.682444
sfrfr_d2_sum_weight_asc <- 3.307395-5.61334+5.643092-2.682444
sfrfr_d2_sum_weight_asc ##0.654703

##D2 descending weights: finding sum of various integrals (using weights of 1, 0.75, 0.5, and 0.25)
integrate(sin_frfr_D2, 0, 20) ##13.22958
13.22958*1 ##13.22958
integrate(sin_frfr_D2, 20, 40) ##-11.22668
-11.22668*0.75 ##-8.42001
integrate(sin_frfr_D2, 40, 60) ##7.524122
7.524122*0.5 ##3.762061
integrate(sin_frfr_D2, 60, 80) ##-2.682444
-2.682444*0.25 ##-0.670611
sfrfr_d2_sum_weight_des <- 13.22958-8.42001+3.762061-0.670611
sfrfr_d2_sum_weight_des ##7.90102

##D2 weighted end of life: finding sum of various integrals (ages 0-60, 60-80)
integrate(sin_frfr_D2, 0, 60) ##9.527017
integrate(sin_frfr_D2, 60, 80) ##-2.682444
-2.682444*2 ##weighing the end of life (ages 60-80) by 2 = -5.364888
sfrfr_d2_sum_weight_end <- 9.527017-5.364888
sfrfr_d2_sum_weight_end ##4.162129

##D2 unweighted and weighted averages (sum/80)
sfrfr_d2_avg_unweight <- 6.844573/80
sfrfr_d2_avg_unweight ##0.08555716
sfrfr_d2_avg_weight_prime <- 5.251336/80
sfrfr_d2_avg_weight_prime ##0.0656417
sfrfr_d2_avg_weight_asc <- 0.654703/80
sfrfr_d2_avg_weight_asc ##0.008183788
sfrfr_d2_avg_weight_des <- 7.90102/80
sfrfr_d2_avg_weight_des ##0.09876275
sfrfr_d2_avg_weight_end <- 4.162129/80
sfrfr_d2_avg_weight_end ##0.05202661

##manually generated minima, maxima, and total
##number_minima: 2
##number_maxima: 1
##number_extrema: 3

##21. Sinusoidal Rise, Fall, Rise, Fall (RFRF) ---------------------------------------------------------------------------------------------------------

sin_rfrf <- function(x) {50+50*(sin(x*0.1375))} ##function
sin_rfrf_plot <- plot(sin_rfrf, lwd = 3, xlim = c(0,80), ylim = c(0,100), main = "Sinusoidal Rise, Fall, Rise, Fall", 
                         xlab = "Age", ylab = "Meaning", col = "dodgerblue2", cex.lab = 1.5) ##plot function
lifelinesZ[21, ]

sin_rfrf_D1 <- D(50+50*(sin(x*0.1375)) ~ x) ##first derivative
sin_rfrf_D1
sin_rfrf_D1_plot <- plot(sin_rfrf_D1, lwd = 3, xlim = c(0,80), ylim = c(-10,10), 
                            main = "First Derivative of Sinusoidal RFRF", col = "dodgerblue2", cex.lab = 1.5)
sin_rfrf_D2 <- D(50+50*(sin(x*0.1375)) ~ x & x) ##second derivative
sin_rfrf_D2
sin_rfrf_D2_plot <- plot(sin_rfrf_D2, lwd = 3, xlim = c(0,80), ylim = c(-2,2), 
                            main = "Second Derivative of Sinusoidal RFRF", col = "dodgerblue2", cex.lab = 1.5)

sin_rfrf_area <- integrate(sin_rfrf, 0, 80) ##area
sin_rfrf_area ##4362.027

sin_rfrf_min <- optimize(sin_rfrf, interval = c(0,80)) ##minimum
sin_rfrf_min$objective ##0 
sin_rfrf_max <- optimize(sin_rfrf, interval = c(0,80), maximum = TRUE) ##maximum
sin_rfrf_max$objective ##100 

##D1 unweighted sum
srfrf_d1_sum_unweight <- integrate(sin_rfrf_D1, 0, 80)
srfrf_d1_sum_unweight ##-49.99951

##D1 weighted prime: finding sum of various integrals (ages 0-18, 18-30, 30-80)
integrate(sin_rfrf_D1, 0, 18) ##30.91558
integrate(sin_rfrf_D1, 18, 30) ##-72.53511
-72.53511*2 ##weighing the prime (ages 18-30) by 2 = -145.0702
integrate(sin_rfrf_D1, 30, 80) ##-8.379985
srfrf_d1_sum_weight_prime <- 30.91558-145.0702-8.379985
srfrf_d1_sum_weight_prime ##-122.5346

##D1 ascending weights: finding sum of various integrals (using weights of 0.25, 0.5, 0.75, and 1)
integrate(sin_rfrf_D1, 0, 20) ##19.08305
19.08305*0.25 ##4.770763
integrate(sin_rfrf_D1, 20, 40) ##-54.36007
-54.36007*0.5 ##-27.18004
integrate(sin_rfrf_D1, 40, 60) ##81.40723
81.40723*0.75 ##61.05542
integrate(sin_rfrf_D1, 60, 80) ##-96.12972
-96.12972*1 ##-96.12972
srfrf_d1_sum_weight_asc <- 4.770763-27.18004+61.05542-96.12972
srfrf_d1_sum_weight_asc ##-57.48358

##D1 descending weights: finding sum of various integrals (using weights of 1, 0.75, 0.5, and 0.25)
integrate(sin_rfrf_D1, 0, 20) ##19.08305
19.08305*1 ##19.08305
integrate(sin_rfrf_D1, 20, 40) ##-54.36007
-54.36007*0.75 ##-40.77005
integrate(sin_rfrf_D1, 40, 60) ##81.40723
81.40723*0.5 ##40.70361
integrate(sin_rfrf_D1, 60, 80) ##-96.12972
-96.12972*0.25 ##-24.03243
srfrf_d1_sum_weight_des <- 19.08305-40.77005+40.70361-24.03243
srfrf_d1_sum_weight_des ##-5.01582

##D1 weighted end of life: finding sum of various integrals (ages 0-60, 60-80)
integrate(sin_rfrf_D1, 0, 60) ##46.13021
integrate(sin_rfrf_D1, 60, 80) ##-96.12972
-96.12972*2 ##weighing the end of life (ages 60-80) by 2 = -192.2594
srfrf_d1_sum_weight_end <- 46.13021-192.2594
srfrf_d1_sum_weight_end ##-146.1292

##D1 unweighted and weighted averages (sum/80)
srfrf_d1_avg_unweight <- -49.99951/80
srfrf_d1_avg_unweight ##-0.6249939
srfrf_d1_avg_weight_prime <- -122.5346/80
srfrf_d1_avg_weight_prime ##-1.531683
srfrf_d1_avg_weight_asc <- -57.48358/80
srfrf_d1_avg_weight_asc ##-0.7185448
srfrf_d1_avg_weight_des <- -5.01582/80
srfrf_d1_avg_weight_des ##-0.06269775
srfrf_d1_avg_weight_end <- -146.1292/80
srfrf_d1_avg_weight_end ##-1.826615

##D2 unweighted sum
srfrf_d2_sum_unweight <- integrate(sin_rfrf_D2, 0, 80)
srfrf_d2_sum_unweight ##-6.844573

##D2 weighted prime: finding sum of various integrals (ages 0-18, 18-30, 30-80)
integrate(sin_rfrf_D2, 0, 18) ##-12.27829
integrate(sin_rfrf_D2, 18, 30) ##1.593237
1.593237*2 ##weighing the prime (ages 18-30) by 2 = 3.186474
integrate(sin_rfrf_D2, 30, 80) ##3.84048
srfrf_d2_sum_weight_prime <- -12.27829+3.186474+3.84048
srfrf_d2_sum_weight_prime ##-5.251336

##D2 ascending weights: finding sum of various integrals (using weights of 0.25, 0.5, 0.75, and 1)
integrate(sin_rfrf_D2, 0, 20) ##-13.22958
-13.22958*0.25 ##-3.307395
integrate(sin_rfrf_D2, 20, 40) ##11.22668
11.22668*0.5 ##5.61334
integrate(sin_rfrf_D2, 40, 60) ##-7.524122
-7.524122*0.75 ##-5.643092
integrate(sin_rfrf_D2, 60, 80) ##2.682444
2.682444*1 ##2.682444
srfrf_d2_sum_weight_asc <- -3.307395+5.61334-5.643092+2.682444
srfrf_d2_sum_weight_asc ##-0.654703

##D2 descending weights: finding sum of various integrals (using weights of 1, 0.75, 0.5, and 0.25)
integrate(sin_rfrf_D2, 0, 20) ##-13.22958
-13.22958*1 ##-13.22958
integrate(sin_rfrf_D2, 20, 40) ##11.22668
11.22668*0.75 ##8.42001
integrate(sin_rfrf_D2, 40, 60) ##-7.524122
-7.524122*0.5 ##-3.762061
integrate(sin_rfrf_D2, 60, 80) ##2.682444
2.682444*0.25 ##0.670611
srfrf_d2_sum_weight_des <- -13.22958+8.42001-3.762061+0.670611
srfrf_d2_sum_weight_des ##-7.90102

##D2 weighted end of life: finding sum of various integrals (ages 0-60, 60-80)
integrate(sin_rfrf_D2, 0, 60) ##-9.527017
integrate(sin_rfrf_D2, 60, 80) ##2.682444
2.682444*2 ##weighing the end of life (ages 60-80) by 2 = 5.364888
srfrf_d2_sum_weight_end <- -9.527017+5.364888
srfrf_d2_sum_weight_end ##-4.162129

##D2 unweighted and weighted averages (sum/80)
srfrf_d2_avg_unweight <- -6.844573/80
srfrf_d2_avg_unweight ##-0.08555716
srfrf_d2_avg_weight_prime <- -5.251336/80
srfrf_d2_avg_weight_prime ##-0.0656417
srfrf_d2_avg_weight_asc <- -0.654703/80
srfrf_d2_avg_weight_asc ##-0.008183788
srfrf_d2_avg_weight_des <- -7.90102/80
srfrf_d2_avg_weight_des ##-0.09876275
srfrf_d2_avg_weight_end <- -4.162129/80
srfrf_d2_avg_weight_end ##-0.05202661

##manually generated minima, maxima, and total
##number_minima: 1
##number_maxima: 2 
##number_extrema: 3

##Sudden Change at End of Life ---------------------------------------------------------------------------------------------------------

##22. Positive Change (Full) ---------------------------------------------------------------------------------------------------------

positive_change_full <- function(x) {
  (x < 60)*((-5/3)*(x-60)) +
    (60 <= x & x <= 80)*(100+5*(x-80))
  }
vectFunc <- Vectorize(positive_change_full)
plot(vectFunc,0,80, lwd = 3, xlim = c(0,80), ylim = c(0,100), main = "Positive Change (Full)", 
     xlab = "Age", ylab = "Meaning", col = "dodgerblue2", cex.lab = 1.5) ##plot function
lifelinesZ[22, ]

positive_change_full_1_D1 <- D((-5/3)*(x-60) ~ x) ##first derivative, part 1
positive_change_full_1_D1 ##rename as function
positive_change_full_1_D1_fxn <- function(x) {0*x-(5/3)}
positive_change_full_1_D1_fxn_plot <- plot(positive_change_full_1_D1_fxn,0,60, 
                                           lwd = 3, xlim = c(0,80), ylim = c(-10,10), 
                                           main = "First Derivative of Positive Change (Full)", 
                                           col = "dodgerblue2", cex.lab = 1.5)

positive_change_full_2_D1 <- D(100+5*(x-80) ~ x) ##first derivative, part 2
positive_change_full_2_D1 ##rename as function
positive_change_full_2_D1_fxn <- function(x) {0*x+5}
positive_change_full_2_D1_fxn_plot <- curve(positive_change_full_2_D1_fxn,60,80, add = TRUE, lwd = 3, 
                                           xlim = c(60,80), ylim = c(-10,10), 
                                           main = "First Derivative of Positive Change (Full)", 
                                           col = "dodgerblue2", cex.lab = 1.5)

positive_change_full_1_D2 <- D((-5/3)*(x-60) ~  x & x) ##second derivative, part 1
positive_change_full_1_D2 ##0; rename as function
positive_change_full_1_D2_fxn <- function(x) {0*x+0}
positive_change_full_1_D2_fxn_plot <- plot(positive_change_full_1_D2_fxn,0,60, lwd = 3, 
                                           xlim = c(0,80), ylim = c(-2,2), 
                                           main = "Second Derivative of Positive Change (Full)", 
                                           col = "dodgerblue2", cex.lab = 1.5)

positive_change_full_2_D2 <- D(100+5*(x-80) ~ x & x) ##second derivative, part 2
positive_change_full_2_D2 ##0; rename as function
positive_change_full_2_D2_fxn <- function(x) {0*x+0}
positive_change_full_2_D2_fxn_plot <- curve(positive_change_full_2_D2_fxn,60,80, add = TRUE, lwd = 3, 
                                           xlim = c(60,80), ylim = c(-2,2), 
                                           main = "Second Derivative of Positive Change (Full)", 
                                           col = "dodgerblue2", cex.lab = 1.5)

positive_change_full_area <- integrate(positive_change_full, 0, 80) ##area
positive_change_full_area ##4000

positive_change_full_min <- optimize(positive_change_full, interval = c(0,80)) ##minimum
positive_change_full_min$objective ##0 
positive_change_full_max <- optimize(positive_change_full, interval = c(0,80), maximum = TRUE) ##maximum
positive_change_full_max$objective ##100 

##D1 unweighted sums (ages 0-60 and 60-80)
integrate(positive_change_full_1_D1_fxn, 0, 60) ##-100
integrate(positive_change_full_2_D1_fxn, 60, 80) ##100
pc_full_d1_sum_unweight <- -100+100
pc_full_d1_sum_unweight ##0

##D1 weighted prime: finding sum of various integrals (ages 0-18, 18-30, 30-60, 60-80)
integrate(positive_change_full_1_D1_fxn, 0, 18) ##-30
integrate(positive_change_full_1_D1_fxn, 18, 30) ##-20
-20*2 ##weighing the prime (ages 18-30) by 2 = -40
integrate(positive_change_full_1_D1_fxn, 30, 60) ##-50
integrate(positive_change_full_2_D1_fxn, 60, 80) ##100
pc_full_d1_sum_weight_prime <- -30-40-50+100
pc_full_d1_sum_weight_prime ##-20

##D1 ascending weights: finding sum of various integrals (using weights of 0.25, 0.5, 0.75, and 1)
integrate(positive_change_full_1_D1_fxn, 0, 20) ##-33.33333
-33.33333*0.25 ##-8.333332
integrate(positive_change_full_1_D1_fxn, 20, 40) ##-33.33333
-33.33333*0.5 ##-16.66666
integrate(positive_change_full_1_D1_fxn, 40, 60) ##-33.33333
-33.33333*0.75 ##-25
integrate(positive_change_full_2_D1_fxn, 60, 80) ##100
100*1 ##100
pc_full_d1_sum_weight_asc <- -8.333332-16.66666-25+100
pc_full_d1_sum_weight_asc ##50

##D1 descending weights: finding sum of various integrals (using weights of 1, 0.75, 0.5, and 0.25)
integrate(positive_change_full_1_D1_fxn, 0, 20) ##-33.33333
-33.33333*1 ##-33.33333
integrate(positive_change_full_1_D1_fxn, 20, 40) ##-33.33333
-33.33333*0.75 ##-25
integrate(positive_change_full_1_D1_fxn, 40, 60) ##-33.33333
-33.33333*0.5 ##-16.66666
integrate(positive_change_full_2_D1_fxn, 60, 80) ##100
100*0.25 ##25
pc_full_d1_sum_weight_des <- -33.33333-25-16.66666+25
pc_full_d1_sum_weight_des ##-50

##D1 weighted end of life: finding sum of various integrals (ages 0-60, 60-80)
integrate(positive_change_full_1_D1_fxn, 0, 60) ##-100
integrate(positive_change_full_2_D1_fxn, 60, 80) ##100
100*2 ##weighing the end of life (ages 60-80) by 2 = 200
pc_full_d1_sum_weight_end <- -100+200
pc_full_d1_sum_weight_end ##100

##D1 unweighted and weighted averages (sum/80)
pc_full_d1_avg_unweight <- 0/80
pc_full_d1_avg_unweight ##0
pc_full_d1_avg_weight_prime <- -20/80
pc_full_d1_avg_weight_prime ##-0.25
pc_full_d1_avg_weight_asc <- 50/80
pc_full_d1_avg_weight_asc ##0.625
pc_full_d1_avg_weight_des <- -50/80
pc_full_d1_avg_weight_des ##-0.625
pc_full_d1_avg_weight_end <- 100/80
pc_full_d1_avg_weight_end ##1.25

##D2: because the second derivative is zero, all the values below will also be zero
pc_full_d2_sum_unweight <- 0
pc_full_d2_sum_weight_prime <- 0
pc_full_d2_sum_weight_asc <- 0
pc_full_d2_sum_weight_des <- 0
pc_full_d2_sum_weight_end <- 0
pc_full_d2_avg_unweight <- 0
pc_full_d2_avg_weight_prime <- 0
pc_full_d2_avg_weight_asc <- 0
pc_full_d2_avg_weight_des <- 0
pc_full_d2_avg_weight_end <- 0

##manually generated minima, maxima, and total
##number_minima: 1
##number_maxima: 0
##number_extrema: 1

##23. Positive Change (Partial) ---------------------------------------------------------------------------------------------------------

##Note: Would an end-of-life change that only reaches 50 (a "partial" change) 
##be as effective as one that reaches 100 (a "full" change)? To investigate this,
##we will be creating partial plots below.

positive_change_partial <- function(x) {
  (x < 60)*((-5/3)*(x-60)) +
    (60 <= x & x <= 80)*(50+2.5*(x-80))
}
vectFunc2 <- Vectorize(positive_change_partial)
plot(vectFunc2,0,80, lwd = 3, xlim = c(0,80), ylim = c(0,100), main = "Positive Change (Partial)", 
     xlab = "Age", ylab = "Meaning", col = "dodgerblue2", cex.lab = 1.5) ##plot function
lifelinesZ[23, ]

positive_change_partial_1_D1 <- D((-5/3)*(x-60) ~ x) ##first derivative, part 1
positive_change_partial_1_D1 ##rename as function
positive_change_partial_1_D1_fxn <- function(x) {0*x-(5/3)}
positive_change_partial_1_D1_fxn_plot <- plot(positive_change_partial_1_D1_fxn,0,60, 
                                           lwd = 3, xlim = c(0,80), ylim = c(-10,10), 
                                           main = "First Derivative of Positive Change (Partial)", 
                                           col = "dodgerblue2", cex.lab = 1.5)

positive_change_partial_2_D1 <- D(50+2.5*(x-80) ~ x) ##first derivative, part 2
positive_change_partial_2_D1 ##rename as function
positive_change_partial_2_D1_fxn <- function(x) {0*x+2.5}
positive_change_partial_2_D1_fxn_plot <- curve(positive_change_partial_2_D1_fxn,60,80, add = TRUE, lwd = 3, 
                                           xlim = c(60,80), ylim = c(-10,10), 
                                           main = "First Derivative of Positive Change (Partial)", 
                                           col = "dodgerblue2", cex.lab = 1.5)

positive_change_partial_1_D2 <- D((-5/3)*(x-60) ~  x & x) ##second derivative, part 1
positive_change_partial_1_D2 ##0; rename as function
positive_change_partial_1_D2_fxn <- function(x) {0*x+0}
positive_change_partial_1_D2_fxn_plot <- plot(positive_change_partial_1_D2_fxn,0,60, lwd = 3, 
                                           xlim = c(0,80), ylim = c(-2,2), 
                                           main = "Second Derivative of Positive Change (Partial)", 
                                           col = "dodgerblue2", cex.lab = 1.5)

positive_change_partial_2_D2 <- D(50+2.5*(x-80) ~ x & x) ##second derivative, part 2
positive_change_partial_2_D2 ##0; rename as function
positive_change_partial_2_D2_fxn <- function(x) {0*x+0}
positive_change_partial_2_D2_fxn_plot <- curve(positive_change_partial_2_D2_fxn,60,80, add = TRUE, lwd = 3, 
                                           xlim = c(60,80), ylim = c(-2,2), 
                                           main = "Second Derivative of Positive Change (Partial)", 
                                           col = "dodgerblue2", cex.lab = 1.5)

positive_change_partial_area <- integrate(positive_change_partial, 0, 80) ##area
positive_change_partial_area ##3500

positive_change_partial_min <- optimize(positive_change_partial, interval = c(0,80)) ##minimum
positive_change_partial_min$objective ##0 
positive_change_partial_max <- optimize(positive_change_partial, interval = c(0,80), maximum = TRUE) ##maximum
positive_change_partial_max$objective ##100 

##D1 unweighted sums (ages 0-60 and 60-80)
integrate(positive_change_partial_1_D1_fxn, 0, 60) ##-100
integrate(positive_change_partial_2_D1_fxn, 60, 80) ##50
pc_partial_d1_sum_unweight <- -100+50
pc_partial_d1_sum_unweight ##-50

##D1 weighted prime: finding sum of various integrals (ages 0-18, 18-30, 30-60, 60-80)
integrate(positive_change_partial_1_D1_fxn, 0, 18) ##-30
integrate(positive_change_partial_1_D1_fxn, 18, 30) ##-20
-20*2 ##weighing the prime (ages 18-30) by 2 = -40
integrate(positive_change_partial_1_D1_fxn, 30, 60) ##-50
integrate(positive_change_partial_2_D1_fxn, 60, 80) ##50
pc_partial_d1_sum_weight_prime <- -30-40-50+50
pc_partial_d1_sum_weight_prime ##-70

##D1 ascending weights: finding sum of various integrals (using weights of 0.25, 0.5, 0.75, and 1)
integrate(positive_change_partial_1_D1_fxn, 0, 20) ##-33.33333
-33.33333*0.25 ##-8.333332
integrate(positive_change_partial_1_D1_fxn, 20, 40) ##-33.33333
-33.33333*0.5 ##-16.66666
integrate(positive_change_partial_1_D1_fxn, 40, 60) ##-33.33333
-33.33333*0.75 ##-25
integrate(positive_change_partial_2_D1_fxn, 60, 80) ##50
50*1 ##50
pc_partial_d1_sum_weight_asc <- -8.333332-16.66666-25+50
pc_partial_d1_sum_weight_asc ##0

##D1 descending weights: finding sum of various integrals (using weights of 1, 0.75, 0.5, and 0.25)
integrate(positive_change_partial_1_D1_fxn, 0, 20) ##-33.33333
-33.33333*1 ##-33.33333
integrate(positive_change_partial_1_D1_fxn, 20, 40) ##-33.33333
-33.33333*0.75 ##-25
integrate(positive_change_partial_1_D1_fxn, 40, 60) ##-33.33333
-33.33333*0.5 ##-16.66666
integrate(positive_change_partial_2_D1_fxn, 60, 80) ##50
50*0.25 ##12.5
pc_partial_d1_sum_weight_des <- -33.33333-25-16.66666+12.5
pc_partial_d1_sum_weight_des ##-62.5

##D1 weighted end of life: finding sum of various integrals (ages 0-60, 60-80)
integrate(positive_change_partial_1_D1_fxn, 0, 60) ##-100
integrate(positive_change_partial_2_D1_fxn, 60, 80) ##50
50*2 ##weighing the end of life (ages 60-80) by 2 = 100
pc_partial_d1_sum_weight_end <- -100+100
pc_partial_d1_sum_weight_end ##0

##D1 unweighted and weighted averages (sum/80)
pc_partial_d1_avg_unweight <- -50/80
pc_partial_d1_avg_unweight ##-0.625
pc_partial_d1_avg_weight_prime <- -70/80
pc_partial_d1_avg_weight_prime ##-0.875
pc_partial_d1_avg_weight_asc <- 0/80
pc_partial_d1_avg_weight_asc ##0
pc_partial_d1_avg_weight_des <- -62.5/80
pc_partial_d1_avg_weight_des ##-0.78125
pc_partial_d1_avg_weight_end <- 0/80
pc_partial_d1_avg_weight_end ##0

##D2: because the second derivative is zero, all the values below will also be zero
pc_partial_d2_sum_unweight <- 0
pc_partial_d2_sum_weight_prime <- 0
pc_partial_d2_sum_weight_asc <- 0
pc_partial_d2_sum_weight_des <- 0
pc_partial_d2_sum_weight_end <- 0
pc_partial_d2_avg_unweight <- 0
pc_partial_d2_avg_weight_prime <- 0
pc_partial_d2_avg_weight_asc <- 0
pc_partial_d2_avg_weight_des <- 0
pc_partial_d2_avg_weight_end <- 0

##manually generated minima, maxima, and total
##number_minima: 1
##number_maxima: 0
##number_extrema: 1

##24. Negative Change (Full) ---------------------------------------------------------------------------------------------------------

negative_change_full <- function(x) {
  (x < 60)*(5/3)*x +
    (60 <= x & x <= 80)*(-5*(x-80))
}
vectFunc3 <- Vectorize(negative_change_full)
plot(vectFunc3,0,80, lwd = 3, xlim = c(0,80), ylim = c(0,100), main = "Negative Change (Full)", 
     xlab = "Age", ylab = "Meaning", col = "dodgerblue2", cex.lab = 1.5) ##plot function
lifelinesZ[24, ]

negative_change_full_1_D1 <- D((5/3)*x ~ x) ##first derivative, part 1
negative_change_full_1_D1 ##rename as function
negative_change_full_1_D1_fxn <- function(x) {0*x+(5/3)}
negative_change_full_1_D1_fxn_plot <- plot(negative_change_full_1_D1_fxn,0,60, 
                                           lwd = 3, xlim = c(0,80), ylim = c(-10,10), 
                                           main = "First Derivative of Negative Change (Full)", 
                                           col = "dodgerblue2", cex.lab = 1.5)

negative_change_full_2_D1 <- D(-5*(x-80) ~ x) ##first derivative, part 2
negative_change_full_2_D1 ##rename as function
negative_change_full_2_D1_fxn <- function(x) {0*x-5}
negative_change_full_2_D1_fxn_plot <- curve(negative_change_full_2_D1_fxn,60,80, add = TRUE, lwd = 3, 
                                           xlim = c(60,80), ylim = c(-10,10), 
                                           main = "First Derivative of Negative Change (Full)", 
                                           col = "dodgerblue2", cex.lab = 1.5)

negative_change_full_1_D2 <- D((5/3)*x ~  x & x) ##second derivative, part 1
negative_change_full_1_D2 ##0; rename as function
negative_change_full_1_D2_fxn <- function(x) {0*x+0}
negative_change_full_1_D2_fxn_plot <- plot(negative_change_full_1_D2_fxn,0,60, lwd = 3, 
                                           xlim = c(0,80), ylim = c(-2,2), 
                                           main = "Second Derivative of Negative Change (Full)", 
                                           col = "dodgerblue2", cex.lab = 1.5)

negative_change_full_2_D2 <- D(-5*(x-80) ~ x & x) ##second derivative, part 2
negative_change_full_2_D2 ##0; rename as function
negative_change_full_2_D2_fxn <- function(x) {0*x+0}
negative_change_full_2_D2_fxn_plot <- curve(negative_change_full_2_D2_fxn,60,80, add = TRUE, lwd = 3, 
                                           xlim = c(60,80), ylim = c(-2,2), 
                                           main = "Second Derivative of Negative Change (Full)", 
                                           col = "dodgerblue2", cex.lab = 1.5)

negative_change_full_area <- integrate(negative_change_full, 0, 80) ##area
negative_change_full_area ##4000

negative_change_full_min <- optimize(negative_change_full, interval = c(0,80)) ##minimum
negative_change_full_min$objective ##0 
negative_change_full_max <- optimize(negative_change_full, interval = c(0,80), maximum = TRUE) ##maximum
negative_change_full_max$objective ##100 

##D1 unweighted sums (ages 0-60 and 60-80)
integrate(negative_change_full_1_D1_fxn, 0, 60) ##100
integrate(negative_change_full_2_D1_fxn, 60, 80) ##-100
nc_full_d1_sum_unweight <- 100-100
nc_full_d1_sum_unweight ##0

##D1 weighted prime: finding sum of various integrals (ages 0-18, 18-30, 30-60, 60-80)
integrate(negative_change_full_1_D1_fxn, 0, 18) ##30
integrate(negative_change_full_1_D1_fxn, 18, 30) ##20
20*2 ##weighing the prime (ages 18-30) by 2 = 40
integrate(negative_change_full_1_D1_fxn, 30, 60) ##50
integrate(negative_change_full_2_D1_fxn, 60, 80) ##-100
nc_full_d1_sum_weight_prime <- 30+40+50-100
nc_full_d1_sum_weight_prime ##20

##D1 ascending weights: finding sum of various integrals (using weights of 0.25, 0.5, 0.75, and 1)
integrate(negative_change_full_1_D1_fxn, 0, 20) ##33.33333
33.33333*0.25 ##8.333332
integrate(negative_change_full_1_D1_fxn, 20, 40) ##33.33333
33.33333*0.5 ##16.66666
integrate(negative_change_full_1_D1_fxn, 40, 60) ##33.33333
33.33333*0.75 ##25
integrate(negative_change_full_2_D1_fxn, 60, 80) ##-100
-100*1 ##-100
nc_full_d1_sum_weight_asc <- 8.333332+16.66666+25-100
nc_full_d1_sum_weight_asc ##-50

##D1 descending weights: finding sum of various integrals (using weights of 1, 0.75, 0.5, and 0.25)
integrate(negative_change_full_1_D1_fxn, 0, 20) ##33.33333
33.33333*1 ##33.33333
integrate(negative_change_full_1_D1_fxn, 20, 40) ##33.33333
33.33333*0.75 ##25
integrate(negative_change_full_1_D1_fxn, 40, 60) ##33.33333
33.33333*0.5 ##16.66666
integrate(negative_change_full_2_D1_fxn, 60, 80) ##-100
-100*0.25 ##-25
nc_full_d1_sum_weight_des <- 33.33333+25+16.66666-25
nc_full_d1_sum_weight_des ##50

##D1 weighted end of life: finding sum of various integrals (ages 0-60, 60-80)
integrate(negative_change_full_1_D1_fxn, 0, 60) ##100
integrate(negative_change_full_2_D1_fxn, 60, 80) ##-100
-100*2 ##weighing the end of life (ages 60-80) by 2 = -200
nc_full_d1_sum_weight_end <- 100-200
nc_full_d1_sum_weight_end ##-100

##D1 unweighted and weighted averages (sum/80)
nc_full_d1_avg_unweight <- 0/80
nc_full_d1_avg_unweight ##0
nc_full_d1_avg_weight_prime <- 20/80
nc_full_d1_avg_weight_prime ##0.25
nc_full_d1_avg_weight_asc <- -50/80
nc_full_d1_avg_weight_asc ##-0.625
nc_full_d1_avg_weight_des <- 50/80
nc_full_d1_avg_weight_des ##0.625
nc_full_d1_avg_weight_end <- -100/80
nc_full_d1_avg_weight_end ##-1.25

##D2: because the second derivative is zero, all the values below will also be zero
nc_full_d2_sum_unweight <- 0
nc_full_d2_sum_weight_prime <- 0
nc_full_d2_sum_weight_asc <- 0
nc_full_d2_sum_weight_des <- 0
nc_full_d2_sum_weight_end <- 0
nc_full_d2_avg_unweight <- 0
nc_full_d2_avg_weight_prime <- 0
nc_full_d2_avg_weight_asc <- 0
nc_full_d2_avg_weight_des <- 0
nc_full_d2_avg_weight_end <- 0

##manually generated minima, maxima, and total
##number_minima: 0
##number_maxima: 1
##number_extrema: 1

##25. Negative Change (Partial) ---------------------------------------------------------------------------------------------------------

##See note in ##23 (Positive Change (Partial)).

negative_change_partial <- function(x) {
  (x < 60)*(5/3)*x +
    (60 <= x & x <= 80)*(-2.5*(x-100))
}
vectFunc4 <- Vectorize(negative_change_partial)
plot(vectFunc4,0,80, lwd = 3, xlim = c(0,80), ylim = c(0,100), main = "Negative Change (Partial)", 
     xlab = "Age", ylab = "Meaning", col = "dodgerblue2", cex.lab = 1.5) ##plot function
lifelinesZ[25, ]

negative_change_partial_1_D1 <- D((5/3)*x ~ x) ##first derivative, part 1
negative_change_partial_1_D1 ##rename as function
negative_change_partial_1_D1_fxn <- function(x) {0*x+(5/3)}
negative_change_partial_1_D1_fxn_plot <- plot(negative_change_partial_1_D1_fxn,0,60, 
                                           lwd = 3, xlim = c(0,80), ylim = c(-10,10), 
                                           main = "First Derivative of Negative Change (Partial)", 
                                           col = "dodgerblue2", cex.lab = 1.5)

negative_change_partial_2_D1 <- D(-2.5*(x-100) ~ x) ##first derivative, part 2
negative_change_partial_2_D1 ##rename as function
negative_change_partial_2_D1_fxn <- function(x) {0*x-2.5}
negative_change_partial_2_D1_fxn_plot <- curve(negative_change_partial_2_D1_fxn,60,80, add = TRUE, lwd = 3, 
                                           xlim = c(60,80), ylim = c(-10,10), 
                                           main = "First Derivative of Negative Change (Partial)", 
                                           col = "dodgerblue2", cex.lab = 1.5)

negative_change_partial_1_D2 <- D((5/3)*x ~  x & x) ##second derivative, part 1
negative_change_partial_1_D2 ##0; rename as function
negative_change_partial_1_D2_fxn <- function(x) {0*x+0}
negative_change_partial_1_D2_fxn_plot <- plot(negative_change_partial_1_D2_fxn,0,60, lwd = 3, 
                                           xlim = c(0,80), ylim = c(-2,2), 
                                           main = "Second Derivative of Negative Change (Partial)", 
                                           col = "dodgerblue2", cex.lab = 1.5)

negative_change_partial_2_D2 <- D(-2.5*(x-100) ~ x & x) ##second derivative, part 2
negative_change_partial_2_D2 ##0; rename as function
negative_change_partial_2_D2_fxn <- function(x) {0*x+0}
negative_change_partial_2_D2_fxn_plot <- curve(negative_change_partial_2_D2_fxn,60,80, add = TRUE, lwd = 3, 
                                           xlim = c(60,80), ylim = c(-2,2), 
                                           main = "Second Derivative of Negative Change (Partial)", 
                                           col = "dodgerblue2", cex.lab = 1.5)

negative_change_partial_area <- integrate(negative_change_partial, 0, 80) ##area
negative_change_partial_area ##4500

negative_change_partial_min <- optimize(negative_change_partial, interval = c(0,80)) ##minimum
negative_change_partial_min$objective ##0 
negative_change_partial_max <- optimize(negative_change_partial, interval = c(0,80), maximum = TRUE) ##maximum
negative_change_partial_max$objective ##100 

##D1 unweighted sums (ages 0-60 and 60-80)
integrate(negative_change_partial_1_D1_fxn, 0, 60) ##100
integrate(negative_change_partial_2_D1_fxn, 60, 80) ##-50
nc_partial_d1_sum_unweight <- 100-50
nc_partial_d1_sum_unweight ##50

##D1 weighted prime: finding sum of various integrals (ages 0-18, 18-30, 30-60, 60-80)
integrate(negative_change_partial_1_D1_fxn, 0, 18) ##30
integrate(negative_change_partial_1_D1_fxn, 18, 30) ##20
20*2 ##weighing the prime (ages 18-30) by 2 = 40
integrate(negative_change_partial_1_D1_fxn, 30, 60) ##50
integrate(negative_change_partial_2_D1_fxn, 60, 80) ##-50
nc_partial_d1_sum_weight_prime <- 30+40+50-50
nc_partial_d1_sum_weight_prime ##70

##D1 ascending weights: finding sum of various integrals (using weights of 0.25, 0.5, 0.75, and 1)
integrate(negative_change_partial_1_D1_fxn, 0, 20) ##33.33333
33.33333*0.25 ##8.333332
integrate(negative_change_partial_1_D1_fxn, 20, 40) ##33.33333
33.33333*0.5 ##16.66666
integrate(negative_change_partial_1_D1_fxn, 40, 60) ##33.33333
33.33333*0.75 ##25
integrate(negative_change_partial_2_D1_fxn, 60, 80) ##-50
-50*1 ##-50
nc_partial_d1_sum_weight_asc <- 8.333332+16.66666+25-50
nc_partial_d1_sum_weight_asc ##0

##D1 descending weights: finding sum of various integrals (using weights of 1, 0.75, 0.5, and 0.25)
integrate(negative_change_partial_1_D1_fxn, 0, 20) ##33.33333
33.33333*1 ##33.33333
integrate(negative_change_partial_1_D1_fxn, 20, 40) ##33.33333
33.33333*0.75 ##25
integrate(negative_change_partial_1_D1_fxn, 40, 60) ##33.33333
33.33333*0.5 ##16.66666
integrate(negative_change_partial_2_D1_fxn, 60, 80) ##-50
-50*0.25 ##-12.5
nc_partial_d1_sum_weight_des <- 33.33333+25+16.66666-12.5
nc_partial_d1_sum_weight_des ##62.5

##D1 weighted end of life: finding sum of various integrals (ages 0-60, 60-80)
integrate(negative_change_partial_1_D1_fxn, 0, 60) ##100
integrate(negative_change_partial_2_D1_fxn, 60, 80) ##-50
-50*2 ##weighing the end of life (ages 60-80) by 2 = -100
nc_partial_d1_sum_weight_end <- 100-100
nc_partial_d1_sum_weight_end ##0

##D1 unweighted and weighted averages (sum/80)
nc_partial_d1_avg_unweight <- 50/80
nc_partial_d1_avg_unweight ##0.625
nc_partial_d1_avg_weight_prime <- 70/80
nc_partial_d1_avg_weight_prime ##0.875
nc_partial_d1_avg_weight_asc <- 0/80
nc_partial_d1_avg_weight_asc ##0
nc_partial_d1_avg_weight_des <- 62.5/80
nc_partial_d1_avg_weight_des ##0.78125
nc_partial_d1_avg_weight_end <- 0/80
nc_partial_d1_avg_weight_end ##0

##D2: because the second derivative is zero, all the values below will also be zero
nc_partial_d2_sum_unweight <- 0
nc_partial_d2_sum_weight_prime <- 0
nc_partial_d2_sum_weight_asc <- 0
nc_partial_d2_sum_weight_des <- 0
nc_partial_d2_sum_weight_end <- 0
nc_partial_d2_avg_unweight <- 0
nc_partial_d2_avg_weight_prime <- 0
nc_partial_d2_avg_weight_asc <- 0
nc_partial_d2_avg_weight_des <- 0
nc_partial_d2_avg_weight_end <- 0

##manually generated minima, maxima, and total
##number_minima: 0
##number_maxima: 1
##number_extrema: 1

##26. Linear Rise, Sharp Fall (Old Testament) ---------------------------------------------------------------------------------------------------------

linear_rise_sharp_fall <- function(x) {
  (x < 40)*(1.25*x) +
    (40 < x & x <= 80)*(0*x+0)
}
vectFunc5 <- Vectorize(linear_rise_sharp_fall)
linear_rise_sharp_fall_plot <- plot(vectFunc5,0,80, lwd = 3, xlim = c(0,80), ylim = c(0,100), main = "Linear Rise, Sharp Fall", 
    xlab = "Age", ylab = "Meaning", col = "dodgerblue2", cex.lab = 1.5) ##plot function
mysubtitle = "(Old Testament)"
mtext(line = 0.5, mysubtitle)
lifelinesZ[26, ]

linear_rise_sharp_fall_1_D1 <- D(1.25*x ~ x) ##first derivative, part 1
linear_rise_sharp_fall_1_D1 ##rename as function
linear_rise_sharp_fall_1_D1_fxn <- function(x) {0*x+1.25}
linear_rise_sharp_fall_1_D1_fxn_plot <- plot(linear_rise_sharp_fall_1_D1_fxn,0,40, 
                                              lwd = 3, xlim = c(0,80), ylim = c(-10,10), 
                                              main = "First Derivative of Linear Rise, Sharp Fall", 
                                              col = "dodgerblue2", cex.lab = 1.5)
mysubtitle = "(Old Testament)"
mtext(line = 0.5, mysubtitle)

linear_rise_sharp_fall_2_D1 <- D(0*x+0 ~ x) ##first derivative, part 2
linear_rise_sharp_fall_2_D1 ##rename as function
linear_rise_sharp_fall_2_D1_fxn <- function(x) {0*x-0}
linear_rise_sharp_fall_2_D1_fxn_plot <- curve(linear_rise_sharp_fall_2_D1_fxn,40,80, add = TRUE, lwd = 3, 
                                               xlim = c(40,80), ylim = c(-10,10), 
                                               main = "First Derivative of Linear Rise, Sharp Fall", 
                                               col = "dodgerblue2", cex.lab = 1.5)

linear_rise_sharp_fall_1_D2 <- D(1.25*x ~  x & x) ##second derivative, part 1
linear_rise_sharp_fall_1_D2 ##0; rename as function
linear_rise_sharp_fall_1_D2_fxn <- function(x) {0*x+0}
linear_rise_sharp_fall_1_D2_fxn_plot <- plot(linear_rise_sharp_fall_1_D2_fxn,0,40, lwd = 3, 
                                              xlim = c(0,80), ylim = c(-2,2), 
                                              main = "Second Derivative of Linear Rise, Sharp Fall", 
                                              col = "dodgerblue2", cex.lab = 1.5)
mysubtitle = "(Old Testament)"
mtext(line = 0.5, mysubtitle)

linear_rise_sharp_fall_2_D2 <- D(0*x+0 ~ x & x) ##second derivative, part 2
linear_rise_sharp_fall_2_D2 ##0; rename as function
linear_rise_sharp_fall_2_D2_fxn <- function(x) {0*x+0}
linear_rise_sharp_fall_2_D2_fxn_plot <- curve(linear_rise_sharp_fall_2_D2_fxn,40,80, add = TRUE, lwd = 3, 
                                               xlim = c(40,80), ylim = c(-2,2), 
                                               main = "Second Derivative of Linear Rise, Sharp Fall", 
                                               col = "dodgerblue2", cex.lab = 1.5)

linear_rise_sharp_fall_area <- integrate(linear_rise_sharp_fall, 0, 80) ##area
linear_rise_sharp_fall_area ##1000

linear_rise_sharp_fall_min <- optimize(linear_rise_sharp_fall, interval = c(0,80)) ##minimum
linear_rise_sharp_fall_min$objective ##0 
linear_rise_sharp_fall_max <- optimize(linear_rise_sharp_fall, interval = c(0,80), maximum = TRUE) ##maximum
linear_rise_sharp_fall_max$objective ##50

##D1 unweighted sums (ages 0-40 and 40-80)
integrate(linear_rise_sharp_fall_1_D1_fxn, 0, 40) ##50
integrate(linear_rise_sharp_fall_2_D1_fxn, 40, 80) ##0
lrsf_d1_sum_unweight <- 50+0
lrsf_d1_sum_unweight ##50

##D1 weighted prime: finding sum of various integrals (ages 0-18, 18-30, 30-40, 40-80)
integrate(linear_rise_sharp_fall_1_D1_fxn, 0, 18) ##22.5
integrate(linear_rise_sharp_fall_1_D1_fxn, 18, 30) ##15
15*2 ##weighing the prime (ages 18-30) by 2 = 30
integrate(linear_rise_sharp_fall_1_D1_fxn, 30, 40) ##12.5
integrate(linear_rise_sharp_fall_2_D1_fxn, 40, 80) ##0
lrsf_d1_sum_weight_prime <- 22.5+30+12.5+0
lrsf_d1_sum_weight_prime ##65

##D1 ascending weights: finding sum of various integrals (using weights of 0.25, 0.5, 0.75, and 1)
integrate(linear_rise_sharp_fall_1_D1_fxn, 0, 20) ##25
25*0.25 ##6.25
integrate(linear_rise_sharp_fall_1_D1_fxn, 20, 40) ##25
25*0.5 ##12.5
integrate(linear_rise_sharp_fall_2_D1_fxn, 40, 60) ##0
0*0.75 ##0
integrate(linear_rise_sharp_fall_2_D1_fxn, 60, 80) ##0
0*1 ##0
lrsf_d1_sum_weight_asc <- 6.25+12.5+0+0
lrsf_d1_sum_weight_asc ##18.75

##D1 descending weights: finding sum of various integrals (using weights of 1, 0.75, 0.5, and 0.25)
integrate(linear_rise_sharp_fall_1_D1_fxn, 0, 20) ##25
25*1 ##25
integrate(linear_rise_sharp_fall_1_D1_fxn, 20, 40) ##25
25*0.75 ##18.75
integrate(linear_rise_sharp_fall_2_D1_fxn, 40, 60) ##0
0*0.5 ##0
integrate(linear_rise_sharp_fall_2_D1_fxn, 60, 80) ##0
0*0.25 ##0
lrsf_d1_sum_weight_des <- 25+18.75+0+0
lrsf_d1_sum_weight_des ##43.75

##D1 weighted end of life: finding sum of various integrals (ages 0-40, 40-60, 60-80)
integrate(linear_rise_sharp_fall_1_D1_fxn, 0, 40) ##50
integrate(linear_rise_sharp_fall_2_D1_fxn, 40, 60) ##0
integrate(linear_rise_sharp_fall_2_D1_fxn, 60, 80) ##0
0*2 ##weighing the end of life (ages 60-80) by 2 = 0
lrsf_d1_sum_weight_end <- 50+0+0+0
lrsf_d1_sum_weight_end ##50

##D1 unweighted and weighted averages (sum/80)
lrsf_d1_avg_unweight <- 50/80
lrsf_d1_avg_unweight ##0.625
lrsf_d1_avg_weight_prime <- 65/80
lrsf_d1_avg_weight_prime ##0.8125
lrsf_d1_avg_weight_asc <- 18.75/80
lrsf_d1_avg_weight_asc ##0.234375
lrsf_d1_avg_weight_des <- 43.75/80
lrsf_d1_avg_weight_des ##0.546875
lrsf_d1_avg_weight_end <- 50/80
lrsf_d1_avg_weight_end ##0.625

##D2: because the second derivative is zero, all the values below will also be zero
lrsf_d2_sum_unweight <- 0
lrsf_d2_sum_weight_prime <- 0
lrsf_d2_sum_weight_asc <- 0
lrsf_d2_sum_weight_des <- 0
lrsf_d2_sum_weight_end <- 0
lrsf_d2_avg_unweight <- 0
lrsf_d2_avg_weight_prime <- 0
lrsf_d2_avg_weight_asc <- 0
lrsf_d2_avg_weight_des <- 0
lrsf_d2_avg_weight_end <- 0

##manually generated minima, maxima, and total
##number_minima: 0
##number_maxima: 1
##number_extrema: 1

##27. Linear Rise, Sharp Fall, Exponential Rise (New Testament/Cinderella) ---------------------------------------------------------------------------------------------------------

linear_rise_sharp_fall_exp_rise <- function(x) {
  (x < 40)*(1.25*x) +
    (40 < x & x <= 80)*(1.0606^x-10)
}
vectFunc6 <- Vectorize(linear_rise_sharp_fall_exp_rise)
linear_rise_sharp_fall_exp_rise_plot <- plot(vectFunc6,0,80, lwd = 3, xlim = c(0,80), ylim = c(0,100), main = "Linear Rise, Sharp Fall, Exponential Rise", 
     xlab = "Age", ylab = "Meaning", col = "dodgerblue2", cex.lab = 1.5) ##plot function
mysubtitle = "(New Testament/Cinderella)"
mtext(line = 0.5, mysubtitle)
lifelinesZ[27, ]

linear_rise_sharp_fall_exp_rise_1_D1 <- D(1.25*x ~ x) ##first derivative, part 1
linear_rise_sharp_fall_exp_rise_1_D1 ##rename as function
linear_rise_sharp_fall_exp_rise_1_D1_fxn <- function(x) {0*x+1.25}
linear_rise_sharp_fall_exp_rise_1_D1_fxn_plot <- plot(linear_rise_sharp_fall_exp_rise_1_D1_fxn,0,40, 
                                             lwd = 3, xlim = c(0,80), ylim = c(-10,10), 
                                             main = "First Derivative of Linear Rise, Sharp Fall, Exponential Rise", 
                                             col = "dodgerblue2", cex.lab = 1.5)
mysubtitle = "(New Testament/Cinderella)"
mtext(line = 0.5, mysubtitle)

linear_rise_sharp_fall_exp_rise_2_D1 <- D(1.0606^x-10 ~ x) ##first derivative, part 2
linear_rise_sharp_fall_exp_rise_2_D1
linear_rise_sharp_fall_exp_rise_2_D1_fxn_plot <- curve(linear_rise_sharp_fall_exp_rise_2_D1,40,80, add = TRUE, lwd = 3, 
                                              xlim = c(40,80), ylim = c(-10,10), 
                                              main = "First Derivative of Linear Rise, Sharp Fall, Exponential Rise", 
                                              col = "dodgerblue2", cex.lab = 1.5)

linear_rise_sharp_fall_exp_rise_1_D2 <- D(1.25*x ~  x & x) ##second derivative, part 1
linear_rise_sharp_fall_exp_rise_1_D2 ##0; rename as function
linear_rise_sharp_fall_exp_rise_1_D2_fxn <- function(x) {0*x+0}
linear_rise_sharp_fall_exp_rise_1_D2_fxn_plot <- plot(linear_rise_sharp_fall_exp_rise_1_D2_fxn,0,40, lwd = 3, 
                                             xlim = c(0,80), ylim = c(-2,2), 
                                             main = "Second Derivative of Linear Rise, Sharp Fall, Exponential Rise", 
                                             col = "dodgerblue2", cex.lab = 1.5)
mysubtitle = "(New Testament/Cinderella)"
mtext(line = 0.5, mysubtitle)

linear_rise_sharp_fall_exp_rise_2_D2 <- D(1.0606^x-10 ~ x & x) ##second derivative, part 2
linear_rise_sharp_fall_exp_rise_2_D2
linear_rise_sharp_fall_exp_rise_2_D2_plot <- curve(linear_rise_sharp_fall_exp_rise_2_D2,40,80, add = TRUE, lwd = 3, 
                                              xlim = c(40,80), ylim = c(-2,2), 
                                              main = "Second Derivative of Linear Rise, Sharp Fall, Exponential Rise", 
                                              col = "dodgerblue2", cex.lab = 1.5)

linear_rise_sharp_fall_exp_rise_area <- integrate(linear_rise_sharp_fall_exp_rise, 0, 80) ##area
linear_rise_sharp_fall_exp_rise_area ##2302.637

linear_rise_sharp_fall_exp_rise_min <- optimize(linear_rise_sharp_fall_exp_rise, interval = c(0,80)) ##minimum
linear_rise_sharp_fall_exp_rise_min$objective ##0 
linear_rise_sharp_fall_exp_rise_max <- optimize(linear_rise_sharp_fall_exp_rise, interval = c(0,80), maximum = TRUE) ##maximum
linear_rise_sharp_fall_exp_rise_max$objective ##100

##D1 unweighted sums (ages 0-40 and 40-80)
integrate(linear_rise_sharp_fall_exp_rise_1_D1_fxn, 0, 40) ##50
integrate(linear_rise_sharp_fall_exp_rise_2_D1, 40, 80) ##100.1743
lrsfer_d1_sum_unweight <- 50+100.1743
lrsfer_d1_sum_unweight ##150.1743

##D1 weighted prime: finding sum of various integrals (ages 0-18, 18-30, 30-40, 40-80)
integrate(linear_rise_sharp_fall_exp_rise_1_D1_fxn, 0, 18) ##22.5
integrate(linear_rise_sharp_fall_exp_rise_1_D1_fxn, 18, 30) ##15
15*2 ##weighing the prime (ages 18-30) by 2 = 30
integrate(linear_rise_sharp_fall_exp_rise_1_D1_fxn, 30, 40) ##12.5
integrate(linear_rise_sharp_fall_exp_rise_2_D1, 40, 80) ##100.1743
lrsfer_d1_sum_weight_prime <- 22.5+30+12.5+100.1743
lrsfer_d1_sum_weight_prime ##165.1743

##D1 ascending weights: finding sum of various integrals (using weights of 0.25, 0.5, 0.75, and 1)
integrate(linear_rise_sharp_fall_exp_rise_1_D1_fxn, 0, 20) ##25
25*0.25 ##6.25
integrate(linear_rise_sharp_fall_exp_rise_1_D1_fxn, 20, 40) ##25
25*0.5 ##12.5
integrate(linear_rise_sharp_fall_exp_rise_2_D1, 40, 60) ##23.60575
23.60575*0.75 ##17.70431
integrate(linear_rise_sharp_fall_exp_rise_2_D1, 60, 80) ##76.56852
76.56852*1 ##76.56852
lrsfer_d1_sum_weight_asc <- 6.25+12.5+17.70431+76.56852
lrsfer_d1_sum_weight_asc ##113.0228

##D1 descending weights: finding sum of various integrals (using weights of 1, 0.75, 0.5, and 0.25)
integrate(linear_rise_sharp_fall_exp_rise_1_D1_fxn, 0, 20) ##25
25*1 ##25
integrate(linear_rise_sharp_fall_exp_rise_1_D1_fxn, 20, 40) ##25
25*0.75 ##18.75
integrate(linear_rise_sharp_fall_exp_rise_2_D1, 40, 60) ##23.60575
23.60575*0.5 ##11.80288
integrate(linear_rise_sharp_fall_exp_rise_2_D1, 60, 80) ##76.56852
76.56852*0.25 ##19.14213
lrsfer_d1_sum_weight_des <- 25+18.75+11.80288+19.14213
lrsfer_d1_sum_weight_des ##74.69501

##D1 weighted end of life: finding sum of various integrals (ages 0-40, 40-60, 60-80)
integrate(linear_rise_sharp_fall_exp_rise_1_D1_fxn, 0, 40) ##50
integrate(linear_rise_sharp_fall_exp_rise_2_D1, 40, 60) ##23.60575
integrate(linear_rise_sharp_fall_exp_rise_2_D1, 60, 80) ##76.56852
76.56852*2 ##weighing the end of life (ages 60-80) by 2 = 153.137
lrsfer_d1_sum_weight_end <- 50+23.60575+153.137
lrsfer_d1_sum_weight_end ##226.7428

##D1 unweighted and weighted averages (sum/80)
lrsfer_d1_avg_unweight <- 150.1743/80
lrsfer_d1_avg_unweight ##1.877179
lrsfer_d1_avg_weight_prime <- 165.1743/80
lrsfer_d1_avg_weight_prime ##2.064679
lrsfer_d1_avg_weight_asc <- 113.0228/80
lrsfer_d1_avg_weight_asc ##1.412785
lrsfer_d1_avg_weight_des <- 74.69501/80
lrsfer_d1_avg_weight_des ##0.9336876
lrsfer_d1_avg_weight_end <- 226.7428/80
lrsfer_d1_avg_weight_end ##2.834285

##D2 unweighted sums (ages 0-40 and 40-80)
integrate(linear_rise_sharp_fall_exp_rise_1_D2_fxn, 0, 40) ##0
integrate(linear_rise_sharp_fall_exp_rise_2_D2, 40, 80) ##5.893732
lrsfer_d2_sum_unweight <- 0+5.893732
lrsfer_d2_sum_unweight ##5.893732

##D2 weighted prime: finding sum of various integrals (ages 0-18, 18-30, 30-40, 40-80)
integrate(linear_rise_sharp_fall_exp_rise_1_D2_fxn, 0, 18) ##0
integrate(linear_rise_sharp_fall_exp_rise_1_D2_fxn, 18, 30) ##0
0*2 ##weighing the prime (ages 18-30) by 2 = 0
integrate(linear_rise_sharp_fall_exp_rise_1_D2_fxn, 30, 40) ##0
integrate(linear_rise_sharp_fall_exp_rise_2_D2, 40, 80) ##5.893732
lrsfer_d2_sum_weight_prime <- 0+0+0+5.893732
lrsfer_d2_sum_weight_prime ##5.893732

##D2 ascending weights: finding sum of various integrals (using weights of 0.25, 0.5, 0.75, and 1)
integrate(linear_rise_sharp_fall_exp_rise_1_D2_fxn, 0, 20) ##0
0*0.25 ##0
integrate(linear_rise_sharp_fall_exp_rise_1_D2_fxn, 20, 40) ##0
0*0.5 ##0
integrate(linear_rise_sharp_fall_exp_rise_2_D2, 40, 60) ##1.388839
1.388839*0.75 ##1.041629
integrate(linear_rise_sharp_fall_exp_rise_2_D2, 60, 80) ##4.504893
4.504893*1 ##4.504893
lrsfer_d2_sum_weight_asc <- 0+0+1.041629+4.504893
lrsfer_d2_sum_weight_asc ##5.546522

##D2 descending weights: finding sum of various integrals (using weights of 1, 0.75, 0.5, and 0.25)
integrate(linear_rise_sharp_fall_exp_rise_1_D2_fxn, 0, 20) ##0
0*1 ##0
integrate(linear_rise_sharp_fall_exp_rise_1_D2_fxn, 20, 40) ##0
0*0.75 ##0
integrate(linear_rise_sharp_fall_exp_rise_2_D2, 40, 60) ##1.388839
1.388839*0.5 ##0.6944195
integrate(linear_rise_sharp_fall_exp_rise_2_D2, 60, 80) ##4.504893
4.504893*0.25 ##1.126223
lrsfer_d2_sum_weight_des <- 0+0+0.6944195+1.126223
lrsfer_d2_sum_weight_des ##1.820642

##D2 weighted end of life: finding sum of various integrals (ages 0-40, 40-60, 60-80)
integrate(linear_rise_sharp_fall_exp_rise_1_D2_fxn, 0, 40) ##0
integrate(linear_rise_sharp_fall_exp_rise_2_D2, 40, 60) ##1.388839
integrate(linear_rise_sharp_fall_exp_rise_2_D2, 60, 80) ##4.504893
4.504893*2 ##weighing the end of life (ages 60-80) by 2 = 9.009786
lrsfer_d2_sum_weight_end <- 0+1.388839+9.009786
lrsfer_d2_sum_weight_end ##10.39862

##D2 unweighted and weighted averages (sum/80)
lrsfer_d2_avg_unweight <- 5.893732/80
lrsfer_d2_avg_unweight ##0.07367165
lrsfer_d2_avg_weight_prime <- 5.893732/80
lrsfer_d2_avg_weight_prime ##0.07367165
lrsfer_d2_avg_weight_asc <- 5.546522/80
lrsfer_d2_avg_weight_asc ##0.06933153
lrsfer_d2_avg_weight_des <- 1.820642/80
lrsfer_d2_avg_weight_des ##0.02275803
lrsfer_d2_avg_weight_end <- 10.39862/80
lrsfer_d2_avg_weight_end ##0.1299827

##manually generated minima, maxima, and total
##number_minima: 0
##number_maxima: 1
##number_extrema: 1

## ---------------------------------------------------------------------------------------------------------
##POTENTIAL SEPARATE STUDY ---------------------------------------------------------------------------------
## ---------------------------------------------------------------------------------------------------------

##Early Cutoff (NOT IN 'LIFELINESZ' array) -------------------------------------------------------------

##Early Cutoff array ('early_cutoffs')

name2 <- c("Linear Cut (Rise)", "Linear Cut (Fall)", 
          "Exponential Cut (Rise)", "Exponential Cut (Fall)")
integral2 <- c(250, 1750, 17.6665, 1187.85)
min2 <- c(0, 75, 0, 31.06709)
max2 <- c(25, 100, 2.177001, 100)

d1_sum_unweight2 <- c(25, -25, 2.177015, -69.81022)
d1_sum_weight_prime2 <- c(27.5, -27.5, 2.523828, -73.73971)
d1_sum_weight_asc2 <- c(6.25, -6.25, 0.5442537, -17.45256)
d1_sum_weight_des2 <- c(25, -25, 2.177015, -69.81022)

d1_avg_unweight2 <- c(1.25, -1.25, 0.1088507, -3.490511)
d1_avg_weight_prime2 <- c(1.375, -1.375, 0.1261914, -3.686986)
d1_avg_weight_asc2 <- c(0.3125, -0.3125, 0.02721268, -0.872628)
d1_avg_weight_des2 <- c(1.25, -1.25, 0.1088507, -3.490511)

d2_sum_unweight2 <- c(0, 0, 0.1258251, 4.034828)
d2_sum_weight_prime2 <-c(0, 0, 0.1458699, 4.261941)
d2_sum_weight_asc2 <- c(0, 0, 0.03145627, 1.008707)
d2_sum_weight_des2 <- c(0, 0, 0.1258251, 4.034828)

d2_avg_unweight2 <- c(0, 0, 0.006291255, 0.2017414)
d2_avg_weight_prime2 <- c(0, 0, 0.007293495, 0.2130971)
d2_avg_weight_asc2 <- c(0, 0, 0.001572814, 0.05043535)                 
d2_avg_weight_des2 <- c(0, 0, 0.006291255, 0.2017414)

number_valleys2 <- c(0, 0, 0, 0) 
number_peaks2 <- c(0, 0, 0, 0) 
number_extrema2 <- c(0, 0, 0, 0)                 

early_cutoffs <- data.frame(name2, max2, min2, number_peaks2, number_valleys2, number_extrema2, integral2, 
                            d1_sum_unweight2, d1_sum_weight_prime2, 
                            d1_sum_weight_asc2, d1_sum_weight_des2, d1_avg_unweight2, d1_avg_weight_prime2,
                            d1_avg_weight_asc2, d1_avg_weight_des2, d2_sum_unweight2, d2_sum_weight_prime2, 
                            d2_sum_weight_asc2, d2_sum_weight_des2, d2_avg_unweight2, d2_avg_weight_prime2, 
                            d2_avg_weight_asc2, d2_avg_weight_des2, stringsAsFactors = FALSE)
names(early_cutoffs) <- c("Name", "Maximum", "Minimum", "Number of Peaks", "Number of Valleys", 
                          "Total Number of Peaks and Valleys", "Integral", "Unweighted Sum (1st Derivative)", 
                          "'Prime' (18-30) Weighted Sum (1st Derivative)", "Ascending Weighted Sum (1st Derivative)", 
                          "Descending Weighted Sum (1st Derivative)", "Unweighted Average (1st Derivative)", 
                          "'Prime' (18-30) Weighted Average (1st Derivative)", "Ascending Weighted Average (1st Derivative)", 
                          "Descending Weighted Average (1st Derivative)", "Unweighted Sum (2nd Derivative)", 
                          "'Prime' (18-30) Weighted Sum (2nd Derivative)", "Ascending Weighted Sum (2nd Derivative)", 
                          "Descending Weighted Sum (2nd Derivative)", "Unweighted Average (2nd Derivative)", 
                          "'Prime' (18-30) Weighted Average (2nd Derivative)", "Ascending Weighted Average (2nd Derivative)", 
                          "Descending Weighted Average (2nd Derivative)")

##Standardize the difficult-to-interpret values of the unweighted and weighted sums and averages in the 'early_cutoffs' array
##into z-scores and create a new, standardized array called 'early_cutoffsZ'.

early_cutoffsN <- early_cutoffs[7:23] ##getting the columns 7-23 from the 'lifelines' array
df2 <- (early_cutoffsN-rowMeans(early_cutoffsN))/(rowSds(as.matrix(early_cutoffsN)))[row(early_cutoffsN)] ##calculating Z score
sub_early_cutoffs <- data.frame(name, max, min, number_peaks, number_valleys, number_extrema, 
                            stringsAsFactors = FALSE) ##creating a new array from columns 1-6 from 'early_cutoffs'
names(sub_early_cutoffs) <- c("Name", "Maximum", "Minimum", "Number of Peaks", "Number of Valleys", 
                          "Total Number of Peaks and Valleys") ##naming the columns of 'sub_early_cutoffs'
df2 <- cbind(df2, sub_early_cutoffs) ##merging the two arrays: 'df2' and 'sub_early_cutoffs'
column_order <- c("Name", "Maximum", "Minimum", "Number of Peaks", "Number of Valleys", 
               "Total Number of Peaks and Valleys", "Integral", "Unweighted Sum (1st Derivative)", 
               "'Prime' (18-30) Weighted Sum (1st Derivative)", "Ascending Weighted Sum (1st Derivative)", 
               "Descending Weighted Sum (1st Derivative)", "Unweighted Average (1st Derivative)", 
               "'Prime' (18-30) Weighted Average (1st Derivative)", "Ascending Weighted Average (1st Derivative)", 
               "Descending Weighted Average (1st Derivative)", "Unweighted Sum (2nd Derivative)", 
               "'Prime' (18-30) Weighted Sum (2nd Derivative)", "Ascending Weighted Sum (2nd Derivative)", 
               "Descending Weighted Sum (2nd Derivative)", "Unweighted Average (2nd Derivative)", 
               "'Prime' (18-30) Weighted Average (2nd Derivative)", "Ascending Weighted Average (2nd Derivative)", 
               "Descending Weighted Average (2nd Derivative)") ##ordering the columns in preparation for the final array
early_cutoffsZ <- df2[, column_order] ##creating the final array, 'early_cutoffsZ'
View(early_cutoffsZ)
write.csv(early_cutoffsZ, 'early_cutoffsZ.csv')

##28. Linear Cut (Rise) ---------------------------------------------------------------------------------------------------------

linear_cut_rise <- function(x) {1.25*x}
linear_cut_rise_plot <- ggplot(data.frame(x = c(0, 20)), aes(x)) +
  stat_function(fun = function(x){1.25*x}, color="dodgerblue2", lwd = 1.25, xlim=c(0,20)) + 
  coord_cartesian(ylim=c(0,100))+
  scale_x_continuous(limits = c(0, 80))+
  ggtitle("Linear Cut (Rise)") + xlab("Age") + ylab("Meaning") + 
  theme_classic()  ##plot function
linear_cut_rise_plot

linear_cut_rise_D1 <- D(1.25*x ~ x) ##first derivative/slope
linear_cut_rise_D1 ##rewrite as function
linear_cut_rise_D1_fxn <- function(x) {0*x+1.25}
linear_cut_rise_D1_plot <- plot(linear_cut_rise_D1_fxn,0,20, lwd = 3, xlim = c(0,80), ylim = c(-10,10), 
                                main = "First Derivative of Linear Cut (Rise)", col = "dodgerblue2", cex.lab = 1.5) ##plot of first derivative

linear_cut_rise_D2 <- D(1.25*x ~ x & x) ##second derivative
linear_cut_rise_D2 ##rewrite as function
linear_cut_rise_D2_fxn <- function(x) {0*x+0}
linear_cut_rise_D2_plot <- plot(linear_cut_rise_D2_fxn,0,20, lwd = 3, xlim = c(0,80), ylim = c(-2,2), 
                                main = "Second Derivative of Linear Cut (Rise)", col = "dodgerblue2", cex.lab = 1.5)

linear_cut_rise_area <- integrate(linear_cut_rise, 0, 20) ##area
linear_cut_rise_area ##250

linear_cut_rise_min <- optimize(linear_cut_rise, interval = c(0,20)) ##minimum
linear_cut_rise_min$objective ##0 
linear_cut_rise_max <- optimize(linear_cut_rise, interval = c(0,20), maximum = TRUE) ##maximum
linear_cut_rise_max$objective ##25

##D1 unweighted sum
lcr_d1_sum_unweight <- integrate(linear_cut_rise_D1_fxn, 0, 20)
lcr_d1_sum_unweight ##25

##D1 weighted "prime": finding sum of various integrals (ages 0-18, 18-20)
integrate(linear_cut_rise_D1_fxn, 0, 18) ##22.5
integrate(linear_cut_rise_D1_fxn, 18, 20) ##2.5
2.5*2 ##weighing the prime (ages 18-20) by 2 = 5
lcr_d1_sum_weight_prime <- 22.5+5
lcr_d1_sum_weight_prime ##27.5

##D1 "ascending" weights: finding sum of (0,20) integral using weight of 0.25)
integrate(linear_cut_rise_D1_fxn, 0, 20) ##25
25*0.25 ##6.25
lcr_d1_sum_weight_asc <- 6.25
lcr_d1_sum_weight_asc ##6.25

##D1 "descending" weights: finding sum of (0,20) integral using weight of 1)
integrate(linear_cut_rise_D1_fxn, 0, 20) ##25
25*1 ##25
lcr_d1_sum_weight_des <- 25
lcr_d1_sum_weight_des ##25

##D1 unweighted and weighted averages (sum/20)
lcr_d1_avg_unweight <- 25/20
lcr_d1_avg_unweight ##1.25
lcr_d1_avg_weight_prime <- 27.5/20
lcr_d1_avg_weight_prime ##1.375
lcr_d1_avg_weight_asc <- 6.25/20
lcr_d1_avg_weight_asc ##0.3125
lcr_d1_avg_weight_des <- 25/20
lcr_d1_avg_weight_des ##1.25

##D2: because the second derivative is zero, all the values below will also be zero
lcr_d2_sum_unweight <- 0
lcr_d2_sum_weight_prime <- 0
lcr_d2_sum_weight_asc <- 0
lcr_d2_sum_weight_des <- 0
lcr_d2_avg_unweight <- 0
lcr_d2_avg_weight_prime <- 0
lcr_d2_avg_weight_asc <- 0
lcr_d2_avg_weight_des <- 0

##manually generated minima, maxima, and total
##number_minima: 0
##number_maxima: 0
##number_extrema: 0

##29. Linear Cut (Fall) ---------------------------------------------------------------------------------------------------------

linear_cut_fall <- function(x) {100-1.25*x}
linear_cut_fall_plot <- ggplot(data.frame(x = c(0, 20)), aes(x)) +
  stat_function(fun = function(x){100-1.25*x}, color="dodgerblue2", lwd = 1.25, xlim=c(0,20)) + 
  coord_cartesian(ylim=c(0,100))+
  scale_x_continuous(limits = c(0, 80))+
  ggtitle("Linear Cut (Fall)") + xlab("Age") + ylab("Meaning") + 
  theme_classic()  ##plot function
linear_cut_fall_plot

linear_cut_fall_D1 <- D(100-1.25*x ~ x) ##first derivative/slope
linear_cut_fall_D1 ##rewrite as function
linear_cut_fall_D1_fxn <- function(x) {0*x-1.25}
linear_cut_fall_D1_plot <- plot(linear_cut_fall_D1_fxn,0,20, lwd = 3, xlim = c(0,80), ylim = c(-10,10), 
                                main = "First Derivative of Linear Cut (Fall)", col = "dodgerblue2", cex.lab = 1.5) ##plot of first derivative

linear_cut_fall_D2 <- D(100-1.25*x ~ x & x) ##second derivative
linear_cut_fall_D2 ##rewrite as function
linear_cut_fall_D2_fxn <- function(x) {0*x+0}
linear_cut_fall_D2_plot <- plot(linear_cut_fall_D2_fxn,0,20, lwd = 3, xlim = c(0,80), ylim = c(-2,2), 
                                main = "Second Derivative of Linear Cut (Fall)", col = "dodgerblue2", cex.lab = 1.5) ##plot of second derivative

linear_cut_fall_area <- integrate(linear_cut_fall, 0, 20) ##area
linear_cut_fall_area ##1750

linear_cut_fall_min <- optimize(linear_cut_fall, interval = c(0,20)) ##minimum
linear_cut_fall_min$objective ##75
linear_cut_fall_max <- optimize(linear_cut_fall, interval = c(0,20), maximum = TRUE) ##maximum
linear_cut_fall_max$objective ##100  

##D1 unweighted sum
lcf_d1_sum_unweight <- integrate(linear_cut_fall_D1_fxn, 0, 20)
lcf_d1_sum_unweight ##-25

##D1 weighted "prime": finding sum of various integrals (ages 0-18, 18-20)
integrate(linear_cut_fall_D1_fxn, 0, 18) ##-22.5
integrate(linear_cut_fall_D1_fxn, 18, 20) ##-2.5
-2.5*2 ##weighing the prime (ages 18-20) by 2 = -5
lcf_d1_sum_weight_prime <- -22.5-5
lcf_d1_sum_weight_prime ##-27.5

##D1 "ascending" weights: finding sum of (0,20) integral using weight of 0.25)
integrate(linear_cut_fall_D1_fxn, 0, 20) ##-25
-25*0.25 ##-6.25
lcf_d1_sum_weight_asc <- -6.25
lcf_d1_sum_weight_asc ##-6.25

##D1 "descending" weights: finding sum of (0,20) integral using weight of 1)
integrate(linear_cut_fall_D1_fxn, 0, 20) ##-25
-25*1 ##-25
lcf_d1_sum_weight_des <- -25
lcf_d1_sum_weight_des ##-25

##D1 unweighted and weighted averages (sum/20)
lcf_d1_avg_unweight <- -25/20
lcf_d1_avg_unweight ##-1.25
lcf_d1_avg_weight_prime <- -27.5/20
lcf_d1_avg_weight_prime ##-1.375
lcf_d1_avg_weight_asc <- -6.25/20
lcf_d1_avg_weight_asc ##-0.3125
lcf_d1_avg_weight_des <- -25/20
lcf_d1_avg_weight_des ##-1.25

##D2: because the second derivative is zero, all the values below will also be zero
lcf_d2_sum_unweight <- 0
lcf_d2_sum_weight_prime <- 0
lcf_d2_sum_weight_asc <- 0
lcf_d2_sum_weight_des <- 0
lcf_d2_avg_unweight <- 0
lcf_d2_avg_weight_prime <- 0
lcf_d2_avg_weight_asc <- 0
lcf_d2_avg_weight_des <- 0

##manually generated minima, maxima, and total
##number_minima: 0
##number_maxima: 0
##number_extrema: 0

##30. Exponential Cut (Rise) ---------------------------------------------------------------------------------------------------------

exp_cut_rise <- function(x) {1.0595^x-1} ##function
exp_cut_rise_plot <- ggplot(data.frame(x = c(0, 20)), aes(x)) +
  stat_function(fun = function(x){1.0595^x-1}, color="dodgerblue2", lwd = 1.25, xlim=c(0,20)) + 
  coord_cartesian(ylim=c(0,100))+
  scale_x_continuous(limits = c(0, 80))+
  ggtitle("Exponential Cut (Rise)") + xlab("Age") + ylab("Meaning") + 
  theme_classic()  ##plot function
exp_cut_rise_plot

exp_cut_rise_D1 <- D(1.0595^x-1 ~ x) ##first derivative
exp_cut_rise_D1
exp_cut_rise_D1_plot <- plot(exp_cut_rise_D1,0,20, lwd = 3, xlim = c(0,80), ylim = c(-10,10), 
                             main = "First Derivative of Exponential Cut (Rise)", col = "dodgerblue2", cex.lab = 1.5) ##plot of first derivative

exp_cut_rise_D2 <- D(1.0595^x-1 ~ x & x) ##second derivative
exp_cut_rise_D2
exp_cut_rise_D2_plot <- plot(exp_cut_rise_D2,0,20, lwd = 3, xlim = c(0,80), ylim = c(-2,2), 
                             main = "Second Derivative of Exponential Cut (Rise)", col = "dodgerblue2", cex.lab = 1.5) ##plot of second derivative

exp_cut_rise_area <- integrate(exp_cut_rise, 0, 20) ##area
exp_cut_rise_area ##17.6665

exp_cut_rise_min <- optimize(exp_cut_rise, interval = c(0,20)) ##minimum
exp_cut_rise_min$objective ##0
exp_cut_rise_max <- optimize(exp_cut_rise, interval = c(0,20), maximum = TRUE) ##maximum
exp_cut_rise_max$objective ##2.177001

##D1 unweighted sum
ecr_d1_sum_unweight <- integrate(exp_cut_rise_D1, 0, 20)
ecr_d1_sum_unweight ##2.177015

##D1 weighted "prime": finding sum of various integrals (ages 0-18, 18-20)
integrate(exp_cut_rise_D1, 0, 18) ##1.830201
integrate(exp_cut_rise_D1, 18, 20) ##0.3468136
0.3468136*2 ##weighing the prime (ages 18-20) by 2 = 0.6936272
ecr_d1_sum_weight_prime <- 1.830201+0.6936272
ecr_d1_sum_weight_prime ##2.523828

##D1 "ascending" weights: finding sum of (0,20) integral using weight of 0.25)
integrate(exp_cut_rise_D1, 0, 20) ##2.177015
2.177015*0.25 ##0.5442537
ecr_d1_sum_weight_asc <- 0.5442537
ecr_d1_sum_weight_asc ##0.5442537

##D1 "descending" weights: finding sum of (0,20) integral using weight of 1)
integrate(exp_cut_rise_D1, 0, 20) ##2.177015
2.177015*1 ##2.177015
ecr_d1_sum_weight_des <- 2.177015
ecr_d1_sum_weight_des ##2.177015

##D1 unweighted and weighted averages (sum/20)
ecr_d1_avg_unweight <- 2.177015/20
ecr_d1_avg_unweight ##0.1088507
ecr_d1_avg_weight_prime <- 2.523828/20
ecr_d1_avg_weight_prime ##0.1261914
ecr_d1_avg_weight_asc <- 0.5442537/20
ecr_d1_avg_weight_asc ##0.02721268
ecr_d1_avg_weight_des <- 2.177015/20
ecr_d1_avg_weight_des ##0.1088507

##D2 unweighted sum
ecr_d2_sum_unweight <- integrate(exp_cut_rise_D2, 0, 20)
ecr_d2_sum_unweight ##0.1258251

##D2 weighted "prime": finding sum of various integrals (ages 0-18, 18-20)
integrate(exp_cut_rise_D2, 0, 18) ##0.1057803
integrate(exp_cut_rise_D2, 18, 20) ##0.02004482
0.02004482*2 ##weighing the prime (ages 18-20) by 2 = 0.04008964
ecr_d2_sum_weight_prime <- 0.1057803+0.04008964
ecr_d2_sum_weight_prime ##0.1458699

##D2 "ascending" weights: finding sum of (0,20) integral using weight of 0.25)
integrate(exp_cut_rise_D2, 0, 20) ##0.1258251
0.1258251*0.25 ##0.03145627
ecr_d2_sum_weight_asc <- 0.03145627
ecr_d2_sum_weight_asc ##0.03145627

##D2 "descending" weights: finding sum of (0,20) integral using weight of 1)
integrate(exp_cut_rise_D2, 0, 20) ##0.1258251
0.1258251*1 ##0.1258251
ecr_d2_sum_weight_des <- 0.1258251
ecr_d2_sum_weight_des ##0.1258251

##D1 unweighted and weighted averages (sum/20)
ecr_d2_avg_unweight <- 0.1258251/20
ecr_d2_avg_unweight ##0.006291255
ecr_d2_avg_weight_prime <- 0.1458699/20
ecr_d2_avg_weight_prime ##0.007293495
ecr_d2_avg_weight_asc <- 0.03145627/20
ecr_d2_avg_weight_asc ##0.001572814
ecr_d2_avg_weight_des <- 0.1258251/20
ecr_d2_avg_weight_des ##0.006291255

##manually generated minima, maxima, and total
##number_minima: 0
##number_maxima: 0
##number_extrema: 0

##31. Exponential Cut (Fall) ---------------------------------------------------------------------------------------------------------

exp_cut_fall <- function(x) {1.0595^(-x+80)-1} ##function
exp_cut_fall_plot <- ggplot(data.frame(x = c(0, 20)), aes(x)) +
  stat_function(fun = function(x){1.0595^(-x+80)-1}, color="dodgerblue2", lwd = 1.25, xlim=c(0,20)) + 
  coord_cartesian(ylim=c(0,100))+
  scale_x_continuous(limits = c(0, 80))+
  ggtitle("Exponential Cut (Fall)") + xlab("Age") + ylab("Meaning") + 
  theme_classic()  ##plot function
exp_cut_fall_plot

exp_cut_fall_D1 <- D(1.0595^(-x+80)-1 ~ x) ##first derivative
exp_cut_fall_D1
exp_cut_fall_D1_plot <- plot(exp_cut_fall_D1,0,20, lwd = 3, xlim = c(0,80), ylim = c(-10,10), 
                             main = "First Derivative of Exponential Cut (Fall)", col = "dodgerblue2", cex.lab = 1.5) ##plot of first derivative

exp_cut_fall_D2 <- D(1.0595^(-x+80)-1 ~ x & x) ##second derivative
exp_cut_fall_D2
exp_cut_fall_D2_plot <- plot(exp_cut_fall_D2,0,20, lwd = 3, xlim = c(0,80), ylim = c(-2,2), 
                             main = "Second Derivative of Exponential Cut (Fall)", col = "dodgerblue2", cex.lab = 1.5) ##plot of second derivative

exp_cut_fall_area <- integrate(exp_cut_fall, 0, 20) ##area
exp_cut_fall_area ##1187.85

exp_cut_fall_min <- optimize(exp_cut_fall, interval = c(0,20)) ##minimum
exp_cut_fall_min$objective ##31.06709
exp_cut_fall_max <- optimize(exp_cut_fall, interval = c(0,20), maximum = TRUE) ##maximum
exp_cut_fall_max$objective ##100

##D1 unweighted sum
ecf_d1_sum_unweight <- integrate(exp_cut_fall_D1, 0, 20)
ecf_d1_sum_unweight ##-69.81022

##D1 weighted "prime": finding sum of various integrals (ages 0-18, 18-20)
integrate(exp_cut_fall_D1, 0, 18) ##-65.88073
integrate(exp_cut_fall_D1, 18, 20) ##-3.929492
-3.929492*2 ##weighing the prime (ages 18-20) by 2 = -7.858984
ecf_d1_sum_weight_prime <- -65.88073-7.858984
ecf_d1_sum_weight_prime ##-73.73971

##D1 "ascending" weights: finding sum of (0,20) integral using weight of 0.25)
integrate(exp_cut_fall_D1, 0, 20) ##-69.81022
-69.81022*0.25 ##-17.45256
ecf_d1_sum_weight_asc <- -17.45256
ecf_d1_sum_weight_asc ##-17.45256

##D1 "descending" weights: finding sum of (0,20) integral using weight of 1)
integrate(exp_cut_fall_D1, 0, 20) ##-69.81022
-69.81022*1 ##-69.81022
ecf_d1_sum_weight_des <- -69.81022
ecf_d1_sum_weight_des ##-69.81022

##D1 unweighted and weighted averages (sum/20)
ecf_d1_avg_unweight <- -69.81022/20
ecf_d1_avg_unweight ##-3.490511
ecf_d1_avg_weight_prime <- -73.73971/20
ecf_d1_avg_weight_prime ##-3.686986
ecf_d1_avg_weight_asc <- -17.45256/20
ecf_d1_avg_weight_asc ##-0.872628
ecf_d1_avg_weight_des <- -69.81022/20
ecf_d1_avg_weight_des ##-3.490511

##D2 unweighted sum
ecf_d2_sum_unweight <- integrate(exp_cut_fall_D2, 0, 20)
ecf_d2_sum_unweight ##4.034828

##D2 weighted "prime": finding sum of various integrals (ages 0-18, 18-20)
integrate(exp_cut_fall_D2, 0, 18) ##3.807715
integrate(exp_cut_fall_D2, 18, 20) ##0.2271132
0.2271132*2 ##weighing the prime (ages 18-20) by 2 = 0.4542264
ecf_d2_sum_weight_prime <- 3.807715+0.4542264
ecf_d2_sum_weight_prime ##4.261941

##D2 "ascending" weights: finding sum of (0,20) integral using weight of 0.25)
integrate(exp_cut_fall_D2, 0, 20) ##4.034828
4.034828*0.25 ##1.008707
ecf_d2_sum_weight_asc <- 1.008707
ecf_d2_sum_weight_asc ##1.008707

##D2 "descending" weights: finding sum of (0,20) integral using weight of 1)
integrate(exp_cut_fall_D2, 0, 20) ##4.034828
4.034828*1 ##4.034828
ecf_d2_sum_weight_des <- 4.034828
ecf_d2_sum_weight_des ##4.034828

##D2 unweighted and weighted averages (sum/20)
ecf_d2_avg_unweight <- 4.034828/20
ecf_d2_avg_unweight ##0.2017414
ecf_d2_avg_weight_prime <- 4.261941/20
ecf_d2_avg_weight_prime ##0.2130971
ecf_d2_avg_weight_asc <- 1.008707/20
ecf_d2_avg_weight_asc ##0.05043535
ecf_d2_avg_weight_des <- 4.034828/20
ecf_d2_avg_weight_des ##0.2017414

##manually generated minima, maxima, and total
##number_minima: 0
##number_maxima: 0
##number_extrema: 0

## -------------------------------------------------------------------------------------------------------------------
## View Plots Side by Side -------------------------------------------------------------------------------------------
## -------------------------------------------------------------------------------------------------------------

pdf(file="lifelines.pdf")

par(mfrow = c(3,3))
linear_rise_plot <- plot(linear_rise, lwd = 3, xlim = c(0,80), ylim = c(0,100), main = "Linear Rise", 
                         xlab = "Age", ylab = "Meaning", col = "dodgerblue2", cex.lab = 1.5)
mysubtitle = "(Creation Story)"
mtext(line = 0.5, mysubtitle)
linear_fall_plot <- plot(linear_fall, lwd = 3, xlim = c(0,80), ylim = c(0,100), main = "Linear Fall", 
                         xlab = "Age", ylab = "Meaning", col = "dodgerblue2", cex.lab = 1.5)
linear_low_plot <- plot(linear_low, lwd = 3, xlim = c(0,80), ylim = c(0,100), main = "Linear Low", 
                        xlab = "Age", ylab = "Meaning", col = "dodgerblue2", cex.lab = 1.5)
linear_middle_plot <- plot(linear_middle, lwd = 3, xlim = c(0,80), ylim = c(0,100), main = "Linear Middle", 
                           xlab = "Age", ylab = "Meaning", col = "dodgerblue2", cex.lab = 1.5)
mysubtitle = "(Which Way is Up?)"
mtext(line = 0.5, mysubtitle)
linear_high_plot <- plot(linear_high, lwd = 3, xlim = c(0,80), ylim = c(0,100), main = "Linear High", 
                         xlab = "Age", ylab = "Meaning", col = "dodgerblue2", cex.lab = 1.5)
exp_rise_convex_plot <- plot(exp_rise_convex, lwd = 3, xlim = c(0,80), ylim = c(0,100), main = "Exponential Rise (Convex)", 
                             xlab = "Age", ylab = "Meaning", col = "dodgerblue2", cex.lab = 1.5)
exp_fall_convex_plot <- plot(exp_fall_convex, lwd = 3, xlim = c(0,80), ylim = c(0,100), main = "Exponential Fall (Convex)", 
                             xlab = "Age", ylab = "Meaning", col = "dodgerblue2", cex.lab = 1.5)
exp_rise_concave_plot <- plot(exp_rise_concave, lwd = 3, xlim = c(0,80), ylim = c(0,100), main = "Exponential Rise (Concave)", 
                              xlab = "Age", ylab = "Meaning", col = "dodgerblue2", cex.lab = 1.5)
exp_fall_concave_plot <- plot(exp_fall_concave, lwd = 3, xlim = c(0,80), ylim = c(0,100), main = "Exponential Fall (Concave)", 
                              xlab = "Age", ylab = "Meaning", col = "dodgerblue2", cex.lab = 1.5)
mysubtitle = "(From Bad to Worse)"
mtext(line = 0.5, mysubtitle)

par(mfrow = c(3,3))
sin_fr_full_plot <- plot(sin_fr_full, lwd = 3, xlim = c(1,79), ylim = c(0,100), main = "Sinusoidal Fall, Rise (Full)", 
                         xlab = "Age", ylab = "Meaning", col = "dodgerblue2", cex.lab = 1.5)
mysubtitle = "(Man in Hole)"
mtext(line = 0.5, mysubtitle)
sin_fr_partial_plot <- plot(sin_fr_partial, lwd = 3, xlim = c(0,80), ylim = c(0,100), main = "Sinusoidal Fall, Rise (Partial)", 
                            xlab = "Age", ylab = "Meaning", col = "dodgerblue2", cex.lab = 1.5)
sin_rf_full_plot <- plot(sin_rf_full, lwd = 3, xlim = c(0,80), ylim = c(0,100), main = "Sinusoidal Rise, Fall (Full)", 
                         xlab = "Age", ylab = "Meaning", col = "dodgerblue2", cex.lab = 1.5)
sin_rf_partial_plot <- plot(sin_rf_partial, lwd = 3, xlim = c(0,80), ylim = c(0,100), main = "Sinusoidal Rise, Fall (Partial)", 
                            xlab = "Age", ylab = "Meaning", col = "dodgerblue2", cex.lab = 1.5)
sin_rfr_full_plot <- plot(sin_rfr_full, lwd = 3, xlim = c(0,80), ylim = c(0,100), main = "Sinusoidal Rise, Fall, Rise (Full)", 
                          xlab = "Age", ylab = "Meaning", col = "dodgerblue2", cex.lab = 1.5)
mysubtitle = "(Boy Meets Girl)"
mtext(line = 0.5, mysubtitle)
sin_rfr_partial_plot <- plot(sin_rfr_partial, lwd = 3, xlim = c(0,80), ylim = c(0,100), main = "Sinusoidal Rise, Fall, Rise (Partial)", 
                             xlab = "Age", ylab = "Meaning", col = "dodgerblue2", cex.lab = 1.5)
sin_frf_full_plot <- plot(sin_frf_full, lwd = 3, xlim = c(0,80), ylim = c(0,100), main = "Sinusoidal Fall, Rise, Fall (Full)", 
                          xlab = "Age", ylab = "Meaning", col = "dodgerblue2", cex.lab = 1.5)
sin_frf_partial_plot <- plot(sin_frf_partial, lwd = 3, xlim = c(0,80), ylim = c(0,100), main = "Sinusoidal Fall, Rise, Fall (Partial)", 
                             xlab = "Age", ylab = "Meaning", col = "dodgerblue2", cex.lab = 1.5)

par(mfrow = c(3,3))
logistic_rise_plot <- plot(logistic_rise, lwd = 3, xlim = c(0,80), ylim = c(0,100), main = "Logistic Rise", 
                           xlab = "Age", ylab = "Meaning", col = "dodgerblue2", cex.lab = 1.5)
logistic_fall_plot <- plot(logistic_fall, lwd = 3, xlim = c(0,80), ylim = c(0,100), main = "Logistic Fall", 
                           xlab = "Age", ylab = "Meaning", col = "dodgerblue2", cex.lab = 1.5)
sin_frfr_plot <- plot(sin_frfr, lwd = 3, xlim = c(0,80), ylim = c(0,100), main = "Sinusoidal Fall, Rise, Fall, Rise", 
                      xlab = "Age", ylab = "Meaning", col = "dodgerblue2", cex.lab = 1.5)
sin_rfrf_plot <- plot(sin_rfrf, lwd = 3, xlim = c(0,80), ylim = c(0,100), main = "Sinusoidal Rise, Fall, Rise, Fall", 
                      xlab = "Age", ylab = "Meaning", col = "dodgerblue2", cex.lab = 1.5)
plot(vectFunc,0,80, lwd = 3, xlim = c(0,80), ylim = c(0,100), main = "Positive Change (Full)", 
     xlab = "Age", ylab = "Meaning", col = "dodgerblue2", cex.lab = 1.5) ##plot function
plot(vectFunc2,0,80, lwd = 3, xlim = c(0,80), ylim = c(0,100), main = "Positive Change (Partial)", 
     xlab = "Age", ylab = "Meaning", col = "dodgerblue2", cex.lab = 1.5) ##plot function
plot(vectFunc3,0,80, lwd = 3, xlim = c(0,80), ylim = c(0,100), main = "Negative Change (Full)", 
     xlab = "Age", ylab = "Meaning", col = "dodgerblue2", cex.lab = 1.5) ##plot function
plot(vectFunc4,0,80, lwd = 3, xlim = c(0,80), ylim = c(0,100), main = "Negative Change (Partial)", 
     xlab = "Age", ylab = "Meaning", col = "dodgerblue2", cex.lab = 1.5) ##plot function

par(mfrow = c(3,3))
linear_rise_sharp_fall_plot <- plot(vectFunc5,0,80, lwd = 3, xlim = c(0,80), ylim = c(0,100), main = "Linear Rise, Sharp Fall", 
                                    xlab = "Age", ylab = "Meaning", col = "dodgerblue2", cex.lab = 1.5) ##plot function
mysubtitle = "(Old Testament)"
mtext(line = 0.5, mysubtitle)
linear_rise_sharp_fall_exp_rise_plot <- plot(vectFunc6,0,80, lwd = 3, xlim = c(0,80), ylim = c(0,100), main = "Linear Rise, Sharp Fall, Exponential Rise", 
                                  xlab = "Age", ylab = "Meaning", col = "dodgerblue2", cex.lab = 1.5) ##plot function
mysubtitle = "(New Testament/Cinderella)"
mtext(line = 0.5, mysubtitle)

dev.off()

#----

pdf(file="lifelinesD1.pdf")

par(mfrow = c(3,3))
linear_rise_D1_plot <- plot(linear_rise_D1_fxn, lwd = 3, xlim = c(0,80), ylim = c(-10,10), 
                            main = "First Derivative of Linear Rise)", col = "dodgerblue2", cex.lab = 1.5)
mysubtitle = "(Creation Story)"
mtext(line = 0.5, mysubtitle)
linear_fall_D1_plot <- plot(linear_fall_D1_fxn, lwd = 3, xlim = c(0,80), ylim = c(-10,10), 
                            main = "First Derivative of Linear Fall)", col = "dodgerblue2", cex.lab = 1.5)
linear_low_D1_plot <- plot(linear_low_D1_fxn, lwd = 3, xlim = c(0,80), ylim = c(-10,10), 
                           main = "First Derivative of Linear Low", col = "dodgerblue2", cex.lab = 1.5)
linear_middle_D1_plot <- plot(linear_middle_D1_fxn, lwd = 3, xlim = c(0,80), ylim = c(-10,10), 
                              main = "First Derivative of Linear Middle", col = "dodgerblue2", cex.lab = 1.5)
mysubtitle = "(Which Way is Up?)"
mtext(line = 0.5, mysubtitle)
linear_high_D1_plot <- plot(linear_high_D1_fxn, lwd = 3, xlim = c(0,80), ylim = c(-10,10), 
                            main = "First Derivative of Linear High", col = "dodgerblue2", cex.lab = 1.5)
exp_rise_convex_D1_plot <- plot(exp_rise_convex_D1, lwd = 3, xlim = c(0,80), ylim = c(-10,10), 
                                main = "First Derivative of Exponential Rise (Convex)", col = "dodgerblue2", cex.lab = 1.5)
exp_fall_convex_D1_plot <- plot(exp_fall_convex_D1, lwd = 3, xlim = c(0,80), ylim = c(-10,10), 
                                main = "First Derivative of Exponential Fall (Convex)", col = "dodgerblue2", cex.lab = 1.5)
exp_rise_concave_D1_plot <- plot(exp_rise_concave_D1, lwd = 3, xlim = c(0,80), ylim = c(-10,10), 
                                 main = "First Derivative of Exponential Rise (Concave)", col = "dodgerblue2", cex.lab = 1.5)
exp_fall_concave_D1_plot <- plot(exp_fall_concave_D1, lwd = 3, xlim = c(0,80), ylim = c(-10,10), 
                                 main = "First Derivative of Exponential Fall (Concave)", col = "dodgerblue2", cex.lab = 1.5)
mysubtitle = "(From Bad to Worse)"
mtext(line = 0.5, mysubtitle)

par(mfrow = c(3,3))
sin_fr_full_D1_plot <- plot(sin_fr_full_D1, lwd = 3, xlim = c(0,80), ylim = c(-10,10), 
                            main = "First Derivative of Sinusoidal FR (Full)", col = "dodgerblue2", cex.lab = 1.5)
mysubtitle = "(Man in Hole)"
mtext(line = 0.5, mysubtitle)
sin_fr_partial_D1_plot <- plot(sin_fr_partial_D1, lwd = 3, xlim = c(0,80), ylim = c(-10,10), 
                               main = "First Derivative of Sinusoidal FR (Partial)", col = "dodgerblue2", cex.lab = 1.5)
sin_rf_full_D1_plot <- plot(sin_rf_full_D1, lwd = 3, xlim = c(0,80), ylim = c(-10,10), 
                            main = "First Derivative of Sinusoidal RF (Full)", col = "dodgerblue2", cex.lab = 1.5)
sin_rf_partial_D1_plot <- plot(sin_rf_partial_D1, lwd = 3, xlim = c(0,80), ylim = c(-10,10), 
                               main = "First Derivative of Sinusoidal RF (Partial)", col = "dodgerblue2", cex.lab = 1.5)
sin_rfr_full_D1_plot <- plot(sin_rfr_full_D1, lwd = 3, xlim = c(0,80), ylim = c(-10,10), 
                             main = "First Derivative of Sinusoidal RFR (Full)", col = "dodgerblue2", cex.lab = 1.5)
mysubtitle = "(Boy Meets Girl)"
mtext(line = 0.5, mysubtitle)
sin_rfr_partial_D1_plot <- plot(sin_rfr_partial_D1, lwd = 3, xlim = c(0,80), ylim = c(-10,10), 
                                main = "First Derivative of Sinusoidal RFR (Partial)", col = "dodgerblue2", cex.lab = 1.5)
sin_frf_full_D1_plot <- plot(sin_frf_full_D1, lwd = 3, xlim = c(0,80), ylim = c(-10,10), 
                             main = "First Derivative of Sinusoidal FRF (Full)", col = "dodgerblue2", cex.lab = 1.5)
sin_frf_partial_D1_plot <- plot(sin_frf_partial_D1, lwd = 3, xlim = c(0,80), ylim = c(-10,10), 
                                main = "First Derivative of Sinusoidal FRF (Partial)", col = "dodgerblue2", cex.lab = 1.5)

par(mfrow = c(3,3))
logistic_rise_D1_plot <- plot(logistic_rise_D1, lwd = 3, xlim = c(0,80), ylim = c(-10,10), 
                              main = "First Derivative of Logistic Rise", col = "dodgerblue2", cex.lab = 1.5)
logistic_fall_D1_plot <- plot(logistic_fall_D1, lwd = 3, xlim = c(0,80), ylim = c(-10,10), 
                              main = "First Derivative of Logistic Fall", col = "dodgerblue2", cex.lab = 1.5)
sin_frfr_D1_plot <- plot(sin_frfr_D1, lwd = 3, xlim = c(0,80), ylim = c(-10,10), 
                         main = "First Derivative of Sinusoidal FRFR", col = "dodgerblue2", cex.lab = 1.5)
sin_rfrf_D1_plot <- plot(sin_rfrf_D1, lwd = 3, xlim = c(0,80), ylim = c(-10,10), 
                         main = "First Derivative of Sinusoidal RFRF", col = "dodgerblue2", cex.lab = 1.5)
positive_change_full_1_D1_fxn_plot <- plot(positive_change_full_1_D1_fxn,0,60, 
                                           lwd = 3, xlim = c(0,80), ylim = c(-10,10), 
                                           main = "First Derivative of Positive Change (Full)", 
                                           col = "dodgerblue2", cex.lab = 1.5)
positive_change_full_2_D1_fxn_plot <- curve(positive_change_full_2_D1_fxn,60,80, add = TRUE, lwd = 3, 
                                            xlim = c(60,80), ylim = c(-10,10), 
                                            main = "First Derivative of Positive Change (Full)", 
                                            col = "dodgerblue2", cex.lab = 1.5)
positive_change_partial_1_D1_fxn_plot <- plot(positive_change_partial_1_D1_fxn,0,60, 
                                              lwd = 3, xlim = c(0,80), ylim = c(-10,10), 
                                              main = "First Derivative of Positive Change (Partial)", 
                                              col = "dodgerblue2", cex.lab = 1.5)
positive_change_partial_2_D1_fxn_plot <- curve(positive_change_partial_2_D1_fxn,60,80, add = TRUE, lwd = 3, 
                                               xlim = c(60,80), ylim = c(-10,10), 
                                               main = "First Derivative of Positive Change (Partial)", 
                                               col = "dodgerblue2", cex.lab = 1.5)
negative_change_full_1_D1_fxn_plot <- plot(negative_change_full_1_D1_fxn,0,60, 
                                           lwd = 3, xlim = c(0,80), ylim = c(-10,10), 
                                           main = "First Derivative of Negative Change (Full)", 
                                           col = "dodgerblue2", cex.lab = 1.5)
negative_change_full_2_D1_fxn_plot <- curve(negative_change_full_2_D1_fxn,60,80, add = TRUE, lwd = 3, 
                                            xlim = c(60,80), ylim = c(-10,10), 
                                            main = "First Derivative of Negative Change (Full)", 
                                            col = "dodgerblue2", cex.lab = 1.5)
negative_change_full_1_D1_fxn_plot <- plot(negative_change_full_1_D1_fxn,0,60, 
                                              lwd = 3, xlim = c(0,80), ylim = c(-10,10), 
                                              main = "First Derivative of Negative Change (Partial)", 
                                              col = "dodgerblue2", cex.lab = 1.5)
negative_change_full_2_D1_fxn_plot <- curve(negative_change_full_2_D1_fxn,60,80, add = TRUE, lwd = 3, 
                                               xlim = c(60,80), ylim = c(-10,10), 
                                               main = "First Derivative of Negative Change (Partial)", 
                                               col = "dodgerblue2", cex.lab = 1.5)

par(mfrow = c(3,3))
linear_rise_sharp_fall_1_D1_fxn_plot <- plot(linear_rise_sharp_fall_1_D1_fxn,0,40, 
                                             lwd = 3, xlim = c(0,80), ylim = c(-10,10), 
                                             main = "First Derivative of Linear Rise, Sharp Fall", 
                                             col = "dodgerblue2", cex.lab = 1.5)
mysubtitle = "(Old Testament)"
mtext(line = 0.5, mysubtitle)
linear_rise_sharp_fall_2_D1_fxn_plot <- curve(linear_rise_sharp_fall_2_D1_fxn,40,80, add = TRUE, lwd = 3, 
                                              xlim = c(40,80), ylim = c(-10,10), 
                                              main = "First Derivative of Linear Rise, Sharp Fall", 
                                              col = "dodgerblue2", cex.lab = 1.5)
linear_rise_sharp_fall_exp_rise_1_D1_fxn_plot <- plot(linear_rise_sharp_fall_exp_rise_1_D1_fxn,0,40, 
                                           lwd = 3, xlim = c(0,80), ylim = c(-10,10), 
                                           main = "First Derivative of Linear Rise, Sharp Fall, Exponential Rise", 
                                           col = "dodgerblue2", cex.lab = 1.5)
mysubtitle = "(New Testament/Cinderella)"
mtext(line = 0.5, mysubtitle)
linear_rise_sharp_fall_exp_rise_2_D1_fxn_plot <- curve(linear_rise_sharp_fall_exp_rise_2_D1,40,80, add = TRUE, lwd = 3, 
                                            xlim = c(40,80), ylim = c(-10,10), 
                                            main = "First Derivative of Linear Rise, Sharp Fall, Exponential Rise", 
                                            col = "dodgerblue2", cex.lab = 1.5)

dev.off()

#----

pdf(file="lifelinesD2.pdf")

par(mfrow = c(3,3))
linear_rise_D2_plot <- plot(linear_rise_D2_fxn, lwd = 3, xlim = c(0,80), ylim = c(-2,2), 
                            main = "Second Derivative of Linear Rise)", col = "dodgerblue2", cex.lab = 1.5)
mysubtitle = "(Creation Story)"
mtext(line = 0.5, mysubtitle)
linear_fall_D2_plot <- plot(linear_fall_D2_fxn, lwd = 3, xlim = c(0,80), ylim = c(-2,2), 
                            main = "Second Derivative of Linear Fall)", col = "dodgerblue2", cex.lab = 1.5)
linear_low_D2_plot <- plot(linear_low_D2_fxn, lwd = 3, xlim = c(0,80), ylim = c(-2,2), 
                           main = "Second Derivative of Linear Low", col = "dodgerblue2", cex.lab = 1.5)
linear_middle_D2_plot <- plot(linear_middle_D2_fxn, lwd = 3, xlim = c(0,80), ylim = c(-2,2), 
                              main = "Second Derivative of Linear Middle", col = "dodgerblue2", cex.lab = 1.5)
mysubtitle = "(Which Way is Up?)"
mtext(line = 0.5, mysubtitle)
linear_high_D2_plot <- plot(linear_high_D2_fxn, lwd = 3, xlim = c(0,80), ylim = c(-2,2), 
                            main = "Second Derivative of Linear High", col = "dodgerblue2", cex.lab = 1.5)
exp_rise_convex_D2_plot <- plot(exp_rise_convex_D2, lwd = 3, xlim = c(0,80), ylim = c(-2,2), 
                                main = "Second Derivative of Exponential Rise (convex)", col = "dodgerblue2", cex.lab = 1.5)
exp_fall_convex_D2_plot <- plot(exp_fall_convex_D2, lwd = 3, xlim = c(0,80), ylim = c(-2,2), 
                                main = "Second Derivative of Exponential Fall (convex)", col = "dodgerblue2", cex.lab = 1.5)
exp_rise_concave_D2_plot <- plot(exp_rise_concave_D2, lwd = 3, xlim = c(0,80), ylim = c(-2,2), 
                                 main = "Second Derivative of Exponential Rise (Concave)", col = "dodgerblue2", cex.lab = 1.5)
exp_fall_concave_D2_plot <- plot(exp_fall_concave_D2, lwd = 3, xlim = c(0,80), ylim = c(-2,2), 
                                 main = "Second Derivative of Exponential Fall (Concave)", col = "dodgerblue2", cex.lab = 1.5)
mysubtitle = "(From Bad to Worse)"
mtext(line = 0.5, mysubtitle)

par(mfrow = c(3,3))
sin_fr_full_D2_plot <- plot(sin_fr_full_D2, lwd = 3, xlim = c(0,80), ylim = c(-2,2), 
                            main = "Second Derivative of Sinusoidal FR (Full)", col = "dodgerblue2", cex.lab = 1.5)
mysubtitle = "(Man in Hole)"
mtext(line = 0.5, mysubtitle)
sin_fr_partial_D2_plot <- plot(sin_fr_partial_D2, lwd = 3, xlim = c(0,80), ylim = c(-2,2), 
                               main = "Second Derivative of Sinusoidal FR (Partial)", col = "dodgerblue2", cex.lab = 1.5)
sin_rf_full_D2_plot <- plot(sin_rf_full_D2, lwd = 3, xlim = c(0,80), ylim = c(-2,2), 
                            main = "Second Derivative of Sinusoidal RF (Full)", col = "dodgerblue2", cex.lab = 1.5)
sin_rf_partial_D2_plot <- plot(sin_rf_partial_D2, lwd = 3, xlim = c(0,80), ylim = c(-2,2), 
                               main = "Second Derivative of Sinusoidal RF (Partial)", col = "dodgerblue2", cex.lab = 1.5)
sin_rfr_full_D2_plot <- plot(sin_rfr_full_D2, lwd = 3, xlim = c(0,80), ylim = c(-2,2), 
                             main = "Second Derivative of Sinusoidal RFR (Full)", col = "dodgerblue2", cex.lab = 1.5)
mysubtitle = "(Boy Meets Girl)"
mtext(line = 0.5, mysubtitle)
sin_rfr_partial_D2_plot <- plot(sin_rfr_partial_D2, lwd = 3, xlim = c(0,80), ylim = c(-2,2), 
                                main = "Second Derivative of Sinusoidal RFR (Partial)", col = "dodgerblue2", cex.lab = 1.5)
sin_frf_full_D2_plot <- plot(sin_frf_full_D2, lwd = 3, xlim = c(0,80), ylim = c(-2,2), 
                             main = "Second Derivative of Sinusoidal FRF (Full)", col = "dodgerblue2", cex.lab = 1.5)
sin_frf_partial_D2_plot <- plot(sin_frf_partial_D2, lwd = 3, xlim = c(0,80), ylim = c(-2,2), 
                                main = "Second Derivative of Sinusoidal FRF (Partial)", col = "dodgerblue2", cex.lab = 1.5)

par(mfrow = c(3,3))
logistic_rise_D2_plot <- plot(logistic_rise_D2, lwd = 3, xlim = c(0,80), ylim = c(-2,2), 
                              main = "Second Derivative of Logistic Rise", col = "dodgerblue2", cex.lab = 1.5)
logistic_fall_D2_plot <- plot(logistic_fall_D2, lwd = 3, xlim = c(0,80), ylim = c(-2,2), 
                              main = "Second Derivative of Logistic Fall", col = "dodgerblue2", cex.lab = 1.5)
sin_frfr_D2_plot <- plot(sin_frfr_D2, lwd = 3, xlim = c(0,80), ylim = c(-2,2), 
                         main = "Second Derivative of Sinusoidal FRFR", col = "dodgerblue2", cex.lab = 1.5)
sin_rfrf_D2_plot <- plot(sin_rfrf_D2, lwd = 3, xlim = c(0,80), ylim = c(-2,2), 
                         main = "Second Derivative of Sinusoidal RFRF", col = "dodgerblue2", cex.lab = 1.5)
positive_change_full_1_D2_fxn_plot <- plot(positive_change_full_1_D2_fxn,0,60, lwd = 3, 
                                           xlim = c(0,80), ylim = c(-2,2), 
                                           main = "Second Derivative of Positive Change (Full)", 
                                           col = "dodgerblue2", cex.lab = 1.5)
positive_change_full_2_D2_fxn_plot <- curve(positive_change_full_2_D2_fxn,60,80, add = TRUE, lwd = 3, 
                                            xlim = c(60,80), ylim = c(-2,2), 
                                            main = "Second Derivative of Positive Change (Full)", 
                                            col = "dodgerblue2", cex.lab = 1.5)
positive_change_partial_1_D2_fxn_plot <- plot(positive_change_partial_1_D2_fxn,0,60, lwd = 3, 
                                              xlim = c(0,80), ylim = c(-2,2), 
                                              main = "Second Derivative of Positive Change (Partial)", 
                                              col = "dodgerblue2", cex.lab = 1.5)
positive_change_partial_2_D2_fxn_plot <- curve(positive_change_partial_2_D2_fxn,60,80, add = TRUE, lwd = 3, 
                                               xlim = c(60,80), ylim = c(-2,2), 
                                               main = "Second Derivative of Positive Change (Partial)", 
                                               col = "dodgerblue2", cex.lab = 1.5)
negative_change_full_1_D2_fxn_plot <- plot(negative_change_full_1_D2_fxn,0,60, lwd = 3, 
                                           xlim = c(0,80), ylim = c(-2,2), 
                                           main = "Second Derivative of Negative Change (Full)", 
                                           col = "dodgerblue2", cex.lab = 1.5)
negative_change_full_2_D2_fxn_plot <- curve(negative_change_full_2_D2_fxn,60,80, add = TRUE, lwd = 3, 
                                            xlim = c(60,80), ylim = c(-2,2), 
                                            main = "Second Derivative of Negative Change (Full)", 
                                            col = "dodgerblue2", cex.lab = 1.5)
negative_change_full_1_D2_fxn_plot <- plot(negative_change_full_1_D2_fxn,0,60, lwd = 3, 
                                              xlim = c(0,80), ylim = c(-2,2), 
                                              main = "Second Derivative of Negative Change (Partial)", 
                                              col = "dodgerblue2", cex.lab = 1.5)
negative_change_full_2_D2_fxn_plot <- curve(negative_change_full_2_D2_fxn,60,80, add = TRUE, lwd = 3, 
                                               xlim = c(60,80), ylim = c(-2,2), 
                                               main = "Second Derivative of Negative Change (Partial)", 
                                               col = "dodgerblue2", cex.lab = 1.5)
par(mfrow = c(3,3))
linear_rise_sharp_fall_1_D2_fxn_plot <- plot(linear_rise_sharp_fall_1_D2_fxn,0,40, lwd = 3, 
                                             xlim = c(0,80), ylim = c(-2,2), 
                                             main = "Second Derivative of Linear Rise, Sharp Fall", 
                                             col = "dodgerblue2", cex.lab = 1.5)

mysubtitle = "(Old Testament)"
mtext(line = 0.5, mysubtitle)
linear_rise_sharp_fall_2_D2_fxn_plot <- curve(linear_rise_sharp_fall_2_D2_fxn,40,80, add = TRUE, lwd = 3, 
                                              xlim = c(40,80), ylim = c(-2,2), 
                                              main = "Second Derivative of Linear Rise, Sharp Fall", 
                                              col = "dodgerblue2", cex.lab = 1.5)
linear_rise_sharp_fall_exp_rise_1_D2_fxn_plot <- plot(linear_rise_sharp_fall_exp_rise_1_D2_fxn,0,40, lwd = 3, 
                                           xlim = c(0,80), ylim = c(-2,2), 
                                           main = "Second Derivative of Linear Rise, Sharp Fall, Exponential Rise", 
                                           col = "dodgerblue2", cex.lab = 1.5)
mysubtitle = "(New Testament/Cinderella)"
mtext(line = 0.5, mysubtitle)
linear_rise_sharp_fall_exp_rise_2_D2_plot <- curve(linear_rise_sharp_fall_exp_rise_2_D2,40,80, add = TRUE, lwd = 3, 
                                        xlim = c(40,80), ylim = c(-2,2), 
                                        main = "Second Derivative of Linear Rise, Sharp Fall, Exponential Rise", 
                                        col = "dodgerblue2", cex.lab = 1.5)

dev.off()

#----

pdf(file="early_cutoffs.pdf")
grid.arrange(linear_cut_rise_plot, linear_cut_fall_plot, exp_cut_rise_plot, exp_cut_fall_plot)

par(mfrow = c(2,2))
linear_cut_rise_D1_plot <- plot(linear_cut_rise_D1_fxn,0,20, lwd = 3, xlim = c(0,80), ylim = c(-10,10), 
                                main = "First Derivative of Linear Cut (Rise)", col = "dodgerblue2", cex.lab = 1.5) ##plot of first derivative
linear_cut_fall_D1_plot <- plot(linear_cut_fall_D1_fxn,0,20, lwd = 3, xlim = c(0,80), ylim = c(-10,10), 
                                main = "First Derivative of Linear Cut (Fall)", col = "dodgerblue2", cex.lab = 1.5)
exp_cut_rise_D1_plot <- plot(exp_cut_rise_D1,0,20, lwd = 3, xlim = c(0,80), ylim = c(-10,10), 
                             main = "First Derivative of Exponential Cut (Rise)", col = "dodgerblue2", cex.lab = 1.5)
exp_cut_fall_D1_plot <- plot(exp_cut_fall_D1,0,20, lwd = 3, xlim = c(0,80), ylim = c(-10,10), 
                             main = "First Derivative of Exponential Cut (Fall)", col = "dodgerblue2", cex.lab = 1.5)
par(mfrow = c(2,2))
linear_cut_rise_D2_plot <- plot(linear_cut_rise_D2_fxn,0,20, lwd = 3, xlim = c(0,80), ylim = c(-2,2), 
                                main = "Second Derivative of Linear Cut (Rise)", col = "dodgerblue2", cex.lab = 1.5)
linear_cut_fall_D2_plot <- plot(linear_cut_fall_D2_fxn,0,20, lwd = 3, xlim = c(0,80), ylim = c(-2,2), 
                                main = "Second Derivative of Linear Cut (Fall)", col = "dodgerblue2", cex.lab = 1.5)
exp_cut_rise_D2_plot <- plot(exp_cut_rise_D2,0,20, lwd = 3, xlim = c(0,80), ylim = c(-2,2), 
                             main = "Second Derivative of Exponential Cut (Rise)", col = "dodgerblue2", cex.lab = 1.5)
exp_cut_fall_D2_plot <- plot(exp_cut_fall_D2,0,20, lwd = 3, xlim = c(0,80), ylim = c(-2,2), 
                             main = "Second Derivative of Exponential Cut (Fall)", col = "dodgerblue2", cex.lab = 1.5)

dev.off()

pdf(file="study_checks.pdf")
par(mfrow = c(3,3))
plot1 <- plot(check1, lwd = 3, xlim = c(0,80), ylim = c(0,100), main = "", 
              xlab = "Age", ylab = "Meaning", col = "dodgerblue2", cex.lab = 1.5)
plot2 <- plot(check2, lwd = 3, xlim = c(0,80), ylim = c(0,100), main = "", 
              xlab = "Age", ylab = "Meaning", col = "dodgerblue2", cex.lab = 1.5)
plot3 <- plot(check3, lwd = 3, xlim = c(0,80), ylim = c(0,100), main = "", 
              xlab = "Age", ylab = "Meaning", col = "dodgerblue2", cex.lab = 1.5)

dev.off()

