#Julian De Freitas and Pechthida Kim
#Lifelines

# This code is structured as follows: 
# Define a bunch of functions, then call them. 
# Skip to 'main script' section for the flow of calls.  

#import libraries
if (!require(pacman)) {install.packages("pacman")}
pacman::p_load('rlang', 'dplyr', 'matrixStats', 'ggplot2',
               'calculus', 'mosaic', 'mosaicCalc', 'magrittr', 'filesstrings')

## -------------------------------------------------------------------------------------------------------------
                                              # DEFINE DATA ARRAY 
## -------------------------------------------------------------------------------------------------------------

## DEFINING THE FUNCTION FEATURES ARRAY

# Define equation feature array
feature_names <- c("max", "min", "number_peaks", "number_valleys", "number_extrema", "integral", "d1_sum_unweight", "d1_sum_weight_prime",
                   "d1_sum_weight_asc", "d1_sum_weight_des", "d1_sum_weight_end", "d1_avg_unweight", "d1_avg_weight_prime",
                   "d1_avg_weight_asc", "d1_avg_weight_des", "d1_avg_weight_end", "d2_sum_unweight", "d2_sum_weight_prime",
                   "d2_sum_weight_asc", "d2_sum_weight_des", "d2_sum_weight_end", "d2_avg_unweight", "d2_avg_weight_prime",
                   "d2_avg_weight_asc", "d2_avg_weight_des", "d2_avg_weight_end")

#fyi:FRFR = Fall, Rise, Fall, Rise
plot_names <- c("linear_rise", "linear_fall", 
                "linear_low", "linear_middle", "linear_high", 
                "exp_rise_convex", "exp_fall_convex", "exp_rise_concave", "exp_fall_concave", 
                "sin_fr_full", "sin_fr_partial", "sin_rf_full", "sin_rf_partial", 
                "sin_rfr_full", "sin_rfr_partial", 
                "sin_frf_full", "sin_frf_partial", 
                "sin_frfr", "sin_rfrf", 
                "logistic_rise", "logistic_fall", 
                "positive_change_full", "positive_change_partial", 
                "negative_change_full", "negative_change_partial", 
                "linear_rise_sharp_fall", "linear_rise_sharp_fall_exp_rise")

features <- array(0,dim=c(length(plot_names), length(feature_names))) 
colnames(features) <- feature_names
rownames(features) <- plot_names

## -------------------------------------------------------------------------------------------------------------
                                        # DEFINE EQUATIONS FOR GRAPHING
## -------------------------------------------------------------------------------------------------------------

# Create simple functions we present during initial comprehension checks
create_comp_equations <- function() {
    check1 <- function(x) {x} 
    check2 <- function(x) {50-50*(cos((x-20)*0.079))}
    check3 <- function(x) {100/(1+(exp(1))^-(x-70))}
    check_equations <- c(check1, check2, check3)
    
    return(check_equations)
}

# Define main graph functions 
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
    sin_frfr <- function(x) {50-50*(sin(x*0.1375))}
    sin_rfrf <- function(x) {50+50*(sin(x*0.1375))} 
    logistic_rise <- function(x) {100/(1+(exp(1))^-(x-40))}
    logistic_fall <- function(x) {100-100/(1+(exp(1))^-(x-40))}
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
                   sin_frfr, sin_rfrf, logistic_rise, logistic_fall, 
                   positive_change_full, positive_change_partial, negative_change_full, negative_change_partial, 
                   linear_rise_sharp_fall, linear_rise_sharp_fall_exp_rise)
    
    return(equations)
}

# Define graph first derivative functions
# Note, the first derivative of a linear fn. is just a constant, and you can't take a single
# derivative of a (discontinuous) piecewise function. We have written custom functions for the linear (1-5)
# and piecewise (22-27) fns., in order to be able to plot their derivatives across the range of x values
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
    sin_frfr <- D(50-50*(sin(x*0.1375)) ~ x)
    sin_rfrf <- D(50+50*(sin(x*0.1375)) ~ x)
    logistic_rise <- D(100/(1+(exp(1))^-(x-40)) ~ x)
    logistic_fall <- D(100-100/(1+(exp(1))^-(x-40)) ~ x)
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
                           sin_frfr, sin_rfrf, logistic_rise, logistic_fall, 
                           positive_change_full, positive_change_partial, negative_change_full, negative_change_partial, 
                           linear_rise_sharp_fall, linear_rise_sharp_fall_exp_rise)
    
    return(first_derivatives)
} 

# Define graph second derivative functions
# Ditto note above about linear and piecewise fns. 
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
    sin_frfr <- D(50-50*(sin(x*0.1375)) ~ x&x)
    sin_rfrf <- D(50+50*(sin(x*0.1375)) ~ x&x)
    logistic_rise <- D(100/(1+(exp(1))^-(x-40)) ~ x & x)
    logistic_fall <- D(100-100/(1+(exp(1))^-(x-40)) ~ x & x)
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
                            sin_frfr, sin_rfrf, logistic_rise, logistic_fall,
                            positive_change_full, positive_change_partial, negative_change_full, negative_change_partial, 
                            linear_rise_sharp_fall, linear_rise_sharp_fall_exp_rise)
    
    return(second_derivatives)
} 

# For equations 1-21, subtracting the values of the first derivative and averaging them is the same as
# integrating (finding the area of) and averaging the second derivative. 
# As for the first derivatives, we implement custom solutions for the linear and piecewise fns. 

## -------------------------------------------------------------------------------------------------------------
                                    # DEFINE FUNCTIONS FOR GETTING BASIC FEATURES
## -------------------------------------------------------------------------------------------------------------

# Minima
get_min <- function(equation, start_age, end_age) {
    answer <- optimize(equation, interval = c(start_age, end_age))
    return(answer$objective[1])
}

# Maxima
# Note: the R fn. for getting maxes only works for functions 1-26; but makes a mistake on function 27
# So we define a custom fn. for 27 immediately below
get_max <- function(equation, start_age, end_age) {
    answer <- optimize(equation, interval = c(start_age, end_age), maximum = TRUE)
    return(answer$objective[1])
}

# Find maximum for function 27
get_max_27 <- function(end_age) {
    answer <- optimize(function(x) {
      (x < end_age/2)*(1.25*x) + (end_age/2 < x & x <= end_age)*(1.0606^x-10)}, 
      interval = c(end_age/2, end_age), maximum = TRUE)
    return(answer$objective[1])
}

# Integral
get_integral <- function(equation, end_age) {
    answer <- integrate(equation, 0, end_age)
    return(answer$value)
}

## -------------------------------------------------------------------------------------------------------------
                       # DEFINE FUNCTIONS FOR GETTING FIRST DERIVATIVE-RELATED FEATURES
## -------------------------------------------------------------------------------------------------------------

# Find unweighted sum of the first derivative: this is equal to subtracting
# the values of the original function (i.e., integrating f'(x) from 0 to 80 = f(80)-f(0)).
# Note: these sum and average functions do not work for piecewise functions (26-27)
my_unweight_sum <- function(equation) {
   a <- equation(80)-equation(0)
   return(a)
}

# Find (customized) sum separately for piecewise functions 26 & 27
my_unweight_sum_D1_2 <- function() {   
    linear_rise_sharp_fall <- integrate(function(x) {
       (x <= 40)*(0*x+1.25) + (40 < x & x <= 80)*(0*x+0)}, 0, 80)
    linear_rise_sharp_fall_exp_rise <- integrate(function(x) {
       (x <= 40)*(0*x+1.25) + (40 < x & x <= 80)*(1.0606^x * log(1.0606))}, 0, 80)
    equations <- c(linear_rise_sharp_fall, linear_rise_sharp_fall_exp_rise)
    
    return(equations)
}

# Find sum of first derivative with weighted "prime years of life" (ages 18-30)
my_prime_sum <- function(equation) {
    a <- equation(18)-equation(0)
    b <- 2*(equation(30)-equation(18))
    c <- equation(80)-equation(30)
    d <- a+b+c
    
    return(d)
}

# Find (customized) first derivative with weighted "prime years of life"
# separately for piecewise functions 26 & 27
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

# Find sum of first derivative with ascending weights (0.25, 0.5, 0.75, and 1), i.e.,
# later yrs. matter more
my_ascending_sum <- function(equation) {
    a <- 0.25*(equation(20)-equation(0))
    b <- 0.5*(equation(40)-equation(20))
    c <- 0.75*(equation(60)-equation(40))
    d <- 1*(equation(80)-equation(60))
    e <- a+b+c+d
    return(e)
}

# Find (customized) sum separately for piecewise functions 26 & 27
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

# Find sum of first derivative with descending weights (1, 0.75, 0.5, and 0.25)
# i.e., earlier years matter more
my_descending_sum <- function(equation) {
    a <- 1*(equation(20)-equation(0))
    b <- 0.75*(equation(40)-equation(20))
    c <- 0.5*(equation(60)-equation(40))
    d <- 0.25*(equation(80)-equation(60))
    e <- a+b+c+d
    
    return(e)
}

# Ditto above fn. for piecewise functions 26 & 27
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

# Find sum of first derivative with weighted "end" (ages 60-80), i.e., the very end matters most
my_end_sum <- function(equation) {
  a <- equation(60)-equation(0)
  b <- 2*(equation(80)-equation(60))
  c <- a+b
  
  return(c)
}

# Ditto above fn. with (customized) sum separately for piecewise functions 26 & 27
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

## -------------------------------------------------------------------------------------------------------------
                          # DEFINE FUNCTIONS FOR GETTING SECOND DERIVATIVE-RELATED FEATURES
## -------------------------------------------------------------------------------------------------------------

# Define equations to find sums and averages of second derivative
# To find these values for equations 1-21, we will use their first derivatives (defined above 
# as "create_D1"). For piecewise functions 22-27, we will implement custom solutions as shown below.

# All (customized) functions for piecewise equations 22-27:
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

#...and when prime years of life weighted more
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

#... and when later years weighted increasingly more
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

#... earlier years weighted more
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

#...final years weighted more
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

## -------------------------------------------------------------------------------------------------------------
                                    # DEFINE OTHER FUNCTIONS
## -------------------------------------------------------------------------------------------------------------

## Plot functions 
plotter <- function(equation, x_label, y_label, y_range) {
  plot(equation, lwd = 7, xlim = c(0, 80), ylim = y_range, main = "", 
       xlab = x_label, ylab = y_label, col = "firebrick3", cex.lab = 1.5, cex.axis = 1.5)
  
  return(plotter)
}

# Z-score
# The diff. features have different scales, so we standardize them to same z scale, and save as featuresZ
z_scorer <- function(data) {
    df <- (features-rowMeans(features))/(rowSds(as.matrix(features)))[row(features)] ##calculating Z score
    is.nan.data.frame <- function(x) ##replacing NaN values with 0 (zero), part 1
      do.call(cbind, lapply(x, is.nan))
    df[is.nan(df)] <- 0 ##replacing NaN values with 0 (zero), part 2
    write.csv(df, 'featuresZ.csv')
}

## -------------------------------------------------------------------------------------------------------------
                                                # MAIN SCRIPT
## -------------------------------------------------------------------------------------------------------------

### ----- Create equations
equations <- create_equations()
D1_equations_1_to_21 <- create_D1()[1:21]

### ---- Get their features
num_normal_plots <- 25
start_age <- 0
end_age <- 80

features[ ,"min"] <- sapply(equations, get_min, start_age, end_age)
features[ ,"max"] <- c(sapply(equations[1:26], get_max, start_age, end_age), get_max_27(end_age))
features[ ,"integral"] <- sapply(equations, get_integral, end_age)

features[ ,"d1_sum_unweight"] <- c(sapply(equations[1:num_normal_plots], my_unweight_sum), unlist(my_unweight_sum_D1_2()[seq(1, length(my_unweight_sum_D1_2()), 5)]))
features[ ,"d1_sum_weight_prime"] <- c(sapply(equations[1:num_normal_plots], my_prime_sum), unlist(my_prime_sum_D1_2()[seq(1, length(my_prime_sum_D1_2()), 5)]))
features[ ,"d1_sum_weight_asc"] <- c(sapply(equations[1:num_normal_plots], my_ascending_sum), unlist(my_ascending_sum_D1_2()[seq(1, length(my_ascending_sum_D1_2()), 5)]))
features[ ,"d1_sum_weight_des"] <- c(sapply(equations[1:num_normal_plots], my_descending_sum), unlist(my_descending_sum_D1_2()[seq(1, length(my_descending_sum_D1_2()), 5)]))
features[ ,"d1_sum_weight_end"] <- c(sapply(equations[1:num_normal_plots], my_end_sum), unlist(my_end_sum_D1_2()[seq(1, length(my_end_sum_D1_2()), 5)]))
features[ ,"d1_avg_unweight"] <- features[ ,"d1_sum_unweight"]/end_age
features[ ,"d1_avg_weight_prime"] <- features[ ,"d1_sum_weight_prime"]/end_age
features[ ,"d1_avg_weight_asc"] <- features[ ,"d1_sum_weight_asc"]/end_age
features[ ,"d1_avg_weight_des"] <- features[ ,"d1_sum_weight_des"]/end_age
features[ ,"d1_avg_weight_end"] <- features[ ,"d1_sum_weight_end"]/end_age

features[ ,"d2_sum_unweight"] <- c(sapply(D1_equations_1_to_21, my_unweight_sum), unlist(my_unweight_sum_D2_2()[seq(1, length(my_unweight_sum_D2_2()), 5)]))
features[ ,"d2_sum_weight_prime"] <- c(sapply(D1_equations_1_to_21, my_prime_sum), unlist(my_prime_sum_D2_2()[seq(1, length(my_prime_sum_D2_2()), 5)]))
features[ ,"d2_sum_weight_asc"] <- c(sapply(D1_equations_1_to_21, my_ascending_sum), unlist(my_ascending_sum_D2_2()[seq(1, length(my_ascending_sum_D2_2()), 5)]))
features[ ,"d2_sum_weight_des"] <- c(sapply(D1_equations_1_to_21, my_descending_sum), unlist(my_descending_sum_D2_2()[seq(1, length(my_descending_sum_D2_2()), 5)]))
features[ ,"d2_sum_weight_end"] <- c(sapply(D1_equations_1_to_21, my_end_sum), unlist(my_end_sum_D2_2()[seq(1, length(my_end_sum_D2_2()), 5)]))
features[ ,"d2_avg_unweight"] <- features[ ,"d2_sum_unweight"]/end_age
features[ ,"d2_avg_weight_prime"] <- features[ ,"d2_sum_weight_prime"]/end_age
features[ ,"d2_avg_weight_asc"] <- features[ ,"d2_sum_weight_asc"]/end_age
features[ ,"d2_avg_weight_des"] <- features[ ,"d2_sum_weight_des"]/end_age
features[ ,"d2_avg_weight_end"] <- features[ ,"d2_sum_weight_end"]/end_age

# R's max and min functions count graph endpoints, but we just want the number of valleys and peaks (different from abs. max/min).
#Hence we hand-coded them. Note valleys = minima, peaks = maxima.
features[ ,"number_valleys"] <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 1, 1, 1, 0, 0, 2, 1, 1, 1, 0, 0, 0, 0)
features[ ,"number_peaks"] <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 0, 0, 1, 2, 0, 0, 1, 1, 1, 1)
features[ ,"number_extrema"] <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 2, 0, 0, 3, 3, 1, 1, 1, 1, 1, 1)

### --- Standardize their features
z_scorer(features)

### ----- Plot the functions
plot_array <- c("comprehension", "experiments", "d1", "d2")

for(plot in plot_array) {
  pdf(file = paste0(plot, "_plots.pdf", ""))
  par(mfrow = c(3,3))
  if(plot == 'comprehension') {
    sapply(create_comp_equations(), plotter, "Age", "Fulfillment", c(0,100))
  }
  else if(plot == 'experiments') {
    sapply(create_equations(), plotter, "Age", "Fulfillment", c(0,100))
  }
  else if(plot == 'd1') {
    sapply(create_D1(), plotter, "", "", c(-10,10))
  }
  else if(plot == 'd2') {
    sapply(create_D2(), plotter, "", "", c(-2,2))
  }
  dev.off()
}

dir.create("e1_plots_and_features")
files <- c("comprehension_plots.pdf", "experiments_plots.pdf", "d1_plots.pdf", "d2_plots.pdf", "featuresZ.csv")
file.move(files, "e1_plots_and_features", overwrite = TRUE)

## END -------------------------------------------------------------------------------------------------------------------



## -------------------------------------------------------------------------------------------------------------------
                                          ## EARLY CUTOFFS (POTENTIAL STUDY 2) 
## -------------------------------------------------------------------------------------------------------------------

## -------------------------------------------------------------------------------------------------------------
                                                  # DEFINE DATA ARRAY 
## -------------------------------------------------------------------------------------------------------------

## DEFINING THE FUNCTION FEATURE ARRAY

# Define features_2 array
feature_names_2 <- c("max", "min", "number_peaks", "number_valleys", "number_extrema", "integral", 
                     "d1_sum_unweight", "d1_sum_weight_prime", "d1_sum_weight_asc", "d1_sum_weight_des", 
                     "d1_avg_unweight", "d1_avg_weight_prime", "d1_avg_weight_asc", "d1_avg_weight_des", 
                     "d2_sum_unweight", "d2_sum_weight_prime", "d2_sum_weight_asc", "d2_sum_weight_des", 
                     "d2_avg_unweight", "d2_avg_weight_prime", "d2_avg_weight_asc", "d2_avg_weight_des")

#fyi:FRFR = Fall, Rise, Fall, Rise
plot_names_2 <- c("linear_cut_rise", "linear_cut_fall", 
                  "linear_cut_low", "linear_cut_middle", "linear_cut_high", 
                  "exp_cut_rise_convex", "exp_cut_fall_convex", "exp_cut_rise_concave", "exp_cut_fall_concave", 
                  "sin_cut_fr_full", "sin_cut_fr_partial", "sin_cut_rf_full", "sin_cut_rf_partial", 
                  "sin_cut_rfr_full", "sin_cut_rfr_partial", 
                  "sin_cut_frf_full", "sin_cut_frf_partial", 
                  "sin_cut_frfr", "sin_cut_rfrf", 
                  "logistic_cut_rise", "logistic_cut_fall")


features_2 <- array(0,dim=c(length(plot_names_2), length(feature_names_2)))
colnames(features_2) <- feature_names_2
rownames(features_2) <- plot_names_2

## -------------------------------------------------------------------------------------------------------------
                                          # DEFINE EQUATIONS FOR GRAPHING
## -------------------------------------------------------------------------------------------------------------

my_plot_cutoffs <- function(equation) {
    dev.new(width = 8, height = 6, noRStudioGD = TRUE)
    plot(equation,0,40, lwd = 7, xlim = c(0,80), ylim = c(0,100), main = "", 
         xlab = "Age", ylab = "Fulfillment", col = "firebrick3", cex.lab = 1.5, cex.axis = 1.5)
    return(my_plot_cutoffs)
}

# Define main cutoff functions 
cutoff_functions <- function() {
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
    sin_frfr <- function(x) {50-50*(sin(x*0.1375))}
    sin_rfrf <- function(x) {50+50*(sin(x*0.1375))} 
    logistic_rise <- function(x) {100/(1+(exp(1))^-(x-40))}
    logistic_fall <- function(x) {100-100/(1+(exp(1))^-(x-40))}
    equations <- c(linear_rise, linear_fall, linear_low, linear_middle, linear_high, 
                   exp_rise_convex, exp_fall_convex, exp_rise_concave, exp_fall_concave, 
                   sin_fr_full, sin_fr_partial, sin_rf_full, sin_rf_partial, 
                   sin_rfr_full, sin_rfr_partial, sin_frf_full, sin_frf_partial, 
                   sin_frfr, sin_rfrf, logistic_rise, logistic_fall)
    
    return(equations)
}

# Define graph first derivative functions
# Note: the first derivative of a linear function is just a constant, so we have written 
# custom functions for the linear functions (1-5) so that can can plot their derivatives 
# across the range of x values.
D1_cutoff_functions <- function() {
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
    sin_frfr <- D(50-50*(sin(x*0.1375)) ~ x)
    sin_rfrf <- D(50+50*(sin(x*0.1375)) ~ x)
    logistic_rise <- D(100/(1+(exp(1))^-(x-40)) ~ x)
    logistic_fall <- D(100-100/(1+(exp(1))^-(x-40)) ~ x)
    first_derivatives <- c(linear_rise, linear_fall, linear_low, linear_middle, linear_high, 
                           exp_rise_convex, exp_fall_convex, exp_rise_concave, exp_fall_concave, 
                           sin_fr_full, sin_fr_partial, sin_rf_full, sin_rf_partial, 
                           sin_rfr_full, sin_rfr_partial, sin_frf_full, sin_frf_partial, 
                           sin_frfr, sin_rfrf, logistic_rise, logistic_fall)
    
    return(first_derivatives)
}

# Define graph second derivative functions
# See note above about linear functions
D2_cutoff_functions <- function() {
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
    sin_frfr <- D(50-50*(sin(x*0.1375)) ~ x&x)
    sin_rfrf <- D(50+50*(sin(x*0.1375)) ~ x&x)
    logistic_rise <- D(100/(1+(exp(1))^-(x-40)) ~ x & x)
    logistic_fall <- D(100-100/(1+(exp(1))^-(x-40)) ~ x & x)
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
                            sin_frfr, sin_rfrf, logistic_rise, logistic_fall)
    
    return(second_derivatives)
} 

# Subtracting the values of the first derivative and averaging them is the same as
# integrating (finding the area of) and averaging the second derivative. 
# As for the first derivatives, we implement custom solutions for the linear functions. 

## -------------------------------------------------------------------------------------------------------------
                      # DEFINE FUNCTIONS FOR GETTING FIRST DERIVATIVE-RELATED FEATURES
## -------------------------------------------------------------------------------------------------------------

# Find unweighted sum of the first derivative: this is equal to subtracting
# the values of the original function (i.e., integrating f'(x) from 0 to 80 = f(80)-f(0)).
my_unweight_sum_cutoffs <- function(equation) {
  a <- equation(40)-equation(0)
  return(a)
}

# Find sum of first derivative with weighted "prime years of life" (ages 18-30)
my_prime_sum_cutoffs <- function(equation) {
  a <- equation(18)-equation(0)
  b <- 2*(equation(30)-equation(18))
  c <- equation(40)-equation(30)
  d <- a+b+c
  return(d)
}

# Find sum of first derivative with ascending weights (0.25, 0.5, 0.75, and 1), i.e.,
# later yrs. matter more
my_ascending_sum_cutoffs <- function(equation) {
  a <- 0.25*(equation(20)-equation(0))
  b <- 0.5*(equation(40)-equation(20))
  c <- a+b
  return(c)
}

# Find sum of first derivative with descending weights (1, 0.75, 0.5, and 0.25)
# i.e., earlier years matter more
my_descending_sum_cutoffs <- function(equation) {
  a <- 1*(equation(20)-equation(0))
  b <- 0.75*(equation(40)-equation(20))
  c <- a+b
  return(c)
}

## -------------------------------------------------------------------------------------------------------------
                      # DEFINE FUNCTIONS FOR GETTING SECOND DERIVATIVE-RELATED FEATURES
## -------------------------------------------------------------------------------------------------------------

# Define equations to find sums and averages of second derivative
# To find these values, we will use the first derivatives of the functions (defined above 
# as "D1_cutoff_functions").

## -------------------------------------------------------------------------------------------------------------
                                          # DEFINE OTHER FUNCTIONS
## -------------------------------------------------------------------------------------------------------------

## Plot functions 
plotter_2 <- function(equation, x_label, y_label, y_range) {
  plot(equation, 0, 40, lwd = 7, xlim = c(0,80), ylim = y_range, main = "", 
       xlab = x_label, ylab = y_label, col = "firebrick3", cex.lab = 1.5, cex.axis = 1.5)
  
  return(plotter_2)
}

# Z-score
# The diff. features have different scales, so we standardize them to same z scale, and save as featuresZ_2

z_scorer_2 <- function(data) {
  featuresZ_2 <- (features_2-rowMeans(features_2))/(rowSds(as.matrix(features_2)))[row(features_2)] ##calculating Z score
  is.nan.data.frame <- function(x) ##replacing NaN values with 0 (zero), part 1
    do.call(cbind, lapply(x, is.nan))
  featuresZ_2[is.nan(featuresZ_2)] <- 0 ##replacing NaN values with 0 (zero), part 2
  write.csv(featuresZ_2, 'featuresZ_early_cutoff.csv')
}

## -------------------------------------------------------------------------------------------------------------
                                                  # MAIN SCRIPT
## -------------------------------------------------------------------------------------------------------------

### ----- Create equations
early_cutoffs <- cutoff_functions()
D1_equations_cutoffs <- D1_cutoff_functions()

### ---- Get their features
age_range <- 40

features_2[ ,"min"] <- sapply(early_cutoffs, get_min, age_range)
features_2[ ,"max"] <- sapply(early_cutoffs, get_max, age_range)
features_2[ ,"integral"] <- sapply(early_cutoffs, get_integral, age_range)

features_2[ ,"d1_sum_unweight"] <- sapply(early_cutoffs, my_unweight_sum_cutoffs)
features_2[ ,"d1_sum_weight_prime"] <- sapply(early_cutoffs, my_prime_sum_cutoffs)
features_2[ ,"d1_sum_weight_asc"] <- sapply(early_cutoffs, my_ascending_sum_cutoffs)
features_2[ ,"d1_sum_weight_des"] <- sapply(early_cutoffs, my_descending_sum_cutoffs)
features_2[ ,"d1_avg_unweight"] <- features_2[ ,"d1_sum_unweight"]/age_range
features_2[ ,"d1_avg_weight_prime"] <- features_2[ ,"d1_sum_weight_prime"]/age_range
features_2[ ,"d1_avg_weight_asc"] <- features_2[ ,"d1_sum_weight_asc"]/age_range
features_2[ ,"d1_avg_weight_des"] <- features_2[ ,"d1_sum_weight_des"]/age_range

features_2[ ,"d2_sum_unweight"] <- sapply(D1_equations_cutoffs, my_unweight_sum_cutoffs)
features_2[ ,"d2_sum_weight_prime"] <- sapply(D1_equations_cutoffs, my_prime_sum_cutoffs)
features_2[ ,"d2_sum_weight_asc"] <- sapply(D1_equations_cutoffs, my_ascending_sum_cutoffs)
features_2[ ,"d2_sum_weight_des"] <- sapply(D1_equations_cutoffs, my_descending_sum_cutoffs)
features_2[ ,"d2_avg_unweight"] <- features_2[ ,"d2_sum_unweight"]/age_range
features_2[ ,"d2_avg_weight_prime"] <- features_2[ ,"d2_sum_weight_prime"]/age_range
features_2[ ,"d2_avg_weight_asc"] <- features_2[ ,"d2_sum_weight_asc"]/age_range
features_2[ ,"d2_avg_weight_des"] <- features_2[ ,"d2_sum_weight_des"]/age_range

# R's max and min functions count graph endpoints, but we just want the number of valleys and peaks (different from abs. max/min).
#Hence we hand-coded them. Note valleys = minima, peaks = maxima.

features_2[ ,"number_valleys"] <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0)
features_2[ ,"number_peaks"] <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 1, 1, 0, 0)
features_2[ ,"number_extrema"] <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 1, 1, 1, 1, 2, 2, 0, 0)

### --- Standardize their features
z_scorer_2(features_2)

### ----- Plot the functions
plot_array <- c("early", "d1", "d2")

for(plot in plot_array) {
  pdf(file = paste0(plot, "_cutoff_plots.pdf", ""))
  par(mfrow = c(3,3))
  if(plot == 'early') {
    sapply(cutoff_functions(), plotter_2, "Age", "Fulfillment", c(0,100))
  }
  else if(plot == 'd1') {
    sapply(D1_cutoff_functions(), plotter_2, "", "", c(-10,10))
  }
  else if(plot == 'd2') {
    sapply(D2_cutoff_functions(), plotter_2, "", "", c(-2,2))
  }
  dev.off()
}

dir.create("early_cutoff_plots")
files_2 <- c("early_cutoff_plots.pdf", "d1_cutoff_plots.pdf", "d2_cutoff_plots.pdf", "featuresZ_early_cutoff.csv")
file.move(files_2, "early_cutoff_plots", overwrite = TRUE)

## END -------------------------------------------------------------------------------------------------------------------
