#Julian De Freitas and Pechthida Kim
#Lifelines

# This code is structured as follows: 
# Define a bunch of functions, then call them. 
# Skip to 'main script' section for the flow of calls.  

#import libraries
if (!require(pacman)) {install.packages("pacman")}
pacman::p_load('rlang', 'dplyr', 'matrixStats', 'ggplot2',
               'calculus', 'mosaic', 'mosaicCalc', 'inflection', 'magrittr')

## -------------------------------------------------------------------------------------------------------------
                                              # DEFINE DATA ARRAY 
## -------------------------------------------------------------------------------------------------------------

## DEFINING THE FUNCTION FEATURE ARRAY

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
                "logistic_rise", "logistic_fall", 
                "sin_frfr", "sin_rfrf", 
                "positive_change_full", "positive_change_partial", 
                "negative_change_full", "negative_change_partial", 
                "linear_rise_sharp_fall", "linear_rise_sharp_fall_exp_rise")

features <- array(0,dim=c(length(plot_names), length(feature_names))) #averaging across vignettes
colnames(features) <- feature_names
rownames(features) <- plot_names

#!!!!!!!!!!!!!!!!!!!!!!!!! I grouped the functions into sections based on similar functionality, just to organize a bit 

## -------------------------------------------------------------------------------------------------------------
                                        # DEFINE EQUATIONS FOR GRAPHING
## -------------------------------------------------------------------------------------------------------------

#!!!!!!!!!!!!!!!! I deleted comments that didn't seem like they went with any particular section of code, so pls. check that
# I didn't accidentally delete anything that is needed!

# !!!!!!!!!!!!!!!! For all code inside functions, I used double indents, in order to malke
# occurence of functions more easily identifiable from afar. If the function was long and included a 'return'
# I also included an empty line before the return, so that the return is easily identifiable

# !!!!!!!!!!!!!!!! If we can shorten this particular section, e.g., only write out equestions once, that would be great
# but I realize this might be impossible.

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
    sin_frfr <- function(x) {50-50*(sin(x*0.1375))} ## !!!!!!!!!!!!!!!!!!! these two were plotted after logistic, but I figured it's neater to plot all the sinusoid ones together
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

# !!!!!!!!!!!!!!!!!!! aren't these equations the same as the d1 equations, except for the positive change functions? 
# if so, how about re-using the first couple of d1 equations, rather than writing them all out again?
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
    sin_frfr <- D(50-50*(sin(x*0.1375)) ~ x)
    sin_rfrf <- D(50+50*(sin(x*0.1375)) ~ x)
    logistic_rise <- D(100/(1+(exp(1))^-(x-40)) ~ x)
    logistic_fall <- D(100-100/(1+(exp(1))^-(x-40)) ~ x)
    
    equations_2 <- c(linear_rise, linear_fall, linear_low, linear_middle, linear_high, 
                     exp_rise_convex, exp_fall_convex, exp_rise_concave, exp_fall_concave, 
                     sin_fr_full, sin_fr_partial, sin_rf_full, sin_rf_partial, 
                     sin_rfr_full, sin_rfr_partial, sin_frf_full, sin_frf_partial, 
                     logistic_rise, logistic_fall, sin_frfr, sin_rfrf)
    
    return(equations_2)
} 


## -------------------------------------------------------------------------------------------------------------
                                    # DEFINE FUNCTIONS FOR GETTING BASIC FEATURES
## -------------------------------------------------------------------------------------------------------------

# Minima
get_min <- function(equation) {
    answer <- optimize(equation, interval = c(0,80))
    return(answer$objective[1])
}

# Maxima
# Note: the R fn. for getting maxes only works for functions 1-26; but makes a mistake on function 27
# So we define a custom fn. for 27 immediately below
get_max <- function(equation) {
    answer <- optimize(equation, interval = c(0,80), maximum = TRUE)
    return(answer$objective[1])
}

# Find maximum for function 27
get_max_2 <- function(equation) {
    answer <- optimize(function(x) {
      (x < 40)*(1.25*x) + (40 < x & x <= 80)*(1.0606^x-10)}, 
      interval = c(40,80), maximum = TRUE)
    return(answer$objective[1])
}

# Find integral
get_integral <- function(equation) {
    answer <- integrate(equation, 0, 80)
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

# All customized functions for piecewise equations 22-27:
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

#...prime years of life weighted more
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

#... later years weighted increasingly more
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

##plotting functions 
#!!!!!!!!!!!!!!!!!!!! I made a single plotting function that takes as inputs the equation, labels, and y range 

plotter <- function(equation, x_label, y_label, y_range) {
  #dev.new(width = 8, height = 6, noRStudioGD = TRUE) #!!!!!!! I deleted this line, so that R isn't also plotting the functions in windows, otherwise the user has to hand-delete all the plots
  plot(equation, lwd = 7, xlim = c(0,80), ylim = y_range, main = "", 
       xlab = x_label, ylab = y_label, col = "firebrick3", cex.lab = 1.5, cex.axis = 1.5)
  
  return(plotter)
}

#z-score
# The diff. features have different scales, so we standardize them to same z scale, and save as featuresZ

#!!!!!!!!!!!!!!!!! I turned your code below into a z-score function
##!!!!!!!!!!!!!!!!!!!!! #is the only reason you're focusing on columns 6-26 because it
#looks better when you make the d1 and d2 plots?
#if so, lets make two dataframes: one that only includes 6:26 (as you currently do), and one that standardizes
#across *all* columns. We can use the first one for plotting, and the second can be saved as a .csv (and for analyses) 
#otherwise, the regression we run will be skewed by the unstandardized variables (given their much larger values)

z_scorer <- function(data) {
    featuresN <- data[ ,6:26] ##getting the columns 6-26 from the 'features' array 
    df <- (featuresN-rowMeans(featuresN))/(rowSds(as.matrix(featuresN)))[row(featuresN)] ##calculating Z score
    is.nan.data.frame <- function(x) ##replacing NaN values with 0 (zero), part 1
      do.call(cbind, lapply(x, is.nan))
    df[is.nan(df)] <- 0 ##replacing NaN values with 0 (zero), part 2
    sub_features <- features[ ,1:5] #extracting columns 1-5 from 'features'
    df <- cbind(df, sub_features) ##merging 'df' and 'sub_features'
    featuresZ <- df[, feature_names] ##creating the final dataframe, 'featuresZ'
    View(featuresZ)
    write.csv(featuresZ, 'featuresZ.csv')
}


## -------------------------------------------------------------------------------------------------------------
                                                # MAIN SCRIPT
## -------------------------------------------------------------------------------------------------------------

### ----- Create equations
equations <- create_equations()
equations_2 <- create_equations_2() # !!!!!!!!! maybe give this a more descriptive name?


### ---- Get their features
num_normal_plots <- 25 #!!!!!!!!!!! notice that I'm setting these variables here, rather than writing them out. if later we want to change these, then we don't need to change these everywhere else in the code too
max_age <- 80

features[ ,"min"] <- sapply(equations, get_min)
features[ ,"max"] <- c(sapply(equations[1:26], get_max), get_max_2())
features[ ,"integral"] <- sapply(equations, get_integral)

features[ ,"d1_sum_unweight"] <- c(sapply(equations[1:num_normal_plots], my_unweight_sum), unlist(my_unweight_sum_D1_2()[seq(1, length(my_unweight_sum_D1_2()), 5)]))
features[ ,"d1_sum_weight_prime"] <- c(sapply(equations[1:num_normal_plots], my_prime_sum), unlist(my_prime_sum_D1_2()[seq(1, length(my_prime_sum_D1_2()), 5)]))
features[ ,"d1_sum_weight_asc"] <- c(sapply(equations[1:num_normal_plots], my_ascending_sum), unlist(my_ascending_sum_D1_2()[seq(1, length(my_ascending_sum_D1_2()), 5)]))
features[ ,"d1_sum_weight_des"] <- c(sapply(equations[1:num_normal_plots], my_descending_sum), unlist(my_descending_sum_D1_2()[seq(1, length(my_descending_sum_D1_2()), 5)]))
features[ ,"d1_sum_weight_end"] <- c(sapply(equations[1:num_normal_plots], my_end_sum), unlist(my_end_sum_D1_2()[seq(1, length(my_end_sum_D1_2()), 5)]))
features[ ,"d1_avg_unweight"] <- features[ ,"d1_sum_unweight"]/max_age
features[ ,"d1_avg_weight_prime"] <- features[ ,"d1_sum_weight_prime"]/max_age
features[ ,"d1_avg_weight_asc"] <- features[ ,"d1_sum_weight_asc"]/max_age
features[ ,"d1_avg_weight_des"] <- features[ ,"d1_sum_weight_des"]/max_age
features[ ,"d1_avg_weight_end"] <- features[ ,"d1_sum_weight_end"]/max_age

features[ ,"d2_sum_unweight"] <- c(sapply(equations_2, my_unweight_sum), unlist(my_unweight_sum_D2_2()[seq(1, length(my_unweight_sum_D2_2()), 5)]))
features[ ,"d2_sum_weight_prime"] <- c(sapply(equations_2, my_prime_sum), unlist(my_prime_sum_D2_2()[seq(1, length(my_prime_sum_D2_2()), 5)]))
features[ ,"d2_sum_weight_asc"] <- c(sapply(equations_2, my_ascending_sum), unlist(my_ascending_sum_D2_2()[seq(1, length(my_ascending_sum_D2_2()), 5)]))
features[ ,"d2_sum_weight_des"] <- c(sapply(equations_2, my_descending_sum), unlist(my_descending_sum_D2_2()[seq(1, length(my_descending_sum_D2_2()), 5)]))
features[ ,"d2_sum_weight_end"] <- c(sapply(equations_2, my_end_sum), unlist(my_end_sum_D2_2()[seq(1, length(my_end_sum_D2_2()), 5)]))
features[ ,"d2_avg_unweight"] <- features[ ,"d2_sum_unweight"]/max_age
features[ ,"d2_avg_weight_prime"] <- features[ ,"d2_sum_weight_prime"]/max_age
features[ ,"d2_avg_weight_asc"] <- features[ ,"d2_sum_weight_asc"]/max_age
features[ ,"d2_avg_weight_des"] <- features[ ,"d2_sum_weight_des"]/max_age
features[ ,"d2_avg_weight_end"] <- features[ ,"d2_sum_weight_end"]/max_age

# R's max and min functions count graph endpoints, but we just want the number of valleys and peaks (different from abs. max/min).
#Hence we hand-coded them. NOte valleys = minima, peaks = maxima.
features[ ,"number_valleys"] <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 1, 1, 1, 0, 0, 2, 1, 1, 1, 0, 0, 0, 0)
features[ ,"number_peaks"] <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 0, 0, 1, 2, 0, 0, 1, 1, 1, 1)
features[ ,"number_extrema"] <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 2, 0, 0, 3, 3, 1, 1, 1, 1, 1, 1)


### --- Standardize their features
z_scorer(features)


### ----- Plot the functions
plot_array <- c("comprehension", "experiments", "d1", "d2")

for(plot in plot_array) {
  pdf(file = paste(plot, "_plots.pdf", ""))
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

## END -------------------------------------------------------------------------------------------------------------------



## -------------------------------------------------------------------------------------------------------------------
                                          ## EARLY CUTOFFS (POTENTIAL STUDY 2) 
## -------------------------------------------------------------------------------------------------------------------


#!!!!!!!!!!!!!!!!!!!!!!!! Apply analogous changes from above to the code below


## -------------------------------------------------------------------------------------------------------------------
## DEFINE ALL FUNCTIONS ----------------------------------------------------------------------------------------------
## -------------------------------------------------------------------------------------------------------------------

#define feature array
feature_names_2 <- c("max", "min", "number_peaks", "number_valleys", "number_extrema", "integral", 
                     "d1_sum_unweight", "d1_sum_weight_prime", "d1_sum_weight_asc", "d1_sum_weight_des", 
                     "d1_avg_unweight", "d1_avg_weight_prime", "d1_avg_weight_asc", "d1_avg_weight_des", 
                     "d2_sum_unweight", "d2_sum_weight_prime", "d2_sum_weight_asc", "d2_sum_weight_des", 
                     "d2_avg_unweight", "d2_avg_weight_prime", "d2_avg_weight_asc", "d2_avg_weight_des")


#!!!!!!!!!!!!!!!!!!!! is there a reason we're only looking at this subset of plots? Can we do cutoff plots for all the functions? 
#Also, lets write the code so that it's easy to change the age without needing to edit any other code, e.g., you can have
# a variable called 'age_of_death=40', then you can plut that variable in wherever you're currently typing '40'

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

##!!!!!!!!!!!!!!!!!!!!!!!!!! I deleted straggling code from previous versions. Let's always do this:
# If we want to save old code, then we can just keep old versions of this R script. 
