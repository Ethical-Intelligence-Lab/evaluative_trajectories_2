# Calculate predictors for enjoyment (e.g., derivative, integral...)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #set working directory to current directory

#import libraries
if (!require(pacman)) {install.packages("pacman")}
pacman::p_load('rlang', 'dplyr', 'matrixStats', 'ggplot2', 'ggpubr',
               'calculus', 'mosaic', 'mosaicCalc', 'magrittr', 'filesstrings', 'jsonlite', 'rjson', 'Dict')

## -------------------------------------------------------------------------------------------------------------
                                              # DEFINE DATA ARRAY
## -------------------------------------------------------------------------------------------------------------

## DEFINING THE FUNCTION FEATURES ARRAY

# Define equation feature array
feature_names <- c("max", "min", "end_value", "number_peaks", "number_valleys", "number_extrema", "integral", "d1_sum_unweight", "d1_sum_weight_prime",
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

# Create more functions that we can present during the second round of
# comprehension checks if people fail the first
create_comp_equations_2 <- function() {
    check4 <- function(x) {80-x}
    check5 <- function(x) {50+30*(cos((x-20)*0.079))}
    check6 <- function(x) {100/(1+(exp(1))^(x-70))}
    check_equations_2 <- c(check4, check5, check6)

    return(check_equations_2)
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
        (x < end_age*3/4)*((-5/3)*(x-60)) +
        (end_age*3/4 <= x & x <= end_age)*(100+5*(x-80))
    }
    positive_change_partial <- function(x) {
      (x < end_age*3/4)*((-5/3)*(x-60)) +
        (end_age*3/4 <= x & x <= end_age)*(50+2.5*(x-80))
    }
    negative_change_full <- function(x) {
      (x < end_age*3/4)*(5/3)*x +
        (end_age*3/4 <= x & x <= end_age)*(-5*(x-80))
    }
    negative_change_partial <- function(x) {
      (x < end_age*3/4)*(5/3)*x +
        (end_age*3/4 <= x & x <= end_age)*(-2.5*(x-100))
    }
    linear_rise_sharp_fall <- function(x) {
      (x < end_age/2)*(1.25*x) +
        (end_age/2 < x & x <= end_age)*(0*x+0)
    }
    linear_rise_sharp_fall_exp_rise <- function(x) {
      (x < end_age/2)*(1.25*x) +
        (end_age/2 < x & x <= end_age)*(1.0606^x-10)
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
      (x < end_age*3/4)*(0*x-(5/3)) + (end_age*3/4 <= x & x <= end_age)*(0*x+5)
    }
    positive_change_partial <- function(x) {
      (x < end_age*3/4)*(0*x-(5/3)) + (end_age*3/4 <= x & x <= end_age)*(0*x+2.5)
    }
    negative_change_full <- function(x) {
      (x < end_age*3/4)*(0*x+(5/3)) + (end_age*3/4 <= x & x <= end_age)*(0*x-5)
    }
    negative_change_partial <- function(x) {
      (x < end_age*3/4)*(0*x+(5/3)) + (end_age*3/4 <= x & x <= end_age)*(0*x-2.5)
    }
    linear_rise_sharp_fall <- function(x) {
      (x < end_age/2)*(0*x+1.25) + (end_age/2 < x & x <= end_age)*(0*x+0)
    }
    linear_rise_sharp_fall_exp_rise <- function(x) {
      (x < end_age/2)*(0*x+1.25) + (end_age/2 < x & x <= end_age)*(1.0606^x * log(1.0606))
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
      (x < end_age/2)*(0*x+0) + (end_age/2 < x & x <= end_age)*(1.0606^x * log(1.0606) * log(1.0606))
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
    answer <- integrate(equation, start_age, end_age)
    return(answer$value)
}

# End Value
get_end_value <- function(equations, end_age) {
    end_value <- list()
    for(i in 1:length(equations)) {
      options(scipen=999)
      end_value[[i]] <- equations[[i]](end_age)

    }
    return(end_value)
}

## -------------------------------------------------------------------------------------------------------------
                       # DEFINE FUNCTIONS FOR GETTING FIRST DERIVATIVE-RELATED FEATURES
## -------------------------------------------------------------------------------------------------------------

# Find unweighted sum of the first derivative: this is equal to subtracting
# the values of the original function (i.e., integrating f'(x) from 0 to 80 = f(80)-f(0)).
# Note: these sum and average functions do not work for piecewise functions (26-27)
my_unweight_sum <- function(equation) {
   a <- equation(end_age)-equation(start_age)
   return(a)
}

# Find (customized) sum separately for piecewise functions 26 & 27
my_unweight_sum_D1_2 <- function() {
    linear_rise_sharp_fall <- integrate(function(x) {
       (x <= end_age/2)*(0*x+1.25) + (end_age/2 < x & x <= end_age)*(0*x+0)}, start_age, end_age)
    linear_rise_sharp_fall_exp_rise <- integrate(function(x) {
       (x <= end_age/2)*(0*x+1.25) + (end_age/2 < x & x <= end_age)*(1.0606^x * log(1.0606))}, start_age, end_age)
    equations <- c(linear_rise_sharp_fall, linear_rise_sharp_fall_exp_rise)

    return(equations)
}

# Find sum of first derivative with weighted "prime years of life" (ages 18-30)
my_prime_sum <- function(equation) {
    a <- equation(prime_age_start)-equation(start_age)
    b <- 2*(equation(prime_age_end)-equation(prime_age_start))
    c <- equation(end_age)-equation(prime_age_end)
    d <- a+b+c

    return(d)
}

# Find (customized) first derivative with weighted "prime years of life"
# separately for piecewise functions 26 & 27
my_prime_sum_D1_2 <- function() {
    linear_rise_sharp_fall <- integrate(function(x) {
      (x <= prime_age_start)*(0*x+1.25) + (prime_age_start < x & x <= prime_age_end)*(0*x+1.25)*2 +
        (prime_age_end < x & x <= end_age/2)*(0*x+1.25) + (end_age/2 < x & x <= end_age)*(0*x+0)}, start_age, end_age)
    linear_rise_sharp_fall_exp_rise <- integrate(function(x) {
      (x <= prime_age_start)*(0*x+1.25) + (prime_age_start < x & x <= prime_age_end)*(0*x+1.25)*2 +
        (prime_age_end < x & x <= end_age/2)*(0*x+1.25) + (end_age/2 < x & x <= end_age)*(1.0606^x * log(1.0606))}, start_age, end_age)
    equations <- c(linear_rise_sharp_fall, linear_rise_sharp_fall_exp_rise)
    lapply(equations, eval)
}

# Find sum of first derivative with ascending weights (0.25, 0.5, 0.75, and 1), i.e.,
# later yrs. matter more
my_ascending_sum <- function(equation) {
    a <- 0.25*(equation(end_age/4)-equation(start_age))
    b <- 0.5*(equation(end_age/2)-equation(end_age/4))
    c <- 0.75*(equation(end_age*3/4)-equation(end_age/2))
    d <- 1*(equation(end_age)-equation(end_age*3/4))
    e <- a+b+c+d
    return(e)
}

# Find (customized) sum separately for piecewise functions 26 & 27
my_ascending_sum_D1_2 <- function() {
    linear_rise_sharp_fall <- integrate(function(x) {
      (x <= end_age/4)*(0*x+1.25)*0.25 + (end_age/4 < x & x <= end_age/2)*(0*x+1.25)*0.5 +
        (end_age/2 < x & x <= end_age*3/4)*(0*x+0)*0.75 + (end_age*3/4 < x & x <= end_age)*(0*x+0)*1}, start_age, end_age)
    linear_rise_sharp_fall_exp_rise <- integrate(function(x) {
      (x <= end_age/4)*(0*x+1.25)*0.25 + (end_age/4 < x & x <= end_age/2)*(0*x+1.25)*0.5 +
        (end_age/2 < x & x <= end_age*3/4)*(1.0606^x * log(1.0606))*0.75 + (end_age*3/4 < x & x <= end_age)*(1.0606^x * log(1.0606))*1}, start_age, end_age)
    equations <- c(linear_rise_sharp_fall, linear_rise_sharp_fall_exp_rise)
    lapply(equations, eval)
}

# Find sum of first derivative with descending weights (1, 0.75, 0.5, and 0.25)
# i.e., earlier years matter more
my_descending_sum <- function(equation) {
    a <- 1*(equation(end_age/4)-equation(start_age))
    b <- 0.75*(equation(end_age/2)-equation(end_age/4))
    c <- 0.5*(equation(end_age*3/4)-equation(end_age/2))
    d <- 0.25*(equation(end_age)-equation(end_age*3/4))
    e <- a+b+c+d

    return(e)
}

# Ditto above fn. for piecewise functions 26 & 27
my_descending_sum_D1_2 <- function() {
    linear_rise_sharp_fall <- integrate(function(x) {
      (x <= end_age/4)*(0*x+1.25)*1 + (end_age/4 < x & x <= end_age/2)*(0*x+1.25)*0.75 +
        (end_age/2 < x & x <= end_age*3/4)*(0*x+0)*0.5 + (end_age*3/4 < x & x <= end_age)*(0*x+0)*0.25}, start_age, end_age)
    linear_rise_sharp_fall_exp_rise <- integrate(function(x) {
      (x <= end_age/4)*(0*x+1.25)*1 + (end_age/4 < x & x <= end_age/2)*(0*x+1.25)*0.75 +
        (end_age/2 < x & x <= end_age*3/4)*(1.0606^x * log(1.0606))*0.5 + (end_age*3/4 < x & x <= end_age)*(1.0606^x * log(1.0606))*0.25}, start_age, end_age)
    equations <- c(linear_rise_sharp_fall, linear_rise_sharp_fall_exp_rise)
    lapply(equations, eval)
}

# Find sum of first derivative with weighted "end" (ages 60-80), i.e., the very end matters most
my_end_sum <- function(equation) {
  a <- equation(end_age*3/4)-equation(start_age)
  b <- 2*(equation(end_age)-equation(end_age*3/4))
  c <- a+b

  return(c)
}

# Ditto above fn. with (customized) sum separately for piecewise functions 26 & 27
my_end_sum_D1_2 <- function() {
  linear_rise_sharp_fall <- integrate(function(x) {
    (x <= end_age/2)*(0*x+1.25) + (end_age/2 < x & x <= end_age*3/4)*(0*x+0) +
      (end_age*3/4 < x & x <= end_age)*(0*x+0)*2}, start_age, end_age)
  linear_rise_sharp_fall_exp_rise <- integrate(function(x) {
    (x <= end_age/2)*(0*x+1.25) + (end_age/2 < x & x <= end_age*3/4)*(1.0606^x * log(1.0606)) +
      (end_age*3/4 < x & x <= end_age)*(1.0606^x * log(1.0606))*2}, start_age, end_age)
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
    positive_change_full <- integrate(function(x) {0*x+0}, start_age, end_age)
    positive_change_partial <- integrate(function(x) {0*x+0}, start_age, end_age)
    negative_change_full <- integrate(function(x) {0*x+0}, start_age, end_age)
    negative_change_partial <- integrate(function(x) {0*x+0}, start_age, end_age)
    linear_rise_sharp_fall <- integrate(function(x) {0*x+0}, start_age, end_age)
    linear_rise_sharp_fall_exp_rise <- integrate(function(x) {
      (x < end_age/2)*(0*x+0) + (end_age/2 < x & x <= end_age)*(1.0606^x * log(1.0606) * log(1.0606))}, start_age, end_age)
    equations <- c(positive_change_full, positive_change_partial, negative_change_full, negative_change_partial,
                   linear_rise_sharp_fall, linear_rise_sharp_fall_exp_rise)
    lapply(equations, eval)
}

#...and when prime years of life weighted more
my_prime_sum_D2_2 <- function() {
    positive_change_full <- integrate(function(x) {0*x+0}, start_age, end_age)
    positive_change_partial <- integrate(function(x) {0*x+0}, start_age, end_age)
    negative_change_full <- integrate(function(x) {0*x+0}, start_age, end_age)
    negative_change_partial <- integrate(function(x) {0*x+0}, start_age, end_age)
    linear_rise_sharp_fall <- integrate(function(x) {0*x+0}, start_age, end_age)
    linear_rise_sharp_fall_exp_rise <- integrate(function(x) {
      (x < end_age/2)*(0*x+0) + (end_age/2 < x & x <= end_age)*(1.0606^x * log(1.0606) * log(1.0606))}, start_age, end_age)
    equations <- c(positive_change_full, positive_change_partial, negative_change_full, negative_change_partial,
                   linear_rise_sharp_fall, linear_rise_sharp_fall_exp_rise)
    lapply(equations, eval)
}

#... and when later years weighted increasingly more
my_ascending_sum_D2_2 <- function() {
    positive_change_full <- integrate(function(x) {0*x+0}, start_age, end_age)
    positive_change_partial <- integrate(function(x) {0*x+0}, start_age, end_age)
    negative_change_full <- integrate(function(x) {0*x+0}, start_age, end_age)
    negative_change_partial <- integrate(function(x) {0*x+0}, start_age, end_age)
    linear_rise_sharp_fall <- integrate(function(x) {0*x+0}, start_age, end_age)
    linear_rise_sharp_fall_exp_rise <- integrate(function(x) {
      (x < end_age/4)*(0*x+0) + (end_age/2 < x & x <= end_age*3/4)*(1.0606^x * log(1.0606) * log(1.0606))*0.75 +
        (end_age*3/4 < x & x <= end_age)*(1.0606^x * log(1.0606) * log(1.0606))*1}, start_age, end_age)
    equations <- c(positive_change_full, positive_change_partial, negative_change_full, negative_change_partial,
                   linear_rise_sharp_fall, linear_rise_sharp_fall_exp_rise)
    lapply(equations, eval)
}

#... earlier years weighted more
my_descending_sum_D2_2 <- function() {
    positive_change_full <- integrate(function(x) {0*x+0}, start_age, end_age)
    positive_change_partial <- integrate(function(x) {0*x+0}, start_age, end_age)
    negative_change_full <- integrate(function(x) {0*x+0}, start_age, end_age)
    negative_change_partial <- integrate(function(x) {0*x+0}, start_age, end_age)
    linear_rise_sharp_fall <- integrate(function(x) {0*x+0}, start_age, end_age)
    linear_rise_sharp_fall_exp_rise <- integrate(function(x) {
      (x < end_age/4)*(0*x+0) + (end_age/2 < x & x <= end_age*3/4)*(1.0606^x * log(1.0606) * log(1.0606))*0.5 +
        (end_age*3/4 < x & x <= end_age)*(1.0606^x * log(1.0606) * log(1.0606))*0.25}, start_age, end_age)
    equations <- c(positive_change_full, positive_change_partial, negative_change_full, negative_change_partial,
                   linear_rise_sharp_fall, linear_rise_sharp_fall_exp_rise)
    lapply(equations, eval)
}

#...final years weighted more
my_end_sum_D2_2 <- function() {
    positive_change_full <- integrate(function(x) {0*x+0}, start_age, end_age)
    positive_change_partial <- integrate(function(x) {0*x+0}, start_age, end_age)
    negative_change_full <- integrate(function(x) {0*x+0}, start_age, end_age)
    negative_change_partial <- integrate(function(x) {0*x+0}, start_age, end_age)
    linear_rise_sharp_fall <- integrate(function(x) {0*x+0}, start_age, end_age)
    linear_rise_sharp_fall_exp_rise <- integrate(function(x) {
      (x < end_age/4)*(0*x+0) + (end_age/2 < x & x <= end_age*3/4)*(1.0606^x * log(1.0606) * log(1.0606)) +
        (end_age*3/4 < x & x <= end_age)*(1.0606^x * log(1.0606) * log(1.0606))*2}, start_age, end_age)
    equations <- c(positive_change_full, positive_change_partial, negative_change_full, negative_change_partial,
                   linear_rise_sharp_fall, linear_rise_sharp_fall_exp_rise)
    lapply(equations, eval)
}

## -------------------------------------------------------------------------------------------------------------
                                    # DEFINE OTHER FUNCTIONS
## -------------------------------------------------------------------------------------------------------------

## Plot functions
plotter <- function(equation, x_label, y_label, x_range, y_range) {
  #dev.new(width = 8, height = 6, noRStudioGD = TRUE)
  my_plot <- plot(equation, lwd = 7, xlim = x_range, ylim = y_range, main = "",
                  xlab = x_label, ylab = y_label, col = "firebrick3", cex.lab = 1.5, cex.axis = 1.5)

  return(my_plot)
}

# Z-score
# The diff. features have different scales, so we standardize them to same z scale, and save as featuresZ
z_scorer <- function(data) {
  df <- (data-rowMeans(data))/(rowSds(as.matrix(data)))[row(data)] ##calculating Z score
  is.nan.data.frame <- function(x) ##replacing NaN values with 0 (zero), part 1
    do.call(cbind, lapply(x, is.nan))
  df[is.nan(df)] <- 0 ##replacing NaN values with 0 (zero), part 2

  return(df)
}

## -------------------------------------------------------------------------------------------------------------
                                                # MAIN SCRIPT
## -------------------------------------------------------------------------------------------------------------

### ----- Define global variables
start_age <- 0
end_age <- 90
prime_age_start <- 18
prime_age_end <- 30
end_y_axis <- 10
cutoff_age <- 45

### ----- Create equations
data <- read.csv('./data/lifelines_cleaned.csv')

# Create R equations of participant enjoyments
create_equation <- function(eqn) {
    eqn <- fromJSON(eqn)

  if( length(eqn) <= 1) {print("error")}
    return(function(x) {
      l <- length(eqn) - 1

    sum <- 0
    count <- 0
    for( p in l:0 ) {
      count <- count + 1
      sum <- sum + eqn[count] * `^`(x, p)
    }

    return(sum)
    })
}

### Create all equations
eqns = Dict$new(Horror = 0)
titles = c('Horror', 'Adventure', 'Drama', 'Biography', 'Action', 'Fantasy', 'SciFi', 'Animation')
for( title in titles ) {
  col <- paste(title, "_equation", sep="")
  eqns[title] <- apply(data[col], 1, create_equation)

  col_d1 <- paste(title, "_first_derivative", sep="")
  eqns[paste(title, "_d1", sep="")] <- apply(data[col_d1], 1, create_equation)
}


### ----- Get equation features and save them into our dataframe
for( title in titles ) {
  data[, paste(title, "_min", sep="")] <- sapply(eqns[title], get_min, start_age, end_age)
  data[, paste(title, "_max", sep="")] <- sapply(eqns[title], get_max, start_age, end_age)
  data[, paste(title, "_integral", sep="")] <- sapply(eqns[title], get_integral, end_age)
  data[, paste(title, "_end_value", sep="")] <- unlist(get_end_value(eqns[title], end_age))
  data[, paste(title, "_d1_sum_unweight", sep="")] <- sapply(eqns[title], my_unweight_sum)
  data[, paste(title, "_d1_sum_weight_prime", sep="")] <- sapply(eqns[title], my_prime_sum)
  data[, paste(title, "_d1_sum_weight_asc", sep="")] <- sapply(eqns[title], my_ascending_sum)
  data[, paste(title, "_d1_sum_weight_des", sep="")] <- sapply(eqns[title], my_descending_sum)
  data[, paste(title, "_d1_sum_weight_end", sep="")] <- sapply(eqns[title], my_end_sum)
  data[, paste(title, "_d1_avg_unweight", sep="")] <- data[ , paste(title, "_d1_sum_unweight", sep="")] / end_age
  data[, paste(title, "_d1_avg_weight_prime", sep="")] <- data[ , paste(title, "_d1_sum_weight_prime", sep="")] / end_age
  data[, paste(title, "_d1_avg_weight_asc", sep="")] <- data[ , paste(title, "_d1_sum_weight_asc", sep="")] / end_age
  data[, paste(title, "_d1_avg_weight_des", sep="")] <- data[ , paste(title, "_d1_sum_weight_des", sep="")] / end_age
  data[, paste(title, "_d1_avg_weight_end", sep="")] <- data[ , paste(title, "_d1_sum_weight_end", sep="")] / end_age

  data[, paste(title, "_d2_sum_unweight", sep="")] <- sapply(eqns[paste(title, "_d1", sep="")], my_unweight_sum)
  data[, paste(title, "_d2_sum_weight_prime", sep="")] <- sapply(eqns[paste(title, "_d1", sep="")], my_prime_sum)
  data[, paste(title, "_d2_sum_weight_asc", sep="")] <- sapply(eqns[paste(title, "_d1", sep="")], my_ascending_sum)
  data[, paste(title, "_d2_sum_weight_des", sep="")] <- sapply(eqns[paste(title, "_d1", sep="")], my_descending_sum)
  data[, paste(title, "_d2_sum_weight_end", sep="")] <- sapply(eqns[paste(title, "_d1", sep="")], my_end_sum)
  data[, paste(title, "_d2_avg_unweight", sep="")] <- data[paste(title, "_d2_sum_unweight", sep="")] / end_age
  data[, paste(title, "_d2_avg_weight_prime", sep="")] <- data[paste(title, "_d2_sum_weight_prime", sep="")] / end_age
  data[, paste(title, "_d2_avg_weight_asc", sep="")] <- data[paste(title, "_d2_sum_weight_asc", sep="")] / end_age
  data[, paste(title, "_d2_avg_weight_des", sep="")] <- data[paste(title, "_d2_sum_weight_des", sep="")] / end_age
  data[, paste(title, "_d2_avg_weight_end", sep="")] <- data[paste(title, "_d2_sum_weight_end", sep="")] / end_age
}

write.csv(data, './data/lifelines_w_features.csv')

### --- Standardize their features_participants
#df <- z_scorer(features_participants)
#write.csv(df, 'features_participantsZ.csv')

### ----- Plot the functions
plot_experiment_figures <- FALSE


# Individual plots
if(plot_experiment_figures == TRUE) {
  my_comp_equations <- create_comp_equations()
  for(i in 1:length(my_comp_equations)) {
    png(file = paste0(i,"_comprehension_plots.png", ""))
    sapply(my_comp_equations[i], plotter, "Age", "Stress", c(start_age, end_age), c(0, end_y_axis))
    dev.off()
  }
  my_comp_equations_2 <- create_comp_equations_2()
  for(i in 1:length(my_comp_equations_2)) {
    png(file = paste0(i,"_comprehension_plots_2.png", ""))
    sapply(my_comp_equations_2[i], plotter, "Age", "Stress", c(start_age, end_age), c(0, end_y_axis))
    dev.off()
  }
  my_equations <- create_equations()
  for(i in 1:length(my_equations)) {
    png(file = paste0(i,"_experimental_plots.png", ""))
    sapply(my_equations[i], plotter, "Age", "Happiness", c(start_age, end_age), c(0, end_y_axis))
    dev.off()
  }
  dir.create("e1b_basic_effect/lifeline_plots", recursive = TRUE)
  plot_individuals <- c(list.files(pattern = ".png"))
  file.move(plot_individuals, "e1b_basic_effect/lifeline_plots", overwrite = TRUE)
  file.move('features_participantsZ.csv', "e1b_basic_effect/lifeline_plots", overwrite = TRUE)
}


# In a grid
if(plot_experiment_figures == TRUE) {
  plot_array <- c("comprehension", "experiments", "d1", "d2")
  for(plot in plot_array) {
    pdf(file = paste0(plot, "_plots.pdf", ""))
    par(mfrow = c(3,3))
    if(plot == 'comprehension') {
      sapply(create_comp_equations(), plotter, "Age", "Stress", c(start_age, end_age), c(0, end_y_axis))
    }
    else if(plot == 'experiments') {
      sapply(create_equations(), plotter, "Age", "Happiness", c(start_age, end_age), c(0, end_y_axis))
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
  file.move(files, "e1b_basic_effect/lifeline_plots", overwrite = TRUE)
}


# Combine axis labels using ggpubr::ggarrange()
if(plot_experiment_figures == TRUE) {
  pdf(file = "grid_plots.pdf", 16, 5.5)

  par(mar = c(2, 2, 2, 2))
  par(mfrow = c(3,9), omi=c(0.5,0.6,0,0))

  sapply(create_equations(), plotter, "", "", c(start_age, end_age), c(0, end_y_axis))
  mtext("Happiness", side = 2, outer = TRUE, cex = 2.5, line = 1.5, font=1)
  mtext("Age", side = 1, outer = TRUE, cex = 2.5, line = 2, font=1)

  dev.off()

  files <- list.files(pattern = c("(.pdf|.png)"))
  file.move(files, "e1b_basic_effect/lifeline_plots", overwrite = TRUE)
}

## END -------------------------------------------------------------------------------------------------------------------
