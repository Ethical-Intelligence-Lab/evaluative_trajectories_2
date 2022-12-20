# Calculate predictors for enjoyment (e.g., derivative, integral...)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #set working directory to current directory

#import libraries
if (!require(pacman)) { install.packages("pacman") }
pacman::p_load('rlang', 'dplyr', 'matrixStats', 'ggplot2', 'ggpubr',
               'calculus', 'mosaic', 'mosaicCalc', 'magrittr', 'filesstrings', 'jsonlite', 'rjson', 'Dict')
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
        (x < end_age / 2) * (1.25 * x) + (end_age / 2 < x & x <= end_age) * (1.0606^x - 10) },
                       interval = c(end_age / 2, end_age), maximum = TRUE)
    return(answer$objective[1])
}

# Integral
get_integral <- function(equation, end_age) {
    answer <- integrate(equation, start_age, end_age)
    return(answer$value)
}

# Start Value
get_start_value <- function(equations) {
    end_value <- list()
    for (i in 1:length(equations)) {

#        curve(equations[[i]], from=1, to=90, , xlab="x", ylab="y")

        options(scipen = 999)
        end_value[[i]] <- integrate(equations[[i]], 0, 3)$value

    }
    return(end_value)
}

# End Value
get_end_value <- function(equations, end_age) {
    end_value <- list()
    for (i in 1:length(equations)) {
        options(scipen = 999)
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
    a <- equation(end_age) - equation(start_age)
    return(a)
}

# Find (customized) sum separately for piecewise functions 26 & 27
my_unweight_sum_D1_2 <- function() {
    linear_rise_sharp_fall <- integrate(function(x) {
        (x <= end_age / 2) * (0 * x + 1.25) + (end_age / 2 < x & x <= end_age) * (0 * x + 0) }, start_age, end_age)
    linear_rise_sharp_fall_exp_rise <- integrate(function(x) {
        (x <= end_age / 2) * (0 * x + 1.25) + (end_age / 2 < x & x <= end_age) * (1.0606^x * log(1.0606)) }, start_age, end_age)
    equations <- c(linear_rise_sharp_fall, linear_rise_sharp_fall_exp_rise)

    return(equations)
}

# Find sum of first derivative with weighted "prime years of life" (ages 18-30)
my_prime_sum <- function(equation) {
    a <- equation(prime_age_start) - equation(start_age)
    b <- 2 * (equation(prime_age_end) - equation(prime_age_start))
    c <- equation(end_age) - equation(prime_age_end)
    d <- a + b + c

    return(d)
}

# Find (customized) first derivative with weighted "prime years of life"
# separately for piecewise functions 26 & 27
my_prime_sum_D1_2 <- function() {
    linear_rise_sharp_fall <- integrate(function(x) {
        (x <= prime_age_start) * (0 * x + 1.25) +
            (prime_age_start < x & x <= prime_age_end) *
                (0 * x + 1.25) *
                2 +
            (prime_age_end < x & x <= end_age / 2) * (0 * x + 1.25) +
            (end_age / 2 < x & x <= end_age) * (0 * x + 0) }, start_age, end_age)
    linear_rise_sharp_fall_exp_rise <- integrate(function(x) {
        (x <= prime_age_start) * (0 * x + 1.25) +
            (prime_age_start < x & x <= prime_age_end) *
                (0 * x + 1.25) *
                2 +
            (prime_age_end < x & x <= end_age / 2) * (0 * x + 1.25) +
            (end_age / 2 < x & x <= end_age) * (1.0606^x * log(1.0606)) }, start_age, end_age)
    equations <- c(linear_rise_sharp_fall, linear_rise_sharp_fall_exp_rise)
    lapply(equations, eval)
}

# Find sum of first derivative with ascending weights (0.25, 0.5, 0.75, and 1), i.e.,
# later yrs. matter more
my_ascending_sum <- function(equation) {
    a <- 0.25 * (equation(end_age / 4) - equation(start_age))
    b <- 0.5 * (equation(end_age / 2) - equation(end_age / 4))
    c <- 0.75 * (equation(end_age * 3 / 4) - equation(end_age / 2))
    d <- 1 * (equation(end_age) - equation(end_age * 3 / 4))
    e <- a + b + c + d
    return(e)
}

# Find (customized) sum separately for piecewise functions 26 & 27
my_ascending_sum_D1_2 <- function() {
    linear_rise_sharp_fall <- integrate(function(x) {
        (x <= end_age / 4) * (0 * x + 1.25) * 0.25 +
            (end_age / 4 < x & x <= end_age / 2) *
                (0 * x + 1.25) *
                0.5 +
            (end_age / 2 < x & x <= end_age * 3 / 4) *
                (0 * x + 0) *
                0.75 +
            (end_age * 3 / 4 < x & x <= end_age) *
                (0 * x + 0) *
                1 }, start_age, end_age)
    linear_rise_sharp_fall_exp_rise <- integrate(function(x) {
        (x <= end_age / 4) * (0 * x + 1.25) * 0.25 +
            (end_age / 4 < x & x <= end_age / 2) *
                (0 * x + 1.25) *
                0.5 +
            (end_age / 2 < x & x <= end_age * 3 / 4) *
                (1.0606^x * log(1.0606)) *
                0.75 +
            (end_age * 3 / 4 < x & x <= end_age) *
                (1.0606^x * log(1.0606)) *
                1 }, start_age, end_age)
    equations <- c(linear_rise_sharp_fall, linear_rise_sharp_fall_exp_rise)
    lapply(equations, eval)
}

# Find sum of first derivative with descending weights (1, 0.75, 0.5, and 0.25)
# i.e., earlier years matter more
my_descending_sum <- function(equation) {
    a <- 1 * (equation(end_age / 4) - equation(start_age))
    b <- 0.75 * (equation(end_age / 2) - equation(end_age / 4))
    c <- 0.5 * (equation(end_age * 3 / 4) - equation(end_age / 2))
    d <- 0.25 * (equation(end_age) - equation(end_age * 3 / 4))
    e <- a + b + c + d

    return(e)
}

# Ditto above fn. for piecewise functions 26 & 27
my_descending_sum_D1_2 <- function() {
    linear_rise_sharp_fall <- integrate(function(x) {
        (x <= end_age / 4) * (0 * x + 1.25) * 1 +
            (end_age / 4 < x & x <= end_age / 2) *
                (0 * x + 1.25) *
                0.75 +
            (end_age / 2 < x & x <= end_age * 3 / 4) *
                (0 * x + 0) *
                0.5 +
            (end_age * 3 / 4 < x & x <= end_age) *
                (0 * x + 0) *
                0.25 }, start_age, end_age)
    linear_rise_sharp_fall_exp_rise <- integrate(function(x) {
        (x <= end_age / 4) * (0 * x + 1.25) * 1 +
            (end_age / 4 < x & x <= end_age / 2) *
                (0 * x + 1.25) *
                0.75 +
            (end_age / 2 < x & x <= end_age * 3 / 4) *
                (1.0606^x * log(1.0606)) *
                0.5 +
            (end_age * 3 / 4 < x & x <= end_age) *
                (1.0606^x * log(1.0606)) *
                0.25 }, start_age, end_age)
    equations <- c(linear_rise_sharp_fall, linear_rise_sharp_fall_exp_rise)
    lapply(equations, eval)
}

# Find sum of first derivative with weighted "end" (ages 60-80), i.e., the very end matters most
my_end_sum <- function(equation) {
    a <- equation(end_age * 3 / 4) - equation(start_age)
    b <- 2 * (equation(end_age) - equation(end_age * 3 / 4))
    c <- a + b

    return(c)
}

# Ditto above fn. with (customized) sum separately for piecewise functions 26 & 27
my_end_sum_D1_2 <- function() {
    linear_rise_sharp_fall <- integrate(function(x) {
        (x <= end_age / 2) * (0 * x + 1.25) +
            (end_age / 2 < x & x <= end_age * 3 / 4) * (0 * x + 0) +
            (end_age * 3 / 4 < x & x <= end_age) *
                (0 * x + 0) *
                2 }, start_age, end_age)
    linear_rise_sharp_fall_exp_rise <- integrate(function(x) {
        (x <= end_age / 2) * (0 * x + 1.25) +
            (end_age / 2 < x & x <= end_age * 3 / 4) * (1.0606^x * log(1.0606)) +
            (end_age * 3 / 4 < x & x <= end_age) *
                (1.0606^x * log(1.0606)) *
                2 }, start_age, end_age)
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
    positive_change_full <- integrate(function(x) { 0 * x + 0 }, start_age, end_age)
    positive_change_partial <- integrate(function(x) { 0 * x + 0 }, start_age, end_age)
    negative_change_full <- integrate(function(x) { 0 * x + 0 }, start_age, end_age)
    negative_change_partial <- integrate(function(x) { 0 * x + 0 }, start_age, end_age)
    linear_rise_sharp_fall <- integrate(function(x) { 0 * x + 0 }, start_age, end_age)
    linear_rise_sharp_fall_exp_rise <- integrate(function(x) {
        (x < end_age / 2) * (0 * x + 0) + (end_age / 2 < x & x <= end_age) * (1.0606^x * log(1.0606) * log(1.0606)) }, start_age, end_age)
    equations <- c(positive_change_full, positive_change_partial, negative_change_full, negative_change_partial,
                   linear_rise_sharp_fall, linear_rise_sharp_fall_exp_rise)
    lapply(equations, eval)
}

#...and when prime years of life weighted more
my_prime_sum_D2_2 <- function() {
    positive_change_full <- integrate(function(x) { 0 * x + 0 }, start_age, end_age)
    positive_change_partial <- integrate(function(x) { 0 * x + 0 }, start_age, end_age)
    negative_change_full <- integrate(function(x) { 0 * x + 0 }, start_age, end_age)
    negative_change_partial <- integrate(function(x) { 0 * x + 0 }, start_age, end_age)
    linear_rise_sharp_fall <- integrate(function(x) { 0 * x + 0 }, start_age, end_age)
    linear_rise_sharp_fall_exp_rise <- integrate(function(x) {
        (x < end_age / 2) * (0 * x + 0) + (end_age / 2 < x & x <= end_age) * (1.0606^x * log(1.0606) * log(1.0606)) }, start_age, end_age)
    equations <- c(positive_change_full, positive_change_partial, negative_change_full, negative_change_partial,
                   linear_rise_sharp_fall, linear_rise_sharp_fall_exp_rise)
    lapply(equations, eval)
}

#... and when later years weighted increasingly more
my_ascending_sum_D2_2 <- function() {
    positive_change_full <- integrate(function(x) { 0 * x + 0 }, start_age, end_age)
    positive_change_partial <- integrate(function(x) { 0 * x + 0 }, start_age, end_age)
    negative_change_full <- integrate(function(x) { 0 * x + 0 }, start_age, end_age)
    negative_change_partial <- integrate(function(x) { 0 * x + 0 }, start_age, end_age)
    linear_rise_sharp_fall <- integrate(function(x) { 0 * x + 0 }, start_age, end_age)
    linear_rise_sharp_fall_exp_rise <- integrate(function(x) {
        (x < end_age / 4) * (0 * x + 0) +
            (end_age / 2 < x & x <= end_age * 3 / 4) *
                (1.0606^x * log(1.0606) * log(1.0606)) *
                0.75 +
            (end_age * 3 / 4 < x & x <= end_age) *
                (1.0606^x * log(1.0606) * log(1.0606)) *
                1 }, start_age, end_age)
    equations <- c(positive_change_full, positive_change_partial, negative_change_full, negative_change_partial,
                   linear_rise_sharp_fall, linear_rise_sharp_fall_exp_rise)
    lapply(equations, eval)
}

#... earlier years weighted more
my_descending_sum_D2_2 <- function() {
    positive_change_full <- integrate(function(x) { 0 * x + 0 }, start_age, end_age)
    positive_change_partial <- integrate(function(x) { 0 * x + 0 }, start_age, end_age)
    negative_change_full <- integrate(function(x) { 0 * x + 0 }, start_age, end_age)
    negative_change_partial <- integrate(function(x) { 0 * x + 0 }, start_age, end_age)
    linear_rise_sharp_fall <- integrate(function(x) { 0 * x + 0 }, start_age, end_age)
    linear_rise_sharp_fall_exp_rise <- integrate(function(x) {
        (x < end_age / 4) * (0 * x + 0) +
            (end_age / 2 < x & x <= end_age * 3 / 4) *
                (1.0606^x * log(1.0606) * log(1.0606)) *
                0.5 +
            (end_age * 3 / 4 < x & x <= end_age) *
                (1.0606^x * log(1.0606) * log(1.0606)) *
                0.25 }, start_age, end_age)
    equations <- c(positive_change_full, positive_change_partial, negative_change_full, negative_change_partial,
                   linear_rise_sharp_fall, linear_rise_sharp_fall_exp_rise)
    lapply(equations, eval)
}

#...final years weighted more
my_end_sum_D2_2 <- function() {
    positive_change_full <- integrate(function(x) { 0 * x + 0 }, start_age, end_age)
    positive_change_partial <- integrate(function(x) { 0 * x + 0 }, start_age, end_age)
    negative_change_full <- integrate(function(x) { 0 * x + 0 }, start_age, end_age)
    negative_change_partial <- integrate(function(x) { 0 * x + 0 }, start_age, end_age)
    linear_rise_sharp_fall <- integrate(function(x) { 0 * x + 0 }, start_age, end_age)
    linear_rise_sharp_fall_exp_rise <- integrate(function(x) {
        (x < end_age / 4) * (0 * x + 0) +
            (end_age / 2 < x & x <= end_age * 3 / 4) * (1.0606^x * log(1.0606) * log(1.0606)) +
            (end_age * 3 / 4 < x & x <= end_age) *
                (1.0606^x * log(1.0606) * log(1.0606)) *
                2 }, start_age, end_age)
    equations <- c(positive_change_full, positive_change_partial, negative_change_full, negative_change_partial,
                   linear_rise_sharp_fall, linear_rise_sharp_fall_exp_rise)
    lapply(equations, eval)
}


## -------------------------------------------------------------------------------------------------------------
# MAIN SCRIPT
## -------------------------------------------------------------------------------------------------------------

### ----- Define global variables
start_age <- 0
end_age <- 90
prime_age_start <- 18 * 9 / 8
prime_age_end <- 30 * 9 / 8
end_y_axis <- 100
cutoff_age <- 45 * 9 / 8

### ----- Create equations
data <- read.csv('./data/lifelines_cleaned_deg68.csv')

# Create R equations of participant enjoyments
create_equation <- function(eqn) {
    eqn <- fromJSON(eqn)

    if (length(eqn) <= 1) { print("error") }
    return(function(x) {
        l <- length(eqn) - 1

        sum <- 0
        count <- 0
        for (p in l:0) {
            count <- count + 1
            sum <- sum + eqn[count] * `^`(x, p)
        }

        return(sum)
    })
}

### Create all equations
eqns = Dict$new(Horror = 0)
titles = c('Horror', 'Adventure', 'Drama', 'Biography', 'Action', 'Fantasy', 'SciFi', 'Animation')
for (title in titles) {
    col <- paste(title, "_equation", sep = "")
    eqns[title] <- apply(data[col], 1, create_equation)

    col_d1 <- paste(title, "_first_derivative", sep = "")
    eqns[paste(title, "_d1", sep = "")] <- apply(data[col_d1], 1, create_equation)
}


### ----- Get equation features and save them into our dataframe
for (title in titles) {
    data[, paste(title, "_min", sep = "")] <- sapply(eqns[title], get_min, start_age, end_age)
    data[, paste(title, "_max", sep = "")] <- sapply(eqns[title], get_max, start_age, end_age)
    data[, paste(title, "_integral", sep = "")] <- sapply(eqns[title], get_integral, end_age)
    data[, paste(title, "_start_value", sep = "")] <- unlist(get_start_value(eqns[title]))
    data[, paste(title, "_end_value", sep = "")] <- unlist(get_end_value(eqns[title], end_age))
    data[, paste(title, "_d1_sum_unweight", sep = "")] <- sapply(eqns[title], my_unweight_sum)
    data[, paste(title, "_d1_sum_weight_prime", sep = "")] <- sapply(eqns[title], my_prime_sum)
    data[, paste(title, "_d1_sum_weight_asc", sep = "")] <- sapply(eqns[title], my_ascending_sum)
    data[, paste(title, "_d1_sum_weight_des", sep = "")] <- sapply(eqns[title], my_descending_sum)
    data[, paste(title, "_d1_sum_weight_end", sep = "")] <- sapply(eqns[title], my_end_sum)
    data[, paste(title, "_d1_avg_unweight", sep = "")] <- data[, paste(title, "_d1_sum_unweight", sep = "")] / end_age
    data[, paste(title, "_d1_avg_weight_prime", sep = "")] <- data[, paste(title, "_d1_sum_weight_prime", sep = "")] / end_age
    data[, paste(title, "_d1_avg_weight_asc", sep = "")] <- data[, paste(title, "_d1_sum_weight_asc", sep = "")] / end_age
    data[, paste(title, "_d1_avg_weight_des", sep = "")] <- data[, paste(title, "_d1_sum_weight_des", sep = "")] / end_age
    data[, paste(title, "_d1_avg_weight_end", sep = "")] <- data[, paste(title, "_d1_sum_weight_end", sep = "")] / end_age

    data[, paste(title, "_d2_sum_unweight", sep = "")] <- sapply(eqns[paste(title, "_d1", sep = "")], my_unweight_sum)
    data[, paste(title, "_d2_sum_weight_prime", sep = "")] <- sapply(eqns[paste(title, "_d1", sep = "")], my_prime_sum)
    data[, paste(title, "_d2_sum_weight_asc", sep = "")] <- sapply(eqns[paste(title, "_d1", sep = "")], my_ascending_sum)
    data[, paste(title, "_d2_sum_weight_des", sep = "")] <- sapply(eqns[paste(title, "_d1", sep = "")], my_descending_sum)
    data[, paste(title, "_d2_sum_weight_end", sep = "")] <- sapply(eqns[paste(title, "_d1", sep = "")], my_end_sum)
    data[, paste(title, "_d2_avg_unweight", sep = "")] <- data[paste(title, "_d2_sum_unweight", sep = "")] / end_age
    data[, paste(title, "_d2_avg_weight_prime", sep = "")] <- data[paste(title, "_d2_sum_weight_prime", sep = "")] / end_age
    data[, paste(title, "_d2_avg_weight_asc", sep = "")] <- data[paste(title, "_d2_sum_weight_asc", sep = "")] / end_age
    data[, paste(title, "_d2_avg_weight_des", sep = "")] <- data[paste(title, "_d2_sum_weight_des", sep = "")] / end_age
    data[, paste(title, "_d2_avg_weight_end", sep = "")] <- data[paste(title, "_d2_sum_weight_end", sep = "")] / end_age
}

write.csv(data, './data/lifelines_w_features.csv')

## END -------------------------------------------------------------------------------------------------------------------
