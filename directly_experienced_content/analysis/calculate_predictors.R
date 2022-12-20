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
get_min <- function(equation, start_x, end_x) {
    answer <- optimize(equation, interval = c(start_x, end_x))
    return(answer$objective[1])
}

get_max <- function(equation, start_x, end_x) {
    answer <- optimize(equation, interval = c(start_x, end_x), maximum = TRUE)
    return(answer$objective[1])
}

# Integral
get_integral <- function(equation, end_x) {
    answer <- integrate(equation, start_x, end_x)
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
get_end_value <- function(equations, end_x) {
    end_value <- list()
    for (i in 1:length(equations)) {
        options(scipen = 999)
        end_value[[i]] <- equations[[i]](end_x)

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
    a <- equation(end_x) - equation(start_x)
    return(a)
}

# Find sum of first derivative with weighted "prime years of life" (ages 18-30)
my_early_sum <- function(equation) {
    a <- equation(early_age_x) - equation(start_x)
    b <- 2 * (equation(early_age_x) - equation(early_age_x))
    c <- equation(end_x) - equation(early_age_x)
    d <- a + b + c

    return(d)
}

# Find sum of first derivative with ascending weights (0.25, 0.5, 0.75, and 1), i.e.,
# later yrs. matter more
my_ascending_sum <- function(equation) {
    a <- 0.25 * (equation(end_x / 4) - equation(start_x))
    b <- 0.5 * (equation(end_x / 2) - equation(end_x / 4))
    c <- 0.75 * (equation(end_x * 3 / 4) - equation(end_x / 2))
    d <- 1 * (equation(end_x) - equation(end_x * 3 / 4))
    e <- a + b + c + d
    return(e)
}

# Find sum of first derivative with descending weights (1, 0.75, 0.5, and 0.25)
# i.e., earlier years matter more
my_descending_sum <- function(equation) {
    a <- 1 * (equation(end_x / 4) - equation(start_x))
    b <- 0.75 * (equation(end_x / 2) - equation(end_x / 4))
    c <- 0.5 * (equation(end_x * 3 / 4) - equation(end_x / 2))
    d <- 0.25 * (equation(end_x) - equation(end_x * 3 / 4))
    e <- a + b + c + d

    return(e)
}

# Find sum of first derivative with weighted "end" (ages 60-80), i.e., the very end matters most
my_end_sum <- function(equation) {
    a <- equation(end_x * 3 / 4) - equation(start_x)
    b <- 2 * (equation(end_x) - equation(end_x * 3 / 4))
    c <- a + b

    return(c)
}

## -------------------------------------------------------------------------------------------------------------
# MAIN SCRIPT
## -------------------------------------------------------------------------------------------------------------

### ----- Define global variables
start_x <- 0
end_x <- 90
early_age_x <- 18 * 9 / 8
early_age_x <- 30 * 9 / 8
end_y_axis <- 100

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
    data[, paste(title, "_min", sep = "")] <- sapply(eqns[title], get_min, start_x, end_x)
    data[, paste(title, "_max", sep = "")] <- sapply(eqns[title], get_max, start_x, end_x)
    data[, paste(title, "_integral", sep = "")] <- sapply(eqns[title], get_integral, end_x)
    data[, paste(title, "_start_value", sep = "")] <- unlist(get_start_value(eqns[title]))
    data[, paste(title, "_end_value", sep = "")] <- unlist(get_end_value(eqns[title], end_x))
    data[, paste(title, "_d1_sum_unweight", sep = "")] <- sapply(eqns[title], my_unweight_sum)
    data[, paste(title, "_d1_sum_weight_prime", sep = "")] <- sapply(eqns[title], my_early_sum)
    data[, paste(title, "_d1_sum_weight_asc", sep = "")] <- sapply(eqns[title], my_ascending_sum)
    data[, paste(title, "_d1_sum_weight_des", sep = "")] <- sapply(eqns[title], my_descending_sum)
    data[, paste(title, "_d1_sum_weight_end", sep = "")] <- sapply(eqns[title], my_end_sum)
    data[, paste(title, "_d1_avg_unweight", sep = "")] <- data[, paste(title, "_d1_sum_unweight", sep = "")] / end_x
    data[, paste(title, "_d1_avg_weight_prime", sep = "")] <- data[, paste(title, "_d1_sum_weight_prime", sep = "")] / end_x
    data[, paste(title, "_d1_avg_weight_asc", sep = "")] <- data[, paste(title, "_d1_sum_weight_asc", sep = "")] / end_x
    data[, paste(title, "_d1_avg_weight_des", sep = "")] <- data[, paste(title, "_d1_sum_weight_des", sep = "")] / end_x
    data[, paste(title, "_d1_avg_weight_end", sep = "")] <- data[, paste(title, "_d1_sum_weight_end", sep = "")] / end_x

    data[, paste(title, "_d2_sum_unweight", sep = "")] <- sapply(eqns[paste(title, "_d1", sep = "")], my_unweight_sum)
    data[, paste(title, "_d2_sum_weight_prime", sep = "")] <- sapply(eqns[paste(title, "_d1", sep = "")], my_early_sum)
    data[, paste(title, "_d2_sum_weight_asc", sep = "")] <- sapply(eqns[paste(title, "_d1", sep = "")], my_ascending_sum)
    data[, paste(title, "_d2_sum_weight_des", sep = "")] <- sapply(eqns[paste(title, "_d1", sep = "")], my_descending_sum)
    data[, paste(title, "_d2_sum_weight_end", sep = "")] <- sapply(eqns[paste(title, "_d1", sep = "")], my_end_sum)
    data[, paste(title, "_d2_avg_unweight", sep = "")] <- data[paste(title, "_d2_sum_unweight", sep = "")] / end_x
    data[, paste(title, "_d2_avg_weight_prime", sep = "")] <- data[paste(title, "_d2_sum_weight_prime", sep = "")] / end_x
    data[, paste(title, "_d2_avg_weight_asc", sep = "")] <- data[paste(title, "_d2_sum_weight_asc", sep = "")] / end_x
    data[, paste(title, "_d2_avg_weight_des", sep = "")] <- data[paste(title, "_d2_sum_weight_des", sep = "")] / end_x
    data[, paste(title, "_d2_avg_weight_end", sep = "")] <- data[paste(title, "_d2_sum_weight_end", sep = "")] / end_x
}

write.csv(data, './data/data_w_features.csv')

## END -------------------------------------------------------------------------------------------------------------------
