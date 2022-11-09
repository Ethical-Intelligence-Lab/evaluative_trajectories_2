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
