# Analysis script for 'Evaluative Summaries'
# Lifelines

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #set working directory to current directory

# Import libraries
if (!require(pacman)) { install.packages("pacman") }
pacman::p_load('data.table', #rename data frame columns 
               'ggplot2', #plot stuff
               'ggpubr', #customize plots 
               'gtools', #sorts files and places them into numerical order
               'cowplot', #reads image files into R; add images as x-axis labels
               'magick', #image processing
               'ggridges', #image processing
               'png', #read PNG files
               'SnowballC', #text stemming
               'ggwordcloud', #make word clouds (using ggplot)
               'grid', #raster
               'gridGraphics', #make grids
               'gridExtra', #make grids
               'sentimentr', #sentiment analysis
               'tm', #text mining 
               'wordcloud', #visualize wordclouds for topic models 
               #'ldatuning', #find number of topics in topic models 
               'lme4', #run mixed effects linear regression
               'lmerTest', #used in conjunction with lme4; get p-values
               'robustHD', #for the standardize function
               'corrplot', #for corrplot()
               'plotrix', #for std.error()
               'psych', #for principal components analysis (PCA)
               'glmnet', #for ridge (L2) regression
               'lmtest', #for likelihood ratio test 
               'recipes', #for feature engineering
               'caret', #for automating tuning process
               'tidyr', #for gather(), which takes multiple columns and collapses them into key-value pairs
               'tidyverse', #used in conjunction with tidyr; contains dplyr, used for select(); load last because of conflict!
               'slam', #utility functions for sparse matrices 
               'broom' #install separately if does not work
)

source('../../tools/common_functions.R')
source('./plotting.R')

##================================================================================================================
##FUNCTIONS FOR PREPROCESSING##
##================================================================================================================

PerformExclusions <- function(data) {
    "
    Excludes participants if they do not finish the survey, finished it too quickly (under 120 seconds), 
    gave duplicate answers, or failed important attention and comprehension checks.
    Input: data   #num_rows = num_ss
    Output: data after it has been 'cleaned'
    "

    # Exclude those who did not finish the survey
    data <- subset(data, (data$Finished == TRUE))
    n_before_exclusions <- dim(data)[1] #203

    # Exclude those who finished it in less than 2 minutes
    data <- subset(data, (data$Duration..in.seconds. > 120))

    # Exclude those who gave the same answers to meaningfulness and personal desirability questions 
    meaning_cols <- data[, grep("meaning", colnames(data), value = TRUE)]
    meaning_dups <- meaning_cols[apply(meaning_cols, 1, function(x) length(unique(x[!is.na(x)])) == 1),]

    pd_cols <- data[, grep("preference", colnames(data), value = TRUE)]
    pd_dups <- pd_cols[apply(pd_cols, 1, function(x) length(unique(x[!is.na(x)])) == 1),]

    #(1) attention checks
    #round #1
    #att_check_1 = Who is taller: John or Paul?
    #att_check_2 = What color is grass (please say it's purple)
    #round #2
    #att_check_3_1: place slider at specific number
    #att_check_3_2: place slider at specific number
    #att_check_3_3: place slider at specific number
    #att_check_4: how many fatal heart attacks

    # Perform first round of attention checks
    data$attention_check <- ifelse(((data$att_check_1 == 'Paul') &
        (data$att_check_2 == 'Purple')), 0, 1)

    # Perform second round of attention checks, if they failed the first
    data$attention_check <- ifelse(((is.na(data$att_check_3_1 == TRUE)) |
        ((data$att_check_4 == 0) &
            (data$att_check_3_3 > data$att_check_3_2) &
            (data$att_check_3_2 > data$att_check_3_1) &
            (data$att_check_3_2 %% 10 == 0) &
            (data$att_check_3_1 == 15))), 0, 1)

    print(paste('percentage excluded, attention checks: ',
                table(data$attention_check)[2] / length(data$attention_check)))

    # Perform comprehension checks
    data$attention_check2 <- ifelse((data$comp_check_1 == 80 &
        data$comp_check_2 == 0 &
        data$comp_check_3 == 'They were highly unstressed early in life, then highly stressed later in life'
                                    ), 0, 1)

    #(2) comprehension questions
    #round #1
    #comp_check_1: how old was person when most stressed
    #comp_check_2: how stressed when they were 20 years old
    #comp_check_3: which is true of the life of the person above
    #round #2
    #comp_checks_4-6: same questions as above
    #comp_7: What was labeled on y-axis
    #comp_8: What was labeled on x-axis
    #comp_9: What question were you asked about the plot

    #Perform second round of comprehension checks, if they failed the first
    data$comp_check <- ifelse(((is.na(data$comp_check_4 == TRUE))
        &
        (data$comp_check_7 == 'Happiness') &
        (data$comp_check_8 == 'Age') &
        (data$comp_check_9 == 'Meaningfulness')
        |
        ((data$comp_check_4 == 0) &
            (data$comp_check_5 == 80)
            &
            (data$comp_check_6 == 'They were highly stressed early in life, then highly unstressed later in life') &
            (data$comp_check_7 == 'Happiness') &
            (data$comp_check_8 == 'Age') &
            (data$comp_check_9 == 'Meaningfulness')
        )), 0, 1)

    print(paste('percentage excluded, comprehension checks: ',
                table(data$comp_check)[2] / length(data$comp_check)))

    # Exclude those who failed either attention or comprehension checks
    data <- subset(data, (data$attention_check == 0) & (data$comp_check == 0))

    # Number of subjects after exclusions
    n_after_exclusions <- dim(data)[1] #124
    print(paste('percentage excluded, duplicate answers: ',
                (dim(meaning_dups)[1] + dim(pd_dups)[1]) / n_after_exclusions))
    print(paste('total percentage excluded, comprehension checks: ',
                (n_before_exclusions - n_after_exclusions) / n_before_exclusions))

    data$n_after_exclusions <- n_after_exclusions

    return(data)
}


Preprocess <- function(data, n_plots, plot_names) {
    " 
    Since each plot is shown within-subjects, Qualtrics spits out data in wide format
    Let's convert it to long format, so that we have a row for every plot type
    Input: dataframe with number of rows = n_subjects
    Output: dataframe with number of rows = n_subjects*n_plot_types (=)
    "

    # Define new data frame that we'll extract preprocessed data into

    # Define row and column names
    data_subset <- 35:115
    last_cols <- 116:119

    column_names <- c('plot_names', 'meaningfulness', 'personal_desirability', 'word', 'subject')

    df <- array(0, dim = c((nrow(data) * n_plots), length(column_names)))
    df <- as.data.frame(df, stringsAsFactors = FALSE)
    colnames(df) <- column_names

    # Turning wide format into long format, then inserting the answers into the 'df' dataframe
    final_data <- as.data.frame(t(data[data_subset])) #switch rows and columns in preparation for 'gather,' which collects info by columns
    long_data <- gather(final_data, key = "subject", value = "answers")["answers"] #gather the answers of the subjects into one long column 

    for (i in 1:dim(long_data)[2]) {
        df[1] <- plot_names
        df[2] <- long_data[seq(1, nrow(long_data), 3),]
        df[3] <- long_data[seq(2, nrow(long_data), 3),]
        df[4] <- long_data[seq(3, nrow(long_data), 3),]
        df[5] <- rep(1:dim(data)[1], each = n_plots)
    }

    # Merge good data with first and last halves of the original data
    data <- cbind(data[rep(seq_len(nrow(data)), each = n_plots), 1:n_plots], df, data[rep(seq_len(nrow(data)), each = n_plots), last_cols])

    return(data)
}


ProcessForPlots <- function(data, n_plots, plot_names) {
    "
    Create a new data frame to store the meaningfulness and PD scores by ascending meaningfulness scores
    Input: data_long, n_plots, plot_names   #num_rows = num_ss*num_plots 
    Output: data_plot_long (in order of ascending meaningfulness scores)   #num_rows = num_plots*num_questions
    "

    # Get mean scores for all questions, then reshape data from wide to long format
    stats <- Get_stats(data, n_plots)
    data_plot <- data.frame(plot_names = plot_names,
                            meaning_score_avg = unlist(stats)[c(TRUE, FALSE, FALSE, FALSE)],
                            meaning_score_sd = unlist(stats)[c(FALSE, TRUE, FALSE, FALSE)],
                            pd_score_avg = unlist(stats)[c(FALSE, FALSE, TRUE, FALSE)],
                            pd_score_sd = unlist(stats)[c(FALSE, FALSE, FALSE, TRUE)])
    data_plot_sorted <- data_plot[order(data_plot$meaning_score_avg),] #order by meaningfulness
    data_plot_long <- gather(data_plot_sorted, key = question_type, #create separate entries for each question type, i.e., num_plots*num_questions 
                             value = score, meaning_score_avg, pd_score_avg)

    # Compile all standard deviation values
    stan_dev <- gather(data_plot_sorted, key = question_type,
                       value = sd, meaning_score_sd, pd_score_sd)

    # Bind the SE column to the rest of the dataframe
    data_plot_long <- cbind(dplyr::select(data_plot_long, plot_names, question_type, score), sd = stan_dev$sd)

    data_plot_long$plot_names <- factor(data_plot_long$plot_names, levels = c("linear_low", "linear_rise_sharp_fall", "linear_fall",
                                                                              "exp_fall_convex", "logistic_fall", "sin_rf_partial",
                                                                              "positive_change_partial", "linear_middle", "positive_change_full",
                                                                              "exp_fall_concave", "sin_rf_full", "sin_frf_partial",
                                                                              "sin_frf_full", "sin_rfrf", "negative_change_full",
                                                                              "sin_rfr_partial", "sin_fr_full", "sin_frfr",
                                                                              "sin_rfr_full", "linear_rise_sharp_fall_exp_rise", "exp_rise_convex",
                                                                              "logistic_rise", "negative_change_partial", "sin_fr_partial",
                                                                              "linear_rise", "exp_rise_concave", "linear_high"))
    return(data_plot_long)
}


Get_stats <- function(data, n_plots) {
    " Find meaningfulness and personal desirability means and standard deviations for every plot
      Every plot repeats every 27 times, since there are 27 plots title. 
      Hence the 'seq' indeces for each calculation
      Input: data_long, n_plots
      Output: equations (a list of means and standard deviations of meaning and pd scores for every plot)
    "
    meaning_score <- as.numeric(data$meaningfulness)
    pd_score <- as.numeric(data$personal_desirability)

    equations <- c()
    for (i in 1:n_plots) {
        equations[[i]] <- c(mean(meaning_score[seq(i, length(meaning_score), n_plots)]), sd(meaning_score[seq(i, length(meaning_score), n_plots)]),
                            mean(pd_score[seq(i, length(meaning_score), n_plots)]), sd(pd_score[seq(i, length(meaning_score), n_plots)]))
    }

    return(equations)
}

##================================================================================================================
##FUNCTIONS FOR PLOTTING SENTIMENT BAR PLOT##
##================================================================================================================

Get_sentiment_stats <- function(data, n_plots) {
    "
    Find sentiment score means and standard deviations for every plot 
    Input: data_long, n_plots 
    Output: sentiment_list (a list of average mean and standard deviation sentiment scores for every plot)
    "

    # Clean words 
    word_clean <- word(tolower(data$word), 1) #make all words lowercase, and collect only the first word of a given sentence
    word_gen <- gsub("[^a-z]", "", word_clean) #get rid of numbers and special characters, leaving only letters a-z

    # Organize words by plot 
    equations <- c()
    for (i in 1:n_plots) {
        equations[[i]] <- word_gen[seq(i, length(word_gen), n_plots)]
    }

    # Get sentiment score means and standard deviations for every plot 
    sentiment_list <- c()
    for (i in 1:n_plots) {
        sentiment_list[[i]] <- c(mean(sentiment_by(equations[[i]])$ave_sentiment),
                                 sd(sentiment_by(equations[[i]])$ave_sentiment))
    }

    return(sentiment_list)
}


CreateSentimentDataframe <- function(data, n_plots, plot_names) {
    "
    Make a dataframe with sentiment scores for every plot, which we will use later to add to dat_final 
    Input: data_long, n_plots, plot_names 
    Output: sentiment_df (will be important later on when we bind it to dat for analysis)
    "

    sentiment_stats <- Get_sentiment_stats(data, n_plots)
    sentiment_df <- data.frame(plot_names = plot_names,
                               mean = unlist(sentiment_stats)[c(TRUE, FALSE)],
                               sd = unlist(sentiment_stats)[c(FALSE, TRUE)])

    return(sentiment_df)
}


OrderSentimentDataframe <- function(data, n_plots, plot_names) {
    "
    Create a new data frame to store the sentiment scores by ascending meaningfulness scores 
    Input: data_long, n_plots, plot_names 
    Output: sentiment_df_sorted (the sentiment_df ordered by levels in the function factor())
    "

    # Get the order of meaningfulness scores
    stats <- Get_stats(data, n_plots)
    data_plot <- data.frame(plot_names = plot_names,
                            meaning_score_avg = unlist(stats)[c(TRUE, FALSE, FALSE, FALSE)],
                            meaning_score_sd = unlist(stats)[c(FALSE, TRUE, FALSE, FALSE)],
                            pd_score_avg = unlist(stats)[c(FALSE, FALSE, TRUE, FALSE)],
                            pd_score_sd = unlist(stats)[c(FALSE, FALSE, FALSE, TRUE)])

    # Create sentiment data frame ordered by ascending meaningfulness scores
    sentiment_stats <- Get_sentiment_stats(data, n_plots)
    sentiment_df <- data.frame(plot_names = plot_names,
                               mean = unlist(sentiment_stats)[c(TRUE, FALSE)],
                               sd = unlist(sentiment_stats)[c(FALSE, TRUE)])
    sentiment_df_sorted <- sentiment_df[order(data_plot$meaning_score_avg),]
    sentiment_df_sorted$plot_names <- factor(sentiment_df_sorted$plot_names, levels = c("linear_low", "linear_rise_sharp_fall", "linear_fall",
                                                                                        "exp_fall_convex", "logistic_fall", "sin_rf_partial",
                                                                                        "positive_change_partial", "linear_middle", "positive_change_full",
                                                                                        "exp_fall_concave", "sin_rf_full", "sin_frf_partial",
                                                                                        "sin_frf_full", "sin_rfrf", "negative_change_full",
                                                                                        "sin_rfr_partial", "sin_fr_full", "sin_frfr",
                                                                                        "sin_rfr_full", "linear_rise_sharp_fall_exp_rise", "exp_rise_convex",
                                                                                        "logistic_rise", "negative_change_partial", "sin_fr_partial",
                                                                                        "linear_rise", "exp_rise_concave", "linear_high"))

    return(sentiment_df_sorted)
}




##================================================================================================================
##FUNCTIONS FOR ANALYSIS##
##================================================================================================================
GetMainEffects <- function(data, data_long, n_plots, plot_names, my_embeddings) {
    "
    This function gets various correlations and main effects of the participant data.
    Input: This function takes as input a dataframe with rows = num_ss*num_plots*num_questions.
    It also takes data_long (dataframe of averages of question ratings), n_plots (number of plots), and my_embeddings.
    Output: various correlation and linear regression results; also, linear and quadratic plots ordered by meaningfulness scores
    "

    data$plot_type_n <- as.numeric(factor(data$plot_names)) #create numeric version of plot_names
    data$score_n <- as.numeric(data$score) #create numeric version of score (which are characters)
    data$question_type_n <- as.numeric(factor(data$question_type))
    data$subject_n <- as.numeric(factor(data$subject))

    print('Did answers vary depending on question and plot type?')
    effect_mod <- lm(data$score_n ~ data$question_type_n * data$plot_type_n + (1 | data$subject_n))
    print(summary(effect_mod))
    print('-----------------------------------------------------')

    print('Which question type scored higher?')
    t_mod <- t.test(data$score_n ~ data$question_type, paired = TRUE)
    print(t_mod)
    print(paste('Means: ', unique(data$question_type), ': ', tapply(data$score_n, data$question_type, mean)))
    print(paste('SDs: ', unique(data$question_type), ': ', tapply(data$score_n, data$question_type, sd)))
    print('-----------------------------------------------------')

    # Get the order of average meaningfulness scores
    stats <- Get_stats(data_long, n_plots)
    data_plot <- data.frame(plot_names = plot_names,
                            meaning_score_avg = unlist(stats)[c(TRUE, FALSE, FALSE, FALSE)],
                            meaning_score_sd = unlist(stats)[c(FALSE, TRUE, FALSE, FALSE)],
                            pd_score_avg = unlist(stats)[c(FALSE, FALSE, TRUE, FALSE)],
                            pd_score_sd = unlist(stats)[c(FALSE, FALSE, FALSE, TRUE)])
    data_plot <- data_plot[order(data_plot$meaning_score_avg),]
    data_plot$order_num <- 1:n_plots

    # Add a column of the difference between the average ratings of meaningfulness and personal desirability for each lifeline
    data_plot["meaning_pd_diff"] <- data_plot["meaning_score_avg"] - data_plot["pd_score_avg"]
    meaning_score_avg <- data_plot[, "meaning_score_avg"]
    pd_score_avg <- data_plot[, "pd_score_avg"]
    meaning_pd_diff <- data_plot[, "meaning_pd_diff"]

    print('Does a quadratic regression fit the shape of the difference between meaning and desirability ratings better than a linear one does?')
    print('First, the linear fit:')
    meaning_pd_diff_lin <- lm(meaning_pd_diff ~ data_plot$order_num)
    print(summary(meaning_pd_diff_lin))
    linear_plot <- ggplot(meaning_pd_diff_lin, aes(data_plot$order_num, meaning_pd_diff)) +
        theme_classic() +
        geom_point() +
        stat_smooth(method = lm, formula = y ~ x) +
        theme(axis.text = element_text(color = "black", size = 20),
              axis.title.y = element_blank(),
              axis.title.x = element_blank())
    print('-----------------------------------------------------')

    print('Second, the quadratic fit:')
    meaning_pd_diff_quadratic <- lm(meaning_pd_diff ~ data_plot$order_num + I(data_plot$order_num^2))
    print(summary(meaning_pd_diff_quadratic))
    quadratic_plot <- ggplot(meaning_pd_diff_quadratic, aes(data_plot$order_num, meaning_pd_diff)) +
        theme_classic() +
        geom_point() +
        stat_smooth(method = lm, formula = y ~ poly(x, 2, raw = TRUE)) +
        theme(axis.text = element_text(color = "black", size = 20),
              axis.title.y = element_blank(),
              axis.title.x = element_blank())
    print('-----------------------------------------------------')

    print('Difference between linear and quadratic models:')
    meaning_pd_diff_lrt <- lrtest(meaning_pd_diff_lin, meaning_pd_diff_quadratic)
    print(meaning_pd_diff_lrt)
    print('-----------------------------------------------------')

    linear_quad <- ggarrange(linear_plot, quadratic_plot, nrow = 1, ncol = 2)
    # theme(plot.margin = margin(0.1,0.1,-0.5,0.1, "cm"))
    linear_quad <- annotate_figure(linear_quad,
                                   left = text_grob("Difference Between Meaning and Desirability", color = "black", face = "bold", size = 20, rot = 90),
                                   bottom = text_grob("Plot Ordered by Meaningfulness Scores", color = "black", face = "bold", size = 20, vjust = 0.4))
    print(linear_quad)

    print('Regress meaningfulness on embeddings: ')
    meaning_embedding_df <- cbind(meaningfulness = data_long$meaningfulness, my_embeddings[2:513])
    meaning_embedding_lm <- lm(meaningfulness ~ ., data = meaning_embedding_df)
    #print('meaningfulness vs. embeddings:')
    #print(summary(meaning_embedding_lm))
    # Error: 486 not defined because of singularities
    # I checked for multicollinearity with cor(my_embeddings[2:513]) but did not find any perfect correlations 
    # (except for of course the correlation of a given variable with itself, which had a coefficient of 1).

    print('Regress personal desirability on embeddings: ')
    pd_embedding_df <- cbind(personal_desirability = data_long$personal_desirability, my_embeddings[2:513])
    pd_embedding_lm <- lm(personal_desirability ~ ., data = pd_embedding_df)
    #print('personal desirability vs. embeddings:')
    #print(summary(pd_embedding_lm)) 
    # Same error as above

    # Return plots 
    # plot_list <- list(linear_plot, quadratic_plot) 

    return(linear_quad)

}


CreateDataFeaturesDF <- function(data, dat_final, features_df, n_after_exclusions, num_subjects_and_plots) {
    "
    Bind the three dataframes: data, sentiment score, and standardize(features), i.e., the standardized plot features.
    Input: data_long, dat_final, features, n_after_exclusions, num_subjects_and_plots
    Output: score_features_df (which contains all of the predictors and participant scores)
    "

    score_features_df <- cbind(data, sentiment_score = dat_final$sentiment_score[1:num_subjects_and_plots],
                               as.data.frame(do.call("rbind", replicate(n_after_exclusions, standardize(features_df), simplify = FALSE))))
    score_features_df["meaningfulness"] <- as.data.frame(apply(score_features_df["meaningfulness"], 2, as.numeric))
    score_features_df["personal_desirability"] <- as.data.frame(apply(score_features_df["personal_desirability"], 2, as.numeric))
    score_features_df["subject"] <- as.data.frame(apply(score_features_df["subject"], 2, as.numeric))
    score_features_df["plot_names"] <- as.data.frame(as.numeric(factor(score_features_df$plot_names)))
    score_features_df["meaningfulness"] <- standardize(score_features_df["meaningfulness"])
    score_features_df["personal_desirability"] <- standardize(score_features_df["personal_desirability"])
    score_features_df["sentiment_score"] <- standardize(score_features_df["sentiment_score"])
    score_features_df["embeddings"] <- standardize(score_features_df["embeddings"])
    score_features_df["interestingness"] <- standardize(score_features_df["interestingness"])

    return(score_features_df)

}


Get_spearman_brown_correction <- function(cor_value) {
    "
    Adjusting correlation value with the Spearman-Brown prophecy formula: (2 * r) / (1 + r)
    Input: cor_value (any correlation value)
    Output: cor_value_adj (the adjusted correlation value from the formula)
    "

    cor_value_adj <- (2 * cor_value) / (1 + cor_value)

    return(cor_value_adj)
}


Get_noise_ceiling <- function(dat_long, question_type, n_ss) {
    "
    Find correlation values between two randomly sample halves of the data, 
    correct with the Spearman-Brown Prophecy formula (defined above), and put into a list.
    Input: data_long, question type ('meaningfulness' or 'personal_desirability')
    Output: summary of the correlation results, to be used to plot noise ceiling (25th and 75th percentiles)
    "

    # Convert "meaningfulness" and "personal_desirability" columns into numeric
    dat_long[, c("meaningfulness", "personal_desirability")] <- sapply(dat_long[, c("meaningfulness", "personal_desirability")], as.numeric)

    # Filter the scores using the "subject" column, and put into a list
    list_of_question <- c()
    for (i in plot_names) {
        list_of_question[i] <- dat_long %>%
            filter(plot_names == i) %>%
            select(question_type)
    }

    # Convert to data frame 
    df_of_question <- data.frame(list_of_question)

    # Find correlation values between two randomly-sampled halves of the data, 
    # correct with the Spearman-Brown Prophecy formula (defined above), and put into a list. 

    # Divide number of participants in half 
    half_n_ss <- n_ss / 2

    # If even number of participants:  
    if (half_n_ss == round(half_n_ss)) {
        coded <- c(rep(1, half_n_ss), rep(2, half_n_ss))
    }

    # If odd number of participants: 
    if (half_n_ss != round(half_n_ss)) {
        coded <- c(rep(1, half_n_ss + 0.5), rep(2, half_n_ss - 0.5))
    }

    # Create a list to store the simulations 
    sims <- 1000
    store <- rep(NA, sims)
    set.seed(2)

    # Perform 1000 correlations 
    for (i in 1:sims) {

        # Get random samples
        rand_assign <- sample(coded, n_ss, FALSE) #randomly assign rows as either 1 or 2
        assign_1 <- df_of_question[rand_assign == 1,] #random sample 1
        assign_2 <- df_of_question[rand_assign == 2,] #random sample 2
        means_1 <- colMeans(assign_1) #get the means of random sample 1
        means_2 <- colMeans(assign_2) #get the means of random sample 2

        # Perform correlations on the means of both random samples
        store[i] <- cor(means_1, means_2)
    }

    # Apply Spearman-Brown correction: cor_value_adj <- (2 * cor_value) / (1 + cor_value) 
    corrected_store <- Get_spearman_brown_correction(store)
    store_summary <- summary(corrected_store)

    return(store_summary)
}

MakePCAFunction <- function(score_features_df) {
    "
    Perform mixed-effects regression based on PCA-reduced features of our predictors. 
    Input: score_features_df 
    Output: the structure of the PCA fit, the PCA correlation values for both meaning and pd 
    scores, and score_features_df (now with the addition of PC1 through PC5 scores)
    "

    # Define the columns that we want for the PCA: from embeddings to integral and the D1 & D2 predictors.
    score_features_ss <- subset(score_features_df, select = c(embeddings:integral, d1_avg_unweight:d1_avg_weight_end, d2_avg_unweight:d2_avg_weight_end))

    # Fit the PCA
    my_PCA <- principal(score_features_ss, 5, rotate = "promax")
    print(my_PCA)
    colnames(my_PCA$Structure) <- c("PC1", "PC2", "PC3", "PC4", "PC5")
    colnames(my_PCA$scores) <- c("PC1", "PC2", "PC3", "PC4", "PC5")

    # Print, then save the features corrplot
    corrplot(my_PCA$Structure, method = "circle", mar = c(0, 0, 2, 0))
    mtext("Features Correlation Matrix \nover PCs", at = 1, line = 1, cex = 1.5)
    pdf(file = "features_corrplot.pdf")
    corrplot(my_PCA$Structure, method = "circle", mar = c(0, 0, 4, 0))
    mtext("Features Correlation Matrix \nover PCs", at = 1, line = 1, cex = 1.5)
    dev.off()

    print('principal components by features')
    print(my_PCA$Structure)

    # Bind the PC1 through PC5 scores to the score_features_df data frame
    score_features_df <- cbind(score_features_df, my_PCA$scores)

    # 1. Fit mixed effects regression predicting meaningfulness
    meaning_features <- lmer(data = score_features_df,
                             meaningfulness ~ PC1 +
                                 PC2 +
                                 PC3 +
                                 PC4 +
                                 PC5 +
                                 (1 | subject) +
                                 (1 | plot_names))

    print('meaningfulness vs. features:')
    print(summary(meaning_features, correlation = TRUE))


    # 2. Fit mixed effects regression predicting personal desirability
    pd_features <- lmer(data = score_features_df,
                        personal_desirability ~ PC1 +
                            PC2 +
                            PC3 +
                            PC4 +
                            PC5 +
                            (1 | subject) +
                            (1 | plot_names))

    print('personal desirability vs. features:')
    print(summary(pd_features, correlation = TRUE))

    return(score_features_df)

}


CrossValidationAnalysisWtPCs <- function(dat, dat_long, n_ss, n_plots) {
    "
    Measure the performance of each of our PCs by doing cross-validated regressions, holding out 
    one participant for each cross-validation step. 
    Input: data_wt_PCs, data_long, n_after_exclusions, n_plots
    Output: relative importance of principal components and its graph
    "

    set.seed(1)
    pcs <- c('PC1', 'PC2', 'PC3', 'PC4', 'PC5')
    n_folds <- n_ss
    folds <- cut(seq(1, nrow(dat)), breaks = n_folds, labels = FALSE)
    folds2 <- rep(seq(1, n_plots), times = n_folds) #plot x subjects folds
    indeces <- seq(1, (n_plots * n_folds))

    #-------------------------------------------------------------------------------------------------------------------

    #1. Meaningfulness 
    results_meaning <- data.frame(matrix(NA, nrow = length(pcs), ncol = n_folds))
    rownames(results_meaning) <- pcs

    for (i in 1:length(pcs)) {
        for (j in 1:n_folds) {
            ss_results <- c()
            truths <- c()

            for (k in 1:n_plots) {
                trainIndeces <- indeces[(folds == j) & (folds2 != k)]
                testIndeces <- indeces[(folds == j) & (folds2 == k)]
                fitpc <- lm(meaningfulness ~ get(pcs[i]), data = dat, subset = trainIndeces) #fit model on subset of train data
                ss_results <- c(ss_results, predict(fitpc, dat)[testIndeces])
                truths <- c(truths, dat$meaningfulness[testIndeces])
            }

            results_meaning[i, j] <- cor(truths, ss_results)
        }

        print(paste('meaningfulness: mean PC result,', pcs[i], ': ', mean(as.numeric(results_meaning[i,]), na.rm = TRUE)))
    }

    # Reorder pcs according to their significance 
    t_results_meaning <- as.data.frame(t(results_meaning))
    colnames(t_results_meaning) <- c("PC1\nFirst Derivative\nPredictors and End Value", "PC2\nSecond Derivative\nPredictors",
                                     "PC3\nFluctuations, Embeddings,\nand Interestingness", "PC4\nIntegral, Sentiment,\nMax, and Min",
                                     "PC5\nNumber of Peaks\nand Extrema")
    results_meaning_long <- gather(t_results_meaning, key = principal_components, value = pcs_results, colnames(t_results_meaning)) #length(pcs)*n_folds
    meaning_new_order <- with(results_meaning_long, reorder(principal_components, pcs_results, mean, na.rm = TRUE))
    results_meaning_long["meaning_new_order"] <- meaning_new_order

    # Get_noise_ceiling function
    summary_meaning <- Get_noise_ceiling(dat_long, "meaningfulness", n_ss)

    #-------------------------------------------------------------------------------------------------------------------

    #2. Personal Desirability
    results_pd <- data.frame(matrix(NA, nrow = length(pcs), ncol = n_folds))
    rownames(results_pd) <- pcs

    for (i in 1:length(pcs)) {
        for (j in 1:n_folds) {
            ss_results <- c()
            truths <- c()

            for (k in 1:n_plots) {
                trainIndeces <- indeces[(folds == j) & (folds2 != k)]
                testIndeces <- indeces[(folds == j) & (folds2 == k)]
                fitpc <- lm(personal_desirability ~ get(pcs[i]), data = dat, subset = trainIndeces) #fit model on subset of train data
                ss_results <- c(ss_results, predict(fitpc, dat)[testIndeces])
                truths <- c(truths, dat$personal_desirability[testIndeces])
            }

            results_pd[i, j] <- cor(truths, ss_results)
        }

        print(paste('personal desirability: mean PC result,', pcs[i], ': ', mean(as.numeric(results_pd[i,]), na.rm = TRUE)))
    }

    # Reorder pcs according to their significance 
    t_results_pd <- as.data.frame(t(results_pd))
    colnames(t_results_pd) <- c("PC1\nFirst Derivative\nPredictors and End Value", "PC2\nSecond Derivative\nPredictors",
                                "PC3\nFluctuations, Embeddings,\nand Interestingness", "PC4\nIntegral, Sentiment,\nMax, and Min",
                                "PC5\nNumber of Peaks\nand Extrema")
    results_pd_long <- gather(t_results_pd, key = principal_components, value = pcs_results, colnames(t_results_pd)) #length(pcs)*n_folds
    pd_new_order <- with(results_pd_long, reorder(principal_components, pcs_results, mean, na.rm = TRUE))
    results_pd_long["pd_new_order"] <- pd_new_order
    results_pd_long <- results_pd_long[order(match(results_pd_long[, "pd_new_order"], results_meaning_long[, "meaning_new_order"])),] #order by meaningfulness scores

    # Get_noise_ceiling function
    summary_pd <- Get_noise_ceiling(dat_long, "personal_desirability", n_ss)

    #-------------------------------------------------------------------------------------------------------------------

    #3. Plotting 
    pcs_results_ordered <- data.frame(pcs_order = results_meaning_long$meaning_new_order,
                                      meaning_results = results_meaning_long$pcs_results,
                                      pd_results = results_pd_long$pcs_results) #combine meaning and pd results
    pcs_results_long <- gather(pcs_results_ordered, key = question_type, value = results, meaning_results, pd_results)

    # Make boxplot from CV_plotter function
    pcs_plot <- CV_plotter(pcs_results_long, pcs_results_long$pcs_order, pcs_results_long$results, pcs_results_long$question_type, "Principal Components", summary_meaning, summary_pd)

    # Get the labels
    x_labs <- ggplot_build(pcs_plot)$layout$panel_params[[1]]$
        x$
        get_labels()

    # Perform Wilcoxon tests and get stars for significance 
    # Define empty lists 
    wilcox_test_1_wt_meaning <- c()
    wilcox_test_2_wt_meaning <- c()
    p_value_stars_1_meaning <- c()
    p_value_stars_2_meaning <- c()
    wilcox_test_1_wt_pd <- c()
    wilcox_test_2_wt_pd <- c()
    p_value_stars_1_pd <- c()
    p_value_stars_2_pd <- c()

    # Loop through the pcs, comparing each to null, then PC2 vs PC3, PC3 vs PC5, PC5 vs PC1, and PC1 vs PC4 
    # Meaningfulness: One-sided Wilcox test
    print("Meaningfulness: --------------------------------------------------------------------------------------")
    for (i in 1:length(pcs)) {
        pcs_index <- x_labs[i]
        pcs_index_plus_one <- x_labs[i + 1]
        wilcox_test_1_wt_meaning[[i]] <- wilcox.test(t_results_meaning[, pcs_index], y = NULL, alternative = "greater",
                                                     conf.int = TRUE, data = t_results_meaning)
        p_value_stars_1_meaning[i] <- stars.pval(wilcox_test_1_wt_meaning[[i]]$"p.value") #get stars

        print(paste0(x_labs[i], " --------------------------------------------------------------------------------------"))
        print(wilcox_test_1_wt_meaning[[i]])
    }

    # Personal Desirability: One-sided Wilcox test
    print("Personal Desirability: --------------------------------------------------------------------------------------")
    for (i in 1:length(pcs)) {
        pcs_index <- x_labs[i]
        pcs_index_plus_one <- x_labs[i + 1]
        wilcox_test_1_wt_pd[[i]] <- wilcox.test(t_results_pd[, pcs_index], y = NULL, alternative = "greater",
                                                conf.int = TRUE, data = t_results_pd)
        p_value_stars_1_pd[i] <- stars.pval(wilcox_test_1_wt_pd[[i]]$"p.value") #get stars

        print(paste0(x_labs[i], " --------------------------------------------------------------------------------------"))
        print(wilcox_test_1_wt_pd[[i]])
    }

    # Meaningfulness: Two-sided Wilcox test
    print("Meaningfulness: --------------------------------------------------------------------------------------")
    for (i in 1:(length(pcs) - 1)) {
        pcs_index <- x_labs[i]
        pcs_index_plus_one <- x_labs[i + 1]
        wilcox_test_2_wt_meaning[[i]] <- wilcox.test(t_results_meaning[, pcs_index], y = t_results_meaning[, pcs_index_plus_one],
                                                     alternative = "two.sided", conf.int = TRUE, data = t_results_meaning)
        p_value_stars_2_meaning[i] <- stars.pval(wilcox_test_2_wt_meaning[[i]]$"p.value") #get stars
        if (p_value_stars_2_meaning[i] %in% c("", " ")) {
            p_value_stars_2_meaning[i] <- "ns"
        }

        print(paste0(sub("\\\n.*", "", pcs_index), " vs ", sub("\\\n.*", "", pcs_index_plus_one), #print PC comparisons vs one another
                     " --------------------------------------------------------------------------------------"))
        print(wilcox_test_2_wt_meaning[[i]])
    }

    # Personal Desirability: Two-sided Wilcox test
    print("Personal Desirability: --------------------------------------------------------------------------------------")
    for (i in 1:(length(pcs) - 1)) {
        pcs_index <- x_labs[i]
        pcs_index_plus_one <- x_labs[i + 1]
        wilcox_test_2_wt_pd[[i]] <- wilcox.test(t_results_pd[, pcs_index], y = t_results_pd[, pcs_index_plus_one],
                                                alternative = "two.sided", conf.int = TRUE, data = t_results_pd)
        p_value_stars_2_pd[i] <- stars.pval(wilcox_test_2_wt_pd[[i]]$"p.value") #get stars
        if (p_value_stars_2_pd[i] %in% c("", " ")) {
            p_value_stars_2_pd[i] <- "ns"
        }

        print(paste0(sub("\\\n.*", "", pcs_index), " vs ", sub("\\\n.*", "", pcs_index_plus_one), #print PC comparisons vs one another
                     " --------------------------------------------------------------------------------------"))
        print(wilcox_test_2_wt_pd[[i]])
    }

    # Define heights of annotations 
    bottom_y <- -1.05 #y value for all bottom stars 

    meaning_color <- "#56B4E9"
    meaning_bottom_x <- 0.813 #x value for bottom stars 
    meaning_top_x <- meaning_bottom_x + 0.5 #x value for top stars 
    meaning_top_y <- 1.39 #y value for top stars 
    meaning_bracket_y <- 1.31 #y value for top bracket  
    meaning_bracket_start <- 0.85 #x starting point for top bracket 
    meaning_bracket_end <- 1.8 #x ending point for top bracket 

    pd_color <- "#009E73"
    pd_bottom_x <- 1.19 #x value for bottom stars 
    pd_top_x <- pd_bottom_x + 0.5 #x value for top stars 
    pd_top_y <- 1.17 #y value for top stars 
    pd_bracket_y <- 1.09 #y value for top bracket  
    pd_bracket_start <- 1.24 #x starting point for top bracket 
    pd_bracket_end <- 2.15 #x ending point for top bracket

    # Add to the plot: stars indicating significance 
    pcs_plot <- pcs_plot +

        # One-sided Wilcox test
        ggplot2::annotate("text", x = meaning_bottom_x, y = bottom_y, size = 8, label = p_value_stars_1_meaning[[1]]) +
        ggplot2::annotate("text", x = meaning_bottom_x + 1, y = bottom_y, size = 8, label = p_value_stars_1_meaning[[2]]) +
        ggplot2::annotate("text", x = meaning_bottom_x + 2, y = bottom_y, size = 8, label = p_value_stars_1_meaning[[3]]) +
        ggplot2::annotate("text", x = meaning_bottom_x + 3, y = bottom_y, size = 8, label = p_value_stars_1_meaning[[4]]) +
        ggplot2::annotate("text", x = meaning_bottom_x + 4, y = bottom_y, size = 8, label = p_value_stars_1_meaning[[5]]) +
        ggplot2::annotate("text", x = pd_bottom_x, y = bottom_y, size = 8, label = p_value_stars_1_pd[[1]]) +
        ggplot2::annotate("text", x = pd_bottom_x + 1, y = bottom_y, size = 8, label = p_value_stars_1_pd[[2]]) +
        ggplot2::annotate("text", x = pd_bottom_x + 2, y = bottom_y, size = 8, label = p_value_stars_1_pd[[3]]) +
        ggplot2::annotate("text", x = pd_bottom_x + 3, y = bottom_y, size = 8, label = p_value_stars_1_pd[[4]]) +
        ggplot2::annotate("text", x = pd_bottom_x + 4, y = bottom_y, size = 8, label = p_value_stars_1_pd[[5]]) +

        # Two-sided Wilcox test (with brackets)
        geom_segment(aes(x = meaning_bracket_start, xend = meaning_bracket_end, y = meaning_bracket_y, yend = meaning_bracket_y, colour = meaning_color)) +
        ggplot2::annotate("text", x = meaning_top_x, y = meaning_top_y, size = 8, label = p_value_stars_2_meaning[[1]]) +
        geom_segment(aes(x = meaning_bracket_start + 1, xend = meaning_bracket_end + 1, y = meaning_bracket_y, yend = meaning_bracket_y, color = meaning_color)) +
        ggplot2::annotate("text", x = meaning_top_x + 1, y = meaning_top_y, size = 8, label = p_value_stars_2_meaning[[2]]) +
        geom_segment(aes(x = meaning_bracket_start + 2, xend = meaning_bracket_end + 2, y = meaning_bracket_y, yend = meaning_bracket_y, color = meaning_color)) +
        ggplot2::annotate("text", x = meaning_top_x + 2, y = meaning_top_y, size = 8, label = p_value_stars_2_meaning[[3]]) +
        geom_segment(aes(x = meaning_bracket_start + 3, xend = meaning_bracket_end + 3, y = meaning_bracket_y, yend = meaning_bracket_y, color = meaning_color)) +
        ggplot2::annotate("text", x = meaning_top_x + 3, y = meaning_top_y, size = 8, label = p_value_stars_2_meaning[[4]]) +
        geom_segment(aes(x = pd_bracket_start, xend = pd_bracket_end, y = pd_bracket_y, yend = pd_bracket_y, color = pd_color)) +
        ggplot2::annotate("text", x = pd_top_x, y = pd_top_y, size = 8, label = p_value_stars_2_pd[[1]]) +
        geom_segment(aes(x = pd_bracket_start + 1, xend = pd_bracket_end + 1, y = pd_bracket_y, yend = pd_bracket_y, color = pd_color)) +
        ggplot2::annotate("text", x = pd_top_x + 1, y = pd_top_y, size = 8, label = p_value_stars_2_pd[[2]]) +
        geom_segment(aes(x = pd_bracket_start + 2, xend = pd_bracket_end + 2, y = pd_bracket_y, yend = pd_bracket_y, color = pd_color)) +
        ggplot2::annotate("text", x = pd_top_x + 2, y = pd_top_y, size = 8, label = p_value_stars_2_pd[[3]]) +
        geom_segment(aes(x = pd_bracket_start + 3, xend = pd_bracket_end + 3, y = pd_bracket_y, yend = pd_bracket_y, color = pd_color)) +
        ggplot2::annotate("text", x = pd_top_x + 3, y = pd_top_y, size = 8, label = p_value_stars_2_pd[[4]]) +
        scale_colour_identity()

    #-------------------------------------------------------------------------------------------------------------------

    return(pcs_plot)
}


CrossValidationAnalysisWtPredictors <- function(dat, n_ss, n_plots) {
    "
    Measure the performance of each of our predictors by doing cross-validated regressions, holding out 
    one participant for each cross-validation step. 
    Input: data_wt_PCs, data_long, n_after_exclusions, n_plots 
    Output: importance of individual predictors and its graph 
    "

    # dat <- data_wt_PCs 
    # dat_long <- data_long
    # n_ss <- n_after_exclusions 

    predictors_old <- c("embeddings", "interestingness", "sentiment_score", "max", "min", "end_value", "number_peaks", "number_valleys", "number_extrema", "integral",
                        "d1_avg_unweight", "d1_avg_weight_prime", "d1_avg_weight_asc", "d1_avg_weight_des", "d1_avg_weight_end",
                        "d2_avg_unweight", "d2_avg_weight_prime", "d2_avg_weight_asc", "d2_avg_weight_des", "d2_avg_weight_end")
    predictors <- c("Embeddings", "Interestingness", "Sentiment Score", "Maximum", "Minimum", "End Value", "Number of\nPeaks", "Number of\nValleys", "Number of\nExtrema", "Integral",
                    "1st Derivative", "1st Derivative\nPrime", "1st Derivative\nAscending", "1st Derivative\nDescending", "1st Derivative\nEnd",
                    "2nd Derivative", "2nd Derivative\nPrime", "2nd Derivative\nAscending", "2nd Derivative\nDescending", "2nd Derivative\nEnd")

    if (colnames(d_long)[43] != "Minimum") {
        setnames(dat, old = predictors_old, new = predictors)
    }



    set.seed(1)
    n_folds <- n_ss
    folds <- cut(seq(1, nrow(dat)), breaks = n_folds, labels = FALSE)
    folds2 <- rep(seq(1, n_plots), times = n_folds) #plot x subjects folds
    indeces <- seq(1, (n_plots * n_folds))

    #-------------------------------------------------------------------------------------------------------------------

    #1. Meaningfulness
    results_meaning <- data.frame(matrix(NA, nrow = length(predictors), ncol = n_folds))
    rownames(results_meaning) <- predictors

    for (i in 1:length(predictors)) {
        for (j in 1:n_folds) {
            ss_results <- c()
            truths <- c()

            for (k in 1:n_plots) {
                trainIndeces <- indeces[(folds == j) & (folds2 != k)]
                testIndeces <- indeces[(folds == j) & (folds2 == k)]
                fitpc <- lm(meaningfulness ~ get(predictors[i]), data = dat, subset = trainIndeces) #fit model on subset of train data
                ss_results <- c(ss_results, predict(fitpc, dat)[testIndeces])
                truths <- c(truths, dat$meaningfulness[testIndeces])
            }

            results_meaning[i, j] <- cor(truths, ss_results)
        }

        print(paste('meaningfulness: mean predictor result,', predictors[i], ': ', mean(as.numeric(results_meaning[i,]), na.rm = TRUE)))
        print(paste('meaningfulness: median predictor result,', predictors[i], ': ', median(as.numeric(results_meaning[i,]), na.rm = TRUE)))
    }

    # Reorder predictors according to their significance 
    t_results_meaning <- as.data.frame(t(results_meaning))
    colnames(t_results_meaning) <- predictors
    results_meaning_long <- gather(t_results_meaning, key = predictors, value = predictors_results, colnames(t_results_meaning)) #length(predictors)*n_folds
    meaning_new_order <- with(results_meaning_long, reorder(predictors, predictors_results, mean, na.rm = TRUE))
    results_meaning_long["meaning_new_order"] <- meaning_new_order

    # Get_noise_ceiling function
    summary_meaning <- Get_noise_ceiling(dat, "meaningfulness", n_ss)

    #-------------------------------------------------------------------------------------------------------------------

    #2. Personal Desirability 
    results_pd <- data.frame(matrix(NA, nrow = length(predictors), ncol = n_folds))
    rownames(results_pd) <- predictors

    for (i in 1:length(predictors)) {
        for (j in 1:n_folds) {
            ss_results <- c()
            truths <- c()

            for (k in 1:n_plots) {
                trainIndeces <- indeces[(folds == j) & (folds2 != k)]
                testIndeces <- indeces[(folds == j) & (folds2 == k)]
                fitpc <- lm(personal_desirability ~ get(predictors[i]), data = dat, subset = trainIndeces) #fit model on subset of train data
                ss_results <- c(ss_results, predict(fitpc, dat)[testIndeces])
                truths <- c(truths, dat$personal_desirability[testIndeces])
            }

            results_pd[i, j] <- cor(truths, ss_results)
        }

        print(paste('personal desirability: mean predictor result,', predictors[i], ': ', mean(as.numeric(results_pd[i,]), na.rm = TRUE)))
        print(paste('personal desirability: median predictor result,', predictors[i], ': ', median(as.numeric(results_pd[i,]), na.rm = TRUE)))
    }

    # Reorder predictors according to their significance 
    t_results_pd <- as.data.frame(t(results_pd))
    colnames(t_results_pd) <- predictors
    results_pd_long <- gather(t_results_pd, key = predictors, value = predictors_results, colnames(t_results_pd)) #length(predictors)*n_folds
    pd_new_order <- with(results_pd_long, reorder(predictors, predictors_results, mean, na.rm = TRUE))
    results_pd_long["pd_new_order"] <- pd_new_order
    results_pd_long <- results_pd_long[order(match(results_pd_long[, "pd_new_order"], results_meaning_long[, "meaning_new_order"])),] #order by meaningfulness scores

    # Get_noise_ceiling function
    summary_pd <- Get_noise_ceiling(dat, "personal_desirability", n_ss)

    #-------------------------------------------------------------------------------------------------------------------

    #3. Plotting 
    predictors_results_ordered <- data.frame(predictors_order = results_meaning_long$meaning_new_order,
                                             meaning_results = results_meaning_long$predictors_results,
                                             pd_results = results_pd_long$predictors_results) #combine meaning and pd results
    predictors_results_long <- gather(predictors_results_ordered, key = question_type, value = results, meaning_results, pd_results)

    # Make boxplot from CV_plotter function
    predictors_results_long[predictors_results_long['question_type'] == 'meaning_results', 'question_type'] = "Meaningfulness"
    predictors_results_long[predictors_results_long['question_type'] == 'pd_results', 'question_type'] = "Personal Desirability"
    predictors_plot <- CV_plotter(predictors_results_long, predictors_results_long$predictors_order, predictors_results_long$results, predictors_results_long$question_type, "Predictors", summary_meaning, summary_pd)

    # Get the labels
    x_labs <- ggplot_build(predictors_plot)$layout$panel_params[[1]]$
        x$
        get_labels()

    # Perform Wilcoxon tests and get stars for significance 
    # Define empty lists 
    wilcox_test_wt_meaning <- c()
    p_value_stars_meaning <- c()
    wilcox_test_wt_pd <- c()
    p_value_stars_pd <- c()

    # Loop through the predictors, comparing each to a null distribution
    # Meaningfulness: One-sided Wilcox test
    print("Meaningfulness: --------------------------------------------------------------------------------------")
    for (i in x_labs) {
        print(paste0(i, " --------------------------------------------------------------------------------------"))
        wilcox_test_wt_meaning[[i]] <- wilcox.test(t_results_meaning[, i], y = NULL, alternative = "greater",
                                                   conf.int = TRUE, data = t_results_meaning)
        p_value_stars_meaning[i] <- stars.pval(wilcox_test_wt_meaning[[i]]$"p.value") #get stars
        print(wilcox_test_wt_meaning[[i]])
    }

    # Personal Desirability: One-sided Wilcox test
    print("Personal Desirability: --------------------------------------------------------------------------------------")
    for (i in x_labs) {
        print(paste0(i, " --------------------------------------------------------------------------------------"))
        wilcox_test_wt_pd[[i]] <- wilcox.test(t_results_pd[, i], y = NULL, alternative = "greater",
                                              conf.int = TRUE, data = t_results_pd)
        p_value_stars_pd[i] <- stars.pval(wilcox_test_wt_pd[[i]]$"p.value") #get stars
        print(wilcox_test_wt_pd[[i]])
    }

    # Define heights of annotations 
    meaning_bottom_x <- 0.8 #x value for bottom stars 
    meaning_bottom_y <- -1.05 #y value for bottom stars 
    pd_bottom_x <- 1.2 #x value for bottom stars 
    pd_bottom_y <- meaning_bottom_y - 0.10 #y value for bottom stars

    for (i in 1:20) {
        predictors_plot <- predictors_plot + ggplot2::annotate("text", x = meaning_bottom_x + i - 1,
                                                               y = meaning_bottom_y, size = 8,
                                                               label = p_value_stars_meaning[[i]])
        predictors_plot <- predictors_plot + ggplot2::annotate("text", x = pd_bottom_x + i - 1,
                                                               y = pd_bottom_y, size = 8,
                                                               label = p_value_stars_pd[[i]])
    }

    return(predictors_plot)
    #-------------------------------------------------------------------------------------------------------------------

    # 4. Parallel Mediation 

    # Check how well the predictors perform against one another via parallel mediation analysis 
    # process(data = dat, y = "meaningfulness", x = "plot_names", m = predictors, model = 4, 
    #         effsize = 1, total = 1, stand = 1, contrast = 1, boot = 10000 , modelbt = 1, seed = 123) 
    # ERROR: No more than 10 mediators are allowed in a PROCESS command. 
    # Try again with only the significant predictors or split in two.  

    # i. Meaningfulness 

    # Parallel mediation analysis with significant predictors. Takes about 5 minutes. 
    meaning_sig_pred <- c() #define list for significant predictors 
    for (i in x_labs) { #loop through the predictors, ordered from least to most accurate in predicting participant outcome
        if (p_value_stars_meaning[[i]] >= "*") {
            meaning_sig_pred <- append(meaning_sig_pred, names(p_value_stars_meaning[i]))
        }
    }
    meaning_pred_process <- process(data = dat, y = "meaningfulness", x = "plot_names", m = meaning_sig_pred, model = 4,
                                    effsize = 1, total = 1, stand = 1, contrast = 1, boot = 10000, modelbt = 1, seed = 123)

    # Parallel mediation analysis with all predictors (split in two because process() can only take up to 10 mediators at a time)
    # meaning_pred_process_1 <- process(data = dat, y = "meaningfulness", x = "plot_names", m = x_labs[1:10], model = 4, 
    #                                   effsize = 1, total = 1, stand = 1, contrast = 1, boot = 10000 , modelbt = 1, seed = 123) 
    # meaning_pred_process_2 <- process(data = dat, y = "meaningfulness", x = "plot_names", m = x_labs[11:20], model = 4, 
    #                                   effsize = 1, total = 1, stand = 1, contrast = 1, boot = 10000 , modelbt = 1, seed = 123) 

    # ii. Personal Desirability 

    # Parallel mediation analysis with significant predictors. 
    pd_sig_pred <- c() #define list for significant predictors 
    for (i in x_labs) { #loop through the predictors, ordered from least to most accurate in predicting participant outcome
        if (p_value_stars_pd[[i]] >= "*") {
            pd_sig_pred <- append(pd_sig_pred, names(p_value_stars_pd[i]))
        }
    }
    pd_pred_process <- process(data = dat, y = "personal_desirability", x = "plot_names", m = pd_sig_pred, model = 4,
                               effsize = 1, total = 1, stand = 1, contrast = 1, boot = 10000, modelbt = 1, seed = 123)

    # Parallel mediation analysis with all predictors (split in two because process() can only take up to 10 mediators at a time)
    # pd_pred_process_1 <- process(data = dat, y = "personal_desirability", x = "plot_names", m = x_labs[1:10], model = 4, 
    #                                   effsize = 1, total = 1, stand = 1, contrast = 1, boot = 10000 , modelbt = 1, seed = 123) 
    # pd_pred_process_2 <- process(data = dat, y = "personal_desirability", x = "plot_names", m = x_labs[11:20], model = 4, 
    #                                   effsize = 1, total = 1, stand = 1, contrast = 1, boot = 10000 , modelbt = 1, seed = 123) 

    #-------------------------------------------------------------------------------------------------------------------

    return(predictors_plot)
}


AnalyzeRidgeRegressionLifelines <- function(score_features_df) {
    "
    Measure the performance of individual predictors by doing cross-validated (nfold = 10) ridge regression
    Input: score_features_df
    Output: Ridge regression results for meaningfulness and personal desirability scores and their plots
    "

    # Define the columns that we want for the regression: from embeddings to integral and the D1 & D2 predictors.
    score_features_ss <- subset(score_features_df, select = c(embeddings:integral, d1_avg_unweight:d1_avg_weight_end, d2_avg_unweight:d2_avg_weight_end))
    my_predictors <- data.matrix(score_features_ss)

    # 1. Meaningfulness
    meaning_scores <- score_features_df$meaningfulness

    # Create testing and training data 
    set.seed(1)
    indeces <- sample(nrow(my_predictors), nrow(my_predictors) * 0.8)

    my_predictors_train <- my_predictors[indeces,]
    meaning_scores_train <- meaning_scores[indeces]

    my_predictors_test <- my_predictors[-indeces,]
    meaning_scores_test <- meaning_scores[-indeces]

    # Standardize data 
    my_predictors_train_stdz <- apply(my_predictors_train, 2, scale)
    meaning_scores_train_stdz <- scale(meaning_scores_train)

    my_predictors_test_stdz <- apply(my_predictors_test, 2, scale)
    meaning_scores_test_stdz <- scale(meaning_scores_test)

    # Run regular regression 
    lm_meaning_features <- glmnet(my_predictors_train_stdz, meaning_scores_train_stdz,
                                  alpha = 0, lambda = 0)

    # Run ridge regression 
    set.seed(123)
    lambdas <- seq(1, 10e-5, length = 100)
    ridge_meaning_features <- cv.glmnet(my_predictors_train_stdz, meaning_scores_train_stdz,
                                        nfolds = 10, alpha = 0, lambda = lambdas)
    plot(ridge_meaning_features)
    lambda_best <- ridge_meaning_features$lambda.min
    ridge_meaning_features1 <- glmnet(my_predictors_train_stdz, meaning_scores_train_stdz,
                                      alpha = 0, lambda = lambda_best)

    # Order the regression results from most to least important predictors (rounded to 5 digits)
    meaning_features_rounded <- round(coef(ridge_meaning_features1), 5)
    x_meaning_ordered = order(meaning_features_rounded@x, decreasing = TRUE)
    meaning_features_rounded@Dimnames[[1]] <- meaning_features_rounded@Dimnames[[1]][x_meaning_ordered]
    meaning_features_rounded@x <- meaning_features_rounded@x[x_meaning_ordered]

    print('meaningfulness vs. features:')
    print(meaning_features_rounded)

    # Compare standard regression to ridge regression 
    predict_meaning_lm <- predict(lm_meaning_features, my_predictors_test_stdz)
    mse_meaning_lm <- mean((meaning_scores_test_stdz - predict_meaning_lm)^2)
    mse_meaning_lm
    predict_meaning_ridge <- predict(ridge_meaning_features1, my_predictors_test_stdz)
    mse_meaning_ridge <- mean((meaning_scores_test_stdz - predict_meaning_ridge)^2)
    mse_meaning_ridge
    #Hmm, linear regression seems to perform a bit better than ridge regression 

    # Plot functions
    plot_Fit <- function(y_test, y_predicted, my_title) {
        g2 <- ggplot(data.frame(cbind(y_predicted, y_test)),
                     aes(x = y_test, y = y_predicted)) + geom_point()

        theme1 <- theme(
            plot.background = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),
            axis.line = element_line(size = .4)
        )

        textlab = cor.test(y_test, y_predicted)$estimate[[1]]
        textlab = round(textlab^2, digits = 5)
        textlab = as.character(paste("R^2 ==", textlab))

        data.label <- data.frame(x = 5,
                                 y = 2,
                                 label = textlab)

        g3 = g2 +
            theme1 +
            labs(x = "Actual Score",
                 y = "Predicted Score", title = my_title) +
            theme(plot.title = element_text(color = "black", hjust = 0.5)) +
            geom_smooth(aes(), method = "lm", se = FALSE) +
            geom_text(
                data = data.label,
                aes(x = x, y = y, label = label),
                size = 4,
                family = "Times",
                parse = TRUE,
                face = "italic"
            )

        g3

    }

    plot_Fit(meaning_scores_test_stdz, predict_meaning_lm, my_title = "LM: Meaningfulness")
    plot_Fit(meaning_scores_test_stdz, predict_meaning_ridge, my_title = "RIDGE: Meaningfulness")

    #------------------------------------------------------------------------------------------------

    # 2. Personal Desirability
    pd_scores <- score_features_df$personal_desirability

    # Create testing and training data 
    set.seed(1)
    indeces <- sample(nrow(my_predictors), nrow(my_predictors) * 0.8)

    my_predictors_train <- my_predictors[indeces,]
    pd_scores_train <- pd_scores[indeces]

    my_predictors_test <- my_predictors[-indeces,]
    pd_scores_test <- pd_scores[-indeces]

    # Standardize data 
    my_predictors_train_stdz <- apply(my_predictors_train, 2, scale)
    pd_scores_train_stdz <- scale(pd_scores_train)

    my_predictors_test_stdz <- apply(my_predictors_test, 2, scale)
    pd_scores_test_stdz <- scale(pd_scores_test)

    # Run regular regression 
    lm_pd_features <- glmnet(my_predictors_train_stdz, pd_scores_train_stdz,
                             alpha = 0, lambda = 0)

    # Run ridge regression 
    set.seed(123)
    lambdas <- seq(1, 10e-5, length = 100)
    ridge_pd_features <- cv.glmnet(my_predictors_train_stdz, pd_scores_train_stdz,
                                   nfolds = 10, alpha = 0, lambda = lambdas)
    plot(ridge_pd_features)
    lambda_best <- ridge_pd_features$lambda.min
    ridge_pd_features1 <- glmnet(my_predictors_train_stdz, pd_scores_train_stdz,
                                 alpha = 0, lambda = lambda_best)

    # Order the regression results from most to least important predictors (rounded to 5 digits)
    pd_features_rounded <- round(coef(ridge_pd_features1), 5)
    x_pd_ordered = order(pd_features_rounded@x, decreasing = TRUE)
    pd_features_rounded@Dimnames[[1]] <- pd_features_rounded@Dimnames[[1]][x_pd_ordered]
    pd_features_rounded@x <- pd_features_rounded@x[x_pd_ordered]

    print('personal desirability vs. features:')
    print(pd_features_rounded)

    # Compare standard regression to ridge regression 
    predict_pd_lm <- predict(lm_pd_features, my_predictors_test_stdz)
    mse_pd_lm <- mean((pd_scores_test_stdz - predict_pd_lm)^2)
    mse_pd_lm
    predict_pd_ridge <- predict(ridge_pd_features1, my_predictors_test_stdz)
    mse_pd_ridge <- mean((pd_scores_test_stdz - predict_pd_ridge)^2)
    mse_pd_ridge
    #Again, linear regression does slightly better than ridge regression. 

    # Plot functions
    plot_Fit(pd_scores_test_stdz, predict_pd_lm, my_title = "LM: Personal Desirability")
    plot_Fit(pd_scores_test_stdz, predict_pd_ridge, my_title = "RIDGE: Personal Desirability")
}


##================================================================================================================
##MAIN##
##================================================================================================================

# Define global variables
n_plots <- 27
meaning_scores <- 1:27
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

# Read Data and Create Folder for Saving Files
data <- read.csv('./data/data.csv')
dir.create("./plots/analysis_plots")

## ================================= (1) Perform Exclusions and Process Data =====================================
"
- Perform exclusions
- Create data_Long (nrows = num_ss*num_plots)
- Prepare for semantic and interestingness analyses
  - Create csv for semantic analysis
  - Create semantic embeddings dataframe
  - Create interestingness dataframe
- Create data_plot_long (nrows = num_plots*num_questions, i.e averages for plotting)
"

d_raw <- PerformExclusions(data) #num_rows = num_ss
n_after_exclusions <- d_raw$n_after_exclusions[1]
num_subjects_and_plots <- n_after_exclusions * n_plots

data_long <- Preprocess(d_raw, n_plots, plot_names) #num_rows = num_ss*num_plots [to see data without exclusions, replace data_clean with data]
data_long[, c("meaningfulness", "personal_desirability")] <- sapply(data_long[, c("meaningfulness", "personal_desirability")], as.numeric) #turn ratings to numeric


### (i) CREATE CSV FOR SEMANTIC ANALYSIS
analyze_words <- GetWordAnalysis(data_long, n_plots)
words_df <- as.data.frame(matrix(unlist(analyze_words), ncol = length(unlist(analyze_words[1]))))
analyze_words_df <- cbind(plot_names = plot_names, words = words_df$V1)
write.csv(analyze_words_df, "word_analysis.csv", row.names = FALSE) #create word analysis csv for google colab code


### (ii) CREATE SEMANTIC EMBEDDINGS DATAFRAME [**NB: YOU NEED TO HAVE ALREADY EXTRACTED EMBEDDINGS FOR word_analysis.csv]
my_embeddings <- read.csv("data/embeddings.csv", header = TRUE)
embeddings_avg <- data.frame(embeddings = rowMeans(my_embeddings[2:28])) #create a dataframe 


### (iii) CREATE INTERESTINGNESS DATAFRAME
interestingness <- GetInterestingness(data_long, n_plots)


### (iv) PROCESS FOR PLOTS
data_long <- cbind(data_long, embeddings_avg, interestingness)
data_plot_long <- ProcessForPlots(data_long, n_plots, plot_names) #num_rows = num_plots*num_questions
dim(data_plot_long)

calculate_sentiment <- FALSE
if(calculate_sentiment) {
    data_long[, "sentiment_score"] <- sapply(data_long["word"], CalculateSentiment, model_type = 'ai')
    write.csv(data.frame(sentiment_score = data_long[, "sentiment_score"]), "./data/sentiment_scores.csv", row.names = FALSE)
} else {
    data_long[, "sentiment_score"] <- read.csv('./data/sentiment_scores.csv')
}

data_long$sentiment_score[is.na(data_long$sentiment_score)] <- 0
data_long[, "is_word"] <- lapply(data_long["word"], is.word)

## ========================================== (2) Plot Data and Save ==================================================
"
Create bar plot, word clouds, and sentiment plot
"

#### (2.1) MAKE BAR PLOT OF MEANINGFULNESS SCORES
grouped_bar_plot <- MakeGroupedBarPlot(data_plot_long)
plot_images <- MakeGroupedBarPlotImages(grouped_bar_plot, plot_names) #the little lifeline icons

pdf(file = "lifelines_bar_plot.pdf", width = 17, height = 8)
ggdraw(insert_xaxis_grob(grouped_bar_plot, plot_images, position = "bottom"))
dev.off()


if (FALSE) {
    #### (2.2) MAKE WORD CLOUDS (WARNING: takes ~5 minutes; feel free to skip)
    MakeWordClouds(data_long, n_plots, plot_names) #make word cloud images
    arranged_word_clouds <- ArrangeWordClouds(data_long) #arrange word clouds into a grid

    pdf(file = "lifelines_word_clouds.pdf", width = 18, height = 8)
    plot(arranged_word_clouds)
    dev.off()


    #### (2.3) MAKE PLOT OF SENTIMENT SCORES, ORDERED BY MEANINGFULNESS SCORES
    sentiment_bar_plot <- MakeSentimentBarPlot(data_long, n_plots, plot_names)
    sentiment_plot_images <- MakeGroupedBarPlotImages(sentiment_bar_plot, plot_names) #the little lifeline icons

    pdf(file = "lifelines_sentiment_plot.pdf", width = 17, height = 8)
    ggdraw(insert_xaxis_grob(sentiment_bar_plot, sentiment_plot_images, position = "bottom"))
    dev.off()

    #### (2.4) MAKE FREQUENCY PLOTS FOR TOPIC MODELING
    topic_modeling <- TopicModeling(data_long, n_plots, plot_names)
}


## ============================================== (3) Analysis =====================================================
"
Get main statistical effects, and run descriptive and predictive analyses
"

#### (3.1) GET MAIN EFFECTS

# Get dataframe for analysis (dat_final), with nrows = num_ss*num_plots*num_questions
dat <- gather(data_long, key = question_type, value = score, meaningfulness, personal_desirability)
dat <- dplyr::select(dat, subject, plot_names, question_type, score) #rows = num_ss*num_plots*num_questions
sentiment_scores <- CreateSentimentDataframe(data_long, n_plots, plot_names)
dat_final <- cbind(dat, sentiment_score = sentiment_scores[rep(seq_len(nrow(sentiment_scores)), n_after_exclusions),]$mean)

write.csv(data.frame(word = dat_final), "./data/d_long.csv", row.names = FALSE) #create word analysis csv for google colab code

# Get main statistical effects
main_effects <- GetMainEffects(dat, data_long, n_plots, plot_names, my_embeddings)
#See error: 486 not defined because of singularities; checked for perfect correlation but did not find any

pdf(file = "linear_vs_quadratic_fit.pdf", width = 13, height = 6.5)
plot(main_effects)
main_effects
dev.off()


#### (3.2) RUN DESCRIPTIVE ANALYSES

# Create a dataframe of features and subject scores 
d_long <- CreateDataFeaturesDF(data_long, dat_final, features, n_after_exclusions, num_subjects_and_plots)

cross_validation_analysis_wt_predictors <- CrossValidationAnalysisWtPredictors(d_long, n_after_exclusions, n_plots)
pdf(file = "predictions_wt_predictors_cv_plot.pdf", width = 16, height = 9)
plot(cross_validation_analysis_wt_predictors)
dev.off()
# same note above 

## =========================================== (4) Move Files ====================================================

plot_files <- list.files(pattern = c("(.pdf|.png)"))
file.move(plot_files, "./plots/analysis_plots", overwrite = TRUE)
analysis_files <- list.files(pattern = c("(.csv)"))
file.move(analysis_files, "./data", overwrite = TRUE)

##================================================================================================================
##END##
##================================================================================================================


