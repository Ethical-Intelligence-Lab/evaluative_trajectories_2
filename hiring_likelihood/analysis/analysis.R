## Analysis Script for 'Evaluative Summaries'
# Experiment 5: Interview Performance 

## Needed for semantic analysis: https://colab.research.google.com/drive/1ZUl0yQQ0n0_zki-l8fEb2fTz40DQfTA1?usp=sharing

## Import libraries
if (!require(pacman)) { install.packages("pacman") }
pacman::p_load('ggplot2', #plot stuff
               'ggpubr', #customize plots 
               'data.table', #replace column names in data frame (setnames())
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
               'plotrix', #for std.error() function 
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

# Call in the Lifelines_Generate_Plots.R script from the Lifelines folder for plot images
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #set working directory to current directory
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
    n_before_exclusions <- dim(data)[1] #295

    # Exclude those who finished it in less than 2 minutes
    data <- subset(data, (data$Duration..in.seconds. > 120))

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

    print(paste0("Number before exclusions (those who both finished the survey and passed all attention checks): ", dim(data)[1]))

    # Perform comprehension checks
    data$attention_check2 <- ifelse((data$comp_check_1 == 80 &
        data$comp_check_2 == 0 &
        data$comp_check_3 == 'I was highly unstressed by the candidate early in their interview, then highly stressed by the candidate later in their interview.'
                                    ), 0, 1)

    #Perform second round of comprehension checks, if they failed the first
    data$comp_check <- ifelse(((is.na(data$comp_check_4 == TRUE))
        &
        (data$comp_check_7 == 'Perceived Performance') &
        (data$comp_check_8 == 'Time') &
        (data$comp_check_9 == 'Indicate how likely it was that you would hire the candidate')
        |
        ((data$comp_check_4 == 0) &
            (data$comp_check_5 == 80)
            &
            (data$comp_check_6 == 'I was highly stressed by the candidate early in their interview, then highly unstressed by the candidate later in their interview.') &
            (data$comp_check_7 == 'Perceived Performance') &
            (data$comp_check_8 == 'Time') &
            (data$comp_check_9 == 'Indicate how likely it was that you would hire the candidate')
        )), 0, 1)


    # Exclude those who failed either attention or comprehension checks
    data <- subset(data, (data$attention_check == 0) & (data$comp_check == 0))

    # Number of subjects after exclusions
    n_after_exclusions <- dim(data)[1] #140
    print(paste0("Number of participants excluded: ", n_before_exclusions - dim(data)[1]))

    data$n_after_exclusions <- n_after_exclusions

    print('Mean age:')
    print(mean(as.numeric(data$age), trim = 0, na.rm = TRUE)) ## mean age

    print('% Female:')
    print(table(data$gender)[1] / sum(table(data$gender))) ## percentage of females


    return(data)
}


Preprocess <- function(data, n_plts, plt_names) {
    " 
    Since each plot is shown within-subjects, Qualtrics spits out data in wide format
    Let's convert it to long format, so that we have a row for every plot type
    Input: data_clean (dataframe with number of rows = n_subjects), n_plots, plot_names 
    Output: dataframe with number of rows = n_subjects*n_plot_types (=27)
    "

    # data <- data_clean 
    # n_plts <- n_plots 
    # plt_names <- plot_names 

    # Define new data frame that we'll extract preprocessed data into

    # Define row and column names
    data_subset <- which(colnames(data) == "lr_preference_1"):which(colnames(data) == "lrsfer_word_gen_1") #35:88
    last_cols <- (which(colnames(data) == "lrsfer_word_gen_1") + 1):ncol(data) #89:105

    column_names <- c('plot_names', 'hiring_likelihood', 'word_gen', 'subject')

    df <- array(0, dim = c((nrow(data) * n_plts), length(column_names)))
    df <- as.data.frame(df, stringsAsFactors = FALSE)
    colnames(df) <- column_names

    # Turning wide format into long format, then inserting the answers into the 'df' dataframe
    final_data <- as.data.frame(t(data[data_subset])) #switch rows and columns in preparation for 'gather,' which collects info by columns
    long_data <- gather(final_data, key = "subject", value = "answers")["answers"] #gather the answers of the subjects into one long column 

    df[1] <- plt_names #plot_names
    df[2] <- long_data[seq(1, nrow(long_data), 2),] #hiring_likelihood
    df[3] <- long_data[seq(2, nrow(long_data), 2),] #word_gen
    df[4] <- rep(1:dim(data)[1], each = n_plts) #subject

    # Merge good data with first and last halves of the original data
    data <- cbind(data[rep(seq_len(nrow(data)), each = n_plts), 1:n_plts], df, data[rep(seq_len(nrow(data)), each = n_plts), last_cols])

    return(data)
}


ProcessForPlots <- function(data, n_plots, plot_names) {
    "
    Create a new data frame to store the ascending hiring likelihood scores
    Input: data_long, n_plots, plot_names   #num_rows = num_ss*num_plots 
    Output: data_plot_long (in order of ascending hiring likelihood scores)   #num_rows = num_plots*num_questions
    "

    # Get mean scores for all questions, then reshape data from wide to long format
    stats <- Get_stats(data, n_plots)
    data_plot <- data.frame(plot_names = plot_names,
                            hiring_likelihood_score_avg = unlist(stats)[c(TRUE, FALSE)],
                            hiring_likelihood_score_sd = unlist(stats)[c(FALSE, TRUE)])
    data_plot_sorted <- data_plot[order(data_plot$hiring_likelihood_score_avg),] #order by hiring likelihood
    data_plot_long <- gather(data_plot_sorted, key = question_type, #create separate entries for each question type, i.e., num_plots*num_questions
                             value = score, hiring_likelihood_score_avg)

    # Compile all standard deviation values
    stan_dev <- gather(data_plot_sorted, key = question_type,
                       value = sd, hiring_likelihood_score_sd)

    # Bind the sd column to the rest of the dataframe
    data_plot_long <- cbind(dplyr::select(data_plot_long, plot_names, question_type, score), sd = stan_dev$sd)
    data_plot_long$plot_names <- factor(data_plot_long$plot_names, levels = data_plot_long$plot_names[1:27])

    return(data_plot_long)
}


Get_stats <- function(data, n_plots) {
    #Find hiring likelihood means and standard deviations for every plot
    # Every plot repeats every 27 times, since there are 27 plots title.
    # Hence the 'seq' indeces for each calculation
    # Input: data_long, n_plots
    # Output: equations (a list of means and standard deviations of hiring likelihood scores for every plot)

    hiring_likelihood_score <- as.numeric(data$hiring_likelihood)

    equations <- c()
    for (i in 1:27) {
        equations[[i]] <- c(mean(hiring_likelihood_score[seq(i, length(hiring_likelihood_score), n_plots)]), sd(hiring_likelihood_score[seq(i, length(hiring_likelihood_score), n_plots)]))
    }

    return(equations)
}

##================================================================================================================
##FUNCTIONS FOR PLOTTING SENTIMENT BAR PLOT##
##================================================================================================================

Get_sentiment_stats <- function(data, n_plots) {
    #Find sentiment score means and standard deviations for every plot
    #Input: data_long, n_plots
    #Output: sentiment_list (a list of average mean and standard deviation sentiment scores for every plot)

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
    #Make a dataframe with sentiment scores for every plot, which we will use later to add to dat_final
    #Input: data_long, n_plots, plot_names
    #Output: sentiment_df (will be important later on when we bind it to dat for analysis)

    sentiment_stats <- Get_sentiment_stats(data, n_plots)
    sentiment_df <- data.frame(plot_names = plot_names,
                               mean = unlist(sentiment_stats)[c(TRUE, FALSE)],
                               sd = unlist(sentiment_stats)[c(FALSE, TRUE)])

    return(sentiment_df)
}


OrderSentimentDataframe <- function(data, n_plots, plot_names) {
    #Create a new data frame to store the sentiment scores by ascending hiring likelihood scores
    #Input: data_long, n_plots, plot_names
    #Output: sentiment_df_sorted (the sentiment_df ordered by levels in the function factor())

    # Get the order of hiring_likelihood scores
    stats <- Get_stats(data, n_plots)
    data_plot <- data.frame(plot_names = plot_names,
                            hiring_likelihood_score_avg = unlist(stats)[c(TRUE, FALSE)],
                            hiring_likelihood_score_sd = unlist(stats)[c(FALSE, TRUE)])

    # Create sentiment data frame ordered by ascending hiring likelihood scores
    sentiment_stats <- Get_sentiment_stats(data, n_plots)
    sentiment_df <- data.frame(plot_names = plot_names,
                               mean = unlist(sentiment_stats)[c(TRUE, FALSE)],
                               sd = unlist(sentiment_stats)[c(FALSE, TRUE)])
    sentiment_df_sorted <- sentiment_df[order(data_plot$hiring_likelihood_score_avg),]
    sentiment_df_sorted$plot_names <- factor(sentiment_df_sorted$plot_names, levels = data_plot_long$plot_names[1:27])

    return(sentiment_df_sorted)
}


##================================================================================================================
##FUNCTIONS FOR ANALYSIS##
##================================================================================================================
GetMainEffects <- function(data, n_plots, plot_names, my_embeddings) {
    data$plot_type_n <- as.numeric(factor(data$plot_names)) #create numeric version of plot_names
    data$score_n <- as.numeric(data$score) #create numeric version of score (which are characters)
    data$question_type_n <- as.numeric(factor(data$question_type, levels = unique(data$question_type)))
    data$subject_n <- as.numeric(factor(data$subject))

    print('*-*-*-*-*-*-*-*-* Did hiring likelihood scores vary depending on plot type? *-*-*-*-*-*-*-*-*')
    effect_mod <- lm(data = data[data['question_type'] == "hiring_likelihood",], score ~ plot_type_n + subject_n)
    print(summary(effect_mod))
    print('-----------------------------------------------------')

    return()

}


CreateDataFeaturesDF <- function(data, dat_final, features_df, n_after_exclusions, num_subjects_and_plots) {
    #Bind the three dataframes: data, sentiment score, and standardize(features), i.e., the standardized plot features.
    #Input: data_long, dat_final, features, n_after_exclusions, num_subjects_and_plots
    #Output: score_features_df (which contains all of the predictors and participant scores)

    score_features_df <- cbind(data,
                               as.data.frame(do.call("rbind", replicate(n_after_exclusions, standardize(features_df), simplify = FALSE))))
    score_features_df["hiring_likelihood"] <- as.data.frame(apply(score_features_df["hiring_likelihood"], 2, as.numeric))
    score_features_df["subject"] <- as.data.frame(apply(score_features_df["subject"], 2, as.numeric))
    score_features_df["plot_names"] <- as.data.frame(as.numeric(factor(score_features_df$plot_names)))
    score_features_df["hiring_likelihood"] <- standardize(score_features_df["hiring_likelihood"])
    score_features_df["sentiment_score"] <- standardize(score_features_df["sentiment_score"])
    score_features_df["embeddings"] <- standardize(score_features_df["embeddings"])
    score_features_df["interestingness"] <- standardize(score_features_df["interestingness"])

    return(score_features_df)

}


MakePCAFunction <- function(score_features_df) {
    #Perform mixed-effects regression based on PCA-reduced features of our predictors.
    #Input: score_features_df
    #Output: the structure of the PCA fit, the PCA correlation values for hiring likelihood
    #scores, and score_features_df (now with the addition of PC1 through PC5 scores)

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
    pdf(file = "./plots/analysis_plots/features_corrplot.pdf")
    corrplot(my_PCA$Structure, method = "circle", mar = c(0, 0, 4, 0))
    mtext("Features Correlation Matrix \nover PCs", at = 1, line = 1, cex = 1.5)
    dev.off()

    print('principal components by features')
    print(my_PCA$Structure)

    # Bind the PC1 through PC5 scores to the score_features_df data frame
    score_features_df <- cbind(score_features_df, my_PCA$scores)

    # Fit mixed effects regression predicting hiring likelihood
    hiring_likelihood_features <- lmer(data = score_features_df,
                                       hiring_likelihood ~ PC1 +
                                           PC2 +
                                           PC3 +
                                           PC4 +
                                           PC5 +
                                           (1 | subject) +
                                           (1 | plot_names))

    print('hiring likelihood vs. features:')
    print(summary(hiring_likelihood_features, correlation = TRUE))

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

    # Hiring Likelihood 
    results_hiring_likelihood <- data.frame(matrix(NA, nrow = length(pcs), ncol = n_folds))
    rownames(results_hiring_likelihood) <- pcs

    for (i in 1:length(pcs)) {
        for (j in 1:n_folds) {
            ss_results <- c()
            truths <- c()

            for (k in 1:n_plots) {
                trainIndeces <- indeces[(folds == j) & (folds2 != k)]
                testIndeces <- indeces[(folds == j) & (folds2 == k)]
                fitpc <- lm(hiring_likelihood ~ get(pcs[i]), data = dat, subset = trainIndeces) #fit model on subset of train data
                ss_results <- c(ss_results, predict(fitpc, dat)[testIndeces])
                truths <- c(truths, dat$hiring_likelihood[testIndeces])
            }

            results_hiring_likelihood[i, j] <- cor(truths, ss_results)
        }

        print(paste('hiring likelihood: mean PC result,', pcs[i], ': ', mean(as.numeric(results_hiring_likelihood[i,]), na.rm = TRUE)))
        print(paste('hiring likelihood: median PC result,', pcs[i], ': ', median(as.numeric(results_hiring_likelihood[i,]), na.rm = TRUE)))
    }

    # Reorder pcs according to their significance 
    t_results_hiring_likelihood <- as.data.frame(t(results_hiring_likelihood))
    colnames(t_results_hiring_likelihood) <- c("PC1\nFirst Derivative\nPredictors and End Value", "PC2\nSecond Derivative\nPredictors",
                                               "PC3\nFluctuations, Embeddings,\nand Interestingness", "PC4\nIntegral, Sentiment,\nMax, and Min",
                                               "PC5\nNumber of Peaks\nand Extrema") #rename using corrplot(my_PCA$Structure, method = "circle", mar = c(0, 0, 4, 0)) in MakePCAFunction()

    results_hiring_likelihood_long <- gather(t_results_hiring_likelihood, key = principal_components, value = pcs_results, colnames(t_results_hiring_likelihood)) #length(pcs)*n_folds
    hiring_likelihood_new_order <- with(results_hiring_likelihood_long, reorder(principal_components, pcs_results, median, na.rm = TRUE))
    results_hiring_likelihood_long["hiring_likelihood_new_order"] <- hiring_likelihood_new_order

    #-------------------------------------------------------------------------------------------------------------------

    #2. Plotting 
    pcs_results_ordered <- data.frame(pcs_order = results_hiring_likelihood_long$hiring_likelihood_new_order,
                                      hiring_likelihood_results = results_hiring_likelihood_long$pcs_results)
    pcs_results_long <- gather(pcs_results_ordered, key = question_type, value = results, hiring_likelihood_results)

    # Make boxplot from CV_plotter function
    pcs_plot <- CV_plotter(pcs_results_long, pcs_results_long$pcs_order, pcs_results_long$results, pcs_results_long$question_type, "Principal Components")

    # Get the labels (in order)
    x_labs <- ggplot_build(pcs_plot)$layout$panel_params[[1]]$
        x$
        get_labels()

    # Perform Wilcoxon tests and get stars for significance 
    # Define empty lists 
    wilcox_test_1_wt_hiring_likelihood <- c()
    wilcox_test_2_wt_hiring_likelihood <- c()
    p_value_stars_1_hiring_likelihood <- c()
    p_value_stars_2_hiring_likelihood <- c()

    # Loop through the pcs, comparing each to null, then PC2 vs PC3, PC3 vs PC5, PC5 vs PC1, and PC1 vs PC4 
    # Hiring Likelihood: One-sided Wilcox test
    print("Hiring Likelihood: --------------------------------------------------------------------------------------")
    for (i in 1:length(pcs)) {
        pcs_index <- x_labs[i]
        pcs_index_plus_one <- x_labs[i + 1]
        wilcox_test_1_wt_hiring_likelihood[[i]] <- wilcox.test(t_results_hiring_likelihood[, pcs_index], y = NULL, alternative = "greater",
                                                               conf.int = TRUE, data = t_results_hiring_likelihood)
        p_value_stars_1_hiring_likelihood[i] <- stars.pval(wilcox_test_1_wt_hiring_likelihood[[i]]$"p.value") #get stars

        print(paste0(x_labs[i], " --------------------------------------------------------------------------------------"))
        print(wilcox_test_1_wt_hiring_likelihood[[i]])
    }

    # Hiring Likelihood: Two-sided Wilcox test
    print("Hiring Likelihood: --------------------------------------------------------------------------------------")
    for (i in 1:(length(pcs) - 1)) {
        pcs_index <- x_labs[i]
        pcs_index_plus_one <- x_labs[i + 1]
        wilcox_test_2_wt_hiring_likelihood[[i]] <- wilcox.test(t_results_hiring_likelihood[, pcs_index], y = t_results_hiring_likelihood[, pcs_index_plus_one],
                                                               alternative = "two.sided", conf.int = TRUE, data = t_results_hiring_likelihood)
        p_value_stars_2_hiring_likelihood[i] <- stars.pval(wilcox_test_2_wt_hiring_likelihood[[i]]$"p.value") #get stars
        if (p_value_stars_2_hiring_likelihood[i] %in% c("", " ")) {
            p_value_stars_2_hiring_likelihood[i] <- "ns"
        }

        print(paste0(sub("\\\n.*", "", pcs_index), " vs ", sub("\\\n.*", "", pcs_index_plus_one), #print PC comparisons vs one another
                     " --------------------------------------------------------------------------------------"))
        print(wilcox_test_2_wt_hiring_likelihood[[i]])
    }

    # Define heights of annotations 
    bottom_y <- -1.05 #y value for all bottom stars 

    hiring_likelihood_color <- "#56B4E9"
    hiring_likelihood_bottom_x <- 1 #x value for bottom stars 
    hiring_likelihood_top_x <- hiring_likelihood_bottom_x + 0.5 #x value for top stars 
    hiring_likelihood_top_y <- 1.17 #y value for top stars 
    hiring_likelihood_bracket_y <- 1.09 #y value for top bracket  
    hiring_likelihood_bracket_start <- 1.05 #x starting point for top bracket 
    hiring_likelihood_bracket_end <- 1.95 #x ending point for top bracket 

    # Add to the plot: stars indicating significance 
    pcs_plot <- pcs_plot +

        # One-sided Wilcox test
        ggplot2::annotate("text", x = hiring_likelihood_bottom_x, y = bottom_y, size = 8, label = p_value_stars_1_hiring_likelihood[[1]]) +
        ggplot2::annotate("text", x = hiring_likelihood_bottom_x + 1, y = bottom_y, size = 8, label = p_value_stars_1_hiring_likelihood[[2]]) +
        ggplot2::annotate("text", x = hiring_likelihood_bottom_x + 2, y = bottom_y, size = 8, label = p_value_stars_1_hiring_likelihood[[3]]) +
        ggplot2::annotate("text", x = hiring_likelihood_bottom_x + 3, y = bottom_y, size = 8, label = p_value_stars_1_hiring_likelihood[[4]]) +
        ggplot2::annotate("text", x = hiring_likelihood_bottom_x + 4, y = bottom_y, size = 8, label = p_value_stars_1_hiring_likelihood[[5]]) +

        # Two-sided Wilcox test (with brackets)
        geom_segment(aes(x = hiring_likelihood_bracket_start, xend = hiring_likelihood_bracket_end, y = hiring_likelihood_bracket_y, yend = hiring_likelihood_bracket_y, colour = hiring_likelihood_color)) +
        ggplot2::annotate("text", x = hiring_likelihood_top_x, y = hiring_likelihood_top_y, size = 8, label = p_value_stars_2_hiring_likelihood[[1]]) +
        geom_segment(aes(x = hiring_likelihood_bracket_start + 1, xend = hiring_likelihood_bracket_end + 1, y = hiring_likelihood_bracket_y, yend = hiring_likelihood_bracket_y, color = hiring_likelihood_color)) +
        ggplot2::annotate("text", x = hiring_likelihood_top_x + 1, y = hiring_likelihood_top_y, size = 8, label = p_value_stars_2_hiring_likelihood[[2]]) +
        geom_segment(aes(x = hiring_likelihood_bracket_start + 2, xend = hiring_likelihood_bracket_end + 2, y = hiring_likelihood_bracket_y, yend = hiring_likelihood_bracket_y, color = hiring_likelihood_color)) +
        ggplot2::annotate("text", x = hiring_likelihood_top_x + 2, y = hiring_likelihood_top_y, size = 8, label = p_value_stars_2_hiring_likelihood[[3]]) +
        geom_segment(aes(x = hiring_likelihood_bracket_start + 3, xend = hiring_likelihood_bracket_end + 3, y = hiring_likelihood_bracket_y, yend = hiring_likelihood_bracket_y, color = hiring_likelihood_color)) +
        ggplot2::annotate("text", x = hiring_likelihood_top_x + 3, y = hiring_likelihood_top_y, size = 8, label = p_value_stars_2_hiring_likelihood[[4]]) +

        scale_colour_identity()

    #-------------------------------------------------------------------------------------------------------------------

    return(pcs_plot)
}


CrossValidationAnalysisWtPredictors <- function(dat, dat_long, n_ss, n_plots) {
    print("Running cross validation...")

    # dat <- data_wt_PCs 
    # dat_long <- data_long
    # n_ss <- n_after_exclusions 

    predictors_old <- c("embeddings", "interestingness", "sentiment_score", "max", "min", "end_value", "number_peaks", "number_valleys", "number_extrema", "integral",
                        "d1_avg_unweight", "d1_avg_weight_prime", "d1_avg_weight_asc", "d1_avg_weight_des", "d1_avg_weight_end",
                        "d2_avg_unweight", "d2_avg_weight_prime", "d2_avg_weight_asc", "d2_avg_weight_des", "d2_avg_weight_end")
    predictors <- c("Embeddings", "Interestingness", "Sentiment Score", "Maximum", "Minimum", "End Value", "Number of\nPeaks", "Number of\nValleys", "Number of\nExtrema", "Integral",
                    "1st Derivative", "1st Derivative\nPrime", "1st Derivative\nAscending", "1st Derivative\nDescending", "1st Derivative\nEnd",
                    "2nd Derivative", "2nd Derivative\nPrime", "2nd Derivative\nAscending", "2nd Derivative\nDescending", "2nd Derivative\nEnd")
    setnames(dat, old = predictors_old, new = predictors)

    set.seed(1)
    n_folds <- n_ss
    folds <- cut(seq(1, nrow(dat)), breaks = n_folds, labels = FALSE)
    folds2 <- rep(seq(1, n_plots), times = n_folds) #plot x subjects folds
    indeces <- seq(1, (n_plots * n_folds))

    #-------------------------------------------------------------------------------------------------------------------

    #1. Hiring Likelihood
    results_hiring_likelihood <- data.frame(matrix(NA, nrow = length(predictors), ncol = n_folds))
    rownames(results_hiring_likelihood) <- predictors

    for (i in 1:length(predictors)) {
        for (j in 1:n_folds) {
            ss_results <- c()
            truths <- c()

            for (k in 1:n_plots) {
                trainIndeces <- indeces[(folds == j) & (folds2 != k)]
                testIndeces <- indeces[(folds == j) & (folds2 == k)]

                if (predictors[i] == "Sentiment Score") { # Exclude train indexes that is not a word
                    trainIndeces <- subset(trainIndeces, dat$is_word[trainIndeces])
                }

                if (predictors[i] == "Sentiment Score" && !dat$is_word[testIndeces]) { # Do not fit if not a word
                    next
                } else {
                    fitpc <- lm(hiring_likelihood ~ get(predictors[i]), data = dat, subset = trainIndeces) #fit model on subset of train data
                    ss_results <- c(ss_results, predict(fitpc, dat)[testIndeces])
                    truths <- c(truths, dat$hiring_likelihood[testIndeces])
                }
            }

            results_hiring_likelihood[i, j] <- cor(truths, ss_results)
        }

        print(paste('hiring likelihood: mean predictor result,', predictors[i], ': ', mean(as.numeric(results_hiring_likelihood[i,]), na.rm = TRUE)))
    }

    # Reorder predictors according to their significance 
    t_results_hiring_likelihood <- as.data.frame(t(results_hiring_likelihood))
    colnames(t_results_hiring_likelihood) <- predictors
    results_hiring_likelihood_long <- gather(t_results_hiring_likelihood, key = predictors, value = predictors_results, colnames(t_results_hiring_likelihood)) #length(predictors)*n_folds
    hiring_likelihood_new_order <- with(results_hiring_likelihood_long, reorder(predictors, predictors_results, mean, na.rm = TRUE))
    results_hiring_likelihood_long["hiring_likelihood_new_order"] <- hiring_likelihood_new_order

    #-------------------------------------------------------------------------------------------------------------------

    #2. Plotting 
    predictors_results_ordered <- data.frame(predictors_order = results_hiring_likelihood_long$hiring_likelihood_new_order,
                                             hiring_likelihood_results = results_hiring_likelihood_long$predictors_results)
    predictors_results_long <- gather(predictors_results_ordered, key = question_type, value = results, hiring_likelihood_results)

    # Make boxplot from CV_plotter function
    predictors_plot <- CV_plotter(predictors_results_long, predictors_results_long$predictors_order, predictors_results_long$results, predictors_results_long$question_type, "Predictors")

    # Get the labels
    x_labs <- ggplot_build(predictors_plot)$layout$panel_params[[1]]$
        x$
        get_labels()

    # Perform Wilcoxon tests and get stars for significance 
    # Define empty lists 
    wilcox_test_wt_hiring_likelihood <- c()
    p_value_stars_hiring_likelihood <- c()

    # Loop through the predictors, comparing each to a null distribution
    # Hiring Likelihood: One-sided Wilcox test
    print("Hiring Likelihood: --------------------------------------------------------------------------------------")
    for (i in x_labs) {
        print(paste0(i, " --------------------------------------------------------------------------------------"))
        wilcox_test_wt_hiring_likelihood[[i]] <- wilcox.test(t_results_hiring_likelihood[, i], y = NULL, alternative = "greater",
                                                             conf.int = TRUE, data = t_results_hiring_likelihood)
        p_value_stars_hiring_likelihood[i] <- stars.pval(wilcox_test_wt_hiring_likelihood[[i]]$"p.value") #get stars
        print(wilcox_test_wt_hiring_likelihood[[i]])
    }

    # Define heights of annotations 
    hiring_likelihood_bottom_x <- 1 #x value for bottom stars 
    hiring_likelihood_bottom_y <- -1.05 #y value for bottom stars 

    # Add to the plot: stars indicating significance 
    predictors_plot <- predictors_plot +

        # One-sided Wilcox test
        ggplot2::annotate("text", x = hiring_likelihood_bottom_x, y = hiring_likelihood_bottom_y, size = 8, label = p_value_stars_hiring_likelihood[[1]]) +
        ggplot2::annotate("text", x = hiring_likelihood_bottom_x + 1, y = hiring_likelihood_bottom_y, size = 8, label = p_value_stars_hiring_likelihood[[2]]) +
        ggplot2::annotate("text", x = hiring_likelihood_bottom_x + 2, y = hiring_likelihood_bottom_y, size = 8, label = p_value_stars_hiring_likelihood[[3]]) +
        ggplot2::annotate("text", x = hiring_likelihood_bottom_x + 3, y = hiring_likelihood_bottom_y, size = 8, label = p_value_stars_hiring_likelihood[[4]]) +
        ggplot2::annotate("text", x = hiring_likelihood_bottom_x + 4, y = hiring_likelihood_bottom_y, size = 8, label = p_value_stars_hiring_likelihood[[5]]) +
        ggplot2::annotate("text", x = hiring_likelihood_bottom_x + 5, y = hiring_likelihood_bottom_y, size = 8, label = p_value_stars_hiring_likelihood[[6]]) +
        ggplot2::annotate("text", x = hiring_likelihood_bottom_x + 6, y = hiring_likelihood_bottom_y, size = 8, label = p_value_stars_hiring_likelihood[[7]]) +
        ggplot2::annotate("text", x = hiring_likelihood_bottom_x + 7, y = hiring_likelihood_bottom_y, size = 8, label = p_value_stars_hiring_likelihood[[8]]) +
        ggplot2::annotate("text", x = hiring_likelihood_bottom_x + 8, y = hiring_likelihood_bottom_y, size = 8, label = p_value_stars_hiring_likelihood[[9]]) +
        ggplot2::annotate("text", x = hiring_likelihood_bottom_x + 9, y = hiring_likelihood_bottom_y, size = 8, label = p_value_stars_hiring_likelihood[[10]]) +
        ggplot2::annotate("text", x = hiring_likelihood_bottom_x + 10, y = hiring_likelihood_bottom_y, size = 8, label = p_value_stars_hiring_likelihood[[11]]) +
        ggplot2::annotate("text", x = hiring_likelihood_bottom_x + 11, y = hiring_likelihood_bottom_y, size = 8, label = p_value_stars_hiring_likelihood[[12]]) +
        ggplot2::annotate("text", x = hiring_likelihood_bottom_x + 12, y = hiring_likelihood_bottom_y, size = 8, label = p_value_stars_hiring_likelihood[[13]]) +
        ggplot2::annotate("text", x = hiring_likelihood_bottom_x + 13, y = hiring_likelihood_bottom_y, size = 8, label = p_value_stars_hiring_likelihood[[14]]) +
        ggplot2::annotate("text", x = hiring_likelihood_bottom_x + 14, y = hiring_likelihood_bottom_y, size = 8, label = p_value_stars_hiring_likelihood[[15]]) +
        ggplot2::annotate("text", x = hiring_likelihood_bottom_x + 15, y = hiring_likelihood_bottom_y, size = 8, label = p_value_stars_hiring_likelihood[[16]]) +
        ggplot2::annotate("text", x = hiring_likelihood_bottom_x + 16, y = hiring_likelihood_bottom_y, size = 8, label = p_value_stars_hiring_likelihood[[17]]) +
        ggplot2::annotate("text", x = hiring_likelihood_bottom_x + 17, y = hiring_likelihood_bottom_y, size = 8, label = p_value_stars_hiring_likelihood[[18]]) +
        ggplot2::annotate("text", x = hiring_likelihood_bottom_x + 18, y = hiring_likelihood_bottom_y, size = 8, label = p_value_stars_hiring_likelihood[[19]]) +
        ggplot2::annotate("text", x = hiring_likelihood_bottom_x + 19, y = hiring_likelihood_bottom_y, size = 8, label = p_value_stars_hiring_likelihood[[20]])

    #-------------------------------------------------------------------------------------------------------------------

    print(predictors_plot)
    return(predictors_plot)
}

##======##
## MAIN ##
##======##

# Define global variables
n_plots <- 27
hiring_likelihood_scores <- 1:27
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
d_raw <- read.csv('./data/data.csv')

## ================================= (1) Perform Exclusions and Process Data =====================================

#- Perform exclusions
#- Create data_long (nrows = num_ss*num_plots)
#- Prepare for semantic and interestingness analyses
#  - Create csv for semantic analysis
#  - Create semantic embeddings dataframe
#  - Create interestingness dataframe
#- Create data_plot_long (nrows = num_plots*num_questions, i.e averages for plotting)


d <- PerformExclusions(d_raw) #num_rows = num_ss
n_after_exclusions <- d$n_after_exclusions[1]
num_subjects_and_plots <- n_after_exclusions * n_plots

d_long <- Preprocess(d, n_plots, plot_names) #num_rows = num_ss*num_plots [to see data without exclusions, replace data_clean with data]
d_long[, "hiring_likelihood"] <- sapply(d_long[, "hiring_likelihood"], as.numeric) #turn ratings to numeric

### (i) CREATE CSV FOR SEMANTIC ANALYSIS
analyze_words <- GetWordAnalysis(d_long, n_plots)
words_df <- as.data.frame(matrix(unlist(analyze_words), ncol = length(unlist(analyze_words[1]))))
analyze_words_df <- cbind(plot_names = plot_names, words = words_df$V1)
write.csv(analyze_words_df, "./data/word_analysis.csv", row.names = FALSE) #create word analysis csv for google colab code
write.csv(data.frame(word = d_long), "./data/d_long.csv", row.names = FALSE) #create word analysis csv for google colab code

### (ii) CREATE SEMANTIC EMBEDDINGS DATAFRAME [**NB: YOU NEED TO HAVE ALREADY EXTRACTED EMBEDDINGS FOR word_analysis.csv]
my_embeddings <- read.csv("data/embeddings_long.csv", header = TRUE)
my_embeddings$X = NULL
embeddings_avg <- data.frame(embeddings = rowMeans(my_embeddings)) #create a dataframe

### (iii) CREATE INTERESTINGNESS DATAFRAME
interestingness <- GetInterestingness(d_long, n_plots)

### (iv) PROCESS FOR PLOTS
d_long <- cbind(d_long, embeddings_avg)
d_long <- cbind(d_long, interestingness)

calculate_sentiment <- FALSE
if(calculate_sentiment) {
    d_long[, "sentiment_score"] <- sapply(d_long["word_gen"], CalculateSentiment, model_type = 'ai')
    write.csv(data.frame(sentiment_score = d_long[, "sentiment_score"]), "./data/sentiment_scores.csv", row.names = FALSE)
} else {
    d_long[, "sentiment_score"] <- read.csv('./data/sentiment_scores.csv')
}

d_long$sentiment_score[is.na(d_long$sentiment_score)] <- 0
d_long[, "is_word"] <- lapply(d_long["word_gen"], is.word)

data_plot_long <- ProcessForPlots(d_long, n_plots, plot_names) #num_rows = num_plots*num_questions

## ========================================== (2) Plot Data and Save ==================================================

#### (2.1) MAKE BAR PLOT OF HIRING LIKELIHOOD SCORES
grouped_bar_plot <- MakeGroupedBarPlot(data_plot_long)
plot_images <- MakeGroupedBarPlotImages(grouped_bar_plot, plot_names) #the little interview performance icons

pdf(file = "./plots/analysis_plots/interview_performance_bar_plot.pdf", width = 17, height = 8)
ggdraw(insert_xaxis_grob(grouped_bar_plot, plot_images, position = "bottom"))
dev.off()

if (FALSE) {  ## Takes some time
    #### (2.2) MAKE WORD CLOUDS (WARNING: takes ~5 minutes; feel free to skip)
    MakeWordClouds(d_long, n_plots, plot_names) #make word cloud images
    arranged_word_clouds <- ArrangeWordClouds(d_long) #arrange word clouds into a grid

    pdf(file = "./plots/analysis_plots/interview_performance_word_clouds.pdf", width = 18, height = 8)
    plot(arranged_word_clouds)
    dev.off()

    #### (2.3) MAKE PLOT OF SENTIMENT SCORES, ORDERED BY HIRING LIKELIHOOD SCORES
    sentiment_bar_plot <- MakeSentimentBarPlot(d_long, n_plots, plot_names, title = 'Hiring')
    sentiment_plot_images <- MakeGroupedBarPlotImages(sentiment_bar_plot, plot_names) #the little interview performance icons

    pdf(file = "./plots/analysis_plots/interview_performance_sentiment_plot.pdf", width = 17, height = 8)
    ggdraw(insert_xaxis_grob(sentiment_bar_plot, sentiment_plot_images, position = "bottom"))
    dev.off()

    #### (2.4) MAKE FREQUENCY PLOTS FOR TOPIC MODELING
    topic_modeling <- TopicModeling(d_long, n_plots, plot_names)
}


## ============================================== (3) Analysis =====================================================
"
Get main statistical effects, and run descriptive and predictive analyses
"

#### (3.1) GET MAIN EFFECTS

# Get dataframe for analysis (dat_final), with nrows = num_ss*num_plots*num_questions
dat <- gather(d_long, key = question_type, value = score, hiring_likelihood)
dat <- dplyr::select(dat, subject, plot_names, question_type, score, sentiment_score) #rows = num_ss*num_plots*num_questions

write.csv(data.frame(word = d_long), "./data/d_long.csv", row.names = FALSE) #create word analysis csv for google colab code
write.csv(data.frame(word = dat), "./data/dat.csv", row.names = FALSE) #create word analysis csv for google colab code

main_effects <- GetMainEffects(dat, n_plots, plot_names, my_embeddings)
pdf(file = "linear_vs_quadratic_fit.pdf", width = 13, height = 6.5)
#plot(main_effects)
#dev.off()


#### (3.2) RUN DESCRIPTIVE ANALYSES

# Create a dataframe of features and subject scores 
score_features_df <- CreateDataFeaturesDF(d_long, dat, features, n_after_exclusions, num_subjects_and_plots)

##### (3.3) RUN PREDICTIVE ANALYSES

# Get performance of each predictor and PCA-reduced feature using cross-validation.
if (FALSE) {
    # Run regularized regression on all predictors
    ridge_regression_wt_predictors <- AnalyzeRidgeRegression(score_features_df, metric = 'hiring_likelihood')

    # Run mixed-effects regression on PCA-reduced features
    data_wt_PCs <- MakePCAFunction(score_features_df)


    cross_validation_analysis_wt_pcs <- CrossValidationAnalysisWtPCs(data_wt_PCs, d_long, n_after_exclusions, n_plots)
    pdf(file = "./plots/analysis_plots/predictions_wt_pcs_cv_plot.pdf", width = 15, height = 9)
    cross_validation_analysis_wt_pcs
    dev.off()
}

cross_validation_analysis_wt_predictors <- CrossValidationAnalysisWtPredictors(score_features_df, d_long, n_after_exclusions, n_plots)
pdf(file = "./plots/analysis_plots/predictions_wt_predictors_cv_plot.pdf", width = 15, height = 9)
cross_validation_analysis_wt_predictors
dev.off()
# same note above

print("**** For correlations across studies, please run 'between_experiment_analyses/analysis.R' ****")
## =========================================== (4) Move Files ====================================================

plot_files <- list.files(pattern = c("(.pdf|.png)"))
file.move(plot_files, "./plots/analysis_plots", overwrite = TRUE)
analysis_files <- list.files(pattern = c("word_analysis.csv|embeddings.csv|correlations.csv"))
file.move(analysis_files, "./data", overwrite = TRUE)

##=====##
## END ##
##=====##