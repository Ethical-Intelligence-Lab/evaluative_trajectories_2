## Analysis Script for 'Evaluative Summaries'
# Experiment 5: Interview Performance 
rm(list = ls())

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
               'broom', #install separately if does not work
               'filesstrings'
)

# Call in the Lifelines_Generate_Plots.R script from the Lifelines folder for plot images
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #set working directory to current directory
source('../../tools/common_functions.R')
source('./plotting.R')
source('../../tools/Lifelines_Generate_Plots.R')
dir.create("plots/analysis_plots", recursive = TRUE)

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

GetMainEffects <- function(data, n_plots, plot_names, my_embeddings, data_plot_long) {
    data_plot_long$index <- 1:nrow(data_plot_long)
    get_plot_index <- function(row) {
        return(data_plot_long[data_plot_long$plot_names == row['plot_names'], 'index'])
    }

    data$plot_type_n <- apply(data, 1, get_plot_index)
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
write.csv(data.frame(d_long), "./data/d_long.csv", row.names = FALSE) #create word analysis csv for google colab code

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
if (calculate_sentiment) {
    pacman::p_load('sentiment.ai')

    ####### Run only first time if you are using this package #######
    #init_sentiment.ai()
    #install_sentiment.ai()

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

    #### (2.4) MAKE FREQUENCY PLOTS FOR TOPIC MODELING
    pacman::p_load('topicmodels')
    topic_modeling <- TopicModeling(d_long, n_plots, plot_names, study = 'hiring')
}


## ============================================== (3) Analysis =====================================================
"
Get main statistical effects, and run descriptive and predictive analyses
"

#### (3.1) GET MAIN EFFECTS

# Get dataframe for analysis (dat_final), with nrows = num_ss*num_plots*num_questions
dat <- gather(d_long, key = question_type, value = score, hiring_likelihood)
dat <- dplyr::select(dat, subject, plot_names, question_type, score, sentiment_score) #rows = num_ss*num_plots*num_questions

write.csv(data.frame(d_long), "./data/d_long.csv", row.names = FALSE) #create word analysis csv for google colab code
write.csv(data.frame(dat), "./data/dat.csv", row.names = FALSE) #create word analysis csv for google colab code

main_effects <- GetMainEffects(dat, n_plots, plot_names, my_embeddings, data_plot_long)
pdf(file = "linear_vs_quadratic_fit.pdf", width = 13, height = 6.5)
#plot(main_effects)
#dev.off()


# Create a dataframe of features and subject scores 
score_features_df <- CreateDataFeaturesDF(d_long, dat, features, n_after_exclusions, num_subjects_and_plots)

##### (3.3) RUN PREDICTIVE ANALYSES

fold_amount <- 10
n_reps <- 10
cv_result <- CrossValidationAnalysis(score_features_df, fold_amount = fold_amount, dep_var = 'hiring_likelihood', n_reps = n_reps, load_results = TRUE)
pdf(file = paste0("./plots/analysis_plots/cv_fold_amt=", fold_amount, "n_reps=", n_reps, ".pdf"), width = 15, height = 9)
plot(cv_result[[1]])
dev.off()


print("**** For correlations across studies, please run 'between_experiment_analyses/analysis.R' ****")
## =========================================== (4) Move Files ====================================================

plot_files <- list.files(pattern = c("(.pdf|.png)"))
file.move(plot_files, "./plots/analysis_plots", overwrite = TRUE)
analysis_files <- list.files(pattern = c("word_analysis.csv|embeddings.csv|correlations.csv"))
file.move(analysis_files, "./data", overwrite = TRUE)

##=====##
## END ##
##=====##