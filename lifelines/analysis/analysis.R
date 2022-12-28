# Analysis script for 'Evaluative Summaries'
# Lifelines
rm(list = ls())

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
               'broom', #install separately if does not work
               'hash',
               'effsize'
)

source('../../tools/common_functions.R')
source('./plotting.R')
source('../../tools/Lifelines_Generate_Plots.R')

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

    print(paste0("Number before exclusions (those who both finished the survey and passed all attention checks): ", dim(data)[1]))

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

    # Exclude those who failed either attention or comprehension checks
    data <- subset(data, (data$attention_check == 0) & (data$comp_check == 0))

    # Number of subjects after exclusions
    n_after_exclusions <- dim(data)[1] #124
    print(paste0("Number of participants excluded: ", n_before_exclusions - dim(data)[1]))
    print('Mean age:')
    print(mean(as.numeric(data$age), trim = 0, na.rm = TRUE)) ## mean age

    print('% Female:')
    print(table(data$gender)[1] / sum(table(data$gender))) ## percentage of females


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
##FUNCTIONS FOR ANALYSIS##
##================================================================================================================
GetMainEffects <- function(data, data_long, n_plots, plot_names, my_embeddings, data_plot_long) {
    "
    This function gets various correlations and main effects of the participant data.
    Input: This function takes as input a dataframe with rows = num_ss*num_plots*num_questions.
    It also takes data_long (dataframe of averages of question ratings), n_plots (number of plots), and my_embeddings.
    Output: various correlation and linear regression results; also, linear and quadratic plots ordered by meaningfulness scores
    "

    data_plot_long$index <- 1:nrow(data_plot_long)
    get_plot_index <- function(row) {
        return(data_plot_long[data_plot_long$plot_names == row['plot_names'] &
                              data_plot_long$question_type == 'meaning_score_avg', 'index'])
    }

    data$plot_type_n <- apply(data, 1, get_plot_index) #create numeric version of plot_names
    data$score_n <- as.numeric(data$score) #create numeric version of score (which are characters)
    data$question_type_n <- as.numeric(factor(data$question_type))
    data$subject_n <- as.numeric(factor(data$subject))

    print('Did answers vary depending on question and plot type?')
    effect_mod <- lmer(score_n ~ question_type_n * plot_type_n + (1 | subject_n), data = data)
    print(summary(effect_mod))
    print('-----------------------------------------------------')

    print('Which question type scored higher?')
    t_mod <- t.test(data$score_n ~ data$question_type, paired = TRUE)
    print(t_mod)
    
    print('Cohen\'s D:')
    print(cohen.d(data$score_n ~ data$question_type))
    
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


CreateDataFeaturesDF <- function(data, features_df, n_after_exclusions, num_subjects_and_plots) {
    "
    Bind the three dataframes: data, sentiment score, and standardize(features), i.e., the standardized plot features.
    Input: data_long, dat_final, features, n_after_exclusions, num_subjects_and_plots
    Output: score_features_df (which contains all of the predictors and participant scores)
    "

    score_features_df <- cbind(data,
                               as.data.frame(do.call("rbind", replicate(n_after_exclusions, standardize(features_df), simplify = FALSE))))
    score_features_df["meaningfulness"] <- as.data.frame(standardize(apply(score_features_df["meaningfulness"], 2, as.numeric)))
    score_features_df["personal_desirability"] <- as.data.frame(standardize(apply(score_features_df["personal_desirability"], 2, as.numeric)))
    score_features_df["subject"] <- as.data.frame(apply(score_features_df["subject"], 2, as.numeric))
    score_features_df["plot_names"] <- as.data.frame(as.numeric(factor(score_features_df$plot_names)))
    score_features_df["meaningfulness"] <- standardize(score_features_df["meaningfulness"])
    score_features_df["personal_desirability"] <- standardize(score_features_df["personal_desirability"])
    score_features_df["sentiment_score"] <- standardize(score_features_df["sentiment_score"])
    score_features_df["embeddings"] <- standardize(score_features_df["embeddings"])
    score_features_df["interestingness"] <- standardize(score_features_df["interestingness"])

    return(score_features_df)

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
if (calculate_sentiment) {
    pacman::p_load('sentiment.ai')

    ####### Run only first time if you are using this package #######
    #init_sentiment.ai()
    #install_sentiment.ai()

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

    pdf(file = "data_word_clouds.pdf", width = 18, height = 8)
    plot(arranged_word_clouds)
    dev.off()

    #### (2.4) MAKE FREQUENCY PLOTS FOR TOPIC MODELING
    pacman::p_load('topicmodels')
    topic_modeling <- TopicModeling(data_long, n_plots, plot_names, "lifelines")
}


## ============================================== (3) Analysis =====================================================
"
Get main statistical effects, and run descriptive and predictive analyses
"

#### (3.1) GET MAIN EFFECTS

# Get dataframe for analysis (dat_final), with nrows = num_ss*num_plots*num_questions
dat <- gather(data_long, key = question_type, value = score, meaningfulness, personal_desirability)
dat <- dplyr::select(dat, subject, plot_names, question_type, score) #rows = num_ss*num_plots*num_questions
write.csv(data.frame(word = dat), "./data/d_long.csv", row.names = FALSE) #create word analysis csv for google colab code

# Get main statistical effects
main_effects <- GetMainEffects(dat, data_long, n_plots, plot_names, my_embeddings, data_plot_long)
pdf(file = "linear_vs_quadratic_fit.pdf", width = 13, height = 6.5)
plot(main_effects)
main_effects
dev.off()


#### (3.2) RUN DESCRIPTIVE ANALYSES

# Create a dataframe of features and subject scores 
d_long <- CreateDataFeaturesDF(data_long, features, n_after_exclusions, num_subjects_and_plots)

fold_amount <- 10
n_reps <- 10
cv_result <- CrossValidationAnalysis(d_long, fold_amount = fold_amount, dep_var = c("meaningfulness",
                                                                                    "personal_desirability"),
                                     n_reps = n_reps, load_results = TRUE)
pdf(file = paste0("./plots/analysis_plots/cv_fold_amt=", fold_amount, "n_reps=", n_reps, ".pdf"), width = 15, height = 9)
plot(cv_result[[1]])
dev.off()

## =========================================== (4) Move Files ====================================================

plot_files <- list.files(pattern = c("(.pdf|.png)"))
file.move(plot_files, "./plots/analysis_plots", overwrite = TRUE)
analysis_files <- list.files(pattern = c("(.csv)"))
file.move(analysis_files, "./data", overwrite = TRUE)

##=======##
##  END  ##
##=======##


