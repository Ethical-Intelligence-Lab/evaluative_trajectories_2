rm(list = ls())

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #set working directory to current directory

## Import libraries
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
               # 'filesstrings', #create and move files
               'recipes', #for feature engineering
               'caret', #for automating tuning process
               'tidyr', #for gather(), which takes multiple columns and collapses them into key-value pairs
               'tidyverse', #used in conjunction with tidyr; contains dplyr, used for select(); load last because of conflict!
               'slam', #utility functions for sparse matrices
               'broom',
               'rcompanion',
               'pscl',
               'splitstackshape'
)

source('./plotting.R')
source('../../tools/common_functions.R')

ProcessForPlots <- function(data, n_plots, plot_names) {
    "
    Create a new data frame to store the willing and PD scores by ascending willing scores
    Input: data_long, n_plots, plot_names   #num_rows = num_ss*num_plots
    Output: data_plot_long (in order of ascending willing scores)   #num_rows = num_plots*num_questions
    "

    # Get mean scores for all questions, then reshape data from wide to long format
    stats <- Get_stats(data, n_plots)
    data_plot <- data.frame(cluster_names = cluster_names,
                            willing_score_avg = unlist(stats)[c(TRUE, FALSE, FALSE)],
                            willing_score_sd = unlist(stats)[c(FALSE, TRUE, FALSE)],
                            raffle_percentage = unlist(stats)[c(FALSE, FALSE, TRUE)]
    )
    data_plot_sorted <- data_plot[order(data_plot$willing_score_avg),] #order by willing
    data_plot_long <- gather(data_plot_sorted, key = question_type, #create separate entries for each question type, i.e., num_plots*num_questions
                             value = score, willing_score_avg)

    # Compile all standard deviation values
    stan_dev <- gather(data_plot_sorted, key = question_type,
                       value = sd, willing_score_sd)

    # Bind the SE column to the rest of the dataframe
    data_plot_long <- cbind(dplyr::select(data_plot_long, cluster_names, question_type, score), sd = stan_dev$sd, raffle_percentage = data_plot_sorted$raffle_percentage)
    data_plot_long$cluster_names <- factor(data_plot_long$cluster_names, levels = data_plot_long$cluster_names[1:n_clusters])

    return(data_plot_long)
}


TransformWTP <- function(data_long) {
    "
    Tranform willingness to pay measure
    Input: data_long
    Output: data_long
    "

    # Fix column name
    names(data_long)[names(data_long) == "willingness to pay"] <- "willingness_to_pay"

    # Turn willingness to pay to numeric
    wtp <- as.numeric(unlist(data_long["willingness_to_pay"])) #turn to numeric
    hist(wtp) #see that amounts are right-skewed
    shapiro.test(wtp) #significantly different from a normal distribution, p < .001

    wtp_new <- log(wtp) + 1 #transform wtp
    hist(wtp_new) #see distribution; now normal

    # Insert back into data frame, then turn all non-numerical values into NAs
    data_long["willingness_to_pay"] <- wtp_new
    data_long["willingness_to_pay"][!is.finite(unlist(data_long["willingness_to_pay"])),] <- NA

    return(data_long)
}


Get_stats <- function(data, n_plots) {
    "
    Find willing and personal desirability means and standard deviations for every plot
    Every plot repeats every 8 times, since there are 8 plots title.
    Hence the 'seq' indeces for each calculation
    Input: data_long, n_plots
    Output: equations (a list of means and standard deviations of willingness scores for every plot)
    "

    # Get means and standard deviations
    equations <- c()
    for (i in 0:(n_clusters - 1)) {
        equations[[i + 1]] <- c(mean(data[data$cluster_labels == i, 'willing']),
                                sd(data[data$cluster_labels == i, 'willing']),
                                mean(data[data$cluster_labels == i, 'picked_movie']))
    }

    return(equations)
}

##========================##
## FUNCTIONS FOR ANALYSIS ##
##========================##

GetMainEffects <- function(data, n_plots, plot_names, my_embeddings, data_plot_long) {
    print("*-*-*-*-*-*-*-*-*-*-*-* We found a significant effect of cluster type on willingness to buy *-*-*-*-*-*-*-*-*-*-*-*")
    data_plot_long$index <- 1:nrow(data_plot_long)
    get_index <- function(row) {
        return(data_plot_long[data_plot_long$cluster_names == paste0('cluster_', as.numeric(row['cluster_labels'])), 'index'])
    }

    data$cluster_n <- apply(data, 1, get_index)

    print(summary(lm(data = data, willing ~ cluster_n)))
}


CreateDataFeaturesDF <- function(data) {
    "
    Bind the three dataframes: data, sentiment score, and standardize(features), i.e., the standardized plot features.
    Input: data_long, e3_dat, features, n_after_exclusions, num_subjects_and_plots
    Output: score_features_df (which contains all of the predictors and participant scores)
    "

    score_features_df <- data
    score_features_df["willing"] <- as.data.frame(standardize(apply(score_features_df["willing"], 2, as.numeric)))
    score_features_df["subject"] <- as.data.frame(apply(score_features_df["subject"], 2, as.numeric))
    score_features_df["genre"] <- as.data.frame(as.numeric(factor(score_features_df$genre)))
    score_features_df["sentiment_score"] <- standardize(score_features_df["sentiment_score"])
    score_features_df["embeddings"] <- standardize(score_features_df["embeddings"])
    score_features_df["interestingness"] <- standardize(score_features_df["interestingness"])

    return(score_features_df)

}

simulate_f1_score <- function(dat) {
    pm <- as.numeric(dat$genre == genres[match(dat$movie_choice, movies)])
    n_participants <- dim(dat)[1] / n_plots


    f1s <- c()
    # Pick a movie randomly for each participant
    for (i in 1:10000) {
        choices <- replicate(dim(dat)[1], 0)
        for (i in 1:n_participants) {
            random_pick <- sample(1:8, 1)
            choices[random_pick + ((i - 1) * 8)] <- 1
        }

        cm_r <- confusionMatrix(as.factor(choices), as.factor(pm), mode = "everything", positive = "1")
        f1s <- append(f1s, cm_r$byClass['F1'])

    }

    print(mean(f1s))


    print("1/8 Pick:")
    cm_r <- confusionMatrix(as.factor(choices), as.factor(pm), mode = "everything", positive = "1")
    print(cm_r)

    print("All 0:")
    cm_r <- confusionMatrix(as.factor(replicate(dim(dat)[1], 0)), as.factor(pm), mode = "everything", positive = "1")
    print(cm_r)

    print("All 1:")
    cm_r <- confusionMatrix(as.factor(replicate(dim(dat)[1], 1)), as.factor(pm), mode = "everything", positive = "1")
    print(cm_r)
}

CrossValidationAnalysisForRaffle <- function(dat, n_plots, fold_amount = 10,
                                             perf_metric = "F1", weight = 7, n_reps = 10, load_results = FALSE) {
    print("----------- Running cross validation analysis for raffle choice... -----------")
    choices <- data.frame()

    pm <- as.numeric(dat$genre == genres[match(dat$movie_choice, movies)])
    dat$picked_movie <- pm

    # Get max willingness in each
    maxs <- aggregate(dat$willing, by = list(dat$Unnamed..0), max)
    maxs <- maxs[rep(seq_len(nrow(maxs)), each = 8),]

    dat$max_willing <- as.numeric(dat$willing == maxs$x)

    # Indexes of participants who do not have more than one max willingness rating
    ip <- which((colSums(matrix(dat$max_willing, nrow=8)) > 1) == FALSE)
    dat_exc <- dat[dat$Unnamed..0 %in% (ip - 1),]

    # For each participant, find highest wtp value
    for (i in dat_exc$subject) {
        curr <- dat_exc[dat_exc$subject == i,]
        max <- curr[which.max(curr$willing),]
        selected_genre <- genres[match(max$movie_choice, movies)]

        # To calculate percentage
        choices[i, 'correct'] <- selected_genre == max$genre

        # To calculate willingness average
        choices[i, 'willing'] <- curr[curr$genre == selected_genre,]$willing
    }

    choices <- na.omit(choices)
    counts <- choices %>% count(correct)
    print(paste0('Percentage of subjects that picked the movie with the highest willingness that they scored: ',
                 counts[counts$correct == TRUE,]$n / sum(counts$n)))

    print(paste0('Average willingness of the picked movie: ',
                 mean(choices$willing), ', SD: ', sd(choices$willing)))

    predictors_old <- c("embeddings", "interestingness", "sentiment_score", "max", "min", "end_value", "start_value", "number_peaks", "number_valleys", "number_extrema", "integral",
                        "d1_avg_unweight", "d1_avg_weight_prime", "d1_avg_weight_asc", "d1_avg_weight_des", "d1_avg_weight_end",
                        "d2_avg_unweight", "d2_avg_weight_prime", "d2_avg_weight_asc", "d2_avg_weight_des", "d2_avg_weight_end")
    predictors <- c("Embeddings", "Interestingness", "Sentiment Score", "Maximum", "Minimum", "End Value", "Start Value", "Num. of Peaks", "Num. of Valleys", "Num. of Extrema", "Integral",
                    "1st Deriv.", "1st Deriv. Early", "1st Deriv. Asc.", "1st Deriv. Desc.", "1st Deriv. End",
                    "2nd Deriv.", "2nd Deriv. Early", "2nd Deriv. Asc.", "2nd Deriv. Desc.", "2nd Deriv. End")
    if (colnames(dat)[10] != "Integral") {
        setnames(dat, old = predictors_old, new = predictors)
    }

    n_participants <- dim(dat)[1] / n_plots
    fit_wts = ifelse(dat$picked_movie == TRUE, weight, 1)

    if (load_results) {
        results_raffle <- read.csv(paste0('cv_results/cv_', fold_amount, '_', n_reps, '_raffle', '.csv'), row.names = 1)
    } else {
        scores_list <- list()
        for (p in 1:n_reps) {
            # Create fold_amount random partitions with equal class distribution
            print(paste("Running iteration ", p, "..."))
            set.seed(p)
            folds <- createFolds(factor(dat$picked_movie), k = fold_amount, list = TRUE)
            all_indices <- createFolds(factor(dat$picked_movie), k = 1, list = TRUE)[[1]]

            # Train on all data instead of single fold with size n_ss / fold_amount
            results_raffle <- data.frame(matrix(NA, nrow = length(predictors), ncol = fold_amount))
            rownames(results_raffle) <- predictors

            for (i in 1:length(predictors)) {
                for (j in 1:fold_amount) {  # Number of folds
                    ss_results <- c()
                    truths <- c()

                    trainIndeces <- c(setdiff(c(all_indices), c(folds[[j]])))  # Select all data except the fold
                    fitpc <- glm(picked_movie ~ get(predictors[i]), data = dat, subset = trainIndeces, family = binomial, weights = fit_wts) #fit model on subset of train data

                    for (k in 1:length(folds[[j]])) {  # Predict each participant in fold
                        testIndeces <- folds[[j]][k]
                        probabilities <- fitpc %>% predict(dat[testIndeces,], type = "response")
                        predicted.classes <- ifelse(probabilities > 0.5, 1, 0)
                        ss_results <- c(ss_results, predicted.classes)
                        truths <- c(truths, dat$picked_movie[testIndeces])
                    }

                    cm <- confusionMatrix(as.factor(c(matrix(ss_results))), as.factor(c(truths)), mode = "everything", positive = "1")

                    if (perf_metric == "Accuracy") {
                        score <- cm$overall[perf_metric]
                    } else {
                        score <- cm$byClass[perf_metric]
                    }

                    if (is.na(score)) { score <- 0 }

                    results_raffle[i, j] <- score
                }

                #print(paste('Raffle: mean predictor result,', predictors[i], ': ', mean(as.numeric(results_raffle[i,]), na.rm = TRUE)))
            }

            scores_list <- append(scores_list, list(results_raffle))
        }

        results_df <- data.frame("1" = scores_list[[1]])
        for (i in 2:n_reps) {
            col_name <- i
            results_df <- cbind(results_df, data.frame(col_name = scores_list[[i]]))
        }

        names(results_df) <- (1:n_reps * fold_amount)

        results_raffle <- results_df
        write.csv(results_raffle, paste0('cv_results/cv_', fold_amount, '_', n_reps, '_raffle', '.csv'))
    }

    # Reorder predictors according to their significance
    t_results_raffle <- as.data.frame(t(results_raffle))
    cm <- colMeans(t_results_raffle)

    print(paste0("Mean F1 Score of all for fold amount ", fold_amount, ": ", mean(as.matrix(t_results_raffle))))
    print(paste0("Max F1 Score of all for fold amount ", fold_amount, ": ", max(cm)))

    results_raffle_long <- gather(t_results_raffle, key = predictors, value = predictors_results, colnames(t_results_raffle)) #length(predictors)*n_folds
    willing_new_order <- with(results_raffle_long, reorder(predictors, predictors_results, get_mean))
    results_raffle_long["willing_new_order"] <- willing_new_order

    means_h <- aggregate(results_raffle_long$predictors_results, list(results_raffle_long$predictors), FUN = mean)
    ordered_pred <- means_h[order(-means_h$x),]
    cat("\n")
    for (i in ordered_pred$Group.1) {
        print(sprintf("%-20s *-*-*-*-* Mean Score:  %f", i, means_h[means_h$Group.1 == i, 'x']))
    }
    #-------------------------------------------------------------------------------------------------------------------
    #-------------------------------------------------------------------------------------------------------------------
    #3. Plotting
    predictors_results_ordered <- data.frame(predictors_order = results_raffle_long$willing_new_order,
                                             willing_results = results_raffle_long$predictors_results)
    predictors_results_long <- gather(predictors_results_ordered, key = question_type, value = results, willing_results)

    # Make boxplot from CV_plotter function
    predictors_plot <- CV_plotter(predictors_results_long, predictors_results_long$predictors_order,
                                  predictors_results_long$results, predictors_results_long$question_type,
                                  "Predictors", y_axis = paste0(perf_metric, " Score"), no_kfold = FALSE)

    x_labs <- ggplot_build(predictors_plot)$layout$panel_params[[1]]$
        x$
        get_labels()

    # Loop through the predictors, comparing each to a null distribution
    # willing: One-sided Wilcox test
    print("RAFFLE CHOICE: --------------------------------------------------------------------------------------")
    wilcox_test_wt_willing <- c()
    p_value_stars_willing <- c()
    wilcox_test_wt_pd <- c()
    p_value_stars_pd <- c()

    for (i in x_labs) {
        vt <- var.test(as.numeric(t_results_raffle[, i]), rep(0.222, length(t_results_raffle[, i])))

        # Shapiro test first -> if it's normal, use t-test, if not, use wilcox test
        if ((shapiro.test(as.numeric(t_results_raffle[, i]))$p.value < 0.05) ||
            (shapiro.test(as.numeric(t_results_raffle[, i]))$p.value < 0.05)) {
            wilcox_test_wt_willing[[i]] <- wilcox.test(x = t_results_raffle[, i],
                                                       y = rep(0.222, length(t_results_raffle[, i])), alternative = "greater")
        } else {
            wilcox_test_wt_willing[[i]] <- t.test(x = t_results_raffle[, i],
                                                  y = rep(0.222, length(t_results_raffle[, i])),
                                                  alternative = "greater", var.equal = vt$p.value > 0.05)
        }

        wilcox_test_wt_willing[[i]] <- wilcox.test(t_results_raffle[, i], y = rep(0.222, length(t_results_raffle[, i])),
                                                   conf.int = TRUE, alternative = "greater")  # Comparing with .222 (all 1's)
        p_value_stars_willing[i] <- stars.pval(wilcox_test_wt_willing[[i]]$"p.value") #get stars
        print(paste0("---------------", i))
        print(wilcox_test_wt_willing[[i]])
    }

    # Define heights of annotations
    willing_bottom_x <- 1.0 #x value for bottom stars
    willing_bottom_y <- 0 #y value for bottom stars

    rw <- results_raffle_long[!is.na(results_raffle_long$predictors_results),]
    means <- aggregate(rw$predictors_results, list(rw$predictors), FUN = mean)

    for (i in 1:length(predictors)) {
        predictors_plot <- predictors_plot + ggplot2::annotate("text", x = willing_bottom_x + i - 1,
                                                               y = willing_bottom_y, size = 8, label = p_value_stars_willing[[i]])
    }

    print(predictors_plot)
    return(list(predictors_plot, mean(as.matrix(t_results_raffle)), max(cm)))
}

GetInterestingness <- function(data, n_plots) {
    "
    Group 'clean' words (only lowercase letters a-z) together into individual dataframes by plot type,
    then stem words to get the root/lemmatized words only. Calculate the length of each stemmed word
    to get the 'interestingness' predictor.
    Input: data_long, n_plots
    Output: stemmed_words_df (dataframe of length of stemmed words from each participant for each plot)
    "

    # Clean words
    word_clean <- word(tolower(data$word), 1) #make all words lowercase, and collect only the first word of a given sentence
    word_gen <- gsub("[^a-z]", "", word_clean) #get rid of numbers and special characters, leaving only letters a-z

    # Group words together into individual dataframes by plot type
    equations <- c()
    for (i in 0:(n_plots - 1)) {
        equations[[i + 1]] <- word_gen[data[data$cluster_labels == i, 'X'] + 1]
    }

    # Stem words
    stemmed_words <- c()
    length_stemmed_words <- c()
    for (i in 1:n_plots) {
        stemmed_words[[i]] <- table(wordStem(equations[[i]]))
        length_stemmed_words[[i]] <- length(stemmed_words[[i]])
        stemmed_words_df <- data.frame(interestingness = unlist(length_stemmed_words))
    }

    return(stemmed_words_df)
}

##======##
## MAIN ##
##======##

oldw <- getOption("warn")
options(warn = -1)

# Define global variables
genres <- c('Horror', 'Adventure', 'Drama', 'Biography', 'Action', 'Fantasy', 'SciFi', 'Animation')
movies <- c('Knock at the Cabin', 'Dungeons and Dragons', 'She Said', 'I Wanna Dance With Somebody',
            'Mission: Impossible -- Dead Reckoning', 'Shazam! Fury of the Gods', 'Avatar: The Way of Water',
            'Puss in Boots')
plot_names <- genres
n_plots <- length(genres)
sentence_data = FALSE
n_clusters <- 27
cluster_names_sorted <- c()

# Read Data and Create Folder for Saving Files
if (sentence_data) { fname <- './data/data_sentence.csv' } else { fname <- paste0('./data/data_long_cluster_27', '.csv') }

d_long <- read.csv(fname)
dir.create("plots/analysis_plots")
dir.create("plots/analysis_plots_sentence")

## ================================= (1) Process Data =====================================

num_subjects_and_plots <- dim(d_long)[1]
n_subjects <- num_subjects_and_plots / length(genres);
print(paste0("Number after exclusions: ", n_subjects))
print("Demographics and exclusion process in 'main.ipynb'")

### (i) CREATE CSV FOR SEMANTIC ANALYSIS
analyze_words <- GetWordAnalysis(d_long, n_plots)
words_df <- as.data.frame(matrix(unlist(analyze_words), ncol = length(unlist(analyze_words[1]))))
analyze_words_df <- cbind(genres = genres, words = words_df$V1)

if (sentence_data) { fname <- "word_analysis_sentence.csv" } else { fname <- "word_analysis.csv" }
write.csv(analyze_words_df, , row.names = FALSE) #create word analysis csv for google colab code

if (sentence_data) { fname <- "./data/d_long_sentence.csv" } else { fname <- "./data/d_long.csv" }
write.csv(d_long, fname, row.names = FALSE) #create word analysis csv for google colab code

### (ii) CREATE SEMANTIC EMBEDDINGS DATAFRAME [**NB: YOU NEED TO HAVE ALREADY EXTRACTED EMBEDDINGS FOR word_analysis.csv]
if (sentence_data) { fname <- "data/embeddings_long_sentence.csv" } else { fname <- "data/embeddings_long.csv" }
my_embeddings <- read.csv(fname, header = TRUE)
embeddings_avg <- data.frame(embeddings = rowMeans(my_embeddings)) #create a dataframe


pm <- as.numeric(d_long$genre == genres[match(d_long$movie_choice, movies)])
d_long$picked_movie <- pm

### (iii) CREATE INTERESTINGNESS DATAFRAME
interestingness <- GetInterestingness(d_long, 27)

for (i in 0:26) {
    d_long[d_long$cluster_label == i, 'interestingness'] <- interestingness[i + 1,]
}

### (iv) PROCESS FOR PLOTS
d_long <- cbind(d_long, embeddings_avg)
data_plot_long = NULL

calculate_sentiment <- FALSE
if (calculate_sentiment) {
    d_long[, "sentiment_score"] <- sapply(d_long["word"], CalculateSentiment, model_type = 'ai')
    write.csv(data.frame(sentiment_score = d_long[, "sentiment_score"]), "./data/sentiment_scores.csv", row.names = FALSE)
} else {
    d_long[, "sentiment_score"] <- read.csv('./data/sentiment_scores.csv')
}

d_long$sentiment_score[is.na(d_long$sentiment_score)] <- 0


cluster_names <- c()
for (i in 0:(n_clusters - 1)) {
    cluster_names[i + 1] <- paste0('cluster_', i)
}

data_plot_long <- ProcessForPlots(d_long, n_plots, plot_names)


#### (2.1) MAKE BAR PLOT OF willing SCORES
grouped_bar_plot <- MakeGroupedBarPlot(data_plot_long)
plot_images <- MakeGroupedBarPlotImages(grouped_bar_plot, data_plot_long) #the little customer journey icons
pdf(file = paste0("./plots/analysis_plots/customer_journeys_bar_plot_", "k=", n_clusters, ".pdf"), width = 17, height = 8)
ggdraw(insert_xaxis_grob(grouped_bar_plot, plot_images, position = "bottom"))
dev.off()

grouped_bar_plot <- MakeGroupedBarPlot(data_plot_long, raffle_percentage = TRUE)
plot_images <- MakeGroupedBarPlotImages(grouped_bar_plot, data_plot_long) #the little customer journey icons
pdf(file = paste0("./plots/analysis_plots/customer_journeys_bar_plot_", "k=", n_clusters, "_raffle.pdf"), width = 17, height = 8)
ggdraw(insert_xaxis_grob(grouped_bar_plot, plot_images, position = "bottom"))
dev.off()

## ============================================== (2) Analysis =====================================================
print("-----------------------------------------------------------------------------------------------------------------------------------------")
print("*-*-*-* !!!! Exclusions and line fitting is in 'main.ipynb', and clustering analyses are in 'TimeSeriesClustering.ipynb file !!!! *-*-*-*")
print("-----------------------------------------------------------------------------------------------------------------------------------------")

#### GET MAIN EFFECTS
if (sentence_data) {
    d_long[, "sentiment_score_sentence"] <- sapply(d_long["sentence"], CalculateSentiment, model_type = 'ai', is_sentence = TRUE)
}

d_long[, "is_word"] <- lapply(d_long["word"], is.word)

if (sentence_data) { fname <- "./data/d_long_sentence.csv" } else { fname <- "./data/d_long.csv" }
write.csv(data.frame(d_long), fname, row.names = FALSE) #create word analysis csv for google colab code

#### RUN DESCRIPTIVE ANALYSES
GetMainEffects(d_long, n_plots, plot_names, my_embeddings, data_plot_long)

print("Do percentages of raffle choices correlate with mean WTP?")
print(cor.test(data_plot_long$score, data_plot_long$raffle_percentage))


# Simulating F1 Score
simulate_f1_score(d_long)

# Create a dataframe of features and subject scores
dat <- d_long
d_long <- CreateDataFeaturesDF(d_long)

d_for_comparison <- gather(dat, key = question_type, value = score, willing)
d_for_comparison <- dplyr::select(d_for_comparison, subject, cluster_labels, question_type, score, sentiment_score) #rows = num_ss*num_plots*num_questions
write.csv(data.frame(d_for_comparison), "./data/dat_for_comparison.csv", row.names = FALSE) #create word analysis csv for google colab code


##### RUN PREDICTIVE ANALYSES
fold_amount <- 10
n_reps <- 10
cv_result <- CrossValidationAnalysis(d_long, fold_amount = fold_amount, dep_var = "willing", n_reps = n_reps, load_results = TRUE)
pdf(file = paste0("./plots/analysis_plots/cv_fold_amt=", fold_amount, "n_reps=", n_reps, ".pdf"), width = 15, height = 9)
plot(cv_result[[1]])
dev.off()

avg_f1s <- c()
max_f1s <- c()
fold_amount <- 10
results_list <- CrossValidationAnalysisForRaffle(dat, n_plots, fold_amount = fold_amount,
                                                 perf_metric = "F1", load_results = TRUE)
pdf(file = paste0("./plots/analysis_plots/raffle_kfold_random_f1_", fold_amount, ".pdf"), width = 17, height = 9)
plot(results_list[[1]])
dev.off()

print("Same significant features predicting willingness to pay were significant for predicting the raffle choice")
print(cor.test(c(0.75, 0.73, 0.72, 0.71, 0.67, 0.63, 0.63, 0.56, 0.54, 0.44, 0.17),
               c(0.37, 0.36, 0.37, 0.35, 0.36, 0.34, 0.35, 0.30, 0.33, 0.28, 0.23)))

avg_f1s <- append(avg_f1s, results_list[[2]])
max_f1s <- append(max_f1s, results_list[[3]])

options(warn = oldw)

print("Correlations between studies are in 'between_experiment_analyses/analysis.R' file.")

##=====##willing
## END ##
##=====##
