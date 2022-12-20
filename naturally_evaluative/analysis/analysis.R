rm(list = ls())

# Set working directory to current file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source('../../tools/Lifelines_Generate_Plots.R')
source('../../tools/common_functions.R')


# Import libraries
if (!require(pacman)) { install.packages("pacman") }
pacman::p_load('plotrix', #for standard error 
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
               'stopwords', #remove stopwords 
               'wordcloud', #visualize wordclouds for topic models 
               'lme4', #run mixed effects linear regression
               'lmerTest', #used in conjunction with lme4; get p-values
               'robustHD', #for the standardize function
               'corrplot', #for corrplot()
               'tidyr', #for gather(), which takes multiple columns and collapses them into key-value pairs
               'tidyverse', #used in conjunction with tidyr; contains dplyr, used for select(); load last because of conflict!
               'slam', #utility functions for sparse matrices 
               'broom', #install separately if does not work 
               'filesstrings' #create and move files
                               #'sentiment.ai'
)

PerformExclusions <- function(data) {
    "
    Excludes participants if they do not finish the survey, finished it too quickly (under 120 seconds), 
    gave duplicate answers, or failed important attention and comprehension checks.
    Input: data   #'lifelines_data.csv'; num_rows = num_ss
    Output: data after it has been 'cleaned'
    "

    # Exclude those who did not finish the survey
    data <- subset(data, (data$Finished == TRUE))
    n_before_exclusions <- dim(data)[1] #296 

    # Exclude those who finished it in less than 2 minutes
    data <- subset(data, (data$Duration..in.seconds. > 120))

    # Exclude those who gave the same exact answers to sentence/word generation questions across lifelines 
    sentence_cols <- data[, grep("sent_gen", colnames(data), value = TRUE)]
    sentence_dups <- sentence_cols[apply(sentence_cols, 1, function(x) length(unique(x[!is.na(x)])) == 1),]
    data <- anti_join(data, sentence_dups, by = grep("sent_gen", colnames(data), value = TRUE))

    word_cols <- data[, grep("word_gen", colnames(data), value = TRUE)]
    word_dups <- word_cols[apply(word_cols, 1, function(x) length(unique(x[!is.na(x)])) == 1),]
    data <- anti_join(data, word_dups, by = grep("word_gen", colnames(data), value = TRUE))

    #(1) attention checks

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
        data$comp_check_3 == 'They were highly unstressed early in their customer experience, then highly stressed later in their customer experience'
                                    ), 0, 1)

    #(2) comprehension questions

    #Perform second round of comprehension checks, if they failed the first
    data$comp_check <- ifelse(((is.na(data$comp_check_4 == TRUE))
        &
        (data$comp_check_7 == 'Happiness') &
        (data$comp_check_8 == 'Customer Touchpoint') &
        (data$comp_check_9 == 'Give a one-word summary of the customer journey')
        |
        ((data$comp_check_4 == 0) &
            (data$comp_check_5 == 80)
            &
            (data$comp_check_6 == 'They were highly stressed early in their customer experience, then highly unstressed later in their customer experience') &
            (data$comp_check_7 == 'Happiness') &
            (data$comp_check_8 == 'Customer Touchpoint') &
            (data$comp_check_9 == 'Give a one-word summary of the customer journey')
        )), 0, 1)

    # Exclude those who failed either attention or comprehension checks
    data <- subset(data, (data$attention_check == 0) & (data$comp_check == 0))

    # Number of subjects after exclusions
    n_after_exclusions <- dim(data)[1] #218
    print(paste0("Number of participants excluded: ", n_before_exclusions - dim(data)[1]))

    print('Mean age:')
    print(mean(as.numeric(data$age), trim = 0, na.rm = TRUE)) ## mean age

    print('% Female:')
    print(table(data$gender)[1] / sum(table(data$gender))) ## percentage of females


    data$n_after_exclusions <- n_after_exclusions

    return(data)
}


Preprocess <- function(data, n_plts, plt_names) {
    " 
    Since each plot is shown within-subjects, Qualtrics spits out data in wide format
    Let's convert it to long format, so that we have a row for every plot type
    Input: dataframe with number of rows = n_subjects, n_plots, plot_names 
    Output: dataframe with number of rows = n_subjects*n_plot_types 
    "

    # data <- data_clean 
    # n_plts <- n_plots 
    # plt_names <- plot_names

    # Define new data frame that we'll extract preprocessed data into

    # Define row and column names
    data_subset <- which(colnames(data) == "lr_word_gen_1"):which(colnames(data) == "lrsfer_sent_gen") #35:88
    last_cols <- (which(colnames(data) == "lrsfer_sent_gen") + 1):ncol(data) #89:106

    column_names <- c('plot_names', 'word_gen', 'sentence_gen', 'subject')

    df <- array(0, dim = c((nrow(data) * n_plts), length(column_names)))
    df <- as.data.frame(df, stringsAsFactors = FALSE)
    colnames(df) <- column_names

    # Turning wide format into long format, then inserting the answers into the 'df' dataframe
    final_data <- as.data.frame(t(data[data_subset])) #switch rows and columns in preparation for 'gather,' which collects info by columns
    long_data <- gather(final_data, key = "subject", value = "answers")["answers"] #gather the answers of the subjects into one long column 

    df[1] <- plt_names #plot_names
    df[2] <- long_data[seq(1, nrow(long_data), 2),] #sentence_gen
    df[3] <- long_data[seq(2, nrow(long_data), 2),] #word_gen
    df[4] <- rep(1:dim(data)[1], each = n_plts) #subject

    # Merge good data with first and last halves of the original data
    data <- cbind(data[rep(seq_len(nrow(data)), each = n_plts), 1:n_plts], df, data[rep(seq_len(nrow(data)), each = n_plts), last_cols])

    return(data)
}


##================================================================================================================
##FUNCTIONS FOR SENTIMENT ANALYSIS##
##================================================================================================================


Get_sentence_sentiment <- function(dat_long, n_plts) {
    "
    Create funtion to get sentiment score means and standard errors for sentences by condition: eval or non-eval   
    Input: data_long, n_plots 
    Output: average mean and standard error sentiment scores for sentences by condition, sorted by the same 27 lifelines  
            AND individual participant responses
    "

    # dat_long <- data_long 
    # n_plts <- n_plots

    # Clean words 
    condition_gen <- dat_long$sentence_gen
    condition_responses <- tolower(condition_gen) #make all words in each sentence lowercase 
    condition_clean <- gsub("[^[:alnum:][:space:]]", "", condition_responses) #sentence: keep only alphanumeric characters and spaces 

    # condition_clean <- removeWords(condition_clean, stopwords("en")) #remove stopwords like "I", "me", "my", "the", etc.
    condition_clean <- gsub("\\s+", " ", condition_clean) #remove extra whitespace 

    # Sort words by plot type
    sentiment_sorted <- c()
    for (i in 1:n_plts) {
        sentiment_sorted[[i]] <- condition_clean[seq(i, length(condition_clean), n_plts)]
    }

    # Get means and standard errors of words for every plot 
    sentiment_summary <- c()
    sent_scores <- c()
    for (i in 1:n_plts) {
        sent_score <- sentiment_score(sentiment_sorted[[i]])
        sent_scores <- append(sent_scores, sent_score)
        sentiment_summary[[i]] <- c(mean(sent_score), sd(sent_score))
    }

    sentiment_list <- list(sentiment_summary, sent_scores, sentiment_sorted)
    return(sentiment_list)
}


Get_word_sentiment <- function(dat_long, n_plts) {
    "
    Create funtion to get sentiment score means and standard errors for words by condition: eval or non-eval   
    Input: data_long, n_plots 
    Output: sentiment scores for words, sorted by the same 27 lifelines and individual participant responses
    "

    # Clean words 
    condition_gen <- dat_long$word_gen
    condition_responses <- word(tolower(condition_gen), 1) #make all words lowercase, AND collect only the first word of a given sentence
    condition_clean <- gsub("[^a-z]", "", condition_responses) #get rid of numbers and special characters, leaving only letters a-z

    # Sort words by plot type
    sentiment_sorted <- c()
    for (i in 1:n_plts) {
        sentiment_sorted[[i]] <- condition_clean[seq(i, length(condition_clean), n_plts)]
    }

    # Get means and standard errors of words for every plot 
    sentiment_summary <- c()
    sent_scores <- c()
    for (i in 1:n_plts) {
        sent_score <- sentiment_score(sentiment_sorted[[i]])
        sent_scores <- append(sent_scores, sent_score)
        sentiment_summary[[i]] <- c(mean(sent_score), sd(sent_score))
    }

    sentiment_list <- list(sentiment_summary, sent_scores, sentiment_sorted)
    return(sentiment_list)
}


Get_sentiment_scores <- function(dat_long, plt_names, n_plts, n_ss) {
    "
    Calls Get_sentence_sentiment() and Get_word_sentiment()
    Get sentiment scores means and standard errors for each question type, sorted by plot type. 
    Input: data_long, plot_names, n_plots, n_after_exclusions 
    Output: data frame for average mean and standard error sentiment scores AND individual participant responses  
    "
    # 1. Organize means and standard errors (for use in plotting)

    # Get sentiment scores means and standard errors for each question type, sorted by plot type. 
    sentence_data <- Get_sentence_sentiment(dat_long, n_plts)[[1]]
    word_data <- Get_word_sentiment(dat_long, n_plts)[[1]]

    sentiment_df <- data.frame(plot_names = plt_names,
                               sentence_mean = unlist(sentence_data)[c(TRUE, FALSE)], sentence_sd = unlist(sentence_data)[c(FALSE, TRUE)],
                               word_mean = unlist(word_data)[c(TRUE, FALSE)], word_sd = unlist(word_data)[c(FALSE, TRUE)])

    # Make the data frame long 
    # Get mean sentiment scores and rename conditions 
    sentiment_mean <- sentiment_df %>%
        gather(key = question_type, #create separate entries for each question type
               value = mean, sentence_mean, word_mean) %>%
        mutate(question_type = sub("_mean", "", question_type)) #rename question_type as fit

    # Compile all standard error values
    sentiment_sd <- gather(sentiment_df, key = question_type, value = sd, sentence_sd, word_sd)

    # Bind the standard error column to the rest of the data frame
    sentiment_df_long <- cbind(dplyr::select(sentiment_mean, plot_names, question_type, mean), sd = sentiment_sd$sd)

    # ----

    # 2. Organize all sentiment scores for every subject, by plot type (for use in linear mixed effects regression)

    # Get sentiment scores  
    all_sentence <- Get_sentence_sentiment(dat_long, n_plts)[[2]]
    all_word <- Get_word_sentiment(dat_long, n_plts)[[2]]

    # Combine into one data frame 
    col_names <- c('plot_names', 'question_type', 'sentiment_score', 'subject')

    all_sentiment_df_long <- array(0, dim = c(nrow(dat_long) * 2, length(col_names))) #nrow(dat_long)*2 = n_after_exclusions*n_plots*2 questions
    all_sentiment_df_long <- as.data.frame(all_sentiment_df_long, stringsAsFactors = FALSE)
    colnames(all_sentiment_df_long) <- col_names

    # Assign plot names 
    all_sentiment_df_long[1] <- rep(plt_names, each = n_ss)

    # Assign conditions 
    all_sentiment_df_long[2] <- rep(c("sentence", "word"),
                                    each = n_ss * n_plts)

    # Assign values 
    all_sentiment_df_long[3] <- c(unlist(all_sentence), unlist(all_word)) #sentence score 

    # Assign subjects
    all_sentiment_df_long[4] <- rep(1:n_ss, times = n_plts)

    sentiment_data <- list(sentiment_df_long, all_sentiment_df_long)

    vt <- var.test(sentiment_data[[2]][sentiment_data[[2]]$question_type == 'sentence', 'sentiment_score'],
                   sentiment_data[[2]][sentiment_data[[2]]$question_type == 'word', 'sentiment_score'])
    t.test(sentiment_data[[2]][sentiment_data[[2]]$question_type == 'sentence', 'sentiment_score'],
           sentiment_data[[2]][sentiment_data[[2]]$question_type == 'word', 'sentiment_score'], paired = TRUE)

    return(sentiment_data)
}

Get_sentiment_barplot <- function(sentiment_data, n_plts, plt_names, s1_order) {
    # Get sentiment data 
    sentiment_stats <- sentiment_data[[1]]
    sentiment_stats$plot_names <- as.factor(sentiment_stats$plot_names)

    # Reorder the sentiment scores according to customer journeys study
    sentiment_stats <- sentiment_stats %>%
        arrange(factor(plot_names, levels = s1_order))

    sentiment_stats$plot_names <- factor(sentiment_stats$plot_names, levels = s1_order) #set the reordered data using plot names (for plotting)

    # Plot bar graph 
    ques_cond <- unique(sentiment_stats$question_type) #get the question type names 

    sentiment_bar <- ggplot(sentiment_stats, aes(x = plot_names, y = mean, fill = question_type)) +
        geom_bar(position = "dodge", stat = "identity") +
        geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = .2,
                      position = position_dodge(.9)) +
        ggtitle(paste0("Mean Sentiment Scores")) +
        xlab(paste0("Customer Journey Plots")) +
        ylab("Mean Sentiment Score") +
        theme(
            plot.title = element_blank(), #element_text(size = 31, face="bold", hjust = 0.5, color = "black"),
            legend.title = element_blank(), #element_text(size = 30, color = "black"),
            legend.text = element_text(color = "black", size = 28),
            legend.position = "top",
            legend.title.align = 0.5,
            text = element_text(size = 25, color = "black"),
            axis.title.y = element_text(size = 30, face = "bold", color = "black"),
            axis.title.x = element_text(size = 30, face = "bold", color = "black"),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank()
        ) +
        scale_fill_manual(
            name = "Judgment Type",
            breaks = ques_cond,
            labels = str_to_title(gsub("_", " ", ques_cond)),
            values = c("darkorange4", "darkorange"),   #"turquoise4", "turquoise2"; #c("gray20", "gray40", "gray60", "gray80"), #c("#56B4E9", "#009E73", "firebrick3", "darkorange"),
            guide = guide_legend(title.position = "top")
        )

    # Return items 
    bar_plot_list <- list(sentiment_bar, s1_order)
    return(bar_plot_list)
}


PlotAxisLabels <- function(plt_names, my_equation, my_plot, plot_num) {
    "
    Make a plotter function that produces 'clean' (no labels) version of individual images 
    for the x-axis. Then, plot the images in order of sentiment results.
    Input: plot_names, my_equations, sentiment_bar_raw, sentiment_bar_icons 
    Output: plot labels (the little lifeline icons)
    "

    # plt_names <- plot_names
    # my_equation <- my_equations 
    # my_plot <- sentiment_bar_raw 
    # plot_num <- sentiment_bar_icons 

    # Make "clean" (no labels) version of individual images for x-axis
    Plotter_2 <- function(equation, x_range, y_range) {
        plot(equation, lwd = 30, xlim = c(start_age, end_age), ylim = c(0, end_y_axis), main = "",
             xlab = "", ylab = "", axes = FALSE, col = "firebrick3")

        return(Plotter_2)
    }

    # Print the images that will comprise the x-axis
    for (i in 1:length(my_equation)) { #print individual plots
        png(file = paste0(plt_names[i], "_plot.png", ""))
        sapply(my_equation[i], Plotter_2)
        dev.off()
    }

    # Assemble images in order 
    plot_icons <- axis_canvas(my_plot, axis = 'x') +
        draw_image(paste0(plot_num[1], "_plot.png"), x = 0.5) +
        draw_image(paste0(plot_num[2], "_plot.png"), x = 1.5) +
        draw_image(paste0(plot_num[3], "_plot.png"), x = 2.5) +
        draw_image(paste0(plot_num[4], "_plot.png"), x = 3.5) +
        draw_image(paste0(plot_num[5], "_plot.png"), x = 4.5) +
        draw_image(paste0(plot_num[6], "_plot.png"), x = 5.5) +

        draw_image(paste0(plot_num[7], "_plot.png"), x = 6.5) +
        draw_image(paste0(plot_num[8], "_plot.png"), x = 7.5) +
        draw_image(paste0(plot_num[9], "_plot.png"), x = 8.5) +
        draw_image(paste0(plot_num[10], "_plot.png"), x = 9.5) +
        draw_image(paste0(plot_num[11], "_plot.png"), x = 10.5) +
        draw_image(paste0(plot_num[12], "_plot.png"), x = 11.5) +
        draw_image(paste0(plot_num[13], "_plot.png"), x = 12.5) +
        draw_image(paste0(plot_num[14], "_plot.png"), x = 13.5) +
        draw_image(paste0(plot_num[15], "_plot.png"), x = 14.5) +
        draw_image(paste0(plot_num[16], "_plot.png"), x = 15.5) +
        draw_image(paste0(plot_num[17], "_plot.png"), x = 16.5) +

        draw_image(paste0(plot_num[18], "_plot.png"), x = 17.5) +
        draw_image(paste0(plot_num[19], "_plot.png"), x = 18.5) +
        draw_image(paste0(plot_num[20], "_plot.png"), x = 19.5) +
        draw_image(paste0(plot_num[21], "_plot.png"), x = 20.5) +
        draw_image(paste0(plot_num[22], "_plot.png"), x = 21.5) +
        draw_image(paste0(plot_num[23], "_plot.png"), x = 22.5) +
        draw_image(paste0(plot_num[24], "_plot.png"), x = 23.5) +
        draw_image(paste0(plot_num[25], "_plot.png"), x = 24.5) +
        draw_image(paste0(plot_num[26], "_plot.png"), x = 25.5) +
        draw_image(paste0(plot_num[27], "_plot.png"), x = 26.5)

    return(plot_icons)
}

##================================================================================================================
##FUNCTIONS FOR DATA ANALYSIS##
##================================================================================================================


CV_plotter <- function(results_df, x_order, results_order, ques_type, x_labels, sum_satisfaction, sum_pd) {
    "
    What this function does: creates a grouped box plot of the cross-validated prediction results
    Inputs: results_df, x_order, results_order, ques_type, x_labels, sum_satisfaction, sum_pd
    Output: a boxplot of participant rating predictions with either principal components or predictors
    "

    grouped_box_plot <- ggplot() +
        scale_x_discrete() +
        geom_rect(aes(xmin = 0.4, xmax = Inf, ymin = sum_satisfaction["1st Qu."], ymax = sum_satisfaction["3rd Qu."]),
                  alpha = 1, fill = "gray60") + #"dodgerblue4") + # #56B4E9
        geom_rect(aes(xmin = 0.4, xmax = Inf, ymin = sum_pd["1st Qu."], ymax = sum_pd["3rd Qu."]),
                  alpha = 1, fill = "gray60") + #"forestgreen") + # #009E73
        geom_hline(yintercept = 0, color = "gray60") +
        geom_boxplot(data = results_df, aes(x = x_order, y = results_order, fill = ques_type), outlier.shape = NA) +
        ggtitle(paste0("Satisfaction and Desirability\nPredictions with ", x_labels)) +
        xlab(x_labels) +
        ylab("Prediction Accuracy\n(Cross-Validated Pearson's r)") +
        scale_y_continuous(breaks = round(seq(-1, 1, by = 0.2), 1)) +
        scale_fill_manual(
            name = "Judgment Type",
            breaks = c("satisfaction_results", "pd_results"),
            labels = c("Satisfaction", "Personal Desirability"),
            values = c("#56B4E9", "#009E73"),
            guide = guide_legend(title.position = "top")) +
        theme_bw() +
        if (x_labels == "Predictors") {
            theme(element_blank(),
                  plot.title = element_blank(), #element_text(color = "black", size=30, face = "bold", hjust = 0.5),
                  text = element_text(color = "black", size = 25),
                  axis.title.y = element_text(color = "black", size = 25, face = "bold"),
                  axis.title.x = element_text(color = "black", size = 25, face = "bold"),
                  axis.text.x = element_text(color = "black", angle = 60, vjust = 1, hjust = 1),
                  legend.title = element_blank(), #element_text(color = "black", size=25),
                  legend.text = element_text(color = "black", size = 25),
                  legend.position = "top",
                  legend.title.align = 0.5)
        } else {
            theme(element_blank(),
                  plot.title = element_blank(), #element_text(color = "black", size=30, face = "bold", hjust = 0.5),
                  text = element_text(color = "black", size = 25),
                  axis.title.y = element_text(color = "black", size = 25, face = "bold"),
                  axis.title.x = element_text(color = "black", size = 25, face = "bold", margin = margin(t = 20, r = 0, b = 0, l = 0)),
                  axis.text.x = element_text(color = "black", size = 25),
                  legend.title = element_blank(), #element_text(color = "black", size=25),
                  legend.text = element_text(color = "black", size = 25),
                  legend.position = "top",
                  legend.title.align = 0.5) }


    return(grouped_box_plot)
}


##======##
## MAIN ##
##======##

##================================================================================================================
# Read Data
d_raw <- read.csv("./data/data.csv")
d_s1 <- read.csv("../../satisfaction_of_a_customer_journey/analysis/data/dat.csv")
d_s1_mean <- aggregate(d_s1, list(d_s1$word.plot_names, d_s1$word.question_type), mean)
d_s1_m <- aggregate(d_s1, list(d_s1$word.plot_names), mean)
d_s1_order <- d_s1_m[order(d_s1_m$word.sentiment_score),]

d_s1_mean_satis <- d_s1_mean[d_s1_mean$Group.2 == 'satisfaction',]
d_s1_order_satisfaction <- d_s1_mean_satis[order(d_s1_mean_satis$word.sentiment_score),]

d_s1_mean_pd <- d_s1_mean[d_s1_mean$Group.2 == 'personal_desirability',]
d_s1_order_pd <- d_s1_mean_pd[order(d_s1_mean_pd$word.sentiment_score),]

# Process Data
d <- PerformExclusions(d_raw) #num_rows = num_ss
n_after_exclusions <- d$n_after_exclusions[1]
n_subjects_and_plots <- n_after_exclusions * 27
n_plots <- 27
data_long <- Preprocess(d, n_plots, plot_names) #num_rows = num_ss*num_plots [to see data without exclusions, replace data_clean with data]


##================================================================================================================
# Analyze Sentiment
calculate_sentiment <- FALSE
if (calculate_sentiment) {
    sentiment_df <- Get_sentiment_scores(data_long, plot_names, n_plots, n_after_exclusions)
    write.csv(sentiment_df[[1]], "./data/sentiment_scores_1.csv")
    write.csv(sentiment_df[[2]], "./data/sentiment_scores_2.csv")
} else {
    sentiment_df <- list(read.csv("./data/sentiment_scores_1.csv"),
                         read.csv("./data/sentiment_scores_2.csv"))
}


# Plot Sentiment Bar Plot
sentiment_bar_list <- Get_sentiment_barplot(sentiment_df, n_plots, plot_names, d_s1_order[, 1])
sentiment_bar_e4 <- PlotAxisLabels(plot_names, my_equations, sentiment_bar_list[[1]], sentiment_bar_list[[2]])

pdf(file = paste0("sentiment_barplot.pdf"), width = 17, height = 8)
ggdraw(insert_xaxis_grob(sentiment_bar_list[[1]], sentiment_bar_e4, position = "bottom"))
dev.off()


##================================================================================================================
# Get Main Effects
ordered_d <- sentiment_df[[1]] %>%
    arrange(factor(plot_names, levels = d_s1_order[, 1]))

print("Do sentiment scores from study 1 correlate with study S1 sentiment results (word)?")
cor.test(d_s1_order$word.sentiment_score, ordered_d[ordered_d$question_type == 'word', 'mean'])

print("Do sentiment scores from study 1 correlate with study S1 sentiment results (sentence)?")
cor.test(d_s1_order$word.sentiment_score, ordered_d[ordered_d$question_type == 'sentence', 'mean'])

print("Do satisfaction scores from study 1 correlate with study S1 sentiment results (word)?")
cor.test(d_s1_order_satisfaction$word.score, ordered_d[ordered_d$question_type == 'word', 'mean'])

print("Do satisfaction scores from study 1 correlate with study S1 sentiment results (sentence)?")
cor.test(d_s1_order_satisfaction$word.score, ordered_d[ordered_d$question_type == 'sentence', 'mean'])

print("Do personal desirability scores from study 1 correlate with study S1 sentiment results (word)?")
cor.test(d_s1_order_pd$word.score, ordered_d[ordered_d$question_type == 'word', 'mean'])

print("Do personal desirability scores from study 1 correlate with study S1 sentiment results (sentence)?")
cor.test(d_s1_order_pd$word.score, ordered_d[ordered_d$question_type == 'sentence', 'mean'])


vt <- var.test(sentiment_df[[2]][sentiment_df[[2]]$question_type == 'sentence', 'sentiment_score'],
               sentiment_df[[2]][sentiment_df[[2]]$question_type == 'word', 'sentiment_score'])

print("Sentence vs. Word:")
t.test(sentiment_df[[2]][sentiment_df[[2]]$question_type == 'sentence', 'sentiment_score'],
       sentiment_df[[2]][sentiment_df[[2]]$question_type == 'word', 'sentiment_score'])

n_ss <- dim(data_long)[1] / n_plots

data_long[,'sentence'] <- sentiment_df[[2]][sentiment_df[[2]]$question_type=='sentence', 'sentiment_score']
data_long[,'word'] <- sentiment_df[[2]][sentiment_df[[2]]$question_type=='word', 'sentiment_score']
##================================================================================================================
# Move Files

# Move Data CSV 
dir.create("./data")
data_files <- list.files(pattern = "(.csv)")
file.move(data_files, "data", overwrite = TRUE)


# Move Plots 
dir.create("./plots")
plot_files <- list.files(pattern = c("(.pdf|.png)"))
file.move(plot_files, "analysis_plots", overwrite = TRUE)


##=====##
## END ##
##=====##