## Analysis script for 'Evaluative Summaries'
## Study 1: Customer Journeys

## Needed for semantic analysis: https://colab.research.google.com/drive/19cwz29yei-RwLnQ8HuEbjy74HRx94A6b?usp=sharing

## Clear workspace
# rm(list = ls())

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
               'topicmodels', #fitting topic models
               'ldatuning', #find number of topics in topic models
               'lme4', #run mixed effects linear regression
               'lmerTest', #used in conjunction with lme4; get p-values
               'robustHD', #for the standardize function
               'corrplot', #for corrplot()
               'plotrix', #for std.error()
               'psych', #for principal components analysis (PCA)
               'glmnet', #for ridge (L2) regression
               'lmtest', #for likelihood ratio test
               # 'filesstrings', #create and move files
               'multicon', #for split-half correlations
               'recipes', #for feature engineering
               'caret', #for automating tuning process
               'tidyr', #for gather(), which takes multiple columns and collapses them into key-value pairs
               'tidyverse', #used in conjunction with tidyr; contains dplyr, used for select(); load last because of conflict!
               'slam', #utility functions for sparse matrices
               'broom' #install separately if does not work
)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #set working directory to current directory
source('../../tools/common_functions.R')

ProcessForPlots <- function(data, n_plots, plot_names) {
    "
    Create a new data frame to store the enjoyment and PD scores by ascending enjoyment scores
    Input: data_long, n_plots, plot_names   #num_rows = num_ss*num_plots
    Output: data_plot_long (in order of ascending enjoyment scores)   #num_rows = num_plots*num_questions
    "

    # Get mean scores for all questions, then reshape data from wide to long format
    stats <- Get_stats(data, n_plots)
    data_plot <- data.frame(plot_names = plot_names,
                            enjoyment_score_avg = unlist(stats)[c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE)],
                            enjoyment_score_sd = unlist(stats)[c(FALSE, TRUE, FALSE, FALSE, FALSE, FALSE)],
                            pd_score_avg = unlist(stats)[c(FALSE, FALSE, TRUE, FALSE, FALSE, FALSE)],
                            pd_score_sd = unlist(stats)[c(FALSE, FALSE, FALSE, TRUE, FALSE, FALSE)],
                            wtp_score_avg = unlist(stats)[c(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE)],
                            wtp_score_sd = unlist(stats)[c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE)])
    data_plot_sorted <- data_plot[order(data_plot$enjoyment_score_avg),] #order by enjoyment
    data_plot_long <- gather(data_plot_sorted, key = question_type, #create separate entries for each question type, i.e., num_plots*num_questions
                             value = score, enjoyment_score_avg, pd_score_avg)

    # Compile all standard deviation values
    stan_dev <- gather(data_plot_sorted, key = question_type,
                       value = sd, enjoyment_score_sd, pd_score_sd)

    # Bind the SE column to the rest of the dataframe
    data_plot_long <- cbind(dplyr::select(data_plot_long, plot_names, question_type, score), sd = stan_dev$sd)

    # data_plot_long$plot_names <- factor(data_plot_long$plot_names, levels = c("linear_low", "linear_rise_sharp_fall", "linear_fall",
    #                                                                           "exp_fall_convex", "logistic_fall", "sin_rf_partial",
    #                                                                           "positive_change_partial", "linear_middle", "positive_change_full",
    #                                                                           "exp_fall_concave", "sin_rf_full", "sin_frf_partial",
    #                                                                           "sin_frf_full", "sin_rfrf", "negative_change_full",
    #                                                                           "sin_rfr_partial", "sin_fr_full", "sin_frfr",
    #                                                                           "sin_rfr_full", "linear_rise_sharp_fall_exp_rise", "exp_rise_convex",
    #                                                                           "logistic_rise", "negative_change_partial", "sin_fr_partial",
    #                                                                           "linear_rise", "exp_rise_concave", "linear_high"))

    data_plot_long$plot_names <- factor(data_plot_long$plot_names, levels = data_plot_long$plot_names[1:n_plots])

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
    Find enjoyment and personal desirability means and standard deviations for every plot
    Every plot repeats every 8 times, since there are 8 plots title.
    Hence the 'seq' indeces for each calculation
    Input: data_long, n_plots
    Output: equations (a list of means and standard deviations of enjoyment and pd scores for every plot)
    "

    # Transform all measures
    wtp_score <- as.numeric(data$willing)

    # Get means and standard deviations
    equations <- c()
    for (i in 1:n_plots) {
        equations[[i]] <- c(mean(wtp_score[seq(i, length(wtp_score), n_plots)]), sd(wtp_score[seq(i, length(wtp_score), n_plots)]))
    }

    return(equations)
}

##================================================================================================================
##FUNCTIONS FOR PLOTTING BAR CHARTS##
##================================================================================================================

MakeGroupedBarPlot <- function(data_plot_long) {
    "
    Plot the grouped bar graph in order of ascending enjoyment scores
    Input: data_plot_long
    Output: grouped_bar_plot (the grouped bar graph)
    "

    grouped_bar_plot <- ggplot(data_plot_long, aes(x = plot_names, y = score, fill = question_type)) +
        geom_bar(position = "dodge", stat = "identity") +
        geom_errorbar(aes(ymin = score - sd, ymax = score + sd), width = .2,
                      position = position_dodge(.9)) +
        ggtitle("Summarizing the enjoyment and Desirability of Different Customer Journeys") +
        xlab("Customer Journey Plots") +
        ylab("Scaled Rating") +
        scale_y_continuous(breaks = seq(0, 80, 40)) +
        theme(
            plot.title = element_blank(), #element_text(color = "black", size=30, face="bold", hjust = 0.5),
            legend.title = element_blank(), #element_text(color = "black", size=30),
            legend.text = element_text(color = "black", size = 28),
            legend.position = "top",
            legend.title.align = 0.5,
            text = element_text(color = "black", size = 25),
            axis.title.y = element_text(color = "black", size = 30, face = "bold"),
            axis.title.x = element_text(color = "black", size = 30, face = "bold"),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank()
        ) +
        scale_fill_manual(
            name = "Judgment Type",
            breaks = c("enjoyment_score_avg", "pd_score_avg"),
            labels = c("enjoyment", "Personal Desirability"),
            values = c("#56B4E9", "#009E73"),
            guide = guide_legend(title.position = "top")
        )
    return(grouped_bar_plot)
}


MakeGroupedBarPlotImages <- function(LifelinesPlot, plot_names) {
    "
    Make a plotter function that produces 'clean' (no labels) version of individual images
    for the x-axis. Then, plot the images in order of ascending satisfaction scores,
    which can be determined by the order in data_plot_long$plot_names[1:27].
    Input: grouped_bar_plot, plot_names
    Output: the plot labels for the grouped bar graph and the sentiment bar graph
    "

    # Make "clean" (no labels) version of individual images for x-axis
    Plotter_2 <- function(equation, x_range, y_range) {
        plot(equation, lwd = 30, xlim = c(start_age, end_age), ylim = c(0, end_y_axis), main = "",
             xlab = "", ylab = "", axes = FALSE, col = "firebrick3")

        return(Plotter_2)
    }

    # Print the images that will comprise the x-axis
    for (i in 1:27) { #print individual plots
        png(file = paste0(plot_names[i], "_plot.png", ""))
        sapply(equations[i], Plotter_2)
        dev.off()
    }

    # Assemble images in the order of data_plot_long$plot_names[1:27]
    plot_images <- axis_canvas(LifelinesPlot, axis = 'x')

    for (i in 27) {
        plot_images <- plot_images + draw_image(paste0(data_plot_long$plot_names[i], "_plot.png"), x = i - 0.5)
    }

    return(plot_images)
}

##================================================================================================================
##FUNCTIONS FOR PLOTTING SENTIMENT BAR PLOT##
##================================================================================================================


OrderSentimentDataframe <- function(data, n_plots, plot_names) {
    "
    Create a new data frame to store the sentiment scores by ascending enjoyment scores
    Input: data_long, n_plots, plot_names
    Output: sentiment_df_sorted (the sentiment_df ordered by levels in the function factor())
    "

    # Get the order of enjoyment scores
    stats <- Get_stats(data, n_plots)
    data_plot <- data.frame(plot_names = plot_names,
                            enjoyment_score_avg = unlist(stats)[c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE)],
                            enjoyment_score_sd = unlist(stats)[c(FALSE, TRUE, FALSE, FALSE, FALSE, FALSE)],
                            pd_score_avg = unlist(stats)[c(FALSE, FALSE, TRUE, FALSE, FALSE, FALSE)],
                            pd_score_sd = unlist(stats)[c(FALSE, FALSE, FALSE, TRUE, FALSE, FALSE)],
                            wtp_score_avg = unlist(stats)[c(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE)],
                            wtp_score_sd = unlist(stats)[c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE)])

    # Create sentiment data frame ordered by ascending enjoyment scores
    sentiment_stats <- Get_sentiment_stats(data, n_plots)
    sentiment_df <- data.frame(plot_names = plot_names,
                               mean = unlist(sentiment_stats)[c(TRUE, FALSE)],
                               sd = unlist(sentiment_stats)[c(FALSE, TRUE)])
    sentiment_df_sorted <- sentiment_df[order(data_plot$enjoyment_score_avg),]
    sentiment_df_sorted$plot_names <- factor(sentiment_df_sorted$plot_names, levels = data_plot_long$plot_names[1:n_plots])

    return(sentiment_df_sorted)
}


MakeSentimentBarPlot <- function(data, n_plots, plot_names) {
    "
    Plot the sentiment bar graph in order of ascending enjoyment scores.
    Input: data_long, n_plots, plot_names
    Output: the sentiment bar graph by ascending enjoyment scores
    "

    sentiment_df <- OrderSentimentDataframe(data, n_plots, plot_names)
    sentiment_bar_plot <- ggplot(sentiment_df, aes(x = plot_names, y = mean)) +
        geom_bar(position = "dodge", stat = "identity", fill = "darkorange") +
        geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = .2,
                      position = position_dodge(.9)) +
        ggtitle("Mean Sentiment Scores by Ascending enjoyment Scores") +
        xlab("Customer Journey Plots") +
        ylab("Mean Sentiment Score") +
        theme(
            plot.title = element_blank(), #element_text(color = "black", size=31, face="bold", hjust = 0.5),
            text = element_text(color = "black", size = 25),
            axis.title.y = element_text(color = "black", size = 30, face = "bold"),
            axis.title.x = element_text(color = "black", size = 30, face = "bold"),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank()
        )

    return(sentiment_bar_plot)
}


##================================================================================================================
##FUNCTIONS FOR ANALYSIS##
##================================================================================================================
GetMainEffects <- function(data, data_long, data_plot_long, e1b_data_plot_long, n_plots, plot_names, my_embeddings) {
    "
    This function gets various correlations and main effects of the participant data.
    Input: This function takes as input a dataframe with rows = num_ss*num_plots*num_questions.
          (e3_dat_final, d_long, data_plot_long, data_plot_long, n_plots, plot_names, my_embeddings)
    Output: various correlation and linear regression results; also, linear and quadratic plots ordered by enjoyment scores
    "

    # 1. Question & Plot Types

    data$plot_type_n <- as.numeric(factor(data$plot_names)) #create numeric version of plot_names
    data$score_n <- as.numeric(data$score) #create numeric version of score (which are characters)
    data$question_type_n <- as.numeric(factor(data$question_type, levels = unique(data$question_type)))
    data$willingness_to_pay <- as.numeric(data$willingness_to_pay) #create numeric version of willingness_to_pay
    data$subject_n <- as.numeric(factor(data$subject))

    print('Did answers vary depending on question and plot type?')
    effect_mod <- lm(data$score_n ~ data$question_type_n * data$plot_type_n + (1 | data$subject_n))
    print(summary(effect_mod))
    print('-----------------------------------------------------')

    print('Which question type scored higher?')
    t_mod <- t.test(data$score_n ~ data$question_type, paired = TRUE)
    print(t_mod)
    print(paste('Means: ', unique(data$question_type)[1], ': ', tapply(data$score_n, data$question_type, mean)[[unique(data$question_type)[1]]]))
    print(paste('Means: ', unique(data$question_type)[2], ': ', tapply(data$score_n, data$question_type, mean)[[unique(data$question_type)[2]]]))
    print(paste('SDs: ', unique(data$question_type)[1], ': ', tapply(data$score_n, data$question_type, sd)[[unique(data$question_type)[1]]]))
    print(paste('SDs: ', unique(data$question_type)[2], ': ', tapply(data$score_n, data$question_type, sd)[[unique(data$question_type)[2]]]))
    print('-----------------------------------------------------')

    print('Did sentiment scores vary depending on plot type?')
    effect_mod <- lm(data = data, sentiment_score ~ plot_type_n + (1 | subject_n))
    print(summary(effect_mod))
    print('-----------------------------------------------------')

    print('Do the sentiment scores correlate with enjoyment ratings?')
    enjoyment_corr <- cor.test(data$sentiment_score[data$question_type == "enjoyment"],
                               data$score_n[data$question_type == "enjoyment"])
    print('sentiment vs. enjoyment:')
    print(enjoyment_corr)
    print('-----------------------------------------------------')

    print('Do the sentiment scores correlate with personal desirability ratings?')
    pd_corr <- cor.test(data$sentiment_score[data$question_type == "personal_desirability"],
                        data$score_n[data$question_type == "personal_desirability"])
    print('sentiment vs. personal desirability:')
    print(pd_corr)
    print('-----------------------------------------------------')

    # 2. Do E3 enjoyment and personal desirability ratings correlate with E1 meaningfulness and personal desirability ratings?

    print('Does E3 enjoyment correlate with E1 meaningfulness ratings?')
    q1_corr <- cor.test(e1b_data_plot_long$score[e1b_data_plot_long$question_type == "meaning_score_avg"],
                        data_plot_long$score[data_plot_long$question_type == "enjoyment_score_avg"])
    print('E3 enjoyment vs E1 meaningfulness:')
    print(q1_corr)
    print('-----------------------------------------------------')

    print('Does E3 personal desirability correlate with E1 personal desirability ratings?')
    q2_corr <- cor.test(e1b_data_plot_long$score[e1b_data_plot_long$question_type == "pd_score_avg"],
                        data_plot_long$score[data_plot_long$question_type == "pd_score_avg"])
    print('E3 personal desirability vs E1 personal desirability:')
    print(q2_corr)
    print('-----------------------------------------------------')

    # 3. Willingness to Pay

    print('Did willingness to pay vary depending on plot type?')
    wtp_mod <- lm(data = data, willingness_to_pay ~ plot_type_n + (1 | subject_n))
    print(summary(wtp_mod))
    print('-----------------------------------------------------')

    print('Does willingness to pay correlate with enjoyment ratings?')
    wtp_enjoyment_corr <- cor.test(data$willingness_to_pay[data$question_type == "enjoyment"],
                                   data$score_n[data$question_type == "enjoyment"])
    print('willingness to pay vs. enjoyment:')
    print(wtp_enjoyment_corr)
    print('-----------------------------------------------------')

    print('Does willingness to pay correlate with personal desirability ratings?')
    wtp_pd_corr <- cor.test(data$willingness_to_pay[data$question_type == "personal_desirability"],
                            data$score_n[data$question_type == "personal_desirability"])
    print('willingness to pay vs. personal desirability:')
    print(wtp_pd_corr)
    print('-----------------------------------------------------')

    # 4. Difference between linear and quadratic models for enjoyment and personal desirability

    # Get the order of average enjoyment scores
    stats <- Get_stats(data_long, n_plots)
    data_plot <- data.frame(plot_names = plot_names,
                            enjoyment_score_avg = unlist(stats)[c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE)],
                            enjoyment_score_sd = unlist(stats)[c(FALSE, TRUE, FALSE, FALSE, FALSE, FALSE)],
                            pd_score_avg = unlist(stats)[c(FALSE, FALSE, TRUE, FALSE, FALSE, FALSE)],
                            pd_score_sd = unlist(stats)[c(FALSE, FALSE, FALSE, TRUE, FALSE, FALSE)],
                            wtp_score_avg = unlist(stats)[c(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE)],
                            wtp_score_sd = unlist(stats)[c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE)])
    data_plot <- data_plot[order(data_plot$enjoyment_score_avg),]
    data_plot$order_num <- 1:n_plots

    # Add a column of the difference between the average ratings of enjoyment and personal desirability for each customer journey
    data_plot["enjoyment_pd_diff"] <- data_plot["enjoyment_score_avg"] - data_plot["pd_score_avg"]
    enjoyment_score_avg <- data_plot[, "enjoyment_score_avg"]
    pd_score_avg <- data_plot[, "pd_score_avg"]
    enjoyment_pd_diff <- data_plot[, "enjoyment_pd_diff"]

    print('Does a quadratic regression fit the shape of the difference between enjoyment and desirability ratings better than a linear one does?')
    print('First, the linear fit:')
    enjoyment_pd_diff_lin <- lm(enjoyment_pd_diff ~ data_plot$order_num)
    print(summary(enjoyment_pd_diff_lin))
    linear_plot <- ggplot(enjoyment_pd_diff_lin, aes(data_plot$order_num, enjoyment_pd_diff)) +
        theme_classic() +
        geom_point() +
        stat_smooth(method = lm, formula = y ~ x) +
        theme(axis.text = element_text(color = "black", size = 20),
              axis.title.y = element_blank(),
              axis.title.x = element_blank())
    print('-----------------------------------------------------')

    print('Second, the quadratic fit:')
    enjoyment_pd_diff_quadratic <- lm(enjoyment_pd_diff ~ data_plot$order_num + I(data_plot$order_num^2))
    print(summary(enjoyment_pd_diff_quadratic))
    quadratic_plot <- ggplot(enjoyment_pd_diff_quadratic, aes(data_plot$order_num, enjoyment_pd_diff)) +
        theme_classic() +
        geom_point() +
        stat_smooth(method = lm, formula = y ~ poly(x, 2, raw = TRUE)) +
        theme(axis.text = element_text(color = "black", size = 20),
              axis.title.y = element_blank(),
              axis.title.x = element_blank())
    print('-----------------------------------------------------')

    print('Difference between linear and quadratic models:')
    enjoyment_pd_diff_lrt <- lrtest(enjoyment_pd_diff_lin, enjoyment_pd_diff_quadratic)
    print(enjoyment_pd_diff_lrt)
    print('-----------------------------------------------------')

    linear_quad <- ggarrange(linear_plot, quadratic_plot, nrow = 1, ncol = 2)
    # theme(plot.margin = margin(0.1,0.1,-0.5,0.1, "cm"))
    linear_quad <- annotate_figure(linear_quad,
                                   left = text_grob("Difference Between enjoyment and Desirability", color = "black", face = "bold", size = 20, rot = 90),
                                   bottom = text_grob("Plot Ordered by enjoyment Scores", color = "black", face = "bold", size = 20, vjust = 0.4))
    print(linear_quad)

    print('Regress enjoyment on embeddings: ')
    enjoyment_embedding_df <- cbind(enjoyment = data_long$enjoyment, my_embeddings[2:513])
    enjoyment_embedding_lm <- lm(enjoyment ~ ., data = enjoyment_embedding_df)
    #print('enjoyment vs. embeddings:')
    #print(summary(enjoyment_embedding_lm))
    # Error: 486 not defined because of singularities
    # I checked for multicollinearity with cor(my_embeddings[2:513]) but did not find any perfect correlations
    # (except for of course the correlation of a given variable with itself, which had a coefficient of 1).

    # print('Regress personal desirability on embeddings: ')
    # pd_embedding_df <- cbind(personal_desirability = data_long$personal_desirability, my_embeddings[2:513])
    # pd_embedding_lm <- lm(personal_desirability ~ ., data = pd_embedding_df)
    #print('personal desirability vs. embeddings:')
    #print(summary(pd_embedding_lm))
    # Same error as above

    # Return plots
    # plot_list <- list(linear_plot, quadratic_plot)
    return(linear_quad)

}


CreateDataFeaturesDF <- function(data) {
    "
    Bind the three dataframes: data, sentiment score, and standardize(features), i.e., the standardized plot features.
    Input: data_long, e3_dat_final, features, n_after_exclusions, num_subjects_and_plots
    Output: score_features_df (which contains all of the predictors and participant scores)
    "

    data["willing"] <- as.data.frame(apply(data["willing"], 2, as.numeric))
    data["subject"] <- as.data.frame(apply(data["subject"], 2, as.numeric))
    data["genre"] <- as.data.frame(as.numeric(factor(data$genre)))
    data["sentiment_score"] <- standardize(data["sentiment_score"])
    data["embeddings"] <- standardize(data["embeddings"])
    data["interestingness"] <- standardize(data["interestingness"])

    return(score_features_df)

}

Get_noise_ceiling <- function(dat_long, question_type, n_ss) {
    "
    Find correlation values between two randomly sample halves of the data,
    correct with the Spearman-Brown Prophecy formula (defined above), and put into a list.
    Input: data_long, question type ('enjoyment' or 'personal_desirability')
    Output: summary of the correlation results, to be used to plot noise ceiling (25th and 75th percentiles)
    "

    # Convert "willing" into numeric
    dat_long[, c("willing")] <- sapply(dat_long[, c("willing")], as.numeric)

    # Filter the scores using the "genre" column, and put into a list
    list_of_question <- c()
    for (i in plot_names) {
        list_of_question[i] <- subset(dat_long, dat_long$genre == i)['willing']
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


CV_plotter <- function(results_df, x_order, results_order, ques_type, x_labels, sum_enjoyment) {
    "
    What this function does: creates a grouped box plot of the cross-validated prediction results
    Inputs: results_df, x_order, results_order, ques_type, x_labels, sum_enjoyment
    Output: a boxplot of participant rating predictions with either principal components or predictors
    "

    grouped_box_plot <- ggplot() +
        scale_x_discrete() +
        #geom_rect(aes(xmin = 0.2, xmax = Inf, ymin = sum_enjoyment["1st Qu."], ymax = sum_enjoyment["3rd Qu."]),
        #          alpha = 1, fill = "gray60") + #"dodgerblue3") + # #56B4E9
        geom_hline(yintercept = 0, color = "gray60") +
        geom_boxplot(data = results_df, aes(x = x_order, y = results_order, fill = ques_type), outlier.shape = NA) +
        ggtitle(paste0("enjoyment and Desirability Predictions with ", x_labels)) +
        xlab(x_labels) +
        ylab("Prediction Accuracy\n(Cross-Validated Pearson's r)") +
        scale_y_continuous(breaks = round(seq(-1, 1, by = 0.2), 1)) +
        scale_fill_manual(
            name = "Judgment Type",
            breaks = c("enjoyment_results"),
            labels = c("Enjoyment"),
            values = c("#56B4E9"),
            guide = guide_legend(title.position = "top")) +
        theme_bw() +
        if (x_labels == "Predictors") {
            theme(element_blank(),
                  plot.title = element_blank(), #element_text(color = "black", size=32, face = "bold", hjust = 0.5),
                  text = element_text(color = "black", size = 25),
                  axis.title.y = element_text(color = "black", size = 30, face = "bold"),
                  axis.title.x = element_text(color = "black", size = 30, face = "bold"),
                  axis.text.x = element_text(color = "black", angle = 60, vjust = 1, hjust = 1),
                  legend.title = element_blank(), #element_text(color = "black", size=30),
                  legend.text = element_text(color = "black", size = 30),
                  legend.position = "top",
                  legend.title.align = 0.5)
        } else {
            theme(element_blank(),
                  plot.title = element_blank(), #element_text(color = "black", size=32, face = "bold", hjust = 0.5),
                  text = element_text(color = "black", size = 25),
                  axis.title.y = element_text(color = "black", size = 30, face = "bold"),
                  axis.title.x = element_text(color = "black", size = 30, face = "bold", margin = margin(t = 20, r = 0, b = 0, l = 0)),
                  axis.text.x = element_text(color = "black", size = 17.5),
                  legend.title = element_blank(), #element_text(color = "black", size=30),
                  legend.text = element_text(color = "black", size = 30),
                  legend.position = "top",
                  legend.title.align = 0.5) }


    return(grouped_box_plot)
}


MakePCAFunction <- function(score_features_df) {
    "
    Perform mixed-effects regression based on PCA-reduced features of our predictors.
    Input: score_features_df
    Output: the structure of the PCA fit, the PCA correlation values for both enjoyment and pd
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

    # 1. Fit mixed effects regression predicting enjoyment
    enjoyment_features <- lmer(data = score_features_df,
                               enjoyment ~ PC1 +
                                   PC2 +
                                   PC3 +
                                   PC4 +
                                   PC5 +
                                   (1 | subject) +
                                   (1 | plot_names))

    print('enjoyment vs. features:')
    print(summary(enjoyment_features, correlation = TRUE))


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

    #1. enjoyment
    results_enjoyment <- data.frame(matrix(NA, nrow = length(pcs), ncol = n_folds))
    rownames(results_enjoyment) <- pcs

    for (i in 1:length(pcs)) {
        for (j in 1:n_folds) {
            ss_results <- c()
            truths <- c()

            for (k in 1:n_plots) {
                trainIndeces <- indeces[(folds == j) & (folds2 != k)]
                testIndeces <- indeces[(folds == j) & (folds2 == k)]
                fitpc <- lm(enjoyment ~ get(pcs[i]), data = dat, subset = trainIndeces) #fit model on subset of train data
                ss_results <- c(ss_results, predict(fitpc, dat)[testIndeces])
                truths <- c(truths, dat$enjoyment[testIndeces])
            }

            results_enjoyment[i, j] <- cor(truths, ss_results)
        }

        print(paste('enjoyment: mean PC result,', pcs[i], ': ', mean(as.numeric(results_enjoyment[i,]), na.rm = TRUE)))
        print(paste('enjoyment: median PC result,', pcs[i], ': ', median(as.numeric(results_enjoyment[i,]), na.rm = TRUE)))
    }

    # Reorder pcs according to their significance
    t_results_enjoyment <- as.data.frame(t(results_enjoyment))
    colnames(t_results_enjoyment) <- c("PC1\nFirst Derivative\nPredictors and End Value", "PC2\nSecond Derivative\nPredictors",
                                       "PC3\nFluctuations, Embeddings,\nand Interestingness", "PC4\nNumber of Peaks\nand Extrema",
                                       "PC5\nIntegral, Sentiment,\nMax, and Min")
    results_enjoyment_long <- gather(t_results_enjoyment, key = principal_components, value = pcs_results, colnames(t_results_enjoyment)) #length(pcs)*n_folds
    enjoyment_new_order <- with(results_enjoyment_long, reorder(principal_components, pcs_results, median, na.rm = TRUE))
    results_enjoyment_long["enjoyment_new_order"] <- enjoyment_new_order

    # Get_noise_ceiling function
    #summary_enjoyment <- Get_noise_ceiling(dat_long, "willing", n_ss)

    #-------------------------------------------------------------------------------------------------------------------

    #3. Plotting
    pcs_results_ordered <- data.frame(pcs_order = results_enjoyment_long$enjoyment_new_order,
                                      enjoyment_results = results_enjoyment_long$pcs_results,
                                      pd_results = results_pd_long$pcs_results) #combine enjoyment and pd results
    pcs_results_long <- gather(pcs_results_ordered, key = question_type, value = results, enjoyment_results, pd_results)

    # Make boxplot from CV_plotter function
    #pcs_plot <- CV_plotter(pcs_results_long, pcs_results_long$pcs_order, pcs_results_long$results, pcs_results_long$question_type, "Principal Components", summary_enjoyment, summary_pd)

    # Get the labels
    x_labs <- ggplot_build(pcs_plot)$layout$panel_params[[1]]$
        x$
        get_labels()

    # Perform Wilcoxon tests and get stars for significance
    # Define empty lists
    wilcox_test_1_wt_enjoyment <- c()
    wilcox_test_2_wt_enjoyment <- c()
    p_value_stars_1_enjoyment <- c()
    p_value_stars_2_enjoyment <- c()
    wilcox_test_1_wt_pd <- c()
    wilcox_test_2_wt_pd <- c()
    p_value_stars_1_pd <- c()
    p_value_stars_2_pd <- c()

    # Loop through the pcs, comparing each to null, then PC2 vs PC3, PC3 vs PC5, PC5 vs PC1, and PC1 vs PC4
    # enjoyment: One-sided Wilcox test
    print("enjoyment: --------------------------------------------------------------------------------------")
    for (i in 1:length(pcs)) {
        pcs_index <- x_labs[i]
        pcs_index_plus_one <- x_labs[i + 1]
        wilcox_test_1_wt_enjoyment[[i]] <- wilcox.test(t_results_enjoyment[, pcs_index], y = NULL, alternative = "greater",
                                                       conf.int = TRUE, data = t_results_enjoyment)
        p_value_stars_1_enjoyment[i] <- stars.pval(wilcox_test_1_wt_enjoyment[[i]]$"p.value") #get stars

        print(paste0(x_labs[i], " --------------------------------------------------------------------------------------"))
        print(wilcox_test_1_wt_enjoyment[[i]])
    }

    # Define heights of annotations
    bottom_y <- -1.05 #y value for all bottom stars

    enjoyment_color <- "#56B4E9"
    enjoyment_bottom_x <- 1.19 #x value for bottom stars
    enjoyment_top_x <- enjoyment_bottom_x + 0.5 #x value for top stars
    enjoyment_top_y <- 1.17 #y value for top stars
    enjoyment_bracket_y <- 1.07 #y value for top bracket
    enjoyment_bracket_start <- 1.24 #x starting point for top bracket
    enjoyment_bracket_end <- 2.15 #x ending point for top bracket

    pd_color <- "#009E73"
    pd_bottom_x <- 0.813 #x value for bottom stars
    pd_top_x <- pd_bottom_x + 0.5 #x value for top stars
    pd_top_y <- 1.39 #y value for top stars
    pd_bracket_y <- 1.31 #y value for top bracket
    pd_bracket_start <- 0.85 #x starting point for top bracket
    pd_bracket_end <- 1.8 #x ending point for top bracket

    # Add to the plot: stars indicating significance
    pcs_plot <- pcs_plot +

        # One-sided Wilcox test
        ggplot2::annotate("text", x = enjoyment_bottom_x, y = bottom_y, size = 8, label = p_value_stars_1_enjoyment[[1]]) +
        ggplot2::annotate("text", x = enjoyment_bottom_x + 1, y = bottom_y, size = 8, label = p_value_stars_1_enjoyment[[2]]) +
        ggplot2::annotate("text", x = enjoyment_bottom_x + 2, y = bottom_y, size = 8, label = p_value_stars_1_enjoyment[[3]]) +
        ggplot2::annotate("text", x = enjoyment_bottom_x + 3, y = bottom_y, size = 8, label = p_value_stars_1_enjoyment[[4]]) +
        ggplot2::annotate("text", x = enjoyment_bottom_x + 4, y = bottom_y, size = 8, label = p_value_stars_1_enjoyment[[5]]) +

        # Two-sided Wilcox test (with brackets)
        geom_segment(aes(x = enjoyment_bracket_start, xend = enjoyment_bracket_end, y = enjoyment_bracket_y, yend = enjoyment_bracket_y, colour = enjoyment_color)) +
        ggplot2::annotate("text", x = enjoyment_top_x, y = enjoyment_top_y, size = 8, label = p_value_stars_2_enjoyment[[1]]) +
        geom_segment(aes(x = enjoyment_bracket_start + 1, xend = enjoyment_bracket_end + 1, y = enjoyment_bracket_y, yend = enjoyment_bracket_y, color = enjoyment_color)) +
        ggplot2::annotate("text", x = enjoyment_top_x + 1, y = enjoyment_top_y, size = 8, label = p_value_stars_2_enjoyment[[2]]) +
        geom_segment(aes(x = enjoyment_bracket_start + 2, xend = enjoyment_bracket_end + 2, y = enjoyment_bracket_y, yend = enjoyment_bracket_y, color = enjoyment_color)) +
        ggplot2::annotate("text", x = enjoyment_top_x + 2, y = enjoyment_top_y, size = 8, label = p_value_stars_2_enjoyment[[3]]) +
        geom_segment(aes(x = enjoyment_bracket_start + 3, xend = enjoyment_bracket_end + 3, y = enjoyment_bracket_y, yend = enjoyment_bracket_y, color = enjoyment_color)) +
        ggplot2::annotate("text", x = enjoyment_top_x + 3, y = enjoyment_top_y, size = 8, label = p_value_stars_2_enjoyment[[4]]) +
        scale_colour_identity()

    #-------------------------------------------------------------------------------------------------------------------

    return(pcs_plot)
}


CrossValidationAnalysisWtPredictors <- function(dat, num_subjects_and_plots, n_plots) {
    "
    Measure the performance of each of our predictors by doing cross-validated regressions, holding out
    one participant for each cross-validation step.
    Input: data_wt_PCs, data_long, n_after_exclusions, n_plots
    Output: relative importance of individual predictors and its graph
    "

    # dat <- e3_data_wt_PCs
    # dat_long <- d_long

    # n_ss <- n_after_exclusions

    predictors_old <- c("embeddings", "interestingness", "sentiment_score", "max", "min", "end_value", "number_peaks", "number_valleys", "number_extrema", "integral",
                        "d1_avg_unweight", "d1_avg_weight_prime", "d1_avg_weight_asc", "d1_avg_weight_des", "d1_avg_weight_end",
                        "d2_avg_unweight", "d2_avg_weight_prime", "d2_avg_weight_asc", "d2_avg_weight_des", "d2_avg_weight_end")
    predictors <- c("Embeddings", "Interestingness", "Sentiment Score", "Maximum", "Minimum", "End Value", "Number of\nPeaks", "Number of\nValleys", "Number of\nExtrema", "Integral",
                    "1st Derivative", "1st Derivative\nPrime", "1st Derivative\nAscending", "1st Derivative\nDescending", "1st Derivative\nEnd",
                    "2nd Derivative", "2nd Derivative\nPrime", "2nd Derivative\nAscending", "2nd Derivative\nDescending", "2nd Derivative\nEnd")
    setnames(dat, old = predictors_old, new = predictors)

    set.seed(1)
    n_folds <- num_subjects_and_plots / n_plots
    folds <- cut(seq(1, nrow(dat)), breaks = n_folds, labels = FALSE)
    folds2 <- rep(seq(1, n_plots), times = n_folds) #plot x subjects folds
    indeces <- seq(1, (n_plots * n_folds))

    #-------------------------------------------------------------------------------------------------------------------

    #1. Willingness to Pay
    results_enjoyment <- data.frame(matrix(NA, nrow = length(predictors), ncol = n_folds))
    rownames(results_enjoyment) <- predictors

    for (i in 1:length(predictors)) {
        for (j in 1:n_folds) {
            ss_results <- c()
            truths <- c()

            for (k in 1:n_plots) {  # Now
                trainIndeces <- indeces[(folds == j) & (folds2 != k)]  # Select fold j, but exclude test index (k)
                testIndeces <- indeces[(folds == j) & (folds2 == k)]

                # TODO: Predict E4 values, based on their metrics
                fitpc <- lm(willing ~ get(predictors[i]), data = dat, subset = trainIndeces) #fit model on subset of train data

                # For predicting, select a random participant
                ss_results <- c(ss_results, predict(fitpc, dat)[testIndeces])
                truths <- c(truths, dat$willing[testIndeces])
            }

            results_enjoyment[i, j] <- cor(truths, ss_results)
        }

        print(paste('enjoyment: mean predictor result,', predictors[i], ': ', mean(as.numeric(results_enjoyment[i,]), na.rm = TRUE)))
        print(paste('enjoyment: median predictor result,', predictors[i], ': ', median(as.numeric(results_enjoyment[i,]), na.rm = TRUE)))
    }

    # Reorder predictors according to their significance
    t_results_enjoyment <- as.data.frame(t(results_enjoyment))
    colnames(t_results_enjoyment) <- predictors
    results_enjoyment_long <- gather(t_results_enjoyment, key = predictors, value = predictors_results, colnames(t_results_enjoyment)) #length(predictors)*n_folds
    enjoyment_new_order <- with(results_enjoyment_long, reorder(predictors, predictors_results, median, na.rm = TRUE))
    results_enjoyment_long["enjoyment_new_order"] <- enjoyment_new_order

    # Get_noise_ceiling function
    summary_enjoyment <- Get_noise_ceiling(dat, "willing", n_folds)

    #-------------------------------------------------------------------------------------------------------------------
    #-------------------------------------------------------------------------------------------------------------------
    # Friendly person on the checkout
    # Make titles & axes biger
    # arrow
    #3. Plotting
    predictors_results_ordered <- data.frame(predictors_order = results_enjoyment_long$enjoyment_new_order,
                                             enjoyment_results = results_enjoyment_long$predictors_results)
    predictors_results_long <- gather(predictors_results_ordered, key = question_type, value = results, enjoyment_results)

    # Make boxplot from CV_plotter function
    predictors_plot <- CV_plotter(predictors_results_long, predictors_results_long$predictors_order, predictors_results_long$results, predictors_results_long$question_type, "Predictors", summary_enjoyment)

    # Get the labels
    x_labs <- ggplot_build(predictors_plot)$layout$panel_params[[1]]$
        x$
        get_labels()

    # Perform Wilcoxon tests and get stars for significance
    # Define empty lists
    wilcox_test_wt_enjoyment <- c()
    p_value_stars_enjoyment <- c()
    wilcox_test_wt_pd <- c()
    p_value_stars_pd <- c()

    # Loop through the predictors, comparing each to a null distribution
    # enjoyment: One-sided Wilcox test
    print("enjoyment: --------------------------------------------------------------------------------------")
    for (i in x_labs) {
        print(paste0(i, " --------------------------------------------------------------------------------------"))
        wilcox_test_wt_enjoyment[[i]] <- wilcox.test(t_results_enjoyment[, i], y = NULL, alternative = "greater",
                                                     conf.int = TRUE)
        p_value_stars_enjoyment[i] <- stars.pval(wilcox_test_wt_enjoyment[[i]]$"p.value") #get stars

        print(wilcox_test_wt_enjoyment[[i]])
    }


    # Define heights of annotations
    enjoyment_bottom_x <- 1.0 #x value for bottom stars
    enjoyment_bottom_y <- -1.0 #y value for bottom stars

    for (i in 1:20) {
        predictors_plot <- predictors_plot + ggplot2::annotate("text", x = enjoyment_bottom_x + i - 1, y = enjoyment_bottom_y, size = 8, label = p_value_stars_enjoyment[[i]])
    }

    return(predictors_plot)
}

##======##
## MAIN ##
##======##

# Define global variables
genres = c('Horror', 'Adventure', 'Drama', 'Biography', 'Action', 'Fantasy', 'SciFi', 'Animation')
n_plots <- length(genres)

# Read Data and Create Folder for Saving Files
d_long <- read.csv('./data/data_e4.csv')
dir.create("plots/analysis_plots")

## ================================= (1) Perform Exclusions and Process Data =====================================
"
- Perform exclusions
- Create d_long (nrows = num_ss*num_plots)
- Prepare for semantic and interestingness analyses
  - Create csv for semantic analysis
  - Create semantic embeddings dataframe
  - Create interestingness dataframe
- Create data_plot_long (nrows = num_plots*num_questions, i.e averages for plotting)
"

num_subjects_and_plots <- dim(d_long)[1]

### (i) CREATE CSV FOR SEMANTIC ANALYSIS
analyze_words <- GetWordAnalysis(d_long, n_plots)
words_df <- as.data.frame(matrix(unlist(analyze_words), ncol = length(unlist(analyze_words[1]))))
analyze_words_df <- cbind(plot_names = plot_names, words = words_df$V1)
write.csv(analyze_words_df, "word_analysis_e4.csv", row.names = FALSE) #create word analysis csv for google colab code


### (ii) CREATE SEMANTIC EMBEDDINGS DATAFRAME [**NB: YOU NEED TO HAVE ALREADY EXTRACTED EMBEDDINGS FOR word_analysis_e4.csv]
my_embeddings <- read.csv("data/embeddings_long_e4.csv", header = TRUE)
embeddings_avg <- data.frame(embeddings = rowMeans(my_embeddings)) #create a dataframe


### (iii) CREATE INTERESTINGNESS DATAFRAME
interestingness <- GetInterestingness(d_long, n_plots)


### (iv) PROCESS FOR PLOTS
d_long <- cbind(d_long, embeddings_avg)
d_long <- cbind(d_long, interestingness)
data_plot_long = NULL
#data_plot_long <- ProcessForPlots(d_long, n_plots, plot_names) #num_rows = num_plots*num_questions

## ========================================== (2) Plot Data and Save ==================================================
if (FALSE) {
    "
    Create bar plot, word clouds, and sentiment plot
    "

    #### (2.1) MAKE BAR PLOT OF enjoyment SCORES
    grouped_bar_plot <- MakeGroupedBarPlot(data_plot_long)
    plot_images <- MakeGroupedBarPlotImages(grouped_bar_plot, plot_names) #the little customer journey icons

    pdf(file = "customer_journeys_bar_plot.pdf", width = 17, height = 8)
    ggdraw(insert_xaxis_grob(grouped_bar_plot, plot_images, position = "bottom"))
    dev.off()


    #### (2.2) MAKE WORD CLOUDS (WARNING: takes ~5 minutes; feel free to skip)
    MakeWordClouds(d_long, n_plots, plot_names) #make word cloud images
    arranged_word_clouds <- ArrangeWordClouds() #arrange word clouds into a grid

    pdf(file = "customer_journeys_word_clouds.pdf", width = 18, height = 8)
    arranged_word_clouds
    dev.off()


    #### (2.3) MAKE PLOT OF SENTIMENT SCORES, ORDERED BY enjoyment SCORES
    sentiment_bar_plot <- MakeSentimentBarPlot(d_long, n_plots, plot_names)
    sentiment_plot_images <- MakeGroupedBarPlotImages(sentiment_bar_plot, plot_names) #the little customer journey icons

    pdf(file = "customer_journeys_sentiment_plot.pdf", width = 17, height = 8)
    ggdraw(insert_xaxis_grob(sentiment_bar_plot, sentiment_plot_images, position = "bottom"))
    dev.off()

    #### (2.4) MAKE FREQUENCY PLOTS FOR TOPIC MODELING
    #topic_modeling <- TopicModeling(d_long, n_plots, plot_names)

}

## ============================================== (3) Analysis =====================================================

"
Get main statistical effects, and run descriptive and predictive analyses
"

#### (3.1) GET MAIN EFFECTS

# Get dataframe for analysis (e4_dat_final), with nrows = num_ss*num_plots*num_questions
#e4_dat <- gather(d_long, key = question_type, value = score, enjoyment, personal_desirability)
#e4_dat <- dplyr::select(e4_dat, subject, plot_names, question_type, score, willingness_to_pay) #rows = num_ss*num_plots*num_questions

n_after_exclusions <- 55;

d_long[, "sentiment_score"] <- sapply(d_long["word"], CalculateSentiment, model_type='vader')
e4_dat_final <- d_long

write.csv(d_long, "d_long_e4.csv", row.names = FALSE) #create word analysis csv for google colab code


# TODO: Need to fix this part
if (FALSE) {
    # Get main statistical effects
    main_effects <- GetMainEffects(e4_dat_final, d_long, data_plot_long, data_plot_long, n_plots, plot_names, my_embeddings)
    #See error: 486 not defined because of singularities; checked for perfect correlation but did not find any

    pdf(file = "linear_vs_quadratic_fit.pdf", width = 13, height = 6.5)
    main_effects
    dev.off()
}


#### (3.2) RUN DESCRIPTIVE ANALYSES

# Create a dataframe of features and subject scores
#d_long <- CreateDataFeaturesDF(d_long)

# Run regularized regression on all predictors
#ridge_regression_wt_predictors <- AnalyzeRidgeRegression(score_features_df, metric='enjoyment')

# Run mixed-effects regression on PCA-reduced features
#e4_data_wt_PCs <- MakePCAFunction(score_features_df)


##### (3.3) RUN PREDICTIVE ANALYSES

# Get performance of each predictor and PCA-reduced feature using cross-validation.
#cross_validation_analysis_wt_pcs <- CrossValidationAnalysisWtPCs(e4_data_wt_PCs, d_long, n_after_exclusions, n_plots)
#pdf(file = "predictions_wt_pcs_cv_plot.pdf", width = 17, height = 9)
#cross_validation_analysis_wt_pcs
#dev.off()
# errors pop up because I removed outliers

# TODO: Do cross validation using E4 Data, using the model trained on E1 Data

cross_validation_analysis_wt_predictors <- CrossValidationAnalysisWtPredictors(e4_dat_final, num_subjects_and_plots, n_plots)
pdf(file = "predictions_wt_predictors_cv_plot.pdf", width = 17, height = 9)
cross_validation_analysis_wt_predictors
dev.off()
# same note above

## =========================================== (4) Move Files ====================================================

plot_files <- list.files(pattern = c("(.pdf|.png)"))
file.move(plot_files, "./plots/analysis_plots", overwrite = TRUE)
analysis_files <- list.files(pattern = c("word_analysis_e4.csv|embeddings_e4.csv|correlations_e4.csv"))
file.move(analysis_files, "data", overwrite = TRUE)

##================================================================================================================
##END##
##================================================================================================================


