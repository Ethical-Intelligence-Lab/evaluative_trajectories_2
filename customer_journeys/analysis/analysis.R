## Analysis script for 'Evaluative Summaries'
## Study 1: Customer Journeys 

## Needed for semantic analysis: https://colab.research.google.com/drive/19cwz29yei-RwLnQ8HuEbjy74HRx94A6b?usp=sharing

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
               'broom', #install separately if does not work
               'vader' # Sentiment
)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #set working directory to current directory
source('../../tools/common_functions.R')

##===============================
## FUNCTIONS FOR PREPROCESSING ##
##===============================

PerformExclusions <- function(data) {
    "
    Excludes participants if they do not finish the survey, finished it too quickly (under 120 seconds), 
    gave duplicate answers, or failed important attention and comprehension checks.
    Input: data   #num_rows = num_ss
    Output: data after it has been 'cleaned'
    "

    # Exclude those who did not finish the survey
    data <- subset(data, (data$Finished == TRUE))
    n_before_exclusions <- dim(data)[1]; n_before_exclusions

    # Exclude those who finished it in less than 2 minutes
    data <- subset(data, (data$Duration..in.seconds. > 120))

    # Exclude those who gave the same answers to all satisfaction and personal desirability questions 
    satisfaction_cols <- data[, grep("satisfy", colnames(data), value = TRUE)]
    satisfaction_dups <- satisfaction_cols[apply(satisfaction_cols, 1,
                                                 function(x) length(unique(x[!is.na(x)])) == 1),]
    data <- anti_join(data, satisfaction_dups, by = grep("satisfy", colnames(data), value = TRUE))

    pd_cols <- data[, grep("preference", colnames(data), value = TRUE)]
    pd_dups <- pd_cols[apply(pd_cols, 1, function(x) length(unique(x[!is.na(x)])) == 1),]
    data <- anti_join(data, pd_dups, by = grep("preference", colnames(data), value = TRUE))

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
                table(data$attention_check)[2] / n_before_exclusions))

    # Perform comprehension checks
    data$attention_check2 <- ifelse((data$comp_check_1 == 80 &
        data$comp_check_2 == 0 &
        data$comp_check_3 == 'They were highly unstressed early in their customer experience, then highly stressed later in their customer experience'
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
        (data$comp_check_8 == 'Customer Touchpoint') &
        (data$comp_check_9 == 'Satisfaction')
        |
        ((data$comp_check_4 == 0) &
            (data$comp_check_5 == 80)
            &
            (data$comp_check_6 == 'They were highly stressed early in their customer experience, then highly unstressed later in their customer experience') &
            (data$comp_check_7 == 'Happiness') &
            (data$comp_check_8 == 'Customer Touchpoint') &
            (data$comp_check_9 == 'Satisfaction')
        )), 0, 1)

    print(paste('percentage excluded, comprehension checks: ',
                table(data$comp_check)[2] / n_before_exclusions))

    # Exclude those who failed either attention or comprehension checks
    data <- subset(data, (data$attention_check == 0) & (data$comp_check == 0))

    # Number of subjects after exclusions
    n_after_exclusions <- dim(data)[1]; n_after_exclusions #177
    print(paste('percentage excluded, duplicate answers: ',
                (dim(satisfaction_dups)[1] + dim(pd_dups)[1]) / n_after_exclusions))
    print(paste('total percentage excluded, comprehension checks: ',
                (n_before_exclusions - n_after_exclusions) / n_before_exclusions))

    data$n_after_exclusions <- n_after_exclusions

    print('mean age:')
    print(mean(as.numeric(data$age), trim = 0, na.rm = TRUE)) ## mean age 

    print('% female:')
    print(table(data$gender)[1] / sum(table(data$gender))) ## percentage of females

    return(data)
}


Preprocess <- function(data, n_plots, plot_names) {
    " 
    Since each plot is shown within-subjects, Qualtrics spits out data in wide format
    Let's convert it to long format, so that we have a row for every plot type
    Input: dataframe with number of rows = n_subjects
    Output: dataframe with number of rows = n_subjects*n_plot_types (=27)
    "

    # Define row and column names
    data_subset <- 35:142
    last_cols <- 143:145

    column_names <- c('plot_names', 'satisfaction', 'personal_desirability', 'word', 'willingness to pay', 'subject')

    df <- array(0, dim = c((nrow(data) * n_plots), length(column_names)))
    df <- as.data.frame(df, stringsAsFactors = FALSE)
    colnames(df) <- column_names

    # Turning wide format into long format, then inserting the answers into the 'df' dataframe
    final_data <- as.data.frame(t(data[data_subset])) #switch rows and columns in preparation for 'gather,' which collects info by columns
    long_data <- gather(final_data, key = "subject", value = "answers")["answers"] #gather the answers of the subjects into one long column 

    for (i in 1:dim(long_data)[2]) {
        df[1] <- plot_names
        df[2] <- long_data[seq(1, nrow(long_data), 4),]
        df[3] <- long_data[seq(2, nrow(long_data), 4),]
        df[4] <- long_data[seq(3, nrow(long_data), 4),]
        df[5] <- long_data[seq(4, nrow(long_data), 4),]
        df[6] <- rep(1:dim(data)[1], each = n_plots)
    }

    # Merge good data with first and last halves of the original data
    data <- cbind(data[rep(seq_len(nrow(data)), each = n_plots), 1:n_plots], df, data[rep(seq_len(nrow(data)), each = n_plots), last_cols])

    return(data)
}


ProcessForPlots <- function(data, n_plots, plot_names) {
    "
    Create a new data frame to store the satisfaction and PD scores by ascending satisfaction scores
    Input: data_long, n_plots, plot_names   #num_rows = num_ss*num_plots 
    Output: data_plot_long (in order of ascending satisfaction scores)   #num_rows = num_plots*num_questions
    "

    # Get mean scores for all questions, then reshape data from wide to long format
    stats <- Get_stats(data, n_plots)
    data_plot <- data.frame(plot_names = plot_names,
                            satisfaction_score_avg = unlist(stats)[c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE)],
                            satisfaction_score_sd = unlist(stats)[c(FALSE, TRUE, FALSE, FALSE, FALSE, FALSE)],
                            pd_score_avg = unlist(stats)[c(FALSE, FALSE, TRUE, FALSE, FALSE, FALSE)],
                            pd_score_sd = unlist(stats)[c(FALSE, FALSE, FALSE, TRUE, FALSE, FALSE)],
                            wtp_score_avg = unlist(stats)[c(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE)],
                            wtp_score_sd = unlist(stats)[c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE)])
    data_plot_sorted <- data_plot[order(data_plot$satisfaction_score_avg),] #order by satisfaction
    data_plot_long <- gather(data_plot_sorted, key = question_type, #create separate entries for each question type, i.e., num_plots*num_questions 
                             value = score, satisfaction_score_avg, pd_score_avg, wtp_score_avg)

    # Compile all standard deviation values
    stan_dev <- gather(data_plot_sorted, key = question_type,
                       value = sd, satisfaction_score_sd, pd_score_sd, wtp_score_sd)

    # Bind the SE column to the rest of the dataframe
    data_plot_long <- cbind(dplyr::select(data_plot_long, plot_names, question_type, score), sd = stan_dev$sd)
    data_plot_long$plot_names <- factor(data_plot_long$plot_names, levels = data_plot_long$plot_names[1:n_plots])

    return(data_plot_long)
}


TransformWTP <- function(data_long) {
    # Fix column name
    names(data_long)[names(data_long) == "willingness to pay"] <- "willingness_to_pay"

    # Turn willingness to pay to numeric  
    wtp <- as.numeric(unlist(data_long["willingness_to_pay"])) #turn to numeric 
    #hist(wtp) #see that amounts are right-skewed
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
    Find satisfaction and personal desirability means and standard deviations for every plot
    Every plot repeats every 27 times, since there are 27 plots title. 
    Hence the 'seq' indeces for each calculation
    Input: data_long, n_plots
    Output: equations (a list of means and standard deviations of satisfaction and pd scores for every plot)
    "

    # Transform all measures
    data <- data %>% replace(is.na(.), 0)

    satisfaction_score <- as.numeric(data$satisfaction)
    pd_score <- as.numeric(data$personal_desirability)
    wtp_score <- as.numeric(data$willingness_to_pay)

    # Get means and standard deviations 
    equations <- c()
    for (i in 1:n_plots) {
        equations[[i]] <- c(mean(satisfaction_score[seq(i, length(satisfaction_score), n_plots)]), sd(satisfaction_score[seq(i, length(satisfaction_score), n_plots)]),
                            mean(pd_score[seq(i, length(satisfaction_score), n_plots)]), sd(pd_score[seq(i, length(satisfaction_score), n_plots)]),
                            mean(wtp_score[seq(i, length(satisfaction_score), n_plots)]), sd(wtp_score[seq(i, length(satisfaction_score), n_plots)]))
    }

    return(equations)
}

##=====================================
## FUNCTIONS FOR PLOTTING BAR CHARTS ##
##=====================================

MakeGroupedBarPlot <- function(data_plot_long, wtp=FALSE) {
    "
    Plot the grouped bar graph in order of ascending satisfaction scores 
    Input: data_plot_long
    Output: grouped_bar_plot (the grouped bar graph)
    "

    if(wtp) {
        data_plot_long <- data_plot_long[data_plot_long$question_type == "wtp_score_avg",]
        grouped_bar_plot <- ggplot(data_plot_long, aes(x = plot_names, y = score, fill = question_type)) +
        geom_bar(position = "dodge", stat = "identity") +
        geom_errorbar(aes(ymin = score - sd, ymax = score + sd), width = .2,
                      position = position_dodge(.9)) +
        ggtitle("Summarizing the Satisfaction and Desirability of Different Customer Journeys") +
        xlab("Customer Journey Plots") +
        ylab("Scaled Rating") +
        theme(
            plot.title = element_blank(),
            legend.title = element_blank(),
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
            breaks = c("wtp_score_avg"),
            labels = c("Willingness to Pay"),
            values = c("#5660e9"),
            guide = guide_legend(title.position = "top")
        )

        return(grouped_bar_plot)
    }

    grouped_bar_plot <- ggplot(data_plot_long, aes(x = plot_names, y = score, fill = question_type)) +
        geom_bar(position = "dodge", stat = "identity") +
        geom_errorbar(aes(ymin = score - sd, ymax = score + sd), width = .2,
                      position = position_dodge(.9)) +
        ggtitle("Summarizing the Satisfaction and Desirability of Different Customer Journeys") +
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
            breaks = c("satisfaction_score_avg", "pd_score_avg", "wtp_score_avg"),
            labels = c("Satisfaction", "Personal Desirability", "WTP"),
            values = c("#56B4E9", "#009E73", "#22009e"),
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
    Create a new data frame to store the sentiment scores by ascending satisfaction scores
    Input: data_long, n_plots, plot_names
    Output: sentiment_df_sorted (the sentiment_df ordered by levels in the function factor())
    "

    # Get the order of satisfaction scores
    stats <- Get_stats(data, n_plots)
    data_plot <- data.frame(plot_names = plot_names,
                            satisfaction_score_avg = unlist(stats)[c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE)],
                            satisfaction_score_sd = unlist(stats)[c(FALSE, TRUE, FALSE, FALSE, FALSE, FALSE)],
                            pd_score_avg = unlist(stats)[c(FALSE, FALSE, TRUE, FALSE, FALSE, FALSE)],
                            pd_score_sd = unlist(stats)[c(FALSE, FALSE, FALSE, TRUE, FALSE, FALSE)],
                            wtp_score_avg = unlist(stats)[c(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE)],
                            wtp_score_sd = unlist(stats)[c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE)])

    # Create sentiment data frame ordered by ascending satisfaction scores
    sentiment_stats <- Get_sentiment_stats(data, n_plots)
    sentiment_df <- data.frame(plot_names = plot_names,
                               mean = unlist(sentiment_stats)[c(TRUE, FALSE)],
                               sd = unlist(sentiment_stats)[c(FALSE, TRUE)])
    sentiment_df_sorted <- sentiment_df[order(data_plot$satisfaction_score_avg),]
    sentiment_df_sorted$plot_names <- factor(sentiment_df_sorted$plot_names, levels = data_plot_long$plot_names[1:n_plots])

    return(sentiment_df_sorted)
}


MakeSentimentBarPlot <- function(data, n_plots, plot_names, title="Satisfaction") {
    "
    Plot the sentiment bar graph in order of ascending satisfaction scores.
    Input: data_long, n_plots, plot_names
    Output: the sentiment bar graph by ascending satisfaction scores
    "

    sentiment_df <- OrderSentimentDataframe(data, n_plots, plot_names)
    sentiment_bar_plot <- ggplot(sentiment_df, aes(x = plot_names, y = mean)) +
        geom_bar(position = "dodge", stat = "identity", fill = "darkorange") +
        geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = .2,
                      position = position_dodge(.9)) +
        ggtitle(paste("Mean Sentiment Scores by Ascending", title, "Scores")) +
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


##========================##
## FUNCTIONS FOR ANALYSIS ##
##========================##
GetMainEffects <- function(data, n_plots, plot_names, my_embeddings) {
    "
    This function gets various correlations and main effects of the participant data.
    Input: This function takes as input a dataframe with rows = num_ss*num_plots*num_questions.
          (dat_final, d_long, data_plot_long, data_plot_long, n_plots, plot_names, my_embeddings)
    Output: various correlation and linear regression results; also, linear and quadratic plots ordered by satisfaction scores
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

    print('Did satisfaction scores vary depending on plot type?')
    effect_mod <- lm(data = data[data['question_type'] == "satisfaction",], score ~ plot_type_n + (1 | subject_n))
    print(summary(effect_mod))
    print('-----------------------------------------------------')

    print('Did pd scores vary depending on plot type?')
    effect_mod <- lm(data = data[data['question_type'] == "personal_desirability",], score ~ plot_type_n + (1 | subject_n))
    print(summary(effect_mod))
    print('-----------------------------------------------------')

    print('Did all scores vary depending on plot type?')
    effect_mod <- lm(data = data, score ~ plot_type_n + (1 | subject_n))
    print(summary(effect_mod))
    print('-----------------------------------------------------')



    print('Do the sentiment scores correlate with satisfaction ratings?')
    satisfaction_corr <- cor.test(data$sentiment_score[data$question_type == "satisfaction"],
                                  data$score_n[data$question_type == "satisfaction"])
    print('sentiment vs. satisfaction:')
    print(satisfaction_corr)
    print('-----------------------------------------------------')

    print('Do the sentiment scores correlate with personal desirability ratings?')
    pd_corr <- cor.test(data$sentiment_score[data$question_type == "personal_desirability"],
                        data$score_n[data$question_type == "personal_desirability"])
    print('sentiment vs. personal desirability:')
    print(pd_corr)
    print('-----------------------------------------------------')

    # 3. Willingness to Pay

    print('Did willingness to pay vary depending on plot type?')
    wtp_mod <- lm(data = data, willingness_to_pay ~ plot_type_n + (1 | subject_n))
    print(summary(wtp_mod))
    print('-----------------------------------------------------')

    print('Does willingness to pay correlate with satisfaction ratings?')
    wtp_satisfaction_corr <- cor.test(data$willingness_to_pay[data$question_type == "satisfaction"],
                                      data$score_n[data$question_type == "satisfaction"])
    print('willingness to pay vs. satisfaction:')
    print(wtp_satisfaction_corr)
    print('-----------------------------------------------------')

    print('Does willingness to pay correlate with personal desirability ratings?')
    wtp_pd_corr <- cor.test(data$willingness_to_pay[data$question_type == "personal_desirability"],
                            data$score_n[data$question_type == "personal_desirability"])
    print('willingness to pay vs. personal desirability:')
    print(wtp_pd_corr)
    print('-----------------------------------------------------')

    # 4. Difference between linear and quadratic models for satisfaction and personal desirability

    # Get the order of average satisfaction scores
    stats <- Get_stats(d_long, n_plots)
    data_plot <- data.frame(plot_names = plot_names,
                            satisfaction_score_avg = unlist(stats)[c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE)],
                            satisfaction_score_sd = unlist(stats)[c(FALSE, TRUE, FALSE, FALSE, FALSE, FALSE)],
                            pd_score_avg = unlist(stats)[c(FALSE, FALSE, TRUE, FALSE, FALSE, FALSE)],
                            pd_score_sd = unlist(stats)[c(FALSE, FALSE, FALSE, TRUE, FALSE, FALSE)],
                            wtp_score_avg = unlist(stats)[c(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE)],
                            wtp_score_sd = unlist(stats)[c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE)])
    data_plot <- data_plot[order(data_plot$satisfaction_score_avg),]
    data_plot$order_num <- 1:n_plots

    # Add a column of the difference between the average ratings of satisfaction and personal desirability for each customer journey
    data_plot["satisfaction_pd_diff"] <- data_plot["satisfaction_score_avg"] - data_plot["pd_score_avg"]
    satisfaction_score_avg <- data_plot[, "satisfaction_score_avg"]
    pd_score_avg <- data_plot[, "pd_score_avg"]
    satisfaction_pd_diff <- data_plot[, "satisfaction_pd_diff"]

    print('Does a quadratic regression fit the shape of the difference between satisfaction and desirability ratings better than a linear one does?')
    print('First, the linear fit:')
    satisfaction_pd_diff_lin <- lm(satisfaction_pd_diff ~ data_plot$order_num)
    print(summary(satisfaction_pd_diff_lin))
    linear_plot <- ggplot(satisfaction_pd_diff_lin, aes(data_plot$order_num, satisfaction_pd_diff)) +
        theme_classic() +
        geom_point() +
        stat_smooth(method = lm, formula = y ~ x) +
        theme(axis.text = element_text(color = "black", size = 20),
              axis.title.y = element_blank(),
              axis.title.x = element_blank())
    print('-----------------------------------------------------')

    print('Second, the quadratic fit:')
    satisfaction_pd_diff_quadratic <- lm(satisfaction_pd_diff ~ data_plot$order_num + I(data_plot$order_num^2))
    print(summary(satisfaction_pd_diff_quadratic))
    quadratic_plot <- ggplot(satisfaction_pd_diff_quadratic, aes(data_plot$order_num, satisfaction_pd_diff)) +
        theme_classic() +
        geom_point() +
        stat_smooth(method = lm, formula = y ~ poly(x, 2, raw = TRUE)) +
        theme(axis.text = element_text(color = "black", size = 20),
              axis.title.y = element_blank(),
              axis.title.x = element_blank())
    print('-----------------------------------------------------')

    print('Difference between linear and quadratic models:')
    satisfaction_pd_diff_lrt <- lrtest(satisfaction_pd_diff_lin, satisfaction_pd_diff_quadratic)
    print(satisfaction_pd_diff_lrt)
    print('-----------------------------------------------------')

    linear_quad <- ggarrange(linear_plot, quadratic_plot, nrow = 1, ncol = 2)
    # theme(plot.margin = margin(0.1,0.1,-0.5,0.1, "cm"))
    linear_quad <- annotate_figure(linear_quad,
                                   left = text_grob("Difference Between Satisfaction and Desirability", color = "black", face = "bold", size = 20, rot = 90),
                                   bottom = text_grob("Plot Ordered by Satisfaction Scores", color = "black", face = "bold", size = 20, vjust = 0.4))
    print(linear_quad)

    print('Regress satisfaction on embeddings: ')
    satisfaction_embedding_df <- cbind(satisfaction = d_long$satisfaction, my_embeddings[2:513])
    satisfaction_embedding_lm <- lm(satisfaction ~ ., data = satisfaction_embedding_df)
    #print('satisfaction vs. embeddings:')
    #print(summary(satisfaction_embedding_lm))
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


CreateDataFeaturesDF <- function(data, features_df, n_after_exclusions) {
    "
    Bind the three dataframes: data, sentiment score, and standardize(features), i.e., the standardized plot features.
    Input: data_long, dat_final, features, n_after_exclusions, num_subjects_and_plots
    Output: score_features_df (which contains all of the predictors and participant scores)
    "

    score_features_df <- cbind(data, sentiment_score = data$sentiment_score,
                               as.data.frame(do.call("rbind", replicate(n_after_exclusions, standardize(features_df), simplify = FALSE))))
    score_features_df["satisfaction"] <- as.data.frame(standardize(apply(score_features_df["satisfaction"], 2, as.numeric)))
    score_features_df["personal_desirability"] <- as.data.frame(apply(score_features_df["personal_desirability"], 2, as.numeric))
    score_features_df["willingness_to_pay"] <- as.data.frame(apply(score_features_df["willingness_to_pay"], 2, as.numeric))
    score_features_df["subject"] <- as.data.frame(apply(score_features_df["subject"], 2, as.numeric))
    score_features_df["plot_names"] <- as.data.frame(as.numeric(factor(score_features_df$plot_names)))
    score_features_df["satisfaction"] <- standardize(score_features_df["satisfaction"])
    score_features_df["personal_desirability"] <- standardize(score_features_df["personal_desirability"])
    score_features_df["willingness_to_pay"] <- standardize(score_features_df["willingness_to_pay"])
    score_features_df["sentiment_score"] <- standardize(score_features_df["sentiment_score"])
    score_features_df["embeddings"] <- standardize(score_features_df["embeddings"])
    score_features_df["interestingness"] <- standardize(score_features_df["interestingness"])

    return(score_features_df)

}

Get_noise_ceiling <- function(dat_long, question_type, n_ss) {
    "
    Find correlation values between two randomly sample halves of the data,
    correct with the Spearman-Brown Prophecy formula (defined above), and put into a list.
    Input: data_long, question type ('satisfaction' or 'personal_desirability')
    Output: summary of the correlation results, to be used to plot noise ceiling (25th and 75th percentiles)
    "

    # Convert "satisfaction" and "personal_desirability" columns into numeric
    dat_long[, c("satisfaction", "personal_desirability")] <- sapply(dat_long[, c("satisfaction", "personal_desirability")], as.numeric)

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


CV_plotter <- function(results_df, x_order, results_order, ques_type, x_labels, sum_satisfaction, sum_pd) {
    "
    What this function does: creates a grouped box plot of the cross-validated prediction results
    Inputs: results_df, x_order, results_order, ques_type, x_labels, sum_satisfaction, sum_pd
    Output: a boxplot of participant rating predictions with either principal components or predictors
    "

    grouped_box_plot <- ggplot() +
        scale_x_discrete() +
        geom_rect(aes(xmin = 0.2, xmax = Inf, ymin = sum_satisfaction["1st Qu."], ymax = sum_satisfaction["3rd Qu."]),
                  alpha = 1, fill = "gray60") + #"dodgerblue3") + # #56B4E9
        geom_rect(aes(xmin = 0.2, xmax = Inf, ymin = sum_pd["1st Qu."], ymax = sum_pd["3rd Qu."]),
                  alpha = 1, fill = "gray60") + #"forestgreen") + # #009E73
        geom_hline(yintercept = 0, color = "gray60") +
        geom_boxplot(data = results_df, aes(x = x_order, y = results_order, fill = ques_type), outlier.shape = NA) +
        ggtitle(paste0("Satisfaction and Desirability Predictions with ", x_labels)) +
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
    Output: the structure of the PCA fit, the PCA correlation values for both satisfaction and pd
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
    pdf(file = "./plots/analysis_plots/features_corrplot.pdf")
    corrplot(my_PCA$Structure, method = "circle", mar = c(0, 0, 4, 0))
    mtext("Features Correlation Matrix \nover PCs", at = 1, line = 1, cex = 1.5)
    dev.off()

    print('principal components by features')
    print(my_PCA$Structure)

    # Bind the PC1 through PC5 scores to the score_features_df data frame
    score_features_df <- cbind(score_features_df, my_PCA$scores)

    # 1. Fit mixed effects regression predicting satisfaction
    satisfaction_features <- lmer(data = score_features_df,
                                  satisfaction ~ PC1 +
                                      PC2 +
                                      PC3 +
                                      PC4 +
                                      PC5 +
                                      (1 | subject) +
                                      (1 | plot_names))

    print('satisfaction vs. features:')
    print(summary(satisfaction_features, correlation = TRUE))


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

    #1. Satisfaction
    results_satisfaction <- data.frame(matrix(NA, nrow = length(pcs), ncol = n_folds))
    rownames(results_satisfaction) <- pcs

    for (i in 1:length(pcs)) {
        for (j in 1:n_folds) {
            ss_results <- c()
            truths <- c()

            for (k in 1:n_plots) {
                trainIndeces <- indeces[(folds == j) & (folds2 != k)]
                testIndeces <- indeces[(folds == j) & (folds2 == k)]
                fitpc <- lm(satisfaction ~ get(pcs[i]), data = dat, subset = trainIndeces) #fit model on subset of train data
                ss_results <- c(ss_results, predict(fitpc, dat)[testIndeces])
                truths <- c(truths, dat$satisfaction[testIndeces])
            }

            results_satisfaction[i, j] <- cor(truths, ss_results)
        }

        print(paste('satisfaction: mean PC result,', pcs[i], ': ', mean(as.numeric(results_satisfaction[i,]), na.rm = TRUE)))
        print(paste('satisfaction: median PC result,', pcs[i], ': ', median(as.numeric(results_satisfaction[i,]), na.rm = TRUE)))
    }

    # Reorder pcs according to their significance
    t_results_satisfaction <- as.data.frame(t(results_satisfaction))
    colnames(t_results_satisfaction) <- c("PC1\nFirst Derivative\nPredictors and End Value", "PC2\nSecond Derivative\nPredictors",
                                          "PC3\nFluctuations, Embeddings,\nand Interestingness", "PC4\nNumber of Peaks\nand Extrema",
                                          "PC5\nIntegral, Sentiment,\nMax, and Min")
    results_satisfaction_long <- gather(t_results_satisfaction, key = principal_components, value = pcs_results, colnames(t_results_satisfaction)) #length(pcs)*n_folds
    satisfaction_new_order <- with(results_satisfaction_long, reorder(principal_components, pcs_results, median, na.rm = TRUE))
    results_satisfaction_long["satisfaction_new_order"] <- satisfaction_new_order

    # Get_noise_ceiling function
    summary_satisfaction <- Get_noise_ceiling(dat_long, "satisfaction", n_ss)

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
        print(paste('personal desirability: median PC result,', pcs[i], ': ', median(as.numeric(results_pd[i,]), na.rm = TRUE)))
    }

    # Reorder pcs according to their significance
    t_results_pd <- as.data.frame(t(results_pd))
    colnames(t_results_pd) <- c("PC1\nFirst Derivative\nPredictors and End Value", "PC2\nSecond Derivative\nPredictors",
                                "PC3\nFluctuations, Embeddings,\nand Interestingness", "PC4\nNumber of Peaks\nand Extrema",
                                "PC5\nIntegral, Sentiment,\nMax, and Min")
    results_pd_long <- gather(t_results_pd, key = principal_components, value = pcs_results, colnames(t_results_pd)) #length(pcs)*n_folds
    pd_new_order <- with(results_pd_long, reorder(principal_components, pcs_results, median, na.rm = TRUE))
    results_pd_long["pd_new_order"] <- pd_new_order
    results_pd_long <- results_pd_long[order(match(results_pd_long[, "pd_new_order"], results_satisfaction_long[, "satisfaction_new_order"])),] #order by satisfaction scores

    # Get_noise_ceiling function
    summary_pd <- Get_noise_ceiling(dat_long, "personal_desirability", n_ss)

    #-------------------------------------------------------------------------------------------------------------------

    #3. Plotting
    pcs_results_ordered <- data.frame(pcs_order = results_satisfaction_long$satisfaction_new_order,
                                      satisfaction_results = results_satisfaction_long$pcs_results,
                                      pd_results = results_pd_long$pcs_results) #combine satisfaction and pd results
    pcs_results_long <- gather(pcs_results_ordered, key = question_type, value = results, satisfaction_results, pd_results)

    # Make boxplot from CV_plotter function
    pcs_plot <- CV_plotter(pcs_results_long, pcs_results_long$pcs_order, pcs_results_long$results, pcs_results_long$question_type, "Principal Components", summary_satisfaction, summary_pd)

    # Get the labels
    x_labs <- ggplot_build(pcs_plot)$layout$panel_params[[1]]$
        x$
        get_labels()

    # Perform Wilcoxon tests and get stars for significance
    # Define empty lists
    wilcox_test_1_wt_satisfaction <- c()
    wilcox_test_2_wt_satisfaction <- c()
    p_value_stars_1_satisfaction <- c()
    p_value_stars_2_satisfaction <- c()
    wilcox_test_1_wt_pd <- c()
    wilcox_test_2_wt_pd <- c()
    p_value_stars_1_pd <- c()
    p_value_stars_2_pd <- c()

    # Loop through the pcs, comparing each to null, then PC2 vs PC3, PC3 vs PC5, PC5 vs PC1, and PC1 vs PC4
    # Satisfaction: One-sided Wilcox test
    print("Satisfaction: --------------------------------------------------------------------------------------")
    for (i in 1:length(pcs)) {
        pcs_index <- x_labs[i]
        pcs_index_plus_one <- x_labs[i + 1]
        wilcox_test_1_wt_satisfaction[[i]] <- wilcox.test(t_results_satisfaction[, pcs_index], y = NULL, alternative = "greater",
                                                          conf.int = TRUE, data = t_results_satisfaction)
        p_value_stars_1_satisfaction[i] <- stars.pval(wilcox_test_1_wt_satisfaction[[i]]$"p.value") #get stars

        print(paste0(x_labs[i], " --------------------------------------------------------------------------------------"))
        print(wilcox_test_1_wt_satisfaction[[i]])
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

    # Satisfaction: Two-sided Wilcox test
    print("Satisfaction: --------------------------------------------------------------------------------------")
    for (i in 1:(length(pcs) - 1)) {
        pcs_index <- x_labs[i]
        pcs_index_plus_one <- x_labs[i + 1]
        wilcox_test_2_wt_satisfaction[[i]] <- wilcox.test(t_results_satisfaction[, pcs_index], y = t_results_satisfaction[, pcs_index_plus_one],
                                                          alternative = "two.sided", conf.int = TRUE, data = t_results_satisfaction)
        p_value_stars_2_satisfaction[i] <- stars.pval(wilcox_test_2_wt_satisfaction[[i]]$"p.value") #get stars
        if (p_value_stars_2_satisfaction[i] %in% c("", " ")) {
            p_value_stars_2_satisfaction[i] <- "ns"
        }

        print(paste0(sub("\\\n.*", "", pcs_index), " vs ", sub("\\\n.*", "", pcs_index_plus_one), #print PC comparisons vs one another
                     " --------------------------------------------------------------------------------------"))
        print(wilcox_test_2_wt_satisfaction[[i]])
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

    satisfaction_color <- "#56B4E9"
    satisfaction_bottom_x <- 1.19 #x value for bottom stars
    satisfaction_top_x <- satisfaction_bottom_x + 0.5 #x value for top stars
    satisfaction_top_y <- 1.17 #y value for top stars
    satisfaction_bracket_y <- 1.07 #y value for top bracket
    satisfaction_bracket_start <- 1.24 #x starting point for top bracket
    satisfaction_bracket_end <- 2.15 #x ending point for top bracket

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
        ggplot2::annotate("text", x = satisfaction_bottom_x, y = bottom_y, size = 8, label = p_value_stars_1_satisfaction[[1]]) +
        ggplot2::annotate("text", x = satisfaction_bottom_x + 1, y = bottom_y, size = 8, label = p_value_stars_1_satisfaction[[2]]) +
        ggplot2::annotate("text", x = satisfaction_bottom_x + 2, y = bottom_y, size = 8, label = p_value_stars_1_satisfaction[[3]]) +
        ggplot2::annotate("text", x = satisfaction_bottom_x + 3, y = bottom_y, size = 8, label = p_value_stars_1_satisfaction[[4]]) +
        ggplot2::annotate("text", x = satisfaction_bottom_x + 4, y = bottom_y, size = 8, label = p_value_stars_1_satisfaction[[5]]) +
        ggplot2::annotate("text", x = pd_bottom_x, y = bottom_y, size = 8, label = p_value_stars_1_pd[[1]]) +
        ggplot2::annotate("text", x = pd_bottom_x + 1, y = bottom_y, size = 8, label = p_value_stars_1_pd[[2]]) +
        ggplot2::annotate("text", x = pd_bottom_x + 2, y = bottom_y, size = 8, label = p_value_stars_1_pd[[3]]) +
        ggplot2::annotate("text", x = pd_bottom_x + 3, y = bottom_y, size = 8, label = p_value_stars_1_pd[[4]]) +
        ggplot2::annotate("text", x = pd_bottom_x + 4, y = bottom_y, size = 8, label = p_value_stars_1_pd[[5]]) +

        # Two-sided Wilcox test (with brackets)
        geom_segment(aes(x = satisfaction_bracket_start, xend = satisfaction_bracket_end, y = satisfaction_bracket_y, yend = satisfaction_bracket_y, colour = satisfaction_color)) +
        ggplot2::annotate("text", x = satisfaction_top_x, y = satisfaction_top_y, size = 8, label = p_value_stars_2_satisfaction[[1]]) +
        geom_segment(aes(x = satisfaction_bracket_start + 1, xend = satisfaction_bracket_end + 1, y = satisfaction_bracket_y, yend = satisfaction_bracket_y, color = satisfaction_color)) +
        ggplot2::annotate("text", x = satisfaction_top_x + 1, y = satisfaction_top_y, size = 8, label = p_value_stars_2_satisfaction[[2]]) +
        geom_segment(aes(x = satisfaction_bracket_start + 2, xend = satisfaction_bracket_end + 2, y = satisfaction_bracket_y, yend = satisfaction_bracket_y, color = satisfaction_color)) +
        ggplot2::annotate("text", x = satisfaction_top_x + 2, y = satisfaction_top_y, size = 8, label = p_value_stars_2_satisfaction[[3]]) +
        geom_segment(aes(x = satisfaction_bracket_start + 3, xend = satisfaction_bracket_end + 3, y = satisfaction_bracket_y, yend = satisfaction_bracket_y, color = satisfaction_color)) +
        ggplot2::annotate("text", x = satisfaction_top_x + 3, y = satisfaction_top_y, size = 8, label = p_value_stars_2_satisfaction[[4]]) +
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


CrossValidationAnalysisWtPredictors <- function(dat, dat_long, n_ss, n_plots) {
    "
    Measure the performance of each of our predictors by doing cross-validated regressions, holding out
    one participant for each cross-validation step.
    Input: data_wt_PCs, data_long, n_after_exclusions, n_plots
    Output: relative importance of individual predictors and its graph
    "

    is_word <- subset(dat, dat['is_word'] == TRUE)['is_word']

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

    #1. Satisfaction
    results_satisfaction <- data.frame(matrix(NA, nrow = length(predictors), ncol = n_folds))
    rownames(results_satisfaction) <- predictors

    for (i in 1:length(predictors)) {
        for (j in 1:n_folds) {
            ss_results <- c()
            truths <- c()

            for (k in 1:n_plots) {
                trainIndeces <- indeces[(folds == j) & (folds2 != k)]
                testIndeces <- indeces[(folds == j) & (folds2 == k)]

                if( predictors[i] == "Sentiment Score" ) { # Exclude train indexes that is not a word
                    trainIndeces <- subset(trainIndeces, dat$is_word[trainIndeces])
                }

                if( predictors[i] == "Sentiment Score" && !dat$is_word[testIndeces] ) { # Do not fit if not a word
                    next
                } else {
                    fitpc <- lm(satisfaction ~ get(predictors[i]), data = dat, subset = trainIndeces) #fit model on subset of train data
                    ss_results <- c(ss_results, predict(fitpc, dat)[testIndeces])
                    truths <- c(truths, dat$satisfaction[testIndeces])
                }
            }
            results_satisfaction[i, j] <- cor(truths, ss_results)
        }

        print(paste('satisfaction: mean predictor result,', predictors[i], ': ', mean(as.numeric(results_satisfaction[i,]), na.rm = TRUE)))
        print(paste('satisfaction: median predictor result,', predictors[i], ': ', median(as.numeric(results_satisfaction[i,]), na.rm = TRUE)))
    }

    # Reorder predictors according to their significance
    t_results_satisfaction <- as.data.frame(t(results_satisfaction))
    colnames(t_results_satisfaction) <- predictors
    results_satisfaction_long <- gather(t_results_satisfaction, key = predictors, value = predictors_results, colnames(t_results_satisfaction)) #length(predictors)*n_folds
    satisfaction_new_order <- with(results_satisfaction_long, reorder(predictors, predictors_results, median, na.rm = TRUE))
    results_satisfaction_long["satisfaction_new_order"] <- satisfaction_new_order

    # Get_noise_ceiling function
    summary_satisfaction <- Get_noise_ceiling(dat_long, "satisfaction", n_ss)

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

                if( predictors[i] == "Sentiment Score" ) { # Exclude train indexes that is not a word
                    trainIndeces <- subset(trainIndeces, dat$is_word[trainIndeces])
                }

                if( predictors[i] == "Sentiment Score" && !dat$is_word[testIndeces] ) { # Do not fit if not a word
                    next
                } else {
                    fitpc <- lm(personal_desirability ~ get(predictors[i]), data = dat, subset = trainIndeces) #fit model on subset of train data
                    ss_results <- c(ss_results, predict(fitpc, dat)[testIndeces])
                    truths <- c(truths, dat$personal_desirability[testIndeces])
                }
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
    pd_new_order <- with(results_pd_long, reorder(predictors, predictors_results, median, na.rm = TRUE))
    results_pd_long["pd_new_order"] <- pd_new_order
    results_pd_long <- results_pd_long[order(match(results_pd_long[, "pd_new_order"], results_satisfaction_long[, "satisfaction_new_order"])),] #order by satisfaction scores

    # Get_noise_ceiling function
    summary_pd <- Get_noise_ceiling(dat_long, "personal_desirability", n_ss)

    #-------------------------------------------------------------------------------------------------------------------

    #3. Plotting
    predictors_results_ordered <- data.frame(predictors_order = results_satisfaction_long$satisfaction_new_order,
                                             satisfaction_results = results_satisfaction_long$predictors_results,
                                             pd_results = results_pd_long$predictors_results) #combine satisfaction and pd results
    predictors_results_long <- gather(predictors_results_ordered, key = question_type, value = results, satisfaction_results, pd_results)

    # Make boxplot from CV_plotter function
    predictors_plot <- CV_plotter(predictors_results_long, predictors_results_long$predictors_order, predictors_results_long$results, predictors_results_long$question_type, "Predictors", summary_satisfaction, summary_pd)

    # Get the labels
    x_labs <- ggplot_build(predictors_plot)$layout$panel_params[[1]]$
        x$
        get_labels()

    # Perform Wilcoxon tests and get stars for significance
    # Define empty lists
    wilcox_test_wt_satisfaction <- c()
    p_value_stars_satisfaction <- c()
    wilcox_test_wt_pd <- c()
    p_value_stars_pd <- c()

    # Loop through the predictors, comparing each to a null distribution
    # Satisfaction: One-sided Wilcox test
    print("Satisfaction: --------------------------------------------------------------------------------------")
    for (i in x_labs) {
        print(paste0(i, " --------------------------------------------------------------------------------------"))
        wilcox_test_wt_satisfaction[[i]] <- wilcox.test(t_results_satisfaction[, i], y = NULL, alternative = "greater",
                                                        conf.int = TRUE, data = t_results_satisfaction)
        p_value_stars_satisfaction[i] <- stars.pval(wilcox_test_wt_satisfaction[[i]]$"p.value") #get stars

        print(wilcox_test_wt_satisfaction[[i]])
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
    pd_bottom_x <- 0.8 #x value for bottom stars
    pd_bottom_y <- -1.0 #y value for bottom stars
    satisfaction_bottom_x <- 1.2 #x value for bottom stars
    satisfaction_bottom_y <- pd_bottom_y - 0.15 #y value for bottom stars

    # Add to the plot: stars indicating significance
    for (i in 1:20) {
        predictors_plot <- predictors_plot + ggplot2::annotate("text", x = satisfaction_bottom_x + i - 1, y = satisfaction_bottom_y, size = 8, label = p_value_stars_satisfaction[[i]])
        predictors_plot <- predictors_plot + ggplot2::annotate("text", x = pd_bottom_x + i - 1, y = pd_bottom_y, size = 8, label = p_value_stars_pd[[i]])
    }

    return(predictors_plot)
}

##======##
## MAIN ##
##======##

# Define global variables
n_plots <- 27
satisfaction_scores <- 1:n_plots
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

d <- PerformExclusions(d_raw) #num_rows = num_ss
n_after_exclusions <- d$n_after_exclusions[1]
num_subjects_and_plots <- n_after_exclusions * n_plots

d_long <- Preprocess(d, n_plots, plot_names) #num_rows = num_ss*num_plots [to see data without exclusions, replace data_clean with data]
d_long[, c("satisfaction", "personal_desirability")] <- sapply(d_long[, c("satisfaction", "personal_desirability")], as.numeric) #turn ratings to numeric

d_long$wtp_original <- as.numeric(d_long$`willingness to pay`)
d_long <- TransformWTP(d_long)
# Warning message: In TransformWTP(d_long) : NAs introduced by coercion
# (Some people gave non-numerical answers, hence the warning message)

### (i) CREATE CSV FOR SEMANTIC ANALYSIS
analyze_words <- GetWordAnalysis(d_long, n_plots)
words_df <- as.data.frame(matrix(unlist(analyze_words), ncol = length(unlist(analyze_words[1]))))
analyze_words_df <- cbind(plot_names = plot_names, words = words_df$V1)
write.csv(analyze_words_df, "./data/word_analysis.csv", row.names = FALSE) #create word analysis csv for google colab code

### (ii) CREATE SEMANTIC EMBEDDINGS DATAFRAME [**NB: YOU NEED TO HAVE ALREADY EXTRACTED EMBEDDINGS FOR word_analysis.csv]
my_embeddings <- read.csv("data/embeddings_long.csv", header = TRUE)
my_embeddings$X = NULL
embeddings <- data.frame(embeddings = rowMeans(my_embeddings)) #create a dataframe

### (iii) CREATE INTERESTINGNESS DATAFRAME
interestingness <- GetInterestingness(d_long, n_plots)

### (iv) PROCESS FOR PLOTS
d_long <- cbind(d_long, embeddings)
d_long <- cbind(d_long, interestingness)
data_plot_long = NULL
data_plot_long <- ProcessForPlots(d_long, n_plots, plot_names) #num_rows = num_plots*num_questions

#### (2.1) MAKE BAR PLOT OF SATISFACTION SCORES


grouped_bar_plot_wtp <- MakeGroupedBarPlot(data_plot_long, wtp=TRUE)
plot_images <- MakeGroupedBarPlotImages(grouped_bar_plot_wtp, plot_names) #the little customer journey icons

pdf(file = "customer_journeys_bar_plot_wtp.pdf", width = 17, height = 8)
ggdraw(insert_xaxis_grob(grouped_bar_plot_wtp, plot_images, position = "bottom"))
dev.off()


grouped_bar_plot <- MakeGroupedBarPlot(data_plot_long)
plot_images <- MakeGroupedBarPlotImages(grouped_bar_plot, plot_names) #the little customer journey icons

pdf(file = "customer_journeys_bar_plot.pdf", width = 17, height = 8)
ggdraw(insert_xaxis_grob(grouped_bar_plot, plot_images, position = "bottom"))
dev.off()






## ========================================== (2) Plot Data and Save ==================================================
if (FALSE) {
    "
    Create bar plot, word clouds, and sentiment plot
    "


    #### (2.2) MAKE WORD CLOUDS (WARNING: takes ~5 minutes; feel free to skip)
    MakeWordClouds(d_long, n_plots, plot_names) #make word cloud images
    arranged_word_clouds <- ArrangeWordClouds() #arrange word clouds into a grid

    pdf(file = "customer_journeys_word_clouds.pdf", width = 18, height = 8)
    arranged_word_clouds
    dev.off()


    #### (2.3) MAKE PLOT OF SENTIMENT SCORES, ORDERED BY SATISFACTION SCORES
    sentiment_bar_plot <- MakeSentimentBarPlot(d_long, n_plots, plot_names)
    sentiment_plot_images <- MakeGroupedBarPlotImages(sentiment_bar_plot, plot_names) #the little customer journey icons

    pdf(file = "customer_journeys_sentiment_plot.pdf", width = 17, height = 8)
    ggdraw(insert_xaxis_grob(sentiment_bar_plot, sentiment_plot_images, position = "bottom"))
    dev.off()

    #### (2.4) MAKE FREQUENCY PLOTS FOR TOPIC MODELING
    #topic_modeling <- TopicModeling(d_long, n_plots, plot_names)

    plot_files <- list.files(pattern = c("(.pdf|.png)"))
    file.move(plot_files, "./plots/analysis_plots", overwrite = TRUE)
    analysis_files <- list.files(pattern = c("word_analysis.csv|embeddings.csv|correlations.csv"))
    file.move(analysis_files, "data", overwrite = TRUE)

}

## ============================================== (3) Analysis =====================================================
"
Get main statistical effects, and run descriptive and predictive analyses
"

#### (3.1) GET MAIN EFFECTS
d_long[, "sentiment_score"] <- sapply(d_long["word"], CalculateSentiment, model_type='ai')
d_long$sentiment_score[is.na(d_long$sentiment_score)] <- 0

d_long[, "is_word"] <- lapply(d_long["word"], is.word)


# Get dataframe for analysis (dat_final), with nrows = num_ss*num_plots*num_questions
dat <- gather(d_long, key = question_type, value = score, satisfaction, personal_desirability)
dat <- dplyr::select(dat, subject, plot_names, question_type, score, willingness_to_pay, sentiment_score, wtp_original) #rows = num_ss*num_plots*num_questions

main_effects <- GetMainEffects(dat, n_plots, plot_names, my_embeddings)


write.csv(data.frame(word = d_long), "./data/d_long.csv", row.names = FALSE) #create word analysis csv for google colab code
write.csv(data.frame(word = dat), "./data/dat.csv", row.names = FALSE) #create word analysis csv for google colab code


# TODO: Need to fix this part
if (FALSE) {
    # Get main statistical effects
    main_effects <- GetMainEffects(dat, n_plots, plot_names, my_embeddings)
    #See error: 486 not defined because of singularities; checked for perfect correlation but did not find any

    pdf(file = "linear_vs_quadratic_fit.pdf", width = 13, height = 6.5)
    main_effects
    dev.off()
}


#### (3.2) RUN DESCRIPTIVE ANALYSES

# Create a dataframe of features and subject scores
score_features_df <- CreateDataFeaturesDF(d_long, features, n_after_exclusions)

# Run regularized regression on all predictors
#ridge_regression_wt_predictors <- AnalyzeRidgeRegression(score_features_df)

# Run mixed-effects regression on PCA-reduced features
#data_wt_PCs <- MakePCAFunction(score_features_df)


##### (3.3) RUN PREDICTIVE ANALYSES

# Get performance of each predictor and PCA-reduced feature using cross-validation.
#cross_validation_analysis_wt_pcs <- CrossValidationAnalysisWtPCs(data_wt_PCs, d_long, n_after_exclusions, n_plots)
#pdf(file = "predictions_wt_pcs_cv_plot.pdf", width = 17, height = 9)
#cross_validation_analysis_wt_pcs
#dev.off()
# errors pop up because I removed outliers

cross_validation_analysis_wt_predictors <- CrossValidationAnalysisWtPredictors(score_features_df, d_long, n_after_exclusions, n_plots)
pdf(file = "predictions_wt_predictors_cv_plot.pdf", width = 17, height = 9)
cross_validation_analysis_wt_predictors
dev.off()
# same note above

## =========================================== (4) Move Files ====================================================

plot_files <- list.files(pattern = c("(.pdf|.png)"))
file.move(plot_files, "./plots/analysis_plots", overwrite = TRUE)
analysis_files <- list.files(pattern = c("word_analysis.csv|embeddings.csv|correlations.csv"))
file.move(analysis_files, "data", overwrite = TRUE)

##=====##
## END ##
##=====##


