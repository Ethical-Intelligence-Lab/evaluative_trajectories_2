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

# Call in the Lifelines_Customer_Journeys.R script from the Lifelines folder for plot images
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #set working directory to current directory
#setwd("../e3_customer_journeys") #go to e3_customer_journeys directory if needed
#source("Lifelines_Customer_Journeys.R") #import plot-generating script
#setwd("../e3_customer_journeys") #go back down to e3_customer_journeys directory

# Read in Lifelines_analysis_e1b.R
#setwd("../e1b_basic_effect") #go one directory up
#source("Lifelines_analysis_e1b.R")
#setwd("../e3_customer_journeys") #go back down to e3_customer_journeys directory


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
    n_before_exclusions <- dim(data)[1]; n_before_exclusions

    # Exclude those who finished it in less than 2 minutes
    data <- subset(data, (data$Duration..in.seconds. > 120))

    # Exclude those who gave the same answers to all enjoyment and personal desirability questions
    enjoyment_cols <- data[, grep("satisfy", colnames(data), value = TRUE)]
    enjoyment_dups <- enjoyment_cols[apply(enjoyment_cols, 1, function(x) length(unique(x[!is.na(x)])) == 1),]
    data <- anti_join(data, enjoyment_dups, by = grep("satisfy", colnames(data), value = TRUE))

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
        (data$comp_check_9 == 'enjoyment')
        |
        ((data$comp_check_4 == 0) &
            (data$comp_check_5 == 80)
            &
            (data$comp_check_6 == 'They were highly stressed early in their customer experience, then highly unstressed later in their customer experience') &
            (data$comp_check_7 == 'Happiness') &
            (data$comp_check_8 == 'Customer Touchpoint') &
            (data$comp_check_9 == 'enjoyment')
        )), 0, 1)

    print(paste('percentage excluded, comprehension checks: ',
                table(data$comp_check)[2] / n_before_exclusions))

    # Exclude those who failed either attention or comprehension checks
    data <- subset(data, (data$attention_check == 0) & (data$comp_check == 0))

    # Number of subjects after exclusions
    n_after_exclusions <- dim(data)[1]; #177
    print(paste('percentage excluded, duplicate answers: ',
                (dim(enjoyment_dups)[1] + dim(pd_dups)[1]) / n_after_exclusions))
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

    # Define new data frame that we'll extract preprocessed data into

    # Define row and column names
    data_subset <- 35:142 # TODO
    last_cols <- 143:145 # TODO

    column_names <- c('plot_names', 'willingness to pay', 'word', 'subject')

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
        df[4] <- rep(1:dim(data)[1], each = n_plots)
    }

    # Merge good data with first and last halves of the original data
    data <- cbind(data[rep(seq_len(nrow(data)), each = n_plots), 1:n_plots], df, data[rep(seq_len(nrow(data)), each = n_plots), last_cols])

    return(data)
}


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
    for the x-axis. Then, plot the images in order of ascending enjoyment scores,
    which can be determined by the order in data_plot_long$plot_names[1:27].
    Input: grouped_bar_plot, plot_names
    Output: the plot labels for the grouped bar graph and the sentiment bar graph
    "

    # Make "clean" (no labels) version of individual images for x-axis
    Plotter_2 <- function(equation, x_range, y_range) {
        start_age <- 0
        end_age <- 80
        end_y_axis <- 100
        plot(equation, lwd = 30, xlim = c(start_age, end_age), ylim = c(0, end_y_axis), main = "",
             xlab = "", ylab = "", axes = FALSE, col = "firebrick3")

        return(Plotter_2)
    }

    # Print the images that will comprise the x-axis
    for (i in 1:length(plot_names)) { #print individual plots
        png(file = paste0(plot_names[i], "_plot.png", ""))
        sapply(plot_names[i], Plotter_2)
        dev.off()
    }

    # Assemble images in the order of data_plot_long$plot_names[1:27]
    plot_images <- axis_canvas(LifelinesPlot, axis = 'x') +
        # for(i in 1:length(data_plot_long$plot_names[1:n_plots])) {
        #   placement = (i - 0.5)
        #   plot_images <- axis_canvas(plot_images, axis = 'x') +
        #     draw_image(paste0(data_plot_long$plot_names[i], "_plot.png"), x = placement)} +

        draw_image(paste0(data_plot_long$plot_names[1], "_plot.png"), x = 0.5) +
        draw_image(paste0(data_plot_long$plot_names[2], "_plot.png"), x = 1.5) +
        draw_image(paste0(data_plot_long$plot_names[3], "_plot.png"), x = 2.5) +
        draw_image(paste0(data_plot_long$plot_names[4], "_plot.png"), x = 3.5) +
        draw_image(paste0(data_plot_long$plot_names[5], "_plot.png"), x = 4.5) +
        draw_image(paste0(data_plot_long$plot_names[6], "_plot.png"), x = 5.5) +
        draw_image(paste0(data_plot_long$plot_names[7], "_plot.png"), x = 6.5) +
        draw_image(paste0(data_plot_long$plot_names[8], "_plot.png"), x = 7.5) +
        draw_image(paste0(data_plot_long$plot_names[9], "_plot.png"), x = 8.5) +

        draw_image(paste0(data_plot_long$plot_names[10], "_plot.png"), x = 9.5) +
        draw_image(paste0(data_plot_long$plot_names[11], "_plot.png"), x = 10.5) +
        draw_image(paste0(data_plot_long$plot_names[12], "_plot.png"), x = 11.5) +
        draw_image(paste0(data_plot_long$plot_names[13], "_plot.png"), x = 12.5) +
        draw_image(paste0(data_plot_long$plot_names[14], "_plot.png"), x = 13.5) +
        draw_image(paste0(data_plot_long$plot_names[15], "_plot.png"), x = 14.5) +
        draw_image(paste0(data_plot_long$plot_names[16], "_plot.png"), x = 15.5) +
        draw_image(paste0(data_plot_long$plot_names[17], "_plot.png"), x = 16.5) +
        draw_image(paste0(data_plot_long$plot_names[18], "_plot.png"), x = 17.5) +

        draw_image(paste0(data_plot_long$plot_names[19], "_plot.png"), x = 18.5) +
        draw_image(paste0(data_plot_long$plot_names[20], "_plot.png"), x = 19.5) +
        draw_image(paste0(data_plot_long$plot_names[21], "_plot.png"), x = 20.5) +
        draw_image(paste0(data_plot_long$plot_names[22], "_plot.png"), x = 21.5) +
        draw_image(paste0(data_plot_long$plot_names[23], "_plot.png"), x = 22.5) +
        draw_image(paste0(data_plot_long$plot_names[24], "_plot.png"), x = 23.5) +
        draw_image(paste0(data_plot_long$plot_names[25], "_plot.png"), x = 24.5) +
        draw_image(paste0(data_plot_long$plot_names[26], "_plot.png"), x = 25.5) +
        draw_image(paste0(data_plot_long$plot_names[27], "_plot.png"), x = 26.5)


    return(plot_images)
}

##================================================================================================================
##FUNCTIONS FOR PLOTTING WORD CLOUDS##
##================================================================================================================

Get_word_stats <- function(data, n_plots) {
    "
    Group 'clean' words (only lowercase letters a-z) together into individual dataframes by plot type
    Input: data_long, n_plots
    Output: equations (individual words from each participant for each plot)
    "

    word_clean <- word(tolower(data$word), 1) #make all words lowercase, and collect only the first word of a given sentence
    word_gen <- gsub("[^a-z]", "", word_clean) #get rid of numbers and special characters, leaving only letters a-z

    equations <- c()
    for (i in 1:n_plots) {
        equations[[i]] <- as.data.frame(table(word_gen[seq(i, length(word_gen), n_plots)]))
    }

    equations$word_gen <- word_gen

    return(equations)
}


MakeWordClouds <- function(data, n_plots, plot_names) {
    "
    Make word clouds and save them as individual images to be read in as files later.
    Input: data_long, n_plots, plot_names
    Output: individual word clouds; takes a long time because we are using ggsave, which saves
    super high-quality images necessary for producing small word clouds
    "

    plot_word_clouds <- function(data) {
        # Define word clouds
        LR <- ggplot(arrange(as.data.frame(Get_word_stats(data, n_plots)[[1]]), -Freq), aes(label = Var1, size = Freq, color = Freq)) +
            geom_text_wordcloud_area(
                #mask = png::readPNG("ggwordcloud_mask.png"), #mask does not work
                shape = "square") +
            scale_size_area(max_size = 30) +
            theme_minimal() +
            scale_color_gradient(low = "mistyrose1", high = "firebrick3")
        LF <- ggplot(arrange(as.data.frame(Get_word_stats(data, n_plots)[[2]]), -Freq), aes(label = Var1, size = Freq, color = Freq)) +
            geom_text_wordcloud_area(shape = "square") +
            scale_size_area(max_size = 30) +
            theme_minimal() +
            scale_color_gradient(low = "mistyrose1", high = "firebrick3")
        LL <- ggplot(arrange(as.data.frame(Get_word_stats(data, n_plots)[[3]]), -Freq), aes(label = Var1, size = Freq, color = Freq)) +
            geom_text_wordcloud_area(shape = "square") +
            scale_size_area(max_size = 30) +
            theme_minimal() +
            scale_color_gradient(low = "mistyrose1", high = "firebrick3")
        LM <- ggplot(arrange(as.data.frame(Get_word_stats(data, n_plots)[[4]]), -Freq), aes(label = Var1, size = Freq, color = Freq)) +
            geom_text_wordcloud_area(shape = "square") +
            scale_size_area(max_size = 30) +
            theme_minimal() +
            scale_color_gradient(low = "mistyrose1", high = "firebrick3")
        LH <- ggplot(arrange(as.data.frame(Get_word_stats(data, n_plots)[[5]]), -Freq), aes(label = Var1, size = Freq, color = Freq)) +
            geom_text_wordcloud_area(shape = "square") +
            scale_size_area(max_size = 30) +
            theme_minimal() +
            scale_color_gradient(low = "mistyrose1", high = "firebrick3")
        ERCV <- ggplot(arrange(as.data.frame(Get_word_stats(data, n_plots)[[6]]), -Freq), aes(label = Var1, size = Freq, color = Freq)) +
            geom_text_wordcloud_area(shape = "square") +
            scale_size_area(max_size = 30) +
            theme_minimal() +
            scale_color_gradient(low = "mistyrose1", high = "firebrick3")
        EFCV <- ggplot(arrange(as.data.frame(Get_word_stats(data, n_plots)[[7]]), -Freq), aes(label = Var1, size = Freq, color = Freq)) +
            geom_text_wordcloud_area(shape = "square") +
            scale_size_area(max_size = 30) +
            theme_minimal() +
            scale_color_gradient(low = "mistyrose1", high = "firebrick3")
        ERCC <- ggplot(arrange(as.data.frame(Get_word_stats(data, n_plots)[[8]]), -Freq), aes(label = Var1, size = Freq, color = Freq)) +
            geom_text_wordcloud_area(shape = "square") +
            scale_size_area(max_size = 30) +
            theme_minimal() +
            scale_color_gradient(low = "mistyrose1", high = "firebrick3")
        EFCC <- ggplot(arrange(as.data.frame(Get_word_stats(data, n_plots)[[9]]), -Freq), aes(label = Var1, size = Freq, color = Freq)) +
            geom_text_wordcloud_area(shape = "square") +
            scale_size_area(max_size = 30) +
            theme_minimal() +
            scale_color_gradient(low = "mistyrose1", high = "firebrick3")
        SFR_FULL <- ggplot(arrange(as.data.frame(Get_word_stats(data, n_plots)[[10]]), -Freq), aes(label = Var1, size = Freq, color = Freq)) +
            geom_text_wordcloud_area(shape = "square") +
            scale_size_area(max_size = 30) +
            theme_minimal() +
            scale_color_gradient(low = "mistyrose1", high = "firebrick3")
        SFR_PAR <- ggplot(arrange(as.data.frame(Get_word_stats(data, n_plots)[[11]]), -Freq), aes(label = Var1, size = Freq, color = Freq)) +
            geom_text_wordcloud_area(shape = "square") +
            scale_size_area(max_size = 30) +
            theme_minimal() +
            scale_color_gradient(low = "mistyrose1", high = "firebrick3")
        SRF_FULL <- ggplot(arrange(as.data.frame(Get_word_stats(data, n_plots)[[12]]), -Freq), aes(label = Var1, size = Freq, color = Freq)) +
            geom_text_wordcloud_area(shape = "square") +
            scale_size_area(max_size = 30) +
            theme_minimal() +
            scale_color_gradient(low = "mistyrose1", high = "firebrick3")
        SRF_PAR <- ggplot(arrange(as.data.frame(Get_word_stats(data, n_plots)[[13]]), -Freq), aes(label = Var1, size = Freq, color = Freq)) +
            geom_text_wordcloud_area(shape = "square") +
            scale_size_area(max_size = 30) +
            theme_minimal() +
            scale_color_gradient(low = "mistyrose1", high = "firebrick3")
        SRFR_FULL <- ggplot(arrange(as.data.frame(Get_word_stats(data, n_plots)[[14]]), -Freq), aes(label = Var1, size = Freq, color = Freq)) +
            geom_text_wordcloud_area(shape = "square") +
            scale_size_area(max_size = 30) +
            theme_minimal() +
            scale_color_gradient(low = "mistyrose1", high = "firebrick3")
        SRFR_PAR <- ggplot(arrange(as.data.frame(Get_word_stats(data, n_plots)[[15]]), -Freq), aes(label = Var1, size = Freq, color = Freq)) +
            geom_text_wordcloud_area(shape = "square") +
            scale_size_area(max_size = 30) +
            theme_minimal() +
            scale_color_gradient(low = "mistyrose1", high = "firebrick3")
        SFRF_FULL <- ggplot(arrange(as.data.frame(Get_word_stats(data, n_plots)[[16]]), -Freq), aes(label = Var1, size = Freq, color = Freq)) +
            geom_text_wordcloud_area(shape = "square") +
            scale_size_area(max_size = 30) +
            theme_minimal() +
            scale_color_gradient(low = "mistyrose1", high = "firebrick3")
        SFRF_PAR <- ggplot(arrange(as.data.frame(Get_word_stats(data, n_plots)[[17]]), -Freq), aes(label = Var1, size = Freq, color = Freq)) +
            geom_text_wordcloud_area(shape = "square") +
            scale_size_area(max_size = 30) +
            theme_minimal() +
            scale_color_gradient(low = "mistyrose1", high = "firebrick3")
        SFRFR <- ggplot(arrange(as.data.frame(Get_word_stats(data, n_plots)[[18]]), -Freq), aes(label = Var1, size = Freq, color = Freq)) +
            geom_text_wordcloud_area(shape = "square") +
            scale_size_area(max_size = 30) +
            theme_minimal() +
            scale_color_gradient(low = "mistyrose1", high = "firebrick3")
        SRFRF <- ggplot(arrange(as.data.frame(Get_word_stats(data, n_plots)[[19]]), -Freq), aes(label = Var1, size = Freq, color = Freq)) +
            geom_text_wordcloud_area(shape = "square") +
            scale_size_area(max_size = 30) +
            theme_minimal() +
            scale_color_gradient(low = "mistyrose1", high = "firebrick3")
        LOG_RISE <- ggplot(arrange(as.data.frame(Get_word_stats(data, n_plots)[[20]]), -Freq), aes(label = Var1, size = Freq, color = Freq)) +
            geom_text_wordcloud_area(shape = "square") +
            scale_size_area(max_size = 30) +
            theme_minimal() +
            scale_color_gradient(low = "mistyrose1", high = "firebrick3")
        LOG_FALL <- ggplot(arrange(as.data.frame(Get_word_stats(data, n_plots)[[21]]), -Freq), aes(label = Var1, size = Freq, color = Freq)) +
            geom_text_wordcloud_area(shape = "square") +
            scale_size_area(max_size = 30) +
            theme_minimal() +
            scale_color_gradient(low = "mistyrose1", high = "firebrick3")
        POS_FULL <- ggplot(arrange(as.data.frame(Get_word_stats(data, n_plots)[[22]]), -Freq), aes(label = Var1, size = Freq, color = Freq)) +
            geom_text_wordcloud_area(shape = "square") +
            scale_size_area(max_size = 30) +
            theme_minimal() +
            scale_color_gradient(low = "mistyrose1", high = "firebrick3")
        POS_PAR <- ggplot(arrange(as.data.frame(Get_word_stats(data, n_plots)[[23]]), -Freq), aes(label = Var1, size = Freq, color = Freq)) +
            geom_text_wordcloud_area(shape = "square") +
            scale_size_area(max_size = 30) +
            theme_minimal() +
            scale_color_gradient(low = "mistyrose1", high = "firebrick3")
        NEG_FULL <- ggplot(arrange(as.data.frame(Get_word_stats(data, n_plots)[[24]]), -Freq), aes(label = Var1, size = Freq, color = Freq)) +
            geom_text_wordcloud_area(shape = "square") +
            scale_size_area(max_size = 30) +
            theme_minimal() +
            scale_color_gradient(low = "mistyrose1", high = "firebrick3")
        NEG_PAR <- ggplot(arrange(as.data.frame(Get_word_stats(data, n_plots)[[25]]), -Freq), aes(label = Var1, size = Freq, color = Freq)) +
            geom_text_wordcloud_area(shape = "square") +
            scale_size_area(max_size = 30) +
            theme_minimal() +
            scale_color_gradient(low = "mistyrose1", high = "firebrick3")
        LRSF <- ggplot(arrange(as.data.frame(Get_word_stats(data, n_plots)[[26]]), -Freq), aes(label = Var1, size = Freq, color = Freq)) +
            geom_text_wordcloud_area(shape = "square") +
            scale_size_area(max_size = 30) +
            theme_minimal() +
            scale_color_gradient(low = "mistyrose1", high = "firebrick3")
        LRSFER <- ggplot(arrange(as.data.frame(Get_word_stats(data, n_plots)[[27]]), -Freq), aes(label = Var1, size = Freq, color = Freq)) +
            geom_text_wordcloud_area(shape = "square") +
            scale_size_area(max_size = 30) +
            theme_minimal() +
            scale_color_gradient(low = "mistyrose1", high = "firebrick3")

        equations <- list(LR, LF, LL, LM, LH, ERCV, EFCV, ERCC, EFCC,
                          SFR_FULL, SFR_PAR, SRF_FULL, SRF_PAR, SRFR_FULL,
                          SRFR_PAR, SFRF_FULL, SFRF_PAR, SFRFR,
                          SRFRF, LOG_RISE, LOG_FALL, POS_FULL, POS_PAR,
                          NEG_FULL, NEG_PAR, LRSF, LRSFER)

        return(equations)
    }

    # Print word clouds
    my_word_clouds <- plot_word_clouds(data)
    print_word_clouds <- for (i in 1:length(my_word_clouds)) { #print individual plots

        # Ggsave takes a long time but produces high quality images necessary for arranging them
        ggsave(paste0(plot_names[i], "_WC.png", sep = ""),
               plot = my_word_clouds[[i]], width = 30, height = 20, units = "cm")
    }

    #return(print_word_clouds)
}


ArrangeWordClouds <- function() {
    "Arrange word clouds and plot labels into a grid using ggplot.
    Input: none
    Output: a grid of word clouds (arranged in order of ascending enjoyment scores)
    "

    # (The simpler alternative is to use grid.arrange with lapply([PNGs], rasterGrob),
    # with [PNGs] being a set of predefined word clouds,
    # but rasterGrob produces an error when used in such a list.)

    a <- rasterGrob(readPNG(paste0(data_plot_long$plot_names[1], "_WC.png")), interpolate = TRUE)
    b <- rasterGrob(readPNG(paste0(data_plot_long$plot_names[2], "_WC.png")), interpolate = TRUE)
    c <- rasterGrob(readPNG(paste0(data_plot_long$plot_names[3], "_WC.png")), interpolate = TRUE)
    d <- rasterGrob(readPNG(paste0(data_plot_long$plot_names[4], "_WC.png")), interpolate = TRUE)
    e <- rasterGrob(readPNG(paste0(data_plot_long$plot_names[5], "_WC.png")), interpolate = TRUE)
    f <- rasterGrob(readPNG(paste0(data_plot_long$plot_names[6], "_WC.png")), interpolate = TRUE)
    g <- rasterGrob(readPNG(paste0(data_plot_long$plot_names[7], "_WC.png")), interpolate = TRUE)
    h <- rasterGrob(readPNG(paste0(data_plot_long$plot_names[8], "_WC.png")), interpolate = TRUE)
    i <- rasterGrob(readPNG(paste0(data_plot_long$plot_names[9], "_WC.png")), interpolate = TRUE)

    j <- rasterGrob(readPNG(paste0(data_plot_long$plot_names[1], "_plot.png")), interpolate = TRUE)
    k <- rasterGrob(readPNG(paste0(data_plot_long$plot_names[2], "_plot.png")), interpolate = TRUE)
    l <- rasterGrob(readPNG(paste0(data_plot_long$plot_names[3], "_plot.png")), interpolate = TRUE)
    m <- rasterGrob(readPNG(paste0(data_plot_long$plot_names[4], "_plot.png")), interpolate = TRUE)
    n <- rasterGrob(readPNG(paste0(data_plot_long$plot_names[5], "_plot.png")), interpolate = TRUE)
    o <- rasterGrob(readPNG(paste0(data_plot_long$plot_names[6], "_plot.png")), interpolate = TRUE)
    p <- rasterGrob(readPNG(paste0(data_plot_long$plot_names[7], "_plot.png")), interpolate = TRUE)
    q <- rasterGrob(readPNG(paste0(data_plot_long$plot_names[8], "_plot.png")), interpolate = TRUE)
    r <- rasterGrob(readPNG(paste0(data_plot_long$plot_names[9], "_plot.png")), interpolate = TRUE)

    s <- rasterGrob(readPNG(paste0(data_plot_long$plot_names[10], "_WC.png")), interpolate = TRUE)
    t <- rasterGrob(readPNG(paste0(data_plot_long$plot_names[11], "_WC.png")), interpolate = TRUE)
    u <- rasterGrob(readPNG(paste0(data_plot_long$plot_names[12], "_WC.png")), interpolate = TRUE)
    v <- rasterGrob(readPNG(paste0(data_plot_long$plot_names[13], "_WC.png")), interpolate = TRUE)
    w <- rasterGrob(readPNG(paste0(data_plot_long$plot_names[14], "_WC.png")), interpolate = TRUE)
    x <- rasterGrob(readPNG(paste0(data_plot_long$plot_names[15], "_WC.png")), interpolate = TRUE)
    y <- rasterGrob(readPNG(paste0(data_plot_long$plot_names[16], "_WC.png")), interpolate = TRUE)
    z <- rasterGrob(readPNG(paste0(data_plot_long$plot_names[17], "_WC.png")), interpolate = TRUE)
    aa <- rasterGrob(readPNG(paste0(data_plot_long$plot_names[18], "_WC.png")), interpolate = TRUE)

    bb <- rasterGrob(readPNG(paste0(data_plot_long$plot_names[10], "_plot.png")), interpolate = TRUE)
    cc <- rasterGrob(readPNG(paste0(data_plot_long$plot_names[11], "_plot.png")), interpolate = TRUE)
    dd <- rasterGrob(readPNG(paste0(data_plot_long$plot_names[12], "_plot.png")), interpolate = TRUE)
    ee <- rasterGrob(readPNG(paste0(data_plot_long$plot_names[13], "_plot.png")), interpolate = TRUE)
    ff <- rasterGrob(readPNG(paste0(data_plot_long$plot_names[14], "_plot.png")), interpolate = TRUE)
    gg <- rasterGrob(readPNG(paste0(data_plot_long$plot_names[15], "_plot.png")), interpolate = TRUE)
    hh <- rasterGrob(readPNG(paste0(data_plot_long$plot_names[16], "_plot.png")), interpolate = TRUE)
    ii <- rasterGrob(readPNG(paste0(data_plot_long$plot_names[17], "_plot.png")), interpolate = TRUE)
    jj <- rasterGrob(readPNG(paste0(data_plot_long$plot_names[18], "_plot.png")), interpolate = TRUE)

    kk <- rasterGrob(readPNG(paste0(data_plot_long$plot_names[19], "_WC.png")), interpolate = TRUE)
    ll <- rasterGrob(readPNG(paste0(data_plot_long$plot_names[20], "_WC.png")), interpolate = TRUE)
    mm <- rasterGrob(readPNG(paste0(data_plot_long$plot_names[21], "_WC.png")), interpolate = TRUE)
    nn <- rasterGrob(readPNG(paste0(data_plot_long$plot_names[22], "_WC.png")), interpolate = TRUE)
    oo <- rasterGrob(readPNG(paste0(data_plot_long$plot_names[23], "_WC.png")), interpolate = TRUE)
    pp <- rasterGrob(readPNG(paste0(data_plot_long$plot_names[24], "_WC.png")), interpolate = TRUE)
    qq <- rasterGrob(readPNG(paste0(data_plot_long$plot_names[25], "_WC.png")), interpolate = TRUE)
    rr <- rasterGrob(readPNG(paste0(data_plot_long$plot_names[26], "_WC.png")), interpolate = TRUE)
    ss <- rasterGrob(readPNG(paste0(data_plot_long$plot_names[27], "_WC.png")), interpolate = TRUE)

    tt <- rasterGrob(readPNG(paste0(data_plot_long$plot_names[19], "_plot.png")), interpolate = TRUE)
    uu <- rasterGrob(readPNG(paste0(data_plot_long$plot_names[20], "_plot.png")), interpolate = TRUE)
    vv <- rasterGrob(readPNG(paste0(data_plot_long$plot_names[21], "_plot.png")), interpolate = TRUE)
    ww <- rasterGrob(readPNG(paste0(data_plot_long$plot_names[22], "_plot.png")), interpolate = TRUE)
    xx <- rasterGrob(readPNG(paste0(data_plot_long$plot_names[23], "_plot.png")), interpolate = TRUE)
    yy <- rasterGrob(readPNG(paste0(data_plot_long$plot_names[24], "_plot.png")), interpolate = TRUE)
    zz <- rasterGrob(readPNG(paste0(data_plot_long$plot_names[25], "_plot.png")), interpolate = TRUE)
    aaa <- rasterGrob(readPNG(paste0(data_plot_long$plot_names[26], "_plot.png")), interpolate = TRUE)
    bbb <- rasterGrob(readPNG(paste0(data_plot_long$plot_names[27], "_plot.png")), interpolate = TRUE)

    #create a grid of word clouds and plot labels
    df <- data.frame() #empty dataframe
    wc_plot <- ggplot(df) + #empty ggplot
        geom_point() +
        xlim(0, 8) +
        ylim(-0.5, 9.5) +
        theme_void() +
        annotation_custom(a, xmin = -0.5, xmax = 0.5, ymin = 8, ymax = 10) +
        annotation_custom(b, xmin = 0.5, xmax = 1.5, ymin = 8, ymax = 10) +
        annotation_custom(c, xmin = 1.5, xmax = 2.5, ymin = 8, ymax = 10) +
        annotation_custom(d, xmin = 2.5, xmax = 3.5, ymin = 8, ymax = 10) +
        annotation_custom(e, xmin = 3.5, xmax = 4.5, ymin = 8, ymax = 10) +
        annotation_custom(f, xmin = 4.5, xmax = 5.5, ymin = 8, ymax = 10) +
        annotation_custom(g, xmin = 5.5, xmax = 6.5, ymin = 8, ymax = 10) +
        annotation_custom(h, xmin = 6.5, xmax = 7.5, ymin = 8, ymax = 10) +
        annotation_custom(i, xmin = 7.5, xmax = 8.5, ymin = 8, ymax = 10) +

        annotation_custom(j, xmin = -0.25, xmax = 0.25, ymin = 7, ymax = 8) +
        annotation_custom(k, xmin = 0.75, xmax = 1.25, ymin = 7, ymax = 8) +
        annotation_custom(l, xmin = 1.75, xmax = 2.25, ymin = 7, ymax = 8) +
        annotation_custom(m, xmin = 2.75, xmax = 3.25, ymin = 7, ymax = 8) +
        annotation_custom(n, xmin = 3.75, xmax = 4.25, ymin = 7, ymax = 8) +
        annotation_custom(o, xmin = 4.75, xmax = 5.25, ymin = 7, ymax = 8) +
        annotation_custom(p, xmin = 5.75, xmax = 6.25, ymin = 7, ymax = 8) +
        annotation_custom(q, xmin = 6.75, xmax = 7.25, ymin = 7, ymax = 8) +
        annotation_custom(r, xmin = 7.75, xmax = 8.25, ymin = 7, ymax = 8) +

        annotation_custom(s, xmin = -0.5, xmax = 0.5, ymin = 4, ymax = 6) +
        annotation_custom(t, xmin = 0.5, xmax = 1.5, ymin = 4, ymax = 6) +
        annotation_custom(u, xmin = 1.5, xmax = 2.5, ymin = 4, ymax = 6) +
        annotation_custom(v, xmin = 2.5, xmax = 3.5, ymin = 4, ymax = 6) +
        annotation_custom(w, xmin = 3.5, xmax = 4.5, ymin = 4, ymax = 6) +
        annotation_custom(x, xmin = 4.5, xmax = 5.5, ymin = 4, ymax = 6) +
        annotation_custom(y, xmin = 5.5, xmax = 6.5, ymin = 4, ymax = 6) +
        annotation_custom(z, xmin = 6.5, xmax = 7.5, ymin = 4, ymax = 6) +
        annotation_custom(aa, xmin = 7.5, xmax = 8.5, ymin = 4, ymax = 6) +

        annotation_custom(bb, xmin = -0.25, xmax = 0.25, ymin = 3, ymax = 4) +
        annotation_custom(cc, xmin = 0.75, xmax = 1.25, ymin = 3, ymax = 4) +
        annotation_custom(dd, xmin = 1.75, xmax = 2.25, ymin = 3, ymax = 4) +
        annotation_custom(ee, xmin = 2.75, xmax = 3.25, ymin = 3, ymax = 4) +
        annotation_custom(ff, xmin = 3.75, xmax = 4.25, ymin = 3, ymax = 4) +
        annotation_custom(gg, xmin = 4.75, xmax = 5.25, ymin = 3, ymax = 4) +
        annotation_custom(hh, xmin = 5.75, xmax = 6.25, ymin = 3, ymax = 4) +
        annotation_custom(ii, xmin = 6.75, xmax = 7.25, ymin = 3, ymax = 4) +
        annotation_custom(jj, xmin = 7.75, xmax = 8.25, ymin = 3, ymax = 4) +

        annotation_custom(kk, xmin = -0.5, xmax = 0.5, ymin = 0, ymax = 2) +
        annotation_custom(ll, xmin = 0.5, xmax = 1.5, ymin = 0, ymax = 2) +
        annotation_custom(mm, xmin = 1.5, xmax = 2.5, ymin = 0, ymax = 2) +
        annotation_custom(nn, xmin = 2.5, xmax = 3.5, ymin = 0, ymax = 2) +
        annotation_custom(oo, xmin = 3.5, xmax = 4.5, ymin = 0, ymax = 2) +
        annotation_custom(pp, xmin = 4.5, xmax = 5.5, ymin = 0, ymax = 2) +
        annotation_custom(qq, xmin = 5.5, xmax = 6.5, ymin = 0, ymax = 2) +
        annotation_custom(rr, xmin = 6.5, xmax = 7.5, ymin = 0, ymax = 2) +
        annotation_custom(ss, xmin = 7.5, xmax = 8.5, ymin = 0, ymax = 2) +

        annotation_custom(tt, xmin = -0.25, xmax = 0.25, ymin = -1, ymax = 0) +
        annotation_custom(uu, xmin = 0.75, xmax = 1.25, ymin = -1, ymax = 0) +
        annotation_custom(vv, xmin = 1.75, xmax = 2.25, ymin = -1, ymax = 0) +
        annotation_custom(ww, xmin = 2.75, xmax = 3.25, ymin = -1, ymax = 0) +
        annotation_custom(xx, xmin = 3.75, xmax = 4.25, ymin = -1, ymax = 0) +
        annotation_custom(yy, xmin = 4.75, xmax = 5.25, ymin = -1, ymax = 0) +
        annotation_custom(zz, xmin = 5.75, xmax = 6.25, ymin = -1, ymax = 0) +
        annotation_custom(aaa, xmin = 6.75, xmax = 7.25, ymin = -1, ymax = 0) +
        annotation_custom(bbb, xmin = 7.75, xmax = 8.25, ymin = -1, ymax = 0)

    return(wc_plot)
}

##================================================================================================================
##FUNCTIONS FOR PLOTTING SENTIMENT BAR PLOT##
##================================================================================================================

CalculateSentiment <- function(rword) {
    rword <- word(tolower(rword), 1)
    rword <- gsub("[^a-z]", "", rword) #get rid of numbers and special characters, leaving only letters a-z
    return( sentiment_by(rword)$ave_sentiment )
}


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
##FUNCTIONS FOR WORD ANALYSIS##
##================================================================================================================

GetWordAnalysis <- function(data, n_plots) {
    "
    Export the list of dataframes of words into a csv file (for semantic analysis in Google Colab code)
    Input: data_long, n_plots
    Output: equations (a list of dataframes of cleaned words from Get_word_stats())
    "

    equations <- c()
    for (i in 1:n_plots) {
        equations[[i]] <- paste0(Get_word_stats(data, n_plots)[[i]]$Var1, collapse = ", ")
    }

    return(equations)
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
    for (i in 1:n_plots) {
        equations[[i]] <- word_gen[seq(i, length(word_gen), n_plots)]
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


TopicModeling <- function(dat_long, n_plots, plot_names) {
    "
    Clean words, then plot topics models.
    Input: data_long, n_plots, plot_names
    Output: topic model word clouds
    "

    # 1. Topic Modeling

    # Create list of all participant words categorized by customer journeys
    words_raw <- c()
    for (i in 1:n_plots) {
        words_raw[[i]] <- paste0(wordStem(Get_word_stats(dat_long, n_plots)[[i]]$Var1), collapse = " ")
    }
    words_raw <- unlist(words_raw)
    names(words_raw) <- plot_names
    words_raw[1:15]

    # Create and clean corpus from word list
    # words_corpus <- Corpus(VectorSource(words_raw))
    cleaned_words_corpus <- Corpus(VectorSource(words_raw)) %>%
        tm_map(content_transformer(tolower)) %>%
        tm_map(removePunctuation) %>%
        #tm_map(removeWords, c(stopwords("english"), myStopwords)) %>%
        tm_map(removeNumbers) %>%
        tm_map(stripWhitespace)

    # Word cloud visualization
    set.seed(1)
    wordcloud(cleaned_words_corpus, colors = brewer.pal(8, "Dark2"), min.freq = 3, random.order = FALSE)

    # Convert corpus to term document matrix
    words_tdm <- TermDocumentMatrix(cleaned_words_corpus)
    words_tdm <- t(words_tdm) #switch the rows and columns so that rows = docs and cols = terms

    # Tf-idf filtering
    # tf_idf <- tapply(words_tdm$v/row_sums(words_tdm)[words_tdm$i], words_tdm$j, mean) * log2(nDocs(words_tdm)/col_sums(words_tdm > 0))
    # #words_tdm <- words_tdm[, tf_idf >= median(tf_idf)]     ## use median cut
    # words_tdm <- words_tdm[, tf_idf >= quantile(tf_idf, probs = c(.10))] #remove terms that occur in less than 10% of the documents
    # words_ind <- which(rowSums(as.matrix(words_tdm)) > 0)
    # words_tdm <- words_tdm[words_ind, ]     ## keep 50% of the most important words
    # words_tdm                      ## DTM still tf-weighted

    # Finding a reasonable number of topics
    k_topics <- FindTopicsNumber(words_tdm, topics = 2:n_plots, metrics = c("Arun2010", "CaoJuan2009", "Griffiths2004", "Deveaud2014"), control = list(seed = 1))
    FindTopicsNumber_plot(k_topics)

    # Fit topic model
    words_k <- 3 #although the metrics suggest using 5-6, 3 is easier to interpret
    words_lda <- LDA(words_tdm, k = words_k, control = list(seed = 1)) #we use the simple EM estimation (another alternative would be Gibbs sampling)
    words_lda

    words_prob <- posterior(words_lda)
    words_prob_terms <- as.data.frame(t(words_prob$terms))
    round(head(words_prob_terms, 10), 4) #probabilistic assignments of words to clusters
    terms(words_lda, 5) #top 5 terms in each topic
    topics(words_lda) #assignments of customer journeys to each topic

    # Visualization using word clouds
    set.seed(1)
    words_gathered <- words_prob_terms %>%
        mutate(word = rownames(words_prob_terms)) %>%
        gather(topic, weight, -word)

    n <- 50
    pal <- rep(brewer.pal(9, "Greys"), each = ceiling(n / 9))[n:1]
    dev.new()
    op <- par(mfrow = c(3, 2), mar = c(3, 0, 2, 0))
    for (i in 1:words_k) {
        words_final <- words_gathered %>%
            dplyr::filter(topic == i) %>%
            arrange(desc(weight))
        with(words_final[1:n,], wordcloud(word, freq = weight, scale = c(2, 0.5), random.order = FALSE,
                                          ordered.colors = TRUE, colors = pal))
        title(paste("Participant Words Topic", i))
    }
    par(op)

    #-------------------------------------------------------------------------------------------------------------------

    #2. Frequency Graphs

    # Define lists
    topic_names <- c("Positive", "Fluctuating", "Negative") #set manually
    freq_df_list <- c()

    # Plot frequency graphs
    for (i in 1:words_k) {

        # Create frequency data frame
        freq_df <- words_prob_terms[order(words_prob_terms[, i], decreasing = TRUE),]
        terms <- factor(rownames(freq_df)[1:15], levels = rownames(freq_df)[1:15])
        freq <- freq_df[1:15, i]
        freq_df_each <- data.frame(terms, freq)

        # Plot frequency bar graph
        freq_df_list[[i]] <- ggplot(data = freq_df_each, mapping = aes(x = freq, y = terms)) +
            theme_classic() +
            geom_col() +
            scale_y_discrete(limits = rev(levels(freq_df_each$terms))) +
            theme(axis.text = element_text(color = "black", size = 25),
                  axis.title.y = element_blank(),
                  axis.title.x = element_blank())
    }

    # Using topics(words_lda), automatically assign plots to their respective topic model
    # Define blank plot and universal starting point
    png("NA_plot.png") #used for when rownames(words_df_i) runs out when everything is automated
    dev.off()
    start <- -0.5005
    image_size <- 0.5

    # Frequency Plot 1
    words_df_1 <- as.data.frame(topics(words_lda)[as.data.frame(topics(words_lda)) == 1])
    end_1 <- (sort(freq_df[, 1], decreasing = TRUE)[1]) + start
    spacing_1 <- (end_1 - start) / (length(rownames(words_df_1)) - 1)
    plots_by_topic_1 <- axis_canvas(freq_df_list[[1]], axis = 'x') +
        draw_image(paste0(rownames(words_df_1)[1], "_plot.png"), scale = image_size, x = start) +
        draw_image(paste0(rownames(words_df_1)[2], "_plot.png"), scale = image_size, x = start + spacing_1) +
        draw_image(paste0(rownames(words_df_1)[3], "_plot.png"), scale = image_size, x = start + 2 * spacing_1) +
        draw_image(paste0(rownames(words_df_1)[4], "_plot.png"), scale = image_size, x = start + 3 * spacing_1) +
        draw_image(paste0(rownames(words_df_1)[5], "_plot.png"), scale = image_size, x = start + 4 * spacing_1) +
        draw_image(paste0(rownames(words_df_1)[6], "_plot.png"), scale = image_size, x = start + 5 * spacing_1) +
        draw_image(paste0(rownames(words_df_1)[7], "_plot.png"), scale = image_size, x = start + 6 * spacing_1) +
        draw_image(paste0(rownames(words_df_1)[8], "_plot.png"), scale = image_size, x = start + 7 * spacing_1) +
        draw_image(paste0(rownames(words_df_1)[9], "_plot.png"), scale = image_size, x = start + 8 * spacing_1) +
        draw_image(paste0(rownames(words_df_1)[10], "_plot.png"), scale = image_size, x = start + 9 * spacing_1) +
        draw_image(paste0(rownames(words_df_1)[11], "_plot.png"), scale = image_size, x = start + 10 * spacing_1) +
        draw_image(paste0(rownames(words_df_1)[12], "_plot.png"), scale = image_size, x = start + 11 * spacing_1)

    # Frequency Plot 2
    words_df_2 <- as.data.frame(topics(words_lda)[as.data.frame(topics(words_lda)) == 2])
    end_2 <- (sort(freq_df[, 2], decreasing = TRUE)[1]) + start
    spacing_2 <- (end_2 - start) / (length(rownames(words_df_2)) - 1)
    plots_by_topic_2 <- axis_canvas(freq_df_list[[2]], axis = 'x') +
        draw_image(paste0(rownames(words_df_2)[1], "_plot.png"), scale = image_size, x = start) +
        draw_image(paste0(rownames(words_df_2)[2], "_plot.png"), scale = image_size, x = start + spacing_2) +
        draw_image(paste0(rownames(words_df_2)[3], "_plot.png"), scale = image_size, x = start + 2 * spacing_2) +
        draw_image(paste0(rownames(words_df_2)[4], "_plot.png"), scale = image_size, x = start + 3 * spacing_2) +
        draw_image(paste0(rownames(words_df_2)[5], "_plot.png"), scale = image_size, x = start + 4 * spacing_2) +
        draw_image(paste0(rownames(words_df_2)[6], "_plot.png"), scale = image_size, x = start + 5 * spacing_2) +
        draw_image(paste0(rownames(words_df_2)[7], "_plot.png"), scale = image_size, x = start + 6 * spacing_2) +
        draw_image(paste0(rownames(words_df_2)[8], "_plot.png"), scale = image_size, x = start + 7 * spacing_2) +
        draw_image(paste0(rownames(words_df_2)[9], "_plot.png"), scale = image_size, x = start + 8 * spacing_2) +
        draw_image(paste0(rownames(words_df_2)[10], "_plot.png"), scale = image_size, x = start + 9 * spacing_2) +
        draw_image(paste0(rownames(words_df_2)[11], "_plot.png"), scale = image_size, x = start + 10 * spacing_2) +
        draw_image(paste0(rownames(words_df_2)[12], "_plot.png"), scale = image_size, x = start + 11 * spacing_2)

    # Frequency Plot 3
    words_df_3 <- as.data.frame(topics(words_lda)[as.data.frame(topics(words_lda)) == 3])
    end_3 <- (sort(freq_df[, 3], decreasing = TRUE)[1]) + start
    spacing_3 <- (end_3 - start) / (length(rownames(words_df_3)) - 1)
    plots_by_topic_3 <- axis_canvas(freq_df_list[[3]], axis = 'x') +
        draw_image(paste0(rownames(words_df_3)[1], "_plot.png"), scale = image_size, x = start) +
        draw_image(paste0(rownames(words_df_3)[2], "_plot.png"), scale = image_size, x = start + spacing_3) +
        draw_image(paste0(rownames(words_df_3)[3], "_plot.png"), scale = image_size, x = start + 2 * spacing_3) +
        draw_image(paste0(rownames(words_df_3)[4], "_plot.png"), scale = image_size, x = start + 3 * spacing_3) +
        draw_image(paste0(rownames(words_df_3)[5], "_plot.png"), scale = image_size, x = start + 4 * spacing_3) +
        draw_image(paste0(rownames(words_df_3)[6], "_plot.png"), scale = image_size, x = start + 5 * spacing_3) +
        draw_image(paste0(rownames(words_df_3)[7], "_plot.png"), scale = image_size, x = start + 6 * spacing_3) +
        draw_image(paste0(rownames(words_df_3)[8], "_plot.png"), scale = image_size, x = start + 7 * spacing_3) +
        draw_image(paste0(rownames(words_df_3)[9], "_plot.png"), scale = image_size, x = start + 8 * spacing_3) +
        draw_image(paste0(rownames(words_df_3)[10], "_plot.png"), scale = image_size, x = start + 9 * spacing_3) +
        draw_image(paste0(rownames(words_df_3)[11], "_plot.png"), scale = image_size, x = start + 10 * spacing_3) +
        draw_image(paste0(rownames(words_df_3)[12], "_plot.png"), scale = image_size, x = start + 11 * spacing_3)

    # Frequency Plot 4
    # words_df_4 <- as.data.frame(topics(words_lda)[as.data.frame(topics(words_lda)) == 4])
    # end_4 <- (sort(freq_df[, 4], decreasing = TRUE)[1])-0.5005
    # spacing_4 <- (end_4 - start)/(length(rownames(words_df_4))-1)
    # plots_by_topic_4 <- axis_canvas(freq_df_list[[4]], axis = 'x') +
    #   draw_image(paste0(rownames(words_df_4)[1], "_plot.png"), x = start) +
    #   draw_image(paste0(rownames(words_df_4)[2], "_plot.png"), x = start + spacing_4) +
    #   draw_image(paste0(rownames(words_df_4)[3], "_plot.png"), x = start + 2*spacing_4) +
    #   draw_image(paste0(rownames(words_df_4)[4], "_plot.png"), x = start + 3*spacing_4) +
    #   draw_image(paste0(rownames(words_df_4)[5], "_plot.png"), x = start + 4*spacing_4) +
    #   draw_image(paste0(rownames(words_df_4)[6], "_plot.png"), x = start + 5*spacing_4) +
    #   draw_image(paste0(rownames(words_df_4)[7], "_plot.png"), x = start + 6*spacing_4) +
    #   draw_image(paste0(rownames(words_df_4)[8], "_plot.png"), x = start + 7*spacing_4) +
    #   draw_image(paste0(rownames(words_df_4)[9], "_plot.png"), x = start + 8*spacing_4) +
    #   draw_image(paste0(rownames(words_df_4)[10], "_plot.png"), x = start + 9*spacing_4) +
    #   draw_image(paste0(rownames(words_df_4)[11], "_plot.png"), x = start + 10*spacing_4) +
    #   draw_image(paste0(rownames(words_df_4)[12], "_plot.png"), x = start + 11*spacing_4)

    # Frequency Plot 5
    # words_df_5 <- as.data.frame(topics(words_lda)[as.data.frame(topics(words_lda)) == 5])
    # end_5 <- (sort(freq_df[, 5], decreasing = TRUE)[1])-0.5005
    # spacing_5 <- (end_5 - start)/(length(rownames(words_df_5))-1)
    # plots_by_topic_5 <- axis_canvas(freq_df_list[[5]], axis = 'x') +
    #   draw_image(paste0(rownames(words_df_5)[1], "_plot.png"), x = start) +
    #   draw_image(paste0(rownames(words_df_5)[2], "_plot.png"), x = start + spacing_5) +
    #   draw_image(paste0(rownames(words_df_5)[3], "_plot.png"), x = start + 2*spacing_5) +
    #   draw_image(paste0(rownames(words_df_5)[4], "_plot.png"), x = start + 3*spacing_5) +
    #   draw_image(paste0(rownames(words_df_5)[5], "_plot.png"), x = start + 4*spacing_5) +
    #   draw_image(paste0(rownames(words_df_5)[6], "_plot.png"), x = start + 5*spacing_5) +
    #   draw_image(paste0(rownames(words_df_5)[7], "_plot.png"), x = start + 6*spacing_5) +
    #   draw_image(paste0(rownames(words_df_5)[8], "_plot.png"), x = start + 7*spacing_5) +
    #   draw_image(paste0(rownames(words_df_5)[9], "_plot.png"), x = start + 8*spacing_5) +
    #   draw_image(paste0(rownames(words_df_5)[10], "_plot.png"), x = start + 9*spacing_5) +
    #   draw_image(paste0(rownames(words_df_5)[11], "_plot.png"), x = start + 10*spacing_5) +
    #   draw_image(paste0(rownames(words_df_5)[12], "_plot.png"), x = start + 11*spacing_5)

    plot_1 <- ggdraw(insert_xaxis_grob(freq_df_list[[1]], plots_by_topic_1, position = "top"))
    plot_2 <- ggdraw(insert_xaxis_grob(freq_df_list[[2]], plots_by_topic_2, position = "top"))
    plot_3 <- ggdraw(insert_xaxis_grob(freq_df_list[[3]], plots_by_topic_3, position = "top"))
    arrange_topics <- ggarrange(plot_3, plot_2, plot_1, nrow = 1, ncol = 3)
    arrange_topics <- annotate_figure(arrange_topics,
                                      left = text_grob("Words", color = "black", face = "bold", size = 25, rot = 90),
                                      bottom = text_grob("Frequency", color = "black", face = "bold", size = 25, vjust = 0.4))
    print(arrange_topics)

    ggsave("topic_model_freq_bar.pdf", arrange_topics, height = 6, width = 18)

    # plot_4 <- ggdraw(insert_xaxis_grob(freq_df_list[[4]], plots_by_topic_4, position = "top"))
    # plot_5 <- ggdraw(insert_xaxis_grob(freq_df_list[[5]], plots_by_topic_5, position = "top"))
    # ggsave("topic_model_freq_bar.pdf", arrangeGrob(plot_1, plot_2, plot_3, plot_4, plot_5), height = 9, width = 8)
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


CreateDataFeaturesDF <- function(data, e3_dat_final, features_df, n_after_exclusions, num_subjects_and_plots) {
    "
    Bind the three dataframes: data, sentiment score, and standardize(features), i.e., the standardized plot features.
    Input: data_long, e3_dat_final, features, n_after_exclusions, num_subjects_and_plots
    Output: score_features_df (which contains all of the predictors and participant scores)
    "

    score_features_df["willing"] <- as.data.frame(apply(score_features_df["willing"], 2, as.numeric))
    score_features_df["subject"] <- as.data.frame(apply(score_features_df["subject"], 2, as.numeric))
    score_features_df["genre"] <- as.data.frame(as.numeric(factor(score_features_df$genre)))
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

            for (k in 1: n_plots) {  # Now
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

    # Add to the plot: stars indicating significance
    predictors_plot <- predictors_plot +

        # One-sided Wilcox test
        ggplot2::annotate("text", x = enjoyment_bottom_x, y = enjoyment_bottom_y, size = 8, label = p_value_stars_enjoyment[[1]]) +
        ggplot2::annotate("text", x = enjoyment_bottom_x + 1, y = enjoyment_bottom_y, size = 8, label = p_value_stars_enjoyment[[2]]) +
        ggplot2::annotate("text", x = enjoyment_bottom_x + 2, y = enjoyment_bottom_y, size = 8, label = p_value_stars_enjoyment[[3]]) +
        ggplot2::annotate("text", x = enjoyment_bottom_x + 3, y = enjoyment_bottom_y, size = 8, label = p_value_stars_enjoyment[[4]]) +
        ggplot2::annotate("text", x = enjoyment_bottom_x + 4, y = enjoyment_bottom_y, size = 8, label = p_value_stars_enjoyment[[5]]) +
        ggplot2::annotate("text", x = enjoyment_bottom_x + 5, y = enjoyment_bottom_y, size = 8, label = p_value_stars_enjoyment[[6]]) +
        ggplot2::annotate("text", x = enjoyment_bottom_x + 6, y = enjoyment_bottom_y, size = 8, label = p_value_stars_enjoyment[[7]]) +
        ggplot2::annotate("text", x = enjoyment_bottom_x + 7, y = enjoyment_bottom_y, size = 8, label = p_value_stars_enjoyment[[8]]) +
        ggplot2::annotate("text", x = enjoyment_bottom_x + 8, y = enjoyment_bottom_y, size = 8, label = p_value_stars_enjoyment[[9]]) +
        ggplot2::annotate("text", x = enjoyment_bottom_x + 9, y = enjoyment_bottom_y, size = 8, label = p_value_stars_enjoyment[[10]]) +
        ggplot2::annotate("text", x = enjoyment_bottom_x + 10, y = enjoyment_bottom_y, size = 8, label = p_value_stars_enjoyment[[11]]) +
        ggplot2::annotate("text", x = enjoyment_bottom_x + 11, y = enjoyment_bottom_y, size = 8, label = p_value_stars_enjoyment[[12]]) +
        ggplot2::annotate("text", x = enjoyment_bottom_x + 12, y = enjoyment_bottom_y, size = 8, label = p_value_stars_enjoyment[[13]]) +
        ggplot2::annotate("text", x = enjoyment_bottom_x + 13, y = enjoyment_bottom_y, size = 8, label = p_value_stars_enjoyment[[14]]) +
        ggplot2::annotate("text", x = enjoyment_bottom_x + 14, y = enjoyment_bottom_y, size = 8, label = p_value_stars_enjoyment[[15]]) +
        ggplot2::annotate("text", x = enjoyment_bottom_x + 15, y = enjoyment_bottom_y, size = 8, label = p_value_stars_enjoyment[[16]]) +
        ggplot2::annotate("text", x = enjoyment_bottom_x + 16, y = enjoyment_bottom_y, size = 8, label = p_value_stars_enjoyment[[17]]) +
        ggplot2::annotate("text", x = enjoyment_bottom_x + 17, y = enjoyment_bottom_y, size = 8, label = p_value_stars_enjoyment[[18]]) +
        ggplot2::annotate("text", x = enjoyment_bottom_x + 18, y = enjoyment_bottom_y, size = 8, label = p_value_stars_enjoyment[[19]]) +
        ggplot2::annotate("text", x = enjoyment_bottom_x + 19, y = enjoyment_bottom_y, size = 8, label = p_value_stars_enjoyment[[20]])

    #-------------------------------------------------------------------------------------------------------------------
    return(predictors_plot)
}


AnalyzeRidgeRegression <- function(score_features_df) {
    "
    Measure the performance of individual predictors by doing cross-validated (nfold = 10) ridge regression
    Input: score_features_df
    Output: Ridge regression results for enjoyment and personal desirability scores and their plots
    "

    # Define the columns that we want for the regression: from embeddings to integral and the D1 & D2 predictors.
    score_features_ss <- subset(score_features_df, select = c(embeddings:integral, d1_avg_unweight:d1_avg_weight_end, d2_avg_unweight:d2_avg_weight_end))
    my_predictors <- data.matrix(score_features_ss)

    # 1. enjoyment
    enjoyment_scores <- score_features_df$enjoyment

    # Create testing and training data
    set.seed(1)
    indeces <- sample(nrow(my_predictors), nrow(my_predictors) * 0.8)

    my_predictors_train <- my_predictors[indeces,]
    enjoyment_scores_train <- enjoyment_scores[indeces]

    my_predictors_test <- my_predictors[-indeces,]
    enjoyment_scores_test <- enjoyment_scores[-indeces]

    # Standardize data
    my_predictors_train_stdz <- apply(my_predictors_train, 2, scale)
    enjoyment_scores_train_stdz <- scale(enjoyment_scores_train)

    my_predictors_test_stdz <- apply(my_predictors_test, 2, scale)
    enjoyment_scores_test_stdz <- scale(enjoyment_scores_test)

    # Run regular regression
    lm_enjoyment_features <- glmnet(my_predictors_train_stdz, enjoyment_scores_train_stdz,
                                       alpha = 0, lambda = 0)

    # Run ridge regression
    set.seed(123)
    lambdas <- seq(1, 10e-5, length = 100)
    ridge_enjoyment_features <- cv.glmnet(my_predictors_train_stdz, enjoyment_scores_train_stdz,
                                             nfolds = 10, alpha = 0, lambda = lambdas)
    plot(ridge_enjoyment_features)
    lambda_best <- ridge_enjoyment_features$lambda.min
    ridge_enjoyment_features1 <- glmnet(my_predictors_train_stdz, enjoyment_scores_train_stdz,
                                           alpha = 0, lambda = lambda_best)

    # Order the regression results from most to least important predictors (rounded to 5 digits)
    enjoyment_features_rounded <- round(coef(ridge_enjoyment_features1), 5)
    x_enjoyment_ordered = order(enjoyment_features_rounded@x, decreasing = TRUE)
    enjoyment_features_rounded@Dimnames[[1]] <- enjoyment_features_rounded@Dimnames[[1]][x_enjoyment_ordered]
    enjoyment_features_rounded@x <- enjoyment_features_rounded@x[x_enjoyment_ordered]

    print('enjoyment vs. features:')
    print(enjoyment_features_rounded)

    # Compare standard regression to ridge regression
    predict_enjoyment_lm <- predict(lm_enjoyment_features, my_predictors_test_stdz)
    mse_enjoyment_lm <- mean((enjoyment_scores_test_stdz - predict_enjoyment_lm)^2)
    mse_enjoyment_lm
    predict_enjoyment_ridge <- predict(ridge_enjoyment_features1, my_predictors_test_stdz)
    mse_enjoyment_ridge <- mean((enjoyment_scores_test_stdz - predict_enjoyment_ridge)^2)
    mse_enjoyment_ridge
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

    plot_Fit(enjoyment_scores_test_stdz, predict_enjoyment_lm, my_title = "LM: enjoyment")
    plot_Fit(enjoyment_scores_test_stdz, predict_enjoyment_ridge, my_title = "RIDGE: enjoyment")

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

d_long[, "sentiment_score"] <- sapply(d_long["word"], CalculateSentiment)
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
#score_features_df <- CreateDataFeaturesDF(d_long, e4_dat_final, features, n_after_exclusions, num_subjects_and_plots)

# Run regularized regression on all predictors
#ridge_regression_wt_predictors <- AnalyzeRidgeRegression(score_features_df)

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
file.move(plot_files, "analysis_plots", overwrite = TRUE)
analysis_files <- list.files(pattern = c("word_analysis_e4.csv|embeddings_e4.csv|correlations_e4.csv"))
file.move(analysis_files, "data", overwrite = TRUE)

##================================================================================================================
##END##
##================================================================================================================


