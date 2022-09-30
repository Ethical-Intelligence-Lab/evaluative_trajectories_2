# Analysis script for 'Lifelines'
# Experiment 1

# Set working directory to current file location 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Clear workspace
rm(list = ls()) 

# Import libraries
if (!require(pacman)) {install.packages("pacman")}
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
               'wordcloud', #visualize wordclouds for topic models 
               'lme4', #run mixed effects linear regression
               'lmerTest', #used in conjunction with lme4; get p-values
               'robustHD', #for the standardize function
               'corrplot', #for corrplot()
               # 'psych', #for principal components analysis (PCA)
               # 'multicon', #for split-half correlations 
               # 'recipes', #for feature engineering
               # 'caret', #for automating tuning process
               'tidyr', #for gather(), which takes multiple columns and collapses them into key-value pairs
               'tidyverse', #used in conjunction with tidyr; contains dplyr, used for select(); load last because of conflict!
               'slam', #utility functions for sparse matrices 
               'broom', #install separately if does not work 
               'filesstrings' #create and move files
)

# Source Lifelines_power_sim_e4.R script for clustering function and dendrogram plot 
source2 <- function(file, start, end, ...) {
  file.lines <- scan(file, what=character(), skip=start-1, nlines=end-start+1, sep='\n')
  file.lines.collapsed <- paste(file.lines, collapse='\n')
  source(textConnection(file.lines.collapsed), ...)
}

source2("Lifelines_power_sim_e4.R", 1, 375) #import Lifelines_power_sim_e4.R script, which calls Lifelines_analysis_e1b.R script  
setwd("../e4_evaluative_journeys") #go back to current directory

# ============================================= DEFINE FUNCTIONS ============================================= #


##================================================================================================================
                                        ##FUNCTIONS FOR PREPROCESSING##
##================================================================================================================


PerformExclusions <- function(data) {
    "
    Excludes participants if they do not finish the survey, finished it too quickly (under 120 seconds), 
    gave duplicate answers, or failed important attention and comprehension checks.
    Input: e4_data   #num_rows = num_ss
    Output: data after it has been 'cleaned'
    "
    
    # Exclude those who did not finish the survey
    data <- subset(data, (data$Finished == TRUE))
    n_before_exclusions <- dim(data)[1] #49 
    
    # Exclude those who finished it in less than 2 minutes
    data <- subset(data, (data$Duration..in.seconds. > 120))
    
    # Exclude those who gave the same exact answers to sentence/word generation questions across lifelines 
    sentence_cols <- data[, grep("sent_gen", colnames(data), value = TRUE)] 
    sentence_dups <- sentence_cols[apply(sentence_cols, 1, function(x) length(unique(x[!is.na(x)])) == 1), ] 
    data <- anti_join(data, sentence_dups, by = grep("sent_gen", colnames(data), value = TRUE)) 
    
    word_cols <- data[, grep("word_gen", colnames(data), value = TRUE)]
    word_dups <- word_cols[apply(word_cols, 1, function(x) length(unique(x[!is.na(x)])) == 1), ]
    data <- anti_join(data, word_dups, by = grep("word_gen", colnames(data), value = TRUE))
    
    #(1) attention checks
    
    # Perform first round of attention checks
    data$attention_check <- ifelse( ((data$att_check_1 == 'Paul') &
                                       (data$att_check_2 == 'Purple')), 0, 1 )
    
    # Perform second round of attention checks, if they failed the first
    data$attention_check <- ifelse( ( (is.na(data$att_check_3_1 == TRUE)) |
                                        ((data$att_check_4 == 0) &
                                           (data$att_check_3_3 > data$att_check_3_2) &
                                           (data$att_check_3_2 > data$att_check_3_1) &
                                           (data$att_check_3_2%%10 == 0) &
                                           (data$att_check_3_1 == 15) ) ), 0, 1)
    
    print(paste('percentage excluded, attention checks: ',
                table(data$attention_check)[2]/length(data$attention_check)))
    
    # Perform comprehension checks
    data$attention_check2 <- ifelse( ((data$comp_check_1 == "Jan-22" &
                                        data$comp_check_2 == "Apr-21" &
                                        data$comp_check_3 == 0) | 
                                     (data$comp_check_1_NE == "Jan-22" &
                                        data$comp_check_2_NE == "Apr-21" &
                                        data$comp_check_3_NE == 0)), 
                                     0, 1 )
    
    #(2) comprehension questions
    
    #Perform second round of comprehension checks, if they failed the first
    data$comp_check <- ifelse( ( ( (is.na(data$comp_check_4 == TRUE)) &
                                   (data$comp_check_7 == "PHash/s") &
                                   (data$comp_check_8 == "Date") &
                                   (data$comp_check_9 == "Give a one-sentence summary of the hashrate trajectory")
                                 |
                                   ((data$comp_check_4 == "Jan-21") & 
                                      (data$comp_check_5 == "Oct-21") & 
                                      (data$comp_check_6 == 100) & 
                                      (data$comp_check_7 == "PHash/s") &
                                      (data$comp_check_8 == "Date") &
                                      (data$comp_check_9 == "Give a one-sentence summary of the hashrate trajectory") ) ) 
                               |   
                                ( (is.na(data$comp_check_4_NE == TRUE)) &
                                     (data$comp_check_7 == "PHash/s") &
                                     (data$comp_check_8 == "Date") &
                                     (data$comp_check_9 == "Give a one-sentence summary of the hashrate trajectory")
                                   |
                                     ((data$comp_check_4_NE == "Jan-21") & 
                                        (data$comp_check_5_NE == "Oct-21") & 
                                        (data$comp_check_6_NE == 100) & 
                                        (data$comp_check_7 == "PHash/s") &
                                        (data$comp_check_8 == "Date") &
                                        (data$comp_check_9 == "Give a one-sentence summary of the hashrate trajectory") ) ) ), 0, 1) 
    
    print(paste('percentage excluded, comprehension checks: ',
                table(data$comp_check)[2]/length(data$comp_check)))
    
    # Exclude those who failed either attention or comprehension checks
    data <- subset(data, (data$attention_check == 0) & (data$comp_check == 0))
    
    # Number of subjects after exclusions
    n_after_exclusions <- dim(data)[1] #40
    print(paste('percentage excluded, duplicate answers: ',
                (dim(sentence_dups)[1] + dim(word_dups)[1])/n_after_exclusions))
    print(paste('total percentage excluded, comprehension checks: ',
                (n_before_exclusions-n_after_exclusions)/n_before_exclusions))
    
    data$n_after_exclusions <- n_after_exclusions 
    
    return(data)
}


Preprocess <- function(data, n_plts, plt_names) {
    " 
    Since each plot is shown within-subjects, Qualtrics spits out data in wide format
    Let's convert it to long format, so that we have a row for every plot type
    Input: dataframe with number of rows = n_subjects, n_plots, plot_names 
    Output: dataframe with number of rows = n_subjects*n_plot_types (=)
    "
    
    # Define new data frame that we'll extract preprocessed data into
    
    # Define row and column names
    data_subset <- 35:88 #from which(colnames(data) == ... )
    last_cols <- 89:113  
    
    column_names <- c('plot_names', 'sentence_gen', 'word_gen', 'subject')
    
    df <- array(0, dim=c((nrow(data)*n_plts), length(column_names)))
    df <- as.data.frame(df, stringsAsFactors=FALSE)
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
    data <- relocate(data, "Condition", .after = "subject") #move the condition column closer to subject number 
    
    return(data)
}


##================================================================================================================
                                  ##FUNCTIONS FOR CLUSTERING ANALYSIS##
##================================================================================================================


ClusterLinesE4 <- function(question_summary, n_plts, plt_names, n_cluster) { 
    "
    Clean words, then plot clusters based on semantic distance between words 
    Input: word_summary OR sentence_summary, n_plots, plot_names, n_clusters (number of clusters desired)   
    Output: dendrogram with lifelines clustered based on word similarity & their icons  
    " 
    
    # Create list of all participant words categorized by lifelines 
    words_raw <- c()
    for(i in 1:n_plts) {
      words_raw[[i]] <- paste0(question_summary[[i]], collapse = " ")
    }
    words_raw <- unlist(words_raw)
    names(words_raw) <- plt_names
    
    # Create and clean corpus from word list  
    cleaned_words_corpus <- Corpus(VectorSource(words_raw)) %>% 
      tm_map(content_transformer(tolower)) %>% 
      tm_map(removePunctuation) %>%
      tm_map(removeNumbers) %>%
      tm_map(stripWhitespace) 
    
    # Convert corpus to term document matrix 
    words_tdm <- TermDocumentMatrix(cleaned_words_corpus)
    words_tdm <- t(words_tdm) #transpose (switch the rows and columns so that rows = docs and cols = terms) 
    
    # Plot frequent terms for each lifeline 
    words_matrix <- as.matrix(words_tdm)
    freq_words <- rowSums(words_matrix)
    
    # Hierarchical clustering 
    words_distance <- dist(scale(words_tdm)) #get the scaled distance between words (so that we can compare distances for every lifelines)
    words_hc <- hclust(words_distance, method = "ward.D") #cluster the lifelines based on how word similarity 
    plot(words_hc) #plot the clusters 
    rect.hclust(words_hc, k = n_cluster) #draw n number of boxes around distinct branches 
    
    # Get lifeline labels from the clusters 
    words_clusters <- cutree(words_hc, k = n_cluster) #which lifelines belong to which cluster? 
    words_table <- table(words_clusters) 
    order_table <- words_table[order(-as.numeric(names(words_table)))] #reorder the labels because the clusters are increasing in height, meaning the dendrogram labels go 3, 2, 1 (left to right)
    cluster_1 <- words_hc$labels[c(words_hc$order)][ 1 : order_table[2] ] 
    cluster_2 <- words_hc$labels[c(words_hc$order)][ (order_table[2] + 1) : (order_table[2] + order_table[1]) ] 
    cluster_3 <- words_hc$labels[c(words_hc$order)][ (order_table[2] + order_table[1] + 1) : (order_table[3] + order_table[2] + order_table[1])] 
    
    # Make the dendrogram a ggplot object using the ggdendro package (to later prettify) 
    dendro_plot <- ggdendrogram(words_hc) + 
      geom_rect(aes(xmin = 1, xmax = order_table[2], ymin = 49, ymax = 53), size = 1, alpha = 0, color = "firebrick3") + 
      geom_rect(aes(xmin = (order_table[2] + 1), xmax = (order_table[2] + order_table[1]), ymin = 49, ymax = 53), size = 1, alpha = 0, color = "firebrick3") + 
      geom_rect(aes(xmin = (order_table[2] + order_table[1] + 1), xmax = (order_table[3] + order_table[2] + order_table[1]), ymin = 49, ymax = 53), size = 1, alpha = 0, color = "firebrick3") + 
      theme_dendro() 
    
    # Return objects 
    dendro_objects <- list(dendro_plot, words_hc, cluster_1, cluster_2, cluster_3)
    return(dendro_objects)
}


PlotAxisLabels <- function(plt_names, my_equation, my_plot, plot_num) {
    "
    Calls  clustering function. 
    Make a plotter function that produces 'clean' (no labels) version of individual images 
    for the x-axis. Then, plot the images in order of dendogram results.
    Input: plot_names, my_equations, desired plot, desired icon arrangement 
    Output: Plot labels (the little lifeline icons)
    "
    
    # Make "clean" (no labels) version of individual images for x-axis
    Plotter_2 <- function(equation, x_range, y_range) {
      plot(equation, lwd = 30, xlim = c(start_age, end_age), ylim = c(0, end_y_axis), main = "", 
           xlab = "", ylab = "", axes = FALSE, col = "firebrick3")
      
      return(Plotter_2)
    }
    
    # Print the images that will comprise the x-axis
    for(i in 1:length(my_equation)) { #print individual plots
      png(file = paste0(plt_names[i],"_plot.png", ""))
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
                                          ##FUNCTIONS FOR SENTIMENT ANALYSIS##
##================================================================================================================


Get_sentence_sentiment <- function(e4_dat_long, condition, n_plts) {
    "
    Create funtion to get sentiment score means and standard errors for sentences by condition: eval or non-eval   
    Input: e4_data_long, condition ('evaluative' or 'non-evaluative'), n_plots 
    Output: average mean and standard error sentiment scores for sentences by condition, sorted by the same 27 lifelines  
            AND individual participant responses
    "
    
    # Clean words 
    condition_gen <- e4_dat_long$sentence_gen[e4_dat_long$Condition == condition] 
    condition_responses <- tolower(condition_gen) #make all words in each sentence lowercase 
    condition_clean <- gsub("[^[:alnum:][:space:]]", "", condition_responses) #sentence: keep only alphanumeric characters and spaces 
    
    # Sort words by plot type
    sentiment_sorted <- c()  
    for(i in 1:n_plts) {
      sentiment_sorted[[i]] <- condition_clean[seq(i, length(condition_clean), n_plts)]
    }
    
    # Get means and standard errors of words for every plot 
    sentiment_summary <- c() 
    for(i in 1:n_plts) {
      sentiment_summary[[i]] <- c(mean(sentiment_by(sentiment_sorted[[i]])$ave_sentiment), 
                                  std.error(sentiment_by(sentiment_sorted[[i]])$ave_sentiment)) 
    }
    
    # Get sentiment scores for ALL of individual subjects and plots  
    sentiment_all <- c() 
    for(i in 1:n_plts) {
      sentiment_all[[i]] <- sentiment_by(sentiment_sorted[[i]])$ave_sentiment 
    }
    
    sentiment_list <- list(sentiment_summary, sentiment_all, sentiment_sorted)
    
    return(sentiment_list) 
}


Get_word_sentiment <- function(e4_dat_long, condition, n_plts) {
    "
    Create funtion to get sentiment score means and standard errors for words by condition: eval or non-eval   
    Input: e4_data_long, condition ('evaluative' or 'non-evaluative'), n_plots 
    Output: average mean and standard error sentiment scores for words by condition, sorted by the same 27 lifelines
            AND individual participant responses
    " 
    
    # Clean words 
    condition_gen <- e4_dat_long$word_gen[e4_dat_long$Condition == condition] 
    condition_responses <- word(tolower(condition_gen), 1) #make all words lowercase, AND collect only the first word of a given sentence
    condition_clean <- gsub("[^a-z]", "", condition_responses) #get rid of numbers and special characters, leaving only letters a-z
    
    # Sort words by plot type
    sentiment_sorted <- c()  
    for(i in 1:n_plts) {
      sentiment_sorted[[i]] <- condition_clean[seq(i, length(condition_clean), n_plts)]
    }
    
    # Get means and standard errors of words for every plot 
    sentiment_summary <- c() 
    for(i in 1:n_plts) {
      sentiment_summary[[i]] <- c(mean(sentiment_by(sentiment_sorted[[i]])$ave_sentiment), 
                                  std.error(sentiment_by(sentiment_sorted[[i]])$ave_sentiment)) 
    }
    
    # Get sentiment scores for ALL of individual subjects and plots  
    sentiment_all <- c() 
    for(i in 1:n_plts) {
      sentiment_all[[i]] <- sentiment_by(sentiment_sorted[[i]])$ave_sentiment 
    }
    
    sentiment_list <- list(sentiment_summary, sentiment_all, sentiment_sorted)
    
    return(sentiment_list) 
}


Get_sentiment_scores <- function(e4_dat_long, plt_names, n_plts, e4_n_ss, n_evl, n_non_evl, e4_condition) {
    "
    Calls Get_sentence_sentiment() and Get_word_sentiment()
    Get sentiment scores means and standard errors for every condition and question type, sorted by plot type. 
    Input: e4_data_long, plot_names, n_plots, e4_n_after_exclusions, n_eval, n_non_eval, e4_conditions
    Output: data frame for average mean and standard error sentiment scores AND individual participant responses  
    " 
    
    # e4_dat_long <- e4_data_long 
    # plt_names <- plot_names
    # n_plts <- n_plots
    # e4_n_ss <- e4_n_after_exclusions
    # n_evl <- n_eval
    # n_non_evl <- n_non_eval
  
    
    # 1. Organize means and standard errors (for use in plotting)
    
    # Get sentiment scores means and standard errors for every condition and question type, sorted by plot type. 
    sentence_eval <- Get_sentence_sentiment(e4_dat_long, e4_condition[1], n_plts)[[1]]
    word_eval <- Get_word_sentiment(e4_dat_long, e4_condition[1], n_plts)[[1]]
    sentence_non_eval <- Get_sentence_sentiment(e4_dat_long, e4_condition[2], n_plts)[[1]]
    word_non_eval <- Get_word_sentiment(e4_dat_long, e4_condition[2], n_plts)[[1]]
    
    sentiment_df <- data.frame(plot_names = plt_names, 
                               sentence_eval_mean = unlist(sentence_eval)[c(TRUE, FALSE)], sentence_eval_se = unlist(sentence_eval)[c(FALSE, TRUE)], 
                               word_eval_mean = unlist(word_eval)[c(TRUE, FALSE)], word_eval_se = unlist(word_eval)[c(FALSE, TRUE)], 
                               sentence_non_eval_mean = unlist(sentence_non_eval)[c(TRUE, FALSE)], sentence_non_eval_se = unlist(sentence_non_eval)[c(FALSE, TRUE)], 
                               word_non_eval_mean = unlist(word_non_eval)[c(TRUE, FALSE)], word_non_eval_se = unlist(word_non_eval)[c(FALSE, TRUE)]) 
    
    # Make the data frame long 
    # Get mean sentiment scores and rename conditions 
    sentiment_mean <- sentiment_df %>% 
      gather(key = condition, #create separate entries for each question and condition type, e.g., "sentence_eval"  
             value = mean, sentence_eval_mean, word_eval_mean, sentence_non_eval_mean, word_non_eval_mean) %>% 
      mutate(condition = sub("_mean", "", condition)) #rename the conditions as fit 
    
    # Compile all standard error values
    sentiment_se <- gather(sentiment_df, key = condition,
                       value = se, sentence_eval_se, word_eval_se, sentence_non_eval_se, word_non_eval_se)
    
    # Bind the standard error column to the rest of the data frame
    sentiment_df_long <- cbind(dplyr::select(sentiment_mean, plot_names, condition, mean), se = sentiment_se$se)
    
    # ----
    
    # 2. Organize all sentiment scores for every subject, by plot type (for use in linear mixed effects regression)
    
    # Get sentiment scores  
    all_sentence_eval <- Get_sentence_sentiment(e4_dat_long, e4_condition[1], n_plts)[[2]]
    all_word_eval <- Get_word_sentiment(e4_dat_long, e4_condition[1], n_plts)[[2]]
    all_sentence_non_eval <- Get_sentence_sentiment(e4_dat_long, e4_condition[2], n_plts)[[2]]
    all_word_non_eval <- Get_word_sentiment(e4_dat_long, e4_condition[2], n_plts)[[2]]
    
    # Combine into one data frame 
    col_names <- c('plot_names', 'condition', 'sentiment_score', 'subject') 
    
    all_sentiment_df_long <- array(0, dim = c(nrow(e4_dat_long)*2, length(col_names))) #nrow(e4_dat_long)*2 = e4_n_after_exclusions*n_plots*2 questions (because we are combining the sentence and word scores into one column and assigning condition names) 
    all_sentiment_df_long <- as.data.frame(all_sentiment_df_long, stringsAsFactors=FALSE)
    colnames(all_sentiment_df_long) <- col_names
    
    # Assign plot names 
    all_sentiment_df_long[1 : (n_evl*n_plts*2), 1] <- rep(rep(plt_names, each = n_evl), times = 2) #eval rows 
    all_sentiment_df_long[((n_evl*n_plts*2) + 1) : nrow(all_sentiment_df_long), 1] <- rep(rep(plt_names, each = n_non_evl), times = 2) #non-eval rows 
    
    # Assign conditions 
    all_sentiment_df_long[2] <- rep(c("sentence_eval", "word_eval", "sentence_non_eval", "word_non_eval"), 
                               times = (c(n_evl, n_evl, n_non_evl, n_non_evl)*n_plts)) 
    
    # Assign values 
    all_sentiment_df_long[3] <- c(unlist(all_sentence_eval), unlist(all_word_eval), 
                             unlist(all_sentence_non_eval), unlist(all_word_non_eval)) #sentence score 

    # Assign subjects
    all_sentiment_df_long[1 : (n_evl*n_plts*2), 4] <- rep(1:n_evl, times = n_plts) #eval rows
    all_sentiment_df_long[((n_evl*n_plts*2) + 1) : nrow(all_sentiment_df_long), 4] <- rep((n_evl + 1) : e4_n_ss, times = n_plts) #non-eval rows
    
    # ----
    
    sentiment_data <- list(sentiment_df_long, all_sentiment_df_long)
    
    return(sentiment_data)
}


Get_sentiment_barplot <- function(e4_sentiment_data) {
    "
    Plot the sentiment bar graph for E4  
    Input: e4_sentiment_df 
    Output: the sentiment bar graph by ascending meaningfulness scores
    " 
    
    # Get sentiment data 
    sentiment_stats <- e4_sentiment_data[[1]] 
    sentiment_stats$plot_names <- as.factor(sentiment_stats$plot_names)
    
    # Reorder the sentiment scores 
    word_eval_data <- sentiment_stats[sentiment_stats$condition == "word_eval", ] #subset only the words in the evaluative condition 
    plot_order <- order(word_eval_data$mean) #get the order of this subset of data by ascending mean 
    names_order <- word_eval_data[plot_order, "plot_names"] #get the order of plot names 

    sentiment_stats$plot_names <- factor(sentiment_stats$plot_names, levels = names_order) #set the reordered data using plot names (for plotting)

    # Plot bar graph 
    ques_cond <- unique(sentiment_stats$condition) #get the condition and question type names 
    
    sentiment_bar <- ggplot(sentiment_stats, aes(x = plot_names, y = mean, fill = condition)) + 
      geom_bar(position="dodge", stat="identity") +
      geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width=.2,
                    position=position_dodge(.9)) +
      ggtitle(paste0("Mean Sentiment Scores")) + 
      xlab(paste0("Lifeline Plots")) + ylab("Mean Sentiment Score") +
      theme(
        plot.title = element_text(size=20, face="bold", hjust = 0.5),
        legend.title = element_text(size=15, face="bold"),
        legend.position = "top",
        legend.title.align = 0.5, 
        text = element_text(size = 15),          
        axis.title.y = element_text(size=15, face="bold"), 
        axis.title.x = element_text(size=15, face="bold"), 
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank() 
      ) + scale_fill_manual(
        name = "Condition Type", 
        breaks = ques_cond, 
        labels = str_to_title(gsub("_", " ", ques_cond)), 
        values = c("darkorange4", "darkorange", "turquoise4", "turquoise2"), #c("gray20", "gray40", "gray60", "gray80"), #c("#56B4E9", "#009E73", "firebrick3", "darkorange"),
        guide = guide_legend(title.position = "top")
      )
    
    # Return items 
    bar_plot_list <- list(sentiment_bar, names_order)
    return(bar_plot_list)
}


Get_word_clouds <- function(e4_dat_long, condition, n_plts, plt_names, list_clusters, label_clusters, n_condition) {
    "
    Create funtion to visualize word clouds for each condition, according to the three lifeline clusters    
    Input: e4_data_long, condition ('evaluative' or 'non-evaluative'), n_plots, 
           cluster_list_e4, cluster_labels_e4, n_eval or n_non_eval  
    Output: word clouds for participant words 
    " 
    
    # Clean words 
    condition_gen <- e4_dat_long$word_gen[e4_dat_long$Condition == condition] 
    condition_responses <- word(tolower(condition_gen), 1) #make all words lowercase, AND collect only the first word of a given sentence
    condition_clean <- gsub("[^a-z]", "", condition_responses) #get rid of numbers and special characters, leaving only letters a-z
    
    # Sort words by plot type
    sentiment_sorted <- c()  
    for(i in 1:n_plts) {
      sentiment_sorted[[plt_names[[i]]]] <- condition_clean[seq(i, length(condition_clean), n_plts)]
    }
    
    # Put into a list with cluster names 
    words_data <- vector("list", length(list_clusters))
    for(j in 1:length(list_clusters)) {  #for every cluster in list_clusters... 
      for(k in 1:length(sentiment_sorted)) {  #go through every plot in sentiment_sorted... 
        if(names(sentiment_sorted)[k] %in% list_clusters[[j]]) {  #check if each plot name exists in a given cluster from list_clusters...
          words_data[[j]] <- append(words_data[[j]], sentiment_sorted[[k]]) #and, if it does, compile it into a new list 
        }
      }
    }
    names(words_data) <- label_clusters 
    
    # Plot word clouds 
    for(i in 1:length(words_data)) {
      wc_data <- data.frame(table(words_data[[i]])) 
      gg_wc <- print(ggplot(arrange(wc_data, -Freq), aes(label = Var1, size = Freq, color = Freq)) +
                       geom_text_wordcloud(shape = "square", area_corr = TRUE, rm_outside = TRUE) + 
                       scale_size_area(max_size = 30) + 
                       theme_minimal() + 
                       scale_color_gradient(low = "mistyrose1", high = "firebrick3") + 
                       ggtitle(paste0("Pattern: ", label_clusters[i], ", ", condition)) + 
                       theme(plot.title = element_text(hjust = 0.5, vjust = -20, size = 20, 
                                                       face = "bold", color = "firebrick4")))
      
      # Save plots to file (and prettify file names)
      ggsave(paste0(tolower(label_clusters[i]), "_", gsub("-", "_", condition), "_WC.png", sep = ""), 
             plot = gg_wc, width = 30, height = 20, units = "cm")
    }
}


##================================================================================================================
                                          ##FUNCTIONS FOR DATA ANALYSIS##
##================================================================================================================


GetMainEffects <- function(e4_sentiment_data, n_plts, plt_names, 
                           n_cluster, list_clusters, label_clusters) {
    "
    Performs desired analysis: lmer (linear mixed-effects regression).  
    Input: e4_sentiment_df, n_plots, plot_names, n_clusters, cluster_list_e4, cluster_labels_e4 
    Output: Interaction results 
    " 
    
    # e4_sentiment_data <- e4_sentiment_df
    # n_plts <- n_plots
    # plt_names <- plot_names
    # n_cluster <- n_clusters #3 
    # list_clusters <- cluster_list_e4 
    # label_clusters <- cluster_labels_e4 
    
    # Get sentiment data 
    sentiment_stats_all <- e4_sentiment_data[[2]] 
    
    # Replace lifeline name with cluster label 
    for(j in 1:length(list_clusters)) {  #for every cluster in list_clusters... 
      for(k in 1:length(sentiment_stats_all$plot_names)) {  #go through every plot name in sentiment_stats_all... 
        if(sentiment_stats_all$plot_names[k] %in% list_clusters[[j]]) {  #check if each plot name exists in a given cluster from list_clusters...
          sentiment_stats_all$plot_names[k] <- j #and, if it does, replace it with that specific cluster  
        }
      }
    }
    
    # Divide condition column into "condition" and "question_type"
    sentiment_stats_all <- sentiment_stats_all
    sentiment_stats_all <- sentiment_stats_all %>% 
      mutate(question_type = sub("_eval|_non_eval", "", condition)) %>% #add a question type column
      mutate(condition = sub("sentence_|word_", "", condition)) #rename the conditions as fit (drop question type label) 
    
    sentiment_stats_all <- sentiment_stats_all[, c(1, 2, 5, 3, 4)] #reorder columns: "plot_names", "condition", "question_type", "sentiment_score", "subject" 
    sentiment_stats_all[c(1:3, 5)] <- lapply(sentiment_stats_all[c(1:3, 5)], function(x) as.numeric(factor(x))) #change values to numeric (for lmer)

    # ------ 
    
    # Run the analyses 
    
    # *** Interaction ***: condition*plot_names 
    sentiment_model <- lmer(sentiment_score ~ condition*plot_names + (1 | subject), data = sentiment_stats_all) 
    # summary(sentiment_model) 
    
    # Sentence scores, between conditions 
    sentiment_stats_sentence <- subset(sentiment_stats_all, question_type == 1) #create new data frame with just sentence sentiment scores  
    sentence_model <- lmer(sentiment_score ~ condition*plot_names + (1 | subject), data = sentiment_stats_sentence) 
    # summary(sentence_model) 
    
    # Word scores, between conditions 
    sentiment_stats_word <- subset(sentiment_stats_all, question_type == 2) #create new data frame with just word sentiment scores  
    word_model <- lmer(sentiment_score ~ condition*plot_names + (1 | subject), data = sentiment_stats_word) 
    # summary(word_model) 
    
    # Eval scores, between sentences and words 
    sentiment_stats_eval <- subset(sentiment_stats_all, condition == 1) #create new data frame with just evaluative scores  
    eval_model <- lmer(sentiment_score ~ question_type*plot_names + (1 | subject), data = sentiment_stats_eval) 
    # summary(eval_model) 
    
    # Non-eval scores, between sentences and words 
    sentiment_stats_non_eval <- subset(sentiment_stats_all, condition == 1) #create new data frame with just non-evaluative scores  
    non_eval_model <- lmer(sentiment_score ~ question_type*plot_names + (1 | subject), data = sentiment_stats_non_eval) 
    # summary(non_eval_model) 
    
    # Interaction: condition*plot_names*question_type 
    sentiment_all_model <- lmer(sentiment_score ~ condition*plot_names*question_type + (1 | subject), data = sentiment_stats_all) 
    # summary(sentiment_all_model) 
    
    #-----
    
    # Results 
    list_sentiment <- list(summary(sentiment_model), summary(sentence_model), summary(word_model), 
                           summary(eval_model), summary(non_eval_model), summary(sentiment_all_model)) 
    list_labels <- c("Sentiment Model", "Sentence-Only Model", "Word-Only Model", 
                     "Evaluative Model", "Non-Evaluative Model", "All Model")
    for(i in 1:length(list_sentiment)) {
      print(list_labels[i]) 
      print(list_sentiment[[i]])
      print("----------------------------------------------------------------------------------------")
    }
}


Plotting <- function(e4_sentiment_data, list_clusters, label_clusters) {
    "
    Graphs interaction plot. 
    Input: e4_sentiment_df, cluster_list_e4, cluster_labels_e4 
    Output: Average significance after multiple iterations 
    "
  
  # Get sentiment data frame 
  sentiment_stats_all <- e4_sentiment_data[[2]] 
  
  # Replace lifeline name with cluster label 
  for(j in 1:length(list_clusters)) {  #for every cluster in list_clusters... 
    for(k in 1:length(sentiment_stats_all$plot_names)) {  #go through every plot name in sentiment_stats_all... 
      if(sentiment_stats_all$plot_names[k] %in% list_clusters[[j]]) {  #check if each plot name exists in a given cluster from list_clusters...
        sentiment_stats_all$plot_names[k] <- label_clusters[j] #and, if it does, replace it with that specific cluster  
      }
    }
  }
  
  # Interaction Plot 
  sentiment_int <- interaction.plot(x.factor = sentiment_stats_all$plot_names, #x-axis variable
                                    trace.factor = sentiment_stats_all$condition, #variable for lines
                                    response = sentiment_stats_all$sentiment_score, #y-axis variable
                                    fun = mean, #metric to plot 
                                    ylab = "Sentiment Score",
                                    xlab = "Lifeline Clusters",
                                    col = c("firebrick3", "black", "orange", "darkgray"), #sentence_eval, word_eval, sentence_non_eval, word_non-eval 
                                    lty = 1, #line type
                                    lwd = 2, #line width
                                    trace.label = "Condition") 
  
  return(sentiment_int)
}


##================================================================================================================
                                                    ##MAIN##
##================================================================================================================


# Define global variables
# n_clusters <- 3 #define how many clusters you would like to divide the lifelines into based on the results of the dendrogram (see dendrogram_plot below)
# n_plots <- 27
# plot_names <- c("linear_rise", "linear_fall", 
#                 "linear_low", "linear_middle", "linear_high", 
#                 "exp_rise_convex", "exp_fall_convex", "exp_rise_concave", "exp_fall_concave", 
#                 "sin_fr_full", "sin_fr_partial", "sin_rf_full", "sin_rf_partial", 
#                 "sin_rfr_full", "sin_rfr_partial", 
#                 "sin_frf_full", "sin_frf_partial", 
#                 "sin_frfr", "sin_rfrf", 
#                 "logistic_rise", "logistic_fall", 
#                 "positive_change_full", "positive_change_partial", 
#                 "negative_change_full", "negative_change_partial", 
#                 "linear_rise_sharp_fall", "linear_rise_sharp_fall_exp_rise")


##================================================================================================================
# (1) Define Global Variables 
e4_conditions <- c("evaluative", "non-evaluative") 


##================================================================================================================
# (2) Read Data
e4_data <- read.csv("data/test_lifelines_e4_data.csv") 


# Process Data  
e4_data_clean <- PerformExclusions(e4_data) #num_rows = num_ss
e4_n_after_exclusions <- e4_data_clean$n_after_exclusions[1] 
e4_n_subjects_and_plots <- e4_n_after_exclusions*n_plots 
e4_data_long <- Preprocess(e4_data_clean, n_plots, plot_names) #num_rows = num_ss*num_plots [to see data without exclusions, replace e4_data_clean with data]


##================================================================================================================
# (3) Get Plot Clusters via Hierarchical Clustering 

# Inspired by ClusterLines() and PlotDendrogram() from power simulation script 
sentence_summary <- Get_sentence_sentiment(e4_data_long, e4_conditions[1], n_plots)[[3]]
word_summary <- Get_word_sentiment(e4_data_long, e4_conditions[1], n_plots)[[3]]

cluster_analysis_e4 <- ClusterLinesE4(word_summary, n_plots, plot_names, n_clusters) #returns a list of dendrogram plot and the lifeline names associated with it 
cluster_list_e4 <- c(cluster_analysis_e4[3], cluster_analysis_e4[4], cluster_analysis_e4[5]) 
cluster_labels_e4 <- c("Cluster_1", "Cluster_2", "Cluster_3") #ID the clusters by the similarity between their plots 


# Get Dendrogram Plot 
dendrogram_raw <- cluster_analysis_e4[[1]]
dendrogram_icons <- cluster_analysis_e4[[2]]$labels[c(cluster_analysis_e4[[2]]$order)]
dendrogram_plot_e4 <- PlotAxisLabels(plot_names, my_equations, dendrogram_raw, dendrogram_icons) 

pdf(file = "dendrogram_plot_e4.pdf", width = 12, height = 8) #save to file
  ggdraw(insert_xaxis_grob(dendrogram_raw, dendrogram_plot_e4, position = "bottom")) 
dev.off() 


##================================================================================================================
# (4) Analyze Sentiment 
n_eval <- length(Get_sentence_sentiment(e4_data_long, e4_conditions[1], n_plots)[[2]][[1]]) #number of subjects in the evaluative condition
n_non_eval <- length(Get_sentence_sentiment(e4_data_long, e4_conditions[2], n_plots)[[2]][[1]]) #number of subjects in the non-evaluative condition
e4_sentiment_df <- Get_sentiment_scores(e4_data_long, plot_names, n_plots, e4_n_after_exclusions, n_eval, n_non_eval, e4_conditions) 


# Plot Sentiment Bar Plot 
sentiment_bar_raw <- Get_sentiment_barplot(e4_sentiment_df)[[1]] 
sentiment_bar_icons <- Get_sentiment_barplot(e4_sentiment_df)[[2]] 
sentiment_bar_e4 <- PlotAxisLabels(plot_names, my_equations, sentiment_bar_raw, sentiment_bar_icons)

pdf(file = paste0("sentiment_barplot_e4.pdf"), width = 17, height = 8) 
  ggdraw(insert_xaxis_grob(sentiment_bar_raw, sentiment_bar_e4, position = "bottom"))
dev.off() 


# Plot Word Clouds (TAKES A FEW MINUTES: word clouds are being saved to file)
Get_word_clouds(e4_data_long, e4_conditions[1], n_plots, plot_names, cluster_list_e4, cluster_labels_e4, n_eval) #evaluative condition
Get_word_clouds(e4_data_long, e4_conditions[2], n_plots, plot_names, cluster_list_e4, cluster_labels_e4, n_non_eval) #non-evaluative condition


##================================================================================================================
# (5) Analyze Data 
GetMainEffects(e4_sentiment_df, n_plots, plot_names, n_clusters, cluster_list_e4, cluster_labels_e4) 


##================================================================================================================
# (6) Plotting 
pdf(file = "interaction_plot.pdf", width = 12, height = 8) #save to file
  Plotting(e4_sentiment_df, cluster_list_e4, cluster_labels_e4) 
dev.off() 


##================================================================================================================
# (7) Move Files 

# Move Data CSV 
dir.create("data") 
data_files <- list.files(pattern = "data.csv") 
file.move(data_files, "data", overwrite = TRUE) 


# Move Plots 
dir.create("analysis_plots") 
plot_files <- list.files(pattern = c("(.pdf|.png)"))
file.move(plot_files, "analysis_plots", overwrite = TRUE)


##================================================================================================================
                                                    ##END##
##================================================================================================================























##================================================================================================================
## CODE GRAVEYARD ## 

