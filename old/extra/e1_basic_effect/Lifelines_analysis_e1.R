# Analysis script for 'Lifelines'
# Experiment 1

## Clear workspace
rm(list = ls()) 

# Import libraries
if (!require(pacman)) {install.packages("pacman")}
pacman::p_load('ggplot2', #plot stuff
               'gtools', #sorts files and places them into numerical order
               'cowplot', #reads image files into R; add images as x-axis labels
               'magick', #image processing
               'ggridges', #image processing
               'png', #read PNG files
               'ggwordcloud', #make word clouds (using ggplot)
               'grid', #raster
               'gridGraphics', #make grids
               'gridExtra', #make grids
               'sentimentr', #sentiment analysis
               'lme4', #run mixed effects linear regression
               'lmerTest', #used in conjunction with lme4; get p-values
               'robustHD', #for the standardize function
               'corrplot', #for corrplot
               'psych', #for principal components analysis (PCA)
               'vegan', #for variance partitioning analysis
               'glmnet', #for ridge (L2) regression
               'MASS', #for ridge (L2) regression
               'filesstrings', #create and move files
               'car', #we use VIF function from this package
               'recipes', #for feature engineering
               'caret', #for automating tuning process
               'vip', #for vip() function
               'tidyr', #for gather(), which takes multiple columns and collapses them into key-value pairs
               'tidyverse' #used in conjunction with tidyr; contains dplyr, used for select(); load last because of conflict!
              )

if (!require(recipes)) {install.packages("recipes"); require(recipes)}
if (!require(caret)) {install.packages("caret"); require(caret)}

# Call in the Lifelines_v7.R script from the Lifelines folder for plot images
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #set working directoy to current directory
setwd("..") #go one directory up
source("Lifelines_Generate_Plots.R") #import plot-generating script
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #go back down one directory

##================================================================================================================
                                         ##FUNCTIONS FOR PREPROCESSING##
##================================================================================================================

PerformExclusions <- function(data) {
    # Perform first round of attention checks
    data$attention_check <- ifelse( ((data$att_check_1 == 'Paul') &
                                  (data$att_check_2 == 'Purple')), 0, 1 )
  
    # Perform second round of attention checks, if they failed the first
    data$attention_check <- ifelse( ( (is.na(data$att_check_3_1 == TRUE)) | 
                                      ((is.na(data$att_check_3_1 == FALSE)) &
                                         (data$att_check_4 == 0) &
                                         (data$att_check_3_3 > data$att_check_3_2) &
                                         (data$att_check_3_2 > data$att_check_3_1) &
                                         (data$att_check_3_2%%10 == 0) &
                                         (data$att_check_3_1 == 15) ) ), 0, 1)
  
    print(paste('percentage excluded, attention checks: ', 
              table(data$attention_check)[2]/length(data$attention_check)))
  
    # Perform comprehension checks
    data$attention_check2 <- ifelse( (data$comp_check_1 == 80 &
                                      data$comp_check_2 == 0 &
                                      data$comp_check_3 == 'They were highly unstressed early in life, then highly stressed later in life' & 
                                      data$comp_check_4 == 'Happiness' & 
                                      data$comp_check_5 == 'Age' &
                                      data$comp_check_6 == 'Meaningfulness'), 0, 1 )
  
    print(paste('percentage excluded, comprehension checks: ', 
              table(data$attention_check2)[2]/length(data$attention_check2)))
  
    # Exclude those who failed either attention or comprehension checks
    data <- subset(data, (data$attention_check == 0) & (data$attention_check2 == 0))
  
    # Number of subjects after exclusions
    n_after_exclusions <- dim(data)[1] #76
    
    return(data)
}


Preprocess <- function(data, n_plots) {
    # Since each plot is shown within-subjects, Qualtrics spits out data in wide format
    # Let's convert it to long format, so that we have a row for every plot type
    # So our number of rows change from n_subjects to n_subjects*n_plot_types
  
    # Define new data frame that we'll extract preprocessed data into
    
    # Define row and column names
    data_subset <- 28:108
    
    column_names <- c('plot_names', 'meaningfulness', 'personal_desirability', 'word', 'subject')
    
    df <- array(0, dim=c((nrow(data)*n_plots), length(column_names)))
    df <- as.data.frame(df, stringsAsFactors=FALSE)
    colnames(df) <- column_names
    
    # Turning wide format into long format, then inserting the answers into the 'df' dataframe
    final_data <- as.data.frame(t(data[data_subset])) #switch rows and columns in preparation for 'gather,' which collects info by columns
    long_data <- gather(final_data, key = "subject", value = "answers")["answers"] #gather the answers of the subjects into one long column 
    
    for(i in 1:dim(long_data)[2]) {
        df[1] <- PLOT_NAMES
        df[2] <- long_data[seq(1, nrow(long_data), 3),]
        df[3] <- long_data[seq(2, nrow(long_data), 3),]
        df[4] <- long_data[seq(3, nrow(long_data), 3),]
        df[5] <- rep(1:dim(data)[1], each=n_plots)
    }
    
    # Merge good data with first and last halves of the original data
    data <- cbind(data[rep(seq_len(nrow(data)), each = n_plots), 1:n_plots], df, data[rep(seq_len(nrow(data)), each = n_plots), 109:119])
    
    return(data)
}


ProcessForPlots <- function(data, n_plots) {
    # Create a new data frame to store the meaningfulness and PD scores by ascending meaningfulness scores
    
    # Get mean scores for all questions, then reshape data from wide to long format
    stats <- Get_stats(data, n_plots)
    data_plot <- data.frame(plot_names = PLOT_NAMES, 
                            meaning_score_avg = unlist(stats)[ c(TRUE,FALSE, FALSE, FALSE) ],
                            meaning_score_sd = unlist(stats)[ c(FALSE,TRUE, FALSE, FALSE) ],
                            pd_score_avg = unlist(stats)[ c(FALSE,FALSE, TRUE, FALSE) ],
                            pd_score_sd = unlist(stats)[ c(FALSE,FALSE, FALSE, TRUE) ])
    data_plot_sorted <- data_plot[order(data_plot$meaning_score_avg),] #order by meaningfulness
    data_plot_long <- gather(data_plot_sorted, key = question_type, #create separate entries for each question type, i.e., num_plots*num_questions 
                             value = score, meaning_score_avg, pd_score_avg) 
    
    # Compile all standard deviation values
    stan_dev <- gather(data_plot_sorted, key = question_type,
                       value = sd, meaning_score_sd, pd_score_sd)
    
    # Bind the SD column to the rest of the dataframe
    data_plot_long <- cbind(dplyr::select(data_plot_long, plot_names, question_type, score), sd = stan_dev$sd)
    
    data_plot_long$plot_names <- factor(data_plot_long$plot_names, levels = c("linear_low", "linear_fall", "exp_fall_convex", 
                                                                              "linear_rise_sharp_fall", "logistic_fall", "positive_change_partial", 
                                                                              "sin_rf_partial", "linear_middle", "positive_change_full",           
                                                                              "sin_rfrf", "sin_rf_full", "negative_change_full",            
                                                                              "sin_frf_full", "sin_frf_partial", "exp_fall_concave",               
                                                                              "sin_fr_full", "sin_rfr_partial", "sin_frfr",                        
                                                                              "sin_rfr_full", "exp_rise_convex", "linear_rise_sharp_fall_exp_rise", 
                                                                              "negative_change_partial", "sin_fr_partial", "logistic_rise",                   
                                                                              "linear_rise", "linear_high", "exp_rise_concave"))
    return(data_plot_long)
}


Get_stats <- function(data, n_plots) {
    # Find meaningfulness and personal desirability means and standard deviations for every plot
    # Every plot repeats every 27 times, since there are 27 plots title. 
    # Hence the 'seq' indeces for each calculation
    meaning_score <- as.numeric(data$meaningfulness)
    pd_score <- as.numeric(data$personal_desirability)
    
    equations <- list()
    for(i in 1:27) {
      equations[[i]] <- c(mean(meaning_score[seq(i, length(meaning_score), n_plots)]), sd(meaning_score[seq(i, length(meaning_score), n_plots)]),
                          mean(pd_score[seq(i, length(meaning_score), n_plots)]), sd(pd_score[seq(i, length(meaning_score), n_plots)]))
    }
                          
    return(equations)
}

##================================================================================================================
                                      ##FUNCTIONS FOR PLOTTING BAR CHARTS##
##================================================================================================================

MakeGroupedBarPlot <- function(data_plot_long) {
    # Plot the grouped bar graph
    grouped_bar_plot <- ggplot(data_plot_long, aes(x=plot_names, y=score, fill=question_type)) + 
                     geom_bar(position="dodge", stat="identity") +
                     geom_errorbar(aes(ymin=score-sd, ymax=score+sd), width=.2,
                     position=position_dodge(.9)) +
                     ggtitle("Meaningfulness and Personal Desirability Scores by Ascending Meaningfulness Scores") +
                     xlab("Lifeline Plots") + ylab("Mean Rating") +
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
                          ) +
                          scale_fill_manual(
                                             name = "Question Type", 
                                             breaks = c("meaning_score_avg", "pd_score_avg"), 
                                             labels = c("Average Meaningfulness Score", "Average Personal Desirability Score"),
                                             values = c("#56B4E9", "#009E73"),
                                             guide = guide_legend(title.position = "top")
                                             )
    return(grouped_bar_plot) 
}


MakeGroupedBarPlotImages <- function(LifelinesPlot) {
    # Make "clean" (no labels) version of individual images for x-axis
    Plotter_2 <- function(equation, x_range, y_range) {
      plot(equation, lwd = 30, xlim = c(start_age, end_age), ylim = c(0, end_y_axis), main = "", 
           xlab = "", ylab = "", axes = FALSE, col = "firebrick3")
      
      return(Plotter_2)
    }
    
    # Print the images that will comprise the x-axis
    for(i in 1:length(my_equations)) { #print individual plots
      png(file = paste0(PLOT_NAMES[i],"_plot.png", ""))
      sapply(my_equations[i], Plotter_2)
      dev.off()
    } 
  
    # Assemble images in the order of data_plot_long$plot_names[1:27]
    plot_images <- axis_canvas(LifelinesPlot, axis = 'x') + 
        draw_image("linear_low_plot.png", x = 0.5) +
        draw_image("linear_fall_plot.png", x = 1.5) +
        draw_image("exp_fall_convex_plot.png", x = 2.5) +
        draw_image("linear_rise_sharp_fall_plot.png", x = 3.5) +
        draw_image("logistic_fall_plot.png", x = 4.5) +
        draw_image("positive_change_partial_plot.png", x = 5.5) + 
        draw_image("sin_rf_partial_plot.png", x = 6.5) + 
        draw_image("linear_middle_plot.png", x = 7.5) + 
        draw_image("positive_change_full_plot.png", x = 8.5) + 
        draw_image("sin_rfrf_plot.png", x = 9.5) + 
        draw_image("sin_rf_full_plot.png", x = 10.5) + 
        draw_image("negative_change_full_plot.png", x = 11.5) + 
        draw_image("sin_frf_full_plot.png", x = 12.5) + 
        draw_image("sin_frf_partial_plot.png", x = 13.5) + 
        draw_image("exp_fall_concave_plot.png", x = 14.5) + 
        draw_image("sin_fr_full_plot.png", x = 15.5) + 
        draw_image("sin_rfr_partial_plot.png", x = 16.5) + 
        draw_image("sin_frfr_plot.png", x = 17.5) + 
        draw_image("sin_rfr_full_plot.png", x = 18.5) + 
        draw_image("exp_rise_convex_plot.png", x = 19.5) + 
        draw_image("linear_rise_sharp_fall_exp_rise_plot.png", x = 20.5) + 
        draw_image("negative_change_partial_plot.png", x = 21.5) + 
        draw_image("sin_fr_partial_plot.png", x = 22.5) + 
        draw_image("logistic_rise_plot.png", x = 23.5) + 
        draw_image("linear_rise_plot.png", x = 24.5) + 
        draw_image("linear_high_plot.png", x = 25.5) + 
        draw_image("exp_rise_concave_plot.png", x = 26.5)
    
    return(plot_images)
}

##================================================================================================================
                                    ##FUNCTIONS FOR PLOTTING WORD CLOUDS##
##================================================================================================================

Get_word_stats <- function(data, n_plots) {
  # Group words together into individual dataframes by plot type
    word_clean <- word(tolower(data$word), 1) #make all words lowercase, and collect only the first word of a given sentence
    word_gen <- gsub("[^a-z]", "", word_clean) #get rid of numbers and special characters, leaving only letters a-z
  
    equations <- list()
    for(i in 1:27) {
    equations[[i]] <- as.data.frame(table(word_gen[seq(i, length(word_gen), n_plots)]))
    }
  
    return(equations)
}


MakeWordClouds <- function(data, n_plots) {
    # Make word clouds
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
        
        equations <- list(LR,LF,LL,LM,LH,ERCV,EFCV,ERCC,EFCC,
                          SFR_FULL, SFR_PAR, SRF_FULL, SRF_PAR, SRFR_FULL, 
                          SRFR_PAR, SFRF_FULL, SFRF_PAR, SFRFR, 
                          SRFRF, LOG_RISE, LOG_FALL, POS_FULL, POS_PAR, 
                          NEG_FULL, NEG_PAR, LRSF, LRSFER)
        
        return(equations)
    }
  
    # Print word clouds
    my_word_clouds <- plot_word_clouds(data)
    print_word_clouds <- for(i in 1:length(my_word_clouds)) { #print individual plots
    
    # Ggsave takes a long time but produces high quality images necessary for arranging them
    ggsave(paste0(PLOT_NAMES[i],"_WC.png", sep = ""), 
           plot = my_word_clouds[[i]], width = 30, height = 20, units = "cm")
    } 
  
    return(print_word_clouds)
}


ArrangeWordClouds <- function(data) {
    # Arrange word clouds and plot labels into a grid using ggplot
    # (The simpler alternative is to use grid.arrange with lapply([PNGs], rasterGrob),
    # with [PNGs] being a set of predefined word clouds, 
    # but rasterGrob produces an error when used in such a list.)
  
    a <- rasterGrob(readPNG("linear_low_WC.png"), interpolate = TRUE)
    b <- rasterGrob(readPNG("linear_fall_WC.png"), interpolate = TRUE)
    c <- rasterGrob(readPNG("exp_fall_convex_WC.png"), interpolate = TRUE)
    d <- rasterGrob(readPNG("linear_rise_sharp_fall_WC.png"), interpolate = TRUE)
    e <- rasterGrob(readPNG("logistic_fall_WC.png"), interpolate = TRUE)
    f <- rasterGrob(readPNG("positive_change_partial_WC.png"), interpolate = TRUE) 
    g <- rasterGrob(readPNG("sin_rf_partial_WC.png"), interpolate = TRUE) 
    h <- rasterGrob(readPNG("linear_middle_WC.png"), interpolate = TRUE) 
    i <- rasterGrob(readPNG("positive_change_full_WC.png"), interpolate = TRUE) 
    
    j <- rasterGrob(readPNG("linear_low_plot.png"), interpolate = TRUE)
    k <- rasterGrob(readPNG("linear_fall_plot.png"), interpolate = TRUE)
    l <- rasterGrob(readPNG("exp_fall_convex_plot.png"), interpolate = TRUE)
    m <- rasterGrob(readPNG("linear_rise_sharp_fall_plot.png"), interpolate = TRUE)
    n <- rasterGrob(readPNG("logistic_fall_plot.png"), interpolate = TRUE)
    o <- rasterGrob(readPNG("positive_change_partial_plot.png"), interpolate = TRUE) 
    p <- rasterGrob(readPNG("sin_rf_partial_plot.png"), interpolate = TRUE) 
    q <- rasterGrob(readPNG("linear_middle_plot.png"), interpolate = TRUE) 
    r <- rasterGrob(readPNG("positive_change_full_plot.png"), interpolate = TRUE) 
    
    s <- rasterGrob(readPNG("sin_rfrf_WC.png"), interpolate = TRUE) 
    t <- rasterGrob(readPNG("sin_rf_full_WC.png"), interpolate = TRUE) 
    u <- rasterGrob(readPNG("negative_change_full_WC.png"), interpolate = TRUE) 
    v <- rasterGrob(readPNG("sin_frf_full_WC.png"), interpolate = TRUE) 
    w <- rasterGrob(readPNG("sin_frf_partial_WC.png"), interpolate = TRUE) 
    x <- rasterGrob(readPNG("exp_fall_concave_WC.png"), interpolate = TRUE) 
    y <- rasterGrob(readPNG("sin_fr_full_WC.png"), interpolate = TRUE) 
    z <- rasterGrob(readPNG("sin_rfr_partial_WC.png"), interpolate = TRUE) 
    aa <- rasterGrob(readPNG("sin_frfr_WC.png"), interpolate = TRUE) 
    
    bb <- rasterGrob(readPNG("sin_rfrf_plot.png"), interpolate = TRUE) 
    cc <- rasterGrob(readPNG("sin_rf_full_plot.png"), interpolate = TRUE) 
    dd <- rasterGrob(readPNG("negative_change_full_plot.png"), interpolate = TRUE) 
    ee <- rasterGrob(readPNG("sin_frf_full_plot.png"), interpolate = TRUE) 
    ff <- rasterGrob(readPNG("sin_frf_partial_plot.png"), interpolate = TRUE) 
    gg <- rasterGrob(readPNG("exp_fall_concave_plot.png"), interpolate = TRUE) 
    hh <- rasterGrob(readPNG("sin_fr_full_plot.png"), interpolate = TRUE) 
    ii <- rasterGrob(readPNG("sin_rfr_partial_plot.png"), interpolate = TRUE) 
    jj <- rasterGrob(readPNG("sin_frfr_plot.png"), interpolate = TRUE) 
    
    kk <- rasterGrob(readPNG("sin_rfr_full_WC.png"), interpolate = TRUE) 
    ll <- rasterGrob(readPNG("exp_rise_convex_WC.png"), interpolate = TRUE) 
    mm <- rasterGrob(readPNG("linear_rise_sharp_fall_exp_rise_WC.png"), interpolate = TRUE) 
    nn <- rasterGrob(readPNG("negative_change_partial_WC.png"), interpolate = TRUE) 
    oo <- rasterGrob(readPNG("sin_fr_partial_WC.png"), interpolate = TRUE) 
    pp <- rasterGrob(readPNG("logistic_rise_WC.png"), interpolate = TRUE) 
    qq <- rasterGrob(readPNG("linear_rise_WC.png"), interpolate = TRUE) 
    rr <- rasterGrob(readPNG("linear_high_WC.png"), interpolate = TRUE) 
    ss <- rasterGrob(readPNG("exp_rise_concave_WC.png"), interpolate = TRUE) 
    
    tt <- rasterGrob(readPNG("sin_rfr_full_plot.png"), interpolate = TRUE) 
    uu <- rasterGrob(readPNG("exp_rise_convex_plot.png"), interpolate = TRUE) 
    vv <- rasterGrob(readPNG("linear_rise_sharp_fall_exp_rise_plot.png"), interpolate = TRUE) 
    ww <- rasterGrob(readPNG("negative_change_partial_plot.png"), interpolate = TRUE) 
    xx <- rasterGrob(readPNG("sin_fr_partial_plot.png"), interpolate = TRUE) 
    yy <- rasterGrob(readPNG("logistic_rise_plot.png"), interpolate = TRUE) 
    zz <- rasterGrob(readPNG("linear_rise_plot.png"), interpolate = TRUE) 
    aaa <- rasterGrob(readPNG("linear_high_plot.png"), interpolate = TRUE)  
    bbb <- rasterGrob(readPNG("exp_rise_concave_plot.png"), interpolate = TRUE)
  
    #create a grid of word clouds and plot labels
    df <- data.frame() #empty dataframe
    wc_plot <- ggplot(df) + #empty ggplot
      geom_point() + xlim(0, 8) + ylim(-0.5, 9.5) + 
      theme_void() +
      annotation_custom(a, xmin=-0.5, xmax=0.5, ymin=8, ymax=10) + 
      annotation_custom(b, xmin=0.5, xmax=1.5, ymin=8, ymax=10) + 
      annotation_custom(c, xmin=1.5, xmax=2.5, ymin=8, ymax=10) + 
      annotation_custom(d, xmin=2.5, xmax=3.5, ymin=8, ymax=10) + 
      annotation_custom(e, xmin=3.5, xmax=4.5, ymin=8, ymax=10) + 
      annotation_custom(f, xmin=4.5, xmax=5.5, ymin=8, ymax=10) + 
      annotation_custom(g, xmin=5.5, xmax=6.5, ymin=8, ymax=10) + 
      annotation_custom(h, xmin=6.5, xmax=7.5, ymin=8, ymax=10) + 
      annotation_custom(i, xmin=7.5, xmax=8.5, ymin=8, ymax=10) +
      
      annotation_custom(j, xmin=-0.25, xmax=0.25, ymin=7, ymax=8) + 
      annotation_custom(k, xmin=0.75, xmax=1.25, ymin=7, ymax=8) + 
      annotation_custom(l, xmin=1.75, xmax=2.25, ymin=7, ymax=8) + 
      annotation_custom(m, xmin=2.75, xmax=3.25, ymin=7, ymax=8) + 
      annotation_custom(n, xmin=3.75, xmax=4.25, ymin=7, ymax=8) + 
      annotation_custom(o, xmin=4.75, xmax=5.25, ymin=7, ymax=8) + 
      annotation_custom(p, xmin=5.75, xmax=6.25, ymin=7, ymax=8) + 
      annotation_custom(q, xmin=6.75, xmax=7.25, ymin=7, ymax=8) + 
      annotation_custom(r, xmin=7.75, xmax=8.25, ymin=7, ymax=8) +        
      
      annotation_custom(s, xmin=-0.5, xmax=0.5, ymin=4, ymax=6) + 
      annotation_custom(t, xmin=0.5, xmax=1.5, ymin=4, ymax=6) + 
      annotation_custom(u, xmin=1.5, xmax=2.5, ymin=4, ymax=6) + 
      annotation_custom(v, xmin=2.5, xmax=3.5, ymin=4, ymax=6) + 
      annotation_custom(w, xmin=3.5, xmax=4.5, ymin=4, ymax=6) + 
      annotation_custom(x, xmin=4.5, xmax=5.5, ymin=4, ymax=6) + 
      annotation_custom(y, xmin=5.5, xmax=6.5, ymin=4, ymax=6) + 
      annotation_custom(z, xmin=6.5, xmax=7.5, ymin=4, ymax=6) + 
      annotation_custom(aa, xmin=7.5, xmax=8.5, ymin=4, ymax=6) + 
      
      annotation_custom(bb, xmin=-0.25, xmax=0.25, ymin=3, ymax=4) + 
      annotation_custom(cc, xmin=0.75, xmax=1.25, ymin=3, ymax=4) + 
      annotation_custom(dd, xmin=1.75, xmax=2.25, ymin=3, ymax=4) + 
      annotation_custom(ee, xmin=2.75, xmax=3.25, ymin=3, ymax=4) + 
      annotation_custom(ff, xmin=3.75, xmax=4.25, ymin=3, ymax=4) + 
      annotation_custom(gg, xmin=4.75, xmax=5.25, ymin=3, ymax=4) + 
      annotation_custom(hh, xmin=5.75, xmax=6.25, ymin=3, ymax=4) + 
      annotation_custom(ii, xmin=6.75, xmax=7.25, ymin=3, ymax=4) + 
      annotation_custom(jj, xmin=7.75, xmax=8.25, ymin=3, ymax=4) + 
      
      annotation_custom(kk, xmin=-0.5, xmax=0.5, ymin=0, ymax=2) + 
      annotation_custom(ll, xmin=0.5, xmax=1.5, ymin=0, ymax=2) + 
      annotation_custom(mm, xmin=1.5, xmax=2.5, ymin=0, ymax=2) + 
      annotation_custom(nn, xmin=2.5, xmax=3.5, ymin=0, ymax=2) + 
      annotation_custom(oo, xmin=3.5, xmax=4.5, ymin=0, ymax=2) + 
      annotation_custom(pp, xmin=4.5, xmax=5.5, ymin=0, ymax=2) + 
      annotation_custom(qq, xmin=5.5, xmax=6.5, ymin=0, ymax=2) + 
      annotation_custom(rr, xmin=6.5, xmax=7.5, ymin=0, ymax=2) + 
      annotation_custom(ss, xmin=7.5, xmax=8.5, ymin=0, ymax=2) + 
      
      annotation_custom(tt, xmin=-0.25, xmax=0.25, ymin=-1, ymax=0) + 
      annotation_custom(uu, xmin=0.75, xmax=1.25, ymin=-1, ymax=0) + 
      annotation_custom(vv, xmin=1.75, xmax=2.25, ymin=-1, ymax=0) + 
      annotation_custom(ww, xmin=2.75, xmax=3.25, ymin=-1, ymax=0) + 
      annotation_custom(xx, xmin=3.75, xmax=4.25, ymin=-1, ymax=0) + 
      annotation_custom(yy, xmin=4.75, xmax=5.25, ymin=-1, ymax=0) + 
      annotation_custom(zz, xmin=5.75, xmax=6.25, ymin=-1, ymax=0) + 
      annotation_custom(aaa, xmin=6.75, xmax=7.25, ymin=-1, ymax=0) + 
      annotation_custom(bbb, xmin=7.75, xmax=8.25, ymin=-1, ymax=0) 
  
    return(wc_plot)
}

##================================================================================================================
                                  ##FUNCTIONS FOR PLOTTING SENTIMENT BAR PLOT##
##================================================================================================================

Get_sentiment_stats <- function(data, n_plots) {
    # Find sentiment score means and standard deviations for every plot
    equations <- list()
    for(i in 1:27) {
    equations[[i]] <- c(mean(sentiment_by(as.character(Get_word_stats(data, n_plots)[[i]]$Var1))$ave_sentiment), 
                   sd(sentiment_by(as.character(Get_word_stats(data, n_plots)[[i]]$Var1))$ave_sentiment))
    }
  
    return(equations)
}


CreateSentimentDataframe <- function(data, n_plots) {
    # This dataframe will be important later on when we bind it to DAT for analysis
    sentiment_stats <- Get_sentiment_stats(data, n_plots)
    sentiment_df <- data.frame(plot_names = PLOT_NAMES, 
                             mean = unlist(sentiment_stats)[ c(TRUE, FALSE) ], 
                             sd = unlist(sentiment_stats)[ c(FALSE, TRUE) ])
    
    return(sentiment_df)
}


OrderSentimentDataframe <- function(data, n_plots) {
    # Create a new data frame to store the sentiment scores by ascending meaningfulness scores
    
    # Get the order of meaningfulness scores
    stats <- Get_stats(data, n_plots)
    data_plot <- data.frame(plot_names = PLOT_NAMES, 
                            meaning_score_avg = unlist(stats)[ c(TRUE,FALSE, FALSE, FALSE) ],
                            meaning_score_sd = unlist(stats)[ c(FALSE,TRUE, FALSE, FALSE) ],
                            pd_score_avg = unlist(stats)[ c(FALSE,FALSE, TRUE, FALSE) ],
                            pd_score_sd = unlist(stats)[ c(FALSE,FALSE, FALSE, TRUE) ])
  
    # Create sentiment data frame ordered by ascending meaningfulness scores
    sentiment_stats <- Get_sentiment_stats(data, n_plots)
    sentiment_df <- data.frame(plot_names = PLOT_NAMES, 
                             mean = unlist(sentiment_stats)[ c(TRUE, FALSE) ], 
                             sd = unlist(sentiment_stats)[ c(FALSE, TRUE) ])
    sentiment_df_sorted <- sentiment_df[order(data_plot$meaning_score_avg),]
    sentiment_df_sorted$plot_names <- factor(sentiment_df_sorted$plot_names, levels = c("linear_low", "linear_fall", "exp_fall_convex", 
                                                                                      "linear_rise_sharp_fall", "logistic_fall", "positive_change_partial", 
                                                                                      "sin_rf_partial", "linear_middle", "positive_change_full",           
                                                                                      "sin_rfrf", "sin_rf_full", "negative_change_full",            
                                                                                      "sin_frf_full", "sin_frf_partial", "exp_fall_concave",               
                                                                                      "sin_fr_full", "sin_rfr_partial", "sin_frfr",                        
                                                                                      "sin_rfr_full", "exp_rise_convex", "linear_rise_sharp_fall_exp_rise", 
                                                                                      "negative_change_partial", "sin_fr_partial", "logistic_rise",                   
                                                                                      "linear_rise", "linear_high", "exp_rise_concave"))
  
    return(sentiment_df_sorted)
}


MakeSentimentBarPlot <- function(data, n_plots) {
    # Plot the sentiment bar graph
    sentiment_df <- OrderSentimentDataframe(data, n_plots)
    sentiment_bar_plot <- ggplot(sentiment_df, aes(x=plot_names, y=mean)) + 
    geom_bar(position="dodge", stat="identity", fill = "darkorange") +
    geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2,
                  position=position_dodge(.9)) +
    ggtitle("Mean Sentiment Scores by Ascending Meaningfulness Scores") +
    xlab("Lifeline Plots") + ylab("Mean Sentiment Score") +
    theme(
      plot.title = element_text(size=20, face="bold", hjust = 0.5),
      text = element_text(size = 15),
      axis.title.y = element_text(size=15, face="bold"), 
      axis.title.x = element_text(size=15, face="bold"), 
      axis.text.x = element_blank(), 
      axis.ticks.x = element_blank()
    )
  
    return(sentiment_bar_plot) 
}

##================================================================================================================
                                            ##FUNCTIONS FOR WORD ANALYSIS##
##================================================================================================================

Get_word_analysis <- function(data, n_plots) {
    # Export the list of dataframes of words into a csv file
    equations <- list()
    for(i in 1:27) {
      equations[[i]] <- paste0(Get_word_stats(data, n_plots)[[i]]$Var1, collapse=", ")
    }
    
    return(equations)
}

##================================================================================================================
                                      ##FUNCTIONS FOR ANALYSIS##
##================================================================================================================

GetEffects <- function(data) {
    #This function takes as input a dataframe with rows = num_ss*num_plots*num_questions
  
    data$plot_type_n <- as.numeric(factor(data$plot_names)) #create numeric version of plot_names
    data$score_n <- as.numeric(data$score) #create numeric version of score (which are characters)
    data$question_type_n <- as.numeric(factor(data$question_type))
    data$subject_n <- as.numeric(factor(data$subject))
    
    print('Did answers vary depending on question and plot type?')
    effect_mod <- lm(data$score_n ~ data$question_type_n*data$plot_type_n + (1 | data$subject_n))
    print(summary(effect_mod))
    
    print('Which question type scored higher?')
    t_mod <- t.test(data$score_n~data$question_type, paired=TRUE)
    print(t_mod)
    print(paste('Means', tapply(data$score_n, data$question_type, mean)))
    
    print('Did sentiment scores vary depending on plot type?')
    effect_mod <- lm(data = data, sentiment_score ~ plot_type_n + (1 | subject_n))
    print(summary(effect_mod))

    print('Do the sentiment scores correlate with the meaningfulness and desirability ratings?')
    meaning_corr <- cor.test(data$sentiment_score[data$question_type=="meaningfulness"], 
             data$score_n[data$question_type=="meaningfulness"])
    print('sentiment vs. meaningfulness:')
    print(meaning_corr)
    
    pd_corr <- cor.test(data$sentiment_score[data$question_type=="personal_desirability"], 
             data$score_n[data$question_type=="personal_desirability"])
    print('sentiment vs. personal desirability:')
    pd_corr
    print(pd_corr)
}


CreateDataFeaturesDF <- function(data, dat_final, features_df, n_after_exclusions) {
  
    # Bind the three dataframes: data, sentiment score, and standardize(features), i.e., the standardized plot features.
    score_features_df <- cbind(data, sentiment_score = dat_final$sentiment_score[1:1107], as.data.frame(do.call("rbind", replicate(n_after_exclusions, standardize(features_df), simplify = FALSE))))
    score_features_df["meaningfulness"] <- as.data.frame(apply(score_features_df["meaningfulness"], 2, as.numeric))
    score_features_df["personal_desirability"] <- as.data.frame(apply(score_features_df["personal_desirability"], 2, as.numeric))
    score_features_df["subject"] <- as.data.frame(apply(score_features_df["subject"], 2, as.numeric))
    score_features_df["plot_names"] <- as.data.frame(as.numeric(factor(score_features_df$plot_names)))
    score_features_df["meaningfulness"] <- standardize(score_features_df["meaningfulness"])
    score_features_df["personal_desirability"] <- standardize(score_features_df["personal_desirability"])
    score_features_df["sentiment_score"] <- standardize(score_features_df["sentiment_score"])
    score_features_df["embeddings"] <- standardize(score_features_df["embeddings"])
    
    return(score_features_df)
    
}


Analyze <- function(score_features_df) {
    
    # 1. Meaningfulness
    meaning_features <- lmer(data = score_features_df, meaningfulness ~ 
                               embeddings + sentiment_score + max + min + number_peaks + number_valleys + number_extrema + integral + 
                               d1_avg_unweight + d1_avg_weight_prime + d1_avg_weight_asc + d1_avg_weight_des + d1_avg_weight_end + 
                               d2_avg_unweight + d2_avg_weight_prime + d2_avg_weight_asc + d2_avg_weight_des + d2_avg_weight_end + 
                               (1 | subject) + (1 | plot_names))
    
    #error: fixed-effect model matrix is rank deficient so dropping 3 columns
    
    print('meaningfulness vs. features:')
    print(summary(meaning_features, correlation=TRUE)) 
    print('check for multicollinearity')
    print(vif(meaning_features)) #check for multi-collinearity; we manually dropped 10 columns (d1_sum_unweight to d1_sum_weight_end and 
    #d1_sum_unweight to d2_sum_weight_end because they were perfectly correlated with their average counterparts), 
    #so now we only have number_extrema, d1_sum_weight_des, and d2_sum_weight_des left to deal with.
    print('check for singularity')
    print(isSingular(meaning_features)) #TRUE
    
    # 2. Personal Desirability
    pd_features <- lmer(data = score_features_df, personal_desirability ~ 
                          embeddings + sentiment_score + max + min + number_peaks + number_valleys + number_extrema + integral + 
                          d1_avg_unweight + d1_avg_weight_prime + d1_avg_weight_asc + d1_avg_weight_des + d1_avg_weight_end + 
                          d2_avg_unweight + d2_avg_weight_prime + d2_avg_weight_asc + d2_avg_weight_des + d2_avg_weight_end + 
                          (1 | subject) + (1 | plot_names))
    
    #error: fixed-effect model matrix is rank deficient so dropping 3 columns
    
    print('personal desirability vs. features:')
    print(summary(pd_features)) #same error as above
    print('check for multicollinearity')
    print(vif(pd_features)) #dropped columns: number_extrema, d1_sum_weight_des, and d2_sum_weight_des
    print('check for singularity')
    print(isSingular(pd_features)) #TRUE

}


VariancePartioning <- function(score_features_df) {
    #Variance partitioning analysis helps us examine the variation between explanatory variables.
    
    # Define the columns that we want: from embeddings to integral and the D1 & D2 predictors.
    # my_predictors <- score_features_df[, c(44:51, 57:61, 67:71)]
    # varpart(score_features_df$meaningfulness, X = ~my_predictors, data = score_features_df)
    # Error: Error in varpart(score_features_df$meaningfulness, X = ~my_predictors,    : 
    # needs two to four explanatory tables
    
    # Instead, I used the rough guidelines of the principle components from the PCA (below): 
    my_var <- varpart(data = score_features_df, score_features_df$meaningfulness, 
                      ~d1_avg_unweight + d1_avg_weight_prime + d1_avg_weight_asc + d1_avg_weight_des + d1_avg_weight_end, 
                      ~embeddings + d2_avg_unweight + d2_avg_weight_prime + d2_avg_weight_asc + d2_avg_weight_des + d2_avg_weight_end, 
                      ~number_peaks + number_valleys + number_extrema, 
                      ~sentiment_score + max + min + integral)
  
    # Error warnings: "collinearity detected"
    
    var_plot <- plot(sample)
    return(my_var)
    
}


MakePCAFunction <- function(score_features_df) {
    # In case linear mixed-effects analysis does not work, we decided to try out principal components analysis (PCA).
    
    # Define the columns that we want for the PCA: from embeddings to integral and the D1 & D2 predictors.
    cols <- c(44:51, 57:61, 67:71)
    
    # Fit the PCA
    my_PCA <- principal(score_features_df[,cols], 5, rotate = "promax")
    corrplot::corrplot(my_PCA$Structure, method = "circle")
    print('principal components by features')
    print(my_PCA$Structure)
    
    # Bind the RC1 through RC5 scores to the score_features_df data frame
    score_features_df <- cbind(score_features_df, my_PCA$scores)
    
    # 1. Meaningfulness
    meaning_features <- lmer(data = score_features_df, 
                               meaningfulness ~ RC1 + RC2 + RC3 + RC4 + RC5 + (1 | subject) + (1 | plot_names))
  
    print('meaningfulness vs. features:')
    print(summary(meaning_features, correlation = TRUE)) 
    
    # 2. Personal Desirability
    pd_features <- lmer(data = score_features_df, 
                      personal_desirability ~ RC1 + RC2 + RC3 + RC4 + RC5 + (1 | subject) + (1 | plot_names))
  
    print('personal desirability vs. features:')
    print(summary(pd_features, correlation = TRUE))
  
}


AnalyzeRidgeRegression <- function(score_features_df) {
    
    my_predictors <- data.matrix(score_features_df[, c(44:51, 57:61, 67:71)]) 
    lambdas <- 10^seq(3, -2, by = -.1)
    
    # 1. Meaningfulness
    meaning_features <- glmnet(my_predictors, score_features_df$meaningfulness, 
                               alpha = 0, lambda = lambdas) 
    find_lambda_meaning <- cv.glmnet(my_predictors, score_features_df$meaningfulness, 
                                    alpha = 0, lambda = lambdas)
    opt_lambda_meaning <- find_lambda_meaning$lambda.min
    meaning_features <- lm.ridge(data = score_features_df, meaningfulness ~ 
                                   sentiment_score + max + min + number_peaks + number_valleys + number_extrema + integral + 
                                   d1_avg_unweight + d1_avg_weight_prime + d1_avg_weight_asc + d1_avg_weight_des + d1_avg_weight_end + 
                                   d2_avg_unweight + d2_avg_weight_prime + d2_avg_weight_asc + d2_avg_weight_des + d2_avg_weight_end, 
                                   lambda = opt_lambda_meaning) 
    
    meaning_features_rounded <- round(meaning_features[["coef"]], 3)
    print('meaningfulness vs. features:')
    print(sort(meaning_features_rounded, decreasing = TRUE))
    
    # 2. Personal Desirability
    pd_features <- glmnet(my_predictors, score_features_df$personal_desirability, 
                          alpha = 0, lambda = lambdas) 
    find_lambda_pd <- cv.glmnet(my_predictors, score_features_df$personal_desirability, 
                                    alpha = 0, lambda = lambdas)
    opt_lambda_pd <- find_lambda_pd$lambda.min
    pd_features <- lm.ridge(data = score_features_df, personal_desirability ~ 
                              sentiment_score + max + min + number_peaks + number_valleys + number_extrema + integral + 
                              d1_avg_unweight + d1_avg_weight_prime + d1_avg_weight_asc + d1_avg_weight_des + d1_avg_weight_end + 
                              d2_avg_unweight + d2_avg_weight_prime + d2_avg_weight_asc + d2_avg_weight_des + d2_avg_weight_end, 
                              lambda = opt_lambda_pd) 
    
    pd_features_rounded <- round(pd_features[["coef"]], 3)
    print('personal desirability vs. features:')
    print(sort(pd_features_rounded, decreasing = TRUE))
  
}


CrossValidationAnalysis <- function(score_features_df) {
    
    my_predictors <- data.matrix(score_features_df[, c(44:51, 57:61, 67:71)]) 
    lambdas <- 10^seq(3, -2, by = -.1)

    # 1. Meaningfulness
    # Cross-validation
    meaning_features <- train(data = score_features_df,
                              meaningfulness ~ 
                                embeddings + sentiment_score + max + min + number_peaks + number_valleys + number_extrema + integral + 
                                d1_avg_unweight + d1_avg_weight_prime + d1_avg_weight_asc + d1_avg_weight_des + d1_avg_weight_end + 
                                d2_avg_unweight + d2_avg_weight_prime + d2_avg_weight_asc + d2_avg_weight_des + d2_avg_weight_end, 
                              method = "glmnet",
                              preProc = c("zv", "center", "scale"),
                              trControl = trainControl(method = "cv", number = 10),
                              tuneLength = 10)
    
    plot(meaning_features, xvar = "lambda")
    print(paste('meaningfulness: best tuning parameters', meaning_features$bestTune))
    opt_alpha_meaning <- meaning_features$bestTune$alpha 
    
    # Elastic regression
    meaning_features_elastic <- glmnet(my_predictors, score_features_df$meaningfulness, 
                                       alpha = opt_alpha_meaning, lambda = lambdas)
    find_lambda_meaning <- cv.glmnet(my_predictors, score_features_df$meaningfulness,
                                     alpha = opt_alpha_meaning, lambda = lambdas)
    opt_lambda_meaning <- find_lambda_meaning$lambda.min
    
    plot(meaning_features_elastic, xvar = "lambda")
    abline(v = log(opt_lambda_meaning), col = "red", lty = "dashed")
    vip(meaning_features_elastic, num_features = 20, geom = "point")
  
    # 2. Personal Desirability
    # Cross-Validation
    pd_features <- train(data = score_features_df, 
                         personal_desirability ~ 
                           embeddings + sentiment_score + max + min + number_peaks + number_valleys + number_extrema + integral + 
                           d1_avg_unweight + d1_avg_weight_prime + d1_avg_weight_asc + d1_avg_weight_des + d1_avg_weight_end + 
                           d2_avg_unweight + d2_avg_weight_prime + d2_avg_weight_asc + d2_avg_weight_des + d2_avg_weight_end, 
                         method = "glmnet",
                         preProc = c("zv", "center", "scale"),
                         trControl = trainControl(method = "cv", number = 10),
                         tuneLength = 10)
    
    plot(pd_features, xvar = "lambda")
    print(paste('personal desirability: best tuning parameters', pd_features$bestTune))
    opt_alpha_pd <- pd_features$bestTune$alpha 
    
    # Elastic regression
    pd_features_elastic <- glmnet(my_predictors, score_features_df$personal_desirability, 
                                       alpha = opt_alpha_pd, lambda = lambdas)
    find_lambda_pd <- cv.glmnet(my_predictors, score_features_df$personal_desirability,
                                     alpha = opt_alpha_pd, lambda = lambdas)
    opt_lambda_pd <- find_lambda_pd$lambda.min
    
    plot(pd_features_elastic, xvar = "lambda")
    abline(v = log(opt_lambda_pd), col = "red", lty = "dashed")
    vip(pd_features_elastic, num_features = 20, geom = "point")
    
}


SanityCheck <- function(data, my_embeddings) {
  
    # Regress meaningfulness on solely on embeddings
    sanity_check_df <- cbind(meaningfulness = data$meaningfulness, my_embeddings[2:513])
    sanity_check <- lm(meaningfulness ~ ., data = sanity_check_df)
    summary(sanity_check)
    # Error: 486 not defined because of singularities
    # I checked for multicollinearity with cor(MY_EMBEDDINGS[2:513]) but did not find any perfect correlations 
    # (except for of course the correlation of a given variable with itself, which had a coefficient of 1).
    
    return(summary(sanity_check))
    
}

##================================================================================================================
                                                  ##MAIN##
##================================================================================================================

## ===================================== Define Global Variables =================================================

N_PLOTS <- 27
N_AFTER_EXCLUSIONS <- 41
MEANING_SCORES <- 1:27
PD_SCORES <- 28:54
PLOT_NAMES <- c("linear_rise", "linear_fall", 
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

## ============================ (1) Read Data and Create Folder for Saving Files =================================

DATA <- read.csv('data/Lifelines_e1_data.csv') 
dir.create("analysis_plots")

## ================================= (2) Perform Exclusions and Process Data =====================================

DATA_CLEAN <- PerformExclusions(DATA) #num_rows = num_ss

DATA_LONG <- Preprocess(DATA_CLEAN, N_PLOTS) #num_rows = num_ss*num_plots
MY_EMBEDDINGS <- read.csv("data/embeddings.csv", header = TRUE) #read in semantic embeddings
EMBEDDINGS_AVG <- data.frame(embeddings = rowMeans(MY_EMBEDDINGS[2:28])) #create a dataframe
EMBEDDINGS_AVG <- do.call("rbind", replicate(N_AFTER_EXCLUSIONS, EMBEDDINGS_AVG, simplify = FALSE)) #replicate rows
DATA_LONG <- cbind(DATA_LONG, EMBEDDINGS_AVG)
DATA_PLOT_LONG <- ProcessForPlots(DATA_LONG, N_PLOTS) #num_rows = num_plots*num_questions
dim(DATA_PLOT_LONG)

## ========================================== (3) Plot Data and Save ==================================================

### (i) BAR PLOT
GROUPED_BAR_PLOT <- MakeGroupedBarPlot(DATA_PLOT_LONG) 
PLOT_IMAGES <- MakeGroupedBarPlotImages(GROUPED_BAR_PLOT)

pdf(file="lifelines_bar_plot.pdf", width = 17,height = 8)
ggdraw(insert_xaxis_grob(GROUPED_BAR_PLOT, PLOT_IMAGES, position = "bottom"))
dev.off()  

### (ii) WORD CLOUDS (will take a while to load)
WORD_CLOUDS <- MakeWordClouds(DATA_LONG, N_PLOTS)
ARRANGE_WORD_CLOUDS <- ArrangeWordClouds()

pdf(file="lifelines_word_clouds.pdf", width = 18, height = 8)
ARRANGE_WORD_CLOUDS
dev.off()

### (iii) SENTIMENT ANALYSIS PLOTS
SENTIMENT_DF <- OrderSentimentDataframe(DATA_LONG, N_PLOTS)
SENTIMENT_BAR_PLOT <- MakeSentimentBarPlot(DATA_LONG, N_PLOTS) 
SENTIMENT_PLOT_IMAGES <- MakeGroupedBarPlotImages(SENTIMENT_BAR_PLOT)

pdf(file="lifelines_sentiment_plot.pdf", width = 17, height = 8)
ggdraw(insert_xaxis_grob(SENTIMENT_BAR_PLOT, SENTIMENT_PLOT_IMAGES, position = "bottom"))
dev.off()  

## =================================== (4) Create CSV for Semantic Analysis ========================================

ANALYZE_WORDS <- Get_word_analysis(DATA_LONG, N_PLOTS)
WORDS_DF <- as.data.frame(matrix(unlist(ANALYZE_WORDS), ncol = length(unlist(ANALYZE_WORDS[1]))))
ANALYZE_WORDS_DF <- cbind(plot_names = PLOT_NAMES, words = WORDS_DF$V1)

write.csv(ANALYZE_WORDS_DF, "word_analysis.csv", row.names = FALSE)

## ============================================== (5) Analysis =====================================================

### (i) GET EFFECTS
DAT <- gather(DATA_LONG, key = question_type, value = score, meaningfulness, personal_desirability)
DAT <- dplyr::select(DAT, subject, plot_names, question_type, score) #rows = num_ss*num_plots*num_questions
SENTIMENT_SCORES <- CreateSentimentDataframe(DATA_LONG, N_PLOTS)
DAT_FINAL <- cbind(DAT, sentiment_score = SENTIMENT_SCORES[rep(seq_len(nrow(SENTIMENT_SCORES)), N_AFTER_EXCLUSIONS), ]$mean)
GetEffects(DAT_FINAL)

### (ii) CREATE A DATAFRAME OF FEATURES AND SUBJECT SCORES
SCORE_FEATURES_DF <- CreateDataFeaturesDF(DATA_LONG, DAT_FINAL, features, N_AFTER_EXCLUSIONS)

### (iii) LINEAR MIXED-EFFECTS REGRESSION
Analyze(SCORE_FEATURES_DF)

### (iv) PRINCIPAL COMPONENTS ANALYSIS
MakePCAFunction(SCORE_FEATURES_DF)

### (v) VARIANCE PARTITIONING AND RIDGE REGRESSION
VariancePartioning(SCORE_FEATURES_DF)
AnalyzeRidgeRegression(SCORE_FEATURES_DF)

### (vi) CROSS-VALIDATION
CrossValidationAnalysis(SCORE_FEATURES_DF)

### (vii) MEANINGFULNESS AND SEMANTIC EMBEDDINGS REGRESSION
SanityCheck(DATA_LONG, MY_EMBEDDINGS) 
# See error: 486 not defined because of singularities; checked for perfect correlation but did not find any

## =========================================== (6) Move Files ================================================================================================================

PLOT_FILES <- list.files(pattern = c("(.pdf|.png|word_analysis.csv|embeddings.csv|correlations.csv)"))
file.move(PLOT_FILES, "analysis_plots", overwrite = TRUE)

##================================================================================================================
                                                      ##END##
##================================================================================================================






