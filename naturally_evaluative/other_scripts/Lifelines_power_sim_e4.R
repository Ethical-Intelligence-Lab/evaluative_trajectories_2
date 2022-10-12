## Lifelines E4 Analyses ## 


# Set working directory to current file location 
# remove(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


# Import packages 
if (!require(pacman)) {install.packages("pacman")}
pacman::p_load(
  'broom.mixed', #used in power simulation; provides a tidy() function for lmer() models 
  'simr', #power simulation package for estimating sample size 
  'tm', #text mining 
  # 'reshape2' #reshape distance matrix into a more accessible data frame 
  'vegan', #reorder dendrogram in hierarchical clustering 
  'dendextend', #used in "cutting" dendrogram into observable clusters 
  'ggdendro' #draw (and prettify) dendrograms in ggplot 
)


# Source Lifelines_analysis_e1b.R script from the Lifelines/e1b_basic_effect folder (for sentiment data) 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #set working directory to current directory
setwd("..") #go one directory up
source("e1b_basic_effect/Lifelines_analysis_e1b.R") #import e1b analysis script 
setwd("../e4_evaluative_journeys") #go back to current directory


# ============================================= DEFINE FUNCTIONS ============================================= #

# Clustering in R: Cluster the lifelines based on how similar participant words used to describe them are 
ClusterLines <- function(dat_long, n_plts, plt_names, n_cluster) { 
    "
    Clean words, then plot clusters based on semantic distance between words 
    Input: data_long, n_plots, plot_names, n_clusters (number of clusters desired)   
    Output: dendrogram with lifelines clustered based on word similarity & their icons  
    " 
    
    # Create list of all participant words categorized by lifelines 
    words_raw <- c()
    for(i in 1:n_plts) {
      words_raw[[i]] <- paste0(wordStem(Get_word_stats(dat_long, n_plts)[[i]]$Var1), collapse = " ")
    }
    words_raw <- unlist(words_raw)
    names(words_raw) <- plt_names
    
    # Create and clean corpus from word list  
    cleaned_words_corpus <- Corpus(VectorSource(words_raw)) %>% 
      tm_map(content_transformer(tolower)) %>% 
      tm_map(removePunctuation) %>%
      tm_map(removeNumbers) %>%
      tm_map(stripWhitespace) 
    
    # Word cloud visualization 
    # set.seed(1)
    # wordcloud(cleaned_words_corpus, colors = brewer.pal(8, "Dark2"), min.freq = 3, random.order = FALSE)
    
    # Convert corpus to term document matrix 
    words_tdm <- TermDocumentMatrix(cleaned_words_corpus)
    words_tdm <- t(words_tdm) #transpose (switch the rows and columns so that rows = docs and cols = terms) 
    
    # Plot frequent terms for each lifeline 
    words_matrix <- as.matrix(words_tdm)
    freq_words <- rowSums(words_matrix)
    # freq_words <- subset(freq_words, freq_words >= 50)
    # barplot(freq_words) 
    
    # Hierarchical clustering 
    words_distance <- dist(scale(words_tdm)) #get the scaled distance between words (so that we can compare distances for every lifelines)
    words_hc <- hclust(words_distance, method = "ward.D") #cluster the lifelines based on how word similarity 
    plot(words_hc) #plot the clusters 
    rect.hclust(words_hc, k = n_cluster) #draw n number of boxes around distinct branches 
    # cluster_labels <- c("Fluctuating", "Decreasing", "Increasing") #name the clusters by their similarity 
    # distance_matrix <- melt(as.matrix(words_distance), varnames = c("row", "col"))
    # View(distance_matrix) 
    
    # Get lifeline labels from the clusters 
    words_clusters <- cutree(words_hc, k = n_cluster) #which lifelines belong to which cluster? 
    words_table <- table(words_clusters) 
    order_table <- words_table[order(-as.numeric(names(words_table)))] #reorder the labels because the clusters are increasing in height, meaning the dendrogram labels go 3, 2, 1 (left to right)
    cluster_3 <- words_hc$labels[c(words_hc$order)][ 1 : order_table[1] ] #1:6
    cluster_2 <- words_hc$labels[c(words_hc$order)][ (order_table[1] + 1) : (order_table[2] + order_table[1]) ] #7:17
    cluster_1 <- words_hc$labels[c(words_hc$order)][ (order_table[2] + order_table[1] + 1) : (order_table[3] + order_table[2] + order_table[1])] #18:27
    
    # Make the dendrogram a ggplot object using the ggdendro package (to later prettify) 
    dendro_plot <- ggdendrogram(words_hc) + 
      geom_rect(aes(xmin = 1, xmax = 6, ymin = 49, ymax = 53), size = 1, alpha = 0, color = "firebrick3") + 
      geom_rect(aes(xmin = 7, xmax = 17, ymin = 49, ymax = 53), size = 1, alpha = 0, color = "firebrick3") + 
      geom_rect(aes(xmin = 18, xmax = 27, ymin = 49, ymax = 53), size = 1, alpha = 0, color = "firebrick3") + 
      theme_dendro() 
    
    # Return objects 
    dendro_objects <- list(dendro_plot, words_hc, cluster_1, cluster_2, cluster_3)
    return(dendro_objects)
}


PlotDendrogram <- function(dat_long, n_plts, plt_names, n_cluster, my_equation) {
    "
    Calls  clustering function. 
    Make a plotter function that produces 'clean' (no labels) version of individual images 
    for the x-axis. Then, plot the images in order of dendogram results.
    Input: data_long, n_plots, plot_names, n_clusters, my_equations 
    Output: Plot labels 
    "
    
    # Call clustering function 
    dendro_plot <- ClusterLines(dat_long, n_plts, plt_names, n_cluster)
    
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
    plot_num <- dendro_plot[[2]]$labels[c(dendro_plot[[2]]$order)]
    plot_icons <- axis_canvas(dendro_plot[[1]], axis = 'x') + 
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


# From Experiment 1 (Lifelines), we learn that people use evaluative words like "sad," "happy," "good," and "bad" 
# to summarize the lifelines. 
# To see means and standard deviations, organized from the least to most meaningful lifeline, view: sentiment_df_sorted 


### Evaluative Condition 
Get_sentiment_eval <- function(dat_long, n_plts, sample_size, plt_names) {
    "
    Get sentiment scores for the evaluative condition and generate sample data 
    Input: data_long, n_plots, sample size to try, plot_names 
    Output: eval_long (data frame of sample sentiment scores from each subject, for every plot) 
    " 
    
    # Clean words 
    word_clean <- word(tolower(dat_long$word), 1) #make all words lowercase, and collect only the first word of a given sentence
    word_gen <- gsub("[^a-z]", "", word_clean) #get rid of numbers and special characters, leaving only letters a-z
    
    # Organize words by plot 
    equations <- c()
    for(i in 1:n_plts) {
      equations[[i]] <- word_gen[seq(i, length(word_gen), n_plts)]
    }
    
    # Get sentiment score means and standard deviations for every plot
    eval_summary <- c()
    for(i in 1:n_plts) {
      eval_summary[[i]] <- c(mean(sentiment_by(equations[[i]])$ave_sentiment),
                             sd(sentiment_by(equations[[i]])$ave_sentiment))
    }
    
    # Put into data frame for ease of subsetting 
    summary_df <- data.frame(plot_names = plt_names, 
                          mean = unlist(eval_summary)[ c(TRUE, FALSE) ], 
                          sd = unlist(eval_summary)[ c(FALSE, TRUE) ])
    
    # Generate data based on the means and standard deviations of each plot type 
    eval_dat <- c() 
    for(i in 1:n_plts) {
      eval_dat[[i]] <- rnorm(sample_size, mean = summary_df$mean[i], sd = summary_df$sd[i]) 
    }
    
    # Create data frame and assign participant IDs and condition 
    eval_df <- data.frame(eval_dat)
    colnames(eval_df) <- plt_names #column names 
    
    eval_long <- pivot_longer(eval_df, cols = everything()) #long version
    eval_long$condition <- "eval" #assign condition name 
    eval_long$subject <- rep(1:sample_size, each = n_plts) #assign participant IDs 
    
    return(eval_long)
}


### Non-evaluative Condition 
# Prediction: sentiment scores will hover around 0 because people will describe the plots using literal words (e.g., "flat," "increasing"), 
# not evaluative ones ("good," "bad," etc.) 


Get_sentiment_non_eval <- function(sample_size, n_plts, plt_names) {
    "
    Get sentiment scores for the nonevaluative condition; equivalent to generating sample data   
    Input: sample size to try, n_plots, plot_names 
    Output: non_eval_sample (data frame of sentiment scores from each subject, for every plot)
    " 
    
    # Generate a possible set of participants (e.g., 27 plots, 124 participants = 5400 ratings) with a mean of 0 and a standard deviation of 0.5    
    # set.seed(123)
    non_eval_dat <- rnorm(n_plts*sample_size, mean = 0, sd = 0.5) 
    
    # Check if the means for each plot is around zero (yes) 
    # for(i in 1:n_plts) {
    #   print(i)
    #   mean_non_eval <- mean(non_eval_dat[(sample_size*(i-1)):(sample_size*i)])
    #   print(mean_non_eval)
    # }
    
    # Organize sentiment scores from participants for each plot 
    non_eval_sample <- c()
    for(i in 1:n_plts) {
      non_eval_sample[[i]] <- non_eval_dat[ ((sample_size*(i-1))+1) : (sample_size*i) ] 
    }
    
    # Create data frame 
    non_eval_df <- data.frame(non_eval_sample) 
    colnames(non_eval_df) <- plt_names #column names 
    non_eval_long <- pivot_longer(non_eval_df, cols = everything()) #long version 
    
    cond_2 <- "non_eval" #assign condition name 
    non_eval_long$condition <- cond_2 #add to data frame 
    subject_2 <- rep((sample_size + 1):(sample_size*2), each = n_plts) #assign participant IDs 
    non_eval_long$subject <- subject_2 #add to data frame
    
    return(non_eval_long) 
}


Get_power <- function(num_iterations, dat_long, n_plts, sample_size, plt_names, n_cluster) {
    "
    Calls clustering function. 
    Generates sample data and performs desired analysis (e.g., lmer); 
      calls Get_sentiment_eval() and Get_sentiment_non_eval(), then combines it into one dataset 
    Input: number of iterations, data_long, n_plots, sample size to try, plot_names, n_clusters 
    Output: Average significance after multiple iterations 
    " 
    
    # Call clustering function 
    dendro_plot <- ClusterLines(dat_long, n_plts, plt_names, n_cluster) 
    cluster_list <- c(dendro_plot[3], dendro_plot[4], dendro_plot[5]) 
    cluster_labels <- c("Increasing", "Decreasing", "Fluctuating") #name the clusters by the similarity between their plots 
    
    # Loop through and get sample data 
    sig_results <- c() #define vector to store power values 
    for (i in 1:num_iterations) {
      
      # Generate data 
      eval_cond <- Get_sentiment_eval(dat_long, n_plts, sample_size, plt_names) #evaluative condition; have to recreate every time based on E1 Lifelines 
      non_eval_cond <- Get_sentiment_non_eval(sample_size, n_plts, plt_names) #non-evaluative condition; have to recreate every time 
      sample_df <- rbind(eval_cond, non_eval_cond) #bind together into one data frame 
      
      # Group lifelines into their respective clusters 
      for(j in 1:length(cluster_list)) {  #for every cluster in cluster_list... 
        for(k in 1:length(sample_df$name)) {  #go through every plot name in sample_df... 
          if(sample_df$name[k] %in% cluster_list[[j]]) {  #check if each plot name exists in a given cluster from cluster_list...
            sample_df$name[k] <- j  #and, if it does, replace it with that specific cluster number 
          }
        }
      }
      
      # Create numeric versions of cluster names, condition type, and subject number 
      sample_df$name <- as.numeric(factor(sample_df$name)) #create numeric version of clusters 
      sample_df$condition <- as.numeric(factor(sample_df$condition))
      sample_df$subject <- as.numeric(factor(sample_df$subject))
      
      # Run the analysis 
      # Fixed effects: plot type (name) and evaluative or non-evaluative (condition); random effects: subject ID
      sample_model <- lmer(value ~ condition*name + (1 | subject), data = sample_df) 
      summary(sample_model) 
      
      # Check what the interaction looks like 
      # interaction.plot(x.factor = sample_df$name, #x-axis variable
      #                  trace.factor = sample_df$condition, #variable for lines
      #                  response = sample_df$value, #y-axis variable
      #                  fun = median, #metric to plot
      #                  ylab = "Sentiment Score",
      #                  xlab = "Lifeline Clusters",
      #                  col = c("firebrick3", "black"),
      #                  lty = 1, #line type
      #                  lwd = 2, #line width
      #                  trace.label = "Condition")
      
      # Check and store the results of the interaction "condition:name" (evaluative or not:plot type)
      sig_results[i] <- tidy(sample_model)$p.value[tidy(sample_model)$term == "condition:name"] <= 0.05 #tidy up p-values: are they significant? 
    }
    
    # Return results 
    sig_results %>%
      mean(na.rm = TRUE) %>%
      return()
}


Get_sample_size <- function(num_iterations, dat_long, n_plts, n_sample, plt_names, n_cluster) {
    "
    Generates power for various sample sizes; calls Get_power() 
    Input: number of iterations, data_long, n_plots, sample_sizes_to_try, plot_names, n_clusters
    Output: Average significance, indicated by mean p-value after multiple iterations 
    " 
    
    # num_iterations <- num_iter 
    # dat_long <- data_long 
    # n_plts <- n_plots
    # n_sample <- sample_sizes_to_try
    # plt_names <- plot_names 
    # n_cluster <- n_clusters 
    
    # Calculate power level 
    power_levels <- c() 
    for (i in 1:length(n_sample)) {
      power_levels[i] <- Get_power(num_iterations, dat_long, n_plts, n_sample[i], plt_names, n_cluster)
    }
    
    # Where do we cross 80%?
    power_results <- tibble(sample = n_sample,
                            power = power_levels)
    print(power_results)

    # Plot results 
    sample_size_plot <- ggplot(power_results, aes(x = sample, y = power)) +
      geom_line(color = 'red', size = 1.5) +
      geom_hline(aes(yintercept = 0.80), linetype = 'dashed') + # add a horizontal line at 80%
      theme_minimal() +
      scale_y_continuous(labels = scales::percent) +
      labs(x = 'Sample Size', y = 'Power')
    
    return(sample_size_plot)
}


# ============================================== MAIN ============================================== #


# Define Variables 
num_iter <- 500 #define how many times you would like to iterate through the power calculation for each sample size 
sample_sizes_to_try <- seq(from = 50, to = 400, by = 50) #define how many samples you would like to try   
n_clusters <- 3 #define how many clusters you would like to divide the lifelines into based on the results of the dendrogram 


# Call Functions
cluster_analysis <- ClusterLines(data_long, n_plots, plot_names, n_clusters) #returns a list of dendrogram plot and the lifeline names associated with it 
dendrogram_plot <- PlotDendrogram(data_long, n_plots, plot_names, n_clusters, my_equations) 

pdf(file = "power_dendrogram.pdf", width = 17, height = 8) #save to file
  ggdraw(insert_xaxis_grob(cluster_analysis[[1]], dendrogram_plot, position = "bottom"))
dev.off() 


# Run Power Analysis; TAKES OVER 6 HOURS 
Get_sample_size(num_iter, data_long, n_plots, sample_sizes_to_try, plot_names, n_clusters)


## ========================================= MOVE FILES ============================================= #


# Move Files 
dir.create("analysis_plots") 
plot_files <- list.files(pattern = c("(.pdf|.png)"))
file.move(plot_files, "analysis_plots", overwrite = TRUE)


# ============================================== END ================================================ #































########################################## CODE GRAVEYARD #############################################


# # Evaluative Condition: Get sentiment scores from participants for each plot  
# eval_sample <- c()
# for(i in 1:n_plts) {
#   eval_sample[[i]] <- sentiment_by(equations[[i]])$ave_sentiment
# }
# 
# # Create data frame 
# eval_df <- data.frame(eval_sample) 
# colnames(eval_df) <- plot_names #column names 
# eval_long <- pivot_longer(eval_df, cols = everything()) #long version 
# 
# cond_1 <- "eval" #assign condition name 
# eval_long$condition <- cond_1 #add to data frame 
# subject_1 <- rep(1:n_final, each = n_plts) #assign participant IDs 
# eval_long$subject <- subject_1 #add to data frame



# sig_results[i] <- tidy(sample_model)$p.value[2:(n_plts + 1)] <= 0.05 #tidy up p values: are they significant? (Yes)



# # Data 
# eval_cond <- Get_sentiment_eval(data_long, n_plots, n_sample) #evaluative condition 
# non_eval_cond <- Get_sentiment_non_eval(n_sample, n_plots, plot_names) #non-evaluative condition 
# sample_data <- rbind(eval_cond, non_eval_cond) #bind together into one data frame 
# 
# 
# # Linear Regression Analysis 
# sample_model <- lmer(value ~ name + condition + (1 | subject), data = sample_data)
# summary(sample_model)



# pacman::p_load(
#   'BiocManager', 
#   'rtools', 
#   'devtools', 
#   'simr' #power simulation package for estimating sample size 
# )
# install_github("SampleSizeShop/invWishartSum")
# install_github("SampleSizeShop/mixedPower")
# library(mixedPower)



# # Power Simulation 
# powerSim(sample_model, nsim = 1000, seed = 123)
# # Warning message: "This appears to be an "observed power" calculation" 
# 
# 
# # Power Simulation Using Likelihood Ratio Test 
# set.seed(123) 
# model_0 <- lmer(value ~ name + (1 | subject), data = sample_data)
# model_1 <- lmer(value ~ name + condition + (1 | subject), data = sample_data) 
# 
# sample_power <- powerSim(model_1, test = compare(model_0, method = "lr"), nsim = 1000)
# sample_power 



# # loop to estimate the minimum effect size of effect that we can detect with 80% power
# coeff_vec <- seq(-5.912182, -5.762182, 0.02)
# coeff_pwr <- GetPower(coeff_vec)
# 
# # fit final minimum effect to make sure that power is still ~0.8 after 1000 sims
# fixef(model1)["intention_cond_num"] <- coeff_vec[2] 
# set.seed(123)
# min_power_blame_company <- powerSim(model1, test=compare(model0, method="lr"), nsim=1000)
# min_power_blame_company

