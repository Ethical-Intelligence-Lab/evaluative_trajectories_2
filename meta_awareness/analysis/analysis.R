## Analysis Script for 'Evaluative Summaries'
# Experiment 3: Meta-Awareness

## Clear workspace
rm(list = ls()) 

## Import libraries
if (!require(pacman)) {install.packages("pacman")}
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
               'topicmodels', #fitting topic models 
               'ldatuning', #find number of topics in topic models 
               'lme4', #run mixed effects linear regression
               'lmerTest', #used in conjunction with lme4; get p-values
               'robustHD', #for the standardize function
               'corrplot', #for corrplot()
               'plotrix', #for std.error() function 
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
               'RankAggreg' #performs aggregation of ordered lists based on ranks 
)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

##================================================================================================================
                                         ##FUNCTIONS##
##================================================================================================================

PerformExclusions <- function(data) {
    "
    Excludes participants if they do not finish the survey, finished it too quickly (under 120 seconds), 
    gave duplicate answers, or failed important attention and comprehension checks.
    Input: e6_data   #num_rows = num_ss
    Output: data after it has been 'cleaned'
    "
    
    # data <- e6_data 
  
    # Exclude those who did not finish the survey
    data <- subset(data, (data$Finished == TRUE))
    n_before_exclusions <- dim(data)[1] #100
    
    # Exclude those who finished it in less than 2 minutes
    data <- subset(data, (data$Duration..in.seconds. > 120))
    
    # Exclude those who did not rearrange the order of features shown (DO = display order)
    data <- subset(data, 
                   !( (data$rank_data_1 == data$rank_data_DO_1) &  
                      (data$rank_data_2 == data$rank_data_DO_2) & 
                      (data$rank_data_16 == data$rank_data_DO_16) & 
                      (data$rank_data_3 == data$rank_data_DO_3) & 
                      (data$rank_data_17 == data$rank_data_DO_17) & 
                      (data$rank_data_18 == data$rank_data_DO_18) & 
                      (data$rank_data_19 == data$rank_data_DO_19) & 
                      (data$rank_data_15 == data$rank_data_DO_15) & 
                      (data$rank_data_6 == data$rank_data_DO_6) & 
                      (data$rank_data_9 == data$rank_data_DO_9) & 
                      (data$rank_data_10 == data$rank_data_DO_10) & 
                      (data$rank_data_11 == data$rank_data_DO_11) 
                   ) ) 
    
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
                table(data$attention_check)[2]/n_before_exclusions))

    # Perform comprehension checks
    data$attention_check2 <- ifelse( (data$comp_check_1 == 5 &
                                      data$comp_check_2 == 1 &
                                      data$comp_check_3 ==2
                                      ), 0, 1 ) 
    
    # Perform second round of comprehension checks, if they failed the first
    data$comp_check <- ifelse( ( (is.na(data$comp_check_4 == TRUE))
                                 &
                                   (data$comp_check_7 == 1) &
                                   (data$comp_check_8 == 2) & 
                                   (data$understand_check <= 2)
                                 |
                                   ((data$comp_check_4 == 5) &
                                      (data$comp_check_5 == 5)
                                    &
                                      (data$comp_check_6 == 1) &
                                      (data$comp_check_7 == 1) &
                                      (data$comp_check_8 == 2) &
                                      (data$understand_check <= 2)
                                   ) ), 0, 1)
    
    print(paste('percentage excluded, comprehension checks: ',
                table(data$comp_check)[2]/n_before_exclusions))
    
    # Exclude those who failed either attention or comprehension checks
    data <- subset(data, (data$attention_check == 0) & (data$comp_check == 0))
    
    # Number of subjects after exclusions
    n_after_exclusions <- dim(data)[1] #54 
    print(paste('total percentage excluded, all checks: ',
                (n_before_exclusions-n_after_exclusions)/n_before_exclusions))
    
    data$n_after_exclusions <- n_after_exclusions 
    
    return(data)
}


GetRankings <- function(data, old_labs, new_labs) {
    " 
    Clean the data frame 
    Input: e6_data_clean (dataframe with number of rows = n_subjects), old_names, new_names 
    Output: Optimal order of features, as ranked by participants 
    " 
    
    # data <- e6_data_clean 
    # old_labs <- old_names
    # new_labs <- new_names 
    
    # Rename columns with features 
    setnames(data, old = old_labs, new = new_labs)
    
    # Make data frame longer by making a column of feature names and a column of their rankings as made by participants 
    data_long <- pivot_longer(data, cols = all_of(new_labs), names_to = "features", values_to = "rankings")
    
    # Subset columns: participant response IDs, feature names, and rankings 
    feature_rankings <- data_long[, c("ResponseId", "features", "rankings")]
    
    # Reorder rankings from most to least important  
    feature_order <- feature_rankings[order(feature_rankings$rankings), ]
    feature_order <- as.data.frame(feature_order) #turn into data frame 
    
    # Turn data frame back to wide 
    feature_wide <- reshape(feature_order, v.names = "features", idvar = c("ResponseId"), timevar="rankings", direction="wide")
    
    # Drop the first column containing participant response IDs 
    feature_wide <- as.matrix(feature_wide[,-1]) 
    
    # Rename rows and columns 
    rownames(feature_wide) <- NULL
    colnames(feature_wide) <- str_replace_all(colnames(feature_wide), pattern = "features.", repl = "")  #drop the "features" part of rankings 
    
    # Perform rank order analysis, using aggregation to find the optimal rank order 
    set.seed(123)
    RankAggreg(feature_wide, length(new_labs), verbose = TRUE)

    # Order by mean ranking
    agg_tbl <- feature_rankings %>% group_by(features) %>%
   summarise(mean_ranking=mean(rankings),
             .groups = 'drop')

    print(agg_tbl[order(agg_tbl$mean_ranking),])
    
    #BruteAggreg(feature_wide, length(new_labs)) #this BruteAggreg() function is apparently preferred, but because we have more than 10 features and a large number of participants, R does not have enough memory to perform the calculation.
    # Source: https://www.rdocumentation.org/packages/RankAggreg/versions/0.6.6/topics/BruteAggreg ("This approach works for small problems only and should not be attempted if k is relatively large (k > 10)."); https://www.r-bloggers.com/2021/03/rank-order-analysis-in-r/ 
}


##=============================================================================================
                                           ## MAIN ##
##=============================================================================================

## =================== (1) Read in Data & Create Folder for Saving Files ======================

d_raw <- read.csv('./data/data.csv')
for(i in 1:dim(d_raw)[1]) {
    d_raw$understand_check[i] <- sum(d_raw[i,65:76], na.rm=TRUE)
}

features <- c('slope', 'accel.', 'end', 'AUC', 'peak', 'valley',
             '# peaks', '# valleys', '# peaks & valleys', 'semantic embeddings',
             'interestingness', 'sentiment')

for (i in 65:76) {
    d_raw[,i][is.na(d_raw[,i])] <- 0
    print(paste(features[i-64], mean(d_raw[,i], na.rm=TRUE)))
}

#dir.create("analysis_plots")

## ================================= (2) Define Variables =====================================

# Get column names of the raw data 
old_names <- c("rank_data_1", "rank_data_2", "rank_data_16", "rank_data_3", 
               "rank_data_17", "rank_data_18", "rank_data_19", "rank_data_15", 
               "rank_data_6", "rank_data_9", "rank_data_10", "rank_data_11")

# Assign new names to the above columns corresponding to the feature names from customer journeys 
new_names <- c("d1_avg_unweight", "d2_avg_unweight", "end_value", "integral", 
               "max", "min", "number_peaks", "number_valleys", 
               "number_extrema", "embeddings", "interestingness", "sentiment_score")

## ================================= (3) Perform Analyses =====================================

# Perform exclusions 
d <- PerformExclusions(d_raw) #num_rows = num_ss
d_n_after_exclusions <- d$d_n_after_exclusions[1]

# Perform rank order analysis 
GetRankings(d, old_names, new_names) 

##==============================================================================================
                                             ##END##
##==============================================================================================

