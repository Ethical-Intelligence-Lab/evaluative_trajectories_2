# Julian De Freitas and Pechthida Kim
# Lifelines 
# Pair Movie Arcs with Lifelines 
# Experiment 2 

# Sentiment Analysis # 
# Trained neural network in Google Colab: 
# https://colab.research.google.com/drive/1zpmSnwtg90p1fAXhMvjxtES-llQMIS9B?usp=sharing)

## Clear workspace
rm(list = ls()) 

# Read in Lifelines_analysis_e1b.R 
source2 <- function(file, start, end, ...) {
  file.lines <- scan(file, what = character(), skip = start - 1, nlines = end - start + 1, sep ='\n')
  file.lines.collapsed <- paste(file.lines, collapse ='\n')
  source(textConnection(file.lines.collapsed), ...)
}

# Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #set working directoy to current directory
setwd("../e1b_basic_effect") #go one directory up
source2("Lifelines_analysis_e1b.R", 0, 1920) #skip wordcloud generation (takes a long time)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #go back to current directory

# Import libraries
if (!require(pacman)) {install.packages("pacman")}
pacman::p_load('ggplot2', #plot stuff 
               'gtools', #sorts files and places them into numerical order
               'tools', #for file_path_sans_ext() function 
               'vegan', #for procrustes() function (Procrustes analysis) 
               'stringr', #for separating strings into individual words 
               'sentimentr', #for sentiment analysis 
               'qdap', #comprehensive package for quantitative/qualitative text analysis
               # NOTE: To use qdap, you might have to install Java that matches your RStudio version (32-bit or 64-bit)
               'tidytext', #for get_sentiments() function 
               'syuzhet', #for get_sentiment() function (not to be confused with get_sentiments())
               'filesstrings', #create and move files 
               'dplyr', #dplyr 
               'plyr', #for ldply() function (for each element of a list, apply function then combine results into a data frame)
               'tidyverse' #tidy workspace 
              )

##================================================================================================================
                                      ## DEFINE FUNCTIONS ##
##================================================================================================================
CreateEquations <- function(num_end_age) {
    "
    Define main graph functions for all lifelines 
    Input: num_end_age (end age)
    Output: all lifelines equations 
    "
    
    linear_rise <- function(x) {1.25*x}
    linear_fall <- function(x) {100-1.25*x}
    linear_low <- function(x) {0*x+0}
    linear_middle <- function(x) {0*x+50}
    linear_high <- function(x) {0*x+100}
    exp_rise_convex <- function(x) {1.0595^x-1}
    exp_fall_convex <- function(x) {1.0595^(-x+80)-1}
    exp_rise_concave <- function(x) {100-1.0595^(-x+80)+1}
    exp_fall_concave <- function(x) {100-1.0595^x+1}
    sin_fr_full <- function(x) {50+50*(cos(x*0.079))}
    sin_fr_partial <- function(x) {50-50*(sin(x*0.05889))}
    sin_rf_full <- function(x) {50-50*(cos(x*0.079))}
    sin_rf_partial <- function(x) {50+50*(sin(x*0.05889))}
    sin_rfr_full <- function(x) {50-50*(cos(x*0.1185))}
    sin_rfr_partial <- function(x) {50+50*(sin(x*0.0982))}
    sin_frf_full <- function(x) {50+50*(cos(x*0.1185))}
    sin_frf_partial <- function(x) {50-50*(sin(x*0.0982))}
    sin_frfr <- function(x) {50-50*(sin(x*0.1375))}
    sin_rfrf <- function(x) {50+50*(sin(x*0.1375))} 
    logistic_rise <- function(x) {100/(1+(exp(1))^-(x-40))}
    logistic_fall <- function(x) {100-100/(1+(exp(1))^-(x-40))}
    positive_change_full <- function(x) {
      (x < num_end_age*3/4)*((-5/3)*(x-60)) +
        (num_end_age*3/4 <= x & x <= num_end_age)*(100+5*(x-80))
    }
    positive_change_partial <- function(x) {
      (x < num_end_age*3/4)*((-5/3)*(x-60)) +
        (num_end_age*3/4 <= x & x <= num_end_age)*(50+2.5*(x-80))
    }
    negative_change_full <- function(x) {
      (x < num_end_age*3/4)*(5/3)*x +
        (num_end_age*3/4 <= x & x <= num_end_age)*(-5*(x-80))
    }
    negative_change_partial <- function(x) {
      (x < num_end_age*3/4)*(5/3)*x +
        (num_end_age*3/4 <= x & x <= num_end_age)*(-2.5*(x-100))
    }
    linear_rise_sharp_fall <- function(x) {
      (x < num_end_age/2)*(1.25*x) +
        (num_end_age/2 < x & x <= num_end_age)*(0*x+0)
    }
    linear_rise_sharp_fall_exp_rise <- function(x) {
      (x < num_end_age/2)*(1.25*x) +
        (num_end_age/2 < x & x <= num_end_age)*(1.0606^x-10)
    }
    
    equations <- c(linear_rise, linear_fall, linear_low, linear_middle, linear_high, 
                   exp_rise_convex, exp_fall_convex, exp_rise_concave, exp_fall_concave, 
                   sin_fr_full, sin_fr_partial, sin_rf_full, sin_rf_partial, 
                   sin_rfr_full, sin_rfr_partial, sin_frf_full, sin_frf_partial, 
                   sin_frfr, sin_rfrf, logistic_rise, logistic_fall, 
                   positive_change_full, positive_change_partial, negative_change_full, negative_change_partial, 
                   linear_rise_sharp_fall, linear_rise_sharp_fall_exp_rise)
    
    return(equations)
}


GetLifelinesDataFrame <- function(bin_dependent, win_size, num_start_age, num_end_age, num_plots) {
    "
    Create one data frame for all lifelines
    Input: index_elements, window_size, start_x, end_x, n_plots 
    Output: lifelines_df 
    "
    
    # Create data frame of y values for each lifeline 
    lifelines_df <- data.frame(matrix(NA, nrow = win_size, ncol = num_plots)) #make a blank data frame
    colnames(lifelines_df) <- plot_names #set plot names as column names 
    rownames(lifelines_df) <- num_start_age:(win_size - 1) #set total number of values 
    for(i in 1:num_plots) {
      lifelines_df[, i] <- CreateEquations(num_end_age)[[i]](bin_dependent) 
    }
    
    return(lifelines_df)
}


GetMovieReviews <- function(movie_reviews_csv) {
    "
    Create one data frame for all movie reviews 
    Input: movie_reviews_csv 
    Output: clean_reviews
    "
    
    # Get movie reviews data frame
    reviews_df <- read.csv(movie_reviews_csv, encoding = "UTF-8") #read in movie reviews CSV
    reviews_df$original_release_date <- as.Date(reviews_df$original_release_date) #convert dates from numeric to date format 
    
    # Clean movie reviews data frame
    clean_reviews <- reviews_df %>% 
      filter(is.na(audience_rating) == FALSE) %>% #get only movies that have an audience rating 
      filter(is.na(original_release_date) == FALSE) #%>% #get only movies that have dates 
      # distinct(movie_title, .keep_all = TRUE) #get only unique movies (in other words, get rid of duplicated movie names)
    
    return(clean_reviews)
}


Chopper <- function(words, win_size) {
    "
    Chops up wikipedia summary (words) based on bin size (words_per_win), then gives the sentiment score for each bin
    Inputs: movie text, window size
    Outputs: a list of sentiment scores (mean_sentiment_list)
    "
    
    # Define how many words will be in each window (will likely vary from text to text)
    words_per_win <- length(words)%/%win_size 
    print(paste0("Now splitting the text into chunks of size: ", words_per_win)) 
    
    # Define mean sentiment list to store sentiment scores for all word chunks 
    # mean_polarity_list <- c()
    mean_sentiment_list <- c()
    
    # Break words into chunks, then get sentiment scores 
    for(i in 1:win_size) {
      chunk_list <- c()
      
      if(i == win_size) {
        # For the last window number, use whatever number of words remain: 
        # (from the first word in the last window to the very last word in the entire text; the "leftover words") 
        for(j in (((i-1)*words_per_win) + 1) : (length(words))) { 
          chunk_list <- append(chunk_list, words[j])
        }
      
      } else { 
        # For every window number except the last one: 
        # (from the first word in the jth window to the very last word in that window) 
        for(j in (((i-1)*words_per_win) + 1) : (i*words_per_win)) { 
          chunk_list <- append(chunk_list, words[j])
        }
      }
      
      # Get sentiment per chunk 
      # cur_polarity_list <- polarity(chunk_list)$group$ave.polarity 
      cur_sentiment_list <- get_sentiment(chunk_list, method = "syuzhet", language = "english")
      
      # Append to mean sentiment list 
      mean_sentiment_list <- append(mean_sentiment_list, mean(cur_sentiment_list))
    }
    
    return(mean_sentiment_list) 
}


ProcessText <- function(raw_text, win_size) {
    "
    Splits words by line and cleans them, then calls the Chopper() function
    Inputs: raw_text (raw movie text), win_size (window size, for the Chopper() function) 
    Outputs: movie text (kwords), a list of sentiment scores (sentiment_list)
    "
    
    # Get text from wiki summary and clean them up 
    raw_text_list <- as.list(raw_text)
    lower_words <- tolower(raw_text_list) #make all words lowercase 
    remove_stopwords <- rm_stopwords(lower_words) #remove stopwords 
    clean_words <- gsub("[^a-z]", " ", remove_stopwords) #get rid of numbers and special characters, leaving only letters a-z
    single_words <- unlist(str_extract_all(unlist(remove_stopwords), "[[:alpha:]]+\\w{2,}")) #get individual words with length of at least 2, getting rid of all spaces 
    
    # Only score the words that exist in the "nrc" lexicon 
    # keep_words <- get_sentiments("nrc")$word #choose the words to keep from the starting documents
    # keep_pattern <- paste0(keep_words, collapse = "|") #create a regex pattern of the keep_words
    # final_text <- unlist(str_extract_all(string = single_words, pattern = keep_pattern)) #remove only the keep_words, as a vector
    
    print(paste0("length of the text: ", length(single_words))) 
    
    # Apply Chopper() function 
    sentiment_list <- Chopper(single_words, win_size) 
    
    return(sentiment_list)
}


# Sanity_Check <- function(win_size) {
#   "Get sentiment scores for sample movies. 
#     Input(s): win_size (window size)
#     Output(s): final_data (final data frame of movie sentiment scores)" 
#   
#   # Get Wikipedia summaries 
#   sample_summaries <- c("Marvel's_The_Avengers", "Baby_Driver", "Deadpool_2", "Die_Another_Day", "Frozen_II", 
#                         "Get_Out", "Lord_of_the_Flies", "Love,_Simon", "Pan's_Labyrinth", "The_Matrix")   
#   # raw_text_clean <- read.delim("./wiki_summaries/Marvel's_The_Avengers.txt", header = FALSE)
#   
#   for(i in 1:length(sample_summaries)) { 
#     movie_name <- sample_summaries[i]  
#     
#     tryCatch( #if error reading file name, print error and continue 
#       raw_text_clean <- read.delim(paste0("./wiki_summaries/", sample_summaries[i], ".txt"), header = FALSE), #read in each movie file 
#       error = function(x) {cat(paste0("ERROR: Cannot read file name: '", sample_summaries[i], ".txt", "':"), conditionMessage(x), "\n")}
#     ) 
#     
#     final_list <- ProcessText(raw_text_clean, win_size) #process text 
#     
#     print(movie_name) 
#     # print(final_list)
#     
#     pdf(paste0(movie_name, "_", win_size, ".pdf"))
#     plot(final_list, type = "l", lwd = 2, col = "deepskyblue3", 
#          xlab = "Number of Bins", ylab = "Sentiment Score", main = movie_name)
#     dev.off()
#   }
# }
 

# GetMovieSentimentScores <- function(win_size, num_movies) {
#     "Get sentiment scores for each movie. 
#     Input(s): win_size (window size), num_movies (number of movies)
#     Output(s): final_data (final data frame of movie sentiment scores)" 
#     
#     # Get Wikipedia summaries 
#     wiki_summaries <- list.files(path = "./wiki_summaries", pattern = "*.txt") #list all files with the pattern ".txt" 
#     movie_names <- file_path_sans_ext(wiki_summaries) #get movie name without file extension 
#     
#     # Define empty master data frame
#     final_data <- data.frame(matrix(NA, nrow = win_size, ncol = num_movies)) 
#     colnames(final_data) <- movie_names 
#     
#     # Get sentiment scores for each movie summary 
#     for(i in 1:length(wiki_summaries)) { 
#       movie_name <- movie_names[i]  
#       
#       tryCatch( #if error reading file name, print error and continue 
#         raw_text_clean <- read.delim(paste0("./wiki_summaries/", wiki_summaries[i]), header = FALSE), #read in each movie file 
#         error = function(x) {
#           cat(paste0("ERROR: R cannot read file name: '", wiki_summaries[i], "':"), conditionMessage(x), "\n")
#           # file.rename(wiki_summaries[i], paste0("renamed_file_", i, ".txt")) 
#           # raw_text_clean <- read.delim(paste0("./wiki_summaries/", wiki_summaries[i]), header = FALSE) #try again
#           raw_text_clean <- NULL
#         }
#       ) 
#       
#       final_list <- ProcessText(raw_text_clean, win_size) #process text 
#       final_data[i] <- final_list #append to final data frame 
#       
#       print(movie_name) 
#     }
#     
#     return(final_data)
# }


GetMoviesDataFrame <- function(scores_csv, clean_reviews) {
    "
    Create one data frame for all movies 
    Input: scores_csv (csv of movie sentiment scores), movie_reviews_df 
    Output: movies_data 
    " 
    
    # Get movie sentiment scores 
    movies_dat <- read.csv(scores_csv, header = TRUE, check.names = FALSE, encoding = "UTF-8") #read in movie sentiment scores   
    movies_dat <- movies_dat[ ,!(colnames(movies_dat) %in% c(""))] #remove any columns with blank column names
    
    # Clean by using only movies with audience reviews 
    colnames(movies_dat) <- gsub("_", " ", colnames(movies_dat)) #clean column names (replace underscores with spaces)
    movies_data <- movies_dat[, colnames(movies_dat) %in% clean_reviews$movie_title]
    
    return(movies_data)
}


CorrelationAnalysis <- function(lifelines_data, movies_data, num_plots, num_movies) {
    "
    Perform correlation analysis to compare the shapes of the movie arcs to our lifelines
    Input: lifelines_df, movies_df (movie and lifeline dataframes); n_plots, n_movies (number of lifelines and movies)
    Output: master_data (contains all relationships between lifelines and movie lines)
    "
    
    # Convert data frames to matrices in preparation for correlation analysis 
    lifelines_matrix <- data.matrix(lifelines_data)
    movies_matrix <- data.matrix(movies_data) 
    
    # Create a "master" data frame for all lifelines and movies 
    master_data <- data.frame(matrix(NA, nrow = num_movies, ncol = num_plots)) #make a blank data frame 
    colnames(master_data) <- colnames(lifelines_matrix) #set lifeline names as column names 
    rownames(master_data) <- colnames(movies_matrix) #set movie names are row names 
    
    # Loop through lifelines and movies to find correlation; takes a few minutes  
    for(i in 1:num_plots) { #loop through columns for lifelines 
      
      # Get correlation   
      proc_2 <- data.matrix(lifelines_matrix[, i]) #proc_2 = the lifeline to be compared 
      
      for(j in 1:num_movies) { #loop through rows for movies 
        proc_1 <- data.matrix(movies_matrix[, j]) #proc_1 = the movie to be compared 
        master_data[j, i] <- cor(proc_1, proc_2) #insert the correlation into the master df
      } 
    } 
    
    # Replace any missing values with zero
    master_data[is.na(master_data)] <- 0
    # is.nan.data.frame <- function(x)
    #   do.call(cbind, lapply(x, is.nan))
    # master_data[is.nan(master_data)] <- 0
    
    return(master_data)
}


Plotter <- function(bin_dependent, equation, weight, start_age, end_age, start_y_axis, end_y_axis, x_label, y_label, line_name, color) {
    "
    Plots the lifeline and movie values as lines 
    Inputs: index_elements, equation, weight, start_age, end_age, start_y_axis, end_y_axis, x_label, y_label, line_name
    Output: Plotter 
    "
    
    plot(x = bin_dependent, y = equation, type = "l", lwd = weight, xlim = c(start_age, end_age), ylim = c(start_y_axis, end_y_axis), 
         main = line_name, xlab = x_label, ylab = y_label, col = color, cex.lab = 1.5, cex.axis = 1.5)
    
    return(Plotter)
}


MatchMovieLineToLifeline <- function(bin_dependent, master_data, lifelines_data, movies_data, movie_reviews_data, 
                                     num_plots, plt_names, start_age, end_age, start_happy, end_happy, num_movie_matches) { 
    "
    Match the best movie line to each of our lifelines 
    Input: index_elements, master_df, lifelines_df, movies_df, movie_reviews_df, 
      n_plots, plot_names, start_x, end_x, start_happiness, end_happiness, n_movie_matches
    Output: best_match 
    "
    
    # movie_reviews_data <- movie_reviews_df
    # bin_dependent <- index_elements
    # master_data <- master_df
    # lifelines_data <- lifelines_df
    # movies_data <- movies_df
    # num_plots <- n_plots
    # plt_names <- plot_names
    # start_age <- start_x
    # end_age <- end_x
    # start_happy <- start_happiness
    # end_happy <- end_happiness
    # num_movie_matches <- n_movie_matches
    
    # 1. Remove lifelines with constant values (no correlation values)
    # Get the smallest distance in each lifeline column (to match the best movie line to the lifeline)
    constant_cols <- c() #define list of constant columns to remove 
    for(lifeline in 1:num_plots) { #loop through lifelines 
      
      # For the lifelines that have all identical y-values (think those with flat horizontal lines)
      if(length(unique(master_data[, lifeline][!is.na(master_data[, lifeline])])) == 1) { #if the column is constant, remove the column and print none
        constant_cols <- append(constant_cols, lifeline)
      }
    }
    
    # Remove the constant lifelines 
    master_data_1 <- master_data[, -constant_cols] 
    master_data_2 <- master_data_1 #define the data frame that will be used in the loop below 
    
    # ----
    
    #2. Loop through the remaining regular lifelines to get movie matches
    
    # Initialize list of movies to be removed (number of bins * number of remaining lifelines)
    remove_movies <- c()
    
    # Define empty data frame to store lifeline and movie matches 
    group_data <- data.frame(matrix(NA, nrow = num_movie_matches, ncol = (num_plots - length(constant_cols)))) 
    colnames(group_data) <- plt_names[-constant_cols] #set column names (total - removed lifelines)
    rownames(group_data) <- 1:num_movie_matches #set row names (number of desired movie matches) 
    
    for(movie in 1:num_movie_matches) {
      while(dim(data.frame(master_data_2))[2] > 0) {
        max_correlation <- max(master_data_2) #get the biggest correlation within this entire data frame 
        lifeline_position <- which(master_data_2 == max_correlation, arr.ind = TRUE)[, "col"]
        movie_position <- which(master_data_2 == max_correlation, arr.ind = TRUE)[, "row"]
        lifeline_name <- colnames(master_data_2)[lifeline_position] #get the lifeline name
        movie_name <- row.names(master_data_2)[movie_position] #get the movie name
        remove_movies <- append(remove_movies, movie_name) #remove movies 
        
        # This code only runs if the same lifeline is matched with multiple movies 
        # (e.g., "Deadpool_2" (R-rated) and "Once_Upon_a_Deadpool" (PG-13)), in which case, 
        # pick the original (earliest release date).
        all_same <- function(x) {length(unique(x)) == 1} #define function to find unique occurrences
        
        
        # If there are multiple matches for the same lifeline, then there will be multiple elements in the lifeline_name vector. 
        if(length(lifeline_name) > 1) { #if there are multiple elements in lifeline_name
          if(all_same(lifeline_name) == TRUE) { #and if those elements are identical (i.e., same lifeline name)
            date_list <- c() 
            for(i in 1:length(movie_name)) { #then get the release dates for each of the matched movies from the movie_name vector
              release_date <- movie_reviews_data[movie_reviews_data$movie_title == movie_name[i], ]$original_release_date
              release_date <- format(release_date, format="%Y-%m-%d") #reformat the dates to be put into a list
              date_list[i] <- release_date
            }
            
            original_date <- min(date_list) #and get the earliest date using the min() function. 
            
            # Get the movie name with the earliest release date and assign it to movie_name 
            for(i in 1:length(movie_name)) {
              movie_name <- movie_reviews_data[
                # Get the movie that is within the movie_name vector 
                (movie_reviews_data$movie_title %in% movie_name) & 
                  
                  # ...and that matches the earliest release date 
                  (movie_reviews_data$original_release_date == original_date), ]$movie_title 
            }
            
            # Reassign lifeline_name so that there is only one element
            lifeline_name <- lifeline_name[1] 
          }
        }
        
        
        # Insert movie name into group data frame 
        group_data[movie, lifeline_name] <- movie_name

        # Remove the previous paired lifeline and movie to find the next best match (on the basis of "mutual attraction")
        master_data_2 <- data.frame(master_data_2[-movie_position, -lifeline_position, drop = FALSE])
      }
      
      # Redefine master_data, each time removing the already matched movies 
      # invisible(capture.output(master_data_2 <- data.frame(anti_join(master_data_1, master_data_1[remove_movies, ]))))
      master_data_2 <- data.frame(anti_join(master_data_1, master_data_1[remove_movies, ]))
    }
    
    #---- 
    
    #3. Plot matches
    # Make side-by-side plot of original lifeline and grouped plots
    op <- par(mfrow = c(1, 1))
    for(i in 1:dim(group_data)[2]) { #loop through the lifelines (columns) 
      
      current_line <- colnames(group_data)[i]
      pdf(file = paste0(current_line, "_match.pdf", ""), width = 15, height = 7)
      par(mfrow = c(1, 2))
      
      #plot original lifeline
      Plotter(bin_dependent, lifelines_data[[current_line]], 3, start_age, end_age, 
              start_happy, end_happy, "Age", "Happiness", current_line, 'firebrick3')
      
      
      #plot very best match, then add lines for remaining best matches
      best_movie <- group_data[[i]][1]
      Plotter(bin_dependent, movies_data[[best_movie]], 3, start_age, end_age,
              # min(movies_data[[best_movie]]), max(movies_data[[best_movie]]), 
              -5, 5, 
              "Time", "Sentiment", paste0("Best Match: ", best_movie), 'green')

      for(j in 2:dim(group_data)[1]) {
        current_movie <- group_data[[i]][j]
        lines(bin_dependent, movies_data[[current_movie]], type = "l", lwd = 3, col = "firebrick3")
      }
      
      current_movie <- group_data[[i]][1]
      lines(bin_dependent, movies_data[[current_movie]], type = "l", lwd = 3, col = "green")
      
      par(op)
      dev.off()
    }
    
    dev.off()
    return(group_data)
}


# start_point <- start_x
# end_point <- end_x
# shrink_ratio <- bin_shrink_ratio
# matches_df <- match_df
# master_data <- master_df
# lifelines_data <- lifelines_df
# movies_data <- movies_df
# num_plots <- n_plots
# num_movie_matches <- n_movie_matches
# dat_long <- data_long
# dat_plot_long <- data_plot_long
# plt_names <- plot_names


GetIntegralCorrelations <- function(matches_df, master_data, lifelines_data, movies_data, 
                                    num_plots, num_movie_matches, plt_names, 
                                    start_point, end_point, shrink_ratio, dat_long, dat_plot_long) {
    "
    What: Correlates the 24 average movie category integrals to the 24 lifeline integrals; calls CreateSentimentDataframe()
    Inputs: match_df, master_df, lifelines_df, movies_df, n_plots, n_movie_matches, start_x, end_x, bin_shrink_ratio, data_plot_long
    Output: a correlation plot of movie and lifeline integrals 
    "
    
    # 1. Remove lifelines with constant values (i.e., the ones that have no correlation values in master_df)
    constant_cols <- c() #define list of constant lifelines to remove 
    for(lifeline in 1:num_plots) { #loop through lifeline names
      
      # For the lifelines that have all identical y-values (think those with flat horizontal lines)
      if(length(unique(master_data[, lifeline][!is.na(master_data[, lifeline])])) == 1) { #if the column is constant, remove the column 
        constant_cols <- append(constant_cols, lifeline)
      }
    } 
    clean_lifelines_data <- lifelines_data[, -constant_cols] #remove the constant lifelines
    
    # --- 
    
    # 2. Create data frames for each lifeline (column: movie names, row: values 0 through 8), then put into list 
    name_plots <- colnames(matches_df) #define the 24 lifeline names to be used 
    i <- 0 #set index for the lifelines list 
    lifelines_list <- list() #define empty list to store lifeline data frames 
    
    for(plt in name_plots) { 
      i <- i + 1 #index loop 
      
      movie_names <- matches_df[, colnames(matches_df) == plt] #get movie names 
      separate_df <- movies_data[, colnames(movies_data) %in% movie_names] #get specific lifeline data frame using movie names
      lifelines_list[[i]] <- assign(paste0(plt, "_df"), separate_df) #put into list 
    }
    
    names(lifelines_list) <- name_plots #name data frames in the list 
    
    # --- 
    
    # 3. Standardize movie data frames for comparison (between 0 and 100)
    scaled_lifelines_list <- list() #define new scaled list 
    for(lifeline in names(lifelines_list)) { 
      scaled_lifelines_list[[lifeline]] <- as.data.frame(lapply(lifelines_list[[lifeline]], function(x) ( ((x - min(x)) / (max(x) - min(x))) *100) ))
      colnames(scaled_lifelines_list[[lifeline]]) <- colnames(lifelines_list[[lifeline]])
    }
    names(scaled_lifelines_list) <- names(lifelines_list)
    
    # --- 
    
    # 4. Find movie integrals for the 27 lifelines using function made from points 
    x_values <- c((start_point:(end_point/shrink_ratio))*10) #define points to integrate (0 to 8, in tens)
    
    # Create data frame to store all integral calculations 
    integral_df <- data.frame(matrix(NA, nrow = num_movie_matches, ncol = length(scaled_lifelines_list))) 
    colnames(integral_df) <- names(scaled_lifelines_list)
    average_integrals <- list() #create list to store integral averages 
    
    # Loop through all movies in each lifeline data frame to find its integral 
    for(lifeline in names(scaled_lifelines_list)) { 
      
      # Create lists to store functions and integrals for each lifeline 
      function_list <- list() 
      for(movie in 1:length(colnames(scaled_lifelines_list[[lifeline]]))) {
        y_values <- scaled_lifelines_list[[lifeline]][, movie] #get all row values from a movie in a lifeline data frame 
        function_list[[movie]] <- approxfun(x_values, y_values, method = "linear") #make function from plot points 
        integral_df[[movie, lifeline]] <- integrate(function_list[[movie]], start_point, end_point)$value #integrate function 
      }
      
      # Find the average integral for each lifeline (movie category)
      average_integrals[[lifeline]] <- mean(integral_df[, lifeline])
    }
    
    # --- 
    
    # 5. Compare (correlate) the 24 average movie category integrals to the 24 lifeline integrals 
    lifeline_integrals <- list() #define list to store the 24 lifeline integrals 
    for(lifeline in names(scaled_lifelines_list)) { 
      lifeline_integrals[[lifeline]] <- features[lifeline, "integral"] 
    }
    
    # Correlate
    movie_lifeline_test <- cor.test(as.numeric(lifeline_integrals), as.numeric(average_integrals))
    print("-----------------------------------------------------------------------")
    print("Correlation between movie arc integrals and lifeline integrals: ")
    print(movie_lifeline_test)
    
    # --- 
    
    # 6. Plot correlation between lifeline integrals with movie arc integrals 
    pdf("integral_lifeline_corrplot.pdf", width = 15, height = 7)
    Plotter2(as.numeric(lifeline_integrals), as.numeric(average_integrals), 
             "Lifeline and Movie Arc Integral Correlation", 
             "Lifeline Integrals", "Average Movie Arc Integrals", "firebrick3", 
             movie_lifeline_test$estimate, 1500, 4600)
    text(stars.pval(movie_lifeline_test$p.value), x = 1500, y = 4550)
    dev.off() 
    
    # --- 
    
    # 7. Correlate movie arc integrals with meaningfulness and personal desirability ratings and sentiment scores 
    
    # Get average meaningfulness ratings 
    meaning_df <- dat_plot_long[dat_plot_long$question_type == "meaning_score_avg", ]
    sub_meaning_df <- meaning_df[meaning_df$plot_names %in% colnames(clean_lifelines_data), ] #get rid of the constant functions 
    sub_meaning_df <- sub_meaning_df[match(colnames(clean_lifelines_data), sub_meaning_df$plot_names), ] #reorder lifelines
    avg_meaning_scores <- sub_meaning_df$score
    names(avg_meaning_scores) <- colnames(clean_lifelines_data)
    
    # Correlate movie arc integrals with meaningfulness ratings
    corr_meaning <- cor.test(as.numeric(avg_meaning_scores), as.numeric(average_integrals)) 
    print("-----------------------------------------------------------------------")
    print("Correlation between movie arc integrals and meaningfulness ratings: ")
    print(corr_meaning)
    
    # Plot results 
    pdf("integral_meaning_corrplot.pdf", width = 15, height = 7)
    Plotter2(as.numeric(avg_meaning_scores), as.numeric(average_integrals), 
             "Meaningfulness Rating and and Movie Arc Integral Correlation",  
             "Average Meaningfulness Ratings", "Average Movie Arc Integrals", 
             "#56B4E9", corr_meaning$estimate, 35, 4600)
    # text(stars.pval(corr_meaning$p.value), x = 35, y = 4550)
    text(paste0("p = ", round(corr_meaning$p.value, 3)), x = 35, y = 4525)
    dev.off() 
    
    # ----
    
    # Get average personal desirability ratings 
    pd_df <- dat_plot_long[dat_plot_long$question_type == "pd_score_avg", ]
    sub_pd_df <- pd_df[pd_df$plot_names %in% colnames(clean_lifelines_data), ] #get rid of the constant functions 
    sub_pd_df <- sub_pd_df[match(colnames(clean_lifelines_data), sub_pd_df$plot_names), ] #reorder lifelines
    avg_pd_scores <- sub_pd_df$score
    names(avg_pd_scores) <- colnames(clean_lifelines_data)
    
    # Correlate movie arc integrals with personal desirability ratings
    corr_pd <- cor.test(as.numeric(avg_pd_scores), as.numeric(average_integrals)) 
    print("------------------------------------------------------------------------------")
    print("Correlation between movie arc integrals and personal desirability ratings: ")
    print(corr_pd)
    
    # Plot results 
    pdf("integral_pd_corrplot.pdf", width = 15, height = 7)
    Plotter2(as.numeric(avg_pd_scores), as.numeric(average_integrals), 
             "Personal Desirability Rating and and Movie Arc Integral Correlation",  
             "Average Personal Desirability Ratings", "Average Movie Arc Integrals", 
             "#009E73", corr_pd$estimate, 25, 4600)
    # text(stars.pval(corr_pd$p.value), x = 25, y = 4550)
    text(paste0("p = ", round(corr_pd$p.value, 3)), x = 25, y = 4525)
    dev.off() 
    
    # ----

    # Get average sentiment scores 
    sentiment_df <- CreateSentimentDataframe(dat_long, num_plots, plt_names) #this function is imported
    sub_sentiment_df <- sentiment_df[sentiment_df$plot_names %in% colnames(clean_lifelines_data), ] #get rid of the constant functions 
    sub_sentiment_df <- sub_sentiment_df[match(colnames(clean_lifelines_data), sub_sentiment_df$plot_names), ] #reorder lifelines
    avg_sentiment_scores <- sub_sentiment_df$mean
    
    # Correlate movie reviews with lifeline sentiment scores
    corr_sentiment <- cor.test(as.numeric(avg_sentiment_scores), as.numeric(average_integrals)) 
    print("-----------------------------------------------------------------")
    print("Correlation between movie arc integrals and lifeline sentiment scores: ")
    print(corr_sentiment) 
    
    # Plot results 
    pdf("integral_sentiment_corrplot.pdf", width = 15, height = 7)
    Plotter2(as.numeric(avg_sentiment_scores), as.numeric(average_integrals), 
             "Sentiment Score and and Movie Arc Integral Correlation",  
             "Average Sentiment Scores", "Average Movie Arc Integrals", 
             "darkorange", corr_sentiment$estimate, -0.35, 4600)
    # text(stars.pval(corr_sentiment$p.value), x = -0.35, y = 4550)
    text(paste0("p = ", round(corr_sentiment$p.value, 3)), x = -0.35, y = 4525)
    dev.off() 
    
    
    integral_list <- list(integral_df, average_integrals)
    
    return(integral_list)
}


Get_geometric_mean <- function(x, na.rm = TRUE) { 
    " 
    Input(s): x (a vector or list)
    Output: the geometric mean of the elements of the list
    "
    
    exp(sum(log(x[x > 0]), na.rm = na.rm) / length(x)) 
}


Get_reliability <- function(movie_integral, dat_long, question_type, n_ss, cor_results, movie_summary = NULL) {
    "
    Find correlation values between two randomly sampled halves of the data, 
    correct with the Spearman-Brown Prophecy formula (defined above), and put into a list.

    Input: movie_integrals, data_long, question type ('meaningfulness', 'personal_desirability', 'word', 'movie', or 'integral'), 
      n_after_exclusions, corr_results, summary_movie
    Output: summaries of reliabilty of each question type 
    "
    
    # movie_integrals
    # dat_long <- data_long
    # question_type <- "movie"
    # n_ss <- n_after_exclusions
    # cor_results <- corr_results
    
    # ----
    
    # Define list of scores 
    list_of_question <- c()
    
    # 1 & 2. Meaningfulness and Personal Desirability Ratings 
    meaning_pd <- c("meaningfulness", "personal_desirability") #make vector of rating type 
    if(question_type %in% meaning_pd) { 
      # Convert "meaningfulness" and "personal_desirability" columns into numeric
      dat_long[, meaning_pd] <- sapply(dat_long[, meaning_pd], as.numeric)
      
      # Filter the scores using the "plot_names" column, and put into a list
      for(i in plot_names) {
        list_of_question[i] <- dat_long %>% 
          filter(plot_names == i) %>% 
          select(question_type) 
      }
    } 
    
    # ----
    
    # 3. Sentiment Scores 
    if(question_type == "word") { 
      # Clean words in column 
      sentiment_long <- dat_long 
      sentiment_long$word <- word(tolower(sentiment_long$word), 1) #all lowercase; get the first word 
      sentiment_long$word <- gsub("[^a-z]", "", sentiment_long$word) #get rid of all special characters, only taking letters a-z
      
      # Get sentiment scores 
      participant_words <- get_sentences(as.character(sentiment_long$word))
      sentiment_long$word <- sentiment_by(participant_words)$ave_sentiment
      
      # Filter the scores using the "plot_names" column, and put into a list
      for(i in plot_names) {
        list_of_question[i] <- sentiment_long %>% 
          filter(plot_names == i) %>% 
          select(question_type) #%>% 
      }
    }
    
    # ----
    
    # 4. Movie Ratings 
    if(question_type == "movie") { 
      # Get movie ratings 
      list_of_question <- cor_results$movie_reviews 
    }
    
    # ----
    
    #5. Movie Integrals 
    if(question_type == "integral") {
      # Get movie category integrals 
      list_of_question <- movie_integrals[[1]]
      list_of_question <- as.list(list_of_question)
    }
    
    # ----
    
    # Perform Split-Half Correlations 
    # Convert to data frame and take out constant columns 
    df_of_question <- data.frame(list_of_question) 
    new_df_question <- df_of_question[colnames(df_of_question) %in% colnames(match_df)] 
    
    # Find correlation values between two randomly-sampled halves of the data, 
    # correct with the Spearman-Brown Prophecy formula (defined above), and put into a list. 
    
    # Divide number of participants in half 
    half_n_ss <- n_ss/2 
    
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
    for(i in 1:sims) {
      
      # Get random samples 
      rand_assign <- sample(coded, n_ss, FALSE) #randomly assign rows as either 1 or 2 
      assign_1 <- new_df_question[rand_assign == 1, ] #random sample 1 
      assign_2 <- new_df_question[rand_assign == 2, ] #random sample 2 
      means_1 <- colMeans(assign_1) #get the means of random sample 1
      means_2 <- colMeans(assign_2) #get the means of random sample 2
      
      # Perform correlations on the means of both random samples 
      store[i] <- cor(means_1, means_2)
    }
    
    # Apply Spearman-Brown correction: cor_value_adj <- (2 * cor_value) / (1 + cor_value) 
    corrected_store <- Get_spearman_brown_correction(store)
    store_summary <- summary(corrected_store)
    
    # Get noise ceiling using geometric mean of the movie reviews and a given participant rating 
    if(question_type %in% c(meaning_pd, "word", "integral")) {
      bottom_values <- c(store_summary["1st Qu."], movie_summary["1st Qu."])
      top_values <- c(store_summary["3rd Qu."], movie_summary["3rd Qu."])
      rect_bottom <- Get_geometric_mean(bottom_values)
      rect_top <- Get_geometric_mean(top_values)
      
      store_summary <- list(noise_bottom = rect_bottom, noise_top = rect_top)
    }
    
    scores_list <- list(corrected_store, store_summary)
    
    return(scores_list)
}


Plotter2 <- function(x_values, y_values, main_name, x_label, y_label, 
                     col, cor_value, x_topleft, y_topleft) {
    "
    Plots correlation values between movie reviews and participant ratings 
    Inputs: x values, y values, main, x_lab, y_lab, col, corr_value$estimate, 
      top left x value, top left y value
    Output: Plotter2
    "
    
    plot(x_values, y_values, main = main_name, xlab = x_label, ylab = y_label, 
         pch = 19, col = col) #cex.lab = 1.5, cex.axis = 1.5
    abline(lm(y_values ~ x_values), col = "firebrick3", lwd = 3) 
    text(paste0("Correlation: ", round(cor_value, 3)), x = x_topleft, y = y_topleft, font = 2)
    
    # Add noise ceilings 
    # if(y_label == "Average Meaningfulness Ratings") {
    #   rect(0, y_bottom, 100, y_top, col = rgb(.337, .706, .914, alpha = 0.15), border = NA) #meaningfulness 
    # } else if(y_label == "Average Personal Desirability Ratings") { 
    #   rect(0, y_bottom, 100, y_top, col = rgb(0, .62, .451, alpha = 0.15), border = NA) #personal desirability 
    # } else if(y_label == "Average Sentiment Scores") {
    #   rect(0, y_bottom, 100, y_top, col = rgb(1, .549, 0, alpha = 0.15), border = NA) #sentiment  
    # }
    
    return(Plotter2)
}


GetCorrelations <- function(movie_integral, num_movie_matches, movie_reviews_data, matches_df, 
                           dat_plot_long, dat_long, num_plots, plt_names) { 
    "
    Get correlation between ratings and movie reviews; also calls CreateSentimentDataframe()
    Input: movie_integrals, n_movie_matches, movie_reviews_df, match_df, data_plot_long, data_long, n_plots, plot_names
    Output: list of results  
    "
    
    # movie_integral <- movie_integrals
    # num_movie_matches <- n_movie_matches
    # movie_reviews_data <- movie_reviews_df
    # matches_df <- match_df
    # dat_plot_long <- data_plot_long
    # dat_long <- data_long
    # num_plots <- n_plots
    # plt_names <- plot_names
    
    # ----
    # Get average movie reviews (per lifeline) 
    
    # We will need to fetch movie ratings in original rotten tomatoes .csv using movie names that have been assigned to lifelines (matches_df)
    # Since movie names in matches_df have underscores, make clearn data frame in which underscores of movie names in matches_df are removed
    clean_matches_df <- data.frame(matrix(NA, nrow = dim(matches_df)[1], ncol = dim(matches_df)[2])) #make clean movie df
    clean_matches_df[] <- lapply(matches_df, function(x) gsub("_", " ", x)) #clean movie names (replace underscores with spaces)
    colnames(clean_matches_df) <- colnames(matches_df) #assign column names
    
    # Get rid of the constant functions from participant ratings df, since we couldn't get matches for these. 
    unique_plots <- as.character(unique(dat_plot_long$plot_names)) %in% colnames(clean_matches_df)  
    unique_plot_names <- as.character(unique(dat_plot_long[unique_plots, ]$plot_names)) #convert factor to character 
    ordered_matches_df <- clean_matches_df[, unique_plot_names] #reorder matches by lifelines rated from least to most meaningful 
    
    # ----
    # Get movie audience ratings by plot type 
    movie_reviews <- c()  
    avg_movie_reviews <- c() #get average audience ratings 
    for(i in colnames(ordered_matches_df)){ 
        movie_names <- ordered_matches_df[, i] #get movie names 
        subset_movies <- movie_reviews_data[movie_reviews_data$movie_title %in% movie_names, ] #subset movies 
        # avg_movie_review <- mean(subset_movies$audience_rating) #get the mean 
        aggregate_movies <- aggregate(audience_rating ~ movie_title, mean, data = subset_movies) #aggregate any duplicate movies
        movie_reviews[[i]] <- aggregate_movies$audience_rating #get audience rating
        avg_movie_review <- mean(movie_reviews[[i]]) #get the mean #, na.rm = TRUE [this is the mean of all best match movies]
        avg_movie_reviews[i] <- avg_movie_review #compile mean per lifeline 
    }
    
    # ----
    # 1. Meaningfulness Ratings 
    
    # Get average meaningfulness ratings 
    meaning_df <- dat_plot_long[dat_plot_long$question_type == "meaning_score_avg", ]
    sub_meaning_df <- meaning_df[meaning_df$plot_names %in% colnames(ordered_matches_df), ] #get rid of the constant functions 
    sub_meaning_df <- sub_meaning_df[match(colnames(ordered_matches_df), sub_meaning_df$plot_names), ] #reorder lifelines
    avg_meaning_scores <- sub_meaning_df$score
    
    # Correlate movie reviews with lifeline meaningfulness ratings 
    corr_meaning <- cor.test(avg_movie_reviews, avg_meaning_scores) 
    print("-----------------------------------------------------------------------")
    print("Correlation between movie reviews and lifeline meaningfulness ratings: ")
    print(corr_meaning)
    
    # ----
    # 2. Personal Desirability Ratings 
    
    # Get average personal desirability ratings 
    pd_df <- dat_plot_long[dat_plot_long$question_type == "pd_score_avg", ]
    sub_pd_df <- pd_df[pd_df$plot_names %in% colnames(ordered_matches_df), ] #get rid of the constant functions 
    sub_pd_df <- sub_pd_df[match(colnames(ordered_matches_df), sub_pd_df$plot_names), ] #reorder lifelines
    avg_pd_scores <- sub_pd_df$score
    
    # Correlate movie reviews with lifeline personal desirability ratings
    corr_pd <- cor.test(avg_movie_reviews, avg_pd_scores) 
    print("------------------------------------------------------------------------------")
    print("Correlation between movie reviews and lifeline personal desirability ratings: ")
    print(corr_pd)
    
    # ----
    # 3. Sentiment Scores  
    
    # Get average sentiment scores 
    sentiment_df <- CreateSentimentDataframe(dat_long, num_plots, plt_names) #this function is imported
    sub_sentiment_df <- sentiment_df[sentiment_df$plot_names %in% colnames(ordered_matches_df), ] #get rid of the constant functions 
    sub_sentiment_df <- sub_sentiment_df[match(colnames(ordered_matches_df), sub_sentiment_df$plot_names), ] #reorder lifelines
    avg_sentiment_scores <- sub_sentiment_df$mean
    
    # Correlate movie reviews with lifeline sentiment scores
    corr_sentiment <- cor.test(avg_movie_reviews, avg_sentiment_scores) 
    print("-----------------------------------------------------------------")
    print("Correlation between movie reviews and lifeline sentiment scores: ")
    print(corr_sentiment) 
    
    # ---- 
    # 4. Movie Integrals 
    
    # Get average movie integral scores 
    mean_movie_integrals <- movie_integral[[2]]
    order_movie_integrals <- mean_movie_integrals[colnames(ordered_matches_df)] #reorder lifelines
    avg_movie_integrals <- as.numeric(order_movie_integrals) 
    
    # Correlate movie reviews with movie integrals
    corr_integral <- cor.test(avg_movie_reviews, avg_movie_integrals) 
    print("-----------------------------------------------------------------")
    print("Correlation between movie reviews and movie integrals: ")
    print(corr_integral) 
    
    
    # Return results 
    my_list <- list("movie_reviews" = movie_reviews, "avg_movie_reviews" = avg_movie_reviews, 
                    "avg_meaning_scores" = avg_meaning_scores, "avg_pd_scores" = avg_pd_scores, 
                    "avg_sentiment_scores" = avg_sentiment_scores, "avg_movie_integrals" = avg_movie_integrals, 
                    "corr_meaning" = corr_meaning, "corr_pd" = corr_pd, 
                    "corr_sentiment" = corr_sentiment, "corr_integral" = corr_integral)
    
    return(my_list)
}


PlotScatterplots <- function(dat_long, n_ss, cor_results) {
    "
    Plots scatterplots; calls Get_reliability and Plotter2
    Input: data_long, n_after_exclusions, corr_results
    Output: list of results
    "
    
    # dat_long <- data_long
    # n_ss <- n_after_exclusions 
    # cor_results <- corr_results
    
    # Define variables
    avg_movie_reviews <- cor_results$avg_movie_reviews
    avg_meaning_scores <- cor_results$avg_meaning_scores
    avg_pd_scores <- cor_results$avg_pd_scores
    avg_sentiment_scores <- cor_results$avg_sentiment_scores
    corr_meaning <- cor_results$corr_meaning
    corr_pd <- cor_results$corr_pd
    corr_sentiment <- cor_results$corr_sentiment


    # ----
    # 0. Movies

    # Get summary values for movies
    summary_movie <- Get_reliability(movie_integral, dat_long, "movie", n_ss, cor_results)[2][[1]]

    # ----
    # 1. Meaningfulness Ratings

    # Get summary
    summary_meaning <- Get_reliability(movie_integral, dat_long, "meaningfulness", n_ss, cor_results, summary_movie)[[2]]

    # Plot results
    pdf("movie_meaning_corrplot.pdf", width = 15, height = 7)
    plot_meaning <- Plotter2(avg_movie_reviews, avg_meaning_scores,
                             "Scatterplot of Movie Reviews with Lifeline Meaningfulness Ratings",
                             "Average Movie Reviews", "Average Meaningfulness Ratings", "#56B4E9",
                             corr_meaning$estimate, 59, 80)
    text(paste0("p = ", round(corr_meaning$p.value, 3)), x = 59, y = 78)
    dev.off()

    # ----
    # 2. Personal Desirability Ratings

    # Get summary
    summary_pd <- Get_reliability(movie_integral, dat_long, "personal_desirability", n_ss, cor_results, summary_movie)[[2]]

    # Plot results
    pdf("movie_pd_corrplot.pdf", width = 15, height = 7)
    plot_pd <- Plotter2(avg_movie_reviews, avg_pd_scores,
                        "Scatterplot of Movie Reviews with Lifeline Personal Desirability Ratings",
                        "Average Movie Reviews", "Average Personal Desirability Ratings", "#009E73",
                        corr_pd$estimate, 59, 75)
    text(paste0("p = ", round(corr_pd$p.value, 3)), x = 59, y = 73)
    dev.off()

    # ----
    # 3. Sentiment Scores

    # Get summary
    summary_sentiment <- Get_reliability(movie_integral, dat_long, "word", n_ss, cor_results, summary_movie)[[2]]

    # Plot results
    pdf("movie_sentiment_corrplot.pdf", width = 15, height = 7)
    plot_sentiment <- Plotter2(avg_movie_reviews, avg_sentiment_scores,
                               "Scatterplot of Movie Reviews with Lifeline Sentiment Scores",
                               "Average Movie Reviews", "Average Sentiment Scores", "darkorange",
                               corr_sentiment$estimate, 59, 0.35)
    text(paste0("p = ", round(corr_sentiment$p.value, 3)), x = 59, y = 0.32)
    dev.off()
}


Reliability_plotter <- function(corrs_df_long, sum_meaning, sum_pd, sum_sentiment, sum_integral) {
    "
    What this function does: creates a box plot of correlations between participant ratings and movie reviews (plus their noise ceilings) 
    Inputs: corr_df_long, summary_meaning, summary_pd, summary_sentiment, summary_integral
    Output: box plot used in PlotReliability() 
    "
    
    # corrs_df_long <- corr_df_long
    # sum_meaning <- summary_meaning
    # sum_pd <- summary_pd 
    # sum_sentiment <- summary_sentiment
    
    corrs_df_long$question_type <- factor(corrs_df_long$question_type,
                                          levels = c("cor_meaning", "cor_pd", "cor_sentiment", "cor_movie", "cor_integral"))
    corr_box_plot <- ggplot() +
      
      # Plot noise ceilings 
      geom_rect(aes(xmin = 0, xmax = Inf, ymin = sum_meaning$noise_bottom, ymax = sum_meaning$noise_top),
                fill = "#56B4E9", alpha = 0.2) +
      geom_rect(aes(xmin = 0, xmax = Inf, ymin = sum_pd$noise_bottom, ymax = sum_pd$noise_top),
                fill = "#009E73", alpha = 0.2) +
      geom_rect(aes(xmin = 0, xmax = Inf, ymin = sum_sentiment$noise_bottom, ymax = sum_sentiment$noise_top),
                fill = "darkorange", alpha = 0.2) + 
      geom_rect(aes(xmin = 0, xmax = Inf, ymin = sum_integral$noise_bottom, ymax = sum_integral$noise_top),
                fill = "gold", alpha = 0.2) + 
      
      # Plot box plot of correlations 
      geom_boxplot(data = corrs_df_long, aes(x = question_type, y = correlations, fill = question_type)) + #outlier.shape = NA
      ggtitle(paste0("Reliability of Participant and Movie Ratings")) +
      xlab("Type of Rating") + ylab("Pearson's r (Correlation)") + 
      theme_bw() + 
      theme(element_blank(), 
            plot.title = element_text(size = 20, face = "bold", hjust = 0.5), 
            text = element_text(size = 10),
            axis.title.y = element_text(size = 15, face = "bold"), 
            axis.title.x = element_text(size = 15, face = "bold"), 
            axis.text.x = element_text(size = 15), #vjust = 1, hjust = 1
            legend.title = element_text(size = 15, face = "bold"),
            legend.position = "top",
            legend.title.align = 0.5) + 
      scale_x_discrete(labels = c("Meaningfulness", "Personal Desirability", "Sentiment", "Movie Review", "Movie Integral")) +
      scale_fill_manual(
        name = "Question Type", 
        labels = c("Meaningfulness", "Personal Desirability", "Sentiment", "Movie Review", "Movie Integral"),
        values = c("#56B4E9", "#009E73", "darkorange", "grey", "gold"),
        guide = guide_legend(title.position = "top")
      ) 
    
    
    return(corr_box_plot)
}


PlotReliability <- function(dat_long, n_ss, cor_results) {
    "
    What: plots reliability box plots for various question types; calls Get_reliability() and Reliability_plotter() 
    Inputs: data_long, n_after_exclusions, corr_results
    Output: reliability box plots 
    "
    
    # dat_long <- data_long
    # n_ss <- n_after_exclusions
    # cor_results <- corr_results
    
    # Define variables 
    avg_movie_reviews <- cor_results$avg_movie_reviews
    avg_meaning_scores <- cor_results$avg_meaning_scores
    avg_pd_scores <- cor_results$avg_pd_scores
    avg_sentiment_scores <- cor_results$avg_sentiment_scores
    avg_movie_integrals <- cor_results$avg_movie_integrals
    corr_meaning <- cor_results$corr_meaning
    corr_pd <- cor_results$corr_pd
    corr_sentiment <- cor_results$corr_sentiment
    corr_integral <- cor_results$corr_integral
    
    # Get summaries for movie reviews, meaningfulness & personal desirability ratings, and sentiment scores 
    summary_movie <- Get_reliability(movie_integral, dat_long, "movie", n_ss, cor_results)[2][[1]] 
    summary_meaning <- Get_reliability(movie_integral, dat_long, "meaningfulness", n_ss, cor_results, summary_movie)[[2]] 
    summary_pd <- Get_reliability(movie_integral, dat_long, "personal_desirability", n_ss, cor_results, summary_movie)[[2]]
    summary_sentiment <- Get_reliability(movie_integral, dat_long, "word", n_ss, cor_results, summary_movie)[[2]]
    summary_integral <- Get_reliability(movie_integral, dat_long, "integral", n_ss, cor_results, summary_movie)[[2]]
    
    # Create correlation data frame for plotting 
    cor_movie <- Get_reliability(movie_integral, dat_long, "movie", n_ss, cor_results)[1][[1]]
    cor_meaning <- Get_reliability(movie_integral, dat_long, "meaningfulness", n_ss, cor_results, summary_movie)[1][[1]]
    cor_pd <- Get_reliability(movie_integral, dat_long, "personal_desirability", n_ss, cor_results, summary_movie)[1][[1]]
    cor_sentiment <- Get_reliability(movie_integral, dat_long, "word", n_ss, cor_results, summary_movie)[1][[1]]
    cor_integral <- Get_reliability(movie_integral, dat_long, "integral", n_ss, cor_results, summary_movie)[1][[1]]
    corr_df <- data.frame(cor_meaning, cor_pd, cor_sentiment, cor_movie, cor_integral) #combine correlation results into a data frame
    corr_df_long <- gather(corr_df, key = question_type, value = correlations, cor_meaning, cor_pd, cor_sentiment, cor_movie, cor_integral) #organize df
    
    # Make box plot from Reliability_plotter
    corr_plot <- Reliability_plotter(corr_df_long, summary_meaning, summary_pd, summary_sentiment, summary_integral) 
    
    # Define empty lists for Wilcoxon tests 
    wilcoxon_test <- c()
    p_value_stars <- c() 
    
    # Loop through question type to perform one-sided Wilcoxon tests, comparing each to a null distribution
    print("One-sided Wilcoxon tests (vs null distribution): --------------------------------------------------------------------------------------")
    for(i in unique(corr_df_long$question_type)) {
      print(paste0(i, " --------------------------------------------------------------------------------------"))
      wilcoxon_test[[i]] <- wilcox.test(corr_df_long[corr_df_long$question_type == i, ]$correlations, 
                                        y = NULL, alternative = "greater", 
                                        conf.int = TRUE, data = corr_df_long)
      p_value_stars[i] <- stars.pval(wilcoxon_test[[i]]$"p.value") #get stars
      print(wilcoxon_test[[i]]) 
    }
    
    # Add to the plot: r values and stars indicating significance 
    corr_box_plot <- corr_plot + 
      
      # Correlations 
      ggplot2::annotate("text", x = 1, y = (max(cor_meaning)) + 0.15, label = paste0("Mean r = ", round(mean(cor_meaning), 3))) + 
      ggplot2::annotate("text", x = 2, y = (max(cor_pd)) + 0.15, label = paste0("Mean r = ", round(mean(cor_pd), 3))) + 
      ggplot2::annotate("text", x = 3, y = (max(cor_sentiment)) + 0.15, label = paste0("Mean r = ", round(mean(cor_sentiment), 3))) +
      ggplot2::annotate("text", x = 4, y = (max(cor_movie)) + 0.15, label = paste0("Mean r = ", round(mean(cor_movie), 3))) +
      ggplot2::annotate("text", x = 5, y = (max(cor_integral)) + 0.15, label = paste0("Mean r = ", round(mean(cor_integral), 3))) +
      
      # Stars 
      ggplot2::annotate("text", x = 1, y = (max(cor_meaning)) + 0.05, label = p_value_stars[[1]]) + 
      ggplot2::annotate("text", x = 2, y = (max(cor_pd)) + 0.05, label = p_value_stars[[2]]) + 
      ggplot2::annotate("text", x = 3, y = (max(cor_sentiment)) + 0.05, label = p_value_stars[[3]]) + 
      ggplot2::annotate("text", x = 4, y = (max(cor_movie)) + 0.05, label = p_value_stars[[4]]) + 
      ggplot2::annotate("text", x = 5, y = (max(cor_integral)) + 0.05, label = p_value_stars[[5]]) 
    
    
    return(corr_box_plot)
}


# GetHeatMap <- function(master_data) { 
#     "Get a heatmap of all the lifelines and movies relationships  
#     Input: master_data (contains all relationships between lifelines and movie lines) 
#     Output: similarity_map 
#     "
#     
#     # Get a heatmap 
#     master_matrix <- data.matrix(master_data) 
#     similarity_map <- heatmap(master_matrix) 
#     
#     return(similarity_map)
# }


##================================================================================================================
                                         ## MAIN SCRIPT ##
##================================================================================================================
"
If bin size changes, change: bin_shrink_ratio, scores_csv (the CSV name)
"

### (i) Define Global Variables

# (a) Define start and endpoints 
start_x <- 0
end_x <- 80   
start_happiness <- 0
end_happiness <- 100


# (b) (Stuff to change) Define window size and movie counts  
n_movie_matches <- n_after_exclusions #set desired number of movies to be grouped to each lifeline (n_after_exclusions from )
bin_shrink_ratio <- 10 #how much do you want to shrink the bin ratio by? From 80 to 8 is 10 

window_size <- (end_x/bin_shrink_ratio) + 1 #the +1 accounts for the fact that the graph starts at 0 (e.g., 0:80 is 81 points)
index_elements <- seq(start_x, end_x, bin_shrink_ratio) #index_elements <- start_x:(end_x/bin_shrink_ratio)
n_movies <- NULL #get from dim(movies_df)[2] below 


# (c) Define number of lifelines and their names 
n_plots <- 27 #number of lifelines 
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


# ## ================================= (0) Sanity Checks ===================================== ## 
# ### (i) Get Sample Movies 
# win_size <- window_size/3
# sample_movies <- Sanity_Check(win_size)
# dir.create("./r_sanity_checks")
# sample_files <- c(list.files(pattern = ".pdf"))
# file.move(sample_files, "./r_sanity_checks", overwrite = TRUE)


## ================================= (1) Get Data ===================================== ## 
### (i) Get Lifeline Coordinates
lifelines_df <- GetLifelinesDataFrame(index_elements, window_size, start_x, end_x, n_plots)

### (ii) Get Movie Reviews 
movie_reviews_df <- GetMovieReviews("rotten_tomatoes_movies.csv")

### (iii) Read in Movie Sentiment Arcs 
# Change depending on what sentiment analysis tool we are using 
# (e.g., get_sentiment() in R (a dictionary lookup) or a trained neural network in Google Colab: 
# https://colab.research.google.com/drive/1zpmSnwtg90p1fAXhMvjxtES-llQMIS9B?usp=sharing)

movies_df <- GetMoviesDataFrame("./scores_csvs/python_9_bins_scores (1).csv", movie_reviews_df)
n_movies <- dim(movies_df)[2]


## =============================== (2) Perform Analyses =============================== ## 
### (i) Perform Correlation Analysis 
master_df <- CorrelationAnalysis(lifelines_df, movies_df, n_plots, n_movies) #takes a few minutes
#"50 or more warnings" pop up because linear_low, linear_middle, and linear_high are all flat horizontal lines and have NA correlation values 


### (ii) Get Best Match Per Lifeline (Mutual Attraction)
match_df <- MatchMovieLineToLifeline(index_elements, master_df, lifelines_df, movies_df, 
                                     movie_reviews_df, n_plots, plot_names, start_x, end_x, 
                                     start_happiness, end_happiness, n_movie_matches)
dir.create("./matched_plots")
matched_files <- c(list.files(pattern = "_match.pdf"))
file.move(matched_files, "./matched_plots", overwrite = TRUE)


### (iii) Get Integral Correlations    
# Get the average of the integrals from every movieline in the 24 movie categories (matched to the 24 lifelines) 

# Note: We are excluding the 3 functions with constant values (linear_low, linear_middle, and linear_high) 
# because they cannot be correlated with anything 
# Also correlates movie arc integrals with participant meaningfulness and personal desirability ratings)

movie_integrals <- GetIntegralCorrelations(match_df, master_df, lifelines_df, movies_df, 
                                           n_plots, n_movie_matches, plot_names, 
                                           start_x, end_x, bin_shrink_ratio, data_long, data_plot_long)


### (iv) Get Correlation Between Participant Ratings and Movie Reviews 
corr_results <- GetCorrelations(movie_integrals, n_movie_matches, movie_reviews_df, match_df, 
                                data_plot_long, data_long, n_plots, plot_names)


## =================================== (3) Plot Data =================================== ## 
### (i) Plot Data  

# Participant Ratings and Movie Review Scatterplots (Old)  
PlotScatterplots(data_long, n_after_exclusions, corr_results)


# Reliability Box Plots 
pdf("box_corrplot.pdf", width = 15, height = 7) 
  PlotReliability(data_long, n_after_exclusions, corr_results) 
dev.off()


# similarity_map <- GetHeatMap(master_df)


## ================================= (4) Move Files ===================================== ## 
### (i) Move Files  

# Matched Plots 
dir.create("./corr_plots")
corr_files <- c(list.files(pattern = "_corrplot.pdf"))
file.move(corr_files, "./corr_plots", overwrite = TRUE)

# Sentiment Scores CSVs  
dir.create("./scores_csvs")
csv_files <- c(list.files(pattern = "*_scores.csv"))
file.move(csv_files, "./scores_csvs", overwrite = TRUE)




               