MakeGroupedBarPlot <- function(data_plot_long) {
    "
    Plot the grouped bar graph in order of ascending meaningfulness scores
    Input: data_plot_long
    Output: grouped_bar_plot (the grouped bar graph)
    "

    grouped_bar_plot <- ggplot(data_plot_long, aes(x = plot_names, y = score, fill = question_type)) +
        geom_bar(position = "dodge", stat = "identity") +
        geom_errorbar(aes(ymin = score - sd, ymax = score + sd), width = .2,
                      position = position_dodge(.9)) +
        ggtitle("Summarizing the Meaningfulness and Desirability of Different Life Trajectories") +
        xlab("Lifeline Plots") +
        ylab("Mean Rating") +
        scale_y_continuous(breaks = seq(0, 80, 40)) +
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
            breaks = c("meaning_score_avg", "pd_score_avg"),
            labels = c("Meaningfulness", "Personal Desirability"),
            values = c("#800000", "#3c7ea3"),
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

    png(file = paste0("NA_plot.png", ""))
    sapply("", Plotter_2)
    dev.off()

    # Assemble images in the order of data_plot_long$plot_names[1:27]
    plot_images <- axis_canvas(LifelinesPlot, axis = 'x')

    for (i in 1:27) {
        plot_images <- plot_images + draw_image(paste0(data_plot_long$plot_names[i], "_plot.png"), x = i - 0.5)
    }

    return(plot_images)
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


TopicModeling <- function(dat_long, n_plots, plot_names) {
    "
    Clean words, then plot topics models.
    Input: data_long, n_plots, plot_names
    Output: topic model word clouds
    "

    # 1. Topic Modeling

    # Create list of all participant words categorized by lifelines
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
    #k_topics <- FindTopicsNumber(words_tdm, topics = 2:n_plots, metrics = c("Arun2010", "CaoJuan2009", "Griffiths2004", "Deveaud2014"), control = list(seed = 1))
    #FindTopicsNumber_plot(k_topics)

    # Fit topic model
    words_k <- 3 #although the metrics suggest using 5-6, 3 is easier to interpret
    words_lda <- LDA(words_tdm, k = words_k, control = list(seed = 1)) #we use the simple EM estimation (another alternative would be Gibbs sampling)
    words_lda

    words_prob <- posterior(words_lda)
    words_prob_terms <- as.data.frame(t(words_prob$terms))
    round(head(words_prob_terms, 10), 4) #probabilistic assignments of words to clusters
    terms(words_lda, 5) #top 5 terms in each topic
    topics(words_lda) #assignments of lifelines to each topic

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
    op <- par(mfrow = c(1, 3), mar = c(3, 0, 2, 0))
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
        # theme(plot.title = element_text(color = "black", size=31, face = "bold", hjust = 0.5),
        # text = element_text(color = "black", size=25),
        # axis.title.y = element_text(color = "black", size=25, face = "bold"),
        # axis.title.x = element_text(color = "black", size=25, face = "bold"))
    }


    # Using topics(words_lda), automatically assign plots to their respective topic model
    # Define blank plot and universal starting point
    png("NA_plot.png") #used for when rownames(words_df_i) runs out when everything is automated
    dev.off()
    start <- -0.5005
    image_size <- 0.5

    # Frequency Plot 1
    words_df_1 <- as.data.frame(topics(words_lda)[as.data.frame(topics(words_lda)) == 1])
    end_1 <- (sort(freq_df[, 1], decreasing = TRUE)[1]) - 0.5005
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
    end_2 <- (sort(freq_df[, 2], decreasing = TRUE)[1]) - 0.5005
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
    end_3 <- (sort(freq_df[, 3], decreasing = TRUE)[1]) - 0.5005
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

    ggsave("topic_model_freq_bar.pdf", arrange_topics, height = 6.5, width = 18)
    # plot_4 <- ggdraw(insert_xaxis_grob(freq_df_list[[4]], plots_by_topic_4, position = "top"))
    # plot_5 <- ggdraw(insert_xaxis_grob(freq_df_list[[5]], plots_by_topic_5, position = "top"))
    # ggsave("topic_model_freq_bar.pdf", arrangeGrob(plot_1, plot_2, plot_3, plot_4, plot_5), height = 9, width = 8)

}

CV_plotter <- function(results_df, x_order, results_order, ques_type, x_labels, random_data) {
    "
    What this function does: creates a grouped box plot of the cross-validated prediction results
    Inputs: results_df, x_order, results_order, ques_type, x_labels, sum_meaning, sum_pd
    Output: a boxplot of participant rating predictions with either principal components or predictors
    "

    results_df[results_df['question_type'] == 'meaningfulness_results', 'question_type'] = "Meaningfulness"
    results_df[results_df['question_type'] == 'personal_desirability_results', 'question_type'] = "Personal Desirability"

    grouped_box_plot <- ggplot(data = results_df, aes(x = x_order, y = results_order, fill = question_type, color = question_type)) +
        scale_colour_manual(values = c("#3c7ea3", "#800000")) +
        scale_x_discrete() +
        stat_summary(fun = absmean, geom = "point", shape = 20, size = 5, aes(group = question_type, color=question_type), position = position_dodge(.75)) +
        stat_summary(fun.data = absse, geom = "errorbar", size=1.2, aes(group = question_type, width=0.5, color=question_type), position = position_dodge(.75)) +
        ggtitle(paste0("Meaningfulness and Desirability Predictions with ", x_labels)) +
        xlab(x_labels) +
        ylab("Prediction Performance\n(Cross Validated Pearson's r)") +
        scale_y_continuous(breaks = round(seq(-1, 1, by = 0.2), 1)) +
        scale_fill_manual(
            name = "question_type",
            values = c("#4b9ecc", "#006b4e"),
            guide = guide_legend(title.position = "top")) +
        geom_hline(yintercept = absmean(random_data$random)) +
        ggplot2::annotate("rect", xmin = -Inf, xmax = Inf, ymin = absse(random_data$random)$ymin,
                          ymax = absse(random_data$random)$ymax, fill = "black", alpha = .2, color = NA) +
        theme_bw() +
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

    return(grouped_box_plot)
}