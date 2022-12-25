pacman::p_load('qdapDictionaries')

CrossValidationAnalysis <- function(dat, fold_amount = 10,
                                    n_reps = 1, load_results = FALSE, dep_var = 'hiring_likelihood') {
    pacman::p_unload('hash')
    pacman::p_load('hash')
    print("Running cross validation...")
    dir.create('cv_results', showWarnings = FALSE)

    predictors_old <- c("embeddings", "interestingness", "sentiment_score", "max", "min", "end_value", "start_value", "number_peaks", "number_valleys", "number_extrema", "integral",
                        "d1_avg_unweight", "d1_avg_weight_prime", "d1_avg_weight_asc", "d1_avg_weight_des", "d1_avg_weight_end",
                        "d2_avg_unweight", "d2_avg_weight_prime", "d2_avg_weight_asc", "d2_avg_weight_des", "d2_avg_weight_end")
    predictors <- c("Embeddings", "Interestingness", "Sentiment Score", "Maximum", "Minimum", "End Value", "Start Value", "Num. of Peaks", "Num. of Valleys", "Num. of Extrema", "Integral",
                    "1st Deriv.", "1st Deriv. Early", "1st Deriv. Asc.", "1st Deriv. Desc.", "1st Deriv. End",
                    "2nd Deriv.", "2nd Deriv. Early", "2nd Deriv. Asc.", "2nd Deriv. Desc.", "2nd Deriv. End")

    if (!('Integral' %in% colnames(dat))) {
        setnames(dat, old = predictors_old, new = predictors)
    }

    predictors <- c('random', predictors)
    dv_results <- hash()
    if (load_results) {
        for (dv in dep_var) {
            results_e <- read.csv(paste0('cv_results/cv_', fold_amount, '_', n_reps, '_', dv, '_results.csv'), row.names = 1)
            errors_e <- read.csv(paste0('cv_results/cv_', fold_amount, '_', n_reps, '_', dv, '_errors.csv'), row.names = 1)

            dv_results[[dv]] <- list(results_e, errors_e)
        }
    } else {
        for (dv in dep_var) {
            results_list <- list()
            errors_list <- list()

            for (p in 1:n_reps) { # Replicate n_reps times to make sure our model is robust
                print(paste("Running iteration ", p, "-", dv, "..."))
                set.seed(p)
                dat$random <- standardize(sample(100, size = nrow(dat), replace = TRUE))

                results_e <- data.frame(matrix(NA, nrow = length(predictors), ncol = fold_amount))
                errors_e <- data.frame(matrix(NA, nrow = length(predictors), ncol = fold_amount))
                rownames(results_e) <- predictors
                rownames(errors_e) <- predictors

                folds <- createFolds(factor(rep(0, dim(dat)[1])), k = fold_amount, list = TRUE) #factor(dat[, 'subject'])
                all_indices <- createFolds(factor(rep(0, dim(dat)[1])), k = 1, list = TRUE)[[1]]

                for (i in 1:length(predictors)) {
                    cat("...")
                    for (j in 1:fold_amount) {  # Number of folds
                        ss_results <- c()
                        truths <- c()

                        trainIndeces <- c(setdiff(c(all_indices), c(folds[[j]])))

                        # Fit model on subset of train data
                        fitpc <- lm(get(dv) ~ get(predictors[i]), data = dat, subset = trainIndeces)

                        for (k in 1:length(folds[[j]])) {  # Predict each participant in fold
                            testIndeces <- folds[[j]][k]
                            ss_results <- c(ss_results, predict(fitpc, dat)[testIndeces])
                            truths <- c(truths, dat[testIndeces, dv])
                        }

                        errors_e[i, j] <- RMSE(truths, ss_results)
                        results_e[i, j] <- cor(truths, ss_results)
                    }
                }

                results_list <- append(results_list, list(results_e))
                errors_list <- append(errors_list, list(errors_e))
            }

            # Collect r's of each run
            results_all <- data.frame("1" = results_list[[1]])
            for (i in 2:n_reps) {
                col_name <- i
                results_all <- cbind(results_all, data.frame(col_name = results_list[[i]]))
            }

            names(results_all) <- (1:n_reps * fold_amount)

            # Collect RMSE's of each run
            errors_all <- data.frame("1" = errors_list[[1]])
            for (i in 2:n_reps) {
                col_name <- i
                errors_all <- cbind(errors_all, data.frame(col_name = errors_list[[i]]))
            }

            names(errors_all) <- (1:n_reps * fold_amount)

            results_e <- results_all
            errors_e <- errors_all

            write.csv(errors_e, paste0('cv_results/cv_', fold_amount, '_', n_reps, '_', dv, '_errors.csv'))
            write.csv(results_e, paste0('cv_results/cv_', fold_amount, '_', n_reps, '_', dv, '_results.csv'))

            dv_results[[dv]] <- list(results_e, errors_e)
        }
    }

    # ----------------------------- Now that we have the results, sort the data and plot -----------------------------

    predictors <- predictors[!predictors %in% 'random']
    if (length(dep_var) == 1) {  # Studies with single question
        # Reorder predictors according to their mean r
        t_results_e <- as.data.frame(t(results_e))
        random_data <- t_results_e["random"]
        t_results_e <- t_results_e[, !names(t_results_e) %in% c("random")]
        colnames(t_results_e) <- predictors
        results_e_long <- gather(t_results_e, key = predictors, value = predictors_results, colnames(t_results_e))
        results_e_long["new_order"] <- with(results_e_long, reorder(predictors, predictors_results, mean))

        means_h <- aggregate(results_e_long$predictors_results, list(results_e_long$predictors), FUN = mean)
        ordered_pred <- means_h[order(-means_h$x),]
        cat("\n")
        for (i in ordered_pred$Group.1) {
            print(sprintf("%-50s: %f *-*-*-*-* Mean RMSE:  %f", paste0('Mean predictor result, ', i),
                          means_h[means_h$Group.1 == i, 'x'], mean(as.numeric(errors_e[i,]))))
        }

        predictors_results_ordered <- data.frame(predictors_order = results_e_long$new_order,
                                                 results = results_e_long$predictors_results)
        predictors_results_long <- gather(predictors_results_ordered, key = question_type, value = results, results)

        # Make boxplot from CV_plotter function
        predictors_plot <- CV_plotter(results_df = predictors_results_long, x_order = predictors_results_long$predictors_order,
                                      results_order = predictors_results_long$results, ques_type = predictors_results_long$question_type,
                                      x_labels = "Predictors", random_data = random_data)

        # Get the labels
        x_labs <- ggplot_build(predictors_plot)$layout$panel_params[[1]]$
            x$
            get_labels()

        # Wilcoxon or t-tests
        wilcox_test_wt_e <- c()
        p_value_stars_e <- c()
        print("--------------------------------------------------------------------------------------")
        for (i in x_labs) {
            print(paste0(i, " --------------------------------------------------------------------------------------"))
            vt <- var.test(as.numeric(errors_e[i,]), as.numeric(errors_e['random',]))

            # Shapiro test first -> if it's normal, use t-test, if not, use wilcox test
            if ((shapiro.test(as.numeric(errors_e[i,]))$p.value < 0.05) ||
                (shapiro.test(as.numeric(errors_e['random',]))$p.value < 0.05)) {
                wilcox_test_wt_e[[i]] <- wilcox.test(x = as.numeric(errors_e[i,]),
                                                     y = as.numeric(errors_e['random',]), alternative = "less")
            } else {
                wilcox_test_wt_e[[i]] <- t.test(x = as.numeric(errors_e[i,]),
                                                y = as.numeric(errors_e['random',]), alternative = "less",
                                                var.equal = vt$p.value > 0.05)
            }

            p_value_stars_e[i] <- stars.pval(wilcox_test_wt_e[[i]]$"p.value") #get stars
            print(wilcox_test_wt_e[[i]])
        }

        # Define heights of annotations
        bottom_x <- 1 #x value for bottom stars
        bottom_y <- -0.1 #y value for bottom stars

        rh <- results_e_long[!is.na(results_e_long$predictors_results),]
        means <- aggregate(rh$predictors_results, list(rh$predictors), FUN = mean)

        # Add to the plot: stars indicating significance
        for (i in 1:21) {
            predictors_plot <- predictors_plot + ggplot2::annotate("text", x = bottom_x + i - 1,
                                                                   y = bottom_y, size = 8,
                                                                   label = p_value_stars_e[[x_labs[i]]])
        }

        print(predictors_plot)
        return(list(predictors_plot, mean(as.matrix(errors_e)), mean(as.matrix(results_e))))
    } else {  # Studies with two questions
        results_prime <- data.frame()
        for (dv in dep_var) {
            # Reorder predictors according to their significance
            t_results <- data.frame(t(dv_results[[dv]][[1]]))
            random_data <- t_results["random"]
            t_results <- t_results[, !colnames(t_results) %in% c("random")]

            colnames(t_results) <- predictors[!predictors %in% 'random']
            results_long <- gather(t_results, key = predictors,
                                   value = predictors_results, colnames(t_results))
            new_order <- with(results_long, reorder(predictors, predictors_results, mean))
            results_long[paste0(dv, "_new_order")] <- new_order
            if (dv != 'personal_desirability') {
                results_prime <- results_long
            }

            if (dv == 'personal_desirability') {  # Match order
                results_long <- results_long[order(match(results_long[, "personal_desirability_new_order"],
                                                         results_prime[, paste0(dep_var[[1]], "_new_order")])),]

                ### Combine results to use at plotting
                predictors_results_ordered <- data.frame(predictors_order = results_prime[, paste0(dep_var[[1]], '_new_order')])
                predictors_results_ordered[, paste0(dep_var[[1]], '_results')] <- results_prime$predictors_results
                predictors_results_ordered[, paste0(dep_var[[2]], '_results')] <- results_long$predictors_results

                predictors_results_long <- gather(predictors_results_ordered, key = question_type, value = results,
                                                  paste0(dep_var[[1]], '_results'),
                                                  paste0(dep_var[[2]], '_results'))
            }

            ### Print mean results and errors
            means_h <- aggregate(results_long$predictors_results,
                                 list(results_long$predictors), FUN = mean)
            ordered_pred <- means_h[order(-means_h$x),]
            cat("\n")
            print(paste("*********", dv, "*********"))
            for (i in ordered_pred$Group.1) {
                print(sprintf("%-50s: %f *-*-*-*-* Mean RMSE:  %f", paste0('Mean predictor result, ', i),
                              means_h[means_h$Group.1 == i, 'x'], mean(as.numeric(errors_e[i,]))))
            }
        }

        ############################ Plotting ############################
        predictors_plot <- CV_plotter(predictors_results_long, predictors_results_long$predictors_order,
                                      predictors_results_long$results, predictors_results_long$question_type,
                                      "Predictors", random_data)

        x_labs <- ggplot_build(predictors_plot)$layout$panel_params[[1]]$
            x$
            get_labels()

        # Wilcoxon or t-tests
        satisfaction_bottom_x <- 0.8 #x value for bottom stars
        satisfaction_bottom_y <- -0.08 #y value for bottom stars
        pd_bottom_x <- 1.2 #x value for bottom stars
        pd_bottom_y <- satisfaction_bottom_y - 0.05 #y value for bottom stars

        wilcox_test_wts <- hash()
        p_value_stars <- hash()
        for (dv in dep_var) {
            print(paste0("------------------------------", dv, "------------------------------"))
            for (label in x_labs) {
                print(paste0(label, " --------------------------------------------------------------------------------------"))
                vt <- var.test(as.numeric(dv_results[[dv]][[2]][label,]), as.numeric(dv_results[[dv]][[2]]['random',]))

                # Shapiro test first -> if it's normal, use t-test, if not, use wilcox test
                if (shapiro.test(as.numeric(dv_results[[dv]][[2]][label,]))$p.value < 0.05 ||
                    shapiro.test(as.numeric(dv_results[[dv]][[2]]['random',]))$p.value < 0.05) {
                    wilcox_test_wts[[dv]][[label]] <- wilcox.test(x = as.numeric(dv_results[[dv]][[2]][label,]),
                                                             y = as.numeric(dv_results[[dv]][[2]]['random',]), alternative = "less")
                } else {
                    wilcox_test_wts[[dv]][[label]] <- t.test(x = as.numeric(dv_results[[dv]][[2]][label,]),
                                                        y = as.numeric(dv_results[[dv]][[2]]['random',]), alternative = "less",
                                                        var.equal = vt$p.value > 0.05)
                }

                p_value_stars[[dv]][[label]] <- stars.pval(wilcox_test_wts[[dv]][[label]]$"p.value") #get stars
                print(wilcox_test_wts[[dv]][[label]])
            }

            for (i in 1:21) {
                # Add stars to the plot
                if (dv == 'personal_desirability') {
                    predictors_plot <- predictors_plot + ggplot2::annotate("text", x = pd_bottom_x + i - 1,
                                                                           y = pd_bottom_y, size = 8,
                                                                           label = p_value_stars[[dv]][[x_labs[i]]])
                } else {
                    predictors_plot <- predictors_plot + ggplot2::annotate("text", x = satisfaction_bottom_x + i - 1,
                                                                           y = satisfaction_bottom_y, size = 8,
                                                                           label = p_value_stars[[dv]][[x_labs[i]]])
                }
            }
        }


        print(predictors_plot)
        return(list(predictors_plot, mean(as.matrix(errors_e)), mean(as.matrix(results_e))))
    }
}

# Return absolute value of the mean
get_mean <- function(x) {
    return(mean(na.omit(x)))
}

# mean-se is 1.96 * std err (https://stulp.gmw.rug.nl/ggplotworkshop/comparinggroupstatistics.html)
get_se <- function(x) {
    return(mean_se(na.omit(x), 1.96))
}

CleanWord <- function(word) {
    word <- word(tolower(word), 1)
    word <- gsub("[^a-z]", "", word) #get rid of numbers and special characters, leaving only letters a-z
    return(word)
}

# Check if input is actually an English word
is.word <- function(word) {
    word <- CleanWord(word)
    return(word %in% GradyAugmented)
}


CalculateSentiment <- function(rword, model_type = 'sentimentr', is_sentence = FALSE) {
    if (!is_sentence) { rword <- CleanWord(rword) }
    #print(paste0("Cleaned word: ", rword))

    if (model_type == 'sentimentr') {
        return(sentiment_by(rword)$ave_sentiment)
    } else if (model_type == 'vader') {
        sent_vals <- vector(mode = "integer", length = length(rword))
        for (i in (1:length(rword))) { sent_vals[i] <- (as.numeric(get_vader(rword[[i]])[['compound']])) }
        return(sent_vals)
    } else if (model_type == 'ai') {
        return(sentiment_score(rword))
    }

}

MakeWordClouds <- function(data, n_plots, plot_names) {
    "
    Make word clouds and save them as individual images to be read in as files later.
    Input: data_long, n_plots, plot_names
    Output: individual word clouds; takes a long time because we are using ggsave, which saves
    super high-quality images necessary for producing small word clouds
    "

    set.seed(1)

    if (n_plots != 27) { # Directly experienced content
        print("Error: n_plots should be 27!")
        return()
    }

    plot_word_clouds <- function(data) {
        # Define word clouds
        LR <- ggplot(arrange(as.data.frame(Get_word_stats(data, n_plots)[[1]]), -Freq), aes(label = Var1, size = Freq, color = Freq)) +
            geom_text_wordcloud_area(shape = "square", rm_outside=TRUE) +
            scale_size_area(max_size = 30) +
            theme_minimal() +
            scale_color_gradient(low = "mistyrose1", high = "firebrick3")
        LF <- ggplot(arrange(as.data.frame(Get_word_stats(data, n_plots)[[2]]), -Freq), aes(label = Var1, size = Freq, color = Freq)) +
            geom_text_wordcloud_area(shape = "square", rm_outside=TRUE) +
            scale_size_area(max_size = 30) +
            theme_minimal() +
            scale_color_gradient(low = "mistyrose1", high = "firebrick3")
        LL <- ggplot(arrange(as.data.frame(Get_word_stats(data, n_plots)[[3]]), -Freq), aes(label = Var1, size = Freq, color = Freq)) +
            geom_text_wordcloud_area(shape = "square", rm_outside=TRUE) +
            scale_size_area(max_size = 30) +
            theme_minimal() +
            scale_color_gradient(low = "mistyrose1", high = "firebrick3")
        LM <- ggplot(arrange(as.data.frame(Get_word_stats(data, n_plots)[[4]]), -Freq), aes(label = Var1, size = Freq, color = Freq)) +
            geom_text_wordcloud_area(shape = "square", rm_outside=TRUE) +
            scale_size_area(max_size = 30) +
            theme_minimal() +
            scale_color_gradient(low = "mistyrose1", high = "firebrick3")
        LH <- ggplot(arrange(as.data.frame(Get_word_stats(data, n_plots)[[5]]), -Freq), aes(label = Var1, size = Freq, color = Freq)) +
            geom_text_wordcloud_area(shape = "square", rm_outside=TRUE) +
            scale_size_area(max_size = 30) +
            theme_minimal() +
            scale_color_gradient(low = "mistyrose1", high = "firebrick3")
        ERCV <- ggplot(arrange(as.data.frame(Get_word_stats(data, n_plots)[[6]]), -Freq), aes(label = Var1, size = Freq, color = Freq)) +
            geom_text_wordcloud_area(shape = "square", rm_outside=TRUE) +
            scale_size_area(max_size = 30) +
            theme_minimal() +
            scale_color_gradient(low = "mistyrose1", high = "firebrick3")
        EFCV <- ggplot(arrange(as.data.frame(Get_word_stats(data, n_plots)[[7]]), -Freq), aes(label = Var1, size = Freq, color = Freq)) +
            geom_text_wordcloud_area(shape = "square", rm_outside=TRUE) +
            scale_size_area(max_size = 30) +
            theme_minimal() +
            scale_color_gradient(low = "mistyrose1", high = "firebrick3")
        ERCC <- ggplot(arrange(as.data.frame(Get_word_stats(data, n_plots)[[8]]), -Freq), aes(label = Var1, size = Freq, color = Freq)) +
            geom_text_wordcloud_area(shape = "square", rm_outside=TRUE) +
            scale_size_area(max_size = 30) +
            theme_minimal() +
            scale_color_gradient(low = "mistyrose1", high = "firebrick3")
        EFCC <- ggplot(arrange(as.data.frame(Get_word_stats(data, n_plots)[[9]]), -Freq), aes(label = Var1, size = Freq, color = Freq)) +
            geom_text_wordcloud_area(shape = "square", rm_outside=TRUE) +
            scale_size_area(max_size = 30) +
            theme_minimal() +
            scale_color_gradient(low = "mistyrose1", high = "firebrick3")
        SFR_FULL <- ggplot(arrange(as.data.frame(Get_word_stats(data, n_plots)[[10]]), -Freq), aes(label = Var1, size = Freq, color = Freq)) +
            geom_text_wordcloud_area(shape = "square", rm_outside=TRUE) +
            scale_size_area(max_size = 30) +
            theme_minimal() +
            scale_color_gradient(low = "mistyrose1", high = "firebrick3")
        SFR_PAR <- ggplot(arrange(as.data.frame(Get_word_stats(data, n_plots)[[11]]), -Freq), aes(label = Var1, size = Freq, color = Freq)) +
            geom_text_wordcloud_area(shape = "square", rm_outside=TRUE) +
            scale_size_area(max_size = 30) +
            theme_minimal() +
            scale_color_gradient(low = "mistyrose1", high = "firebrick3")
        SRF_FULL <- ggplot(arrange(as.data.frame(Get_word_stats(data, n_plots)[[12]]), -Freq), aes(label = Var1, size = Freq, color = Freq)) +
            geom_text_wordcloud_area(shape = "square", rm_outside=TRUE) +
            scale_size_area(max_size = 30) +
            theme_minimal() +
            scale_color_gradient(low = "mistyrose1", high = "firebrick3")
        SRF_PAR <- ggplot(arrange(as.data.frame(Get_word_stats(data, n_plots)[[13]]), -Freq), aes(label = Var1, size = Freq, color = Freq)) +
            geom_text_wordcloud_area(shape = "square", rm_outside=TRUE) +
            scale_size_area(max_size = 30) +
            theme_minimal() +
            scale_color_gradient(low = "mistyrose1", high = "firebrick3")
        SRFR_FULL <- ggplot(arrange(as.data.frame(Get_word_stats(data, n_plots)[[14]]), -Freq), aes(label = Var1, size = Freq, color = Freq)) +
            geom_text_wordcloud_area(shape = "square", rm_outside=TRUE) +
            scale_size_area(max_size = 30) +
            theme_minimal() +
            scale_color_gradient(low = "mistyrose1", high = "firebrick3")
        SRFR_PAR <- ggplot(arrange(as.data.frame(Get_word_stats(data, n_plots)[[15]]), -Freq), aes(label = Var1, size = Freq, color = Freq)) +
            geom_text_wordcloud_area(shape = "square", rm_outside=TRUE) +
            scale_size_area(max_size = 30) +
            theme_minimal() +
            scale_color_gradient(low = "mistyrose1", high = "firebrick3")
        SFRF_FULL <- ggplot(arrange(as.data.frame(Get_word_stats(data, n_plots)[[16]]), -Freq), aes(label = Var1, size = Freq, color = Freq)) +
            geom_text_wordcloud_area(shape = "square", rm_outside=TRUE) +
            scale_size_area(max_size = 30) +
            theme_minimal() +
            scale_color_gradient(low = "mistyrose1", high = "firebrick3")
        SFRF_PAR <- ggplot(arrange(as.data.frame(Get_word_stats(data, n_plots)[[17]]), -Freq), aes(label = Var1, size = Freq, color = Freq)) +
            geom_text_wordcloud_area(shape = "square", rm_outside=TRUE) +
            scale_size_area(max_size = 30) +
            theme_minimal() +
            scale_color_gradient(low = "mistyrose1", high = "firebrick3")
        SFRFR <- ggplot(arrange(as.data.frame(Get_word_stats(data, n_plots)[[18]]), -Freq), aes(label = Var1, size = Freq, color = Freq)) +
            geom_text_wordcloud_area(shape = "square", rm_outside=TRUE) +
            scale_size_area(max_size = 30) +
            theme_minimal() +
            scale_color_gradient(low = "mistyrose1", high = "firebrick3")
        SRFRF <- ggplot(arrange(as.data.frame(Get_word_stats(data, n_plots)[[19]]), -Freq), aes(label = Var1, size = Freq, color = Freq)) +
            geom_text_wordcloud_area(shape = "square", rm_outside=TRUE) +
            scale_size_area(max_size = 30) +
            theme_minimal() +
            scale_color_gradient(low = "mistyrose1", high = "firebrick3")
        LOG_RISE <- ggplot(arrange(as.data.frame(Get_word_stats(data, n_plots)[[20]]), -Freq), aes(label = Var1, size = Freq, color = Freq)) +
            geom_text_wordcloud_area(shape = "square", rm_outside=TRUE) +
            scale_size_area(max_size = 30) +
            theme_minimal() +
            scale_color_gradient(low = "mistyrose1", high = "firebrick3")
        LOG_FALL <- ggplot(arrange(as.data.frame(Get_word_stats(data, n_plots)[[21]]), -Freq), aes(label = Var1, size = Freq, color = Freq)) +
            geom_text_wordcloud_area(shape = "square", rm_outside=TRUE) +
            scale_size_area(max_size = 30) +
            theme_minimal() +
            scale_color_gradient(low = "mistyrose1", high = "firebrick3")
        POS_FULL <- ggplot(arrange(as.data.frame(Get_word_stats(data, n_plots)[[22]]), -Freq), aes(label = Var1, size = Freq, color = Freq)) +
            geom_text_wordcloud_area(shape = "square", rm_outside=TRUE) +
            scale_size_area(max_size = 30) +
            theme_minimal() +
            scale_color_gradient(low = "mistyrose1", high = "firebrick3")
        POS_PAR <- ggplot(arrange(as.data.frame(Get_word_stats(data, n_plots)[[23]]), -Freq), aes(label = Var1, size = Freq, color = Freq)) +
            geom_text_wordcloud_area(shape = "square", rm_outside=TRUE) +
            scale_size_area(max_size = 30) +
            theme_minimal() +
            scale_color_gradient(low = "mistyrose1", high = "firebrick3")
        NEG_FULL <- ggplot(arrange(as.data.frame(Get_word_stats(data, n_plots)[[24]]), -Freq), aes(label = Var1, size = Freq, color = Freq)) +
            geom_text_wordcloud_area(shape = "square", rm_outside=TRUE) +
            scale_size_area(max_size = 30) +
            theme_minimal() +
            scale_color_gradient(low = "mistyrose1", high = "firebrick3")
        NEG_PAR <- ggplot(arrange(as.data.frame(Get_word_stats(data, n_plots)[[25]]), -Freq), aes(label = Var1, size = Freq, color = Freq)) +
            geom_text_wordcloud_area(shape = "square", rm_outside=TRUE) +
            scale_size_area(max_size = 30) +
            theme_minimal() +
            scale_color_gradient(low = "mistyrose1", high = "firebrick3")
        LRSF <- ggplot(arrange(as.data.frame(Get_word_stats(data, n_plots)[[26]]), -Freq), aes(label = Var1, size = Freq, color = Freq)) +
            geom_text_wordcloud_area(shape = "square", rm_outside=TRUE) +
            scale_size_area(max_size = 30) +
            theme_minimal() +
            scale_color_gradient(low = "mistyrose1", high = "firebrick3")
        LRSFER <- ggplot(arrange(as.data.frame(Get_word_stats(data, n_plots)[[27]]), -Freq), aes(label = Var1, size = Freq, color = Freq)) +
            geom_text_wordcloud_area(shape = "square", rm_outside=TRUE) +
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

ArrangeWordClouds <- function(data_plot_long) {
    "Arrange word clouds and plot labels into a grid using ggplot.
    Input: none
    Output: a grid of word clouds (arranged in order of ascending meaningfulness scores)
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

        annotation_custom(s, xmin = -0.5, xmax = 0.5, ymin = 4, ymax = 6) +
        annotation_custom(t, xmin = 0.5, xmax = 1.5, ymin = 4, ymax = 6) +
        annotation_custom(u, xmin = 1.5, xmax = 2.5, ymin = 4, ymax = 6) +
        annotation_custom(v, xmin = 2.5, xmax = 3.5, ymin = 4, ymax = 6) +
        annotation_custom(w, xmin = 3.5, xmax = 4.5, ymin = 4, ymax = 6) +
        annotation_custom(x, xmin = 4.5, xmax = 5.5, ymin = 4, ymax = 6) +
        annotation_custom(y, xmin = 5.5, xmax = 6.5, ymin = 4, ymax = 6) +
        annotation_custom(z, xmin = 6.5, xmax = 7.5, ymin = 4, ymax = 6) +
        annotation_custom(aa, xmin = 7.5, xmax = 8.5, ymin = 4, ymax = 6) +

        annotation_custom(kk, xmin = -0.5, xmax = 0.5, ymin = 0, ymax = 2) +
        annotation_custom(ll, xmin = 0.5, xmax = 1.5, ymin = 0, ymax = 2) +
        annotation_custom(mm, xmin = 1.5, xmax = 2.5, ymin = 0, ymax = 2) +
        annotation_custom(nn, xmin = 2.5, xmax = 3.5, ymin = 0, ymax = 2) +
        annotation_custom(oo, xmin = 3.5, xmax = 4.5, ymin = 0, ymax = 2) +
        annotation_custom(pp, xmin = 4.5, xmax = 5.5, ymin = 0, ymax = 2) +
        annotation_custom(qq, xmin = 5.5, xmax = 6.5, ymin = 0, ymax = 2) +
        annotation_custom(rr, xmin = 6.5, xmax = 7.5, ymin = 0, ymax = 2) +
        annotation_custom(ss, xmin = 7.5, xmax = 8.5, ymin = 0, ymax = 2) +

        annotation_custom(j, xmin = -0.25, xmax = 0.25, ymin = 7, ymax = 8) +
        annotation_custom(k, xmin = 0.75, xmax = 1.25, ymin = 7, ymax = 8) +
        annotation_custom(l, xmin = 1.75, xmax = 2.25, ymin = 7, ymax = 8) +
        annotation_custom(m, xmin = 2.75, xmax = 3.25, ymin = 7, ymax = 8) +
        annotation_custom(n, xmin = 3.75, xmax = 4.25, ymin = 7, ymax = 8) +
        annotation_custom(o, xmin = 4.75, xmax = 5.25, ymin = 7, ymax = 8) +
        annotation_custom(p, xmin = 5.75, xmax = 6.25, ymin = 7, ymax = 8) +
        annotation_custom(q, xmin = 6.75, xmax = 7.25, ymin = 7, ymax = 8) +
        annotation_custom(r, xmin = 7.75, xmax = 8.25, ymin = 7, ymax = 8) +

        annotation_custom(bb, xmin = -0.25, xmax = 0.25, ymin = 3, ymax = 4) +
        annotation_custom(cc, xmin = 0.75, xmax = 1.25, ymin = 3, ymax = 4) +
        annotation_custom(dd, xmin = 1.75, xmax = 2.25, ymin = 3, ymax = 4) +
        annotation_custom(ee, xmin = 2.75, xmax = 3.25, ymin = 3, ymax = 4) +
        annotation_custom(ff, xmin = 3.75, xmax = 4.25, ymin = 3, ymax = 4) +
        annotation_custom(gg, xmin = 4.75, xmax = 5.25, ymin = 3, ymax = 4) +
        annotation_custom(hh, xmin = 5.75, xmax = 6.25, ymin = 3, ymax = 4) +
        annotation_custom(ii, xmin = 6.75, xmax = 7.25, ymin = 3, ymax = 4) +
        annotation_custom(jj, xmin = 7.75, xmax = 8.25, ymin = 3, ymax = 4) +

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

TopicModeling <- function(dat_long, n_plots, plot_names, study='satisfaction') {
    "
    Clean words, then plot topics models.
    Input: data_long, n_plots, plot_names
    Output: topic model word clouds
    "

    print("Running topic modelling...")

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
            scale_y_discrete(limits = rev(levels(freq_df_each$terms)))

        if(study == 'hiring' || study == 'lifelines') {
            x_ax_size = 15
        } else {
            x_ax_size = 25
        }

        freq_df_list[[i]] <- freq_df_list[[i]] +
            theme(axis.text = element_text(color = "black", size = 25),
                  axis.title.y = element_blank(),
                  axis.title.x = element_blank(), axis.text.x = element_text(size=x_ax_size))
    }

    # Using topics(words_lda), automatically assign plots to their respective topic model
    # Define blank plot and universal starting point
    #png("NA_plot.png") #used for when rownames(words_df_i) runs out when everything is automated
    #dev.off()
    start <- -0.5005
    image_size <- 0.5

    # Frequency Plot 1
    words_df_1 <- as.data.frame(topics(words_lda)[as.data.frame(topics(words_lda)) == 1])
    end_1 <- (sort(freq_df[, 1], decreasing = TRUE)[1]) + start
    spacing_1 <- (end_1 - start) / (length(rownames(words_df_1)) - 1)
    plots_by_topic_1 <- axis_canvas(freq_df_list[[1]], axis = 'x') +
        draw_image(paste0("./plots/analysis_plots/", rownames(words_df_1)[1], "_plot.png"), scale = image_size, x = start) +
        draw_image(paste0("./plots/analysis_plots/", rownames(words_df_1)[2], "_plot.png"), scale = image_size, x = start + spacing_1) +
        draw_image(paste0("./plots/analysis_plots/", rownames(words_df_1)[3], "_plot.png"), scale = image_size, x = start + 2 * spacing_1) +
        draw_image(paste0("./plots/analysis_plots/", rownames(words_df_1)[4], "_plot.png"), scale = image_size, x = start + 3 * spacing_1) +
        draw_image(paste0("./plots/analysis_plots/", rownames(words_df_1)[5], "_plot.png"), scale = image_size, x = start + 4 * spacing_1) +
        draw_image(paste0("./plots/analysis_plots/", rownames(words_df_1)[6], "_plot.png"), scale = image_size, x = start + 5 * spacing_1) +
        draw_image(paste0("./plots/analysis_plots/", rownames(words_df_1)[7], "_plot.png"), scale = image_size, x = start + 6 * spacing_1) +
        draw_image(paste0("./plots/analysis_plots/", rownames(words_df_1)[8], "_plot.png"), scale = image_size, x = start + 7 * spacing_1) +
        draw_image(paste0("./plots/analysis_plots/", rownames(words_df_1)[9], "_plot.png"), scale = image_size, x = start + 8 * spacing_1) +
        draw_image(paste0("./plots/analysis_plots/", rownames(words_df_1)[10], "_plot.png"), scale = image_size, x = start + 9 * spacing_1) +
        draw_image(paste0("./plots/analysis_plots/", rownames(words_df_1)[11], "_plot.png"), scale = image_size, x = start + 10 * spacing_1) +
        draw_image(paste0("./plots/analysis_plots/", rownames(words_df_1)[12], "_plot.png"), scale = image_size, x = start + 11 * spacing_1)

    # Frequency Plot 2
    words_df_2 <- as.data.frame(topics(words_lda)[as.data.frame(topics(words_lda)) == 2])
    end_2 <- (sort(freq_df[, 2], decreasing = TRUE)[1]) + start
    spacing_2 <- (end_2 - start) / (length(rownames(words_df_2)) - 1)
    plots_by_topic_2 <- axis_canvas(freq_df_list[[2]], axis = 'x') +
        draw_image(paste0("./plots/analysis_plots/", rownames(words_df_2)[1], "_plot.png"), scale = image_size, x = start) +
        draw_image(paste0("./plots/analysis_plots/", rownames(words_df_2)[2], "_plot.png"), scale = image_size, x = start + spacing_2) +
        draw_image(paste0("./plots/analysis_plots/", rownames(words_df_2)[3], "_plot.png"), scale = image_size, x = start + 2 * spacing_2) +
        draw_image(paste0("./plots/analysis_plots/", rownames(words_df_2)[4], "_plot.png"), scale = image_size, x = start + 3 * spacing_2) +
        draw_image(paste0("./plots/analysis_plots/", rownames(words_df_2)[5], "_plot.png"), scale = image_size, x = start + 4 * spacing_2) +
        draw_image(paste0("./plots/analysis_plots/", rownames(words_df_2)[6], "_plot.png"), scale = image_size, x = start + 5 * spacing_2) +
        draw_image(paste0("./plots/analysis_plots/", rownames(words_df_2)[7], "_plot.png"), scale = image_size, x = start + 6 * spacing_2) +
        draw_image(paste0("./plots/analysis_plots/", rownames(words_df_2)[8], "_plot.png"), scale = image_size, x = start + 7 * spacing_2) +
        draw_image(paste0("./plots/analysis_plots/", rownames(words_df_2)[9], "_plot.png"), scale = image_size, x = start + 8 * spacing_2) +
        draw_image(paste0("./plots/analysis_plots/", rownames(words_df_2)[10], "_plot.png"), scale = image_size, x = start + 9 * spacing_2) +
        draw_image(paste0("./plots/analysis_plots/", rownames(words_df_2)[11], "_plot.png"), scale = image_size, x = start + 10 * spacing_2) +
        draw_image(paste0("./plots/analysis_plots/", rownames(words_df_2)[12], "_plot.png"), scale = image_size, x = start + 11 * spacing_2)

    # Frequency Plot 3
    words_df_3 <- as.data.frame(topics(words_lda)[as.data.frame(topics(words_lda)) == 3])
    end_3 <- (sort(freq_df[, 3], decreasing = TRUE)[1]) + start
    spacing_3 <- (end_3 - start) / (length(rownames(words_df_3)) - 1)
    plots_by_topic_3 <- axis_canvas(freq_df_list[[3]], axis = 'x') +
        draw_image(paste0("./plots/analysis_plots/", rownames(words_df_3)[1], "_plot.png"), scale = image_size, x = start) +
        draw_image(paste0("./plots/analysis_plots/", rownames(words_df_3)[2], "_plot.png"), scale = image_size, x = start + spacing_3) +
        draw_image(paste0("./plots/analysis_plots/", rownames(words_df_3)[3], "_plot.png"), scale = image_size, x = start + 2 * spacing_3) +
        draw_image(paste0("./plots/analysis_plots/", rownames(words_df_3)[4], "_plot.png"), scale = image_size, x = start + 3 * spacing_3) +
        draw_image(paste0("./plots/analysis_plots/", rownames(words_df_3)[5], "_plot.png"), scale = image_size, x = start + 4 * spacing_3) +
        draw_image(paste0("./plots/analysis_plots/", rownames(words_df_3)[6], "_plot.png"), scale = image_size, x = start + 5 * spacing_3) +
        draw_image(paste0("./plots/analysis_plots/", rownames(words_df_3)[7], "_plot.png"), scale = image_size, x = start + 6 * spacing_3) +
        draw_image(paste0("./plots/analysis_plots/", rownames(words_df_3)[8], "_plot.png"), scale = image_size, x = start + 7 * spacing_3) +
        draw_image(paste0("./plots/analysis_plots/", rownames(words_df_3)[9], "_plot.png"), scale = image_size, x = start + 8 * spacing_3) +
        draw_image(paste0("./plots/analysis_plots/", rownames(words_df_3)[10], "_plot.png"), scale = image_size, x = start + 9 * spacing_3) +
        draw_image(paste0("./plots/analysis_plots/", rownames(words_df_3)[11], "_plot.png"), scale = image_size, x = start + 10 * spacing_3) +
        draw_image(paste0("./plots/analysis_plots/", rownames(words_df_3)[12], "_plot.png"), scale = image_size, x = start + 11 * spacing_3)

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

OrderSentimentDataframe <- function(data, n_plots, plot_names) {
    "
    Create a new data frame to store the sentiment scores by ascending willing scores
    Input: data_long, n_plots, plot_names
    Output: sentiment_df_sorted (the sentiment_df ordered by levels in the function factor())
    "

    # Get the order of willing scores
    stats <- Get_stats(data, n_plots)
    data_plot <- data.frame(plot_names = plot_names,
                            willing_score_avg = unlist(stats)[c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE)],
                            willing_score_sd = unlist(stats)[c(FALSE, TRUE, FALSE, FALSE, FALSE, FALSE)],
                            pd_score_avg = unlist(stats)[c(FALSE, FALSE, TRUE, FALSE, FALSE, FALSE)],
                            pd_score_sd = unlist(stats)[c(FALSE, FALSE, FALSE, TRUE, FALSE, FALSE)],
                            wtp_score_avg = unlist(stats)[c(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE)],
                            wtp_score_sd = unlist(stats)[c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE)])

    # Create sentiment data frame ordered by ascending willing scores
    sentiment_stats <- Get_sentiment_stats(data, n_plots)
    sentiment_df <- data.frame(plot_names = plot_names,
                               mean = unlist(sentiment_stats)[c(TRUE, FALSE)],
                               sd = unlist(sentiment_stats)[c(FALSE, TRUE)])
    sentiment_df_sorted <- sentiment_df[order(data_plot$willing_score_avg),]
    sentiment_df_sorted$plot_names <- factor(sentiment_df_sorted$plot_names, levels = data_plot_long$plot_names[1:n_plots])

    return(sentiment_df_sorted)
}