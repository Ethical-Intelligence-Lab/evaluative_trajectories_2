
CV_plotter <- function(results_df, x_order, results_order, ques_type, x_labels, y_axis = "Pearson's r", no_kfold = FALSE) {
    "
    What this function does: creates a grouped box plot of the cross-validated prediction results
    Inputs: results_df, x_order, results_order, ques_type, x_labels, sum_willing
    Output: a boxplot of participant rating predictions with either principal components or predictors
    "

    y_label <- paste0("Prediction Accuracy\n(Cross-Validated ", y_axis, ")")

    box_label <- "Willingness to Buy"
    if (y_axis != "Pearson's r") {
        box_label <- "Raffle Choice"
    }
    if (no_kfold) { y_label <- paste0("Prediction Accuracy\n(", y_axis, ")") }
    grouped_box_plot <- ggplot(data = results_df, aes(x = x_order, y = results_order)) +
        scale_x_discrete() +
        geom_hline(yintercept = 0, color = "gray60") +
        stat_summary(fun = mean, geom = "point", shape = 20, size = 5, color = "#3c7ea3", position = position_dodge(.75)) +
        stat_summary(fun.data = mean_se, geom = "errorbar", color = "#3c7ea3", aes(group = question_type, width = 0.5), position = position_dodge(.75), fun.args = list(mult = 1.96)) +  # mean-se is 1.96 * std err (https://stulp.gmw.rug.nl/ggplotworkshop/comparinggroupstatistics.html)
        ggtitle(paste0("willing and Desirability Predictions with ", x_labels)) +
        xlab(x_labels) +
        ylab(y_label) +
        ylim(0, 0.4) +
        scale_y_continuous(breaks = round(seq(-1, 1, by = 0.2), 1)) +
        scale_fill_manual(
            name = "Judgment Type",
            breaks = c("willing_results"),
            labels = c(box_label),
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


MakeGroupedBarPlot <- function(data_plot_long) {
    "
    Plot the grouped bar graph in order of ascending willing scores
    Input: data_plot_long
    Output: grouped_bar_plot (the grouped bar graph)
    "

    grouped_bar_plot <- ggplot(data_plot_long, aes(x = cluster_names, y = score, fill = question_type)) +
        geom_bar(position = "dodge", stat = "identity") +
        geom_errorbar(aes(ymin = score - sd, ymax = score + sd), width = .2,
                      position = position_dodge(.9)) +
        ggtitle("Summarizing the willing of Different Customer Journeys") +
        xlab("Trailer Experience Clusters") +
        ylab("Mean Rating") +
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
            breaks = c("willing_score_avg"),
            labels = c("Willingness to Buy"),
            values = c("#3c7ea3"),
            guide = guide_legend(title.position = "top")
        )
    return(grouped_bar_plot)
}


MakeGroupedBarPlotImages <- function(LifelinesPlot, data_plot_long) {
    plot_images <- axis_canvas(LifelinesPlot, axis = 'x')

    sorted_cluster_names <- c(data_plot_long[, 'cluster_names'])

    for (i in 1:n_clusters) {
        plot_images <- plot_images + draw_image(paste0("./plots/cluster/", sorted_cluster_names[i], "_", n_clusters, ".png"), x = i - 0.5)
    }

    return(plot_images)
}


MakeSentimentBarPlot <- function(data, n_plots, plot_names) {
    "
    Plot the sentiment bar graph in order of ascending willing scores.
    Input: data_long, n_plots, plot_names
    Output: the sentiment bar graph by ascending willing scores
    "

    sentiment_df <- OrderSentimentDataframe(data, n_plots, plot_names)
    sentiment_bar_plot <- ggplot(sentiment_df, aes(x = plot_names, y = mean)) +
        geom_bar(position = "dodge", stat = "identity", fill = "darkorange") +
        geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = .2,
                      position = position_dodge(.9)) +
        ggtitle("Mean Sentiment Scores by Ascending WTP Scores") +
        xlab("Trailer Experience Clusters") +
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