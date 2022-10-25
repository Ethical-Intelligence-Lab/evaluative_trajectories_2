
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
            breaks = c("willing_score_avg"),
            labels = c("Willingness to Buy"),
            values = c("#56B4E9"),
            guide = guide_legend(title.position = "top")
        )
    return(grouped_bar_plot)
}


MakeGroupedBarPlotImages <- function(LifelinesPlot, data_plot_long) {
    "
    Make a plotter function that produces 'clean' (no labels) version of individual images
    for the x-axis. Then, plot the images in order of ascending satisfaction scores,
    which can be determined by the order in data_plot_long$plot_names[1:n].
    Input: grouped_bar_plot, plot_names
    Output: the plot labels for the grouped bar graph and the sentiment bar graph
    "

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