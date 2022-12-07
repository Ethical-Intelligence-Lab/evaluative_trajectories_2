########## ---- CONTAINS FUNCTIONS ONLY FOR PLOTTING (Common ones are in '/tools/common_functions.R') ---- #########
## For Study: Customer Journeys

MakeGroupedBarPlot <- function(data_plot_long, wtp = FALSE) {
    "
    Plot the grouped bar graph in order of ascending satisfaction scores
    Input: data_plot_long
    Output: grouped_bar_plot (the grouped bar graph)
    "

    if (wtp) {
        data_plot_long <- data_plot_long[data_plot_long$question_type == "wtp_score_avg",]
        grouped_bar_plot <- ggplot(data_plot_long, aes(x = plot_names, y = score, fill = question_type)) +
            geom_bar(position = "dodge", stat = "identity") +
            geom_errorbar(aes(ymin = score - sd, ymax = score + sd), width = .2,
                          position = position_dodge(.9)) +
            ggtitle("Summarizing the Satisfaction and Desirability of Different Customer Journeys") +
            xlab("Customer Journey Plots") +
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
                breaks = c("wtp_score_avg"),
                labels = c("Willingness to Pay"),
                values = c("#5660e9"),
                guide = guide_legend(title.position = "top")
            )

        return(grouped_bar_plot)
    }

    data_plot_long <- data_plot_long[data_plot_long['question_type'] != 'wtp_score_avg',]
    grouped_bar_plot <- ggplot(data_plot_long, aes(x = plot_names, y = score, fill = question_type)) +
        geom_bar(position = "dodge", stat = "identity") +
        geom_errorbar(aes(ymin = score - sd, ymax = score + sd), width = .2,
                      position = position_dodge(.9)) +
        ggtitle("Summarizing the Satisfaction and Desirability of Different Customer Journeys") +
        xlab("Customer Journey Plots") +
        ylab("Mean Rating") +
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
            breaks = c("satisfaction_score_avg", "pd_score_avg"),
            labels = c("Satisfaction", "Personal Desirability"),
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
    for (i in 1:length(my_equations)) { #print individual plots
        png(file = paste0(plot_names[i], "_plot.png", ""))
        sapply(my_equations[i], Plotter_2)
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

MakeSentimentBarPlot <- function(data, n_plots, plot_names, title = "Satisfaction") {
    "
    Plot the sentiment bar graph in order of ascending satisfaction scores.
    Input: data_long, n_plots, plot_names
    Output: the sentiment bar graph by ascending satisfaction scores
    "

    sentiment_df <- OrderSentimentDataframe(data, n_plots, plot_names)
    sentiment_bar_plot <- ggplot(sentiment_df, aes(x = plot_names, y = mean)) +
        geom_bar(position = "dodge", stat = "identity", fill = "darkorange") +
        geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = .2,
                      position = position_dodge(.9)) +
        ggtitle(paste("Mean Sentiment Scores by Ascending", title, "Scores")) +
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

CV_plotter <- function(results_df, x_order, results_order, ques_type, x_labels) {
    "
    What this function does: creates a grouped box plot of the cross-validated prediction results
    Inputs: results_df, x_order, results_order, ques_type, x_labels
    Output: a boxplot of participant rating predictions with either principal components or predictors
    "

    results_df[results_df['question_type'] == 'satisfaction_results', 'question_type'] = "Satisfaction"
    results_df[results_df['question_type'] == 'pd_results', 'question_type'] = "Personal Desirability"

    grouped_box_plot <- ggplot(data = results_df, aes(x = x_order, y = results_order, fill = question_type, color = question_type)) +
        scale_colour_manual(values = c("#3c7ea3", "#800000")) +
        scale_x_discrete() +
        geom_hline(yintercept = 0, color = "gray60") +
        stat_summary(fun = mean, geom = "point", shape = 20, size = 5, aes(group = question_type, color=question_type), position = position_dodge(.75)) +
        stat_summary(fun.data = mean_se, geom = "errorbar", size=1.2, aes(group = question_type, width=0.5, color=question_type), position = position_dodge(.75), fun.args = list(mult = 1.96)) +  # mean-se is 1.96 * std err (https://stulp.gmw.rug.nl/ggplotworkshop/comparinggroupstatistics.html)
        ggtitle(paste0("Satisfaction and Desirability Predictions with ", x_labels)) +
        xlab(x_labels) +
        ylab("Prediction Performance\n(Cross-Validated Pearson's r)") +
        scale_y_continuous(breaks = round(seq(-1, 1, by = 0.2), 1)) +
        scale_fill_manual(
            name = "Judgment Type",
            breaks = c("satisfaction_results", "pd_results"),
            labels = c("Satisfaction", "Personal Desirability"),
            values = c("#4b9ecc", "#006b4e"),
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