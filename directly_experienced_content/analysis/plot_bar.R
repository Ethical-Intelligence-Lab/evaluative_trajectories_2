rm(list = ls())

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #set working directory to current directory

## Import libraries
if (!require(pacman)) { install.packages("pacman") }
pacman::p_load('ggplot2')

# Plot the grouped bar graph in order of ascending willingness scores
CreateBarPlot <- function(data) {
    grouped_bar_plot <- ggplot(data, aes(x = X, y = score, fill = question_type)) +
        geom_bar(position = "dodge", stat = "identity") +
        geom_errorbar(aes(ymin = score - sd, ymax = score + sd), width = .2,
                      position = position_dodge(.9)) +
        ggtitle("Summarizing the willing of Different Customer Journeys") +
        xlab("Trailer Experience Clusters") +
        ylab("Mean Willingness to Pay") +
        theme(
            plot.title = element_blank(),
            legend.title = element_blank(),
            legend.text = element_text(color = "black", size = 28),
            legend.position = "none",
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
            values = c("#3c7ea3"),
            guide = guide_legend(title.position = "top")
        )

    return(grouped_bar_plot)
}

# Plot small cluster icons
CreateClusterIcons <- function(LifelinesPlot, data) {
    plot_images <- axis_canvas(LifelinesPlot, axis = 'x')

    sorted_cluster_names <- c(data[, 'cluster_names'])

    for (i in 1:27) {
        plot_images <- plot_images +
            draw_image(paste0("./cluster_plots/", sorted_cluster_names[i], "_", 27, ".png"), x = i - 0.5)
    }

    return(plot_images)
}


##### MAIN #####
data <- read.csv("data_plot.csv")
bar_plot <- CreateBarPlot(data) # Create bars
cluster_icons <- CreateClusterIcons(bar_plot, data) # Create cluster icons
pdf(file = paste0("bar_plot.pdf"), width = 17, height = 8)
ggdraw(insert_xaxis_grob(bar_plot, cluster_icons, position = "bottom"))
dev.off()