rm(list=ls())

### Set working directory to current directory ###
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

if (!require(pacman)) { install.packages("pacman") }
pacman::p_load('RcppHungarian', 'ggplot2', 'ggpubr', 'rjson')

source('../tools/Lifelines_Generate_Plots.R')

# Create R equations
create_equation <- function(eqn) {
    eqn <- fromJSON(eqn)

    if (length(eqn) <= 1) { print("error") }
    return(function(x) {
        l <- length(eqn) - 1

        sum <- 0
        count <- 0
        for (p in l:0) {
            count <- count + 1
            sum <- sum + eqn[count] * `^`(x * 8 / 9, p)
        }

        return(sum)
    })
}

### Load Data ###
d_customer_journeys <- read.csv('../satisfaction_of_a_customer_journey/analysis/data/dat.csv')
d_interview_performance <- read.csv('../hiring_likelihood/analysis/data/dat.csv')
d_lifelines <- read.csv('../lifelines/analysis/data/d_long.csv')
d_first_person <- read.csv('../directly_experienced_content/analysis/data/dat_for_comparison.csv')

cluster_centers <- read.csv('../directly_experienced_content/analysis/data/cluster_centers.csv')

#correlation between WTP and satisfaction, and WTP and desirability

suppressWarnings({
### Start Analysis ###
print("CORRELATIONS ACROSS STUDIES");
print('***** Customer Journeys Satisfaction v. Personal Desirability *****')
print(cor.test(d_customer_journeys$word.score[d_customer_journeys$word.question_type == "satisfaction"],
         d_customer_journeys$word.score[d_customer_journeys$word.question_type == "personal_desirability"]))

print('***** Customer Journeys Satisfaction v. Lifelines Meaningfulness *****')
d_ll <- d_lifelines[d_lifelines$word.question_type == "meaningfulness",]
d_cj <- d_customer_journeys[d_customer_journeys$word.question_type == "satisfaction",]
print(cor.test(aggregate(d_cj, list(d_cj$word.plot_names), mean)$word.score,
         aggregate(d_ll, list(d_ll$word.plot_names), mean)$word.score))

print('***** Customer Journey Satisfaction v. Hiring Likelihood *****')
d_cj <- d_customer_journeys[d_customer_journeys$word.question_type == "satisfaction",]
d_ip <- d_interview_performance[d_interview_performance$word.question_type == "hiring_likelihood",]
print(cor.test(aggregate(d_cj, list(d_cj$word.plot_names), mean)$word.score,
         aggregate(d_ip, list(d_ip$word.plot_names), mean)$word.score))

print('***** Customer Journeys Personal Desirability v. Lifelines Personal Desirability *****')
d_ll <- d_lifelines[d_lifelines$word.question_type == "personal_desirability",]
d_cj <- d_customer_journeys[d_customer_journeys$word.question_type == "personal_desirability",]
print(cor.test(aggregate(d_cj, list(d_cj$word.plot_names), mean)$word.score,
         aggregate(d_ll, list(d_ll$word.plot_names), mean)$word.score))

print('***** Customer Journeys Personal Desirability v. Hiring Likelihood *****')
d_cj <- d_customer_journeys[d_customer_journeys$word.question_type == "personal_desirability",]
d_ip <- d_interview_performance[d_interview_performance$word.question_type == "hiring_likelihood",]
print(cor.test(aggregate(d_cj, list(d_cj$word.plot_names), mean)$word.score,
         aggregate(d_ip, list(d_ip$word.plot_names), mean)$word.score))

print('***** Lifelines Meaningfulness v. Hiring Likelihood *****')
d_ll <- d_lifelines[d_lifelines$word.question_type == "meaningfulness",]
d_ip <- d_interview_performance[d_interview_performance$word.question_type == "hiring_likelihood",]
print(cor.test(aggregate(d_ll, list(d_ll$word.plot_names), mean)$word.score,
         aggregate(d_ip, list(d_ip$word.plot_names), mean)$word.score))

print('***** Lifelines Personal Desirability v. Hiring Likelihood Correlate? *****')
d_ll <- d_lifelines[d_lifelines$word.question_type == "personal_desirability",]
d_ip <- d_interview_performance[d_interview_performance$word.question_type == "hiring_likelihood",]
print(cor.test(aggregate(d_ll, list(d_ll$word.plot_names), mean)$word.score,
         aggregate(d_ip, list(d_ip$word.plot_names), mean)$word.score))


##############################   FIRST PERSON ANALYSES   #############################
n_clusters <- length(table(d_first_person$cluster_labels))

#########   Find which clusters are closest to which curve   #########
#########   First run 'tools/Lifelines_Generate_Plots.R'     #########

plotter <- function(i, exp, eqn1, eqn2) {
    cluster_line_df <- data.frame(eqn1, head(seq(from = 0, to = 80, by = 8/90), -1))
    colnames(cluster_line_df) <- c("yy", "xx")

    base <- ggplot(data=cluster_line_df, aes( x=xx, y=yy, group=1)) + xlim(0, 80) + ylim(0, 100) +
        geom_point(colour="#00AF50", aes(size = 2)) +
        geom_function(fun = eqn2, colour="firebrick3", aes(size = 2)) +
        xlab("") + ylab("") +
        theme(legend.position="none")

    print(base)
    return(base)
}

set.seed(0)

eqns <- equations
exp <- 'customer_journeys'

# Compare each cluster with each curve in other studies
mses = matrix(nrow = 27, ncol = 27)
for (i in 1:27) {  # Each cluster
    cluster_center <- cluster_centers[, i + 1]
    for (k in 1:27) { # Each curve in other studies
        curr_error <- 0

        for (j in 1:900) {
            curr_error <- sum(c(curr_error, (cluster_center[j] - eqns[[k]]((j - 1) * 80 / 900))^2)) # equations for lifelines
        }

        curr_error <- curr_error / 80
        mses[i, k] <- curr_error
    }
}

result <- HungarianSolver(mses)

errors <- list()
for (i in (1:27)) { errors <- append(errors, mses[i, result[[2]][, 2][i]]) }
print(paste0("Total error: ", Reduce("+", errors)))

plot_list <- list()
for (i in 1:27) {
    print(paste0('Cluster ', i, ' matches with line ', result[[2]][, 2][i]))
    plot_list[[i]] <- plotter(i, exp, cluster_centers[, i + 1], eqns[[result[[2]][, 2][i]]])
}


arrange <- ggarrange(plotlist=plot_list, ncol = 3, nrow = 9) +
  theme(plot.margin = margin(10,10,10,10, "cm"))
ggsave("cluster_matches.png", arrange, limitsize = FALSE, width = 30, height = 60)

############## Check if corresponding clusters correlate in Customer Journeys ##############

d_ll <- d_first_person[d_first_person$word.question_type == "willing",]

get_corr_plot <- function(num) {
    return(plot_names[result[[2]][, 2][num + 1]]);
}

print("****** CORRELATIONS BETWEEN RATINGS ACROSS STUDIES 1, 3, S2 AND 4 ******")
print('***** Customer Journey Satisfaction v. WTP *****')
d_ll$corresponding_plots <- sapply(d_first_person$word.cluster_labels, FUN = get_corr_plot)
d_cj <- d_customer_journeys[d_customer_journeys$word.question_type == "satisfaction",]
print(cor.test(aggregate(d_cj, list(d_cj$word.plot_names), mean)$word.score,
         aggregate(d_ll, list(d_ll$corresponding_plots), mean)$word.score))

print('***** Customer Journey PD v. WTP *****')
d_cj <- d_customer_journeys[d_customer_journeys$word.question_type == "personal_desirability",]
print(cor.test(aggregate(d_cj, list(d_cj$word.plot_names), mean)$word.score,
         aggregate(d_ll, list(d_ll$corresponding_plots), mean)$word.score))

############## Check if corresponding clusters correlate in Hiring ##############

print('***** Hiring v. WTP *****')
d_cj <- d_interview_performance[d_interview_performance$word.question_type == "hiring_likelihood",]
print(cor.test(aggregate(d_cj, list(d_cj$word.plot_names), mean)$word.score,
         aggregate(d_ll, list(d_ll$corresponding_plots), mean)$word.score))


############## Check if corresponding clusters correlate in Lifelines ##############

print('***** Lifelines Meaningfulness v. WTP *****')
d_cj <- d_lifelines[d_lifelines$word.question_type == "meaningfulness",]
print(cor.test(aggregate(d_cj, list(d_cj$word.plot_names), mean)$word.score,
         aggregate(d_ll, list(d_ll$corresponding_plots), mean)$word.score))

print('***** Lifelines PD v. WTP *****')
d_cj <- d_lifelines[d_lifelines$word.question_type == "personal_desirability",]
print(cor.test(aggregate(d_cj, list(d_cj$word.plot_names), mean)$word.score,
         aggregate(d_ll, list(d_ll$corresponding_plots), mean)$word.score))

})
