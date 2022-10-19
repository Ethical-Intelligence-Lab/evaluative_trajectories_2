### Set working directory to current directory ###
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

### Load Data ###
d_customer_journeys <- read.csv('../customer_journeys/analysis/data/dat.csv')
d_interview_performance <- read.csv('../interview_performance/analysis/data/dat.csv')
d_lifelines <- read.csv('../lifelines/analysis/data/d_long.csv')

#correlation between WTP and satisfaction, and WTP and desirability

### Start Analysis ###
print('Do "Customer Journeys" Satisfaction and Personal Desirability Correlate?')
cor.test(d_customer_journeys$word.score[d_customer_journeys$word.question_type == "satisfaction"],
                d_customer_journeys$word.score[d_customer_journeys$word.question_type == "personal_desirability"])


print('Do "Lifelines" Meaningfulness and Personal Desirability Correlate?')
cor.test(d_lifelines$word.score[d_lifelines$word.question_type == "meaningfulness"],
                d_lifelines$word.score[d_lifelines$word.question_type == "personal_desirability"])

print('Do average of each plot of "Customer Journeys" Satisfaction and Hiring Likelihood Correlate?')
d_cj <- d_customer_journeys[d_customer_journeys$word.question_type == "satisfaction",]
d_ip <- d_interview_performance[d_interview_performance$word.question_type == "hiring_likelihood",]
cor.test(aggregate(d_cj, list(d_cj$word.plot_names), mean)$word.score,
         aggregate(d_ip, list(d_ip$word.plot_names), mean)$word.score)

print('Do average of each plot of "Customer Journeys" Personal Desirability and Hiring Likelihood Correlate?')
d_cj <- d_customer_journeys[d_customer_journeys$word.question_type == "personal_desirability",]
d_ip <- d_interview_performance[d_interview_performance$word.question_type == "hiring_likelihood",]
cor.test(aggregate(d_cj, list(d_cj$word.plot_names), mean)$word.score,
         aggregate(d_ip, list(d_ip$word.plot_names), mean)$word.score)

print('Do average of each plot of "Lifelines" Meaningfulness and Hiring Likelihood Correlate?')
d_ll <- d_lifelines[d_lifelines$word.question_type == "meaningfulness",]
d_ip <- d_interview_performance[d_interview_performance$word.question_type == "hiring_likelihood",]
cor.test(aggregate(d_ll, list(d_ll$word.plot_names), mean)$word.score,
         aggregate(d_ip, list(d_ip$word.plot_names), mean)$word.score)

print('Do average of each plot of "Lifelines" Personal Desirability and Hiring Likelihood Correlate?')
d_ll <- d_lifelines[d_lifelines$word.question_type == "personal_desirability",]
d_ip <- d_interview_performance[d_interview_performance$word.question_type == "hiring_likelihood",]
cor.test(aggregate(d_ll, list(d_ll$word.plot_names), mean)$word.score,
         aggregate(d_ip, list(d_ip$word.plot_names), mean)$word.score)

print('Do average of each plot of "Lifelines" Meaningfulness and "Customer Journeys" Satisfaction Correlate?')
d_ll <- d_lifelines[d_lifelines$word.question_type == "meaningfulness",]
d_cj <- d_customer_journeys[d_customer_journeys$word.question_type == "satisfaction",]
cor.test(aggregate(d_cj, list(d_cj$word.plot_names), mean)$word.score,
         aggregate(d_ll, list(d_ll$word.plot_names), mean)$word.score)

print('Do average of each plot of "Lifelines" Meaningfulness and "Customer Journeys" Personal Desirability Correlate?')
d_ll <- d_lifelines[d_lifelines$word.question_type == "meaningfulness",]
d_cj <- d_customer_journeys[d_customer_journeys$word.question_type == "personal_desirability",]
cor.test(aggregate(d_cj, list(d_cj$word.plot_names), mean)$word.score,
         aggregate(d_ll, list(d_ll$word.plot_names), mean)$word.score)

print('Do average of each plot of "Lifelines" Personal Desirability and "Customer Journeys" Satisfaction Correlate?')
d_ll <- d_lifelines[d_lifelines$word.question_type == "personal_desirability",]
d_cj <- d_customer_journeys[d_customer_journeys$word.question_type == "satisfaction",]
cor.test(aggregate(d_cj, list(d_cj$word.plot_names), mean)$word.score,
         aggregate(d_ll, list(d_ll$word.plot_names), mean)$word.score)

i