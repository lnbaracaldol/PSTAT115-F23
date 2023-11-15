library(tidyverse)
library(googlesheets4)

intervals_data <- read_sheet("https://docs.google.com/spreadsheets/d/1aRR4ulf7qwnn2GJMltXQE8vi93mDxT0qCwKucYVzjKk/edit#gid=2121317367")

questions <- c("California Population", "Olympic Swimming Pool Volume", "Median US Income", "Grooves on a quarter", "Gold in Fort Knox", "Population of Australia", "Tsunami", "Disneyland", "Jupiter's Volume", "Netflix")

lower <- intervals_data %>% 
    dplyr::select(`Email Address`, contains("LOWER")) %>% 
    rename_with(function(x) questions, starts_with("LOWER")) %>%
    pivot_longer(cols=2:11, names_to="Question", values_to="Lower Endpoint")
upper <- intervals_data %>% 
    dplyr::select(`Email Address`, contains("UPPER")) %>% 
    rename_with(function(x) questions, starts_with("UPPER")) %>%
    pivot_longer(cols=2:11, names_to="Question", values_to="Upper Endpoint")

num_responses <- nrow(intervals_data)

long_data <- inner_join(lower, upper, by=c("Email Address", "Question"))

truth <- c(12, 660000, 59039, 119, 9206250, 24130000, 1720, 1971, 1321.33, 1997)

long_data %>% filter(Question=="Q1") %>%
    ggplot(aes(x=`Email Address`, group=`Email Address`)) +
    geom_hline(yintercept=truth[1], col="firebrick") + 
    geom_linerange(aes(ymin = `Lower Endpoint`, ymax = `Upper Endpoint`)) + 
    ylim(c(0, 100)) + theme_bw()


long_data %>% filter(Question=="Q2") %>%
    ggplot(aes(x=`Email Address`, group=`Email Address`)) +
    geom_hline(yintercept=truth[2], col="firebrick") + 
    geom_linerange(aes(ymin = `Lower Endpoint`, ymax = `Upper Endpoint`)) + 
    ylim(c(0, 1000000)) + theme_bw()


long_data %>% mutate(truth = rep(truth, num_responses)) %>%
    group_by(Question) %>% 
    summarise(mean = mean(`Lower Endpoint` < truth & `Upper Endpoint` > truth, na.rm=TRUE)) %>% 
    arrange(desc(mean))

## Q8
coverage <- inner_join(lower, upper) %>% mutate(lower = as.numeric(lower), upper = as.numeric(upper)) %>% filter(question == "Q8") %>% summarise(mean = mean(lower < 1971 & upper > 1971, na.rm=TRUE)) %>% as.numeric
inner_join(lower, upper) %>% mutate(lower = as.numeric(lower), upper = as.numeric(upper)) %>% filter(question=="Q8") %>%
    ggplot(aes(x=factor(student), group=factor(student))) + geom_hline(yintercept=1971, col="red") + geom_linerange(aes(ymin = lower, ymax = upper)) + coord_cartesian(ylim=c(1900, 2018)) +   theme_bw() + theme(axis.title.x=element_blank(), axis.text.x=element_blank()) + ggtitle(sprintf("When was Disney World, Orlando founded? (Coverage = %.2f)", coverage))



## Q4
coverage <- inner_join(lower, upper) %>% mutate(lower = as.numeric(lower), upper = as.numeric(upper)) %>% filter(question == "Q4") %>% summarise(mean = mean(lower < 119 & upper > 119)) %>% as.numeric



inner_join(lower, upper) %>% mutate(lower = as.numeric(lower), upper = as.numeric(upper)) %>% filter(question == "Q5") 

inner_join(lower, upper) %>% mutate(lower = as.numeric(lower), upper = as.numeric(upper)) %>% filter(question == "Q3") %>% summarise(mean = mean(lower < 59039 & upper > 59039))

inner_join(lower, upper) %>% mutate(lower = as.numeric(lower), upper = as.numeric(upper)) %>% filter(question == "Q4") %>% summarise(mean = mean(lower < 119 & upper > 119, na.rm=TRUE))

inner_join(lower, upper) %>% mutate(lower = as.numeric(lower), upper = as.numeric(upper)) %>% filter(question == "Q") %>% summarise(mean = mean(lower < 119 & upper > 119, na.rm=TRUE))



inner_join(lower, upper) %>% mutate(lower = as.numeric(lower))
