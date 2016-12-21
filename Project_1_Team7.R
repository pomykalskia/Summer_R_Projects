# STAT 6430
# Project 1
# Team 7
# Isabelle, Muyang, Gabe, Andrew

setwd()

library(readr)
library(stringr)
library(dplyr)
library(ggplot2)
p <- read_lines('pizza_requests.txt')

p1 <- data.frame(matrix())


i = 1
p1_row = 1
while (i <= length(p)) {
  p1_col = 1
  subreddits <- ''
  while (p[i] != '%%%%%%%%%%') {
    if (grepl(': ', p[i])) { 
      n <- regexpr(pattern = ': ', p[i])  # index of colon in each string
      i_entry <- substr(p[i],regexpr(pattern = ': ', p[i]), (nchar(p[i])-2)) # keep only substring after the colon
      i_entry1 <- substr(i_entry, 3, nchar(i_entry)) # delete the colon
      if (i_entry1 == '{}') {
        p1[p1_row,p1_col] <- subreddits
        p1_col = p1_col + 1
      }
      else if (i_entry1 != '') { 
        
        p1[p1_row,p1_col] <- i_entry1 # put the substring into the matrix
        p1_col = p1_col + 1
      }
    }
    else if (str_trim(p[i], side = c("both")) == '},') {
      p1[p1_row,p1_col] <- subreddits
      p1_col = p1_col+1
    }
    else {
      subreddits <- paste(subreddits, str_trim(p[i], side = c("both")))
    }
    i = i + 1
  }
  i = i + 2
  p1_row = p1_row + 1
}

# Naming the column names to match the original text file

colnames(p1)<-c("giver_username_if_known", 
                "in_test_set",
                "number_of_downvotes_of_request_at_retrieval",
                "number_of_upvotes_of_request_at_retrieval",
                "post_was_edited",
                "request_id",
                "request_number_of_comments_at_retrieval",
                "request_text",
                "request_text_edit_aware",
                "request_title",
                "requester_account_age_in_days_at_request",
                "requester_account_age_in_days_at_retrieval",
                "requester_days_since_first_post_on_raop_at_request",
                "requester_days_since_first_post_on_raop_at_retrieval",
                "requester_number_of_comments_at_request",
                "requester_number_of_comments_at_retrieval",
                "requester_number_of_comments_in_raop_at_request",
                "requester_number_of_comments_in_raop_at_retrieval",
                "requester_number_of_posts_at_request",
                "requester_number_of_posts_at_retrieval",
                "requester_number_of_posts_on_raop_at_request",
                "requester_number_of_posts_on_raop_at_retrieval",
                "requester_number_of_subreddits_at_request",
                "requester_received_pizza",
                "requester_subreddits_at_request",
                "requester_upvotes_minus_downvotes_at_request",
                "requester_upvotes_minus_downvotes_at_retrieval",
                "requester_upvotes_plus_downvotes_at_request",
                "requester_upvotes_plus_downvotes_at_retrieval",
                "requester_user_flair",
                "requester_username",
                "unix_timestamp_of_request",
                "unix_timestamp_of_request_utc")




# Appendix Questions #

# Question 1:
# COunting the number of requests that resulted in a pizza
# Also counting the number of requests that did not result in a pizza

count_true = 0
count_false = 0
for (i in 1:nrow(p1)){
  if (p1$requester_received_pizza[i] == "true"){
    count_true = count_true +1
  }
  else if (p1$requester_received_pizza[i] == 'false'){
    count_false = count_false +1
  }
}

# Question 2:
# Calculating the average number of subReddits a person who did not receive a pizza was subscribed to

total_f = 0
count_f = 0
for (i in 1:nrow(p1)){
  if (p1$requester_received_pizza[i] == "false"){
    count_f = count_f + 1
    total_f = total_f + as.integer(p1$requester_number_of_subreddits_at_request[i])
  }
}
average_sbreddit= total_f / count_f

# Question 3:
# The average number of down votes a person received and still received a pizza

total_down = 0
count_rec = 0
for (i in 1:nrow(p1)){
  if (p1$requester_received_pizza[i] == "true"){
    count_rec = count_rec + 1
    total_down = total_down + as.integer(p1$number_of_downvotes_of_request_at_retrieval[i])
  }
}
average_downVotes= total_down / count_rec


# Muyang:
# Analysis of special words in a requester comment log.

#Category 1: "urgent" check
# People using urgent key words and testing whether their chance of receiving pizza was increased

# set "urgent: keywords
urgent<- c("kid", "family", "job", "money", "hungry", "starving", "wife", "girlfriend", "son", "daughter")
people_urgent<-p1[grep(paste(urgent,collapse="|"), p1$request_text), ]
nrow(people_urgent) # There are 3090 "urgent" people.
urgent_get <- people_urgent[people_urgent$requester_received_pizza == 'true',]
nrow(urgent_get) #864 people get pizza in the urgent group

p_u = 864/3090 #proportion of getting pizza in urgent people group
p_a = 1397/5671 #overall proportion of getting pizza

#H0 : p_u = p_a vs. Ha : p_u > p_a
z = (p_u - p_a)/sqrt(p_a * (1- p_a)/3090 )
z #4.292243
# urgency is a significant factor in deciding getting pizza.

#Category 2: "polite check"

#Set "polite" words, considering uppercase condition
polite<-c("please", "appreciate",  "Thank", "kind", "would","Please",
          "hope", "wish", "Wish","May", "grateful", "sincere")

people_polite<-p1[grep(paste(polite,collapse="|"), p1$request_text), ]
nrow(people_polite) #3866
get <- people_polite[people_polite$requester_received_pizza == 'true',] 
nrow(get) #1048

#H0 : p_g = p_a vs. Ha : p_g > p_a
p_g = 1048/3866
z1 = (p_g - p_a)/sqrt(p_a * (1- p_a)/3866)
z1 #3.570081
# Politeness is a significant factor in deciding getting pizza.

#Category 3: "pay back" check
payback <- c("promise", "pay back", "give back", "willing", "forward")
people_payback<-p1[grep(paste(payback,collapse="|"), p1$request_text), ]
nrow(people_payback) #1166
get1 <- people_payback[people_payback$requester_received_pizza == 'true',] 
nrow(get1) #375

#H0 : p_z = p_a vs. Ha : p_z > p_a
p_z = 375/1166
z2 = (p_z - p_a)/sqrt(p_z * (1- p_a)/1166)
z2 #5.220655
#Promising to pay back the giver would significantly raise the opportunity of getting pizza.

# We analyzed the flair column of the data to see if there was any predictive behavior
# This was very pointless and inconclusive


## Isabelle: 
## Analyse the effect of number of comments on receiving pizzas.

p1$request_number_of_comments_at_retrieval <- as.integer(p1$request_number_of_comments_at_retrieval) # convert the data to integer

p <- ggplot(p1, aes(x=p1$requester_received_pizza, y=request_number_of_comments_at_retrieval))
p + geom_boxplot(aes(fill=requester_received_pizza)) + labs(list(x="Whether the Requester Received Pizza", y="Number of Comments")) # Create boxplot

quantile(receive_pizza$request_number_of_comments_at_retrieval) # quantile for successful request: (2,7) 
quantile(receive_pizza_not$request_number_of_comments_at_retrieval) # quantile for unsuccessul request: (0,2)

## The effect is significant. 


## Analyse the effect of request text length on receiving pizzas. (using non-edited text)

for (i in 1:nrow(p1)) {
  p1$request_text_length[i] <- str_length(p1$request_text)[i]-2
} # create a variable for request text length

p <- ggplot(p1, aes(x=requester_received_pizza, y=request_text_length))
p + geom_boxplot(aes(fill=requester_received_pizza)) + labs(list(x="Whether the Requester Received Pizza", y="Request Text Length"))# Create boxplot
# Hard to tell from the graph whether the effect is significant. 
# Not normally distributed. So we cannot directly apply CI formulas. 

# Clean or transform the data to fit normal distribution. (Prepare the data for CI formula)
d <- density(p1$request_text_length) # Look at the density data 
plot(d) # left-skewed

receive_pizza <- subset(p1, p1$requester_received_pizza == "true")
d1 <- density(receive_pizza$request_text_length) # returns the density data for requests that received pizza
plot(d1) # left-skewed

receive_pizza_not <- subset(p1, p1$requester_received_pizza == "false")
d2 <- density(receive_pizza_not$request_text_length) # returns the density data for requests that did not receive pizza
plot(d2) # left-skewed

dlog <- density(log(p1$request_text_length)) # look at the density of log of the data
plot(dlog) # right-skewed

receive_pizza_no_outliers <- subset(receive_pizza, receive_pizza$request_text_length <=1000) # exclude outliers for requests that received pizza
d3 <- density(receive_pizza_no_outliers$request_text_length) # returns the density data 
plot(d3)

receive_pizza_not_no_outliers <- subset(receive_pizza_not, receive_pizza_not$request_text_length <=1000) # exclude outliers for requests that did not receive pizza
d4 <- density(receive_pizza_not_no_outliers$request_text_length) # returns the density data 
plot(d4)

mean(receive_pizza_no_outliers$request_text_length) - 1.96 * sd(receive_pizza_no_outliers$request_text_length) / sqrt(nrow(receive_pizza_no_outliers))
mean(receive_pizza_no_outliers$request_text_length) + 1.96 * sd(receive_pizza_no_outliers$request_text_length) / sqrt(nrow(receive_pizza_no_outliers))

# The 95% CI for the request text length of the requester that received pizzas is: (386.6221, 411.8426)

mean(receive_pizza_not_no_outliers$request_text_length) - 1.96 * sd(receive_pizza_not_no_outliers$request_text_length) / sqrt(nrow(receive_pizza_not_no_outliers))
mean(receive_pizza_not_no_outliers$request_text_length) + 1.96 * sd(receive_pizza_not_no_outliers$request_text_length) / sqrt(nrow(receive_pizza_not_no_outliers))

# The 95% CI for the request text length of the requester that did not receive pizzas is: (311.0012, 324.3674)

## Conclusion: The requests with longer text tend to have greater chances of receiving pizzas.                       ##
## (Ran the same process with text length of edited requests. Came up with same conclusion. )



## Analyse the effect of requesters' subreddits subscription on the probability of receiving pizzas. 

subreddits_total <- vector('character') 
subreddits_receive <- vector('character') 
# create two vectors to store relevant subreddits. 

for (i in 1:nrow(p1)) { 
  tempstr <- p1$requester_subreddits_at_request[i]
  tempstr <- gsub(' ', '', tempstr)
  tempstr <- gsub('\\"', '', tempstr) 
  subreddits <- strsplit(tempstr, ',') 
  subreddits_total <- c(subreddits_total, subreddits[[1]]) # store all subreddits in the first vector 
  if (p1$requester_received_pizza[i] == 'true'){
    subreddits_receive <- c(subreddits_receive, subreddits[[1]])
  } # if a requester received pizza, also store the associated subreddits in the second vector
}

total_by_subreddits <- data.frame(sort(table(subreddits_total), decreasing = TRUE))
colnames(total_by_subreddits) <- c("subreddit", "total") # count the occurrence of subreddits in the first vector
receive_by_subreddits <- data.frame(sort(table(subreddits_receive), decreasing = TRUE))
colnames(receive_by_subreddits) <- c("subreddit", "receive") # count the occurrence of subreddits in the second vector
by_subreddits <- merge(total_by_subreddits, receive_by_subreddits, by='subreddit')
by_subreddits <- filter(by_subreddits, total>100) # remove subreddits that do not have enough data (less than 100 occurrences)
by_subreddits$perc_receive <- percent(by_subreddits$receive / by_subreddits$total) # calculate percentages 

# top 10 subreddits that have high chance of receiving pizza
by_subreddits[order(by_subreddits$perc_receive, decreasing = TRUE),][1:10,]

# top 10 subreddits that have low chance of receiving pizza
by_subreddits[order(by_subreddits$perc_receive, decreasing = FALSE),][1:10,]

## Conclusion: People who received pizzas were more commonly subscribed to positive, mainstream hobby-oriented channels. 
## Whereas people who did not receive pizzas were more commonly subscribed to channels associated with technological interests including video gaming. 



## Analyse the effect of the number of requesters' subreddits on the probability of receiving pizzas. 

p1$requester_number_of_subreddits_at_request <- as.numeric(p1$requester_number_of_subreddits_at_request)

p <- ggplot(p1, aes(x=p1$requester_received_pizza, y=requester_number_of_subreddits_at_request))
p + geom_boxplot(aes(fill=requester_received_pizza)) # Difference is not obvious on the boxplot. 

subreddits_number_freq1 <- data.frame(table(p1$requester_number_of_subreddits_at_request))
colnames(subreddits_number_freq1) <- c('subreddits_number', 'total_occurrance')
subreddits_number_freq2 <- data.frame(table(receive_pizza$requester_number_of_subreddits_at_request))
colnames(subreddits_number_freq2) <- c('subreddits_number', 'receive_occurrance')
subreddits_number_perc <- merge(subreddits_number_freq1, subreddits_number_freq2, by= 'subreddits_number')
library(scales)
subreddits_number_perc$success_rate <- percent(subreddits_number_perc$receive_occurrance / subreddits_number_perc$total_occurrance)
subreddits_number_perc[order(subreddits_number_perc$subreddits_number, decreasing = FALSE),][1:10,]

## Requesters with less than two subreddits have a lower than average likelihood of receiving pizzas. 
## But no significant difference for the requesters who are subscribed to two subreddits or more.  


# This Section is the code for the plots used for up and down votes
# Each plot is accompanied by its respective correlation coefficient
# Plots are labeled by the column they represent and the subsets are given
# Columns of the data frame used are listed in each plot

# Plots
receive_pizza <- subset(p1, p1$requester_received_pizza == "true")

receive_pizza_not <- subset(p1, p1$requester_received_pizza == "false")

p_up_vs_down_total <- ggplot(data = p1) +
  geom_jitter(aes(x = as.integer(number_of_upvotes_of_request_at_retrieval), y = as.integer(number_of_downvotes_of_request_at_retrieval), color = requester_received_pizza)) +
  xlim(c(0,50)) + ylim(c(0,25)) + labs(x="Up Votes", y="DownVotes") 
p_up_vs_down_total

cor_up_vs_down_all <- cor(as.integer(p1$number_of_upvotes_of_request_at_retrieval), as.integer(p1$number_of_downvotes_of_request_at_retrieval))

p_up_vs_down_T <- ggplot(data = receive_pizza) +
  geom_jitter(aes(x = as.integer(number_of_upvotes_of_request_at_retrieval), y = as.integer(number_of_downvotes_of_request_at_retrieval), color = requester_received_pizza)) +
  xlim(c(0,50)) + ylim(c(0,25)) + labs(x="Up Votes", y="DownVotes") 
p_up_vs_down_T

cor_up_vs_down_T <- cor(as.integer(receive_pizza$number_of_downvotes_of_request_at_retrieval), as.integer(receive_pizza$number_of_upvotes_of_request_at_retrieval))

p_up_vs_down_F <- ggplot(data = receive_pizza_not) +
  geom_jitter(aes(x = as.integer(number_of_upvotes_of_request_at_retrieval), y = as.integer(number_of_downvotes_of_request_at_retrieval), color = requester_received_pizza)) +
  xlim(c(0,50)) + ylim(c(0,25)) + labs(x="Up Votes", y="DownVotes") 
p_up_vs_down_F

cor_up_vs_down_F <- cor(as.integer(receive_pizza_not$number_of_downvotes_of_request_at_retrieval), as.integer(receive_pizza_not$number_of_upvotes_of_request_at_retrieval))

# Positive ratio vs total votes, All, True, False

p_up_vs_total_all <- ggplot(data = p1) +
  geom_jitter(aes(x = as.integer(requester_upvotes_plus_downvotes_at_retrieval), y = as.integer(requester_upvotes_minus_downvotes_at_retrieval), color=requester_received_pizza)) +
  ylim(c(-10,15000)) + xlim(c(-10,25000)) + labs(x="Total Votes", y="Positive Ratio of Votes") 
p_up_vs_total_all

cor_up_vs_total_all <- cor(as.integer(p1$requester_upvotes_plus_downvotes_at_retrieval), as.integer(p1$requester_upvotes_minus_downvotes_at_retrieval))

p_up_vs_total_true <- ggplot(data = receive_pizza) +
  geom_jitter(aes(x = as.integer(requester_upvotes_plus_downvotes_at_retrieval), y = as.integer(requester_upvotes_minus_downvotes_at_retrieval), color=requester_received_pizza)) +
  ylim(c(-10,15000)) + xlim(c(-10,25000)) + labs(x="Total Votes", y="Positive Ratio of Votes") 
p_up_vs_total_true

cor_up_vs_total_true <- cor(as.integer(receive_pizza$requester_upvotes_plus_downvotes_at_retrieval), as.integer(receive_pizza$requester_upvotes_minus_downvotes_at_retrieval))

p_up_vs_total_false <- ggplot(data = receive_pizza_not) +
  geom_jitter(aes(x = as.integer(requester_upvotes_plus_downvotes_at_retrieval), y = as.integer(requester_upvotes_minus_downvotes_at_retrieval), color=requester_received_pizza)) +
  ylim(c(-10,15000)) + xlim(c(-10,25000)) + labs(x="Total Votes", y="Positive Ratio of Votes") 
p_up_vs_total_false

cor_up_vs_total_false <- cor(as.integer(receive_pizza_not$requester_upvotes_plus_downvotes_at_retrieval), as.integer(receive_pizza_not$requester_upvotes_minus_downvotes_at_retrieval))

# These are the histograms for the Voting comparisons

ggplot(data=receive_pizza, aes(as.numeric(receive_pizza$number_of_upvotes_of_request_at_retrieval))) + geom_histogram(binwidth = 0.5) + xlim(c(-10,50)) + ylim(c(0,400)) + labs(x ="Up Votes", y="Count", title="Number of Up Votes for a Successful Pizza Request")

ggplot(data=receive_pizza, aes(as.numeric(receive_pizza$number_of_downvotes_of_request_at_retrieval)))+geom_histogram(binwidth = 0.5) + xlim(c(-10,50)) + labs(x ="Down Votes", y="Count", title="Number of Down Votes for a Successful Pizza Request")

ggplot(data=receive_pizza_not, aes(as.numeric(receive_pizza_not$number_of_upvotes_of_request_at_retrieval)))+geom_histogram(binwidth = 0.5) + xlim(c(-10,50)) + ylim(c(0,1200)) + labs(x ="Up Votes", y="Count", title="Number of Up Votes for a Unsuccessful Pizza Request")

ggplot(data=receive_pizza_not, aes(as.numeric(receive_pizza_not$number_of_downvotes_of_request_at_retrieval)))+geom_histogram(binwidth = 0.5) + xlim(c(-10,50)) + labs(x ="Down Votes", y="Count", title="Number of Down Votes for a Unsuccessful Pizza Request")
