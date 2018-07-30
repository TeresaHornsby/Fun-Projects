library(dplyr)
library(dslabs)
data("polls_us_election_2016")

# This is a table called `polls` that filters by  state, date, and reports the spread

polls <- polls_us_election_2016 %>% 
  filter(state != "U.S." & enddate >= "2016-10-31") %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)

#creating columns for upper/lower confidence level
X_hat <- polls$rawpoll_clinton[1]/100
N <- polls$samplesize

cis <- polls %>% mutate(X_hat = (spread+1)/2,
                        se = 2*sqrt(X_hat*(1-X_hat)/N),
                        lower = spread - qnorm(0.975)*se,
                        upper = spread + qnorm(0.975)*se) %>% 
  select(state,startdate,enddate,pollster,grade,spread,lower,upper)
head(cis)

# Adding the actual results to the `cis` data set
add <- results_us_election_2016 %>% mutate(actual_spread = clinton/100 - trump/100) %>% select(state, actual_spread)
cis <- cis %>% mutate(state = as.character(state)) %>% left_join(add, by = "state")

# Creating an object called `p_hits` that summarizes the proportion of confidence intervals that contain the actual value. 
p_hits <- cis %>% mutate(hit=lower<cis$actual_spread & cis$actual_spread<upper)%>% summarize(proportion_hits=mean(hit))
p_hits
add <- results_us_election_2016 %>% mutate(actual_spread = clinton/100 - trump/100) %>% select(state, actual_spread)
cis <- cis %>% mutate(state = as.character(state)) %>% left_join(add, by = "state")

# Creating an object called `p_hits` that summarizes the proportion of hits for each pollster that has more than 5 polls.
p_hits <- cis %>% mutate(hit=lower<actual_spread & actual_spread<upper)%>% group_by(pollster)%>% filter(n()>=5)%>% summarize(proportion_hits=mean(hit),n=n(),grade=grade[1]) %>% arrange(desc(proportion_hits))%>% select(pollster,grade,proportion_hits,n)
p_hits

add <- results_us_election_2016 %>% mutate(actual_spread = clinton/100 - trump/100) %>% select(state, actual_spread)
ci_data <- cis %>% mutate(state = as.character(state)) %>% left_join(add, by = "state")

# Creating an object called `p_hits` that summarizes the proportion of hits for each state that has more than 5 polls.
p_hits <- ci_data %>% mutate(hit = lower < actual_spread & actual_spread < upper)%>% group_by(state) %>% filter(n() > 5) %>% summarize(proportion_hits = mean(hit), n = n()) %>% arrange(desc(proportion_hits))
p_hits
head(p_hits)

# A barplot of the proportion of hits for each state
p_hits%>%ggplot(aes(state,proportion_hits))+geom_bar(stat= "identity")+
  coord_flip()

# Creating an object called `errors` that calculates the difference between the predicted and actual spread and indicates if the correct winner was predicted
errors<- cis %>% mutate(error=spread-actual_spread,hit= sign(spread)==sign(actual_spread))

# Creating an object called `errors` that calculates the difference between the predicted and actual spread and indicates if the correct winner was predicted
errors <- cis %>% mutate(error = spread - actual_spread, hit = sign(spread) == sign(actual_spread))

# Creating an object called `p_hits` that summarizes the proportion of hits for each state that has more than 5 polls
p_hits <- errors %>%  group_by(state) %>%
  filter(n() >=  5) %>%
  summarize(proportion_hits = mean(hit), n = n())

#A barplot of the proportion of hits for each state
p_hits %>% mutate(state = reorder(state, proportion_hits)) %>%
  ggplot(aes(state, proportion_hits)) + 
  geom_bar(stat = "identity") +
  coord_flip()

# A histogram of the error
hist(errors$error)

#A boxplot showing the errors by state for polls with grades B+ or higher
errors %>% filter(grade %in% c("A+","A","A-","B+")| is.na(grade)) %>% mutate(state=reorder(state,error))%>% ggplot(aes(x=state,y=error))+geom_boxplot()+geom_point()
