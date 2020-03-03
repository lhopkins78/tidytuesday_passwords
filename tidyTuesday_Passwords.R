#libraries
library(tidyverse)
library(ggthemes)

# Get the Data
passwords <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-14/passwords.csv')

#transform values to seconds and get lengths
times <- data.frame(c("seconds", "minutes","hours","days", "weeks","months","years"), 
               c(1, 60,60*60,60*60*24,60*60*24*7,60*60*24*365/12,60*60*24*365))
names(times) <- c("time_unit", "value_time")
passwords_times <- merge(passwords,times) %>% 
  mutate(time_seconds = value_time*value, length=str_length(password)) 

#plot
passwords_times %>% ggplot(aes(y=log(value_time), x=length)) + geom_jitter(alpha=0.5) + 
  facet_wrap(~category, nrow=2) + theme_economist() + 
  theme(axis.line.x=element_blank(), axis.text = element_blank(), 
        axis.ticks.x = element_blank()) + labs(x="Length of password", 
        y="Time to crack (log seconds)", title="Passwords by category, length and time to crack")
ggsave("passwords1.png")

#split alphanumeric into 3 alpha, numeric, alphanumeric
passwords_alphanum <- passwords_times %>% filter(category == "simple-alphanumeric") %>%
  mutate(category = ifelse(str_detect(password, "^[0-9]*$"), "numeric", category)) %>%
  mutate(category = ifelse(str_detect(password, "^[a-z]*$"), "alpha", category))

passwords_alphanum %>% ggplot(aes(y=log(value_time), x=length, col=category)) + geom_jitter(alpha=0.5, size=3) + 
  theme_economist() + 
  theme(axis.line.x=element_blank(), axis.text = element_blank(), 
        axis.ticks.x = element_blank()) + labs(x="Length of password", 
                                               y="Time to crack (log seconds)", title="Alphanumeric passwords by length and time to crack")
ggsave("passwords2.png")
