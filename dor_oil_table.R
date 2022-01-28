


library(tidyverse)
library(zoo)
library(lubridate)
library(readxl)
library(quantmod)

oil <- read_excel("~/Downloads/Untitled spreadsheet (1).xlsx")

clean_oil<- oil %>% select(date=Date, ans_p= `ANS West Coast`,wti_p= `West Texas Intermediate`, ans_q= `ANS Production`) %>% 
  na.omit() %>% mutate(date = as.Date(date),
                       date = as.yearmon(date)) %>% group_by(date) %>% summarise(ans_p= mean(ans_p),   
                                                                                 wti_p=  mean(wti_p),
                                                                                 ans_q= mean(ans_q)) %>% arrange(date, .by_group = TRUE) %>% 
  mutate(ans_p_pc = Delt(ans_p, type = "log"),
         ans_q_pc = Delt(ans_q, type = "log"),
         wti_p_pc = Delt(wti_p, type = "log"),
         year = year(date))


clean_oil %>% filter(year < 2008) %>% ggplot(aes(y=ans_p, x =ans_q, color = as.factor(year)))+geom_point()
clean_oil %>% filter(year > 208) %>% ggplot(aes(y=ans_p, x =ans_q, color = as.factor(year)))+geom_point()
clean_oil %>% ggplot(aes(y= wti_p_pc, x = ans_q_pc, color = year)) + geom_point()+coord_flip()

ts_clean_oil<-clean_oil %>% ts(start = c(2000,9), frequency = 12)


ts.plot(ts_clean_oil[,6])


cor(clean_oil$ans_p, clean_oil$ans_q)




