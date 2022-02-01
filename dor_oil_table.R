
###############
#Housekeeping###
###############

library(rio)
library(quantmod)
library(rvest)
library(tidyverse)
library(RSelenium)
library(caTools)
library(bitops)
library(zoo)
library(lubridate)
library(plotly)
library(forecast)

####################

# first open up Docker and run an instance. Make sure the port number is the same with an L at the end
# default password to view the instance on docker web browser is: secret

rD<- rsDriver(browser = 'chrome', port = 4445L, verbose = T)
remDr<- rD[["client"]]
remDr$navigate("http://www.tax.alaska.gov/programs/oil/dailyoil/dailyoil.aspx")

#delete contents in box
remDr$findElement(using = "xpath", value = '//*[@id="ContentPlaceHolder1_txtstart"]')$clearElement()
# other possible ("xpath", "css selector", "id", "name", "tag name", "class name", "link text", "partial link text")
#input the date
date <- "6/1/1996"
remDr$findElement(using = "xpath", value = '//*[@id="ContentPlaceHolder1_txtstart"]')$sendKeysToElement(list(date))
#click the button to load table
remDr$findElement(using = "xpath", value = '/html/body/div/form/div[3]/table/tbody/tr[4]/td/div/input')$doubleclick()
remDr$findElement(using = "xpath", value = '/html/body/div/form/div[3]/table/tbody/tr[4]/td/div/input')$clickElement()


#different functions in selenium: webElement

Sys.sleep(90) # give the page time to fully load
html <- remDr$getPageSource()[[1]]
oil <- read_html(html) %>% # parse HTML
  html_nodes(xpath= '//*[@id="ContentPlaceHolder1_Table1"]/tbody') %>% # extract table nodes with class = "tbl_mapReception"
  #.[3] %>% # keep the third of these tables
  #.[[1]] %>% # keep the first element of this list
  html_table(fill=T) %>% as.data.frame() # have rvest turn it into a dataframe

# rename columns
names(oil) <- c("date", "ans_price", "wti_price", "hh_price", "brent_price", "ans_prod") 

raw_oil<-oil %>% slice(-1) %>% mutate(date= as.Date(date, format = '%m/%d/%Y'),
                                      ans_price = str_replace_all(ans_price, '\\$',''),
                                      ans_price = as.numeric(ans_price),
                                      wti_price = str_replace_all(wti_price, '\\$',''),
                                      wti_price = as.numeric(wti_price),
                                      hh_price = str_replace_all(hh_price, '\\$',''),
                                      hh_price = as.numeric(hh_price),
                                      brent_price = str_replace_all(brent_price, '\\$',''),
                                      brent_price = as.numeric(brent_price),
                                      ans_prod = str_replace_all(ans_prod, '[:punct:]',''),
                                      ans_prod = as.numeric(ans_prod)
)


#Clean data 

clean_oil<- raw_oil%>% select(1,2,3,6) %>% 
  na.omit() %>% 
  mutate(date = as.Date(date),
         date = as.yearmon(date),
         date = as.Date(date)) %>% 
  group_by(date) %>% 
  summarise(ans_p= mean(ans_price),   
            wti_p=  mean(wti_price),
            ans_q= mean(ans_prod)) %>% 
  arrange(date, .by_group = TRUE) %>% 
  mutate(ans_p_pc = Delt(ans_p, type = "log"),
         ans_q_pc = Delt(ans_q, type = "log"),
         wti_p_pc = Delt(wti_p, type = "log"),
         year = year(date))



#add in NYMEX oil futures
nymex<- rio::import('https://www.eia.gov/dnav/pet/xls/PET_PRI_FUT_S1_D.xls', sheet =2, skip=2, header =T) %>% 
  rename(date=1, cushing1 =2, cushing2 =3,cushing3 =4,cushing4 =5) %>% na.omit() %>% 
  mutate(avg_cushing = (cushing1+cushing2+cushing3+cushing4)/4,
         date = as.Date(date),
         date= as.yearmon(date),
         date= as.Date(date)) %>% 
  group_by(date) %>% 
  summarise(avg_cushing = mean(avg_cushing)) %>% 
  filter(date >= '2000-9-01')



#models

ts_oil<-clean_oil %>% select(ans_p) %>% ts(start = c(2000,9), frequency = 12)
nymex_ts<- nymex %>% select(avg_cushing) %>% ts(start = c(2000,9), frequency = 12)


h<- 120
oilfit1 <- meanf(ts_oil, h=h)
oilfit2 <- rwf(ts_oil, h=h)
oilfit3 <- snaive(ts_oil, h=h)
oilfit4 <- rwf(ts_oil, h=h, drift = T)
ets<-ets(ts_oil) %>% forecast(h=h, PI=F)
arima<-auto.arima(ts_oil, seasonal=FALSE)%>% forecast(h=h, PI=F)
nymex_fit<- auto.arima(nymex_ts, seasonal=FALSE) %>% forecast(h=h, PI=F) %>% rbind(nymex_ts)
#checkresiduals(arima)


ts_forecast<- cbind(oilfit1$mean,oilfit2$mean,oilfit3$mean,
                    oilfit4$mean,ets$mean,arima$mean,ts_oil,
                    nymex_fit$mean) %>% 
  round(digits=2)

date<-ts_forecast %>% time()

ts_forecast<- ts_forecast %>% 
  data.frame() %>% 
  cbind(date) %>% 
  mutate(date = as.yearmon(date),
         date = as.Date(date))



saveRDS(ts_forecast, '/Users/masonveilleux/Dropbox/forecasting_projects/oil_price/shiny_oil_forecasting/latest_ans_price_dor.rds')












