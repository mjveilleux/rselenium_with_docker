



library(quantmod)
library(rvest)
library(tidyverse)
library(RSelenium)
library(caTools)
library(bitops)
library(zoo)
library(lubridate)
library(plotly)

#first open up Docker and run the server stand alone thing. Make sure the port is the same with an L at the end
#password to view the instance on docker web browser is: secret

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


#different functions
#webElement

Sys.sleep(90) # give the page time to fully load
html <- remDr$getPageSource()[[1]]


oil <- read_html(html) %>% # parse HTML
  html_nodes(xpath= '//*[@id="ContentPlaceHolder1_Table1"]/tbody') %>% # extract table nodes with class = "tbl_mapReception"
  #.[3] %>% # keep the third of these tables
  #.[[1]] %>% # keep the first element of this list
  html_table(fill=T) %>% as.data.frame() # have rvest turn it into a dataframe

names(oil) <- c("date", "ans_price", "wti_price", "hh_price", "brent_price", "ans_prod") # rename columns

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
  na.omit() %>% mutate(date = as.Date(date),
                       date = as.yearmon(date)) %>% group_by(date) %>% summarise(ans_p= mean(ans_price),   
                                                                                 wti_p=  mean(wti_price),
                                                                                 ans_q= mean(ans_prod)) %>% arrange(date, .by_group = TRUE) %>% 
  mutate(ans_p_pc = Delt(ans_p, type = "log"),
         ans_q_pc = Delt(ans_q, type = "log"),
         wti_p_pc = Delt(wti_p, type = "log"),
         year = year(date))





saveRDS(clean_oil, 'latest_oil_price_dor.rds')


#average, naive, seasonal, ARMA, ARIMA




ts_oil<-latest_oil_price_dor %>% select(ans_p) %>% ts(start = c(2000,9), frequency = 12)

h<- 120
oilfit1 <- meanf(ts_oil, h=h)
oilfit2 <- rwf(ts_oil, h=h)
oilfit3 <- snaive(ts_oil, h=h)
oilfit4 <- rwf(ts_oil, h=h, drift = T)
ets<-ets(ts_oil) %>% forecast(h=h, PI=F)
arima<-auto.arima(ts_oil, seasonal=FALSE)%>% forecast(h=h, PI=F)
#checkresiduals(arima)



ts_forecast<- cbind(oilfit1$mean,oilfit2$mean,oilfit3$mean,oilfit4$mean,ets$mean,arima$mean,ts_oil) %>% round(digits=2)
date<-ts_forecast %>% time()

ts_forecast<- ts_forecast %>% 
  data.frame() %>% cbind(date) %>% mutate(date = as.yearmon(date),
                                          date = as.Date(date))



saveRDS(ts_forecast, 'latest_ans_price_dor.rds')












