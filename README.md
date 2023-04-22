# RSelenium with Docker

Webscraping is cool. Docker is cool. Why not be double cool and webscrape in a container? 

This code scrapes Alaska North Slope Oil Prices from Alaska's Department of Revenue's Crude Oil and Natural Gas Prices service (ANS prices) and the Energy Information Administration (Cushing Crude Oil futures). I added forecasts in the oil plots as part of a presentation I gave to the Economic Research Group. The purpose was to explain the martingale sequence of price and give evidence for why you cannot use historical prices to predict future prices due to the efficient market hypothesis. 

For an overview of forecasting oil prices see file fed_forecasting_oil_price.pdf in the repository.

web app address for forecasting oil prices:
      https://rpubs.com/mjveilleux/861223

data: http://www.tax.alaska.gov/programs/oil/dailyoil/dailyoil.aspx
      https://www.eia.gov/dnav/pet/pet_pri_fut_s1_d.htm
      

Values are taken as monthly averages.
