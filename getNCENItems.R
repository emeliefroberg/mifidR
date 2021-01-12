rm(list=ls())

library(dplyr)
library(RMySQL)
library(stringr)
library(tidyr)

myFiles = list.files()

df = as.data.frame(matrix(NA, ncol = 15, nrow = length(myFiles)))
cols = c("rownr", "id", "cik", "date", "series", "ticker", "classes", 
         "commission_BD", "commission_B", "avgassets",
         "debt_1", "index_1", "balanced_1", "closed_1", "equity_1")
names(df) = cols
for (i in 1:length(myFiles)) {
  if(i != 729) {
  NCEN = read.csv(myFiles[i])
  #get from top rows
  date = NCEN %>% 
      filter (substr(V1, 1, 26)=="CONFORMED PERIOD OF REPORT") %>% 
      transmute(date = (substr(V1, 29, 36)))
  series = length(NCEN[substr(NCEN$V1,1,11)=='<SERIES-ID>','V1'])  
  ticker = length(NCEN[substr(NCEN$V1,1,30)=='<CLASS-CONTRACT-TICKER-SYMBOL>','V1'])  
  classes = length(NCEN[substr(NCEN$V1,1,19)=='<CLASS-CONTRACT-ID>','V1'])  
  cik = NCEN %>% filter (substr(V1,1,11)=='<OWNER-CIK>') %>% transmute(cik = substring(V1, 12))
  #keep only reported data
  first.row = as.numeric(rownames(NCEN[NCEN$V1=="<XML>",])[1])+1
  last.row = as.numeric(rownames(NCEN[NCEN$V1=="</XML>",])[1])-1
  if(is.na(last.row)) {next}
  NCEN = NCEN[first.row:last.row,] 
    #store data
    df$rownr[i] = i
    df$id[i] = myFiles[i]
    df$cik[i] = as.character(cik$cik[1])
    df$date[i] = as.character(date$date)
    df$series[i] = series
    df$ticker[i] = ticker
    df$classes[i] = classes
    temp = NCEN %>% filter(str_detect(V1, '<brokerDealerCommission>'))
    temp = gsub("<brokerDealerCommission>", "", temp$V1)
    temp = gsub("</brokerDealerCommission>", "", temp)
    df$commission_BD[i] = sum(as.numeric(temp))
    temp = NCEN %>% filter(str_detect(V1, '<aggregateCommission>'))
    temp = gsub("<aggregateCommission>", "", temp$V1)
    temp = gsub("</aggregateCommission>", "", temp)
    df$commission_B[i] = sum(as.numeric(temp))
    temp = NCEN %>% filter(str_detect(V1, '<mnthlyAvgNetAssets>'))
    temp = gsub("<mnthlyAvgNetAssets>", "", temp$V1)
    temp = gsub("</mnthlyAvgNetAssets>", "", temp)
    df$avgassets[i] = sum(as.numeric(temp))
    temp = NCEN %>% filter(str_detect(V1, '<fundType>'))
    temp = gsub("<fundType>", "", temp$V1)
    temp = gsub("</fundType>", "", temp)
    df$debt_1 = sum(ifelse(grep("*[Mm]oney [Mm]arket [Ff]und*", temp), 1, 0))
    df$index_1 = sum(ifelse(
      grep("*[Ii]ndex [Ff]und*", temp) |
      grep("*[Ii]nverse of a [Bb]enchmark*", temp) |
      grep("*[Ff]und of [Ff]unds*", temp) |
      grep("*[Mm]aster-[Ff]eeder [Ff]und*", temp) |
      grep("*[Uu]nderlying [Ff]und*", temp),
                            1, 0))
    df$balanced_1 = sum(ifelse(grep("*[Tt]arget [Dd]ate [Ff]und*", temp), 1, 0))
    df$closed_1 = sum(ifelse(grep("*[Ii]nterval [Ff]und*", temp), 1, 0))
    df$equity_1 = sum(ifelse(grep("*N/A*", temp), 1, 0))
    #principalTotalPurchaseSale
    #principalAggregatePurchase
    #isBrokerageResearchPayment
    #separateAccountTotalAsset
  }
}
## GO THROUGH ERROR WARNINGS
pw = {"Kzou2RL2pkQzjl0T"}
con = dbConnect(MySQL(), 
                host = "mysql-1.cda.hhs.se", port = 3306, dbname = "SEC",
                user = "mifid", password = pw)
#based on tickers for all open-end us funds
cik = dbGetQuery(
    con,
    "SELECT
      CIK as cik, 
      Year as year,
      COUNT(DISTINCT Series_ID) AS n_series,
      COUNT(DISTINCT Class_ID) AS n_class,
      SUM(CASE WHEN uniqueCIK>1 THEN 1 ELSE 0 END) n_nonunique,
      SUM(CASE WHEN before_15 < 1 OR after_15 < 1 THEN 1 ELSE 0 END) n_nonboth15,
      SUM(CASE WHEN before_16 < 1 OR after_16 < 1 THEN 1 ELSE 0 END) n_nonboth16
    FROM (
      SELECT ticker, 
      COUNT(DISTINCT s.CIK) AS uniqueCIK,
      SUM(Year<2015) AS before_15, 
      SUM(Year>=2015) AS after_15,
      SUM(Year<2016) AS before_16,
      SUM(Year>=2016) AS after_16
      FROM SEC.TickerCik tc
        LEFT JOIN SEC.SEC s ON tc.ticker = s.Class_Ticker
      GROUP BY ticker) t
      LEFT JOIN SEC ON t.ticker = SEC.Class_Ticker
    GROUP BY SEC.CIK, SEC.Year")
df$year = substr(df$date, 1,4)
df$year = as.numeric(df$year)
df$cik = as.numeric(df$cik)
df.merge = merge(df, cik, 
                 by = c('cik', 'year'),
                 all.x = T)
df.merge = df.merge[!is.na(df.merge$cik),]
df.merge[is.na(df.merge)] = ""

#setwd("~/R/FM/")
write.csv(df.merge, "us_commissions_NCEN.csv")
