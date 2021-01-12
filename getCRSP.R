rm(list=ls())

library(RMySQL)

#setwd("~/R/FM/")

pw = {"Kzou2RL2pkQzjl0T"}
con = dbConnect(MySQL(), 
                host = "mysql-1.cda.hhs.se", port = 3306, dbname = "SEC",
                user = "mifid", password = pw)

#get unique tickers to collect CRSP-data
ticker.unique = dbGetQuery(
  con, "SELECT DISTINCT Class_Ticker FROM SEC")
ticker.unique = ticker.unique[-(1:86),]
write(ticker.unique, "tickers_for_crsp.txt")

#get key between tickers and cik to merge CRSP-data with N-SAR-data
ticker_year_cik = dbGetQuery(
  con, "SELECT Class_Ticker AS ticker, Year AS year, max(CIK) AS cik, count(DISTINCT CIK) AS n 
  FROM SEC GROUP BY Class_Ticker, Year HAVING (n<5)")
ticker_year_cik = ticker_year_cik[-(1:147),]
write.csv(ticker_year_cik, "ticker_year_cik.csv")