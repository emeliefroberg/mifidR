rm(list=ls())
library(RMySQL)
pw = {"Kzou2RL2pkQzjl0T"}
con = dbConnect(MySQL(), 
                host = "mysql-1.cda.hhs.se", port = 3306, dbname = "SEC",
                user = "mifid", password = pw)
df = dbGetQuery(
  con, "SELECT 
  Class_Ticker as ticker,
  Year as year,
  CIK as cik,
  COUNT(DISTINCT CIK) as n
  FROM SEC.SEC
  WHERE Class_Ticker NOT IN ('', 'NULL', '[NULL]')
  GROUP BY ticker, year")
write.csv(df, "ticker_year_cik.csv")
