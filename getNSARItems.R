rm(list=ls())

library(dplyr)
library(RMySQL)
library(stringr)
library(tidyr)

getValue = function(item, FUN = 'none', sub = NA, sub_7 = NA, dummy = F, num = T, j = 12){
  df.get = NSAR[substr(NSAR$V1, 1, 3) == item, 'V1']
  if(length(df.get) < 1){
    return(NA)
  }else{
    if(!is.na(sub)){
      df.get = df.get[substr(df.get, 5, 5) == sub]
      if(length(df.get) < 1){return(NA)}
    }else if(!is.na(sub_7)){
      df.get = df.get[substr(df.get, 7, 7) == sub_7]
      if(length(df.get) < 1){return(NA)}
    }
    df.get = substring(df.get, j)
    if(dummy) {
      df.get = ifelse(df.get == "Y", 1, 0)  
    }else if(num){
      df.get = as.numeric(df.get)
      if(!is.na(sub_7)){
        df.get = df.get[df.get != 0]
        if(length(df.get)==0) {return(NA)}
      }
    }
    if(FUN == 'm'){
      return(mean(df.get))
    }else if(FUN == 'n'){
      return(length(df.get))
    }else if(FUN == 's'){
      return(sum(df.get))
    }else{
      return(df.get)
    }
  } 
}

#setwd("~/NSAR")
myFiles = list.files()

df = as.data.frame(matrix(NA, ncol = 48, nrow = length(myFiles)))
cols = c("rownr", "id", "cik", "date", "series", "ticker", "classes", "family", 
         "index_n", "index_1", "debt_n", "debt_1", 
         "equity_n", "equity_1", "balanced_n", "balanced_1", "foreign_n", "foreign_1", 
         "metal_n", "metal_1","n020", "amount_020", "amount_021", "n022", 
         "purchases_022", "sales_022", "purchases_023", "sales_023",
         "NAV", "advcontr_d", "advcontr_m", "mgmt_fee",
         "pfee_d", "style_aggcapapprec_d", "style_capapprec_d",
         "style_growth_d", "style_growthinc_d", "style_inc_d", "style_totret_d",
         "total_purchases",
         "total_sales", "avgNAV", "turnover", "gross_advisory_fee",
         "TE", "assets", "NA_common", "avgNAV_allfunds")
names(df) = cols
for (i in 1:length(myFiles)) {
  if (i != 1640 & i!= 12438)    {
      NSAR = read.csv(myFiles[i])
    #get from top rows
    date = NSAR %>% 
      filter (substr(V1, 1, 26)=="CONFORMED PERIOD OF REPORT") %>% 
      transmute(date = (substr(V1, 29, 36)))
    series = length(NSAR[substr(NSAR$V1,1,11)=='<SERIES-ID>','V1'])  
    ticker = length(NSAR[substr(NSAR$V1,1,30)=='<CLASS-CONTRACT-TICKER-SYMBOL>','V1'])  
    classes = length(NSAR[substr(NSAR$V1,1,19)=='<CLASS-CONTRACT-ID>','V1'])  
    cik = NSAR %>% filter (substr(V1,1,11)=='<OWNER-CIK>') %>% transmute(cik = substring(V1, 12))
    #keep only reported data
    first.row = as.numeric(rownames(NSAR[NSAR$V1=="<TEXT>",])[1])+1
    last.row = as.numeric(rownames(NSAR[NSAR$V1=="</TEXT>",])[1])-1
    NSAR = NSAR[first.row:last.row,] 
    
    #store data
    df$rownr[i] = i
    df$id[i] = myFiles[i]
    df$cik[i] = as.character(cik$cik[1])
    df$date[i] = as.character(date$date)
    df$series[i] = series
    df$ticker[i] = ticker
    df$classes[i] = classes
    df$family[i] = getValue(item = '019', sub = 'C', j = 13, num = F)
    
    df$index_n[i] = getValue(item = '069', FUN = 'n', dummy = T, j = 13)
    df$index_1[i] = getValue(item = '069', FUN = 's', dummy = T, j = 13)
    df$debt_n[i] = getValue(item = '062', FUN = 'n', sub = 'A', dummy = T, j = 13)
    df$debt_1[i] = getValue(item = '062', FUN = 's', sub = 'A', dummy = T, j = 13)
    df$equity_n[i] = getValue(item = '066', FUN = 'n', sub = 'A', dummy = T, j = 13)
    df$equity_1[i] = getValue(item = '066', FUN = 's', sub = 'A', dummy = T, j = 13)
    df$balanced_n[i] = getValue(item = '067', FUN = 'n', dummy = T, j = 13)
    df$balanced_1[i] = getValue(item = '067', FUN = 's', dummy = T, j = 13)
    df$metal_n[i] = getValue(item = '068', FUN = 'n', sub = 'A', dummy = T, j = 13)
    df$metal_1[i] = getValue(item = '068', FUN = 's', sub = 'A', dummy = T, j = 13)
    df$foreign_n[i] = getValue(item = '068', FUN = 'n', sub = 'B', dummy = T, j = 13)
    df$foreign_1[i] = getValue(item = '068', FUN = 's', sub = 'B', dummy = T, j = 13)
    
    df$n020[i] = getValue(item = '020', sub = 'A', FUN = 'n', j = 13, num = F)
    df$amount_020[i] = getValue(item = '020', sub = 'C', FUN = 's', j = 13)
    df$amount_021[i] = getValue(item = '021', FUN = 's', j = 13)
    df$n022[i] = getValue(item = '022', sub = 'A', FUN = 'n', j = 13, num = F)
    df$purchases_022[i] = getValue(item = '022', sub = 'C', FUN = 's', j = 13)
    df$sales_022[i] = getValue(item = '022', sub = 'D', FUN = 's', j = 13)
    df$purchases_022[i] = getValue(item = '023', sub = 'C', FUN = 's', j = 13)
    data023 = NSAR %>% filter(substr(V1, 1, 3)=='023') %>% transmute(substring(V1, 13)) 
    df$purchases_023[i] = ifelse(nrow(data023)>0, as.numeric(data023[1,1]), NA)
    df$sales_023[i] = ifelse(nrow(data023)>0, as.numeric(data023[2,1]), NA)
    df$NAV[i] = getValue(item = '028', sub = 'H', FUN = 's')
    df$advcontr_d[i] = getValue(item = '045', FUN = 'm', dummy = T)
    df$advcontr_m[i] = getValue(item = '046', FUN = 's', dummy = T)
    df$mgmt_fee[i] = getValue(item = '048', FUN = 'm', sub_7 = "2")
    df$pfee_d[i] = getValue(item = '051', FUN = 'm', dummy = T)
    df$style_aggcapapprec_d[i] = getValue(item = '066', FUN = 'm', sub = 'B', dummy = T, j = 12)
    df$style_capapprec_d[i] = getValue(item = '066', FUN = 'm', sub = 'C', dummy = T, j = 13)
    df$style_growth_d[i] = getValue(item = '066', FUN = 'm', sub = 'D', dummy = T, j = 13)
    df$style_growthinc_d[i] = getValue(item = '066', FUN = 'm', sub = 'E', dummy = T, j = 13)
    df$style_inc_d[i] = getValue(item = '066', FUN = 'm', sub = 'F', dummy = T, j = 13)
    df$style_totret_d[i] = getValue(item = '066', FUN = 'm', sub = 'G', dummy = T, j = 13)
    
    df$total_purchases[i] = getValue(item = '071', FUN = 's', sub = 'A', j = 13)
    df$total_sales[i] = getValue(item = '071', FUN = 's', sub = 'B', j = 13)
    df$avgNAV[i] = getValue(item = '071', FUN = 's', sub = 'C', j = 13)
    #(min(71A,71B)/71C)
    df$turnover[i] = getValue(item = '071', FUN = 'm', sub = 'D', j = 13)
    df$gross_advisory_fee[i] = getValue(item = '072', FUN = 's', sub = 'F', j = 13)
    df$TE[i] = getValue(item = '072', FUN = 's', sub = 'X', j = 13)
    df$assets[i] = getValue(item = '074', FUN = 's', sub = 'N', j = 13)
    df$NA_common[i] = getValue(item = '074', FUN = 's', sub = 'T', j = 13)
    df$avgNAV_allfunds[i] = getValue(item = '075', FUN = 's', sub = 'B', j = 13)
  
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
write.csv(df.merge, "us_commissions_NSAR.csv")

