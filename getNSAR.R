rm(list=ls())

library(tidyr)
library(dplyr)
library(Rcrawler)  
library(RCurl)
library(RMySQL)
library(stringr)
library(XML)

#get cik-list
pw = {"Kzou2RL2pkQzjl0T"}
con = dbConnect(MySQL(), 
                host = "mysql-1.cda.hhs.se", port = 3306, dbname = "SEC",
                user = "mifid", password = pw)
cik_list = dbGetQuery(
  con, "SELECT DISTINCT CIK AS cik FROM SEC")
cik_list = cik_list$cik 

#collect all NSAR-reports
#named as unique submission-name
for (i in 1:length(cik_list)) {
    cik_i = cik_list[i]
    url_archive = paste("https://www.sec.gov/Archives/edgar/data/", cik_i, sep="")
test = ContentScraper(url_archive, 
                      XpathPatterns = "//div[@id='main-content']")
#remove column names
test = substring(test, 22)
#mark row end by replacing time with a dot
test = gsub(' [0-9][0-9]:[0-9][0-9]:[0-9][0-9]', '.', test)
#split into columns
test = strsplit(test, split = '.', fixed = T)
#transpose
test = data.table::transpose(test)
#filter to years 2010-2018; separate filing nr from date
test = tibble(test = test) %>% 
  filter (between(as.numeric(substr(test, 19, 22)), 2010, 2019)) %>%
  separate(test, into = c('filing', 'date'), sep = '[0-9][0-9][0-9][0-9]-')
#include dashes, to access filing in archive
test = paste(substr(test$filing, 1, 10), substr(test$filing,11,12), substr(test$filing,13,18),
              sep = "-")
new_url = paste(url_archive, "/", test, "-index.html", sep="")
#create a dataframe of filings of interest
filing = data.frame(filing = NA)
for(new_url_i in new_url) {
form = substr(ContentScraper(new_url_i, XpathPatterns = "//div[@id='formName']/strong"),6,11)
#check if form is NSAR-B, then collect
if(form == "NSAR-B") {
  filing = rbind(filing, new_url_i)
}
}
#access filings as delimited text file
filing = gsub('-index.html', '.txt', filing[-1,])
#scrape relevant data from every filing
for (filing_i in filing) {
#filing_i = filing[1]
NSAR = read.delim(filing_i, header= F, sep = "\a")

NSAR_name = gsub("\\D", "", filing_i)
NSAR_name = gsub('txt', '.csv', NSAR_name)
write.csv(NSAR,NSAR_name)
}
}