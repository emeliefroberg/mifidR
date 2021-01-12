rm(list=ls())

library(tidyr)
library(dplyr)
library(Rcrawler)
library(RMySQL)
library(RCurl)
library(rvest)
library(stringr)
library(XML)

readUrl = function(url) {
  tryCatch(
    {
      readLines(con=url, warn=FALSE) 
      return(FALSE)
    },
    error=function(cond) {
      return(TRUE)
      },
    warning=function(cond) {
      return(TRUE)
    },
    finally={
      closeAllConnections()
    }
  )    
}

setwd("~/R/FM")

pw = {"Kzou2RL2pkQzjl0T"}
con = dbConnect(MySQL(), 
                host = "mysql-1.cda.hhs.se", port = 3306, dbname = "SEC",
                user = "mifid", password = pw)
cik_list = dbGetQuery(
  con, "SELECT DISTINCT CIK AS cik FROM SEC")

df = as.data.frame(matrix(NA, ncol = 1, nrow = 100000))
names(df) = "href"
k = 1
for (i in 1:length(cik_list$cik)) {
  cik_i = cik_list$cik[i]
  url_archive = paste("https://www.sec.gov/cgi-bin/browse-edgar?action=getcompany&CIK=", 
                        cik_i,
                        "&type=N-CEN",
                        sep="")
  a = read_html(url_archive)
  path = "//div[@id='seriesDiv']//table//*//a[@id='documentsbutton']"    
  xml_nodes = html_nodes(a, xpath = path)
  N = length(xml_nodes)
  if(N==0) {next}
  for(j in 1:N) {
    s = xml_nodes[[j]]
    w = suppressWarnings(str_sub(s, start = 11, end = -48))
    w = paste('https://www.sec.gov/', 
               w, 
               '.txt',
               sep = "")
    df$href[k] = w
    k = k + 1
  }
}

df = df[!is.na(df$href),]
write.csv(df, "href.csv")
df = read.csv("href.csv")
#access filings as delimited text file
#scrape relevant data from every filing
for (i in 1:length(df$x)) {
  href = df$x[i]
  if(readUrl(href)){next}
  NCEN = read.delim(href, header= F, sep = "\a")
  NCEN_name = gsub("\\D", "", df$x[i])
  NCEN_name = paste(NCEN_name, '.csv', "")
  write.csv(NCEN,NCEN_name)
}