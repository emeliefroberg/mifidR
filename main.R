#test
rm(list=ls())
setwd("~/R/FM")

library(dplyr)
library(gdata)
library(ggplot2)
library(plm)
library(RColorBrewer)
library(tidyr)
library(weights)

#AR data SE
#ToDo: update to mendeley data open repository
setwd("~/R/FM")
df.main = read.csv("Rts.csv")

#DOI:10.17632/529ks6bwk6.1#file-86d5251a-b496-4c49-8a81-be26c5ba6811
df.supp = read.csv("https://data.mendeley.com/public-files/datasets/529ks6bwk6/files/86d5251a-b496-4c49-8a81-be26c5ba6811/file_downloaded")

#grouping var
df.internal = df.supp %>% 
  filter(!is.na(research2018)) %>%
  transmute(name = fundname,
            isin = isin,
            internal = ifelse(
              research2018 == 0, 1, 0))

#firm data
#ToDO: update to mendeley data open repository
df.fund.firm = read.csv("Fund_Firm.csv")

#DOI: 10.17632/x92nhgdbxc.1#file-67176d16-33f0-4f56-ac73-f8fc31d20f03
df.firm = read.csv("https://data.mendeley.com/public-files/datasets/x92nhgdbxc/files/67176d16-33f0-4f56-ac73-f8fc31d20f03/file_downloaded")
###############
### Table 4 ### 

#EBIT
df.internal = df.internal %>% left_join(df.fund.firm, by = "isin") %>% 
  left_join(df.firm, "firmnr") %>% 
  select("name", "isin", "internal", "firmnr", "EBIT2017", "EBIT2018", "Familysize2017", "Familysize2018", "Bank_d")

xtab = table(df.internal$internal, df.internal$Bank_d)
xtab
#run chi2 on this?

#research, execution, mgmt. fee
df.internal = df.internal %>% 
  left_join(select(df.supp, "isin", "mgmfee2017", "mgmfee2018", "execution2017", 
                   "execution2018", "research2017", "research2018", "fundsize2017", "fundsize2018"), by = "isin") %>% 
  mutate(research_r2017 = research2017/(fundsize2017*10), research_r2018 = research2018/(fundsize2018*10),
         execution_r2017 = execution2017/(fundsize2017*10), execution_r2018 = execution2018/(fundsize2018*10)) 
#research in long format
research = reshape(df.internal, idvar = "isin", varying = c("research_r2017", "research_r2018"), 
                   v.names = "research_r",
                   times = 2017:2018, new.row.names = 1:852, direction = "long") %>% 
  select ("isin", "time", "research_r", "internal")
research = research[!is.na(research$research_r),]
#execution in long format
execution = reshape(df.internal, idvar = "isin", varying = c("execution_r2017", "execution_r2018"), 
                    v.names = "execution_r",
                   times = 2017:2018, new.row.names = 1:852, direction = "long") %>% 
  select ("isin", "time", "execution_r", "internal")
execution = execution[!is.na(execution$execution_r),]
#plot interactions

pdf(file = "r_iplot.pdf")
par(mfrow=c(2,2), xpd = T)
interaction.plot(x.factor = research$time, trace.factor = research$internal, 
                 response = research$research, fun = mean, 
                 type = "b", legend = F, 
                 xlab = "Year", ylab="Research",
                 pch=c(1,2), col = c("black", "lightgray"), main = "Fund-level")
interaction.plot(x.factor = execution$time, trace.factor = execution$internal, 
                 response = execution$execution, fun = mean, 
                 type = "b", legend = F, 
                 xlab = "Year", ylab="Execution",
                 pch=c(1,2), col = c("black", "lightgray"))
#ebit and mgmfee, firm-level
df.internal = df.internal %>% group_by (firmnr) %>% summarise (ebit2017 = mean(EBIT2017), ebit2018 = mean(EBIT2018),
                                                       mgmfee2017 = mean(mgmfee2017), mgmfee2018 = mean(mgmfee2018),
                                                       internal = mean(internal)) %>% filter (internal == 1 | internal == 0)
#mgmfee long format
mgmfee = reshape(df.internal, idvar = "firmnr", varying = c("mgmfee2017", "mgmfee2018"), 
                   v.names = "mgmfee",
                   times = 2017:2018, new.row.names = 1:62, direction = "long") %>% 
  select ("firmnr", "time", "mgmfee", "internal")
mgmfee = mgmfee[!is.na(mgmfee$mgmfee),]
#ebit long format
ebit = reshape(df.internal, idvar = "firmnr", varying = c("ebit2017", "ebit2018"), 
                    v.names = "ebit",
                    times = 2017:2018, new.row.names = 1:62, direction = "long") %>% 
  select ("firmnr", "time", "ebit", "internal")
ebit = ebit[!is.na(ebit$ebit),]
ebit$ebit = ebit$ebit / 1000
#plot interactions
interaction.plot(x.factor = mgmfee$time, trace.factor = mgmfee$internal, 
                 response = mgmfee$mgmfee, fun = mean, 
                 type = "b", legend = F, 
                 xlab = "Year", ylab="Mgmt. fee",
                 pch=c(1,2), col = c("black", "lightgray"), main = "Firm-level")
interaction.plot(x.factor = ebit$time, trace.factor = ebit$internal, 
                 response = ebit$ebit, fun = mean, 
                 type = "b", legend = F, 
                 xlab = "Year", ylab="EBIT, in MSEK",
                 pch=c(1,2), col = c("black", "lightgray"))
## ToDo: FIX!
legend("bottomleft", inset = c(-0.45, -0.48), title = "Internalized?", legend=c(0,1), 
       col = c("black", "lightgray"), pch=c(1,2), xpd = T, bty = "n", horiz = T)

dev.off()
#regressions with interactions
research$time = as.factor(research$time)
research$internal = as.factor(research$internal)
execution$time = as.factor(execution$time)
execution$internal = as.factor(execution$internal)
mgmfee$time = as.factor(mgmfee$time)
mgmfee$internal = as.factor(mgmfee$internal)
ebit$time = as.factor(ebit$time)
ebit$internal = as.factor(ebit$internal)

#ToDo: Solution is singulair

summary(lm(research_r ~time*internal, research))
summary(lm(execution_r ~time*internal, execution))
summary(lm(mgmfee ~time*internal, mgmfee))
summary(lm(ebit ~time*internal, ebit))

#for anova 2x2 between x within test:
summary(aov(research_r ~ time*internal + Error(isin/time), data=research))
summary(aov(execution_r ~ time*internal + Error(isin/time), data=execution))
summary(aov(mgmfee ~ time*internal + Error(firmnr/time), data=mgmfee))
summary(aov(ebit ~ time*internal + Error(firmnr/time), data=ebit))

################
### Figure 1 ###

#Main, value-weighted
df.2012 = df.main[,c("tiar2012", "ocxfixed2012", "fixed2012")]
ll12 = lapply(df.2012, weighted.mean,  w = df.main$amt2012, na.rm=T)
ll12 = data.frame("2012", ll12, 0, 0)
colnames(ll12) =c("year", "tiar", "ocxfixed", "fixed", "txar", "extanr")
df.2013 = df.main[,c("tiar2013", "ocxfixed2013", "fixed2013")]
ll13 = lapply(df.2013, weighted.mean,  w = df.main$amt2013, na.rm=T)
ll13 = data.frame("2013", ll13, 0, 0)
colnames(ll13) = c("year", "tiar", "ocxfixed", "fixed", "txar", "extanr")
df.2014 = df.main[,c("tiar2014", "ocxfixed2014", "fixed2014")]
ll14 = lapply(df.2014, weighted.mean,  w = df.main$amt2014, na.rm=T)
ll14 = data.frame("2014", ll14, 0, 0)
colnames(ll14) = c("year", "tiar", "ocxfixed", "fixed", "txar", "extanr")
df.2015 = df.main[!is.na(df.main$tiar2015),c("tiar2015", "ocxfixed2015", "fixed2015")]
ll15 = lapply(df.2015, weighted.mean,  w = df.main$amt2015[!is.na(df.main$tiar2015)], na.rm=T)
ll15 = data.frame("2015a", ll15, 0, 0)
colnames(ll15) = c("year", "tiar", "ocxfixed", "fixed", "txar", "extanr")
df.2015.early = df.main[is.na(df.main$tiar2015),c("ocxfixed2015", "fixed2015", "txar2015", "extanr2015")]
ll15.early = lapply(df.2015.early, weighted.mean,  w = df.main$amt2015[is.na(df.main$tiar2015)], na.rm=T)
ll15.early = data.frame("2015b", 0,ll15.early)
colnames(ll15.early) = c("year", "tiar", "ocxfixed", "fixed", "txar", "extanr")
df.2016 = df.main[,c("ocxfixed2016", "fixed2016", "txar2016", "extanr2016")]
ll16 = lapply(df.2016, weighted.mean,  w = df.main$amt2016, na.rm=T)
ll16 = data.frame("2016", 0, ll16)
colnames(ll16) = c("year", "tiar", "ocxfixed", "fixed", "txar", "extanr")
df.2017 = df.main[,c("ocxfixed2017", "fixed2017", "txar2017", "extanr2017")]
ll17 = lapply(df.2017, weighted.mean,  w = df.main$amt2017, na.rm=T)
ll17 = data.frame("2017", 0, ll17)
colnames(ll17) = c("year", "tiar", "ocxfixed", "fixed", "txar", "extanr")
df.2018 = df.main[,c("ocxfixed2018", "fixed2018", "txar2018", "extanr2018")]
ll18 = lapply(df.2018, weighted.mean,  w = df.main$amt2018, na.rm=T)
ll18 = data.frame("2018", 0, ll18)
colnames(ll18) = c("year", "tiar", "ocxfixed", "fixed", "txar", "extanr")
ll = rbind(ll12, ll13, ll14, ll15, ll15.early, ll16, ll17, ll18)
attach(ll)
ll = data.frame(year, fixed, ocxfixed, tiar, extanr, txar)
#plot main value-weighted
sequential = brewer.pal(5, "RdYlGn")

pdf(file = "r_barplot.pdf")
par(mfrow=c(1,2)) 
barplot(t(ll[,-1]),
        names.arg = ll$year, # x-axis labels
        cex.names = 0.7, # makes x-axis labels small enough to show all
        col = sequential, # colors
        xlab = "Year",
        ylab = "Costs, %",
        las = 2,
        #xlim = c(0,20), # these two lines allow space for the legend
        #width = 1,
        main = "Main sample") # these two lines allow space for the legend
# #Equally-weighted
# ll12 = apply(df.2012, 2, mean, na.rm=T)
# ll12 = data.frame("2012", t(ll12), 0, 0)
# colnames(ll12) =c("year", "tiar", "ocxfixed", "fixed", "txar", "extanr")
# ll13 = apply(df.2013, 2, mean, na.rm=T)
# ll13 = data.frame("2013", t(ll13), 0, 0)
# colnames(ll13) = c("year", "tiar", "ocxfixed", "fixed", "txar", "extanr")
# ll14 = apply(df.2014, 2, mean, na.rm=T)
# ll14 = data.frame("2014", t(ll14), 0, 0)
# colnames(ll14) = c("year", "tiar", "ocxfixed", "fixed", "txar", "extanr")
# ll15 = apply(df.2015, 2, mean, na.rm=T)
# ll15 = data.frame("2015a", t(ll15), 0, 0)
# colnames(ll15) = c("year", "tiar", "ocxfixed", "fixed", "txar", "extanr")
# ll15.early = apply(df.2015.early, 2, mean, na.rm=T)
# ll15.early = data.frame("2015b", 0, t(ll15.early))
# colnames(ll15.early) = c("year", "tiar", "ocxfixed", "fixed", "txar", "extanr")
# ll16 = apply(df.2016, 2, mean, na.rm=T)
# ll16 = data.frame("2016", 0, t(ll16))
# colnames(ll16) = c("year", "tiar", "ocxfixed", "fixed", "txar", "extanr")
# ll17 = apply(df.2017, 2, mean, na.rm=T)
# ll17 = data.frame("2017", 0, t(ll17))
# colnames(ll17) = c("year", "tiar", "ocxfixed", "fixed", "txar", "extanr")
# ll18 = apply(df.2018, 2, mean, na.rm=T)
# ll18 = data.frame("2018", 0, t(ll18))
# colnames(ll18) = c("year", "tiar", "ocxfixed", "fixed", "txar", "extanr")
# 
# detach()
# ll = rbind(ll12, ll13, ll14, ll15, ll15.early, ll16, ll17, ll18)
# attach(ll)
# ll = data.frame(year, fixed=fixed, ocxfixed, tiar, extanr, txar)
# 
# par(mfrow=c(1,1))
# barplot(t(ll[,-1]),
#         names.arg = ll$year, # x-axis labels
#         cex.names = 0.7, # makes x-axis labels small enough to show all
#         col = sequential, # colors
#         xlab = "Year",
#         las = 2,
#         ylab = "Costs, %",
#         xlim = c(0,20), # these two lines allow space for the legend
#         width = 1) # these two lines allow space for the legend
# legend("right", cex = 0.7, title = "Cost ratios", c("Execution", "Research", "Bundled commissions", "OC, excl. mgmt fees", "Mgmt fees"), fill = sequential[5:1])


#suppl, value-weighted
df.2016 = df.supp[,c("oc2016", "mgmfee2016", "execution2016", "research2016")]
df.2016$oc2016 = df.2016$oc2016 - df.2016$mgmfee2016
df.2016$execution2016 = df.2016$execution2016/(df.supp$fundsize2016*10)
df.2016$research2016 = df.2016$research2016/(df.supp$fundsize2016*10)
ll16 = lapply(df.2016, weighted.mean,  w = df.supp$fundsize2016, na.rm=T)
ll16 = data.frame("2016", 0, ll16)
colnames(ll16) = c("year", "tiar", "ocxfixed", "fixed", "txar", "extanr")
df.2017 = df.supp[,c("oc2017", "mgmfee2017", "execution2017", "research2017")]
df.2017$oc2017 = df.2017$oc2017 - df.2017$mgmfee2017
df.2017$execution2017 = df.2017$execution2017/(df.supp$fundsize2017*10)
df.2017$research2017 = df.2017$research2017/(df.supp$fundsize2017*10)
ll17 = lapply(df.2017, weighted.mean,  w = df.supp$fundsize2017, na.rm=T)
ll17 = data.frame("2017", 0, ll17)
colnames(ll17) = c("year", "tiar", "ocxfixed", "fixed", "txar", "extanr")
df.2018 = df.supp[,c("oc2018", "mgmfee2018", "execution2018", "research2018")]
df.2018$oc2018 = df.2018$oc2018 - df.2018$mgmfee2018
df.2018$execution2018 = df.2018$execution2018/(df.supp$fundsize2018*10)
df.2018$research2018 = df.2018$research2018/(df.supp$fundsize2018*10)
ll18 = lapply(df.2018[!is.na(df.supp$fundsize2018),], weighted.mean,  w = df.supp$fundsize2018[!is.na(df.supp$fundsize2018)], na.rm=T)
ll18 = data.frame("2018", 0, ll18)
colnames(ll18) = c("year", "tiar", "ocxfixed", "fixed", "txar", "extanr")
ll = rbind(ll16, ll17, ll18)
detach()
attach(ll)
ll = data.frame(year, fixed, ocxfixed, extanr, txar)

sequential.2 = brewer.pal(4, "RdYlGn")
#par(mfrow=c(1,2)) 
barplot(t(ll[,-1]),
        names.arg = ll$year, # x-axis labels
        cex.names = 0.7, # makes x-axis labels small enough to show all
        col = sequential.2, # colors
        xlab = "Year",
        ylab = "Costs, %", 
        las = 2,
        xlim = c(0,8), # these two lines allow space for the legend
        width = 1,
        main = "Suppl. sample ") # these two lines allow space for the legend
legend("right", cex = 0.7,  box.lty=0, inset = -0.3, title = "Cost ratios", c("Execution", "Research", "Bundled commissions", "OC, excl. mgmt fees", "Mgmt fees"), fill = sequential[5:1])
dev.off()
#Equally-weighted, suppl. sample
# ll16 = apply(df.2016, 2, mean, na.rm=T)
# ll16 = data.frame(2016, t(ll16))
# colnames(ll16) = c("year", "ocxfixed", "fixed", "txar", "extanr")
# ll17 = apply(df.2017, 2, mean, na.rm=T)
# ll17 = data.frame(2017, t(ll17))
# colnames(ll17) = c("year", "ocxfixed", "fixed", "txar", "extanr")
# ll18 = apply(df.2018, 2, mean, na.rm=T)
# ll18 = data.frame(2018, t(ll18))
# colnames(ll18) = c("year", "ocxfixed", "fixed", "txar", "extanr")
# 
# detach()
# ll = rbind(ll16, ll17, ll18)
# attach(ll)
# ll = data.frame(year, fixed=fixed, ocxfixed, extanr, txar)
# 
# par(mfrow=c(1,1))
# barplot(t(ll[,-1]),
#         names.arg = ll$year, # x-axis labels
#         cex.names = 0.7, # makes x-axis labels small enough to show all
#         col = sequential, # colors
#         xlab = "Year",
#         ylab = "Costs, %",
#         xlim = c(0,8), # these two lines allow space for the legend
#         width = 1) # these two lines allow space for the legend
# legend("right", title = "Cost ratios", c("Execution", "Research", "OC, excl. mgmt fees", "Mgmt fees"), fill = sequential[4:1])
#A line I wrote on my local computer
