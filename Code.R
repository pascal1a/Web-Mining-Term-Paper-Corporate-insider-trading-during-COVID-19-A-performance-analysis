# install and load the required packages

install.packages("httr")
install.packages("rvest")
install.packages("stringr")
install.packages("tidyverse")
install.packages("writexl")
install.packages("glue")
install.packages("scales")
install.packages("plyr")
install.packages("tidyquant")
install.packages("dplyr")
install.packages("readr")
install.packages("EventStudy")
install.packages("reshape2")
install.packages("data.table")
install.packages("readxl")

library(httr)
library(rvest)
library(stringr)
library(tidyverse)
library(writexl)
library(glue)
library(scales)
library(plyr)
library(EventStudy)
library(tidyquant)
library(dplyr)
library(readr)
library(reshape2)
library(data.table)
library(readxl)


##################### Part 1: Data collection I #############################



# writing a function to directly modify the URL
# allows to directly filter the desired SEC Form 4 filings and load it into a data table 
data_gathering_function <-function(per_page,page_number, start_day, start_month, start_year, end_day, end_month, end_year, purchase, sale, volume, ownership_change ){

session <- html_session(glue(paste("http://openinsider.com/screener?s=&o=&pl=&ph=&ll=&lh=&fd=-1",
                             "&fdr={start_month}%2F{start_day}%2F{start_year}+-+{end_month}%2F{end_day}%2F{end_year}",
                             "&td=-1&tdr={start_month}%2F{start_day}%2F{start_year}+-+{end_month}%2F{end_day}%2F{end_year}",
                             "&fdlyl=&fdlyh=&daysago=&xp={purchase}&xs={sale}&vl={volume}&vh=&ocl=&och={ownership_change}",
                             "&sic1=-1&sicl=100&sich=9999&iscob=1&isceo=1&ispres=1&iscoo=1&iscfo=1&istenpercent=1&grp=0&nfl=",
                             "&nfh=&nil=&nih=&nol=&noh=&v2l=&v2h=&oc2l=&oc2h=&sortcol=0&cnt={per_page}&page={page_number}", sep = "")))

df_list_new <- html_table(session, T, T, T)
df_list_new <-data.frame(df_list_new[12])

return (df_list_new)
}

# call the function with the desired input parameters, load it into a table and format it 
table= data_gathering_function(1000,1, 20,01,2020, 19,02,2020, 0, 1, 50, -5)
table$Filing.Date <- as.Date(table$Filing.Date)
table$Value=gsub("[^0-9.-]", "", table$Value)
table$Value <- as.numeric(table$Value)


##################### Part 2: Data collection II #############################



# set the time period and the unique tickers
# remove transactions with no stock data available via tidyquant
ticker <- c(unique(table$Ticker))
startDate <- "2018-05-01"
endDate <- "2020-04-30"
elements_2_remove = c("KRTX","AAXN", "IBKC", "GPAQ", "ACIA", "TXN","CRWD","TW","SWAV","DT","CSTL","CFB","ADPT","ZM","PHR","FSLY","TPTX","PINS","RVLV","SILK","LEVI","ONTO","REAL","LYFT","BBIO","AKRO", "DDOG", "MDLA", "UBER", "ALEC", "WORK", "GO", "MNTA", "FTSV", "HIIQ","FDEF", "TSCT", "PUB","GSB","MSG","VRTU", "ETFC", "UTX", "TRWH","CBAI","BAS", "DNKN","VSLR", "EIGI", "ZAYO", "MCC", "MLNX","BRPM", "NBL","RTRX","MYOK", "FSCT")
ticker =ticker[!(ticker %in% elements_2_remove)]
firmSymbols <-ticker
firmNames <-ticker

# collect the daily stock prices for all the tickers via tidyquant (from yahoo finance)
# format the data accordingly to use them in the event study
firmSymbols %>% 
  tidyquant::tq_get(from = startDate, to = endDate) %>% 
  dplyr::mutate(date = format(date, "%d.%m.%Y")) -> firmData
knitr::kable(head(firmData), pad=0)


# collect the daily stock prices from the market index (Russell 2000)
# format the data accordingly to use them in the event study
indexSymbol <- c("^RUT")
indexName <- c("RUT")
indexSymbol %>% 
  tidyquant::tq_get(from = startDate, to = endDate) %>% 
  dplyr::mutate(date = format(date, "%d.%m.%Y")) -> indexData
indexData$symbol <- "RUT"
knitr::kable(head(indexData), pad=0)


# The following event study package requires 3 csv files formatted in a specific way, which will be sent to the server via the API.
# The three files are as follows: "01_requestFile.csv","02_firmDataPrice.csv" and "03_marketDataPrice.csv". 
# These files are created in the next step
firmData %>% 
  dplyr::select(symbol, date, adjusted) %>% 
  readr::write_delim(path      = "02_firmDataPrice.csv", 
                     delim     = ";", 
                     col_names = F)

indexData %>% 
  dplyr::select(symbol, date, adjusted) %>% 
  readr::write_delim(path      = "03_marketDataPrice.csv", 
                     delim     = ";", 
                     col_names = F)



group <- c(rep("ONE", 312), rep("Other", 0))#can be ignored, since no groups are analyzed
request <- cbind(c(1:312), firmSymbols, rep(indexName, 312), rep("20.02.2020", 312), group, rep(-10, 312), rep(30, 312), rep(-11, 312), rep(250, 312))
request %>% 
  as.data.frame() %>% 
  readr::write_delim("01_requestFile.csv", delim = ";", col_names = F)


##################### Part 3: Event Study #############################



# For the event study the APi from https://www.eventstudytools.com/ is used.
key <- "bdeb8606bbab73a4b8dca51a4a48b52f"
est <- EventStudyAPI$new()
est$authentication(apiKey = key)


# get & set parameters for the abnormal return Event Study
# As a benchmark model the market model is used (mm)
esaParams <- EventStudy::ARCApplicationInput$new()
esaParams$setResultFileType("csv")
esaParams$setBenchmarkModel("mm")

dataFiles <- c("request_file" = "01_requestFile.csv",
               "firm_data"    = "02_firmDataPrice.csv",
               "market_data"  = "03_marketDataPrice.csv")

# check all data files
EventStudy::checkFiles(dataFiles)

# perform the Event Study
arEventStudy <- est$performEventStudy(estParams     = esaParams, 
                                      dataFiles     = dataFiles, 
                                      downloadFiles = T)


#look at the results
knitr::kable(head(arEventStudy$arResults))
knitr::kable(head(arEventStudy$aarResults))
knitr::kable(head(arEventStudy$carResults))
knitr::kable(head(arEventStudy$aarStatistics))
#environment variable
arEventStudy 


##################### Part 4: Plots #############################



#plot the default Abnormal and average abnormal return plots
arPlot(arEventStudy, xlab = "Event window",ncol = 4, addAAR = TRUE, facetVar = "Group")
aarPlot(arEventStudy, group = "ONE", window = c(-10,30), facet = T, ncol = 4, cumSum = F,xlab = "Event window")



#Calculate the CAAR and plot it
df1 <- read.csv("C:/Users/pasca/Desktop/Uni/R-Web mining/results/ar_results.csv", sep=";", header= TRUE)
df1 <- mutate_all(df1, function(x) as.numeric(as.character(x)))
Stats <- summarize_all(df1, mean)
DFnew <- rbind(df1, Stats)
car <-DFnew[313, 1:42]
car[nrow(car)+1,] <- NA
car[2,1] <- 0
car[2,2] <- car[1,2]
for (loop in (2:ncol(car)-1))
{
  car[2,1+loop] <- car[2,0+loop]+car[1,1+loop]
  
}
x_axis = c(-10,-9,-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30)
plot(x_axis, car[2,2:42],xlab="Event Time", ylab="CAAR")
lines(x_axis, car[2,2:42])
title("CAAR")


# scatter plot for the insider sales over time
table <- table[ ! table$Ticker %in% elements_2_remove, ]
base_breaks <- function(n = 10){
  function(x) {
    axisTicks(log10(range(x, na.rm = TRUE)), log = TRUE, n = n)
  }
}

plot1 <-ggplot(table, aes(x=Filing.Date, y=-Value)) +geom_point()+expand_limits(y = c(0,NA))+
  scale_y_continuous(trans = log_trans(), breaks = base_breaks(),labels = scales::comma) + 
  theme(panel.grid.minor = element_blank()) +ggtitle("Insider Sales")+ labs(x="Filing Date", y = "Volume (in USD)")+
  theme(plot.title = element_text(hjust = 0.5))
plot1


#Number of transactions analyzed by group and volumes
table$Title <-gsub("(.*),.*", "\\1", table$Title)
table$unique1 <-1
groupColumns = c("Title")
dataColumns = c("unique1", "Value")
plot2 = ddply(table, groupColumns, function(x) colSums(x[dataColumns]))
plot2

res[order(res$unique1,decreasing = TRUE ),]
sum(table$Value)

# plot for the Percentage of transactions with significant negative abnormal returns

Overview <- read_excel("Overview.xlsx")
plot3<-ggplot(data=Overview, aes(x=Count, y=Percentage_pos))+
  geom_bar(stat="identity")+
ggtitle("Percentage of transactions with significant negative abnormal returns (alpha = 5%)")+ labs(x="Event Window", y = "Percentage of Transactions")+
  theme(plot.title = element_text(hjust = 0.5))
plot3

#Plot for the illustration of the different event windows
RUT <- read.csv("C:/Users/pasca/Desktop/Uni/R-Web mining/^RUT.csv")
RUT <- RUT[,colnames(RUT) != "Adj.Close"]
plot(RUT$Close,main=paste("Closing prices of SP"))


eventwindow1 <- data.frame(xstart = as.POSIXct('2020-02-20 00:00:00'),
                           xend = as.POSIXct('2020-03-20 00:00:00'))
eventwindow2<- data.frame(xstart2 = as.POSIXct('2020-02-10 00:00:00'),
                          xend2 = as.POSIXct('2020-02-20 00:00:00'))
estimationwindow<- data.frame(xstart3 = as.POSIXct('2020-01-01 00:00:00'),
                              xend3 = as.POSIXct('2020-02-10 00:00:00'))

RUT$Date <- as.Date(RUT$Date)
RUT$Date <- as.POSIXct(RUT$Date, format = '%d%b%Y:%H:%M:%S')

dateRanges <- data.frame(
  start = seq(as.POSIXct("2020-01-01 00:00:00"), as.POSIXct("2020-02-01 00:00:00"), "1 year"),
  end = seq(as.POSIXct("2020-02-02 00:00:00"), as.POSIXct("2021-02-01 00:00:00"), "1 year")
)
ggplot(RUT) +
  geom_rect(data = eventwindow1, aes(xmin = xstart , xmax = xend, ymin = -Inf, ymax = Inf),
            inherit.aes=FALSE, alpha = 0.3, fill = c("black"))+
  geom_rect(data = eventwindow2, aes(xmin = xstart2 , xmax = xend2, ymin = -Inf, ymax = Inf),
            inherit.aes=FALSE, alpha = 0.5, fill = c("black"))+
  geom_rect(data = estimationwindow, aes(xmin = xstart3 , xmax = xend3, ymin = -Inf, ymax = Inf),
            inherit.aes=FALSE, alpha = 0.6, fill = c("black"))+
  geom_line(aes(x=  Date, y = Close), size = 1.5)+
  ggtitle("Event Study")+ labs(x="Date", y = "Russel 2000 Index")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(limits = c(1000, 1900))+
  scale_x_datetime(expand=c(0,0))+
  annotate(geom="text",x=as.POSIXct("2020-01-20 00:00:00"),
           y=1800,label="Estimation Window",fontface="bold")+
  
  annotate(geom="text",x=as.POSIXct("2020-02-10 00:00:00"),
           y=1800,label="Event\n  -10 days",fontface="bold")+
  
  annotate(geom="text",x=as.POSIXct("2020-03-20 00:00:00"),
           y=1800,label="Event + 30 days",fontface="bold")+
  annotate(geom="text",x=as.POSIXct("2020-02-20 00:00:00"),
           y=1800,label="Event",fontface="bold") 



##################### Part 5: Sensitivity analysis #############################


# New event window ------------------------------------------------------------------------------------------------------------
# Run the event study again but this time for the event window -10 to +10 days
# Code is identical except for line 255 where rep(30,312) is replaced by rep(10,312)

indexSymbol <- c("^RUT")
indexName <- c("RUT")
indexSymbol %>% 
  tidyquant::tq_get(from = startDate, to = endDate) %>% 
  dplyr::mutate(date = format(date, "%d.%m.%Y")) -> indexData
indexData$symbol <- "RUT"
knitr::kable(head(indexData), pad=0)


# The following event study package requires 3 csv files formatted in a specific way, which will be sent to the server via the API.
# The three files are as follows: "01_requestFile.csv","02_firmDataPrice.csv" and "03_marketDataPrice.csv". 
# These files are created in the next step
firmData %>% 
  dplyr::select(symbol, date, adjusted) %>% 
  readr::write_delim(path      = "02_firmDataPrice.csv", 
                     delim     = ";", 
                     col_names = F)

indexData %>% 
  dplyr::select(symbol, date, adjusted) %>% 
  readr::write_delim(path      = "03_marketDataPrice.csv", 
                     delim     = ";", 
                     col_names = F)


group <- c(rep("ONE", 312), rep("Other", 0))#can be ignored, since no groups are analyzed
request <- cbind(c(1:312), firmSymbols, rep(indexName, 312), rep("20.02.2020", 312), group, rep(-10, 312), rep(10, 312), rep(-11, 312), rep(250, 312))
request %>% 
  as.data.frame() %>% 
  readr::write_delim("01_requestFile.csv", delim = ";", col_names = F)


# For the event study the APi from https://www.eventstudytools.com/ is used.
key <- "bdeb8606bbab73a4b8dca51a4a48b52f"
est <- EventStudyAPI$new()
est$authentication(apiKey = key)


# get & set parameters for the abnormal return Event Study
# As a benchmark model the market model is used (mm)
esaParams <- EventStudy::ARCApplicationInput$new()
esaParams$setResultFileType("csv")
esaParams$setBenchmarkModel("mm")

dataFiles <- c("request_file" = "01_requestFile.csv",
               "firm_data"    = "02_firmDataPrice.csv",
               "market_data"  = "03_marketDataPrice.csv")

# check all data files
EventStudy::checkFiles(dataFiles)

# perform the Event Study
arEventStudy <- est$performEventStudy(estParams     = esaParams, 
                                      dataFiles     = dataFiles, 
                                      downloadFiles = T)


#look at the results
knitr::kable(head(arEventStudy$arResults))
knitr::kable(head(arEventStudy$aarResults))
knitr::kable(head(arEventStudy$carResults))
knitr::kable(head(arEventStudy$aarStatistics))
#environment variable
arEventStudy

# New market index Russell 3000 ------------------------------------------------------------------------------------------------------------


# Run the event study again but this time with the russell 3000 as a market index
# Code is identical except for line 299 where ^RUT is replaced by  ^RUA

indexSymbol <- c("^RUA")
indexName <- c("RUA")
indexSymbol %>% 
  tidyquant::tq_get(from = startDate, to = endDate) %>% 
  dplyr::mutate(date = format(date, "%d.%m.%Y")) -> indexData
indexData$symbol <- "RUA"
knitr::kable(head(indexData), pad=0)


# The following event study package requires 3 csv files formatted in a specific way, which will be sent to the server via the API.
# The three files are as follows: "01_requestFile.csv","02_firmDataPrice.csv" and "03_marketDataPrice.csv". 
# These files are created in the next step
firmData %>% 
  dplyr::select(symbol, date, adjusted) %>% 
  readr::write_delim(path      = "02_firmDataPrice.csv", 
                     delim     = ";", 
                     col_names = F)

indexData %>% 
  dplyr::select(symbol, date, adjusted) %>% 
  readr::write_delim(path      = "03_marketDataPrice.csv", 
                     delim     = ";", 
                     col_names = F)


group <- c(rep("ONE", 312), rep("Other", 0))#can be ignored, since no groups are analyzed
request <- cbind(c(1:312), firmSymbols, rep(indexName, 312), rep("20.02.2020", 312), group, rep(-10, 312), rep(30, 312), rep(-11, 312), rep(250, 312))
request %>% 
  as.data.frame() %>% 
  readr::write_delim("01_requestFile.csv", delim = ";", col_names = F)


# For the event study the APi from https://www.eventstudytools.com/ is used.
key <- "bdeb8606bbab73a4b8dca51a4a48b52f"
est <- EventStudyAPI$new()
est$authentication(apiKey = key)


# get & set parameters for the abnormal return Event Study
# As a benchmark model the market model is used (mm)
esaParams <- EventStudy::ARCApplicationInput$new()
esaParams$setResultFileType("csv")
esaParams$setBenchmarkModel("mm")

dataFiles <- c("request_file" = "01_requestFile.csv",
               "firm_data"    = "02_firmDataPrice.csv",
               "market_data"  = "03_marketDataPrice.csv")

# check all data files
EventStudy::checkFiles(dataFiles)

# perform the Event Study
arEventStudy <- est$performEventStudy(estParams     = esaParams, 
                                      dataFiles     = dataFiles, 
                                      downloadFiles = T)


#look at the results
knitr::kable(head(arEventStudy$arResults))
knitr::kable(head(arEventStudy$aarResults))
knitr::kable(head(arEventStudy$carResults))
knitr::kable(head(arEventStudy$aarStatistics))
#environment variable
arEventStudy



# New market index S&P  500 ------------------------------------------------------------------------------------------------------------


# Run the event study again but this time with the S&P 500 as a market index
# Code is identical except for line 373 where ^RUT is replaced by ^GSPC

indexSymbol <- c("^GSPC")
indexName <- c("S&P_500")
indexSymbol %>% 
  tidyquant::tq_get(from = startDate, to = endDate) %>% 
  dplyr::mutate(date = format(date, "%d.%m.%Y")) -> indexData
indexData$symbol <- "S&P_500"
knitr::kable(head(indexData), pad=0)


# The following event study package requires 3 csv files formatted in a specific way, which will be sent to the server via the API.
# The three files are as follows: "01_requestFile.csv","02_firmDataPrice.csv" and "03_marketDataPrice.csv". 
# These files are created in the next step
firmData %>% 
  dplyr::select(symbol, date, adjusted) %>% 
  readr::write_delim(path      = "02_firmDataPrice.csv", 
                     delim     = ";", 
                     col_names = F)

indexData %>% 
  dplyr::select(symbol, date, adjusted) %>% 
  readr::write_delim(path      = "03_marketDataPrice.csv", 
                     delim     = ";", 
                     col_names = F)


group <- c(rep("ONE", 312), rep("Other", 0))#can be ignored, since no groups are analyzed
request <- cbind(c(1:312), firmSymbols, rep(indexName, 312), rep("20.02.2020", 312), group, rep(-10, 312), rep(30, 312), rep(-11, 312), rep(250, 312))
request %>% 
  as.data.frame() %>% 
  readr::write_delim("01_requestFile.csv", delim = ";", col_names = F)


# For the event study the APi from https://www.eventstudytools.com/ is used.
key <- "bdeb8606bbab73a4b8dca51a4a48b52f"
est <- EventStudyAPI$new()
est$authentication(apiKey = key)


# get & set parameters for the abnormal return Event Study
# As a benchmark model the market model is used (mm)
esaParams <- EventStudy::ARCApplicationInput$new()
esaParams$setResultFileType("csv")
esaParams$setBenchmarkModel("mm")

dataFiles <- c("request_file" = "01_requestFile.csv",
               "firm_data"    = "02_firmDataPrice.csv",
               "market_data"  = "03_marketDataPrice.csv")

# check all data files
EventStudy::checkFiles(dataFiles)

# perform the Event Study
arEventStudy <- est$performEventStudy(estParams     = esaParams, 
                                      dataFiles     = dataFiles, 
                                      downloadFiles = T)


#look at the results
knitr::kable(head(arEventStudy$arResults))
knitr::kable(head(arEventStudy$aarResults))
knitr::kable(head(arEventStudy$carResults))
knitr::kable(head(arEventStudy$aarStatistics))
#environment variable
arEventStudy















