library(dplyr)
library(knitr)
library(plotly)
library(plyr)
library(readODS)
library(fuzzyjoin)
Sys.setlocale("LC_ALL", 'en_US.UTF-8')

#import population data
#link to .ods file:http://ca.gov.taipei/public/Attachment/621617351230.ods
tpe1040<-read_ods("~/Downloads/621617351230.ods",col_names = FALSE)
tpe1<-tpe104[c("A", "G")] 
tpepop<-tpe1[c(5:16),] 
colnames(tpepop)<-c("Location","Population")

#import crime data
crime1040<-read.csv("http://data.moi.gov.tw/MoiOD/System/DownloadFile.aspx?DATA=DBC1AE97-3438-4580-963F-3B6FBFF5CBCD")
crime1041<-read.csv("http://data.moi.gov.tw/MoiOD/System/DownloadFile.aspx?DATA=7C69E281-DAC2-4DD9-BDE5-39A4D06B8A9F")
crime1042<-read.csv("http://data.moi.gov.tw/MoiOD/System/DownloadFile.aspx?DATA=BD117168-93CB-4871-A1E6-0C310C8A4045")
crime1043<-read.csv("http://data.moi.gov.tw/MoiOD/System/DownloadFile.aspx?DATA=B32FA7A5-0519-4287-BD1D-72F6159A3FFD")
crime104<-rbind.fill(crime1040,crime1041,crime1042,crime1043)
crime104$X<-NULL

#drug crimes dataframe 
drugs104<-data.frame(filter(crime104,案類=="毒品                          "))

#make it pretty
drugs104$案類<-NULL
colnames(drugs104)<-c("Date","Location")
drugs104$Date<-NULL
drugs104$Location<-as.character(drugs104$Location)

#rape dataframe
rape104<-data.frame(filter(crime104,案類=="強制性交"))

#make it pretty
rape104$案類<-NULL
colnames(rape104)<-c("Date","Location")
rape104$Date<-NULL
rape104$Location<-as.character(rape104$Location)

#totals dataframes
rapetotal<-as.data.frame(table(rape104))
drugstotal<-as.data.frame(table(drugs104))
colnames(rapetotal)<-c("Location","Frequency")
colnames(drugstotal)<-c("Location","Frequency")

#taipei only
taipeirape<-filter(rapetotal,grepl('台北市',Location))
taipeidrugs<-filter(drugstotal,grepl('台北市',Location))

#add population, remove duplicate cols, beautify
drugpop<-stringdist_left_join(taipeidrugs,tpepop,by='Location',max_dist=4)
rapepop<-test1<-stringdist_left_join(taipeirape,tpepop,by='Location',max_dist=4)
drugpop$Location.y=NULL
rapepop$Location.y=NULL
colnames(rapepop)<-c('Location','Frequency','Population')
colnames(drugpop)<-c('Location','Frequency','Population')
rapepop$Population <- gsub(",","",rapepop$Population)
drugpop$Population <- gsub(",","",drugpop$Population)
rapepop$Population<-as.numeric(rapepop$Population)
drugpop$Population<-as.numeric(drugpop$Population)
  
#function to compute rate/100,000
crimerate<-function(x,y) {
  small<- x/y
  big<-small*100000
final<-round(big,digits = 3)
  return(final)
}

#add rate/100,000 to tables 
rapepop$Rate<-crimerate(rapepop$Frequency,rapepop$Population)
drugpop$Rate<-crimerate(drugpop$Frequency,drugpop$Population)

#drug graph
drugx <- drugpop$Location
drugy <- drugpop$Rate
drugdata <- data.frame(drugx, drugy)

druggraph<- plot_ly(data, x = ~drugx, y = ~drugy, type = 'bar', 
                     text = y, textposition = 'auto',
                     marker = list(color = 'rgb(158,202,225)',
                                   line = list(color = 'rgb(8,48,107)', width = 1.5))) %>%
  layout(title = "Drug Crimes Per 100,000",
         xaxis = list(title = ""),
         yaxis = list(title = ""))
#rape graph
rapex <- rapepop$Location
rapey <- rapepop$Rate
rapedata <- data.frame(rapex, rapey)

rapegraph<- plot_ly(data, x = ~rapex, y = ~rapey, type = 'bar', 
                    text = y, textposition = 'auto',
                    marker = list(color = 'rgb(158,202,225)',
                                  line = list(color = 'rgb(8,48,107)', width = 1.5))) %>%
  layout(title = "Rapes Per 100,000",
         xaxis = list(title = ""),
         yaxis = list(title = ""))

#regress that!
regression<-lm(rapepop$Rate~drugpop$Rate)
summary(regression)
