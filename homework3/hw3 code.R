library(readstata13)
library(Hmisc)
library(BSDA)
library(stargazer)
library(xtable)
Sys.setlocale("LC_ALL", 'en_US.UTF-8')
##import##
stata<-read.dta13("~/Desktop/內容分析法.dta")
working<-stata

##cleaning##
working$new<-working$interval
working$new[working$new != 0] <- 1
working$interval<-factor(working$interval)
working$sex<-factor(working$sex)
colnames(working)[8] <- "anyIssue"

cols<-c("聳動字眼", "過度主觀陳述",   
        "刻意擷取字句","強調色情內容", "強調非重點內容","anyIssue")
working[cols] <- lapply(working[cols], factor)

##regression##
bigRegress<-lm(留言數 ~ 聳動字眼 + 過度主觀陳述+   
               刻意擷取字句+強調色情內容+ 強調非重點內容, data = working)
summary(bigRegress)

##anova##
fitAny <- aov( 留言數 ~ 聳動字眼 + 過度主觀陳述+   
                刻意擷取字句+強調色情內容+ 強調非重點內容, data=working)
summary(fitAny)
