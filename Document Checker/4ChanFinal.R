library(tidyverse)
library(readtext)
library(rvest)
library(tm)
####functions####
getContent<-function(url,nodeName){
  tryCatch(
    url %>%
      as.character() %>% 
      read_html() %>% 
      html_nodes(nodeName) %>% 
      html_text(), 
    error = function(e){NA}    
  )
}
clean_corpus <- function(corpus){
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, stopwords("en"))
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, stripWhitespace)
  return(corpus)
}
InputToCorpus<-function(input){
  s1<-VectorSource(input)
  s2<-VCorpus(s1)
  s3<-clean_corpus(s2)
}
assocToDf<-function(x,word,limit){
  df1<-data.frame(findAssocs(x,word,limit))
  df2<-data.frame(df1,term=rownames(df1))
  df3<-data.frame(df2,row.names = 1:nrow(df2))
}
findOverlaps<-function(term){
  UserAssoc<<-tryCatch(assocToDf(userTDM,term,0.3),error = function(e){NA} )
  if(is.na(UserAssoc)==FALSE){
    GivenAssoc<<-assocToDf(chan_one_sparse,term,0.3)
    user_given_difference<<-data.frame(setdiff(UserAssoc$term,GivenAssoc$term))
    overlapDF<<-inner_join(UserAssoc,GivenAssoc,by="term")
    colnames(overlapDF)<-c("UserAssociation","term","ControlAssociation")
    overlapDF<- overlapDF[,c(2,3,1)]
    overlapDF<<-mutate(overlapDF,difference=UserAssociation-ControlAssociation)}
  else{
    print("No associations found for that term. Try a different term or a bigger document")
  }
} 
####fourchan####
#generate and clean 4chan corpus
fourchanDF<-read.csv("~/Desktop/4chantext.csv")
fourchanDF$X<-NULL
clean_chan<-InputToCorpus(fourchanDF$text)
chan_one<-TermDocumentMatrix(clean_chan)
chan_one_sparse <- removeSparseTerms(chan_one, 0.99)
####user inputs####
userURL<-c("http://boards.4chan.org/pol/")
userNode<-c('.postMessage')
userTerm<-c("white")
####text mining####
#get DF
userDF<-getContent(userURL,userNode)
#check DF
if(is.na(userDF)==TRUE){
  print("Please check your URL and node input")
} else{
  print("looks good!")
}
#build corpus
userCorpus<-InputToCorpus(userDF)
userTDM<-TermDocumentMatrix(userCorpus)
#find overlaps
findOverlaps(userTerm)
#explain overlaps
if(exists("UserAssoc")==FALSE){
  warning("no associations found")
}else if (((nrow(user_given_difference)==nrow(UserAssoc))==TRUE&(str_detect(userURL,"chan")==TRUE))) {
  print("That's a chan document alright, but I don't see any overlaps. Try a different term or a larger document")
} else if (nrow(user_given_difference)==nrow(UserAssoc)) {
  print("Theres no overlap between those documents for that term")
} else {
  print(arrange(overlapDF,desc(difference)))
}



