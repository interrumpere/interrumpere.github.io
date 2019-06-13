library(tidyverse)
library(readtext)
library(rvest)
library(tm)
#user inputs
userURL<-c("http://boards.4chan.org/pol/")
userNode<-c('.postMessage')
userTerm<-c("white")
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
BigramTokenizer <- function(x){
  unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)}
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
    warning("No associations found for that term. Try a different term or a bigger document")
  }
} 
####fourchan####
#pull urls
archive_df<-getContent('http://boards.4chan.org/pol/archive','.quotelink')

#clean urls
urls_df<-as.data.frame(str_sub(archive_df$archive_text,29))
names(urls_df)<-c("url_list")
urls_df<-as.data.frame(str_sub(urls_df$url_list,,-11))
names(urls_df)<-c("url_list")
urls_df$url_list<-paste(c("http://boards.4chan.org/"),urls_df$url_list,sep='')
#pull text
fourchantext_1<-sapply(urls_df$url_list,getContent,'.postMessage')
#corpuses
clean_chan<-InputToCorpus(fourchantext_1)
clean_chan<-tm_map(clean_chan,removeWords,drop_Words)
chan_one<-TermDocumentMatrix(clean_chan)
chan_one_sparse <- removeSparseTerms(chan_one, 0.99)
####text mining####
#user inputs
userURL<-c("input a url")
userNode<-c("input a node")
userTerm<-c("input a term")
#get DF
userDF<-getContent(userURL,userNode)
#check DF
if(is.na(userDF)==TRUE){
  warning("Please check your URL and node input")
} else{
  View(userDF)
}
#build corpus
userCorpus<-InputToCorpus(userDF)
userTDM<-TermDocumentMatrix(userCorpus)
#find overlaps
findOverlaps(userTerm)
#explain overlaps
if (((nrow(user_given_difference)==nrow(userAssoc))==TRUE&(str_detect(userURL,"chan")==TRUE))) {
  print("That's a chan document alright, but I don't see any overlaps. Try a different term or a larger document")
} else if (nrow(user_given_difference)==nrow(userAssoc)) {
  print("Theres no overlap between those documents for that term")
} else {
  View(overlapDF)
}

