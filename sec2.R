#!/usr/bin/Rscript
library(TTR)
library(quantmod)
library(data.table)
library(lubridate)

args<-commandArgs(trailingOnly = TRUE)
if(length(args)<2){stop('<> marketcapmillion adv-million (cutoffs) 800 10 ')}  




#rm(list=ls(all=TRUE))
date_<-Sys.Date()
#date_<-as.Date(args[1],format="%Y-%m-%d")
#yahootickerlist<-function(ticlist,date_){
print('ticlist is small case')
print('<> mcmillion advmillion (cutoffs) ')  
### date_ is the date_ on which you want the return and abnormal volume ranking
st<-stockSymbols(exchange = c("NASDAQ", "NYSE","NYSEMKT"),sort.by = c("Exchange", "Symbol"), quiet = FALSE)
# st[st$tic=='spy','Sector']<-'Needed'   ## otherwise thrown out
st<-st[-c( intersect(         which(is.na(st$Sector)),which(is.na(st$Industry))              )) , ]
st<-st[!is.na(st$LastSale),]
st<-st[!is.na(st$MarketCap),]
#st<-st[st$LastSale>=5,]
colnames(st)[1]<-'tic'
head(st)
st$IPOyear[is.na(st$IPOyear)]<-2000
nrow(st)
#st<-st[st$IPOyear<2014,]

#?(Sys.Date())
convertBM<-function(in_){
  
  in_<-as.character(in_)
  
  tmp<-strsplit(in_,"")[[1]]
  
  if(tmp[length(tmp)]=="M"){as.numeric(paste(tmp[2:(length(tmp)-1)],collapse=""))}else{
    
    if(tmp[length(tmp)]=="B"){1000*as.numeric(paste(tmp[2:(length(tmp)-1)],collapse=""))
      
    }else{return(0)}
    
  }
  
}
st$tic<-tolower(st$tic)
st$MarketCap1<-sapply(st$MarketCap,convertBM)
st<-st[st$MarketCap1>=as.numeric(args[1]),]
#st<-st[st$MarketCap1>=800,]
print(args[1]) 




#'tivo' %in% st$tic  
head(st)
nrow(st)

## capture short term momentum 
### 1 5 10 day returns

## capture short term momentum 
### 1 3 5 day adv compared to 20 day adv


#tic<-'tivo'
#date_<-Sys.Date()
##set standard
res<-try(x<-getSymbols('aapl',auto.assign=F,from=date_-41,to=date_))

x<-tail(x,20)
start_<-index(x)[1]

GG<-length(lastret<-diff(log(as.numeric(x[,6]))))
#dummy<-rep(0,GG)
##set standard
tic='hmsy'
adv<-function(tic,GG){
  print(tic)
  #res<-try(x<-getSymbols(as.character(tic),auto.assign=F,from=date_-21,to=date_))
  res<-try(x<-getSymbols(as.character(tic),auto.assign=F,from=start_,to=date_))
  
  dummy<-NA
  if(class(res)[1]=='try-error'){return(list(dummy))}
  head(x)
  
  ## this is 20 day adv
  adv<-floor(mean(as.numeric(x[,5]*x[,6])/1e6,na.rm=T))
  
  x$adv.ratio<-floor(as.numeric(x[,5]*x[,6])/1e6)/adv
  
  
  lastret<-( (as.numeric(x[,6]))-(as.numeric(x[,1])))/(as.numeric(x[,1]))
  
  prices<-as.numeric(x[,6])
  
  x$returns<-c(0,round(diff(log(prices)),2))
  
  head(x)
  
  if( length(lastret)!= (GG+1) ){return(dummy)}
  a=sum(as.numeric(tail(x,1)$returns))
  b=sum(as.numeric(tail(x,4)$returns))
  c=sum(as.numeric(tail(x,9)$returns))

  e=sum(as.numeric(tail(x,1)$adv.ratio))/1
  f=sum(as.numeric(tail(x,3)$adv.ratio))/3
  g=sum(as.numeric(tail(x,5)$adv.ratio))/5
  

 
  obj<-c(a,(b-a)/3,(c-(a+b))/5,e,f,g)
    
  e=sum(as.numeric(tail(x,1)$adv.ratio))/1
  f=sum(as.numeric(tail(x,3)$adv.ratio))/3
  g=sum(as.numeric(tail(x,5)$adv.ratio))/5
  
  obj<-as.numeric(Map(function(x) round(x,2) ,obj))
  
  
  ifelse(adv>=as.numeric(args[2]),return(obj),return(dummy) )
  #ifelse(adv>=10,return(obj),return(dummy) )
  
}


adv('hmsy',GG)
st<-st[order(st$tic),]
print("Running ADV check $10M $ per day cutoff and last return")
#if(!missing(ticlist)){st<-st[st$tic %in% ticlist ,]}
#x<-sapply(st$tic[1:10],adv,GG=GG)
#st<-st[1:20,]
x<-sapply(st$tic,adv,GG=GG)
#x1<-x
x1<-data.frame(t(data.frame(x)))
x1$tic<-rownames(x1)
x1<-data.table(x1)
#x1<-data.frame(oneDayRet=as.numeric(x),tic=names(x)    )
#colnames(x1)<-'oneDayRet'
#x1$tic<-names(x)
st<-data.table(st)
setkey(st,tic)
setkey(x1,tic)

#colnames(x1)<-c('ret1','ret3','ret5','advratio1','advratio3','advratio5','tic')
st1<-merge(st,x1)
head(st1)
st2<-st1[complete.cases(st1),]

rank<-data.table(scale(st2[,c('X1','X2','X3','X4','X5','X6'),with=FALSE]))
rank[rank>2]<- 2
rank[rank<-2]<- -2


st2$score<-apply(rank,1,sum)
#st1<-st1[order(-st1$oneDayRet),]


length(unique(st2$tic))
#st1<-data.table(st1)

#st1[st1$tic=='msg',]


#st1[st1$tic=='tumi',]


nrow(st2)
st2$idx<-1:nrow(st2)


c<-st2[,list(idx=min(idx)),by='tic']

setkey(st2,idx)
setkey(c,idx)


st3<-merge(st2,c)
st3$idx<-1:nrow(st3)
colnames(st3)[2]<-'tic'

if(length(unique(st3$tic))!=nrow(st3)){print('Does not pass uniqueness test')}else{print('Passes uniqueness test')}
st3<-st3[order(-st3$score),]
print(head(st3,20))
write.csv(st3,'investableUniverse.csv',row.names=FALSE)
