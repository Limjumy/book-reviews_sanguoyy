
# 对书评（543条有内容）的集合做词频分析

library(jiebaR)
## 将所有书评写入一个txt
##获取文本路径
##reviewpath <- "H:/zimeiti/窥视数据背后的逻辑：基于R与python/bookwriting/第十二章舆情分析/rawdata/review_sentiment/train2"
# reviewpath <- "H:/zimeiti/窥视数据背后的逻辑：基于R与python/bookwriting/第十二章舆情分析/rawdata/review_sentiment/test2"
reviewpath <- "E:/python_project/三国演义/演义书评/演义长评论"
completepath <- list.files(reviewpath, pattern = "*.txt$", full.names = TRUE)
read.txt <- function(x) {
  des <- readLines(x,encoding = 'UTF-8')
  return(paste(des, collapse = ""))
}
reviews <- lapply(completepath, read.txt)
length(reviews)
writeLines(unlist(reviews),"reviews_yy.txt")

###正则表达式去html格式标签及空格标签
reviews<-gsub("<br>","",reviews)
reviews<-gsub("&nbsp;","",reviews)
reviews<-gsub("<p>","",reviews)
reviews<-gsub("<span>","",reviews)
reviews<-gsub("</p>","",reviews)
reviews<-gsub("</span>","",reviews)
reviews<-gsub("<.*div>","",reviews)

## 分词
cutter<-worker(bylines=TRUE,stop_word = "sanguo_stop.txt")
review_words<-cutter[reviews]
review_split<-sapply(review_words,paste,collapse=" ")
writeLines(review_split,"review_yy__split.txt")

## 将词语和词频变成列联表
v1=table(unlist(review_words))
View(v1)
v1=rev(sort(v1))
v1=v1[-1]
# 因为v1中有空格，和一个字的，想去掉
d=data.frame(v1)   #创建数据框
d=subset(d, nchar(as.character(d$Var1))>1 )# 筛选词汇大于1的
#d[1:2,]
## 话词云图
library(wordcloud2) #调入绘制词云的库
wordcloud2(d[1:200,])
