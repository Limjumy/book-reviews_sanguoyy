
##统计每篇书评的词频
library(jiebaR)
## 将所有书评写入一个txt
##获取文本路径

reviewpath <- "E:/python_project/三国演义/演义书评/演义长评论"
completepath <- list.files(reviewpath, pattern = "*.txt$", full.names = TRUE)
read.txt <- function(x) {
  des <- readLines(x,encoding = 'UTF-8')
  return(paste(des, collapse = ""))
}
reviews <- lapply(completepath, read.txt)
reviews<-unlist(reviews)
reviews[1:4]

###正则表达式去html格式标签及空格标签
reviews<-gsub("<br>","",reviews)
reviews<-gsub("&nbsp;","",reviews)
reviews<-gsub("<p>","",reviews)
reviews<-gsub("<span>","",reviews)
reviews<-gsub("</p>","",reviews)
reviews<-gsub("</span>","",reviews)
reviews<-gsub("<.*div>","",reviews)

## 分词（以列表元素为单位）
cutter<-worker(bylines=TRUE,stop_word = "sanguo_stop.txt")

## 每篇评论的分词总数

review_each<-lapply(1:length(reviews),function(i) cutter[reviews[i]])# 每篇进行分词

v2=table(unlist(review_each[1]))

review_len<-lapply(1:length(review_each),function(i) length(unlist(review_each[i])))
review_len_<-unlist(review_len)##每篇评论词数
str(review_len_)
#is.list(review_len_)
#review_len_<-as.vector(review_len_)
write.table(review_len_,"演义每篇评论的分词数.csv",col.names = FALSE,row.names = F)


review1<-cutter[reviews[1]]
v=table((review1))
v=rev(sort(v))
View(v2)

## 计算每篇书评词语TF
str(review_each)
length(review_each)
review_each[[1]]
str(review_each[[1]])

## 模仿大音如霜情感分析中的TF方法
words_tf<-data.frame(review_len_)
words_tf$id<-c(1:544)
words_tf$total<-words_tf$review_len_ # total是每篇书评的词语总数

id <- rep(words_tf[, "id"], review_len_)

term<-unlist(review_each)


trainterm <- as.data.frame(cbind(id, term), stringsAsFactors = F)
# 去除“空格“词语



trainterm$tf <- rep(1, nrow(trainterm))# 添加辅助列 
#library(dplyr)
traintfidf <- aggregate(tf ~ id+term, data = trainterm, FUN = sum) 
# 按照id、term、label三列分组后对logic求和

# 此处为去除分词分出的空格和一些一个字的，取子集
traintfidf<-subset(traintfidf, nchar(as.character(traintfidf$term))>1)
traintfidf<-subset(traintfidf, (traintfidf$tf)>1)# tf为1的都去掉了


words_tf<-words_tf[,-1]
# 左关联
traintfidf_2 <- merge(traintfidf, words_tf)
write.csv(traintfidf_2,"tf_each.csv")

