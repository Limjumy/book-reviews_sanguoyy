
##未完成
## 将书评字数作为指标，随机森林模型，二分类情感分析

library(jiebaR)
## 将所有书评写入一个txt
##获取文本路径

reviewpath <- "E:/文本挖掘/三国/演义书评/演义长评论/"
completepath <- list.files(reviewpath, pattern = "*.txt$", full.names = TRUE)# 读取其绝对路径
read.txt <- function(x) {
  des <- readLines(x,encoding = 'UTF-8')
  return(paste(des, collapse = ""))
}
reviews <- lapply(completepath, read.txt)
reviews<-unlist(reviews)
reviews[1:4]

docname <- list.files(reviewpath, pattern = "*.txt$")# 读取文件名

###正则表达式去html格式标签及空格标签
reviews<-gsub("<br>","",reviews)
reviews<-gsub("&nbsp;","",reviews)
reviews<-gsub("<p>","",reviews)
reviews<-gsub("<span>","",reviews)
reviews<-gsub("</p>","",reviews)
reviews<-gsub("</span>","",reviews)
reviews<-gsub("<.*div>","",reviews)
reviews<-gsub("\\t","",reviews)

reviews<-gsub("[a-zA-Z]","",reviews)
reviews<-gsub("[1-9]","",reviews)
reviews<-gsub("\\.\\.","",reviews)
reviews<-gsub("[[:digit:]]","",reviews)

#a<-c(' ',"a")
#b<-c(1,2)
#x<-data.frame(a,b)

#x<-x[grepl("\\S", x$a),]

reviewdf <- as.data.frame(cbind(docname,reviews),stringsAsFactors = F)
colnames(reviewdf) <- c("docuname", "reviews")
reviewdf$id=c(1:534)

# 读标签id和label
d=read.csv("评价等级2.csv",header = F)
d=as.data.frame(d,col.names=F)

colnames(d) <- c("bianhao", "label")
d$id<-c(1:534)
d$label[d$label==3]=4 #改成二分类

review<-merge(reviewdf,d,by="id")
#write.csv(review,"reviews.csv")
colnames(review) <- c("id", "document","reviews","bianhao","label")

# 分词（以列表元素为单位）
cutter<-worker(bylines=TRUE)# stop_word = "sanguo_stop.txt"

## 每篇评论的分词总数
review_each<-lapply(1:length(review$reviews),function(i) cutter[review$reviews[i]])# 每篇进行分词
review_each[[1]]


##每篇评论词数
review_len<-lapply(1:length(review_each),function(i) length(unlist(review_each[i])))
review_len_<-unlist(review_len)

review$review_len_<-review_len_


