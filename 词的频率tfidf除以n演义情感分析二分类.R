
## 接三国演义
## TDIDF随机森林——情感分析 标签改为二分类


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

## 复制每篇评论词数长度的id
id <- rep(review[, "id"], review_len_)
label <- rep(review[, "label"], review_len_)
review_len2<-rep(review[, "review_len_"], review_len_)
review_len2<-as.numeric(review_len2)
# 词语展开
term<-unlist(review_each)


trainterm <- as.data.frame(cbind(id,label,term,review_len2), stringsAsFactors = F)
# 去空格
trainterm<-trainterm[grepl("\\S", trainterm$term),]



trainterm$tf <- rep(1, nrow(trainterm))# 添加辅助列 
## 按照id、term、label三列分组后对logic求和
traintfidf <- aggregate(tf ~ id+label+term+review_len2, data = trainterm, FUN = sum) 

# 此处为去除分词分出的空格和一些一个字的，取子集，觉得一个字与情感无关
#traintfidf<-subset(traintfidf, nchar(as.character(traintfidf$term))>1)
#traintfidf<-subset(traintfidf, (traintfidf$tf)>1)# tf为1的都去掉了

#traintfidf2<-traintfidf[grepl("\\S", traintfidf$term),]
#subset(traintfidf,traintfidf$term=="  ")


library(plyr)
library(dplyr)
total <- length(unique(traintfidf$id)) # 总文档数
temp <- data.frame(table(traintfidf$term)/total)
names(temp) <- c("term", "df") 
traintfidf <- left_join(traintfidf, temp)

## 计算IDF
temp <- data.frame(log(total/(table(traintfidf$term) + 1)))
names(temp) <- c("term", "idf") 
traintfidf <- left_join(traintfidf, temp) 
traintfidf$tfidf <- traintfidf$tf*traintfidf$idf
traintfidf$review_len2<-as.numeric(traintfidf$review_len2)

#traintfidf$freq<- traintfidf$tf/traintfidf$review_len2
traintfidf$tfidf2<- traintfidf$tf*traintfidf$idf/traintfidf$review_len2 ##除以每篇总词数
#traintfidf[is.na(subset(train, select = -label))]
# 查找数据哪里有空
traintfidf[!complete.cases(traintfidf),]
#write.csv(traintfidf,"traintfidf_sanguoyy_shuping.csv",row.names = F)


## 模型构建 改进的tfidf  词语频率（不是频数）##############
library(reshape2) 
train <- dcast(data = traintfidf, 
               id + label ~ term, sum, value.var = "tfidf2") 
# Error: std::bad_alloc 可能会内存不足
library(randomForest) 
row.names(train) <- train[, "id"]
train <- subset(train, select = -id) 

train$label <- as.factor(train$label)

# 查找数据哪里有空
#train[!complete.cases(train),]
#str(train)
Randommodel100 <- randomForest(x = subset(train, select = -label), y = train[, "label"], importance = TRUE, proximity = FALSE, ntree = 100)
#构建模型 
print(Randommodel100) 

#Call:
#  randomForest(x = subset(train, select = -label), y = train[,      "label"], ntree = 100, importance = TRUE, proximity = FALSE) 
#Type of random forest: classification
#Number of trees: 100
#No. of variables tried at each split: 188

#OOB estimate of  error rate: 44.92%
#Confusion matrix:
#  4   5 class.error
#4 78 159   0.6708861
#5 80 215   0.2711864




