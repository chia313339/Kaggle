set.seed(3000)

library(data.table) # Loading data
library(stringr) 
library(parallel)
library(foreach)
library(doSNOW)

# fread是專門針對大量資料的語言，可以快速讀取百萬筆資料，sep = '\t'因為資料是用大空格分隔。
train<-fread("~/R/K/train.tsv", sep = '\t', header = TRUE,encoding = 'UTF-8')


#TRAIN 
n=22000
train_idx<-sample(seq_len(nrow(train)),n,replace = F)
train_data<-train[train_idx]

# name：轉單字字數
data_tmp<-NULL
for (i in 1:length(train_data$name)){
  data_tmp[i]<-length(unlist(strsplit(train_data$name[i],' ')))
}
train_data$name_num<-data_tmp

# item_condition_id：保持不變
# category_name：轉為稀疏矩陣 先加上貼標數
data_tmp<-NULL
for (i in 1:length(train_data$category_name)){
  data_tmp[i]<-length(unlist(strsplit(train_data$category_name[i],'/')))
}
train_data$category_name_cnt<-data_tmp

# category_name：轉為稀疏矩陣 950個貼標
category_name_tmp<-NULL
# 將950個貼標類別轉為list
category_list<-unique(unlist(strsplit(train$category_name,'/')))
# 讓每個商品貼標對應list 回傳TRUE/FALSE 在轉為1/0
cl <- makeCluster(4)
# detectCores()
registerDoSNOW(cl)
x <- foreach( i = 1:length(train_data$category_name), .combine='rbind') %do% as.numeric(category_list %in% unlist(strsplit(train_data$category_name[i],'/')))
stopCluster(cl)
category_name_tmp<-as.data.frame(x)
###
train_data<-cbind(train_data,category_name_tmp)

# 統計出每個品牌及數量
brand_name_df<-as.data.frame(table(train$brand_name))
# 將沒有品牌的欄位拿掉
brand_name_df<-brand_name_df[!brand_name_df$Var1=='',]

# 處理品牌等級
cut<-c(-Inf,exp(seq_len(10))-1,Inf)
brand_name_df$grade<-cut(brand_name_df$Freq,cut,label=c(seq(11)))

# 整理一下 把資料合併進去 並讓空值=0
brand_name_df<-data.frame(brand_name=as.character(brand_name_df$Var1),brand_grade=as.numeric(brand_name_df$grade))
train_data<-merge(train_data, brand_name_df, by = 'brand_name',all.x = TRUE)
train_data$brand_grade[is.na(train_data$brand_grade)==T]='0'
train_data$brand_grade<-as.numeric(train_data$brand_grade)

# item_description：轉為單字
data_tmp<-NULL
for (i in 1:length(train_data$item_description)){
  data_tmp[i]<-length(unlist(strsplit(train_data$item_description[i],' ')))
}
train_data$item_description_num<-data_tmp

# write.table(train_data,"~/R/MSPC.csv")

train_data<-read.csv("~/R/MSPC.csv", sep = ' ', header = TRUE)

sum(is.na(train_data$item_description))

###

Y = log(train_data$price+1)
col_list<-names(train_data) %in% c('brand_name','train_id','name','category_name','item_description','price')
train_data<-train_data[!col_list]
train_data<-train_data[,-c(1,2,3,5,6,8)]

train_data[] <- lapply(train_data, as.numeric)

# XGBOOST
require(xgboost)
require(Matrix)

boost <- xgboost(data = data.matrix(train_data), label = Y, max.depth = 16, eta = 0.1, print_every_n = 15, nthread = 4, nround = 1000, objective = "reg:linear")


##

log_answer <- predict(boost, data.matrix(train_data))

library(Metrics)
rmse(Y,log_answer)












####################################################################################
####################################################################################
####################################################################################
####################################################################################

set.seed(3000)

library(data.table) # Loading data
library(stringr) 

# fread是專門針對大量資料的語言，可以快速讀取百萬筆資料，sep = '\t'因為資料是用大空格分隔。
train<-fread("~/R/K/train.tsv", sep = '\t', header = TRUE,encoding = 'UTF-8')


#TRAIN 
n=10000
test_idx<-sample(seq_len(nrow(train)),n,replace = F)
test_data<-train[test_idx]

# name：轉單字字數
data_tmp<-NULL
for (i in 1:length(test_data$name)){
  data_tmp[i]<-length(unlist(strsplit(test_data$name[i],' ')))
}
test_data$name_num<-data_tmp

# item_condition_id：保持不變
# category_name：轉為稀疏矩陣 先加上貼標數
data_tmp<-NULL
for (i in 1:length(test_data$category_name)){
  data_tmp[i]<-length(unlist(strsplit(test_data$category_name[i],'/')))
}
test_data$category_name_cnt<-data_tmp

# category_name：轉為稀疏矩陣 950個貼標
category_name_tmp<-NULL
# 將950個貼標類別轉為list
category_list<-unique(unlist(strsplit(train$category_name,'/')))
# 讓每個商品貼標對應list 回傳TRUE/FALSE 在轉為1/0
cl <- makeCluster(4)
registerDoSNOW(cl)
x <- foreach( i = 1:length(test_data$category_name), .combine='rbind') %do% as.numeric(category_list %in% unlist(strsplit(test_data$category_name[i],'/')))
#stopCluster(cl)
category_name_tmp<-as.data.frame(x)
###
test_data<-cbind(test_data,category_name_tmp)

# 統計出每個品牌及數量
brand_name_df<-as.data.frame(table(train$brand_name))
# 將沒有品牌的欄位拿掉
brand_name_df<-brand_name_df[!brand_name_df$Var1=='',]

# 處理品牌等級
cut<-c(-Inf,exp(seq_len(10))-1,Inf)
brand_name_df$grade<-cut(brand_name_df$Freq,cut,label=c(seq(11)))

# 整理一下 把資料合併進去 並讓空值=0
brand_name_df<-data.frame(brand_name=as.character(brand_name_df$Var1),brand_grade=as.numeric(brand_name_df$grade))
test_data<-merge(test_data, brand_name_df, by = 'brand_name',all.x = TRUE)
test_data$brand_grade[is.na(test_data$brand_grade)==T]='0'
test_data$brand_grade<-as.numeric(test_data$brand_grade)

# item_description：轉為單字
data_tmp<-NULL
for (i in 1:length(test_data$item_description)){
  data_tmp[i]<-length(unlist(strsplit(test_data$item_description[i],' ')))
}
test_data$item_description_num<-data_tmp

# write.table(test_data,"~/R/MSPC.csv")

test_data<-read.csv("~/R/MSPC.csv", sep = ' ', header = TRUE)

sum(is.na(test_data$name_num))



###

Y = log(test_data$price+1)

col_list<-names(test_data) %in% c('brand_name','train_id','name','category_name','item_description','price')
test_data<-test_data[!col_list]
test_data<-test_data[,-c(1,2,3,5,6,8)]

test_data[] <- lapply(test_data, as.numeric)

# XGBOOST
require(xgboost)
require(Matrix)

##

log_answer <- predict(boost, data.matrix(test_data))

library(Metrics)
rmse(Y,log_answer)









