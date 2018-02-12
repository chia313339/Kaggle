# Mercari Price Suggestion Challenge
# https://www.kaggle.com/c/mercari-price-suggestion-challenge/kernels
# 2/13前提交

# train_id or test_id - the id of the listing
# name - the title of the listing. Note that we have cleaned the data to remove text that look like prices (e.g. $20) to avoid leakage. These removed prices are represented as [rm]
# item_condition_id - the condition of the items provided by the seller
# category_name - category of the listing
# brand_name
# price - the price that the item was sold for. This is the target variable that you will predict. The unit is USD. This column doesn't exist in test.tsv since that is what you will predict.
# shipping - 1 if shipping fee is paid by seller and 0 by buyer
# item_description - the full description of the item. Note that we have cleaned the data to remove text that look like prices (e.g. $20) to avoid leakage. These removed prices are represented as [rm]



library(data.table) # Loading data
library(RColorBrewer) # wordcloud
library(wordcloud) # wordcloud


# 載入資料 沒加encoding在windows上category_name會出現亂碼
train<-fread(file = '~/R/K/train.tsv', sep = '\t', header = TRUE,encoding = 'UTF-8')

# 觀察資料
head(train)
str(train)
summary(train)

# 初步來看train_id 跟 name 可能沒有價值先略過
# 觀察item_condition_id 1跟3佔大多數
table(train$item_condition_id)
hist(x=train$item_condition_id, 
     main="Histogram of item_condition_id",         # 圖片的名稱
     xlab="item_condition_id",                      # X軸的名稱
     ylab="Frequency")

# category_name 需要進行文字探勘
category_list<-unlist(strsplit(train$category_name,'/'))
category_df<-as.data.frame(table(category_list))

wordcloud(category_df$category_list, category_df$Freq, min.freq =500, random.order = F, ordered.colors = F)




library(stringr)
library(dplyr) # data manipulation
str_split_fixed(train$category_name, '/', 4)

category = data.frame(train, str_split_fixed(train$category_name, '/', 4)) %>%
  mutate(cat1=X1, cat2=X2, cat3=X3, cat4=X4) %>% select(-X1, -X2, -X3, -X4)


sum(is.na(train$item_condition_id))




grep("/",train$category_name[3])
length(unlist(gregexpr("/",train$category_name[5])))


str_count(train$category_name, "/")

head(train$item_description,5)


plot(aov.shipping$model)
nchar(train$item_description)

df<-data.frame(item_description=train$item_description,nchar(train$item_description))

#---------------------------------------------------------------------------


category_list<-unlist(strsplit(train$category_name,'/'))
category_df<-as.data.frame(table(category_list))
head(category_df[order(category_df$Freq,decreasing = T),],10)



category<-unlist(strsplit(train$category_name[1],'/'))
length(category)

unique(category_list)
length(unique(category_list))

unique(category_list) %in% category

unique(category_list) %in% category

category_dummydf<-NULL
category_dummydf<-as.numeric(unique(category_list) %in% unlist(strsplit(train$category_name[1],'/')))


for (i in 1:5){
  category_dummydf<-rbind(category_dummydf,as.numeric(unique(category_list) %in% unlist(strsplit(train$category_name[i],'/'))))
}




idx<-sample(seq_len(nrow(data1)),20000,replace = F)


((1.96^2)*1488.885)/(0.5^2)

data_tmp<-NULL
for (i in 1:length(train_data$name)){
  data_tmp[i]<-length(unlist(strsplit(train_data$name[i],' ')))
}



# category_name：轉為稀疏矩陣 950個貼標
data_tmp<-NULL
# 將950個貼標類別轉為list
category_list<-unique(unlist(strsplit(train$category_name,'/')))
# 讓每個商品貼標對應list 回傳TRUE/FALSE 在轉為1/0
for (i in 1:length(train_data$category_name)){
  print(paste("第",i,"筆資料"))
  data_tmp<-rbind(data_tmp,as.numeric(unique(category_list) %in% unlist(strsplit(train$category_name[i],'/'))))
}
  
train_data<-cbind(data_tmp,data_tmp)


brand_name
brand_name_list<-unique(train$brand_name)
brand_name_df<-as.data.frame(table(train$brand_name))
brand_name_df[order(brand_name_df$Freq,decreasing = T),][1:20,]
brand_name_df[brand_name_df$Freq>50,]
summary(brand_name_df$Freq)

hist(log(brand_name_df$Freq+1))
hist(brand_name_df$Freq)

boxplot(brand_name_df$Freq)

brand_name_df<-brand_name_df[!brand_name_df$Var1=='',]


c<-c(1:100)
cut(c,3,c('A','B','C'))

24 %in% c(20,25)


grade<-NULL
cut<-exp(seq_len(10))-1
for (i in 1:nrow(brand_name_df)){
  if(brand_name_df$Freq[i]<cut[1]) grade[i]=1
  else if(brand_name_df$Freq[i]>=cut[1] & brand_name_df$Freq[i]<cut[2]) grade[i]=2
  else if(brand_name_df$Freq[i]>=cut[2] & brand_name_df$Freq[i]<cut[3]) grade[i]=3
  else if(brand_name_df$Freq[i]>=cut[3] & brand_name_df$Freq[i]<cut[4]) grade[i]=4
  else if(brand_name_df$Freq[i]>=cut[4] & brand_name_df$Freq[i]<cut[5]) grade[i]=5
  else if(brand_name_df$Freq[i]>=cut[5] & brand_name_df$Freq[i]<cut[6]) grade[i]=6
  else if(brand_name_df$Freq[i]>=cut[6] & brand_name_df$Freq[i]<cut[7]) grade[i]=7
  else if(brand_name_df$Freq[i]>=cut[7] & brand_name_df$Freq[i]<cut[8]) grade[i]=8
  else if(brand_name_df$Freq[i]>=cut[8] & brand_name_df$Freq[i]<cut[9]) grade[i]=9
  else if(brand_name_df$Freq[i]>=cut[9] & brand_name_df$Freq[i]<cut[10]) grade[i]=10
  else grade[i]=11
  print(i)
}




cut<-c(-Inf,exp(seq_len(10))-1,Inf)
brand_name_df$grade<-cut(brand_name_df$Freq,cut,label=c(seq(11)))
head(brand_name_df)

data_tmp<-NULL
# 將950個貼標類別轉為list
category_list<-unique(unlist(strsplit(train$category_name,'/')))
# 讓每個商品貼標對應list 回傳TRUE/FALSE 在轉為1/0
for (i in 1:length(train_data$category_name)){
  data_tmp<-rbind(data_tmp,as.numeric(unique(category_list) %in% unlist(strsplit(train$category_name[i],'/'))))
  print(i)
  }

write.csv(train_data,'kaggle.csv')

sum(is.na(train_data$brand_grade))



train_data$brand_grade.x[train_data$brand_grade.x=='3',]

table(train_data$brand_grade.x)


x <- cbind(c("Tom", "Joe", "Vicky"), c(27, 29, 28))
y <- cbind(c("Tom", "Joe", "Vicky"), c(178, 186, 168))
colnames(x) <- c("name", "age")
colnames(y) <- c("name", "tall")


merge(x, y, by = "name",all.x = TRUE)

x <- cbind(c("Tom", "Joe", "Vicky", "Bob"), c(27, 29, 28, 25))
y <- cbind(c("Tom", "Joe", "Vicky", "Bruce"), c(178, 186, 168, 170))
colnames(x) <- c("name", "age")
colnames(y) <- c("name", "tall")


merge(x, y, by = "name",all.x = TRUE)




sum(is.na(train_data))


train_data$brand_grade==1
train_data$brand_name

train_data$brand_grade[is.na(train_data$brand_grade)==T]='0'



train_data<-train_data[names(train_data)!='brand_grade']

train_data$brand_grade

str(train_data)
str(brand_name_df)

brand_name_df$brand_name<-as.character(brand_name_df$brand_name)
brand_name_df$brand_grade<-as.numeric(brand_name_df$brand_grade)


sum(is.na(train_data$brand_grade ))


table(train_data$brand_grade)
train_data<-cbind(train_data,category_name_tmp)




