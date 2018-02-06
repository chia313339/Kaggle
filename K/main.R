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

train$brand_name


plot(aov.shipping$model)


  
