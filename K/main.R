# Mercari Price Suggestion Challenge
# https://www.kaggle.com/c/mercari-price-suggestion-challenge/kernels
# 2/13�e����

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


# ���J��� �S�[encoding�bwindows�Wcategory_name�|�X�{�ýX
train<-fread(file = '~/R/K/train.tsv', sep = '\t', header = TRUE,encoding = 'UTF-8')

# �[����
head(train)
str(train)
summary(train)

# ��B�Ӭ�train_id �� name �i��S�����ȥ����L
# �[��item_condition_id 1��3���j�h��
table(train$item_condition_id)
hist(x=train$item_condition_id, 
     main="Histogram of item_condition_id",         # �Ϥ����W��
     xlab="item_condition_id",                      # X�b���W��
     ylab="Frequency")

# category_name �ݭn�i���r����
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


  