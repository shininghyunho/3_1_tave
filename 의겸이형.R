#### 정치현상 R 빅데이터
rm(list=ls())
#install.packages("readxl")
library(dplyr)
library(readxl)

##### 데이터 로드
cheongwon <- read_excel("data.xlsx")
head(cheongwon)
summary(cheongwon)
str(cheongwon)
max(cheongwon$REC)

cheongwon$REC[is.na(cheongwon$REC)] <- 150.3
cheongwon$REC[is.na(cheongwon$REC)]

table(cheongwon$MONTH)
##### 추천 수 별로 데이터 분할
cheongwon <- cheongwon %>%
  mutate(counter = ifelse(REC>= 100, 
                          ifelse(REC>=500,
                                 ifelse(REC>=1000,
                                        ifelse(REC>=2000,
                                               ifelse(REC>=3000,
                                                      ifelse(REC>=5000,
                                                             ifelse(REC>=10000,
                                                                    ifelse(REC>=30000,
                                                                           ifelse(REC>=50000,
                                                                                  ifelse(REC>=100000, 
                                                                                         ifelse(REC>=200000, "200000~", "100000~200000"), "50000~100000") , "30000~50000") , "10000~30000")
                                                                    , "5000~10000"), "3000~5000"), "2000~3000"), "1000~2000"), "500~1000"), "100~500"), "~100"))

table(cheongwon$counter)
ggplot(data = cheongwon, aes(x = counter)) + geom_col

ggplot(data = R1, aes(x= CATEGORY, y = sum)) +
  geom_col(width=0.5, fill="skyblue2") +
  labs(title = "월별 카테고리에 따른 동의 인원 수") +
  theme(axis.text.x = element_text(angle=75, vjust=0.6))

##### Date 데이터 쪼개기
head(cheongwon$DATE)
#for(i in 1: nrow(cheongwon)) {
#  strDate <- unlist(strsplit(cheongwon$DATE[i], "-")) # -를 기준으로 날짜 나누기
#  cheongwon$Month[i]=strDate[2] # 나눈 값들 중 월에 해당하는 2번째 값을 Month로 저장
#}
cheongwon$Month <- substr(cheongwon$DATE, 4,5)
table(cheongwon$Month)




############## 빈출 키워드
#install.packages("rJava")
#install.packages("memoise")
#install.packages("stringr")
library("KoNLP")
library("dplyr")
library("stringr")
useNIADic()
head(cheongwon)
head(cheongwon$TITLE)
cheongwon$Clean_T <-str_replace_all(cheongwon$TITLE, "\\W", " ") ## \\W는 특수문자를 의미하는 정ㄱ표현식으로 특수문자를 모두 지움 
cheongwon$Clean_T[is.na(cheongwon$Clean_T)] <- 0
nouns <- extractNoun(cheongwon$Clean_T)
wordcount <- table(unlist(nouns))
df_word <- as.data.frame(wordcount, stringsAsFactors = F)
head(df_word)
df_word <- rename(df_word, word = Var1, freq = Freq)
df_word <- filter(df_word, nchar(word)>=2)
top30 <- df_word %>%
  arrange(desc(freq)) %>%
  head(30)

top30[1,] = NA
top30
top30[2,] = NA
top30[3,] = NA
top30[5,] = NA
top30[8,] = NA
top30[18,] = NA
top30[22,] = NA
top30[12,] = NA
top30[23,] = NA
top30[24,] = NA
top20 <- top30 %>%
  filter(!is.na(top30$word))
top20
#### 워드 클라우드 만들기
#install.packages("wordcloud")
library(wordcloud)
library(RColorBrewer)

pal <- brewer.pal(8, "Dark2")
set.seed(1234)

df_word <- df_word %>%
  filter(!df_word$word == "청원",!df_word$word == "해주",!df_word$word == "국민",!df_word$word == "대통령",!df_word$word == "처벌",!df_word$word == "대한민국",!df_word$word == "관련",!df_word$word == "문제",!df_word$word == "한국",!df_word$word == "국가", !is.na(df_word$word))

wordcloud(words = df_word$word,
          freq = df_word$freq,
          min.freq = 2,
          max.words = 200,
          random.order = F,
          rot.per = .1,
          scale = c(3, 0.2),
          colors = pal)


#### 워드 클라우드 2 만들기
#install.packages("wordcloud2")
library(wordcloud2)
df_word2 <- as.matrix(df_word)
wordcloud2(df_word2, size = 10, rotateRatio = 10000)

## 단어 빈도 막대 만들기
library(ggplot2)

order <- arrange(top20, freq)$word

ggplot(data = top20, aes(x = word, y = freq)) +
  ylim(0,10000) +
  geom_col() +
  coord_flip() +
  scale_x_discrete(limit = order) +   #빈도순 막대 정렬
  geom_text(aes(label = freq), hjust = -0.3)+  # 빈도 표시
  labs(title = 'top20 words',
       caption="source: 청와대 국민청원")


#### 월별 빈출 키워드 
table(cheongwon$Month)
cheongwon$Month = NULL
table(cheongwon$MONTH)
cheongwon <- rename(cheongwon, MONTH = Month)
cheongwon$Month <- as.integer(cheongwon$Month)
str(cheongwon$MONTH)
Jan <- cheongwon %>%
  filter(cheongwon$MONTH == "18.01.")
Feb <- cheongwon %>%
  filter(cheongwon$MONTH == "18.02.")
Mar <- cheongwon %>%
  filter(cheongwon$MONTH == "18.03.")
Apr <- cheongwon %>%
  filter(cheongwon$MONTH == "18.04.")
May <- cheongwon %>%
  filter(cheongwon$MONTH == "18.05.")
Jun <- cheongwon %>%
  filter(cheongwon$MONTH == "18.06.")
Jul <- cheongwon %>%
  filter(cheongwon$MONTH == "18.07.")
Aug <- cheongwon %>%
  filter(cheongwon$MONTH == "18.08.")
Oct <- cheongwon %>%
  filter(cheongwon$MONTH == "17.10.")
Nov <- cheongwon %>%
  filter(cheongwon$MONTH == "17.11.")
Dec <- cheongwon %>%
  filter(cheongwon$MONTH == "17.12.")
Sep <- cheongwon %>%
  filter(cheongwon$MONTH == "17.09.")

head(Jan)
Jan_nouns <- extractNoun(Jan$Clean_T)
Feb_nouns <- extractNoun(Feb$Clean_T)
Mar_nouns <- extractNoun(Mar$Clean_T)
Apr_nouns <- extractNoun(Apr$Clean_T)
May_nouns <- extractNoun(May$Clean_T)
Jun_nouns <- extractNoun(Jun$Clean_T)
Jul_nouns <- extractNoun(Jul$Clean_T)
Aug_nouns <- extractNoun(Aug$Clean_T)
Oct_nouns <- extractNoun(Oct$Clean_T)
Nov_nouns <- extractNoun(Nov$Clean_T)
Dec_nouns <- extractNoun(Dec$Clean_T)
Sep_nouns <- extractNoun(Sep$Clean_T)

wordcount_1 <- table(unlist(Jan_nouns))
wordcount_2 <- table(unlist(Feb_nouns))
wordcount_3 <- table(unlist(Mar_nouns))
wordcount_4 <- table(unlist(Apr_nouns))
wordcount_5 <- table(unlist(May_nouns))
wordcount_6 <- table(unlist(Jun_nouns))
wordcount_7 <- table(unlist(Jul_nouns))
wordcount_8 <- table(unlist(Aug_nouns))
wordcount_9 <- table(unlist(Sep_nouns))
wordcount_10 <- table(unlist(Oct_nouns))
wordcount_11 <- table(unlist(Nov_nouns))
wordcount_12 <- table(unlist(Dec_nouns))

df_word1 <- as.data.frame(wordcount_1, stringsAsFactors = F)
df_word2 <- as.data.frame(wordcount_2, stringsAsFactors = F)
df_word3 <- as.data.frame(wordcount_3, stringsAsFactors = F)
df_word4 <- as.data.frame(wordcount_4, stringsAsFactors = F)
df_word5 <- as.data.frame(wordcount_5, stringsAsFactors = F)
df_word6 <- as.data.frame(wordcount_6, stringsAsFactors = F)
df_word7 <- as.data.frame(wordcount_7, stringsAsFactors = F)
df_word8 <- as.data.frame(wordcount_8, stringsAsFactors = F)
df_word9 <- as.data.frame(wordcount_9, stringsAsFactors = F)
df_word10 <- as.data.frame(wordcount_10, stringsAsFactors = F)
df_word11 <- as.data.frame(wordcount_11, stringsAsFactors = F)
df_word12 <- as.data.frame(wordcount_12, stringsAsFactors = F)


df_word1 <- filter(df_word1, nchar(Var1)>=2)
df_word2 <- filter(df_word2, nchar(Var1)>=2)
df_word3 <- filter(df_word3, nchar(Var1)>=2)
df_word4 <- filter(df_word4, nchar(Var1)>=2)
df_word5 <- filter(df_word5, nchar(Var1)>=2)
df_word6 <- filter(df_word6, nchar(Var1)>=2)
df_word7 <- filter(df_word7, nchar(Var1)>=2)
df_word8 <- filter(df_word8, nchar(Var1)>=2)
df_word9 <- filter(df_word9, nchar(Var1)>=2)
df_word10 <- filter(df_word10, nchar(Var1)>=2)
df_word11 <- filter(df_word11, nchar(Var1)>=2)
df_word12 <- filter(df_word12, nchar(Var1)>=2)

top20_1 <- df_word1 %>%
  arrange(desc(Freq)) %>%
  head(20)
top20_2 <- df_word2 %>%
  arrange(desc(Freq)) %>%
  head(20)
top20_3 <- df_word3 %>%
  arrange(desc(Freq)) %>%
  head(20)
top20_4 <- df_word4 %>%
  arrange(desc(Freq)) %>%
  head(20)
top20_5 <- df_word5 %>%
  arrange(desc(Freq)) %>%
  head(20)
top20_6 <- df_word6 %>%
  arrange(desc(Freq)) %>%
  head(20)
top20_7 <- df_word7 %>%
  arrange(desc(Freq)) %>%
  head(20)
top20_8 <- df_word8 %>%
  arrange(desc(Freq)) %>%
  head(20)
top20_9 <- df_word9 %>%
  arrange(desc(Freq)) %>%
  head(20)
top20_10 <- df_word10 %>%
  arrange(desc(Freq)) %>%
  head(20)
top20_11 <- df_word11 %>%
  arrange(desc(Freq)) %>%
  head(20)
top20_12 <- df_word12 %>%
  arrange(desc(Freq)) %>%
  head(20)
order1 <- arrange(top20_1, Freq)$Var1
order2 <- arrange(top20_2, Freq)$Var1
order3 <- arrange(top20_3, Freq)$Var1
order4 <- arrange(top20_4, Freq)$Var1
order5 <- arrange(top20_5, Freq)$Var1
order6 <- arrange(top20_6, Freq)$Var1
order7 <- arrange(top20_7, Freq)$Var1
order8 <- arrange(top20_8, Freq)$Var1
order9 <- arrange(top20_9, Freq)$Var1
order10 <- arrange(top20_10, Freq)$Var1
order11 <- arrange(top20_11, Freq)$Var1
order12 <- arrange(top20_12, Freq)$Var1


ggplot(data = top20_1, aes(x = Var1, y = Freq)) +
  ylim(0,10000) +
  geom_col() +
  coord_flip() +
  scale_x_discrete(limit = order1) +   #빈도순 막대 정렬
  geom_text(aes(label = Freq), hjust = -0.3)  # 빈도 표시



ggplot(data = top20_2, aes(x = Var1, y = Freq)) +
  ylim(0,10000) +
  geom_col() +
  coord_flip() +
  scale_x_discrete(limit = order2) +   #빈도순 막대 정렬
  geom_text(aes(label = Freq), hjust = -0.3)  # 빈도 표시

ggplot(data = top20_3, aes(x = Var1, y = Freq)) +
  ylim(0,10000) +
  geom_col() +
  coord_flip() +
  scale_x_discrete(limit = order3) +   #빈도순 막대 정렬
  geom_text(aes(label = Freq), hjust = -0.3)  # 빈도 표시

ggplot(data = top20_4, aes(x = Var1, y = Freq)) +
  ylim(0,10000) +
  geom_col() +
  coord_flip() +
  scale_x_discrete(limit = order4) +   #빈도순 막대 정렬
  geom_text(aes(label = Freq), hjust = -0.3)  # 빈도 표시

ggplot(data = top20_5, aes(x = Var1, y = Freq)) +
  ylim(0,10000) +
  geom_col() +
  coord_flip() +
  scale_x_discrete(limit = order5) +   #빈도순 막대 정렬
  geom_text(aes(label = Freq), hjust = -0.3)  # 빈도 표시

ggplot(data = top20_6, aes(x = Var1, y = Freq)) +
  ylim(0,10000) +
  geom_col() +
  coord_flip() +
  scale_x_discrete(limit = order6) +   #빈도순 막대 정렬
  geom_text(aes(label = Freq), hjust = -0.3)  # 빈도 표시

ggplot(data = top20_7, aes(x = Var1, y = Freq)) +
  ylim(0,10000) +
  geom_col() +
  coord_flip() +
  scale_x_discrete(limit = order7) +   #빈도순 막대 정렬
  geom_text(aes(label = Freq), hjust = -0.3)  # 빈도 표시

ggplot(data = top20_8, aes(x = Var1, y = Freq)) +
  ylim(0,10000) +
  geom_col() +
  coord_flip() +
  scale_x_discrete(limit = order8) +   #빈도순 막대 정렬
  geom_text(aes(label = Freq), hjust = -0.3)  # 빈도 표시

ggplot(data = top20_9, aes(x = Var1, y = Freq)) +
  ylim(0,10000) +
  geom_col() +
  coord_flip() +
  scale_x_discrete(limit = order9) +   #빈도순 막대 정렬
  geom_text(aes(label = Freq), hjust = -0.3)  # 빈도 표시

ggplot(data = top20_10, aes(x = Var1, y = Freq)) +
  ylim(0,10000) +
  geom_col() +
  coord_flip() +
  scale_x_discrete(limit = order10) +   #빈도순 막대 정렬
  geom_text(aes(label = Freq), hjust = -0.3)  # 빈도 표시

ggplot(data = top20_11, aes(x = Var1, y = Freq)) +
  ylim(0,10000) +
  geom_col() +
  coord_flip() +
  scale_x_discrete(limit = order11) +   #빈도순 막대 정렬
  geom_text(aes(label = Freq), hjust = -0.3)  # 빈도 표시

ggplot(data = top20_12, aes(x = Var1, y = Freq)) +
  ylim(0,10000) +
  geom_col() +
  coord_flip() +
  scale_x_discrete(limit = order12) +   #빈도순 막대 정렬
  geom_text(aes(label = Freq), hjust = -0.3)  # 빈도 표시

########청원 수 별 카테고리
rm(list=setdiff(ls(), "cheongwon"))
table(cheongwon$CATEGORY)
theme_set(theme_bw())

## set the levels in order we want
cheongwon <- within(cheongwon, 
                    CATEGORY <- factor(CATEGORY, 
                                       levels=names(sort(table(CATEGORY), 
                                                         decreasing=TRUE))))
# Draw plot
ggplot(cheongwon, aes(x = cheongwon$CATEGORY)) + 
  geom_bar(width=.5, fill="tomato3") + 
  labs(title="Ordered Bar Chart", 
       subtitle="카테고리 별 청원 수", 
       caption="source: 청와대 국민청원",
       xlab = "Category") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6))



#### 청원 수 별 ID
## set the levels in order we want
cheongwon <- within(cheongwon, 
                    ID <- factor(ID, 
                                 levels=names(sort(table(ID), 
                                                   decreasing=TRUE))))
# Draw plot
ggplot(cheongwon, aes(x = cheongwon$ID)) + 
  geom_bar(width=.5, fill="dodgerblue1") + 
  labs(title="Ordered Bar Chart", 
       subtitle="아이디 별 청원 수", 
       caption="source: 청와대 국민청원",
       xlab = "ID") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6))

#### 월별 청원 수
# Draw plot
ggplot(cheongwon, aes(x = cheongwon$MONTH)) + 
  geom_bar(width=.5, fill="skyblue2") + 
  labs(title="Bar Chart", 
       subtitle="월별 청원 수", 
       caption="source: 청와대 국민청원") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6))



########## pivot table
str(cheongwon$MONTH)
cheongwon$MONTH <- as.character(cheongwon$MONTH)
cheongwon$MONTH <- as.numeric(cheongwon$MONTH)

data_REC <- as.data.frame(cheongwon[cheongwon$MONTH == "18.01.",])
######## 월별 카테고리에 따른 동의 인원 
R1 <- data_REC %>%
  group_by(CATEGORY) %>%
  summarise(sum = sum(REC)) %>%
  select(CATEGORY, sum) %>%
  arrange(sum)


ggplot(data = R1, aes(x= reorder(CATEGORY,-sum), y = sum)) +
  geom_col(width=0.5, fill="skyblue2") +
  labs(title = "월별 카테고리에 따른 동의 인원 수",
       subtitle = "18년 1월") +
  theme(axis.text.x = element_text(angle=75, vjust=0.6))


library("tidyr")

#Create data
set.seed(112)
a <- spread(summarise(group_by(cheongwon, MONTH, CATEGORY), sum=sum(REC)), CATEGORY, sum) 
a$MONTH <- NULL
a <- as.matrix(a)
colnames(ta) = c("1월", "2월", "3월", "4월", "5월", "6월", "7월", "8월", "9월", "10월", "11월", "12월" )
ta <- t(a) ##-- i.e.,  a[i,j] == ta[j,i] for all i,j :
for(j in seq(ncol(a)))
  if(! a[,j] == ta[j,]) stop("wrong transpose")

b <- cbind(a$정치개혁, a$`인권/성평등`, a$`안전/환경`, a$`외교/통일/국방`)
colnames(b) = c("정치개혁", "인권/성평등", "안전/환경", "외교/통일/국방")
rownames(b) = c("1월", "2월", "3월", "4월", "5월", "6월", "7월", "8월", "9월", "10월", "11월", "12월" )
b <- as.matrix(b)
tb <- t(b) ##-- i.e.,  a[i,j] == ta[j,i] for all i,j :
for(j in seq(ncol(b)))
  if(! a[,j] == ta[j,]) stop("wrong transpose")


# Grouped barplot
barplot(ta, col=colors()[c(1,2,3,4,5,6,7,8,9,10,11,12)] , border="white", font.axis=4, beside=T, legend=rownames(ta), xlab="Category", font.lab=2)

barplot(tb, col=c(2,89,12,23) , border="white", font.axis=4, beside=T, xlab="Category", font.lab=2)+
  legend("topleft",cex=0.7,  rownames(tb), fill = c(2,89,12,23))



#############################################################################spread로 피벗테이블을 얻었지만 아직 차트로 만들지는 못했음.

# Grouped barplot
barplot(a, border="white", font.axis=2, beside=T, legend=rownames(a), xlab="group", font.lab=2)
######### 월별 카테고리에 따른 청원 게시글 수
#Create data
set.seed(112)
data_cou <- cheongwon %>%
  group_by(CATEGORY) %>%
  summarize(length(which(MONTH == 18.01)))



data2=matrix(sample(1:30,15) , nrow=3)
data2
colnames(data)=c("A","B","C","D","E")
rownames(data)=c("var1","var2","var3")
data

# Get the stacked barplot
barplot(data, col=colors()[c(23,89,12)] , border="white", space=0.04, font.axis=2, xlab="group")

# Grouped barplot
barplot(data, col=colors()[c(23,89,12)] , border="white", font.axis=2, beside=T, legend=rownames(data), xlab="group", font.lab=2)


##### 동의 구간 별 카테고리 비교
table(cheongwon$counter)
REC_100 <- cheongwon %>% 
  group_by(CATEGORY) %>%
  summarize(count =length(which(counter == "~100"))) %>%
  arrange(desc(count, CATEGORY))

ggplot(data = REC_100, aes(x = CATEGORY, y = count)) + geom_col()+ 
  labs(title="동의 수 ~100 구간 청원 수", 
       caption="source: 청와대 국민청원") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6))

REC_500 <- cheongwon %>% 
  group_by(CATEGORY) %>%
  summarize(count =length(which(counter == "100~500"))) %>%
  arrange(desc(count, CATEGORY))

ggplot(data = REC_500, aes(x = reorder(CATEGORY,-count), y = count)) + geom_col()+ 
  labs(title="동의 수 100~500 구간 청원 수", 
       caption="source: 청와대 국민청원") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6))

REC_1000 <- cheongwon %>% 
  group_by(CATEGORY) %>%
  summarize(count =length(which(counter == "500~1000"))) %>%
  arrange(desc(count, CATEGORY))

ggplot(data = REC_1000, aes(x = reorder(CATEGORY,-count), y = count)) + geom_col()+ 
  labs(title="동의 수 500~1000 구간 청원 수", 
       caption="source: 청와대 국민청원") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6))

REC_2000 <- cheongwon %>% 
  group_by(CATEGORY) %>%
  summarize(count =length(which(counter == "1000~2000"))) %>%
  arrange(desc(count, CATEGORY))

ggplot(data = REC_2000, aes(x = reorder(CATEGORY,-count), y = count)) + geom_col()+ 
  labs(title="동의 수 1000~2000 구간 청원 수", 
       caption="source: 청와대 국민청원") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6))


REC_3000 <- cheongwon %>% 
  group_by(CATEGORY) %>%
  summarize(count =length(which(counter == "2000~3000"))) %>%
  arrange(desc(count, CATEGORY))

ggplot(data = REC_3000, aes(x = reorder(CATEGORY,-count), y = count)) + geom_col()+ 
  labs(title="동의 수 2000~3000 구간 청원 수", 
       caption="source: 청와대 국민청원") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6))


REC_5000 <- cheongwon %>% 
  group_by(CATEGORY) %>%
  summarize(count =length(which(counter == "3000~5000"))) %>%
  arrange(desc(count, CATEGORY))

ggplot(data = REC_5000, aes(x = reorder(CATEGORY,-count), y = count)) + geom_col()+ 
  labs(title="동의 수 3000~5000 구간 청원 수", 
       caption="source: 청와대 국민청원") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6))


REC_10000 <- cheongwon %>% 
  group_by(CATEGORY) %>%
  summarize(count =length(which(counter == "5000~10000"))) %>%
  arrange(desc(count, CATEGORY))

ggplot(data = REC_10000, aes(x = reorder(CATEGORY,-count), y = count)) + geom_col()+ 
  labs(title="동의 수 5000~10000 구간 청원 수", 
       caption="source: 청와대 국민청원") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6))


REC_30000 <- cheongwon %>% 
  group_by(CATEGORY) %>%
  summarize(count =length(which(counter == "10000~30000"))) %>%
  arrange(desc(count, CATEGORY))

ggplot(data = REC_30000, aes(x = reorder(CATEGORY,-count), y = count)) + geom_col()+ 
  labs(title="동의 수 10000~30000 구간 청원 수", 
       caption="source: 청와대 국민청원") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6))


REC_50000 <- cheongwon %>% 
  group_by(CATEGORY) %>%
  summarize(count =length(which(counter == "30000~50000"))) %>%
  arrange(desc(count, CATEGORY))

ggplot(data = REC_50000, aes(x = reorder(CATEGORY,-count), y = count)) + geom_col()+ 
  labs(title="동의 수 30000~50000 구간 청원 수", 
       caption="source: 청와대 국민청원") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6))


REC_100000 <- cheongwon %>% 
  group_by(CATEGORY) %>%
  summarize(count =length(which(counter == "50000~100000"))) %>%
  arrange(desc(count, CATEGORY))

ggplot(data = REC_100000, aes(x = reorder(CATEGORY,-count), y = count)) + geom_col()+ 
  labs(title="동의 수 50000~100000 구간 청원 수", 
       caption="source: 청와대 국민청원") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6))


REC_200000 <- cheongwon %>% 
  group_by(CATEGORY) %>%
  summarize(count =length(which(counter == "100000~200000"))) %>%
  arrange(desc(count, CATEGORY))

ggplot(data = REC_200000, aes(x = reorder(CATEGORY,-count), y = count)) + geom_col()+ 
  labs(title="동의 수 100000~200000 구간 청원 수", 
       caption="source: 청와대 국민청원") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6))


REC_300000 <- cheongwon %>% 
  group_by(CATEGORY) %>%
  summarize(count =length(which(counter == "200000~"))) %>%
  arrange(desc(count, CATEGORY))

ggplot(data = REC_300000, aes(x = reorder(CATEGORY,-count), y = count)) + geom_col()+ 
  labs(title="동의 수 200000~ 구간 청원 수", 
       caption="source: 청와대 국민청원") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6))

#######월별 키워드의 일별 분포를 보여 줌

Jan_day1 <- Jan[grep("가상", Jan$TITLE),]
qplot(Jan_day1$DATE, width=0.5) +
  theme(axis.text.x = element_text(angle=75, vjust=0.6)) +
  labs(title = '1월_"가상"')

Jan_day2 <- Jan[grep("화폐", Jan$TITLE),]
qplot(Jan_day2$DATE, width=0.5) +
  theme(axis.text.x = element_text(angle=75, vjust=0.6)) +
  labs(title = '1월_"화폐"')

Jan_day3 <- Jan[grep("거래", Jan$TITLE),]
qplot(Jan_day3$DATE, width=0.5) +
  theme(axis.text.x = element_text(angle=75, vjust=0.6)) +
  labs(title = '1월_"거래"')


Feb_day1 <- Feb[grep("파면", Feb$TITLE),]
qplot(Feb_day1$DATE, width=0.5) +
  theme(axis.text.x = element_text(angle=75, vjust=0.6)) +
  labs(title = '2월_"파면"')


Feb_day2 <- Feb[grep("판사", Feb$TITLE),]
qplot(Feb_day2$DATE, width=0.5) +
  theme(axis.text.x = element_text(angle=75, vjust=0.6)) +
  labs(title = '2월_"판사"')


Feb_day3 <- Feb[grep("정현식", Feb$TITLE),]
qplot(Feb_day3$DATE, width=0.5) +
  theme(axis.text.x = element_text(angle=75, vjust=0.6)) +
  labs(title = '2월_"정현식"')


Mar_day1 <- Mar[grep("미세먼지", Mar$TITLE),]
qplot(Mar_day1$DATE, width=0.5) +
  theme(axis.text.x = element_text(angle=75, vjust=0.6)) +
  labs(title = '3월_"미세먼지"')


Mar_day2 <- Mar[grep("여성", Mar$TITLE),]
qplot(Mar_day2$DATE, width=0.5) +
  theme(axis.text.x = element_text(angle=75, vjust=0.6)) +
  labs(title = '3월_"여성"')


Mar_day3 <- Mar[grep("미투", Mar$TITLE),]
qplot(Mar_day3$DATE, width=0.5) +
  theme(axis.text.x = element_text(angle=75, vjust=0.6)) +
  labs(title = '3월_"미투"')

Apr_day1 <- Apr[grep("국회의원", Apr$TITLE),]
qplot(Apr_day1$DATE, width=0.5) +
  theme(axis.text.x = element_text(angle=75, vjust=0.6)) +
  labs(title = '4월_"국회의원"')

Apr_day2 <- Apr[grep("대한항공", Apr$TITLE),]
qplot(Apr_day2$DATE, width=0.5) +
  theme(axis.text.x = element_text(angle=75, vjust=0.6)) +
  labs(title = '4월_"대한항공"')

Apr_day3 <- Apr[grep("공매", Apr$TITLE),]
qplot(Apr_day3$DATE, width=0.5) +
  theme(axis.text.x = element_text(angle=75, vjust=0.6)) +
  labs(title = '4월_"공매"')

May_day1 <- May[grep("광주", May$TITLE),]
qplot(May_day1$DATE, width=0.5) +
  theme(axis.text.x = element_text(angle=75, vjust=0.6)) +
  labs(title = '5월_"광주"')


May_day2 <- May[grep("국회의원", May$TITLE),]
qplot(May_day2$DATE, width=0.5) +
  theme(axis.text.x = element_text(angle=75, vjust=0.6)) +
  labs(title = '5월_"국회의원"')


May_day3 <- May[grep("경찰", May$TITLE),]
qplot(May_day3$DATE, width=0.5) +
  theme(axis.text.x = element_text(angle=75, vjust=0.6)) +
  labs(title = '5월_"경찰"')


Jun_day1 <- Jun[grep("난민", Jun$TITLE),]
qplot(Jun_day1$DATE, width=0.5) +
  theme(axis.text.x = element_text(angle=75, vjust=0.6)) +
  labs(title = '6월_"난민"')


Jun_day2 <- Jun[grep("선거", Jun$TITLE),]
qplot(Jun_day2$DATE, width=0.5) +
  theme(axis.text.x = element_text(angle=75, vjust=0.6)) +
  labs(title = '6월_"선거"')


Jun_day3 <- Jun[grep("손흥민", Jun$TITLE),]
qplot(Jun_day3$DATE, width=0.5) +
  theme(axis.text.x = element_text(angle=75, vjust=0.6)) +
  labs(title = '6월_"손흥민"')


Jul_day1 <- Jul[grep("어린이", Jul$TITLE),]
qplot(Jul_day1$DATE, width=0.5) +
  theme(axis.text.x = element_text(angle=75, vjust=0.6)) +
  labs(title = '7월_"어린이"')


Jul_day2 <- Jul[grep("이재명", Jul$TITLE),]
qplot(Jul_day2$DATE, width=0.5) +
  theme(axis.text.x = element_text(angle=75, vjust=0.6)) +
  labs(title = '7월_"이재명"')


Jul_day3 <- Jul[grep("누진세", Jul$TITLE),]
qplot(Jul_day3$DATE, width=0.5) +
  theme(axis.text.x = element_text(angle=75, vjust=0.6)) +
  labs(title = '7월_"누진세"')


Aug_day1 <- Aug[grep("공무원", Aug$TITLE),]
qplot(Aug_day1$DATE, width=0.5) +
  theme(axis.text.x = element_text(angle=75, vjust=0.6)) +
  labs(title = '8월_"공무원"')


Aug_day2 <- Aug[grep("부동산", Aug$TITLE),]
qplot(Aug_day2$DATE, width=0.5) +
  theme(axis.text.x = element_text(angle=75, vjust=0.6)) +
  labs(title = '8월_"부동산"')


Aug_day3 <- Aug[grep("연금", Aug$TITLE),]
qplot(Aug_day3$DATE, width=0.5) +
  theme(axis.text.x = element_text(angle=75, vjust=0.6)) +
  labs(title = '8월_"연금"')


Sep_day1 <- Sep[grep("청소년", Sep$TITLE),]
qplot(Sep_day1$DATE, width=0.5) +
  theme(axis.text.x = element_text(angle=75, vjust=0.6)) +
  labs(title = '9월_"청소년"')


Sep_day2 <- Sep[grep("여중생", Sep$TITLE),]
qplot(Sep_day2$DATE, width=0.5) +
  theme(axis.text.x = element_text(angle=75, vjust=0.6)) +
  labs(title = '9월_"여중생"')


Sep_day3 <- Sep[grep("보호법", Sep$TITLE),]
qplot(Sep_day3$DATE, width=0.5) +
  theme(axis.text.x = element_text(angle=75, vjust=0.6)) +
  labs(title = '9월_"보호법"')


Oct_day1 <- Oct[grep("가상", Oct$TITLE),]
qplot(Oct_day1$DATE, width=0.5) +
  theme(axis.text.x = element_text(angle=75, vjust=0.6)) +
  labs(title = '10월_"가상"')


Oct_day2 <- Oct[grep("가상", Oct$TITLE),]
qplot(Oct_day2$DATE, width=0.5) +
  theme(axis.text.x = element_text(angle=75, vjust=0.6)) +
  labs(title = '10월_"가상"')


Oct_day3 <- Oct[grep("가상", Oct$TITLE),]
qplot(Oct_day3$DATE, width=0.5) +
  theme(axis.text.x = element_text(angle=75, vjust=0.6)) +
  labs(title = '10월_"가상"')

Nov_day1 <- Nov[grep("이명박", Nov$TITLE),]
qplot(Nov_day1$DATE, width=0.5) +
  theme(axis.text.x = element_text(angle=75, vjust=0.6)) +
  labs(title = '11월_"이명박"')


Nov_day2 <- Nov[grep("조두순", Nov$TITLE),]
qplot(Nov_day2$DATE, width=0.5) +
  theme(axis.text.x = element_text(angle=75, vjust=0.6)) +
  labs(title = '11월_"조두순"')

Nov_day3 <- Nov[grep("출국금지", Nov$TITLE),]
qplot(Nov_day3$DATE, width=0.5) +
  theme(axis.text.x = element_text(angle=75, vjust=0.6)) +
  labs(title = '11월_"출국금지"')


Dec_day1 <- Dec[grep("조두순", Dec$TITLE),]
qplot(Dec_day1$DATE, width=0.5) +
  theme(axis.text.x = element_text(angle=75, vjust=0.6)) +
  labs(title = '12월_"조두순"')


Dec_day2 <- Dec[grep("여성", Dec$TITLE),]
qplot(Dec_day2$DATE, width=0.5) +
  theme(axis.text.x = element_text(angle=75, vjust=0.6)) +
  labs(title = '12월_"여성"')


Dec_day3 <- Dec[grep("적폐", Dec$TITLE),]
qplot(Dec_day3$DATE, width=0.5) +
  theme(axis.text.x = element_text(angle=75, vjust=0.6)) +
  labs(title = '12월_"적폐"')


##### 시각화 사이트
#R graph gallary
#https://www.r-graph-gallery.com/portfolio/ggplot2-package/

#top 50 ggplot2 visualizion
#http://r-statistics.co/Top50-Ggplot2-Visualizations-MasterList-R-Code.html



#전체 빈도수 탑 30개 중 월별로 키워드 빈도 분석
#   청원수 별 카테고리, 제목빈출키워드, ID, 게시기간 자료
#   월별 카테고리&빈출키워드에 따른 청원게시글수 
#   월별 카테고리&빈출키워드에 따른 동의자글수