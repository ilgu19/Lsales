## 21. 변수 정리 ----
# 불필요한 칼럼 삭제 (삭제시 조심!!!)


## 22. Feature Engineering ----
cust.1 <-  df.A %>% filter(yyyymm == "201412") %>% group_by(customerid) %>% 
  summarise(trx1M = n_distinct(receiptid), amt1M = sum(amount, na.rm = T), 
            mean1M = round(amt1M/trx1M), item1M = n_distinct(category3))

cust.3 <-  df.A %>% filter(yyyymm >= "201410" & yyyymm <= "201412") %>% 
  group_by(customerid) %>% summarise(trx3M = n_distinct(receiptid), amt3M = sum(amount, na.rm = T), 
                                     mean3M = round(amt3M/trx3M), item3M = n_distinct(category3))

cust.6 <-  df.A %>% filter(yyyymm >= "201407" & yyyymm <= "201412") %>% 
  group_by(customerid) %>% summarise(trx6M = n_distinct(receiptid), amt6M = sum(amount, na.rm = T), 
                                     mean6M = round(amt6M/trx6M), item6M = n_distinct(category3))

customer.dt <-
  customer.dt %>% mutate(age1 = case_when( age == "19세이하"   ~   "19" ,           
                                        age == "20세~24세"  ~     "24" ,
                                        age == "25세~29세"  ~     "29",
                                        age == "30세~34세"  ~     "34",
                                        age == "35세~39세"  ~     "39",
                                        age == "40세~44세"  ~     "44",
                                        age == "45세~49세"  ~     "49",
                                        age == "50세~54세"  ~     "54",
                                        age == "55세~59세"  ~     "59",
                                        age == "60세이상"   ~   "65"  ))

customer.dt <-
  customer.dt %>% mutate(post1 = case_when( substr(post,1,2) %in% post_se  ~   "서울",           
                                            substr(post,1,2) %in% post_gg  ~   "경기",
                                            substr(post,1,2) %in% post_in  ~   "인천",
                                            substr(post,1,2) %in% post_gw  ~   "강원",
                                            substr(post,1,2) %in% post_ch  ~   "충천",
                                            substr(post,1,2) %in% post_da  ~   "경상",
                                            substr(post,1,2) %in% post_jb  ~   "전라",
                                            is.na(post) == T    ~   "없음" ))


# 고객을 금액에 따라 균등하게 segmentation -> 고객을 5개의 group으로 분리 (SVIP, VIP, GOLD, SILVER, NEW)
if(!require(rbin)) {install.packages("rbin") ; library(rbin)}
bins <- rbin_equal_freq(custA.1, response = customerid, predictor = amt, bins = 5)

table(
  cut(custA.1$amt, breaks = c(0, 330000, 1600000, 8500000,25000000, 1500000000),
      labels = c('NEW', 'SILVER', 'GOLD','VIP','SVIP')))    

custA.1$level <- 
  cut(custA.1$amt, breaks = c(0, 330000, 1600000, 8500000,25000000, 1500000000),
      labels = c('NEW', 'SILVER', 'GOLD','VIP','SVIP'))

# 고객 등급 정보
customer.dt <-
  customer.dt %>% left_join(
    custA.1 %>% select(customerid, level)
  )

# Channel 정보 모바일 앱을 얼마나 사용하는지....
channel.dt %>% group_by(subsidary, channel) %>% summarise(cnt = n_distinct(customerid))
channel.dt %>% filter(subsidary == "A") %>% count(customerid) %>% filter(n > 1)
customer.dt <-
  customer.dt %>% left_join(
    channel.dt %>% filter(subsidary == "A") %>% select(customerid, usage)
  ) %>% rename(mobilappusage = usage)

# 멤버쉽 가입정도
customer.dt <- customer.dt %>% 
  left_join(
    member.dt %>% filter(membership.date <= "201412") %>% count(customerid) %>% rename(membership = n)
  )

# 경쟁회사 이용한 유뮤
temp <- rival.dt %>% filter(subsidary == "A", rival.date <= "201412")
# 경쟁사를 이용한 적이 없다는 것은 정보가 부족하거나 고객의 충성도가 높다고 할 수 있다.

str(customer.dt)
customer.dt$age1 <- as.integer(customer.dt$age1)
customer.dt$age <- NULL
customer.dt$post <- NULL

cust2014 <- df.A %>% filter(yyyymm <= "201412") %>% distinct(customerid) %>% arrange()

cust2014 <-
  cust2014 %>% 
  left_join(customer.dt, by = "customerid") %>%
  left_join(cust.1, by = "customerid") %>%
  left_join(cust.3, by = "customerid") %>%
  left_join(cust.6, by = "customerid") 

# cust2015 <- df.A %>% filter(yyyymm == "201501", subsidary == "B") %>% distinct(customerid)
cust2015 <- df.A %>% filter(yyyymm >= "201501" & yyyymm <= "201503") %>% distinct(customerid)
cust2015$churn <- "0"
cust2014$churn <- NULL
cust2014 <- cust2014 %>% left_join(cust2015, by = "customerid")
cust2014$churn[is.na(cust2014$churn)] <- 1

cust2014$churn <- as.factor(cust2014$churn)
summary(cust2014$churn)

write.csv(cust2014, "./data/custB2014.csv", fileEncoding = "cp949")



# 기타 
cust2014$mobilappusage[is.na(cust2014$mobilappusage)] <- 0
cust2014$membership[is.na(cust2014$membership)] <- 0
## 23. Dummy Variable ----

# 범주형 변수를 숫자로 변환하는 방법.
# caret::dummyVars 를 이용하여 변환을 위한 함수 만들기
str(cust2014)
colSums(is.na(cust2014))
cust2014[is.na(cust2014$post1),"post1"] <- "없음"
cust2014[is.na(cust2014)] <- 0

cust2014.2 <- cust2014
cust2014 <- cust2014.2
rownames(cust2014) <- cust2014$customerid 
cust2014$customerid <-NULL
# 함수 적용은 predict, 결과는 matrix 로 저장, Label은 제외된 x들만 저장.

## 24. Data Split ----
set.seed(1234)
index1 <- sample(nrow(cust2014), size = 0.8 * nrow(cust2014))
train_val <- cust2014[index1, ]
test <- cust2014[-index1, ]

set.seed(1234)
index2 <- sample(nrow(train_val), size = 0.8 * nrow(train_val))
train <- train_val[index2, ]
val <- train_val[-index2, ] 

## 25. Scaling & NA 처리 ----
dim(train)
scale_fn <- preProcess(train[,-c(1:4,19)], method= c('range','knnImpute' ))
train <- predict(scale_fn, newdata = train)
val <- predict(scale_fn, newdata = val)
test <- predict(scale_fn, newdata = test)

str(train)
train$level <- as.factor(train$level)
train$sex <- as.factor(train$sex)
train$post1 <- as.factor(train$post1)

#up sampling 
dim(train)
set.seed(1234)
up_train <- upSample(x = train[, -19],
                     y = train$churn, yname = "churn")
summary(up_train$churn)