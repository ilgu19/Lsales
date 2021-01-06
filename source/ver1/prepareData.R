## 21. 변수 정리 ----
head(cust2014)
# 불필요한 칼럼 삭제 (삭제시 조심!!!)


## 22. Feature Engineering ----
unique(cust2014$age)

cust2014 <-
  cust2014 %>% mutate(age1 = case_when( age == "19세이하"   ~   "19" ,           
                                        age == "20세~24세"  ~     "24" ,
                                        age == "25세~29세"  ~     "29",
                                        age == "30세~34세"  ~     "34",
                                        age == "35세~39세"  ~     "39",
                                        age == "40세~44세"  ~     "44",
                                        age == "45세~49세"  ~     "49",
                                        age == "50세~54세"  ~     "54",
                                        age == "55세~59세"  ~     "59",
                                        age == "60세이상"   ~   "60"  ))

# 고객을 금액에 따라 균등하게 segmentation -> 고객을 5개의 group으로 분리 (SVIP, VIP, GOLD, SILVER, NEW)
bins <- rbin_equal_freq(custA.1, response = customerid, predictor = amt, bins = 5)

table(
  cut(custA.1$amt, breaks = c(0, 330000, 1600000, 8500000,25000000, 1500000000),
      labels = c('NEW', 'SILVER', 'GOLD','VIP','SVIP')))    

custA.1$level <- 
  cut(custA.1$amt, breaks = c(0, 330000, 1600000, 8500000,25000000, 1500000000),
      labels = c('NEW', 'SILVER', 'GOLD','VIP','SVIP'))

customer.dt <-
  customer.dt %>% left_join(
    custA.1 %>% select(customerid, level)
  )

# 기타 

cust2014$customerid <-NULL
cust2014$age <-NULL
## 23. Dummy Variable ----

# 범주형 변수를 숫자로 변환하는 방법.
# caret::dummyVars 를 이용하여 변환을 위한 함수 만들기
cust2014[is.na(cust2014)] <- 0

# 함수 적용은 predict, 결과는 matrix 로 저장, Label은 제외된 x들만 저장.

## 24. Data Split ----

index1 <- sample(nrow(cust2014), size = 0.8 * nrow(cust2014))
train_val <- cust2014[index1, ]
test <- cust2014[-index1, ]

index2 <- sample(nrow(train_val), size = 0.8 * nrow(train_val))
train <- train_val[index2, ]
val <- train_val[-index2, ] 

## 25. Scaling & NA 처리 ----
dim(train)
scale_fn <- preProcess(train[,-c(1:2,15)], method= c('range','knnImpute' ))
train <- predict(scale_fn, newdata = train)
val <- predict(scale_fn, newdata = val)
test <- predict(scale_fn, newdata = test)

