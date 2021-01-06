
# 10. 데이터 이해 ----------------------------------------------------------------------------
## 11. 둘러보기 ----

str(df)
head(df)
summary(df)

## 12. 기초통계량 ----

### 12.1 숫자형 변수 

# 구매 금액 
if (!require(psych)) { install.packages('psych') ; library(psych)}
psych::describe(df[,"amount"])

zdesc.stat(df[,"amount"])
df %>% filter(amount > 100000) %>% nrow()

#!구매내역 중에 95%의 데이터가 87000원 이하의 거래 금액이다
#!10만원 이상의 거래 금액이 2년 동안 서울에서 354300건이 됨. 

# 기초 통계와 그래프의 결과가 일치하는지 확인하며 살펴본다.
# 10만원 이상의 거래 금액 확인
df %>% filter(amount > 100000) %>% ggplot(aes(amount)) + geom_histogram(bins = 50) + scale_x_log10()    
df %>% filter(amount > 100000) %>% ggplot(aes(amount)) + geom_density() + scale_x_log10()    


### 12.2 범주형 변수 

# category1 vs계열사 건수(구매 라인 건수) 
table(df$category1, df$subsidary)
#계열사 A는 category1(대분류)이 1~9까지 item을 판매한다 
#계열사 B는 category1(대분류)이 1~92까지 모든 item을 판매한다 
#계열사 C는 category1(대분류)이 1~17까지 item을 판매한다 
#계열사 D는 category1(대분류)이 1~8까지 item을 판매한다 

# 2014한해계열사별, 제품대분별, 매출, 평균, 50,60, 70번째 금액
temp.1 <-
  df %>% filter(yyyymm <= "201412") %>% group_by(subsidary, category1) %>% 
  summarise(amt = sum(amount, na.rm = T),
            trx.num = n_distinct(receiptid),
            amt.mean = sum(amount, na.rm = T) / trx.num,
            amt.50 = quantile(amount, 0.5),
            amt.70 = quantile(amount, 0.7),
            amt.90 = quantile(amount, 0.9)) 

# temp.2 <- 
#   temp.1 %>% select(1:5) %>% pivot_wider(names_from = subsidary, values_from = c(3:5)) %>%
#              mutate_if(is.numeric, ~replace(., is.na(.), 0))

item.category <-
  item.dt %>% distinct(subsidary, category1, category2.name) %>% group_by(subsidary, category1) %>% nest()

zcategory <- function(df) {
  apply(df, 2,paste,collapse=",")
}

temp <- map(item.category$data, zcategory) %>% unlist() 
item.category["products"] <- temp

# 2014한해계열사별, 제품대분별, 기본 통계 금액
temp.3 <-
  df %>% filter(yyyymm <= "201412") %>% group_by(subsidary, category1) %>% 
  summarise(n_shop = n_distinct(shopid),
            n_item = n_distinct(category3), 
            n_customer = n_distinct(customerid),
            n_trx      = n(),
            amt = sum(amount, na.rm = T),
            quantileAmt = paste(round(quantile(amount, c(0.25,0.5,0.75,0.9))), collapse = "-")) 
temp.3 <-
  temp.3 %>% left_join(
    item.category %>% select(-data)
  )
write.csv(temp.3, "./data/계열사2014data.csv", fileEncoding = "UTF-8")

# graph
# 2014한해 1~20 대분류 제품군의계열사별 매출 금액
temp.1 %>% filter(category1 <= 20) %>%  
  ggplot() + geom_line(aes(factor(category1), amt), group = 1, color = "red") +
  scale_y_log10() + facet_grid(subsidary ~. , scales="free")

# 2014한해 1~20 대분류 제품군의계열사별 50, 60, 70번째 금액
temp.1 %>% filter(category1 <= 20) %>%  
  ggplot() + geom_line(aes(factor(category1), amt.90), group = 1, color = "red") +
  geom_line(aes(factor(category1), amt.70), group = 1, color = "blue") +
  geom_line(aes(factor(category1), amt.50), group = 1, color = "green") +
  scale_y_log10() + facet_grid(subsidary ~. , scales="free")


head(df)

cust.1 <-  df %>% filter(yyyymm == "201412", subsidary == "B") %>% group_by(customerid) %>% 
  summarise(trx1M = n_distinct(receiptid), amt1M = sum(amount, na.rm = T), 
            mean1M = round(amt1M/trx1M), item1M = n_distinct(category3))

cust.3 <-  df %>% filter(yyyymm >= "201410" & yyyymm <= "201412", subsidary == "B") %>% 
  group_by(customerid) %>% summarise(trx3M = n_distinct(receiptid), amt3M = sum(amount, na.rm = T), 
                                     mean3M = round(amt3M/trx3M), item3M = n_distinct(category3))

cust.6 <-  df %>% filter(yyyymm >= "201407" & yyyymm <= "201412", subsidary == "B") %>% 
  group_by(customerid) %>% summarise(trx6M = n_distinct(receiptid), amt6M = sum(amount, na.rm = T), 
                                     mean6M = round(amt6M/trx6M), item6M = n_distinct(category3))

cust2014 <- df %>% filter(yyyymm <= "201412", subsidary == "B") %>% distinct(customerid) %>% arrange()

cust2014 <-
  cust2014 %>% 
  left_join(customer.dt, by = "customerid") %>%
  left_join(cust.1, by = "customerid") %>%
  left_join(cust.3, by = "customerid") %>%
  left_join(cust.6, by = "customerid") 

# cust2015 <- df %>% filter(yyyymm == "201501", subsidary == "B") %>% distinct(customerid)
cust2015 <- df %>% filter(yyyymm >= "201501" & yyyymm <= "201503", subsidary == "B") %>% distinct(customerid)
cust2015$churn <- "0"
cust2014$churn <- NULL
cust2014 <- cust2014 %>% left_join(cust2015, by = "customerid")
cust2014$churn[is.na(cust2014$churn)] <- 1

cust2014$churn <- as.factor(cust2014$churn)
summary(cust2014$churn)

write.csv(cust2014, "./data/custB2014.csv", fileEncoding = "cp949")
unique(cust2014$age)







table(cust_churn$Churn, cust_churn$Addr3)
prop.table(table(cust_churn$Churn, cust_churn$Addr3))

plot.categoric(c(), df)


if (!require(mosaic)) install.packages('mosaic')
library(mosaic)
Titanic
mosaicplot(Survived ~Sex  + Pclass
           , data = Titanic, color = TRUE)


head(df)
## 13. 탐색하기 ----