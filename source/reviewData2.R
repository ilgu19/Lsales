
# 10. 데이터 이해 ----------------------------------------------------------------------------
## 11. 둘러보기 ----

str(df.A)
head(df.A)
summary(df.A)

str(purchase.dt)
summary(purchase.dt)
purchase.dt$category1 <- as.factor(purchase.dt$category1)
purchase.dt$category2 <- as.factor(purchase.dt$category2)
purchase.dt$category3 <- as.factor(purchase.dt$category3)
purchase.dt$date <- as.Date(as.character(purchase.dt$date),  format = '%Y%m%d')
purchase.dt$subsidary <- as.factor(purchase.dt$subsidary)
purchase.dt$customerid <- as.character(purchase.dt$customerid)
purchase.dt$shopid <- as.character(purchase.dt$shopid)
purchase.dt$receiptid <- as.character(purchase.dt$receiptid)
# describe(purchase.dt)
## 12. 기초통계량 ----

### 12.1 숫자형 변수 ----
### 12.1.1 고객 관점 ----

# 롯데 계열사 A의 2014년 구매한 거래한 금액  분포 확인
if (!require(psych)) { install.packages('psych') ; library(psych)}
psych::describe(df.A[df.A$yyyymm <= "201412","amount"])
zdesc.stat(df.A[df.A$yyyymm <= "201412","amount"])
# 데이터 구매금액의 range 간격이 큰것을 알수 있다

df.A %>% ggplot(aes(amount)) + geom_histogram(bins = 50) + scale_x_log10()    
df.A %>% ggplot(aes(amount)) + geom_density() + scale_x_log10()    

# 2014년 고객별 구매한 금액 분포, 
# 1000만원이상~1억 미만 지출한 사람들이 상당히 많이 있음을 확인할 수 있다.
# 고객을 금액 구간으로 구분하여 segmentation할 필요가 있을까?
df.A %>% filter(yyyymm <= "201412") %>% group_by(customerid) %>% summarise( amt = sum(amount, na.rm = T)) %>%
  ggplot() + geom_histogram(aes(amt), binwidth = 0.01) + scale_x_log10() + labs(title = "2014년 매출액 구간별 고객 분포", x ="금액", y = "사람수") 

df.A %>% filter(yyyymm <= "201412") %>% group_by(customerid) %>% summarise( amt = sum(amount, na.rm = T)) %>%
  ggplot() + geom_density(aes(amt)) + scale_x_log10() + labs(title = "2014년 매출액 구간별 고객 분포", x ="금액", y = "사람수") +
  scale_fill_viridis(discrete=TRUE, guide=FALSE, option="A") +
  theme_ipsum()

custA.1 <-
  df.A %>% filter(yyyymm <= "201412") %>% 
  group_by(customerid) %>% 
  summarise( amt = sum(amount, na.rm = T),    # 구매금액
             trx.num = n_distinct(receiptid), # 구매건수 
             item3 = n_distinct(category3),   # 구매한 제품 종류, 소분류 기준
             months = n_distinct(yyyymm))     #1년중 방문한 월수 
custA.1
# 고객 데이터에서 구매금액과 상관관계가 있는 변수 분석
custA.1$customerid <- as.character(custA.1$customerid)
if(!require(PerformanceAnalytics)) {install.packages("PerformanceAnalytics") ; library(PerformanceAnalytics)}
chart.Correlation(custA.1[, sapply(custA.1, is.numeric)],histogram=TRUE, pch=19)
custA.1$customerid <- as.integer(custA.1$customerid)

# 고객 지역별 메츨 금액 확인
df.A %>% filter(yyyymm <= "201412") %>% left_join(
  customer.dt %>% select(customerid, post) %>%
    mutate(post = case_when( substr(post,1,2) %in% post_se  ~   "서울",           
                             substr(post,1,2) %in% post_gg  ~   "경기",
                             substr(post,1,2) %in% post_in  ~   "인천",
                             substr(post,1,2) %in% post_gw  ~   "강원",
                             substr(post,1,2) %in% post_ch  ~   "충천",
                             substr(post,1,2) %in% post_da  ~   "경상",
                             substr(post,1,2) %in% post_jb  ~   "전라",
                             is.na(post) == T    ~   "없음" ))
) %>%  group_by(yyyymm, post)  %>% summarise( amt = sum(amount, na.rm = T)) %>% 
  ggplot() + geom_line(aes(yyyymm, amt, color =post, group = post )) + 
  scale_y_log10() + labs(title = "2014년 지역별 월매출액 분포", x ="", y = "금액") + 
  scale_fill_viridis(discrete=TRUE, guide=FALSE, option="A") +
  theme_ipsum()

# 고객 연령별 메츨 금액 확인
df.A %>% filter(yyyymm <= "201412") %>% left_join(
  customer.dt %>% select(customerid, age) %>%
    mutate(age = case_when( age == "19세이하"   ~   "19" ,           
                             age == "20세~24세"  ~     "24" ,
                             age == "25세~29세"  ~     "29",
                             age == "30세~34세"  ~     "34",
                             age == "35세~39세"  ~     "39",
                             age == "40세~44세"  ~     "44",
                             age == "45세~49세"  ~     "49",
                             age == "50세~54세"  ~     "54",
                             age == "55세~59세"  ~     "59",
                             age == "60세이상"   ~   "65"  ))
) %>%  group_by(yyyymm, age)  %>% summarise( amt = sum(amount, na.rm = T)) %>% 
  ggplot() + geom_line(aes(yyyymm, amt, color =age, group = age )) + 
  scale_y_log10() + labs(title = "2014년 연령별 월매출액 분포", x ="", y = "금액") +
  scale_fill_viridis(discrete=TRUE, guide=FALSE, option="A") +
  theme_ipsum()


### 12.1.2 제품 관점 ----
# 2014년 제품 대분류별, 매출, 평균, 50,60, 70번째 금액
itemA.1 <-
  df.A %>% filter(yyyymm <= "201412") %>% group_by(category1) %>% 
  summarise(n_cust = n_distinct(customerid),
            amt = sum(amount, na.rm = T),
            trx.num = n_distinct(receiptid),
            amt.mean = sum(amount, na.rm = T) / trx.num,
            amt.50 = quantile(amount, 0.5),
            amt.70 = quantile(amount, 0.7),
            amt.90 = quantile(amount, 0.9)) %>% arrange(desc(amt.mean))
itemA.1
## 제품명 중분류 기준으로 가져오기 ##
item.category <-
  item.dt %>% distinct(subsidary, category1, category2.name) %>% group_by(subsidary, category1) %>% nest()

temp <- map(item.category$data, zcategory) %>% unlist() 
item.category["products"] <- temp
item.category <- item.category %>% filter(subsidary == "A") %>% ungroup() 

itemA.1 <-
  itemA.1 %>% left_join(
    item.category %>% select(category1, products)
  )
itemA.1

####

# 제품별 매출액 분포 그래프
temp <-
df.A %>% filter(yyyymm <= "201412") %>% 
  group_by(yyyymm, category1) %>% 
  summarise(n_cust = n_distinct(customerid),
            amt = sum(amount, na.rm = T),
            trx.num = n_distinct(receiptid),
            amt.mean = sum(amount, na.rm = T) / trx.num,
            amt.50 = quantile(amount, 0.5),
            amt.70 = quantile(amount, 0.7),
            amt.90 = quantile(amount, 0.9)) %>% ggplot() + 
  geom_line(aes(factor(yyyymm), amt, color = factor(category1), group = factor(category1))) + 
  # scale_y_log10() + 
  scale_fill_viridis(discrete=TRUE, guide=FALSE, option="A") +
  theme_ipsum() +
  labs(title = "2014년 제품별 월매출액 분포", x ="", y = "금액")

# 2014 월별 제품대분류별 매출 금액
itemA.2 <- df.A %>% filter(yyyymm <= "201412") %>%
  group_by(category1, yyyymm) %>% summarise(n_cust = n_distinct(customerid),
                                            amt = sum(amount, na.rm = T)
                                            ) %>% 
  pivot_wider(names_from = yyyymm, values_from = c(3:4)) %>%
  mutate_if(is.numeric, ~replace(., is.na(.), 0))
itemA.2
write.csv(itemA.2, "./data/제품군A2014data.csv", fileEncoding = "UTF-8")

# 월별 매출액 분포 그래프
df.A %>% filter(yyyymm <= "201412") %>% ggplot() +
  geom_boxplot(aes(factor(yyyymm), amount)) + scale_y_log10() +
  scale_fill_viridis(discrete=TRUE, guide=FALSE, option="A") +
  theme_ipsum() +
  labs(title = "2014년 월별 각구매액 분포", x ="년월", y = "금액")

# 월별 제품별 Trend 그래프
if(!require(gapminder)) {install.packages("gapminder") ; library(gapminder)}
if(!require(hrbrthemes)) {install.packages("hrbrthemes") ; library(hrbrthemes)}
if(!require(viridis)) {install.packages("viridis") ; library(viridis)}

df.A %>% filter(yyyymm <= "201412") %>%
  group_by(category1, yyyymm) %>% 
  summarise(n_cust = n_distinct(customerid), 
            amt = sum(amount, na.rm = T)
  ) %>% arrange(desc(n_cust)) %>%
  ggplot(aes(x=yyyymm, y=amt, size=n_cust, color=factor(category1))) +
  geom_point(alpha=0.5) + scale_y_log10() +
  scale_size(range = c(0.1, 24), name="고객수") + scale_fill_viridis(discrete=TRUE, guide=FALSE, option="A") +
  theme_ipsum() + ylab("Amount") + 
  labs(title = "2014년 제품별 월매출액 분포", x ="", y = "금액")
# https://www.r-graph-gallery.com/320-the-basis-of-bubble-plot.html


# 제품기준으로 데이터 좀 더 확인하기 
# 제품을 구매횟수와 금액을 기준으로 재그룹핑이 필요할까?
df.A %>% filter(yyyymm <= "201412") %>% 
  group_by(category1) %>%
  summarise(n_cust = n_distinct(customerid), 
            trx.num = n(), # 구매건수 
            item3 = n_distinct(category3), 
            amt = sum(amount, na.rm = T),
            avg = amt / trx.num)  %>% 
  arrange(desc(avg))

itemA.3 <-
  df.A %>% filter(yyyymm <= "201412") %>% 
  group_by(category3) %>%
  summarise(trx.num = n(), # 구매건수 
            amt = sum(amount, na.rm = T),
            price = round(amt / trx.num))

if(!require(rbin)) {install.packages("rbin") ; library(rbin)}
#seq_range(itemA.3$price, n = 20, pretty = TRUE)
bins <- rbin_equal_freq(itemA.3, response = category3, predictor = price, bins = 10)

table(cut(itemA.3$price, breaks = c(0, 25000, 50000, 70000,100000, 150000, 200000, 300000, 600000, 10000000),
          labels = c('25K', '50K', '70K','100K','150K','200K','300K','600K', "600K<")))

itemA.3$price <- 
  cut(itemA.3$price, breaks = c(0, 25000, 50000, 70000,100000, 150000, 200000, 300000, 600000, 10000000),
      labels = c('25K', '50K', '70K','100K','150K','200K','300K','600K', "600K<"))

item.dt <-
  item.dt %>% left_join(
    itemA.3 %>% select(c(1,4))
  ) 

# temp <- item.dt[is.na(item.dt$price)== TRUE, "price"]
# item.dt[is.na(item.dt$price)== TRUE, "price"] <- "NOSALES"

# if(!require(cluster)) {install.packages("cluster") ; library(cluster)}
# item_kmeans_1 = kmeans(x=itemA.3[,-1], centers = 10) 
# clusplot(itemA.3[,-1], item_kmeans_1$cluster, color = TRUE)
# itemA.3$cluster <- item_kmeans_1$cluster

# 고객별 구매하는 가격대별 제품들 보기 
cust.item <-
  df.A %>% filter(yyyymm <= "201412") %>% left_join(
    item.dt %>% select(category3, price)) %>%
  group_by(customerid, price) %>% summarise(n = n()) %>% 
  pivot_wider(names_from = price, values_from = c(3)) %>%
  mutate_if(is.numeric, ~replace(., is.na(.), 0))
cust.item

### 12.1.3 매장 관점 ----
# 구매횟수와 금액을 기준으로 재그룹핑이 필요할까?
df.A %>% filter(yyyymm <= "201412") %>% 
  group_by(shopid) %>%
  summarise(n_cust = n_distinct(customerid), 
            trx.num = n_distinct(receiptid), # 구매건수 
            item3 = n_distinct(category3), 
            amt = sum(amount, na.rm = T),
            avg = amt / trx.num)  %>% 
  arrange(desc(avg))


### 12.2 범주형 변수 

#table(df.A$category1, df.A$subsidary)
str(df.A)

# 제품군 대분류
table(df.A[df.A$yyyymm <= "201412","category1"])  
prop.table(table(df.A[df.A$yyyymm <= "201412","category1"]))

# 제품군 대분류 vs 점포
table(df.A[df.A$yyyymm <= "201412","shopid"], df.A[df.A$yyyymm <= "201412","category1"]) ; 
round(prop.table(table(df.A[df.A$yyyymm <= "201412","shopid"], df.A[df.A$yyyymm <= "201412","category1"]), 1),2)


# if (!require(mosaic)) install.packages('mosaic')
# library(mosaic)
# mosaicplot(Survived ~Sex  + Pclass
#            , data = t, color = TRUE)

## 13. 탐색하기 ----