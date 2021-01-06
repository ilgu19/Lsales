# 00. 문제 이해 및 정의 -------------------------------------------------------------------------
# 1. 비즈니스 요구사항 
# 1) 2014~2015, 2년간 롯데 그룹 4개 계열사의 구매 데이터, 고객, 상품, 멤버쉽, 채널, 경쟁사 데이터
# 2) 종속 변수 Y Labeling: 구매실적이 최근 3개월내에 구매 내역이 없으면 이탈로 정의 - draft 
# 3) 초기가설 example: 최근 3개월 내에 경쟁사에 1회 이상 구매 고객은 잠재적으로 이탈할 가능성이 높다 - 별도 template 참조
# 4) 

# 01. 환경준비 -------------------------------------------------------------------------------

## 01.1 Library(기본 packages 읽기)

getwd()
# 모델링을 위한 종합 패키지!
if (!require(caret)) { install.packages('caret') ; library(caret)}
# 데이터 전처리를 위한 종합 패키지!
if (!require(tidyverse)) { install.packages('tidyverse') ; library(tidyverse)}
# 대용랭 데이터 처리
if (!require(data.table)) { install.packages('data.table') ; library(data.table)}
# 기초 통계 처리
if (!require(psych)) { install.packages('psych') ; library(psych)}
# 모델링을 위한 패키지2
if (!require(modelr)) { install.packages('modelr') ; library(modelr)}

# 기본 함수 모음
source("./source/Myfunction.R") # setwd가 제대로 되어 있어야 한다.

## 01.2 Data Loading (데이터셋 준비 )

# 데이터셋 읽기 
purchase.dt <- fread("./data/구매상품TR.txt", header = TRUE, stringsAsFactors=FALSE, na.strings=getOption("datatable.na.strings","NA"))
customer.dt <-read.csv("./data/고객DEMO.txt", na.strings=c("","NA","Unknown","NULL"), stringsAsFactors = TRUE)
rival.dt <-read.csv("./data/경쟁사이용.txt", na.strings=c("","NA","Unknown","NULL"), stringsAsFactors = TRUE)
member.dt <-read.csv("./data/멤버십여부.txt", na.strings=c("","NA","Unknown","NULL"), stringsAsFactors = TRUE)
item.dt <-read.csv("./data/상품분류.txt", na.strings=c("","NA","Unknown","NULL"), stringsAsFactors = TRUE)
channel.dt <-read.csv("./data/채널이용.txt", na.strings=c("","NA","Unknown","NULL"), stringsAsFactors = TRUE)

# 컬럼명 영어로 변경
purchase.dt <-
  purchase.dt %>% 
  rename(
    subsidary = 제휴사,
    receiptid = 영수증번호,
    category1 = 대분류코드,
    category2 = 중분류코드,
    category3 = 소분류코드,
    customerid = 고객번호,
    shopid = 점포코드,
    date   = 구매일자,
    time   = 구매시간,
    amount = 구매금액
  )

customer.dt <-
  customer.dt %>% 
  rename(
    customerid = 고객번호,
    sex = 성별,
    age   = 연령대,
    post = 거주지역
  )

rival.dt <-
  rival.dt %>% 
  rename(
    customerid = 고객번호,
    subsidary = 제휴사,
    rival   = 경쟁사,
    rival.date = 이용년월
  )

member.dt <-
  member.dt %>% 
  rename(
    customerid = 고객번호,
    membership  = 멤버십명,
    membership.date = 가입년월
  )

item.dt <-
  item.dt %>% 
  rename(
    subsidary = 제휴사, 
    category1 = 대분류코드,
    category2 = 중분류코드,
    category3   = 소분류코드,
    category2.name = 중분류명,
    category3.name = 소분류명
  ) 

channel.dt <-
  channel.dt %>% 
  rename(
    customerid = 고객번호,
    channel = 제휴사,
    usage   = 이용횟수
  )

#문자 분리하기
channel.dt <-
  channel.dt %>% separate(channel, 
                          into = c("subsidary", "channel"), sep = "_") %>%
  select(c(1,3,4,2))

#우편번호 앞에 0 넣기
customer.dt$post <- formatC(customer.dt$post, width = 3, flag = "0")
formatC(1:9, width = 2, flag = "0")

#서울시 - 01~09
post_seoul <- formatC(1:9, width = 2, flag = "0")
#경기도 - 10~20
post_gg <- formatC(10:20, width = 2, flag = "0")

#서울시 데이터만 sampling
df <-
purchase.dt %>% 
  inner_join(
    customer.dt %>% filter(substr(post,1,2) %in% c(post_seoul)),
    by = "customerid"
  ) 

df.sample <- head(df, 50000)
df.sample <- data.frame(df.sample)

df <- data.frame(df)

# df %>% fwrite("./data/data_seoul.csv", bom = T) # 한글이 깨짐
df <-
  df %>% left_join( 
    item.dt %>% select(category3, category3.name), by = "category3")
df$yyyymm <- substr(df$date,1,6)
head(df)

#롯데 계열사 A data 만 선택하기 
df.A <- purchase.dt %>% filter(subsidary == "A") 
df.A <- data.frame(df.A)

df.A <-
  df.A %>% left_join( 
    item.dt %>% select(category3, category3.name), by = "category3")
df.A$yyyymm <- substr(df.A$date,1,6)

rm(purchase.dt)

# Factor 변수 만들기
# idx <- which(sapply(df, is.character))
# sapply(df[idx] , as.factor)

# 10. 데이터 이해 ----------------------------------------------------------------------------
source("./source/reviewData.R") 






# 20. 데이터 준비 ----------------------------------------------------------------------------
source("./source/prepareData.R") 


# 30. 모델링 ----------------------------------------------------------------------------
source("./source/model10.R") 


# 40. 모델 평가(기술적,비즈니스 관졈) ---------------------------------------------------------

# 41 모델의 성능 정리 ----
# 1) 과제 4번의 결과(confusionMatrix)로 부터 진짜 confusion matrix를 저장합니다.
dim(cust_churn)
cm_knn$table
cm_dt$table
cm_rf$table
cm_xgb$table

# 2) 진짜 confusion matrix를 비율로 변환합니다.
prop.table(cm_knn$table)
prop.table(cm_dt$table)
prop.table(cm_rf$table)
prop.table(cm_xgb$table)

# 42 비즈니스 기대가치 매트릭스 만들기 ----

# 비즈니스 요구사항에 맞게 기대가치 매트릭스를 만드시오.
bv <- matrix(c(0,-20,-40,-20), nrow = 2)

# 각각의 모델에 대해서 비즈니스 기대가치를 계산하시오.
prop.table(cm_knn$table) * bv
prop.table(cm_dt$table) * bv
prop.table(cm_rf$table) * bv
prop.table(cm_xgb$table) * bv
sum(prop.table(cm_knn$table) * bv)
sum(prop.table(cm_dt$table) * bv)
sum(prop.table(cm_rf$table) * bv)
sum(prop.table(cm_xgb$table) * bv)

