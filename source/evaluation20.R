
# 41 모델의 성능 정리 ----
# 1) 과제 4번의 결과(confusionMatrix)로 부터 진짜 confusion matrix를 저장합니다.
cm_lr1$table
cm_lr2$table
cm_knn$table
cm_rf1$table
cm_rf2$table

# 2) 진짜 confusion matrix를 비율로 변환합니다.
round(prop.table(cm_lr1$table),2)
round(prop.table(cm_lr2$table),2)
round(prop.table(cm_rf1$table),2)
round(prop.table(cm_rf2$table),2)
round(prop.table(cm_knn$table),2)

# 42 비즈니스 기대가치 매트릭스 만들기 ----

# 비즈니스 요구사항에 맞게 기대가치 매트릭스를 만드시오.
# random forest 판정 기준값을 0.5->0.3으로 했을 때 test data의 경제성 금액...
rownames(test)
test$customerid <- as.integer(rownames(test))

test.1 <- test %>% left_join(  
  df.A %>% filter(yyyymm >= "201401" & yyyymm <= "201412") %>% 
    group_by(customerid) %>% summarise(amt12M = sum(amount, na.rm = T))  
)  

table(test.1$churn)
test.1 %>% group_by(churn) %>% summarise(n = n(), total = sum(amt12M, na.rm = T))

table(test.1$r5); table(test.1$r3)
test.1 %>% group_by(churn, r5) %>% summarise(total = sum(amt12M, na.rm = T))
test.1 %>% group_by(churn, r3) %>% summarise(total = sum(amt12M, na.rm = T))


# 각각의 모델에 대해서 비즈니스 기대가치를 계산하시오.
# prop.table(cm_knn$table) * bv
# prop.table(cm_dt$table) * bv
# prop.table(cm_rf$table) * bv
# prop.table(cm_xgb$table) * bv
# sum(prop.table(cm_knn$table) * bv)
# sum(prop.table(cm_dt$table) * bv)
# sum(prop.table(cm_rf$table) * bv)
# sum(prop.table(cm_xgb$table) * bv)

test.1
mosaicplot( level ~ churn   , data = test.1, color = T)
mosaicplot( post1 ~ churn   , data = test.1, color = T)
mosaicplot( level ~ post1 + churn   , data = test.1, color = T)

mosaicplot( level ~ r3   , data = test.1, color = T)
mosaicplot( post1 ~ r3   , data = test.1, color = T)
mosaicplot( level ~ post1 + r3   , data = test.1, color = T)


test.1 %>% filter(amt3M < 0.05) %>% ggplot() + geom_boxplot(aes(churn, amt3M))
cust2014.2 %>% ggplot() + geom_boxplot(aes(churn, amt3M)) + scale_y_log10() +
  scale_fill_viridis(discrete=TRUE, guide=FALSE, option="A") +
  theme_ipsum() +
  labs(title = "2014년 이탈 유무에 따른 고객별 3개월 구매액", x ="이탈", y = "3개월구매액")

cust2014.2 %>% ggplot() + geom_boxplot(aes(churn, amt1M)) + scale_y_log10() +
  scale_fill_viridis(discrete=TRUE, guide=FALSE, option="A") +
  theme_ipsum() +
  labs(title = "2014년 이탈 유무에 따른 고객별 1개월 구매액", x ="이탈", y = "1개월구매액")

summary(aov(amt1M ~ churn, data = cust2014.2))
summary(aov(amt3M ~ churn, data = cust2014.2))
