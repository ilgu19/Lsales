##****이진 분류 모델**** ----
# (전진선택법)
## 31. 학습 ----
head(train)
base_model <- glm( churn ~ 1 ,data = train , family = "binomial" )
summary(base_model)

fomula <- paste0("churn ~ ", paste(names(train[,-19]), collapse = " + ") )
forwad_model <- step(base_model, fomula, direction = "forward" )
summary(forwad_model)

# upsampling 데이터로 모델 만들기
base_model2 <- glm( churn ~ 1 ,data = up_train , family = "binomial" ) 
forwad_model2 <- step(base_model2, fomula, direction = "forward" )
summary(forwad_model2)
# 32. 예측 ----

# 예측 
# 예측에서도 입력 test 셋을 matrix로!
test_pred <- predict(forwad_model, newdata = test, type = "response")
result <- as.factor(ifelse(test_pred >= 0.5, 1, 0 ))
table(result)

test_pred2 <- predict(forwad_model2, newdata = test, type = "response")
result2 <- as.factor(ifelse(test_pred2 >= 0.5, 1, 0 ))
table(result2)

# 33. 검증 ----
# 1이 Leave 입니다!
confusionMatrix(result, test$churn, positive = "1")

if(!require(Epi)) {install.packages("Epi") ; library(Epi)}
ROC(test_pred, test$churn)
result <- as.factor(ifelse(test_pred >= 0.33, 1, 0 ))
cm_lr1 <- confusionMatrix(result, test$churn, positive = "1")

ROC(test_pred2, test$churn)
result2 <- as.factor(ifelse(test_pred2 >= 0.62, 1, 0 ))
cm_lr2 <- confusionMatrix(result2, test$churn, positive = "1")

########****knn 모델**** ----
## 31. 학습 ----
CV5 <- trainControl(method = 'cv', number = 5)
modelLookup("knn")

# parameter tunning
result <- data.frame(k = integer(), tr = numeric(), te = numeric())
for (i in 1:10) {
  knn_model <- train( churn ~ . , data = train , method = 'knn'
                      , trControl = CV5
                      , tuneGrid = expand.grid(k = i)) 
  
  # 예측
  ds.results.tr <- predict(knn_model, newdata=train)
  ds.results.te <- predict(knn_model, newdata=test)
  
  # 평가
  tr.conf <- confusionMatrix(ds.results.tr, train$churn, positive = "1")
  te.conf <- confusionMatrix(ds.results.te, test$churn, positive = "1")
  #tr.conf$overall
  result[nrow(result)+1,] <- c(i, tr.conf$overall[1], te.conf$overall[1])
  print(i)  
}
result

ggplot() + 
  geom_line(data = result, aes(x = k, y = tr), color = "blue") +
  geom_line(data = result, aes(x = k, y = te), color = "red") +
  xlab('k') +
  ylab('accuracy')

knn_model$bestTune
plot(knn_model)

# 
ds.results.te <- predict(knn_model, newdata=test, type = "prob")
ds.results.te[,2]

ROC(ds.results.te[,2], test$churn)
result4 <- as.factor(ifelse(ds.results.te[,2] >= 0.3, 1, 0 ))
cm_knn <- confusionMatrix(result4, test$churn, positive = "1")
table(result4)

########****Random Forest**** ----
if(!require(randomForest)) {install.packages("randomForest") ; library(randomForest)}

## 31. 학습 ----
rf_model  <- randomForest(churn~. ,data=train, ntree=100)
rf_model
rf_model$confusion
rf_model$err.rate
rf_model$err.rate[,1]
plot(rf_model$err.rate[,1], type = 'l')

# 32. 예측 ----
pred.rf.class <- predict(rf_model, newdata = test, type="class")
pred.rf.prob <- predict(rf_model, newdata = test, type="prob")

ROC(pred.rf.prob[,2], test$churn)
result3 <- as.factor(ifelse(test_pred >= 0.3, 1, 0 ))
confusionMatrix(result3, test$churn, positive = "1")
table(result3)

test$r5 <- pred.rf.class
test$r3 <- result3


# 33. 검증 ----
rf_model$importance
varImpPlot(rf_model)

cm_rf1 <- confusionMatrix(pred.rf.class, test$churn, positive = "1")
cm_rf2 <- confusionMatrix(result3, test$churn, positive = "1")
cm
cm$overall[1] ; cm$byClass[1] ; cm$byClass[2]



##### 참고 
# Grid Search로 튜닝을 수행하고, 예측하고, 평가해 봅시다.
hparams = expand.grid(k = 2:10)
knn_model2 <- train(CHURN ~ ., data = train.m, 
                    method = "knn", trcontrol = CV.5, tuneGrid = hparams)
plot(knn_model2)

# 예측
val_pred2 <- predict(knn_model2, newdata = val.m)
# 평가
confusionMatrix(val_pred2,val.m$CHURN, positive = "LEAVE")

# 2) rpart2를 이용하여 튜닝을 진행합니다.
# 5-fold cross validation을 수행합니다.
CV.5 <- trainControl(method = "cv", number = 3)

# random Search로 튜닝을 수행하고, 예측하고, 평가해 봅시다.
knn_model3 <- train(CHURN ~ ., data = train.m, 
                    method = "rpart2", trcontrol = CV.5, tuneLength = 3) #random search 3회
plot(knn_model3)  
# 예측
val_pred3 <- predict(knn_model3, newdata = val.m)
# 평가
confusionMatrix(val_pred3, val.m$CHURN, positive = "LEAVE")


# Grid Search로 튜닝을 수행하고, 예측하고, 평가해 봅시다.
modelLookup("rpart2")
hparams = expand.grid(maxdepth = 2:10)
knn_model4 <- train(CHURN ~ ., data = train.m, 
                    method = "rpart2", trcontrol = CV.5, tuneGrid = hparams)
plot(knn_model4)

# 예측
val_pred4 <- predict(knn_model4, newdata = val.m)
# 평가
confusionMatrix(val_pred4, val.m$CHURN, positive = "LEAVE")
