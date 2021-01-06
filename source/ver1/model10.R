##### 이진 분류 모델 ----
# (전진선택법)
## 31. 학습 ----
head(train)

base_model <- glm( churn ~ 1 ,data = train , family = "binomial" )
summary(base_model)

fomula <- paste0("churn ~ ", paste(names(train[,-15]), collapse = " + ") )
forwad_model <- step(base_model, fomula, direction = "forward" )
summary(forwad_model)

# 32. 예측 ----

# 예측 
# 예측에서도 입력 test 셋을 matrix로!
test_pred <- predict(forwad_model, newdata = test, type = "response")
result <- as.factor(ifelse(test_pred >= 0.5, 1, 0 ))

# 33. 검증 ----
# 1이 Leave 입니다!
confusionMatrix(result, test$churn, positive = "1")

##### KNN 모델 ----
## 31. 학습 ----
CV.5 <- trainControl(method = "cv", number = 5)
modelLookup("knn")

knn_model <- train(churn ~ ., data = train, 
                   method = "knn", trcontrol = CV.5, tuneLength = 3)
plot(knn_model)

# 32. 예측 ----
val_pred <- predict(knn_model, newdata = test)

# 33. 검증 ----
confusionMatrix(result, test$churn, positive = "1")








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
                    method = "rpart2", trcontrol = CV.5, tuneLength = 3)
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
