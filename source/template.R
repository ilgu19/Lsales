# 00. 문제 이해 및 정의 -------------------------------------------------------------------------
	# 1. 비즈니스 요구사항 
	# 1) 현재 고객 885명을 유지하는것이 목표입니다.
	# 2) 이탈할 것으로 예상되는 사람에게 특별 혜택을 지급합니다.
	# 3) 이번 특별 혜택은 개인별로 20만원 상당으로, 받은 사람은 절대 이탈하지 않을 만큼 매력적인 것입니다.
	# 4) 이탈한 회원수 만큼 신규로 유치하는 드는 비용은 1인당 40만원이 소요됩니다.

# 01. 환경준비 -------------------------------------------------------------------------------

	## 01.1 Library(기본 packages 읽기)
		
		getwd()
		# 모델링을 위한 종합 패키지!
		if (!require(caret)) { install.packages('caret') ; library(caret)}
		# 데이터 전처리를 위한 종합 패키지!
		if (!require(tidyverse)) { install.packages('tidyverse') ; library(tidyverse)}
		# xgb
		if (!require(xgboost)) { install.packages('xgboost') ; library(xgboost)}


	## 01.2 Data Loading (데이터셋 준비 )

		# 데이터셋 읽기 
		titanic.0 <- read.csv("https://raw.githubusercontent.com/DA4BAM/dataset/master/titanic.1.csv"
							  , stringsAsFactors = T)
		titanic.0$Pclass <- as.factor(titanic.0$Pclass)
	
	
# 10. 데이터 이해 ----------------------------------------------------------------------------

	## 11. 둘러보기 ----
		str(t)
		head(t)
		summary(t)
		
	## 12. 기초통계량 ----

		### 12.1 숫자형 변수 
			# 숫자형 변수에 대해서 파악한 내용을 적어봅시다.
		
			# t.numeric <- sapply(t[sapply(t, is.numeric)], summary)
			# t.numeric <- select(t, PassengerId, Age, SibSp, Parch, Fare)
			t.numeric <- t[ , sapply(t, is.numeric)]
			source('desc.stat.R') # setwd가 제대로 되어 있어야 하며,  desc.stat.R 안에 desc.stat 함수가 있어야 한다.
			sapply(t.numeric, desc.stat)
			
			# 값을 살펴보며 스스로 해석해보자.

			# desc.stat <- function(x) {
			# c(n = length(x), na.count = sum(is.na(x))
			#   , min = min(x, na.rm = T)
			#   , qt1st = quantile(x, 0.25,na.rm = T)
			#   , median = median(x, na.rm = T)
			#   , mean = mean(x, na.rm = T)
			#   , qt3st = quantile(x, 0.75,na.rm = T)
			#   , max = max(x, na.rm = T)
			#   , range = max(x, na.rm = T) - min(x, na.rm = T)
			#   , sd = sqrt( sum( (x - mean(x))^2 ) / (length(x) - 1) )
			# )
			# }

			# 값 분포 확인하기.
			# 기초 통계와 그래프의 결과가 일치하는지 확인하며 살펴본다.
			hist(t$Fare, breaks = 50)
			hist(t$Age)
			hist(t$Age, breaks = 16)
			plot(density(t$Fare))
			plot(density(t$Age, na.rm = T))
		
			# ggplot2 사용해서 그래프 다시 또 그려보기 
			qplot(Family, data=t, geom="histogram")
			qplot(Age, data=t,geom="density")
			
		### 12.2 범주형 변수 
			# 숫자형 변수에 대해서 파악한 내용을 적어봅시다.
			
			table(cust_churn$Churn, cust_churn$Addr3)
			prop.table(table(cust_churn$Churn, cust_churn$Addr3))
			quantile(titanic.0$Fare)
			
			if (!require(mosaic)) install.packages('mosaic')
			library(mosaic)
			mosaicplot(Survived ~Sex  + Pclass
					   , data = t, color = TRUE)

					   
					   
	## 13. 탐색하기 ----

# 20. 데이터 준비 ----------------------------------------------------------------------------

	## 21. 변수 정리 ----

		# 불필요한 칼럼 삭제 (삭제시 조심!!!)
		products$CategoryOrd <-NULL
		drop_columns <- c("PassengerId","Name", "Cabin", "Ticket","Fare2","AgeGroup","Family")
		titanic.0 <- titanic.0[ , !(names(titanic.0) %in% drop_columns)]

	## 22. Feature Engineering ----


	## 23. Dummy Variable ----

		# 범주형 변수를 숫자로 변환하는 방법.
		# caret::dummyVars 를 이용하여 변환을 위한 함수 만들기
		dummies_model <- dummyVars(Survived ~ ., data=titanic.0)

		# 함수 적용은 predict, 결과는 matrix 로 저장, Label은 제외된 x들만 저장.
		titanic.0_x <- predict(dummies_model, newdata = titanic.0)

	## 24. Data Split ----

		tr_idx <- sample(nrow(titanic.0_x), size=0.5*nrow(titanic.0_x))
		train_x <- titanic.0_x[tr_idx,]
		test_x <- titanic.0_x[-tr_idx,]

		# xgboost는 label 값으로 1,0 요구.
		train_y <- titanic.0$Survived[tr_idx] 
		test_y <- titanic.0$Survived[-tr_idx]  

	## 25. Scaling & NA 처리 ----

		# min-max 방식
		preProcValues <- preProcess(train_x, method = "range")
		train_x <- predict(preProcValues, train_x)
		test_x <- predict(preProcValues, test_x)
		# summary(train_x)

# 30. 모델링 ----------------------------------------------------------------------------

	## 31. 학습 ----
	xgb_model <- xgboost(data = train_x, label = train_y
						 , max.depth = 5  #나무의 크기
						 , eta = .3       #learning rate
						 , nthread = 2    #병렬처리 
						 , nrounds = 400  #오차를 줄이기 위해 반복한 횟수
						 , objective = "binary:logistic" #이진분류
						 , verbose = T)   #실행로그 보여줘!

	#모델링 후 training error에 대한 로그를 제공합니다.
	plot(xgb_model$evaluation_log)

	# 32. 예측 ----

		# 예측 
		# 예측에서도 입력 test 셋을 matrix로!
		pred <- predict(xgb_model, test_x)
		# 예측 결과가 확률값으로 나옵니다.--> ROC를 그릴 수 있습니다.
		pred
		prediction <- as.numeric(pred >= 0.5)
		prediction <- as.factor(prediction)
		test_y <- as.factor(test_y)

	# 33. 검증 ----
		# 1이 Leave 입니다!
		confusionMatrix(prediction, test_y, positive = '1')

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

