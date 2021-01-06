
# 기초 통계 함수 ----
zdesc.stat <- function(x) {
  c(n = length(x), na.count = sum(is.na(x))
    , min = min(x, na.rm = T)
    , qt1st = quantile(x, 0.25,na.rm = T)
    , median = median(x, na.rm = T)
    , mean = mean(x, na.rm = T)
    , qt3st = quantile(x, 0.75,na.rm = T)
    , qt90th = quantile(x, 0.9,na.rm = T)
    , qt95th = quantile(x, 0.95,na.rm = T)
    , max = max(x, na.rm = T)
    , range = max(x, na.rm = T) - min(x, na.rm = T)
    , sd = sqrt( sum( (x - mean(x))^2 ) / (length(x) - 1) )
  )
}

# 명목형 변수 바차트  ----
zplot.categoric <- function(cols, df){
  for (col in cols) {
    order.cols <- names(head(sort(table(df[,col]), decreasing = TRUE),10))
    #print(sort(table(df[,col]), decreasing = TRUE))
    
    num.plot <- qplot(df[,col]) +
      geom_bar(fill = 'cornflowerblue') +
      geom_text(aes(label = paste0(round(..count../ sum(..count..),2)*100, "%")), stat='count', vjust=-0.5) +
      theme_minimal() +
      scale_y_continuous(limits = c(0,max(table(df[,col]))*1.1)) +
      scale_x_discrete(limits = order.cols) +
      xlab(col) + ylab('Count') +
      theme(axis.text.x = element_text(angle = 90, size=9)) 
    
    print(num.plot)
  }
}

# 결측치 채우기 ----
NAimputation <- function(df) 
{ 
  v_str <- names(sort(table(df[,col]), decreasing = TRUE))[1] # 명목형 - 최빈값 대체
  idx <- which(is.na(df[,col]))
  df[idx, col] <- v_str
  df
}
col <- "deck"
DF3 <- split(DF, DF$pclass) %>% map(NAimputation) %>% unsplit(DF$pclass)

# 연속형-평균값 대체
DF1$age <- ifelse(is.na(DF1$age), round(mean(DF$age, na.rm = TRUE)), DF1$age)