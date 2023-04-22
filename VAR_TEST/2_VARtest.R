# import the library
library(vars)
library(readxl)
library(zoo)
library(tseries)
timestart<-Sys.time()

traindays = 100 # for VAR test loop
profit = matrix(1,1) # profit list
days= 3 # test VAR for n-days
setwd('data\\2') # the file path
rowdata <- read.csv('test8.csv', sep = ",",
                    encoding = 'UTF-8')
# modify the data
d1 <- rowdata[, !colnames(rowdata)%in%c('Date')]
d1 <- ts(d1) # convert into time series
d1 <- log(d1) # logarithm
d1 = diff(d1) # diff
data_keeper <- d1
dk <- data_keeper
tp <- 0 # time point
stock_num <- dim(data)[2]
lag_max=5

# 生成落後階數list
for (j in c(1:4)){
  lol=r2[j,] # log_order_list
  for (i in c(1:lag_max)){
    if(lol[i]==min(lol)){
      r3[j]= i
    }
  }
}

# 選出眾數
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# 生成p-value matrix
granger_matrix <- matrix(0,2,dim(data)[2]-1)
colnames(granger_matrix) <- colnames(data)[ 2:dim(data)[2] ]
rownames(granger_matrix) <- c('y~x','x~y')

# IRF後predict矩陣生成
m1 <- matrix(0, 2*dim(data)[2]-2, days)
rownames(m1) <- c(colnames(data)[ 2:dim(data)[2] ],
                   colnames(data)[ 2:dim(data)[2] ])

# return matrix
rev_matrix <- matrix()

# ratio df
rate_matrix <- matrix(0, dim(dk)[2], 1)
rownames(rate_matrix) <- colnames(dk)
rate_df <- as.data.frame(rate_matrix)
rate_df <- t(rate_df)

# p-value keeper
pk <- matrix(0, dim(dk)[2]-1, 1)
rownames(pk) <- colnames(dk)[c(2:dim(dk)[2])]
pk <- as.data.frame(pk)
pk <- t(pk)

n<- 652 #
while (tp < n ){
  # train data
  data <- dk[(tp+1):(traindays+tp),]
  
  # test data
  test_start <- (traindays+tp)
  test_end <- (traindays+days+tp+1)
  testdata <- dk[test_start:test_end,] 
  
  # VAR log selection 
  r1 <- VARselect(y=data, lag.max = lag_max, type = c("both"))
  r2 <- r1[["criteria"]]
  r3 <- matrix(0,1,4)
  colnames(r3) <- c('AIC(n)','HQ(n)','SC(n)','FPE(n)')
  lag_max=5
  lag_mode <- getmode(r3[1,])
  ptest <- lag_mode
  if(lag_mode==0){
    lag_mode=1
  }
  
  #建構VAR模型後granger因果檢定 & 預測 loop
  for ( i in c(2:dim(data)[2]) ){
    x_num <- i
    var =  VAR(data[,c(1,x_num)] ,
               ic= 'FPE', 
               #p = ptest, 
               lag.max = 5,
               type = "both")
    # y~x granger test return p-value to martix
    granger_test <- causality(var,
                              cause= colnames(data)[1])
    
    r_granger <- granger_test[["Granger"]]
    pvalue_granger_yx <- r_granger[["p.value"]]
    granger_matrix[1,x_num-1] <- pvalue_granger_yx[1][1]
    
    # VAR prediction
    var.predict <- predict(var,n.ahead=days,ci=0.95)
    pl0 <- var.predict[["fcst"]] # predict list
    pl1 <- pl0[[colnames(data)[i]]]
    pl2 <- pl1[,'fcst']
    m1[i-1,] <- pl2

    # x~y granger test return p-value to martix
    granger_test <- causality(var,
                              cause= colnames(data)[x_num])
    r_granger <- granger_test[["Granger"]]
    pvalue_granger_xy <- r_granger[["p.value"]]
    granger_matrix[2,x_num-1] <- pvalue_granger_xy[1][1]
    
    # VAR prediction
    var.predict <- predict(var,n.ahead=days,ci=0.95)
    pl0 <- var.predict[["fcst"]] # predict list
    pl1 <- pl0[[colnames(data)[1]]]
    pl2 <- pl1[,'fcst']
    m1[i+5-1,] <- pl2
  }
  
  ## 處理days内預測報酬率
  # 預測報酬百分比（去掉%）
  m2 <- (exp(m1)-1)*100
  
  aaa <- m2
#----------------------------------------
  for (aa in c(1:length(aaa))){
      if (aaa[aa]<0){
        aaa[aa]<- rep(0)
      }
    }
#---------------------------------------
  m3 <- aaa
  
  # 檢定p-value martix, m3 經過檢定
  #for (i in c(1:(dim(data)[2]-1))){
  #  if (granger_matrix[1,i]>0.05){
   #   m3[i,] <- rep(0,days)
    #}
  #  if (granger_matrix[2,i]>0.05){
   #   m3[i+days,] <- rep(0,days)
    #}
#  }
  
  pk <- rbind(pk,granger_matrix[2,])
  td <- exp(testdata)-1 # test data
  
  # decision of investitive ratio
  m4 <- m3 
  m5 <- matrix(0,1,days)
  rownames(m5) <- colnames(data)[1]
  
  for (i in c(1:days)){
    if (sum(m3[,i])==0) next
    m4[,i] <- m4[,i]/sum(m3[,i]) 
  }
  # 
  for (i in c(1:days)){
     for (j in c(1:dim(m4)[1])){
       if (m4[j,i]>=3 | m4[j,i]<= -1){
         m4[,i]=0
       }
     }
   }
  for (i in c(1:days)){
    m5[1,i] <- sum(m4[c( stock_num:(stock_num*2-2) ),i])
  }
  
  rate <- rbind (m5, m4[c(1:stock_num-1),])
  
  # rate_matrix <- cbind(rate_matrix, rate)
  # 回測
  rate1 <- t(rate)
  rate_df <- rbind(rate_df, rate1)
  
  testdata <- dk[c(test_start:test_end),] # test data
  testdata <- exp(testdata)-1
  testdata <- t(testdata)
  
  revenue <- testdata[,c(1:days)] * rate
  
  rev_list <- matrix(0,days)
  
  for (i in c(1:days)){
    rev_list[i] <- sum(revenue[,i])
  }
  rev_matrix <- append(rev_matrix, rev_list+1)
  
  tp <- tp+ days
  #print(traindays + tp) # for display of process
  print(tp)
  
}


rev_matrix[is.na(rev_matrix)] <- 1
for (i in c(2:length(rev_matrix))){
  rev_matrix[i] <- rev_matrix[i]*rev_matrix[i-1] 
}

library(ggplot2)
library(grid)
terms <- c(1:length(rev_matrix))
df <- data.frame(terms,rev_matrix)
p1 <- ggplot(df, aes(x=terms, y= rev_matrix-1))+ 
  geom_line()

##日期畫圖
dates <- as.Date(rowdata[c((traindays+1) : (traindays+length(rev_matrix)) ),1],
                 format="%Y-%m-%d")
df_date <- data.frame(dates,rev_matrix)
p2 <- ggplot(df_date, aes(x=dates, y= rev_matrix-1))+ 
  geom_line()+
  theme(plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "inches"))

rownames(rate_df) <- c(1:dim(rate_df)[1])
rate_df <- data.frame(c(1:dim(rate_df)[1]),rate_df)
colnames(rate_df)[1] <- 'terms'

rate_df <- data.frame(dates, rate_df)
#---------------------------------------------#
p3 <- ggplot()+
  geom_line(data = rate_df, aes(dates, TEL))+#####
  theme(plot.margin = unit(c(0.2, 0.2, 0.2, 0.3), "inches"))

#---------------------------------------------#

# 直接使用ggplot對象畫圖
multiplot(p3)
multiplot(p2)

# 將ggplot對象放入列表中，再用列表畫圖, 並設置兩列的並排方式
plot_lst <- list()
plot_lst[[1]] <- p2
plot_lst[[2]] <- p3
# multiplot(plotlist = plot_lst, cols = 1)

##-------pvalue------------##
pk <- data.frame(pk)
#p4 <- ggplot()+
  geom_line(data = pk, aes(index(pk), ))+#####
  geom_hline(aes(yintercept=0.05), colour= 'blue',
             linetype= 2, size=1)
#p4
##-------------------##

timeend<-Sys.time()
runningtime<-timeend-timestart
print(runningtime)
