###유동인구 시각화
##코로나 중심 데이터 처리
library(lubridate)
library(dplyr)

Fpopl <- fread("fpopl.csv",stringsAsFactors = F, data.table = F)
address <- fread("adstrd_master.csv", stringsAsFactors =F, data.table = F)


Fpopl %<>% select(base_ymd,adstrd_code,popltn_cascnt)
address %<>% select(adstrd_code,signgu_nm,adstrd_nm)

##연령, 성별 추가 
Fpopl %<>% select(base_ymd,adstrd_code,sexdstn_se_code,agrde_se_code,popltn_cascnt)

#adress code 중심으로 테이블 합치기

Fpopl_address <- left_join(Fpopl,address, by = "adstrd_code")
#서울시로 한정
Fpopl_address %>% filter(as.integer(adstrd_code / 10000000) == 1)
range(Fpopl_address$adstrd_code)

str(Fpopl_address)
head(Fpopl_address)

Fpopl_address %<>% select(-adstrd_code)

Fpopl_address$base_ymd <- as.character(Fpopl_address$base_ymd)
Fpopl_address$base_ymd <- as.Date(Fpopl_address$base_ymd,"%Y%m%d")
#!!!
Fpopl_address  %<>% select(base_ymd, sexdstn_se_code, agrde_se_code, popltn_cascnt)



str(Fpopl_address)
View(head(Fpopl_address,100))

##주차별로 묶기 
Fpopl_address$base_ymd <- week(Fpopl_address$base_ymd)

###주별로 묶기 
week <- c(1:24)
fpopl_mean_1 <- fpopl_mean
fpopl_mean_1 <- c(rep(0,24))
df <- data.frame(week,fpopl_mean_1)

for (i in 1:24){
  a <- Fpopl_address %>%
    filter(base_ymd == i) %>%
    summarise(n=mean(popltn_cascnt))
    
  df$fpopl_mean_1[i] <- a$n
}
###


Time <- fread("Time.csv", stringsAsFactors = F, data.table = F)

str(Time)
head(Time)


#daily confirmed 
daily_confirmed <- diff(as.vector(Time$confirmed))
head(daily_confirmed)
daily_confirmed <- c(0,daily_confirmed)
Time <- cbind(Time, daily_confirmed)
head(Time)

##finedust, corona 합친거 
Dust_corona <- read.csv("Fdust_Corona.csv")

Dust_corona <- Dust_corona[,-1]
str(Dust_corona)

daily_corona <- Dust_corona %>% 
  select(base_ymd,confirmed,daily_confirmed,Finedust,FFinedust)

str(daily_corona)
View(daily_corona)
View(Dust_corona)

daily_corona$base_ymd <- as.Date.character(daily_corona$base_ymd)

str(Fpopl_address)
View(Fpopl_address)
R <- Fpopl_address %>% 
  group_by(base_ymd, sexdstn_se_code, agrde_se_code) %>% 
  summarise(sum(popltn_cascnt))
View(R)

range(Fpopl_corona$Finedust)
View(Fpopl_corona)

Fpopl_corona <- left_join(R, daily_corona, by = "base_ymd")
head(Fpopl_corona)
View(Fpopl_corona)
#유동인구정보랑 코로나 다 합친거
write.csv(Fpopl_corona, file = "Fpopl_corona.csv")


#그냥 raw table로 합치기 
Processed_Fpopl <- plyr::join(Fpopl_address, daily_corona, by = "base_ymd")
Processed_Fpopl_F <- plyr::join(Fpopl_address, Finedust, by = "base_ymd")
Processed_Fpopl <- plyr::join(Fpopl_address, FFinedust, by = "base_ymd")
str(Processed_Fpopl)


# NA값 0으로
Processed_Fpopl$daily_confirmed_case[is.na(Processed_Fpopl$daily_confirmed_case)] <- 0

View(Processed_Fpopl,100)



head(Processed_Fpopl)
write.csv(processed_Fpopl,'')
write.csv(df,file = "floating people.csv")


########주차별 누적 비교
#누적 confirmed
Time$date <- week(Time$date)
Time %<>% 
  group_by(date) %>% 
  summarize(confirmed_mean= mean(confirmed)) %>% 
  unique()

colnames(Time) <- c("week","Confirmed_mean")

#주차별 유동인구 평균 & 확진자 평균
time_fpopl <- plyr::join(Time, df, by = "week")
time_fpopl <- time_fpopl[time_fpopl$week <= 24,]
View(time_fpopl)


##to do : 미세먼지 데이터 합치기
            #일자별로 합치기
            #지수에 따라 좋음, 나쁨 ... 등등

##미세먼지 데이터 테이블 완성 
library(tidyr)
####미세먼지 7월이 몇개 구가 누락되어있어용!
#미세먼지 전처리
df <- FFeb
str(df)
DustPreprocess <- function(df, month){
  #Transpose
  region <- df[,2]
  df <- df[,3]
  df <- data.frame(t(df))
  #colnames(df) <- region
  # TFjan <- rbind(avg, TFjan) #avg는 걍 뺌
  
  #날짜 넣기
  if(month == 1) {
    base_ymd <- seq(as.Date('2020/01/01', '%Y/%m/%d'),
                    as.Date('2020/01/31', '%Y/%m/%d'),
                    1)
  } else if(month == 2) {
    base_ymd <- seq(as.Date('2020/02/01', '%Y/%m/%d'),
                    as.Date('2020/02/28', '%Y/%m/%d'),
                    1)
  } else if(month == 3) {
    base_ymd <- seq(as.Date('2020/03/01', '%Y/%m/%d'),
                    as.Date('2020/03/31', '%Y/%m/%d'),
                    1)
  } else if(month == 4) {
    base_ymd <- seq(as.Date('2020/04/01', '%Y/%m/%d'),
                    as.Date('2020/04/30', '%Y/%m/%d'),
                    1)
  } else if(month == 5) {
    base_ymd <- seq(as.Date('2020/05/01', '%Y/%m/%d'),
                    as.Date('2020/05/31', '%Y/%m/%d'),
                    1)
  } else if(month == 6) {
    base_ymd <- seq(as.Date('2020/06/01', '%Y/%m/%d'),
                    as.Date('2020/06/30', '%Y/%m/%d'),
                    1)
  } else if(month == 7) {
    base_ymd <- seq(as.Date('2020/07/01', '%Y/%m/%d'),
                    as.Date('2020/07/31', '%Y/%m/%d'),
                    1)
  }
  
  df <- cbind(base_ymd, df)
  
}

Fjan <- fread("F1.csv", stringsAsFactors = F, data.table = F)
FFeb <- fread("F2.csv", stringsAsFactors = F, data.table = F)
FMar <- fread("F3.csv", stringsAsFactors = F, data.table = F)
FApr <- fread("F4.csv", stringsAsFactors = F, data.table = F)
FMay <- fread("F5.csv", stringsAsFactors = F, data.table = F)
FJun <- fread("F6.csv", stringsAsFactors = F, data.table = F)
FJul <- fread("F7.csv", stringsAsFactors = F, data.table = F)


Fjan <- DustPreprocess(Fjan,1)
FFeb <- DustPreprocess(FFeb,2)
FMar <- DustPreprocess(FMar,3)
FApr <- DustPreprocess(FApr,4)
FMay <- DustPreprocess(FMay,5)
FJun <- DustPreprocess(FJun,6)
FJul <- DustPreprocess(FJul,7)


FFjan <- fread("FF1.csv", stringsAsFactors = F, data.table = F)
FFFeb <- fread("FF2.csv", stringsAsFactors = F, data.table = F)
FFMar <- fread("FF3.csv", stringsAsFactors = F, data.table = F)
FFApr <- fread("FF4.csv", stringsAsFactors = F, data.table = F)
FFMay <- fread("FF5.csv", stringsAsFactors = F, data.table = F)
FFJun <- fread("FF6.csv", stringsAsFactors = F, data.table = F)
FFJul <- fread("FF7.csv", stringsAsFactors = F, data.table = F)


FFjan <- DustPreprocess(FFjan,1)
FFFeb <- DustPreprocess(FFFeb,2)
FFMar <- DustPreprocess(FFMar,3)
FFApr <- DustPreprocess(FFApr,4)
FFMay <- DustPreprocess(FFMay,5)
FFJun <- DustPreprocess(FFJun,6)
FFJul <- DustPreprocess(FFJul,7)

##데이터 모두 합치기
Finedust <- rbind(Fjan,FFeb,FMar,FApr,FMay,FJun)
FFinedust <- rbind(FFjan,FFFeb,FFMar,FFApr,FFMay,FFJun)
View(FFinedust)

###미세먼지 평균만 냄기기 
str(Finedust)
Finedust %<>% group_by(base_ymd) %>% 
  summarise(Finedust, Mean = mean())
View(Finedust)


Finedust[is.na(Finedust)] <- 0
FFinedust[is.na(FFinedust)] <- 0

FFinedust %>% is.na() %>% colSums()

Finedust$mean <- apply(Finedust[,c(2:26)],1,mean)
FFinedust$mean <- apply(FFinedust[,c(2:26)],1,mean)
head(Finedust[,c(2:27)])
View(FFinedust)

Processed_Fpopl %<>% 
  left_join(Processed_Fpopl, Finedust$mean, by = "base_ymd")


##to do2: 코로나 데이터에 추가 <- 사회적 거리두기 단계
###Tableau에서 마킹하면 될듯 
  #1. 3/21 ~ 5/5 고강도 사회적 거리두기(교회,클럽,헬스장 x)
  #2. 5/6 ~  6/23 생활 거리두기(but 이태원발 코로나)
  #3. 6/24 ~ 단계적 거리두기(수도권 : 2단계)


##to do3: index 정보들 날짜에 맞춰서 업종별 소비자 지수 합치기 


#### to do : 1) 유동인구 & 평균매출건수 & 평균발생금액 차트 그려보기 (업종별로 유동인구에 영향을 많이 받는 업종?)
          #  2) 코로나, 미세먼지 상관관계 분석


##################
###Final output###
##################
#1. 1월 6월까지 주차별 유동인구 평균 & 확진자 평균(미세먼지 평균 추가예정)
    #time_fpopl
#2. 미세먼지 데이터 
  #Finedust <- 미세먼지 1 ~7월, but 7월 특정구 누락되어있어 추가해야됨
  #FFinedust <- 초미세먼지 1~7월

write.csv(time_fpopl, file = "corona v fpopl.csv")

#####memo
##index.csv : 
  #201901 ~ 20200까지만 이씀
idx <- fread("index.csv", stringsAsFactors = F, data.table = F)
head(idx,20)
str(idx)
range(idx$period)

idx %<>% 
  group_by(period,catm,age,gender) %>% 
  mutate(cgi_mean = mean(cgi)) %>% 
  select(period,catm,age,gender,cgi_mean) 

##card.csv(카드매출내역)
  #20200104 ~ 20200614
card <- fread("card_20200717.csv", stringsAsFactors = F, data.table = F)
str(card)
range(card$receipt_dttm)
View(card[1:100,])
#to do : 1.같은 날짜 기준으로 업종별로 매출 합치기 
#        2. 서울시 한정
#        3. 날짜? 
