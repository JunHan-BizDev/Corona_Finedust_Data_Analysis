install.packages("car")
library(car)


##매출, 유동인구, 코로나, 미세먼지, 남, 연령대 
##회귀분석 <- 코로나 & 미세먼지가 매출금액, 매출량에 영향을 실제로 줬나? 
##다중회귀분석으로. 

Base <- Fpopl_corona
card <- fread("Card_Jun.csv", stringsAsFactors = F, data.table = F)
str(card)
card_raw <- fread("card_20200717.csv", stringsAsFactors = F, data.table = F)
str(card_raw)

##얜보류
refined <- card_raw %>% filter(! (selng_cascnt %in% grep('[ㄱ-힣]',unique(card$selng_cascnt), value = T)),
                               ! (salamt %in% grep('[ㄱ-힣]',unique(card$salamt), value = T))) %>% 
  mutate(selng_cascnt = as.numeric(selng_cascnt),
         salamt = as.numeric(salamt)) %>%
  select(- c(adstrd_code, mrhst_induty_cl_code))
######

card_raw %<>% 
  select(receipt_dttm,adstrd_nm, mrhst_induty_cl_nm,selng_cascnt,salamt) %>% 
  group_by(receipt_dttm, mrhst_induty_cl_nm) %>% 
  summarise(Sum_NumOfSales = sum(selng_cascnt),Sum_Amount = sum(salamt))

card_raw %>% is.na() %>% colSums()

colnames(card_raw) <- c("base_ymd","category","Sum_NumOfSales","Sum_Amount")

str(card_raw)
View(card_raw)
##단순 상관관계 regression 돌리기 위한 준비 
Regre_card <- card_raw %>%
  select(-category) %>% 
  group_by(base_ymd) %>% 
  summarize(Sum_NumOfSales = sum(Sum_NumOfSales), Sum_Amount = sum(Sum_Amount))
View(Regre_card)

Regre_card %>% is.na() %>% colSums()

##날짜 date로 처리
Regre_card$base_ymd <- as.character(Regre_card$base_ymd)
Regre_card$base_ymd <- as.Date(Regre_card$base_ymd,"%Y%m%d")

str(Regre_card)
View(Regre_combined)
str(Fpopl_corona)

Regre_combined <- plyr::join(Regre_card, Fpopl_corona, by = "base_ymd")
str(Regre_combined)
View(Regre_combined)
Regre_combined %>% is.na() %>% colSums()

##결측값 행 삭제 
###카드 데이터에 특정 일이 아예 빠져있습니다. 그 행 전체를 제거해야 올바른 분석이 가능합니다
test <- Regre_combined[complete.cases(Regre_combined[,c("confirmed","Finedust")]),]
colnames(test) <- c("Date","NumSales","Sum_Amount","Sex","age","Fpopl","ConfirmedCDF",
                    "DailyConfirmed","Finedust","FFinedust")
View(test)

##위에꺼 했음 안해도 됨 ##
#확진자 시작일부터 고려하도록 전처리 
Regre_combined %<>%
  filter(base_ymd >= "2020-01-20")

##중간 NA값 다 평균으로 대체 
Rgr_wo_NA <- data.frame(sapply(Regre_combined[,c(-1,-2,-3)], 
                               function(x) ifelse(is.na(x), mean(x, na.rm=TRUE), x))) 
##########
View(test)
test %>% is.na() %>% colSums()
str(test)
write.csv(test, file = "Regression4virusFinedust.csv")

####regression 시작
#독립변수 : 미세먼지, 초미세먼지, 코로나 일일 확진자
#종속변수 : 1)매출액, 2)매출건수 
View(test)
str(test)
summary(test$Sum_Amount)

cor(test[,c(-1,-3,-4,-5,-7,-10)])
#                     Sum_NumOfSales sum(popltn_cascnt) daily_confirmed    Finedust
#Sum_NumOfSales          1.0000000         0.10204418     -0.15914256 -0.16537128
#sum(popltn_cascnt)      0.1020442         1.00000000     -0.06097742 -0.01641983
#daily_confirmed        -0.1591426        -0.06097742      1.00000000  0.10387554
#Finedust               -0.1653713        -0.01641983      0.10387554  1.00000000


plot(x=test$NumSales, y=test$DailyConfirmed, xlab="NumofSale", ylab="dailyCFRM")
plot(x=test$`sum(popltn_cascnt)`, y=test$daily_confirmed, xlab="NumofSale", ylab="dailyCFRM")
str(test)
##다중회귀
result_NumSales <- lm(NumSales ~ DailyConfirmed+Finedust,data = test)

summary(result_NumSales)
vif(result_NumSales)

##ㅅㅂ? 값이 예상과 완전 다르게 ..나오네

result_Fpopl <- lm(Fpopl ~  DailyConfirmed+Finedust,data = test)
summary(result_Fpopl)
vif(result_Fpopl)

rr <- lm(Fpopl ~  DailyConfirmed,data = test)
summary(rr)


result_Amount <- lm(as.numeric(Sum_Amount) ~ DailyConfirmed,data = test)

vif(result_Amount)

typeof(test$Sum_Amount)
typeof(test$DailyConfirmed)


a <- read.csv("Allcmbd.csv", stringsAsFactors = F)
a <- a %>% select(-FFinedust)
str(a)


##########
test$Sum_Amount <- as.numeric(test$Sum_Amount)
str(test)
cor(test[,c(-1,-4,-5,-7,-10)])
#                     Sum_NumOfSales sum(popltn_cascnt) daily_confirmed    Finedust
#Sum_NumOfSales          1.0000000         0.10204418     -0.15914256 -0.16537128
#sum(popltn_cascnt)      0.1020442         1.00000000     -0.06097742 -0.01641983
#daily_confirmed        -0.1591426        -0.06097742      1.00000000  0.10387554
#Finedust               -0.1653713        -0.01641983      0.10387554  1.00000000


result_NumSales_F <- lm(log(NumSales) ~ log(Finedust),data = test)
result_NumSales_C <- lm(log(NumSales+1) ~ log(DailyConfirmed+1),data = test)

result_Fpopl_F <- lm(log(Fpopl) ~   log(Finedust),data = test)
result_Fpopl_C <- lm(log(Fpopl+1) ~  log(DailyConfirmed+1),data = test)

result_Amount_F <- lm(log(Sum_Amount) ~ log(Finedust),data = test)
result_Amount_C <- lm(log(Sum_Amount+1) ~ log(DailyConfirmed+1),data = test)


multi <- lm(log(NumSales+1) ~ log(Finedust+1)+log(DailyConfirmed+1),data = test)
multi2 <- lm(log(Fpopl+1) ~ log(Finedust+1)+log(DailyConfirmed+1),data = test)
multi3 <- lm(log(Sum_Amount+1) ~ log(Finedust+1)+log(DailyConfirmed+1),data = test)

summary(multi) #Corona x
summary(multi2) #Finedust x
summary(multi3) #corona x

summary(result_NumSales_F)
summary(result_NumSales_C) #Reject

summary(result_Fpopl_F) #reject
summary(result_Fpopl_C) 

summary(result_Amount_F)
summary(result_Amount_C) #reject


install.packages("effects")
library("effects")
library(ggplot2)
model<-allEffects(multi)
model<-allEffects(multi)

summary(model)

plot(log(test$Fpopl+1) ~ log(test$DailyConfirmed+1), col = "grey", xlab = "DailyConfirmed",
     ylab = "Floating people", ylim = c(13,15), xlim = c(0,4.5))
abline(result_Fpopl_C, lty = 1, col= "blue", untf = TRUE)

plot(log(test$NumSales+1) ~ log(test$Finedust+1))
abline(result_NumSales_F, lty = 1, col= "red", untf = TRUE)

plot(log(test$Sum_Amount+1) ~ log(test$Finedust+1), col = "grey", xlab = "Finedust",
     ylab = "Amount", ylim = c(25.5,26.5))
abline(result_Amount_F, lty = 1, col= "blue", untf = TRUE)


sp<-ggplot(data = test, mapping = aes(x=daily_confirmed, y=Fpopl))
sp+geom_point()+stat_smooth(method=lm) 


write.csv(a, file = "AllCombined.csv")
###업종별로 나눠서 회귀분석을 진행해보면 더 구체적으로 알 수 있지 않을까?
