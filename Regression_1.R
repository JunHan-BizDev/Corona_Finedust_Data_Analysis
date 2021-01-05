#과정 
test <- read.csv("Regression4virusFinedust.csv", stringsAsFactors = F)
##카드데이터 전처리(필요한 행만 추출)
card_raw %<>% 
  select(receipt_dttm,adstrd_nm, mrhst_induty_cl_nm,selng_cascnt,salamt) %>% 
  group_by(receipt_dttm, mrhst_induty_cl_nm) %>% 
  summarise(Sum_NumOfSales = sum(selng_cascnt),Sum_Amount = sum(salamt))

Regre_card <- card_raw %>% select(-category) %>% group_by(base_ymd) %>% 
  summarize(Sum_NumOfSales = sum(Sum_NumOfSales), Sum_Amount = sum(Sum_Amount))


#카드데이터와 미세먼지, 코로나 데이터 병합
Regre_combined <- plyr::join(Regre_card, Fpopl_corona, by = "base_ymd")

#확진자 시작일부터 고려하도록 전처리 
Regre_combined %<>%
  filter(base_ymd >= "2020-01-20")

##중간 NA값 다 평균으로 대체 
Regre_combined %>% is.na() %>% colSums()
test <- data.frame(sapply(Regre_combined[,c(-1,-2,-3)], 
                               function(x) ifelse(is.na(x), mean(x, na.rm=TRUE), x))) 




