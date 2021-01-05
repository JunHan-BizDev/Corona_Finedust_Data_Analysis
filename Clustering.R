card <- fread('card_20200717.csv',
              header = T, 
              stringsAsFactors = F,
              data.table = F,
              encoding = 'UTF-8')
## 한글 없애기 ##
data <- card %>% filter(! (selng_cascnt %in% grep('[ㄱ-힣]',unique(card$selng_cascnt), value = T)),
                        ! (salamt %in% grep('[ㄱ-힣]',unique(card$salamt), value = T))) %>% 
  mutate(selng_cascnt = as.numeric(selng_cascnt),
         salamt = as.numeric(salamt)) %>%
  select(- c(adstrd_code, mrhst_induty_cl_code))

rm(list = c('card'))

data$receipt_dttm=data$receipt_dttm %>% as.character() %>% as.Date('%Y%m%d')

## 음수 값 확인 - 양수만 넣기## 
data$selng_cascnt %>% summary()
data$salamt %>% summary()

data = data %>% filter(selng_cascnt > 0, salamt > 0) %>% 
  mutate(receipt_dttm = ymd(receipt_dttm),
         week = week(receipt_dttm))

data %>% glimpse()

index1 = which(data$receipt_dttm == '2020-02-22') %>% max() #코로나 폭발 증가 이전
index2 = which(data$receipt_dttm == '2020-03-10') %>% max() #포발 증가시기
index3 = nrow(data) #안정기

data_period = data 
data_period$period = c(rep(1, index1),
                       rep(2, index2 - index1),
                       rep(3, index3 - index2)
                      )

##이상치 및 결측치 처리##

data_period %>% is.na() %>% colSums()

mean_amount=data_period %>%
  group_by(mrhst_induty_cl_nm) %>% 
  summarise(N_amount=mean(selng_cascnt)) %>% 
  arrange(N_amount)

categories_new=mean_amount %>%
  filter(N_amount>=quantile(mean_amount$N_amount)[2]) %>% 
  arrange(desc(N_amount)) %>% select(mrhst_induty_cl_nm)%>% 
  ungroup()

categories_new <- as.data.frame(categories_new)


data_period <- data_period %>% 
  filter(mrhst_induty_cl_nm%in%
           as.matrix(categories_new,nrow = 1))



data_amount_period <- data_period %>% 
  group_by(period, mrhst_induty_cl_nm) %>% 
  summarise(mean_amount = mean(selng_cascnt)) %>% 
  ungroup() %>% 
  spread(period, value = mean_amount)

data_selling_period <- data_period %>% 
  group_by(period, mrhst_induty_cl_nm) %>% 
  summarise(mean_selling = mean(salamt)) %>% 
  ungroup() %>% 
  spread(period, value = mean_selling)

data_price_period <- data_period %>%
  group_by(period,mrhst_induty_cl_nm) %>%
  summarize(once_price=sum(salamt)/sum(selng_cascnt)) %>%
  ungroup() %>%
  spread(period,value = once_price)


colnames(data_amount_period)[-1] = c('amount_1', 'amount_2', 'amount_3')
colnames(data_selling_period)[-1] = c('selling_1', 'selling_2', 'selling_3')



data_clust = data_period %>% group_by(mrhst_induty_cl_nm) %>% 
  summarise(MEAN_SELLING = mean(salamt),
            MEAN_AMOUNT = mean(selng_cascnt),
            once_price = sum(salamt)/sum(selng_cascnt)

  ) %>%
  ungroup %>% 
  left_join(data_amount_period) %>% 
  left_join(data_selling_period) %>%
  left_join(data_price_period)


clust1 = data_clust %>%
  select(-c(mrhst_induty_cl_nm))

clust_scaled = scale(clust1) %>% as_tibble()



set.seed("123455678")
kmeans1 <- kmeans(clust_scaled, nstart = 10, iter.max = 15, centers = 4)
a<-fviz_nbclust(x = clust_scaled, FUNcluster = kmeans, method='wss') + 
  geom_vline(xintercept = 4, linetype = 2)

b<-fviz_nbclust(x = clust_scaled, FUNcluster = kmeans, method = "silhouette") +
  geom_vline(xintercept = 4, linetype = 2)

data_clust$cluster = kmeans1$cluster

fviz_cluster(kmeans1, clust_scaled)+ theme_bw()+theme(
  legend.background = element_rect(color = 'black', 
                                   size = 0.5),plot.margin=margin(50,10,50,10))


final_test_k = data_period %>% 
  left_join(data_clust) %>% 
  group_by(period, cluster) %>% 
  summarise(mean_amount = mean(selng_cascnt),
            mean_selling = mean(salamt),
            once_price = sum(salamt)/sum(selng_cascnt)
  ) %>% ungroup() %>% 
  select(c(period, cluster, mean_amount, mean_selling,once_price))

str(final_test_k)

write.csv(final_test_k, file = "ClusteringResult.csv")

