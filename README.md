# How COVID-19 & Finedust affect on consumer sentiment and actual revenue on each industry. 
**코로나와 미세먼지가 산업군별 매출에 끼친 영향 및 극복 방안 제시**

####  `Presentation slide is available in the list`

## Table of Contents
* data in use
* model in use
* conclusion 

## data in use 

* card usage data - Shinhan card
* Floating people data 
* Finedust data from weather api
* online sales data 

## model in use 

* preprocess the data using pipe function (dplyr)  `Preprocessing.R`
* multiple regression analysis  `regression_1,2.R`
* clustering analysis `Clustering.R`


## conclusion 

 Since the arrival of the coronavirus, the trend of using **private economy**, or private services, even if it costs more, has been noticeable.
 Industries in the `increasing cluster` generally had many private services centered on members. Thus, a significant increase in sales can be achieved by proposing membership-based services to the industry, which is a `declining cluster`, including hotel, movie theater, gym club, fitness center and so on.

코로나바이러스가 출현한 이후 비용이 더 들더라도 **민간경제** 즉 민간 서비스를 이용하는 추세가 두드러졌다.
증가 클러스터 업종은 일반적으로 회원을 중심으로 한 민간 서비스가 많았다. 이에 따라 호텔 영화관 체육관 피트니스센터 등 쇠퇴 클러스터인 회원제 서비스를 업계에 제안하면 매출이 크게 늘어날 수 있다.
