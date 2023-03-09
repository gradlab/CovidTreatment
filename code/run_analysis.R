library(tidyverse) 

rawdat <- read_csv("data/covid_risk_quintile_data_030623.csv") %>% 
	rename(quintile=`risk quintile`) %>% 
	rename(nbene=total) %>% 
	rename(anyDrug=hadAnyDrug) 

epidat <- rawdat %>% 
	select(quintile, nbene, covid22, inpatientCovid, inpatientOther, died, covidDeath) %>% 
	group_by(quintile,nbene) %>% 
	pivot_longer(c("covid22", "inpatientCovid", "inpatientOther", "died", "covidDeath"),names_to="metric",values_to="cases") %>% 
	select(quintile, metric, cases, nbene) 

txdat <- rawdat %>% 
	select(quintile, nbene, anyDrug, bamlan, casiri, sotrov, bebtel, remdes, nirmat, molnup) %>% 
	group_by(quintile, nbene) %>% 
	pivot_longer(c("anyDrug", "bamlan", "casiri", "sotrov", "bebtel", "remdes", "nirmat", "molnup"),names_to="drug",values_to="count") %>% 
	select(quintile, drug, count, nbene) 
