# =============================================================================
# Import 
# =============================================================================

library(tidyverse) 
source('code/utils.R')

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

epidatlist <- epidat %>% 
	mutate(quintile=as.character(quintile)) %>% 
	mutate(quintile=paste0("q",quintile)) %>% 
	split(.$quintile) %>% 
	map(~ split(., .$metric)) %>% 
	map(~ map(., ~ pull(., cases)))

txdatlist <- txdat %>% 
	mutate(quintile=as.character(quintile)) %>% 
	mutate(quintile=paste0("q",quintile)) %>% 
	split(.$quintile) %>% 
	map(~ split(., .$drug)) %>% 
	map(~ map(., ~ pull(., count)))

# =============================================================================
# Generate output 
# =============================================================================

fig_allmax_rawreduction <- plot_outcomes_cf(get_outcomes_cf_allmax(epidatlist, txdatlist, TE=0.8, drug="nirmat", endpt="inpatientCovid"))
fig_allmax_pctreduction <- plot_outcomes_cf_pct(get_outcomes_cf_allmax(epidatlist, txdatlist, TE=0.8, drug="nirmat", endpt="inpatientCovid"))

fig_redist_rawreduction <- plot_outcomes_cf(get_outcomes_cf_redist(epidatlist, txdatlist, TE=0.8, drug="nirmat", endpt="inpatientCovid"))
fig_redist_pctreduction <- plot_outcomes_cf_pct(get_outcomes_cf_redist(epidatlist, txdatlist, TE=0.8, drug="nirmat", endpt="inpatientCovid"))



plot_outcomes_cf(get_outcomes_cf_allmax(epidatlist, txdatlist, TE=0.8, drug="nirmat", endpt="inpatientCovid"))















