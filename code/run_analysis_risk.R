# =============================================================================
# Import 
# =============================================================================

library(tidyverse) 
source('code/utils.R')

rawdat <- read_csv("data/covid_risk_quintile_data_031023.csv") %>% 
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

fig_allmax_rawreduction_hosp <- plot_outcomes_cf(get_outcomes_cf_allmax(epidatlist, txdatlist, TE=0.8, drug="nirmat", endpt="inpatientCovid")) + scale_y_continuous(limits=c(-5000,25000))
fig_allmax_pctreduction_hosp <- plot_outcomes_cf_pct(get_outcomes_cf_allmax(epidatlist, txdatlist, TE=0.8, drug="nirmat", endpt="inpatientCovid"))# + scale_y_continuous(limits=c(-5,20))

fig_redist_rawreduction_hosp <- plot_outcomes_cf(get_outcomes_cf_redist(epidatlist, txdatlist, TE=0.8, drug="nirmat", endpt="inpatientCovid")) + scale_y_continuous(limits=c(-5000,25000))
fig_redist_pctreduction_hosp <- plot_outcomes_cf_pct(get_outcomes_cf_redist(epidatlist, txdatlist, TE=0.8, drug="nirmat", endpt="inpatientCovid"))# + scale_y_continuous(limits=c(-5,20))

ggsave(fig_allmax_rawreduction_hosp + theme(legend.position="bottom"), file="figures/allmax_rawreduction_hosp.pdf", width=3.5, height=3.5/1.6, units="in")
ggsave(fig_allmax_rawreduction_hosp + theme(legend.position="none"), file="figures/allmax_rawreduction_hosp_nolegend.pdf", width=3.5, height=3.5/1.6, units="in")

ggsave(fig_redist_rawreduction_hosp + theme(legend.position="bottom"), file="figures/redist_rawreduction_hosp.pdf", width=3.5, height=3.5/1.6, units="in")
ggsave(fig_redist_rawreduction_hosp + theme(legend.position="none"), file="figures/redist_rawreduction_hosp_nolegend.pdf", width=3.5, height=3.5/1.6, units="in")


ggsave(fig_allmax_pctreduction_hosp + theme(legend.position="bottom"), file="figures/allmax_pctreduction_hosp.pdf", width=3.5, height=3.5/1.6, units="in")
ggsave(fig_allmax_pctreduction_hosp + theme(legend.position="none"), file="figures/allmax_pctreduction_hosp_nolegend.pdf", width=3.5, height=3.5/1.6, units="in")

ggsave(fig_redist_pctreduction_hosp + theme(legend.position="bottom"), file="figures/redist_pctreduction_hosp.pdf", width=3.5, height=3.5/1.6, units="in")
ggsave(fig_redist_pctreduction_hosp + theme(legend.position="none"), file="figures/redist_pctreduction_hosp_nolegend.pdf", width=3.5, height=3.5/1.6, units="in")


fig_allmax_rawreduction_mort <- plot_outcomes_cf(get_outcomes_cf_allmax(epidatlist, txdatlist, TE=0.8, drug="nirmat", endpt="covidDeath")) + scale_y_continuous(limits=c(-5000,25000))
fig_allmax_pctreduction_mort <- plot_outcomes_cf_pct(get_outcomes_cf_allmax(epidatlist, txdatlist, TE=0.8, drug="nirmat", endpt="covidDeath"))# + scale_y_continuous(limits=c(-5,20))

fig_redist_rawreduction_mort <- plot_outcomes_cf(get_outcomes_cf_redist(epidatlist, txdatlist, TE=0.8, drug="nirmat", endpt="covidDeath")) + scale_y_continuous(limits=c(-5000,25000))
fig_redist_pctreduction_mort <- plot_outcomes_cf_pct(get_outcomes_cf_redist(epidatlist, txdatlist, TE=0.8, drug="nirmat", endpt="covidDeath"))# + scale_y_continuous(limits=c(-5,20))

ggsave(fig_allmax_rawreduction_mort + theme(legend.position="bottom"), file="figures/allmax_rawreduction_mort.pdf", width=3.5, height=3.5/1.6, units="in")
ggsave(fig_allmax_rawreduction_mort + theme(legend.position="none"), file="figures/allmax_rawreduction_mort_nolegend.pdf", width=3.5, height=3.5/1.6, units="in")

ggsave(fig_redist_rawreduction_mort + theme(legend.position="bottom"), file="figures/redist_rawreduction_mort.pdf", width=3.5, height=3.5/1.6, units="in")
ggsave(fig_redist_rawreduction_mort + theme(legend.position="none"), file="figures/redist_rawreduction_mort_nolegend.pdf", width=3.5, height=3.5/1.6, units="in")


ggsave(fig_allmax_pctreduction_mort + theme(legend.position="bottom"), file="figures/allmax_pctreduction_mort.pdf", width=3.5, height=3.5/1.6, units="in")
ggsave(fig_allmax_pctreduction_mort + theme(legend.position="none"), file="figures/allmax_pctreduction_mort_nolegend.pdf", width=3.5, height=3.5/1.6, units="in")

ggsave(fig_redist_pctreduction_mort + theme(legend.position="bottom"), file="figures/redist_pctreduction_mort.pdf", width=3.5, height=3.5/1.6, units="in")
ggsave(fig_redist_pctreduction_mort + theme(legend.position="none"), file="figures/redist_pctreduction_mort_nolegend.pdf", width=3.5, height=3.5/1.6, units="in")

