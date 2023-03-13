# =============================================================================
# Import 
# =============================================================================

library(tidyverse) 
source('code/utils.R')

rawdat <- read_csv("data/covid_race_data_031023.csv") %>% 
	rename(nbene=total) %>% 
	rename(anyDrug=hadAnyDrug) 

epidat <- rawdat %>% 
	select(race, nbene, covid22, inpatientCovid, inpatientOther, died, covidDeath) %>% 
	group_by(race,nbene) %>% 
	pivot_longer(c("covid22", "inpatientCovid", "inpatientOther", "died", "covidDeath"),names_to="metric",values_to="cases") %>% 
	select(race, metric, cases, nbene) 

txdat <- rawdat %>% 
	select(race, nbene, anyDrug, bamlan, casiri, sotrov, bebtel, remdes, nirmat, molnup) %>% 
	group_by(race, nbene) %>% 
	pivot_longer(c("anyDrug", "bamlan", "casiri", "sotrov", "bebtel", "remdes", "nirmat", "molnup"),names_to="drug",values_to="count") %>% 
	select(race, drug, count, nbene) 

epidatlist <- epidat %>% 
	split(.$race) %>% 
	map(~ split(., .$metric)) %>% 
	map(~ map(., ~ pull(., cases)))

txdatlist <- txdat %>% 
	split(.$race) %>% 
	map(~ split(., .$drug)) %>% 
	map(~ map(., ~ pull(., count)))

# =============================================================================
# Generate figures
# =============================================================================

fig_allmax_rawreduction_hosp <- plot_outcomes_cf(get_outcomes_cf_allmax(epidatlist, txdatlist, TE=0.4, drug="nirmat", endpt="inpatientCovid"),"Hospitalizations") #+ scale_y_continuous(limits=c(-5000,25000))
fig_allmax_pctreduction_hosp <- plot_outcomes_cf_pct(get_outcomes_cf_allmax(epidatlist, txdatlist, TE=0.4, drug="nirmat", endpt="inpatientCovid"),"hospitalizations")# + scale_y_continuous(limits=c(-5,20))

fig_redist_rawreduction_hosp <- plot_outcomes_cf(get_outcomes_cf_redist(epidatlist, txdatlist, TE=0.4, drug="nirmat", endpt="inpatientCovid"),"Hospitalizations") #+ scale_y_continuous(limits=c(-5000,25000))
fig_redist_pctreduction_hosp <- plot_outcomes_cf_pct(get_outcomes_cf_redist(epidatlist, txdatlist, TE=0.4, drug="nirmat", endpt="inpatientCovid"),"hospitalizations")# + scale_y_continuous(limits=c(-5,20))

ggsave(fig_allmax_rawreduction_hosp + theme(legend.position="bottom"), file="figures/race/allmax/hosp/allmax_rawreduction_hosp_race.pdf", width=3.5, height=3.5/1.6, units="in")
ggsave(fig_allmax_rawreduction_hosp + theme(legend.position="none"), file="figures/race/allmax/hosp/allmax_rawreduction_hosp_nolegend_race.pdf", width=3.5, height=3.5/1.6, units="in")

ggsave(fig_redist_rawreduction_hosp + theme(legend.position="bottom"), file="figures/race/redist/hosp/redist_rawreduction_hosp_race.pdf", width=3.5, height=3.5/1.6, units="in")
ggsave(fig_redist_rawreduction_hosp + theme(legend.position="none"), file="figures/race/redist/hosp/redist_rawreduction_hosp_nolegend_race.pdf", width=3.5, height=3.5/1.6, units="in")


ggsave(fig_allmax_pctreduction_hosp + theme(legend.position="bottom"), file="figures/race/allmax/hosp/allmax_pctreduction_hosp_race.pdf", width=3.5, height=3.5/1.6, units="in")
ggsave(fig_allmax_pctreduction_hosp + theme(legend.position="none"), file="figures/race/allmax/hosp/allmax_pctreduction_hosp_nolegend_race.pdf", width=3.5, height=3.5/1.6, units="in")

ggsave(fig_redist_pctreduction_hosp + theme(legend.position="bottom"), file="figures/race/redist/hosp/redist_pctreduction_hosp_race.pdf", width=3.5, height=3.5/1.6, units="in")
ggsave(fig_redist_pctreduction_hosp + theme(legend.position="none"), file="figures/race/redist/hosp/redist_pctreduction_hosp_nolegend_race.pdf", width=3.5, height=3.5/1.6, units="in")


fig_allmax_rawreduction_mort <- plot_outcomes_cf(get_outcomes_cf_allmax(epidatlist, txdatlist, TE=0.7, drug="nirmat", endpt="covidDeath"),"Mortality") #+ scale_y_continuous(limits=c(-5000,25000))
fig_allmax_pctreduction_mort <- plot_outcomes_cf_pct(get_outcomes_cf_allmax(epidatlist, txdatlist, TE=0.7, drug="nirmat", endpt="covidDeath"),"mortality")# + scale_y_continuous(limits=c(-5,20))

fig_redist_rawreduction_mort <- plot_outcomes_cf(get_outcomes_cf_redist(epidatlist, txdatlist, TE=0.7, drug="nirmat", endpt="covidDeath"),"Mortality") #+ scale_y_continuous(limits=c(-5000,25000))
fig_redist_pctreduction_mort <- plot_outcomes_cf_pct(get_outcomes_cf_redist(epidatlist, txdatlist, TE=0.7, drug="nirmat", endpt="covidDeath"),"mortality")# + scale_y_continuous(limits=c(-5,20))

ggsave(fig_allmax_rawreduction_mort + theme(legend.position="bottom"), file="figures/race/allmax/mort/allmax_rawreduction_mort_race.pdf", width=3.5, height=3.5/1.6, units="in")
ggsave(fig_allmax_rawreduction_mort + theme(legend.position="none"), file="figures/race/allmax/mort/allmax_rawreduction_mort_nolegend_race.pdf", width=3.5, height=3.5/1.6, units="in")

ggsave(fig_redist_rawreduction_mort + theme(legend.position="bottom"), file="figures/race/redist/mort/redist_rawreduction_mort_race.pdf", width=3.5, height=3.5/1.6, units="in")
ggsave(fig_redist_rawreduction_mort + theme(legend.position="none"), file="figures/race/redist/mort/redist_rawreduction_mort_nolegend_race.pdf", width=3.5, height=3.5/1.6, units="in")


ggsave(fig_allmax_pctreduction_mort + theme(legend.position="bottom"), file="figures/race/allmax/mort/allmax_pctreduction_mort_race.pdf", width=3.5, height=3.5/1.6, units="in")
ggsave(fig_allmax_pctreduction_mort + theme(legend.position="none"), file="figures/race/allmax/mort/allmax_pctreduction_mort_nolegend_race.pdf", width=3.5, height=3.5/1.6, units="in")

ggsave(fig_redist_pctreduction_mort + theme(legend.position="bottom"), file="figures/race/redist/mort/redist_pctreduction_mort_race.pdf", width=3.5, height=3.5/1.6, units="in")
ggsave(fig_redist_pctreduction_mort + theme(legend.position="none"), file="figures/race/redist/mort/redist_pctreduction_mort_nolegend_race.pdf", width=3.5, height=3.5/1.6, units="in")

# =============================================================================
# Generate table 
# =============================================================================

hospdf_allmax <- get_outcomes_cf_allmax(epidatlist, txdatlist, TE=0.4, drug="nirmat", endpt="inpatientCovid") %>% 
	filter(ascertainment==0.5) %>% 
	select(q, outcomes_cf, outcomes) %>% 
	mutate(outcometag="Hospitalizations") %>% 
	mutate(cftag="allmax") %>% 
	mutate(outcomes=round(outcomes)) %>% 
	mutate(outcomes_cf=round(outcomes_cf))

hospdf_redist <- get_outcomes_cf_redist(epidatlist, txdatlist, TE=0.4, drug="nirmat", endpt="inpatientCovid") %>% 
	filter(ascertainment==0.5) %>% 
	select(q, outcomes_cf, outcomes) %>% 
	mutate(outcometag="Hospitalizations") %>% 
	mutate(cftag="redist") %>% 
	mutate(outcomes=round(outcomes)) %>% 
	mutate(outcomes_cf=round(outcomes_cf))

mortdf_allmax <- get_outcomes_cf_allmax(epidatlist, txdatlist, TE=0.4, drug="nirmat", endpt="covidDeath") %>% 
	filter(ascertainment==0.5) %>% 
	select(q, outcomes_cf, outcomes) %>% 
	mutate(outcometag="Mortality") %>% 
	mutate(cftag="allmax") %>% 
	mutate(outcomes=round(outcomes)) %>% 
	mutate(outcomes_cf=round(outcomes_cf))

mortdf_redist <- get_outcomes_cf_redist(epidatlist, txdatlist, TE=0.4, drug="nirmat", endpt="covidDeath") %>% 
	filter(ascertainment==0.5) %>% 
	select(q, outcomes_cf, outcomes) %>% 
	mutate(outcometag="Mortality") %>% 
	mutate(cftag="redist") %>% 
	mutate(outcomes=round(outcomes)) %>% 
	mutate(outcomes_cf=round(outcomes_cf))



hospdf_combined <- hospdf_allmax %>% 
	select(q,outcomes_cf,outcomes) %>% 
	rename("Hosp (all-max)"=outcomes_cf) %>% 
	rename("Hosp (obs)" = outcomes) %>% 
	left_join(select(hospdf_redist,q,`Hosp (redist)`=outcomes_cf), by="q") %>% 
	select(q, `Hosp (obs)`, `Hosp (all-max)`, `Hosp (redist)`)

mortdf_combined <- mortdf_allmax %>% 
	select(q,outcomes_cf,outcomes) %>% 
	rename("Mort (all-max)"=outcomes_cf) %>% 
	rename("Mort (obs)" = outcomes) %>% 
	left_join(select(mortdf_redist,q,`Mort (redist)`=outcomes_cf), by="q") %>% 
	select(q, `Mort (obs)`, `Mort (all-max)`, `Mort (redist)`)


table1 <- left_join(hospdf_combined, mortdf_combined, by="q")

write_csv(table1, "output/table1.csv")













