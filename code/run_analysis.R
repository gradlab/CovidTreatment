# =============================================================================
# Import 
# =============================================================================

# [1] "covid22"        "inpatientCovid" "inpatientOther" "died"
# [5] "covidDeath"

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


get_outcomes <- function(cases, ascertainment, treatments, poutcome, TE){

	# Error handling:
	if(ascertainment<0 | ascertainment>1){stop("Ascertainment must be between 0 (no ascertainment) and 1 (full ascertainment)")}
	if(poutcome<0 | poutcome>1){stop("Outcome probability (poutcome) must be between 0 and 1")}
	if(TE<0 | TE>1){stop("Treatment effectiveness (TE) must be between 0 and 1")}
	if(cases/ascertainment<treatments){stop("Ascertainment rate is too high; it must allow the true cases (observed/ascertainment) to exceed the number of treatments")}

	out <- (cases/ascertainment-treatments)*poutcome + treatments*poutcome*(1-TE)

	return(out)

}

get_pout <- function(cases, ascertainment, treatments, TE, outcomes){

	# Error handling:
	if(ascertainment<0 | ascertainment>1){stop("Ascertainment must be between 0 (no ascertainment) and 1 (full ascertainment)")}
	if(TE<0 | TE>1){stop("Treatment effectiveness (TE) must be between 0 and 1")}
	if(cases/ascertainment<treatments){stop("Ascertainment rate is too high; it must allow the true cases (observed/ascertainment) to exceed the number of treatments")}

	pout <- outcomes/((cases/ascertainment) - treatments*TE)
	return(pout)

}


get_outcomes(909758, 0.6, 283897, 0.1, 0.8)


tevec <- seq(from=0, to=1, by=0.01)
poutvec <- unlist(lapply(tevec, function(x){get_pout(909758, 0.6, 283897, x, 12571)}))
tibble(te=tevec, pout=poutvec) %>% 
	ggplot(aes(x=te, y=pout*100)) + 
		geom_line() + 
		theme_classic() 


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


# What if everyone were treated at the highest per-case rate? 

TEvec <- seq(from=0, to=1, by=0.02) 
outcomes_cf_df <- tibble()

xi <- unlist(lapply(1:5, function(x){
	myq <- paste0("q",x)
	out <- txdatlist[[myq]]$nirmat / epidatlist[[myq]]$covid22
	return(out)}))

outcomes <- unlist(lapply(1:5, function(x){
	myq <- paste0("q",x)
	out <- epidatlist[[myq]]$inpatientCovid 
	return(out)}))

for(indexA in 1:length(TEvec)){

	pout <- unlist(lapply(1:5, 
		function(x){
			myq <- paste0("q",x)
			get_pout(
				cases=epidatlist[[myq]]$covid22, 
				ascertainment=0.6, 
				treatments=txdatlist[[myq]]$nirmat,
				TE=TEvec[indexA],
				outcomes=epidatlist[[myq]]$inpatientCovid)
		}))

	outcomes_cf <- unlist(lapply(1:5, 
		function(x){
			myq <- paste0("q",x)
			get_outcomes(
				cases=epidatlist[[myq]]$covid22, 
				ascertainment=0.6, 
				treatments=max(xi)*epidatlist[[myq]]$covid22,
				poutcome=pout[x],
				TE=TEvec[indexA])
		}))

	outcomes_cf_df <- outcomes_cf_df %>% 
		bind_rows(tibble(TE=TEvec[indexA],q=1:5,outcomes_cf=outcomes_cf))

}


fig_allmax <- outcomes_cf_df %>% 
	left_join(tibble(q=1:5, outcomes=outcomes), by="q") %>% 
	mutate(q=as.character(q)) %>% 
	split(.$TE) %>% 
	map(~ bind_rows(., tibble(TE=.$TE[1], q="TOT", outcomes_cf=sum(.$outcomes_cf), outcomes=sum(.$outcomes)))) %>% 
	bind_rows() %>% 
	mutate(diff=outcomes - outcomes_cf) %>% 
	mutate(diff_pct = diff/outcomes*100) %>% 
	ggplot(aes(x=TE, y=diff, col=factor(q), lty=factor(q))) + 
	# ggplot(aes(x=TE, y=diff_pct, col=factor(q), lty=factor(q))) + 
		geom_line() + 
		theme_classic() + 
		scale_color_manual(values=c("cyan1","cyan2","cyan3","cyan4","dodgerblue3","black")) + 
		scale_linetype_manual(values=c("solid","solid","solid","solid","solid","dashed")) 


# What if we gave the same number of treatments, but allocated them better? 


TEvec <- seq(from=0, to=1, by=0.02) 
outcomes_cf_df <- tibble()

xi <- unlist(lapply(1:5, function(x){
	myq <- paste0("q",x)
	out <- txdatlist[[myq]]$nirmat / epidatlist[[myq]]$covid22
	return(out)}))

tx <- unlist(lapply(1:5, function(x){
	myq <- paste0("q",x)
	out <- txdatlist[[myq]]$nirmat
	return(out)}))

outcomes <- unlist(lapply(1:5, function(x){
	myq <- paste0("q",x)
	out <- epidatlist[[myq]]$inpatientCovid 
	return(out)}))

for(indexA in 1:length(TEvec)){

	pout <- unlist(lapply(1:5, 
		function(x){
			myq <- paste0("q",x)
			get_pout(
				cases=epidatlist[[myq]]$covid22, 
				ascertainment=0.6, 
				treatments=txdatlist[[myq]]$nirmat,
				TE=TEvec[indexA],
				outcomes=epidatlist[[myq]]$inpatientCovid)
		}))

	
	tx_cf <- pout/sum(pout)*sum(tx)

	outcomes_cf <- unlist(lapply(1:5, 
		function(x){
			myq <- paste0("q",x)
			get_outcomes(
				cases=epidatlist[[myq]]$covid22, 
				ascertainment=0.6, 
				treatments=tx_cf[x],
				poutcome=pout[x],
				TE=TEvec[indexA])
		}))

	outcomes_cf_df <- outcomes_cf_df %>% 
		bind_rows(tibble(TE=TEvec[indexA],q=1:5,outcomes_cf=outcomes_cf))

}

fig_optim <- outcomes_cf_df %>% 
	left_join(tibble(q=1:5, outcomes=outcomes), by="q") %>% 
	mutate(q=as.character(q)) %>% 
	split(.$TE) %>% 
	map(~ bind_rows(., tibble(TE=.$TE[1], q="TOT", outcomes_cf=sum(.$outcomes_cf), outcomes=sum(.$outcomes)))) %>% 
	bind_rows() %>% 
	mutate(diff=outcomes - outcomes_cf) %>% 
	mutate(diff_pct = diff/outcomes*100) %>% 
	ggplot(aes(x=TE, y=diff, col=factor(q), lty=factor(q))) + 
	# ggplot(aes(x=TE, y=diff_pct, col=factor(q), lty=factor(q))) + 
		geom_line() + 
		theme_classic() + 
		scale_color_manual(values=c("cyan1","cyan2","cyan3","cyan4","dodgerblue3","black")) + 
		scale_linetype_manual(values=c("solid","solid","solid","solid","solid","dashed")) 


































