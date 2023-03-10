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

# What if everyone were treated at the highest per-case rate?
get_outcomes_cf_allmax <- function(epidatlist, txdatlist, ascertainment=0.6, drug="nirmat",endpt="inpatientCovid"){

	TEvec <- seq(from=0, to=1, by=0.02) 
	outcomes_cf_df <- tibble()

	xi <- unlist(lapply(1:5, function(x){
		myq <- paste0("q",x)
		out <- txdatlist[[myq]][[drug]] / epidatlist[[myq]]$covid22
		return(out)}))

	outcomes <- unlist(lapply(1:5, function(x){
		myq <- paste0("q",x)
		out <- epidatlist[[myq]][[endpt]] 
		return(out)}))

	for(indexA in 1:length(TEvec)){

		pout <- unlist(lapply(1:5, 
			function(x){
				myq <- paste0("q",x)
				get_pout(
					cases=epidatlist[[myq]]$covid22, 
					ascertainment=ascertainment, 
					treatments=txdatlist[[myq]][[drug]],
					TE=TEvec[indexA],
					outcomes=epidatlist[[myq]][[endpt]])
			}))

		outcomes_cf <- unlist(lapply(1:5, 
			function(x){
				myq <- paste0("q",x)
				get_outcomes(
					cases=epidatlist[[myq]]$covid22, 
					ascertainment=ascertainment, 
					treatments=max(xi)*epidatlist[[myq]]$covid22,
					poutcome=pout[x],
					TE=TEvec[indexA])
			}))

		outcomes_cf_df <- outcomes_cf_df %>% 
			bind_rows(tibble(TE=TEvec[indexA],q=1:5,outcomes_cf=outcomes_cf))

	}

	outcomes_cf_df <- outcomes_cf_df %>% 
		left_join(tibble(q=1:5, outcomes=outcomes), by="q") %>% 
		mutate(q=as.character(q)) %>% 
		split(.$TE) %>% 
		map(~ bind_rows(., tibble(TE=.$TE[1], q="Total", outcomes_cf=sum(.$outcomes_cf), outcomes=sum(.$outcomes)))) %>% 
		bind_rows() %>% 
		mutate(diff=outcomes - outcomes_cf) %>% 
		mutate(diff_pct = diff/outcomes*100)

	return(outcomes_cf_df)

}


# What if we gave the same number of treatments, but allocated them better? 
get_outcomes_cf_redist <- function(epidatlist, txdatlist, ascertainment=0.6, drug="nirmat",endpt="inpatientCovid"){


	TEvec <- seq(from=0, to=1, by=0.02) 
	outcomes_cf_df <- tibble()

	xi <- unlist(lapply(1:5, function(x){
		myq <- paste0("q",x)
		out <- txdatlist[[myq]][[drug]] / epidatlist[[myq]]$covid22
		return(out)}))

	tx <- unlist(lapply(1:5, function(x){
		myq <- paste0("q",x)
		out <- txdatlist[[myq]][[drug]]
		return(out)}))

	outcomes <- unlist(lapply(1:5, function(x){
		myq <- paste0("q",x)
		out <- epidatlist[[myq]][[endpt]] 
		return(out)}))

	for(indexA in 1:length(TEvec)){

		pout <- unlist(lapply(1:5, 
			function(x){
				myq <- paste0("q",x)
				get_pout(
					cases=epidatlist[[myq]]$covid22, 
					ascertainment=ascertainment, 
					treatments=txdatlist[[myq]][[drug]],
					TE=TEvec[indexA],
					outcomes=epidatlist[[myq]][[endpt]])
			}))

		
		tx_cf <- pout/sum(pout)*sum(tx)

		outcomes_cf <- unlist(lapply(1:5, 
			function(x){
				myq <- paste0("q",x)
				get_outcomes(
					cases=epidatlist[[myq]]$covid22, 
					ascertainment=ascertainment, 
					treatments=tx_cf[x],
					poutcome=pout[x],
					TE=TEvec[indexA])
			}))

		outcomes_cf_df <- outcomes_cf_df %>% 
			bind_rows(tibble(TE=TEvec[indexA],q=1:5,outcomes_cf=outcomes_cf))

	}

	outcomes_cf_df <- outcomes_cf_df %>% 
		left_join(tibble(q=1:5, outcomes=outcomes), by="q") %>% 
		mutate(q=as.character(q)) %>% 
		split(.$TE) %>% 
		map(~ bind_rows(., tibble(TE=.$TE[1], q="Total", outcomes_cf=sum(.$outcomes_cf), outcomes=sum(.$outcomes)))) %>% 
		bind_rows() %>% 
		mutate(diff=outcomes - outcomes_cf) %>% 
		mutate(diff_pct = diff/outcomes*100)

	return(outcomes_cf_df)


}

plot_outcomes_cf <- function(outcomes_cf){
	out <- outcomes_cf %>% 
		ggplot(aes(x=TE, y=diff, col=factor(q), lty=factor(q))) + 
			geom_line() + 
			theme_classic() + 
			scale_color_manual(values=c("cyan1","cyan2","cyan3","cyan4","dodgerblue3","black")) + 
			scale_linetype_manual(values=c("solid","solid","solid","solid","solid","dashed"))  + 
			labs(x="Treatment efficacy", y="Adverse events averted\nthrough treatment re-allocation", col="Risk quantile", lty="Risk quantile")

	return(out)
}

plot_outcomes_cf_pct <- function(outcomes_cf){
	out <- outcomes_cf %>% 
		ggplot(aes(x=TE, y=diff_pct, col=factor(q), lty=factor(q))) + 
			geom_line() + 
			theme_classic() + 
			scale_color_manual(values=c("cyan1","cyan2","cyan3","cyan4","dodgerblue3","black")) + 
			scale_linetype_manual(values=c("solid","solid","solid","solid","solid","dashed"))  + 
			labs(x="Treatment efficacy", y="Percent decrease in adverse events\nthrough treatment re-allocation", col="Risk quantile", lty="Risk quantile")

	return(out)
}

