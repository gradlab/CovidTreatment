__Jump to:__

- [9 Mar 2023](#9-Mar-2023)


# 9 Mar 2023 

Working on a model to describe the risk of hospitalization and death given various levels of treatment, so that we can compare to some counterfactual. 

Consider for now just a single treatment: 

$$ [ncases_k * (1-ptreat_k) * phosp_k] + [ncases_k * ptreat_k * (1-TE_k) * phosp_k] $$ 