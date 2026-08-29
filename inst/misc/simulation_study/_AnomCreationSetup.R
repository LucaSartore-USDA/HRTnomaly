library(tidyr)
library(dplyr)
library(glue)
library(tidyverse)
library(arrow)
library(#hidden code) # Internal NASS package

set.seed(2345)
options(scipen = 999)
options(viewer=NULL)
decimalplaces <- function(x) {
  x[(x - round(x)) < .Machine$double.eps^0.5] <- ""
  a <- nchar(sub('.*\\.', '', x))
  return(a)
}

prep_survey <- function(srvy1, date1){
	df<- #hidden code

	params <- #hidden code

	df <- as.data.frame(df)

	used <- params$varname[grep("display", params$tags)]

	not_used <-#hidden code

	df <- df[, !(names(df) %in% not_used)]

	df$id <- as.numeric(df$id)
	a <- data.frame("class" = unlist(lapply(df, class)))

	df_pvt1 <- df %>%
			   #hidden code

	df_pvt <- df %>%
	          #hidden code

	return(df_pvt)

}

anomomly_tails <- function(df = df1, limit = 0.05, 
                           mstrvrnms = c(df$master_varname[substr(df$master_varname, 1, 1) == "L"]), 
						   hl = 1, hh = 1.1, ll = .9, lh = 1) {

	#select varnames (vs other data)
	if ("VAR_NAME" %in% unique(df$master_varname)) {
		mstrvrnms <- unique(df$master_varname)[substr(unique(df$master_varname), 1, 1) == "L" & unique(df$master_varname) !="#hidden code"]
	} else {
		mstrvrnms <- unique(df$master_varname)[unique(df$master_varname) %in% params$varname[is.na(params$calc_vars)]]
	}

	#subset
	df_pt1 <- df[!is.na(df$current_value_num) & df$current_value_num > 0 & df$master_varname %in% mstrvrnms,] %>%
	          mutate(STATE = ifelse(nchar(id)>10,substr(id,1,2),substr(id,1,1)))

	nrw <- NROW(df_pt1)
	#calculate quantiles for each state/strata/varname
	df_pt2 <- df_pt1 %>%
		      group_by(master_varname, stratay, STATE) %>%
		      summarise(quant.high = as.numeric(quantile(current_value_num, .95)),
			            quant.low = as.numeric(quantile(current_value_num, .05)),
			            max.V = max(current_value_num, na.rm = TRUE),
			            min.V = min(current_value_num, na.rm = TRUE)) %>% 
			  ungroup()
	#calculate anamolous data
	df_pt3 <- df_pt1 %>% 
	          left_join(df_pt2) %>%
	          mutate(probs = runif(n = nrw), 
			         highlow =runif(n = nrw),
					 original_value_num = current_value_num,
					 tail_value_num = case_when(probs < limit & highlow < 0.5 ~ round(quant.high * runif(nrw, hl, hh), 
					                                                                  decimalplaces(original_value_num)),
											    probs < limit & highlow >= 0.5 ~ round(quant.low * runif(nrw, ll, lh), 
												                                       decimalplaces(original_value_num)),
                                                TRUE ~ current_value_num),
					 AnomalyA = ifelse(probs < limit, "t", "")) %>% 
					 select(id, master_varname, tail_value_num, AnomalyA, original_value_num, max.V, min.V, quant.low, quant.high, stratay, STATE)
	return(df_pt3)
}

# unusually high/low reports compared to historical or local data, 
hist_tails <- function(df = df1, limit = 0.05, mstrvrnms = c(df$master_varname[substr(df$master_varname, 1, 1) == "L"]), hl = 1.3, hh = 2, ll = .3, lh = .6) {
	if ("VAR_NAME" %in% unique(df$master_varname)) {
		mstrvrnms <- unique(df$master_varname)[substr(unique(df$master_varname), 1, 1) == "L" & unique(df$master_varname) !="VAR_NAME"]
	} else {
		mstrvrnms <- unique(df$master_varname)[unique(df$master_varname) %in% params$varname[is.na(params$calc_vars)]]
	}

	nrw <- NROW(df %>% filter(master_varname %in% mstrvrnms, prev_value_1 > 0, !is.na(prev_value_1), current_value_num > 0, !is.na(current_value_num)))

	df_pt1 <- df %>%
	          filter(master_varname %in% mstrvrnms, prev_value_1>0, !is.na(prev_value_1), 
			         current_value_num > 0, !is.na(current_value_num)) %>%
              mutate(probs <- runif(n = nrw),
			  		 highlow <- runif(n = nrw), 
					 original_value_num = current_value_num, 
					 hist_value_num = case_when(probs <= limit & highlow >.5 ~ round(prev_value_1 * runif(nrw, hl, hh), 
					                                                                 decimalplaces(current_value_num)),
                                                probs <= limit & highlow <.5 ~ round(prev_value_1 * runif(nrw, ll, lh),
												                                     decimalplaces(current_value_num)),
                                  				TRUE ~ current_value_num), 
					 AnomalyH = ifelse(probs <= limit, "h", "")) %>% 
			  select(id, master_varname, hist_value_num, prev_value_1, AnomalyH, original_value_num)
	return(df_pt1)
}

# unusual response from particular enumerators or enumeration methods etc.
enum_tails <- function(df = df1, limit = 0.05, mstrvrnms = c(df$master_varname[substr(df$master_varname, 1, 1) == "L"]), hl = 1.3, hh = 2, ll = .3, lh = .6) {

	if ("VAR_NAME" %in% unique(df$master_varname)) {
		mstrvrnms <- unique(df$master_varname)[substr(unique(df$master_varname), 1, 1) == "L" & 
		             unique(df$master_varname) != "VAR_NAME" & 
					 unique(df$master_varname) %in% params$calc_vars[!is.na(params$calc_vars)]]
	} else {
		mstrvrnms <- unique(df$master_varname)[unique(df$master_varname) %in% params$varname[is.na(params$calc_vars)]]
	}

	prob_enms <- data.frame("Enums" = unique(df$current_value_num[df$master_varname == "#hidden code"]))
	prob_enms$prblty <- runif(n = NROW(prob_enms))
	prob_enms <- prob_enms$Enums[prob_enms$prblty <= limit]

	enms <- df %>% 
	        filter(master_varname == "MENUMERA") %>% 
			pivot_wider(id_cols = id, 
			            names_from = master_varname, 
						values_from = current_value_num, 
						values_fn = list) %>% 
			unnest(-id)
	nrw <- NROW(df %>% filter(master_varname %in% mstrvrnms, current_value_num > 0, !is.na(current_value_num)))

	df_pt1 <- df %>%
	          filter(master_varname %in% mstrvrnms,
			  		 current_value_num > 0, !is.na(current_value_num)) %>% 
			  left_join(enms) %>%
			  mutate(probs <- runif(n = nrw),
			  		 highlow <- runif(n = nrw),
					 original_value_num = current_value_num,
					 enum_value_num = case_when(
						probs <= limit & MENUMERA %in% prob_enms & highlow > .5 ~ current_value_num * runif(nrw, hl, hh),
						probs <= limit & MENUMERA %in% prob_enms & highlow < .5 ~ current_value_num * runif(nrw, ll, lh),
						TRUE ~ current_value_num),
					 AnomalyE = ifelse(probs <= limit & MENUMERA %in% prob_enms, "e", "")) %>% 
		 	  select(id, master_varname, enum_value_num, AnomalyE, original_value_num, MENUMERA)
	return(df_pt1)
}

relation_tails <- function(df = df1, limit = 0.05, mstrvrnms = c(df$master_varname[substr(df$master_varname, 1, 1) == "L"]), hl = 1.3, hh = 2, ll = .3, lh = .6) {

	# unusual relationships between variables 
	if ("#hidden code" %in% unique(df$master_varname)) {
		mstrvrnms<-unique(df$master_varname)[substr(unique(df$master_varname), 1, 1) == "L" & unique(df$master_varname) !="#hidden code"]
	} else {
		mstrvrnms<-unique(df$master_varname)[unique(df$master_varname) %in% params$varname[is.na(params$calc_vars)]]
	}

	df_pt1 <- df[df$master_varname %in% c('id', mstrvrnms),] %>% 
	          pivot_wider(id_cols = c(id), 
			              names_from = master_varname, 
						  values_from = current_value_num, 
						  values_fn = list) %>% 
			  unnest(-id)
	a_test <- cor(df_pt1[, 2:length(names(df_pt1))], use = "pairwise.complete.obs")
	cor_mat <- a_test %>% 
			   as.data.frame() %>% 
			   rownames_to_column(var = "var1") %>%
			   pivot_longer(cols = -var1, names_to = "var2", values_to = "cor_coeff") %>%
			   filter(var1 != var2 & abs(cor_coeff) >= 0.8 & abs(cor_coeff) < .95)
	df_pt1 <- df %>%
	          filter(master_varname %in% mstrvrnms, current_value_num>0, !is.na(current_value_num))

	nrw <- NROW(df %>% filter(master_varname %in% mstrvrnms, current_value_num > 0, !is.na(current_value_num)))
	df_pt1 <- df_pt1 %>%
              mutate(probs<- runif(n=nrw),
			         highlow <-runif(n = nrw),
					 original_value_num = current_value_num,
					 relat_value_num = case_when(master_varname %in% cor_mat$var1 & probs <= limit & highlow > .5 ~ round(current_value_num * runif(nrw, hl, hh), 
					                                                                                                      decimalplaces(current_value_num)),
												 master_varname %in% cor_mat$var1 & probs <= limit & highlow < .5 ~ round(current_value_num * runif(nrw, ll, lh),
												                                                                          decimalplaces(current_value_num)),
												 TRUE ~ current_value_num),
				     AnomalyR = ifelse(master_varname %in% cor_mat$var1 & probs <= limit, "r", "")) %>% 
	select(id, master_varname, relat_value_num, AnomalyR, original_value_num)
	return(df_pt1)
}

remove_dup_anoms <- function(df1 = df_all, Anom1 = 'AnomalyA', Anom2 = 'AnomalyH') {
	dups <- df1[df1[[Anom1]] != "" & df1[[Anom2]] != "", ]
	dups <- dups[!is.na(dups$id), ]
	if (NROW(dups) > 0) {
		dt = sort(sample(nrow(dups), round(NROW(dups) / 2 + .01, 0)))
		dups[[Anom1]][dt] <- ""
		dups[[Anom2]][-dt] <- ""
		df1[df1$id %in% dups$id & 
		    df1[[Anom1]] != "" & df1[[Anom2]] !="" & 
		    df1$master_varname %in% dups$master_varname & 
		    df1$original_value_num %in% dups$original_value_num & 
		    df1$tail_value_num %in% dups$tail_value_num & 
		    df1$relat_value_num %in% dups$relat_value_num & 
		    df1$hist_value_num %in% dups$hist_value_num, ] <- dups
	} 
	dups <- df1[df1[[Anom1]] != "" & df1[[Anom2]] != "", ]
	dups <- dups[!is.na(dups$id), ]
	print(NROW(dups))
	return(df1)
}

finalize <- function() {
	df_all2 <- merge(at, ht, by = c('id', 'original_value_num', 'master_varname'), all.x = T) %>%
			   merge(rt, by = c('id', 'original_value_num', 'master_varname'), all.x = T) 
	df_all2 <- remove_dup_anoms(df_all2, 'AnomalyA', "AnomalyH")
	df_all2 <- remove_dup_anoms(df_all2, 'AnomalyA', "AnomalyR")
	df_all2 <- remove_dup_anoms(df_all2, 'AnomalyH', "AnomalyR")

	df_all_final <- df_all2 %>%
		mutate(current_value_num = case_when(AnomalyA != "" ~ tail_value_num,
		                                     AnomalyH != "" ~ hist_value_num,
											 AnomalyR != "" ~ relat_value_num,
											 TRUE~original_value_num),
               Anomaly = case_when(AnomalyA != "" ~ AnomalyA,
			                       AnomalyH != "" ~ AnomalyH,
								   AnomalyR != "" ~ AnomalyR,
								   TRUE~"")) %>%
			   select(STATE, stratay, id, master_varname, original_value_num, current_value_num, Anomaly, prev_value_1)
	return(df_all_final)
}

params <- get_survey_parameters(
	domain = 'fake_idartapi-server.dom', 
	survey_abbrev = srvy,
	survey_date = dte
)

df1 <- prep_survey(srvy1 = srvy, date1 = dte)
#combine anomoly tails and historic difference here. 
at <- anomomly_tails(limit = 0.05, hl = 1, hh = 1.1, ll = .9, lh = 1)
ht <- hist_tails(limit = 0.05, hl = 1.3, hh = 2, ll = .3, lh = .6)
rt <- relation_tails(limit = 0.05, hl = 1.3, hh = 2, ll = .3, lh = .6)
df_all_final <- finalize()

write.csv(df_all_final, paste0("~/", srvy, "_", dte, '_low.csv'))
write.csv(unique(df_all_final$id[df_all_final$Anomaly != ""]) , paste0("~/", srvy, "_", dte, '_low_Anom_POIDS.csv'))
write.csv(df_all_final[df_all_final$Anomaly != "",], paste0("~/", srvy, "_", dte, '_low_Anom_POIDS_Vars.csv'))
write.csv(df_all_final[, c("STATE", "stratay", "id", "master_varname", "current_value_num", "prev_value_1" )], paste0("~/", srvy, "_", dte, '_lowF.csv'))

at <- anomomly_tails(limit = 0.05, hl = 2, hh = 3, ll = .2, lh = .3)
ht <- hist_tails(limit = 0.05, hl = 2, hh = 3, ll = .01, lh = .05)
rt <- relation_tails(limit = 0.05, hl = 2, hh = 3, ll = .01, lh = .05)
df_all_final <- finalize()
write.csv(df_all_final, paste0("~/", srvy, "_", dte, '_high.csv'))
write.csv(unique(df_all_final$id[df_all_final$Anomaly != ""]), paste0("~/", srvy, "_", dte, '_high_Anom_POIDS.csv'))
write.csv(df_all_final[df_all_final$Anomaly != "",], paste0("~/", srvy, "_", dte, '_high_Anom_POIDS_Vars.csv'))
write.csv(df_all_final[,c("STATE", "stratay", "id", "master_varname", "current_value_num", "prev_value_1" )], paste0("~/", srvy, "_", dte, '_highF.csv'))
write.csv(params, paste0("~/", srvy, "_", dte, '_params.csv'))
