rm(list=ls())

library(dplyr)
library(stringr)
library(choroplethr) 
library(tidyr)
require(devtools)
library(INLA)# inla version 19.10.01
library(usdm)
library(dplyr)
library(corrgram)
library(dplyr)
library(arm)
library(utils)
setwd("~/Dropbox/")

cov_data <- read.csv("update_full_data_for_county_low_ses_inla_7_1.csv")


# first- znorm  covariates

znorm <- function(ts){
  ts.mean <- mean(ts, na.rm = T)
  ts.dev <- sd(ts, na.rm = T)
  (ts - ts.mean)/ts.dev
}


cov_data['mc_imscoverage'] = cov_data$ims_coverage
cov_data['mc_low_ses_pop'] = znorm(cov_data$low_ses_pop)

# susceptibility
cov_data['pc_poor_health_rate'] = znorm(cov_data$low_edu_poor_health_rate) 

#social cohesion
cov_data['pc_hh_size_rate'] = znorm(cov_data$low_edu_hh_size) 

#absenteeism
cov_data['pc_paid_leave_rate'] = znorm(cov_data$low_ses_absenteeism_rate) 

# vaccination
cov_data['pc_vacc_rate']= znorm(cov_data$low_edu_vacc_rate)

# healthcare utilization
cov_data['pc_insured_rate'] = znorm(cov_data$low_edu_insured_rate)

cov_data['pc_personal_doc_rate'] = znorm(cov_data$low_edu_personal_dr_rate)

cov_data['pc_no_doc_rate'] = znorm(cov_data$low_edu_no_dr_due_to_cost_rate)


# flu
cov_data['pc_humidity_rate']= cov_data$humidity




cov_data %>%
  dplyr::select(starts_with('pc'))->proc_covs

cov_data %>%
  dplyr::select(starts_with('mc'))->meas_covs






vifcor(cbind(proc_covs, meas_covs))

corrgram(cbind(proc_covs, meas_covs))



# response data

cov_data %>% 
  dplyr::select('low_ses_ili_over_ac_02_03', 'low_ses_ili_over_ac_03_04', 'low_ses_ili_over_ac_04_05',
                'low_ses_ili_over_ac_05_06',
                'low_ses_ili_over_ac_06_07', 'low_ses_ili_over_ac_07_08')->response_data



cov_data%>%
  dplyr::select(expected_low_ses_ili_over_ac)-> mod_expected


cov_data %>%
  dplyr::select(state_fips_x)-> state_fips

cov_data %>%
  dplyr::select(mc_low_ses_pop)-> mc_low_ses_pop

cov_data %>%
  dplyr::select(mc_imscoverage)-> mc_imscoverage

mc_imscoverage = as.matrix(mc_imscoverage)
mc_low_ses_pop = as.matrix(mc_low_ses_pop)

Observed= as.matrix(response_data*1000)
plot(hist(Observed))
full_observed = Observed
Observed[Observed<1]
Observed[Observed<1] <- NA


quantile(Observed, probs = 0.95, na.rm = T)
#Expected= as.matrix(rep(1, length(state_fips)))
Expected = as.matrix(mod_expected*1000)
Expected[Expected<1] <- NA
state = as.matrix(state_fips)

proc_covs %>%
  dplyr::select(-pc_humidity_rate)->proc_covs
# proc_covs %>%
#    dplyr::select(-pc_no_doc_rate)->proc_covs
# proc_covs %>%
#   dplyr::select(-pc_insured_rate)->proc_covs
#proc_covs %>%
#  dplyr::select(-pc_personal_doc_rate)->proc_covs

colnames(proc_covs)
lamDM = as.matrix(proc_covs)

#lamDM = as.matrix(proc_covs)

#subset(response_data/10, low_edu_ili_norm_counts_02_03<5)
#plot(hist(log(response_data$low_edu_ili_norm_counts_02_03)))

# all_data = as.data.frame(cbind(Observed, Expected, lamDM, mc_imscoverage))
# 
# sub_all_data = subset(all_data, low_edu_ili_norm_rate_02_03< quantile(Observed, probs = 0.9, na.rm = T))
# sub_all_data = subset(sub_all_data, low_edu_ili_norm_rate_03_04< quantile(Observed, probs = 0.9, na.rm = T))
# sub_all_data = subset(sub_all_data, low_edu_ili_norm_rate_04_05< quantile(Observed, probs = 0.9, na.rm = T))
# sub_all_data = subset(sub_all_data, low_edu_ili_norm_rate_05_06< quantile(Observed, probs = 0.9, na.rm = T))
# sub_all_data = subset(sub_all_data, low_edu_ili_norm_rate_06_07< quantile(Observed, probs = 0.9, na.rm = T))
# sub_all_data = subset(sub_all_data, low_edu_ili_norm_rate_07_08< quantile(Observed, probs = 0.9, na.rm = T))
# 
# sub_all_data %>%
#   dplyr::select(starts_with('low_edu_ili_norm_rate'))->Observed
# 
# sub_all_data %>%
#   dplyr::select(starts_with('low_edu_ili_rate_norm_'))->Expected
# 
# sub_all_data %>%
#   dplyr::select(starts_with('pc'))->lamDM
# 
# sub_all_data %>%
#   dplyr::select(starts_with('mc'))->mc_imscoverage
# mc_imscoverage=as.matrix(mc_imscoverage)

# proc_covs %>%
#   dplyr::select(-pc_no_doc_rate)->proc_covs
# proc_covs %>%
#   dplyr::select(-pc_insured_rate)->proc_covs
#proc_covs %>%
#   dplyr::select(-pc_personal_doc_rate)->proc_covs

mc_no_doc = as.matrix(cov_data$pc_no_doc_rate)
mc_insured= as.matrix(cov_data$pc_insured_rate)
mc_personal_doc = as.matrix(cov_data$pc_personal_doc_rate)

Y1<<- inla.mdata(y = Observed, 1, lamDM)
#Y1<<- inla.mdata(y = Observed, Expected, lamDM)
Modeled<- inla(Y1 ~ -1
               + mc_imscoverage  # measurement model
               + mc_low_ses_pop # mc: measurement covariate, pc: process covariate
               #+ mc_no_doc
               #+ mc_insured
               #+ mc_personal_doc
               + f(state, model="iid")
               , data = list(Y=Y1,
                             mc_imscoverage = mc_imscoverage,
                             mc_low_ses_pop=mc_low_ses_pop#, 
                             #mc_no_doc=mc_no_doc,
                             #mc_insured = mc_insured#,
                             #mc_personal_doc = mc_personal_doc
               ),
               family = 'nmixnb'
               , control.predictor = list(compute = TRUE, link = rep(1, length(Observed[,1])))
               # # returns DIC and WAIC for model fit measures
               , control.compute = list(dic=TRUE, waic=TRUE),
               verbose = TRUE
               # # set parameters for fixed effects
               , control.fixed = list(mean=0)
               , control.inla = list(tolerance = 1e-8, h = 0.0001, restart = 2)
               # # set parameters for process covariates; fix first hyperparameter (theta1) at 1 because it is paired with the 'Expected' offset
               , control.family = list(hyper = list(theta1 = list(mean=1)))
)

summary(Modeled)

#saveRDS(Modeled, file = 'low_ses_county_level_rate_model_rds_file.rds')



process_covar_mean <- Modeled$summary.hyperpar$mean
process_covar_sd <- Modeled$summary.hyperpar$sd




process_covar_mean <- process_covar_mean[1:(length(process_covar_mean)-2)] 
process_covar_sd <- process_covar_sd[1:(length(process_covar_sd)-2)]



meas_covar_mean <- Modeled$summary.fixed$mean#[2] # first item is intercept
meas_covar_sd <- Modeled$summary.fixed$sd#[2]

#meas_covar_mean <- Modeled$summary.fixed$mean # first item is intercept
#meas_covar_sd <- Modeled$summary.fixed$sd

mean_coeffs  <-c(meas_covar_mean, process_covar_mean[2:length(process_covar_mean)])
sd_coeffs <- c(meas_covar_sd, process_covar_sd[2:length(process_covar_mean)])
meas_names = c('ims coverage', 'low_ses_pop')
label_coeffs <- c(meas_names, colnames(proc_covs))

coef_plot_df = as.data.frame(cbind(label_coeffs, mean_coeffs, sd_coeffs))
write.csv(coef_plot_df, 'df_for_coefplot_update_7_1.csv')
coefplot(mean_coeffs, sd_coeffs, varnames = label_coeffs)

png(filename="unnorm_state_model_best_as_of_6_1/caterplot_update_6_1.png", width=900, bg="white")
par(mar=c(12,12,4,1)+.1)
coefplot(mean_coeffs, sd_coeffs, varnames = label_coeffs)
dev.off()

coefplot(mean_coeffs, sd_coeffs, varnames = label_coeffs)




#if (intercept_status == 'ones'){
#lamDM[is.na(lamDM)] <- 0
#mean_log_lambda <-  (Expected*process_covar_mean[1]) + (lamDM %*% process_covar_mean[2:(length(process_covar_mean))])
#}
#if (intercept_status == 'expected'){
#lamDM[is.na(lamDM)] <- 0
#mean_log_lambda <-  (log(as.matrix(Expected))*process_covar_mean[1]) + (as.matrix(lamDM) %*% process_covar_mean[2:(length(process_covar_mean))])
mean_log_lambda <-  (1*process_covar_mean[1]) + (as.matrix(lamDM) %*% process_covar_mean[2:(length(process_covar_mean))])
#mean_log_lambda <-  (log(Modeled$summary.hyperpar$mean[1])*process_covar_mean[1]) + (as.matrix(lamDM) %*% process_covar_mean[2:(length(process_covar_mean))])

#}
#print()
lambda <- exp(mean_log_lambda)

modeled_p <- Modeled$summary.fitted.values$mean

print(length(lambda))
print(length(modeled_p))
modeled_y <- lambda*modeled_p


plot(Observed[,1], modeled_y, xlab="Observed", ylab="Modeled", main = 'Observed vs. Modeled 2002-2003', cex.lab = 2, cex.axis =2, cex.main = 2)
abline(a=0, b=1)

scaled_up_expected = Expected/mean(modeled_p)
rel_risk = lambda/scaled_up_expected

mp <- data.frame("region"=cov_data$fips, "value"=rel_risk)
names(mp)<- c("region", "value")
county_choropleth(mp, num_colors = 7)


save_df = as.data.frame(cbind(cov_data$fips, Observed, Expected, modeled_y, modeled_p, lambda))
colnames(save_df) = c( 'fips', 'low_edu_ili_02_03', 'low_edu_ili_03_04', 'low_edu_ili_04_05',
                       'low_edu_ili_05_06', 'low_edu_ili_06_07', 'low_edu_ili_07_08','mod_expected', 'modeled_y',  'modeled_p', 'lambda')
write.csv(save_df,'best_county_level_results_7_1.csv')




plot(cov_data$prop_low_edu, lambda)
abline(lm(lambda~cov_data$prop_low_edu))
summary(lm(lambda~cov_data$prop_low_edu))


#dev.off()

# 
#pdf(paste0(path_to_folder,"/mod_vs_obs1_no_re.pdf"))
png(filename="obs_vs_mod_1.png", width=900, bg="white")
par(mar=c(5,6,4,5)+.1)
plot(Observed[,1], modeled_y, xlab="Observed", ylab="Modeled", main = 'Observed vs. Modeled 2002-2003', cex.lab = 2, cex.axis =2, cex.main = 2)
abline(a=0, b=1)
dev.off()

png(filename="obs_vs_mod_2.png", width=900, bg="white")
par(mar=c(5,6,4,5)+.1)
plot(Observed[,2], modeled_y, xlab="Observed 2", ylab="Modeled", main = 'Observed vs. Modeled 2003-2004', cex.lab = 2, cex.axis =2, cex.main = 2)
abline(a=0, b=1)
dev.off()

png(filename="obs_vs_mod_3.png", width=900, bg="white")
par(mar=c(5,6,4,5)+.1)
plot(Observed[,3], modeled_y, xlab="Observed 3", ylab="Modeled", main = 'Observed vs. Modeled 2004-2005', cex.lab = 2, cex.axis =2, cex.main = 2)
abline(a=0, b=1)
dev.off()

png(filename="obs_vs_mod_4.png", width=900, bg="white")
par(mar=c(5,6,4,5)+.1)
plot(Observed[,4], modeled_y, xlab="Observed 4", ylab="Modeled", main = 'Observed vs. Modeled 2005-2006', cex.lab = 2, cex.axis =2, cex.main = 2)
abline(a=0, b=1)
dev.off()

png(filename="obs_vs_mod_5.png", width=900, bg="white")
par(mar=c(5,6,4,5)+.1)
plot(Observed[,5], modeled_y, xlab="Observed 5", ylab="Modeled", main = 'Observed vs. Modeled 2006-2007', cex.lab = 2, cex.axis =2, cex.main = 2)
abline(a=0, b=1)
dev.off()

png(filename="obs_vs_mod_6.png", width=900, bg="white")
par(mar=c(5,6,4,5)+.1)
plot(Observed[,6], modeled_y, xlab="Observed 6", ylab="Modeled", main = 'Observed vs. Modeled 2007-2008', cex.lab = 2, cex.axis =2, cex.main = 2)
abline(a=0, b=1)
dev.off()
#dev.off()

png(filename="low_ses_vs_lambda.png", width=900, bg="white")
plot(cov_data$prop_low_edu, lambda/1000, xlab = 'Proportion low SES', ylab = 'Modeled low SES ILI, normalized by all cause')
abline(lm(lambda/1000~cov_data$prop_low_edu), lty = 2)
legend(0.13, y=0.065, 'Linear regression, \n p < 0.001', lty = 2)
dev.off()

png(filename="lambda_map.png", width=900, bg="white")
mp <- data.frame("region"=cov_data$fips, "value"=(lambda))
names(mp)<- c("region", "value")
county_choropleth(mp, num_colors = 7)
dev.off()

png(filename="mod_y.png", width=900, bg="white")
mp <- data.frame("region"=cov_data$fips,"value"=(modeled_y/1000)*cov_data$AC_07_08)
names(mp)<- c("region", "value")
county_choropleth(mp, num_colors = 7)
dev.off()

png(filename="mod_p.png", width=900, bg="white")
mp <- data.frame("region"=cov_data$fips, "value"=modeled_p)
names(mp)<- c("region", "value")
county_choropleth(mp, num_colors = 7)
dev.off()


save_df = as.data.frame(cbind(cov_data$fips, Observed, Expected, modeled_y, modeled_p, lambda))
colnames(save_df) = c( 'fips', 'low_edu_ili_02_03', 'low_edu_ili_03_04', 'low_edu_ili_04_05',
                       'low_edu_ili_05_06', 'low_edu_ili_06_07', 'low_edu_ili_07_08','mod_expected', 'modeled_y',  'modeled_p', 'lambda')
write.csv(save_df,'county_low_edu_ili_model_results_6_18.csv')

data(df_pop_state)
# 
pdf(paste0(path_to_folder,"/mod_vs_obs2_no_re.pdf"))
plot(Observed[,2], modeled_y, xlab="Observed 2", ylab="Modeled")
abline(a=0, b=1)
dev.off()
# 
pdf(paste0(path_to_folder,"/mod_vs_obs3_no_re.pdf"))
plot(Observed[,3], modeled_y, xlab="Observed 3", ylab="Modeled")
abline(a=0, b=1)
dev.off()
# 
pdf(paste0(path_to_folder,"/mod_vs_obs4_no_re.pdf"))
plot(Observed[,4], modeled_y, xlab="Observed 4", ylab="Modeled")
abline(a=0, b=1)
dev.off()
# 
pdf(paste0(path_to_folder,"/mod_vs_obs5_no_re.pdf"))
plot(Observed[,5], modeled_y, xlab="Observed 5", ylab="Modeled")
abline(a=0, b=1)
dev.off()
# 
pdf(paste0(path_to_folder,"/mod_vs_obs6_no_re.pdf"))
plot(Observed[,6], modeled_y, xlab="Observed 6", ylab="Modeled")
abline(a=0, b=1)
dev.off()



# imputation of neighboring coutnies

adj= read.csv('county_adj_update_1_16.csv')

all_counties = list(unique(c(adj$main_county_fips, adj$adj_county_fips)))
all_counties = all_counties[[1]]

county_and_state_fips = as.data.frame(cbind(all_counties, round(all_counties/1000)))
colnames(county_and_state_fips) = c('fips', 'state_fips')

included_states_fips = subset(county_and_state_fips, state_fips %in% cov_data$state_fips_x)
mp <- data.frame("region"=included_states_fips$fips, "value"=rep(5, length(included_states_fips$fips )))
names(mp)<- c("region", "value")
county_choropleth(mp, num_colors = 7)

cov_data_for_all_fips = merge(cov_data, included_states_fips, by = 'fips', all = TRUE)
na_fips_df = subset(cov_data_for_all_fips, is.na(pc_vacc_rate)==T)
for (each_row in seq(1, length(cov_data_for_all_fips$fips))){
  each_fips = cov_data_for_all_fips$fips[each_row]
  if (each_fips %in% na_fips_df$fips){
    each_fips_adj_counties = subset(adj, main_county_fips == each_fips)
    list_adj_counties = each_fips_adj_counties$adj_county_fips
    
    adj_data = subset(cov_data_for_all_fips, fips %in% list_adj_counties)
    mean_adj_data = colMeans(adj_data, na.rm = T)
    mean_adj_data['fips']=each_fips
    
    print(each_fips)
    print(list_adj_counties)
    print(adj_data)
    print(mean_adj_data)
    #print(each_fips)
    #print(mean_adj_data)
    cov_data_for_all_fips[each_row, ] = mean_adj_data
  }
}


mod_p_df = as.data.frame(cbind(cov_data$fips, modeled_p))
colnames(mod_p_df) = c('fips', 'p')
all_fips_p = merge(mod_p_df, included_states_fips, by = 'fips', all = TRUE)
p_na_fips_df = subset(all_fips_p, is.na(p)==T)
for (each_row in seq(1, length(all_fips_p$fips))){
  each_fips = all_fips_p$fips[each_row]
  if (each_fips %in% p_na_fips_df$fips){
    each_fips_adj_counties = subset(adj, main_county_fips == each_fips)
    list_adj_counties = each_fips_adj_counties$adj_county_fips
    
    adj_data = subset(all_fips_p, fips %in% list_adj_counties)
    
    mean_adj_p_data = colMeans(adj_data, na.rm = T)
    
    print(each_fips)
    print(list_adj_counties)
    print(adj_data)
    print(mean_adj_p_data)
   
    mean_adj_p_data['fips']=each_fips
    
    all_fips_p[each_row, ] = mean_adj_p_data
  }
}



expected_df = as.data.frame(cbind(cov_data$fips, cov_data$expected_low_ses_ili_over_ac))
colnames(expected_df) = c('fips', 'expected')
all_fips_expected_df = merge(expected_df, included_states_fips, by = 'fips', all = TRUE)
expected_na_fips_df = subset(all_fips_expected_df, is.na(expected)==T)
for (each_row in seq(1, length(all_fips_expected_df$fips))){
  each_fips = all_fips_expected_df$fips[each_row]
  
  if (each_fips %in% p_na_fips_df$fips){
     print(each_fips)
     each_fips_adj_counties = subset(adj, main_county_fips == each_fips)
     list_adj_counties = each_fips_adj_counties$adj_county_fips
     
     adj_data = subset(all_fips_expected_df, fips %in% list_adj_counties)
     print(adj_data)

     mean_adj_exp_data = colMeans(adj_data, na.rm = T)
     print(mean_adj_exp_data)
  
  #   
     mean_adj_exp_data['fips']=each_fips
  #   
     all_fips_expected_df[each_row, ] = mean_adj_exp_data
  }
}

scaled_up_expected = (all_fips_expected_df$expected*10000)/mean(modeled_p)
rel_risk = lambda/scaled_up_expected

mp <- data.frame("region"=cov_data$fips, "value"=rel_risk)
names(mp)<- c("region", "value")
county_choropleth(mp, num_colors = 7)



all_fips_p 

cov_data_for_all_fips['mc_imscoverage'] = cov_data_for_all_fips$ims_coverage
cov_data_for_all_fips['mc_low_ses_pop'] = znorm(cov_data_for_all_fips$low_ses_pop)

# susceptibility
cov_data_for_all_fips['pc_poor_health_rate'] = znorm(cov_data_for_all_fips$low_edu_poor_health_rate) 

#social cohesion
cov_data_for_all_fips['pc_hh_size_rate'] = znorm(cov_data_for_all_fips$low_edu_hh_size) 

#absenteeism
cov_data_for_all_fips['pc_paid_leave_rate'] = znorm(cov_data_for_all_fips$low_ses_absenteeism_rate) 

# vaccination
cov_data_for_all_fips['pc_vacc_rate']= znorm(cov_data_for_all_fips$low_edu_vacc_rate)

# healthcare utilization
cov_data_for_all_fips['pc_insured_rate'] = znorm(cov_data_for_all_fips$low_edu_insured_rate)

cov_data_for_all_fips['pc_personal_doc_rate'] = znorm(cov_data_for_all_fips$low_edu_personal_dr_rate)

cov_data_for_all_fips['pc_no_doc_rate'] = znorm(cov_data_for_all_fips$low_edu_no_dr_due_to_cost_rate)


# flu
cov_data_for_all_fips['pc_humidity_rate']= cov_data_for_all_fips$humidity


cov_data_for_all_fips %>%
  dplyr::select(starts_with('pc'))->proc_covs


proc_covs %>%
  dplyr::select(-pc_humidity_rate)->proc_covs
# proc_covs %>%
#    dplyr::select(-pc_no_doc_rate)->proc_covs
# proc_covs %>%
#   dplyr::select(-pc_insured_rate)->proc_covs
#proc_covs %>%
#  dplyr::select(-pc_personal_doc_rate)->proc_covs

colnames(proc_covs)
lamDM = as.matrix(proc_covs)


mean_log_lambda <-  (1*process_covar_mean[1]) + (as.matrix(lamDM) %*% process_covar_mean[2:(length(process_covar_mean))])
#mean_log_lambda <-  (log(Modeled$summary.hyperpar$mean[1])*process_covar_mean[1]) + (as.matrix(lamDM) %*% process_covar_mean[2:(length(process_covar_mean))])

#}
#print()
lambda <- exp(mean_log_lambda)

modeled_p <- Modeled$summary.fitted.values$mean

print(length(lambda))
print(length(modeled_p))
modeled_y <- lambda*all_fips_p$p


mp <- data.frame("region"=cov_data_for_all_fips$fips, "value"=lambda)
names(mp)<- c("region", "value")
county_choropleth(mp, num_colors = 7)


save_df = as.data.frame(cbind(cov_data_for_all_fips$fips, lambda, all_fips_p$p, modeled_y))
colnames(save_df) = c( 'fips', 'lambda', 'p', 'y')
write.csv(save_df,'best_county_level_imputed_results_7_7.csv')

