set.seed(121008)

# Both vaccines in one file
# All data in the IC added
# Incidence and p_PHN from CPRD Jemma study
# VE zostavax now includes ZEST study
# VE updated to incorporate age pyramid for ZVL and age variation for data in time for both vaccines
# item of service fee updated to £10.06  (for 2018/19, https://www.nhsemployers.org/-/media/Employers/Documents/Primary-care-contracts/V-and-I/201819-Vaccination-and-immunisation-guidance-and-audit-requirements.PDF?la=en&hash=B3DFFE1BE23C5826841A87704FF305964A481A42)
# QALYs updated new data Brisson and Drolet vs Rampakakis Canada
# incidences transformed to probabilities
# mortality of HZ included
# updated p_PHN excluding individuals vaccinated from 2013
# oputpatients costs age 90+ all the same as age 90
# updated costs of hospitalisation HZ using robust regression
# 29-05-2018 extra QL avoided o3m PHN only for ZVL
# Decide whether to include the extra reduction in QL against PHN for breakthrough HZ  (reduction in QL for the first 6 months meant for Zostavax but included for Sept 2017 CEA JCVI for both and o3m QL included for both in May 2018)
# change vaccine_cost_per_dose, number_courses and vaccine effectiveness, extra QL adverted, for other vaccine  
# For both vaccines QL-> Area between model and 1, up to when model reaches one (LE)
# QL with transformation 0.3-0.7 (QL 22-11-2017 file) (vs. transformation 0.25-0.75 as per Sept 2017 CEA JCVI)
# PSA


#########################################################################################################################
# Cost-effectiveness analysis of herpes zoster vaccination
#########################################################################################################################



##########################
#### Prep

## set wd directory
wdir<-"H:\\HZ work\\Projects\\Model"
# wdir<-"/Users/aliciarosello/Desktop/HZ work/Projects/Model/"
setwd(wdir)

### paths
Plots<-paste(wdir,"\\R\\Plots\\", sep="")
Data<-paste(wdir,"\\Data\\", sep="")
Output<-paste(wdir, "\\R\\Output\\", sep="")

### libraries
library(dplyr); library(ggplot2);library(betareg); #library(lhs); library(triangle); library(meta)

### start timer
start_time <- Sys.time()

### start loop
for (k in 50:90){


  
  ##########################
  #### Vaccine, IC status, age
  
  ### vaccine
  # vaccine<-"zostavax"
  vaccine<-"shingrix"
  
  ### vaccination age
  vaccination_age<-k
  
  ### IC status
  # IC_status<-"non IC"
  IC_status<- "IC"
  
  
  ##########################
  #### PSA
  
  n.run <- 1000
  
  
  ##########################
  #### Parameters
  
  ### discount rate costs
  discount_rate_costs<-0.035
  
  ### discount rate effects
  discount_rate_effects<-0.035
  
  ### vaccine coverage
  ## AJ's model
  # vaccine_coverage<-0.735 
  ## PHE report 0.483 in 2016/17, 0.549 in 2015/16, 0.59 in 2014/15, 0.618 in 2013/14
  vaccine_coverage<-0.483 
  
  ### cost per vaccine dose
  if(vaccine=="zostavax"){
  vaccine_cost_per_dose<-100
  } else if (vaccine=="shingrix"){
  vaccine_cost_per_dose<-75
  } else {print("Error")}
  
  ### admin cost per vaccine dose (item of service fee for 2018/19, https://www.nhsemployers.org/-/media/Employers/Documents/Primary-care-contracts/V-and-I/201819-Vaccination-and-immunisation-guidance-and-audit-requirements.PDF?la=en&hash=B3DFFE1BE23C5826841A87704FF305964A481A42)
  admin_cost_per_dose<-10
  
  ### number of doses
  if(vaccine=="zostavax"){
  number_courses<-1
  } else if (vaccine=="shingrix"){
  number_courses<-2
  } else {print("Error")}
  
  
  
  ##########################
  #### Data
  
  
  ### Incidence HZ 
  ## AJs model-> incidence IC and non-IC
  # Incidence_HZ<-read.csv(file=paste(Data,"Incidence_HZ_old_AJ_model.csv", sep=""))
  # names(Incidence_HZ)<-c("age", "Incidence_HZ")
  # # false positives when determining incidence
  # p_Incidence_false_positives<-0.05
  # # proportion of HZ cases in IC
  # p_Incidence_IC<-0.089523809524
  # # Incidence removing IC and false positives
  # Incidence_HZ$Incidence_HZ_nIC<-Incidence_HZ$Incidence_HZ*(1-p_Incidence_false_positives)*(1-p_Incidence_IC)
  ## Data Nick
  #Incidence_HZ<-read.csv(file=paste(Data,"Incidence_RCGP_2017_Nick.csv", sep=""))
  ## Data Anu-> Incidence in the non-IC
  # Incidence_HZ<-read.csv(file=paste(Data,"clean_df_o90_grouped.csv", sep=""), row.names = 1)
  # Incidence_HZ_coef<-read.csv(file=paste(Data, "sim_coef_age2degreepoly_1000.csv", sep=""), row.names=1)
  ## Data Jemma CPRD supplemented by HES
  if(IC_status=="non IC"){
  Incidence_HZ_coef<-read.csv(file=paste(Data, "sim_coef_age2degreepoly_nb_nIC_1000.csv", sep=""), row.names=1)
  } else if (IC_status=="IC"){
  Incidence_HZ_coef<-read.csv(file=paste(Data, "sim_coef_age2degreepoly_pois_IC_1000.csv", sep=""), row.names=1)
  } else {print("Error")}
  # Make a list to store all data
  df<-data.frame(age=rep(50:90,n.run),
             Incidence_HZ_nIC=rep(1:n.run, each=length(50:90)))
  l<-split(df, df$Incidence_HZ_nIC)
  # function to get incidence from coefficients
  get_incidence<-function(age){as.numeric(as.character(exp(sampled_coef[1])*(exp(sampled_coef[2])^age)*(exp(sampled_coef[3])^(age^2))))}
  # Populate list with data
  if(IC_status=="non IC"){
  for(i in 1:n.run){
  sampled_coef<-Incidence_HZ_coef[sample(1:1000,1),] #sample randomly from the 1000 simulated coefficient rows
  # obtain incidence from sampled coefficients for each age and store in list l
  l[[i]]<-data.frame(age=50:100,
                     Incidence_HZ=sapply(50:100,FUN=get_incidence))
  }
  # Add incidence below 65 NA
  add_extra_years_top<-function(x){
  extra_rows<-data.frame(age=0:49,
                         Incidence_HZ=rep(NA,length(0:49)))
  rbind(extra_rows,x)
  }
  l<-lapply(l, add_extra_years_top)
  # Calculate probability of HZ from incidence
  calc_prob_inc_HZ<-function(x){
  x$p_HZ<-1-exp(-x$Incidence_HZ) #conversion instantaneous rate to probability
  x
  }
  l<-lapply(l, calc_prob_inc_HZ)
  } else if (IC_status=="IC")
  {
  for(i in 1:n.run){
  sampled_coef<-Incidence_HZ_coef[sample(1:1000,1),] #sample randomly from the 1000 simulated coefficient rows
  # obtain incidence from sampled coefficients for each age and store in list l
  l[[i]]<-data.frame(age=50:95,
                     Incidence_HZ=sapply(50:95,FUN=get_incidence))
  }
  # Add incidence below 65 NA
  add_extra_years_top<-function(x){
  extra_rows<-data.frame(age=0:49,
                         Incidence_HZ=rep(NA,length(0:49)))
  rbind(extra_rows,x)
  }
  l<-lapply(l, add_extra_years_top)
  # Extrapolate incidence to 100
  add_extra_years<-function(x){
  extra_rows<-data.frame(age=96:100,
                         Incidence_HZ=rep(x[x$age==max(x$age),2],length(96:100)))
  rbind(x,extra_rows)
  }
  l<-lapply(l, add_extra_years)
  # Calculate probability of HZ from incidence
  calc_prob_inc_HZ<-function(x){
  x$p_HZ<-1-exp(-x$Incidence_HZ) #conversion instantaneous rate to probability
  x
  }
  l<-lapply(l, calc_prob_inc_HZ)
  } else {print("Error")}
  
  
  ### Incidence HZ GP only
  ## Data Jemma CPRD GP only
  if(IC_status=="non IC"){
  Incidence_HZ_coef_GP_only<-read.csv(file=paste(Data, "sim_coef_age2degreepoly_nb_nIC_onlyGP_1000.csv", sep=""), row.names=1)
  } else if (IC_status=="IC"){
  Incidence_HZ_coef_GP_only<-read.csv(file=paste(Data, "sim_coef_age2degreepoly_pois_IC_onlyGP_1000.csv", sep=""), row.names=1)
  } else {print("Error")}
  # Populate list with data
  if(IC_status=="non IC"){
  for(i in 1:n.run){
  sampled_coef<-Incidence_HZ_coef_GP_only[sample(1:1000,1),] #sample randomly from the 1000 simulated coefficient rows
  # obtain incidence from sampled coefficients for each age and store in list l
  df<-data.frame(age=50:100,
                     Incidence_HZ_GP_only=sapply(50:100,FUN=get_incidence))
  extra_rows<-data.frame(age=0:49,
                         Incidence_HZ_GP_only=rep(NA,length(0:49)))
  df2<-rbind(extra_rows,df)
  l[[i]]<-merge(l[[i]], df2, by="age", all.x=T)
  }
  # Calculate probability of HZ from incidence
  calc_prob_inc_HZ<-function(x){
  x$p_HZ_GP_only<-1-exp(-x$Incidence_HZ_GP_only) #conversion instantaneous rate to probability
  x
  }
  l<-lapply(l, calc_prob_inc_HZ)
  } else if (IC_status=="IC")
  {
  for(i in 1:n.run){
  sampled_coef<-Incidence_HZ_coef_GP_only[sample(1:1000,1),] #sample randomly from the 1000 simulated coefficient rows
  # obtain incidence from sampled coefficients for each age and store in list l
  df<-data.frame(age=50:95,
                 Incidence_HZ_GP_only=sapply(50:95,FUN=get_incidence))
  extra_rows_top<-data.frame(age=0:49,
                         Incidence_HZ_GP_only=rep(NA,length(0:49)))
  extra_rows_bottom<-data.frame(age=96:100,
                         Incidence_HZ_GP_only=rep(df[df$age==max(df$age),2],length(96:100)))
  df2<-rbind(extra_rows_top, df, extra_rows_bottom)
  l[[i]]<-merge(l[[i]], df2, by="age", all.x=T)
  }
  # Calculate probability of HZ from incidence
  calc_prob_inc_HZ<-function(x){
  x$p_HZ_GP_only<-1-exp(-x$Incidence_HZ_GP_only) #conversion instantaneous rate to probability
  x
  }
  l<-lapply(l, calc_prob_inc_HZ)
  } else {print("Error")}
  
  
  ### England population
  ## AJ's old model
  # Pop_2015<-read.csv(file=paste(Data,"Background population AJ model.csv", sep=""))
  # names(Pop_2015)<-c("Age", "Pop")
  ## ONS mid-year estimate population England 2015
  Pop_2015<-read.csv(file=paste(Data,"midyearestimates2015 ONS population England.csv", sep=""))
  Pop_2015$Pop <- gsub(",","",Pop_2015$Pop)
  Pop_2015$Pop<-as.numeric(as.character(Pop_2015$Pop))
  Pop_2015<-Pop_2015[2:nrow(Pop_2015),] # remove age All
  Pop_2015$Age<-as.numeric(as.character(Pop_2015$Age))
  Pop_IC<-read.csv(file=paste(Data,"01-01-2019 zos denominator - IC - age vs year 1 year brackets 100y.csv", sep=""))
  Pop_IC<-Pop_IC[-(nrow(Pop_IC)),c(1,ncol(Pop_IC))]
  Pop_IC$X2012.13<-as.numeric(as.character(Pop_IC$X2012.13))
  names(Pop_IC)<-c("Age", "Pop_IC")
  Pop_IC<-Pop_IC%>%filter(Age%in%50:89)
  Pop_IC$Age<-as.numeric(as.character(Pop_IC$Age))
  Pop_IC<-rbind(Pop_IC,c(Age=90,Pop_IC=Pop_IC[Pop_IC$Age==max(Pop_IC$Age),2]))
  Pop_2015<-merge(Pop_IC, Pop_2015, by="Age")
  Pop_2015$p_IC<-Pop_2015$Pop_IC/Pop_2015$Pop #0.9% at age 50 to 2.6% at age 90
  
  
  ### Background mortality 
  # Background_mortality<-read.csv(file=paste(Data,"Background_mortality_old_AJ_model.csv", sep=""))
  # names(Background_mortality)<-c("age", "Background_mortality_rate")
  # ONS mortality projections for 2014-2016 in England (using 2014 data) by sex (PROBABILITY)
  Background_mortality<-read.csv(file=paste(Data,"Background_mortality_rates_2014_2016_projections_ONS.csv", sep=""))
  # ONS mid-year estimate population England 2015 by sex
  Pop_male_female<-read.csv(file=paste(Data,"Pop_england_estimates_mid_2015_male_females.csv", sep=""))
  # merge both
  Background_mortality<-merge(Background_mortality, Pop_male_female, by="age")
  # calculate the proportion of males and females in each age
  Background_mortality$ENGLAND.males <- gsub(",","",Background_mortality$ENGLAND.males)
  Background_mortality$ENGLAND.males<-as.numeric(as.character(Background_mortality$ENGLAND.males))
  Background_mortality$ENGLAND.females <- gsub(",","",Background_mortality$ENGLAND.females)
  Background_mortality$ENGLAND.females<-as.numeric(as.character(Background_mortality$ENGLAND.females))
  Background_mortality$total<-Background_mortality$ENGLAND.males+Background_mortality$ENGLAND.females
  Background_mortality$p_females<-Background_mortality$ENGLAND.females/Background_mortality$total
  Background_mortality$p_males<-Background_mortality$ENGLAND.males/Background_mortality$total
  # calculate background mortality for both sexes together
  Background_mortality$Background_mortality_rate<-((Background_mortality$Male.mortality.rate*Background_mortality$p_males)+(Background_mortality$Female.mortality.rate*Background_mortality$p_females))
  # remove unnecessary columns
  Background_mortality<-Background_mortality[,c("age", "Background_mortality_rate")]
  # Mortality at 100 (100+) set to 1
  Background_mortality[Background_mortality$age==100,]$Background_mortality_rate<-1
  # add to list
  l<-lapply(l, merge, y=Background_mortality, by="age")
  
  
  ### QALY loss HZ
  ## Area between model and 1, up to when model reaches one (LE) when rescaling with scale 0.25-0.75 (as opposed to 0.3-0.7)
  # QL_HZ<-read.csv(file=paste(Data,"QL with LE Simpsons asymptote per year.csv", sep=""), row.names=1)
  ## Area between model and 1 until t=200, then area between mean EQ5D for t=200 (all ages, as not age-dependent) and 1 up to LE
  # QL_HZ<-read.csv(file=paste(Data,"QL with LE simpsons asymptote per year using t200 mean EQ5D.csv", sep=""), row.names=1)
  ## Area between model and 1, up to when model reaches one (LE) when rescaling with scale 0.3-0.7 (as opposed to 0.25-0.75) NO BOOTSTRAP
  # QL_HZ<-read.csv(file=paste(Data,"22-11-2017 QL with LE Simpsons asymptote per year.csv", sep=""), row.names=1)
  ## Area between model and 1, up to when model reaches one (LE) when rescaling with scale 0.3-0.7 (as opposed to 0.25-0.75) BOOTSTRAP
  QL_y1<-read.csv(file=paste(Data,"11-07-2018 QL with LE ph simps 1000 bootstrap runs y1.csv", sep=""), row.names=1)
  QL_y2<-read.csv(file=paste(Data,"11-07-2018 QL with LE ph simps 1000 bootstrap runs y2.csv", sep=""), row.names=1)
  ## AJ's QL
  # QL_HZ<-read.csv(file=paste(Data,"QL_AJ.csv", sep=""))
  # names(QL_HZ)<-c("age", "QL_HZ")
  ## add to list and extrapolate to age 100
  for(i in 1:n.run){
  df<-data.frame(age=50:90,
               QL_y1=as.numeric(as.character(QL_y1[i,])),
               QL_y2=as.numeric(as.character(QL_y2[i,])))
  extra_rows<-data.frame(age=91:100,
                       QL_y1=rep(df[df$age==max(df$age),2],length(91:100)),
                       QL_y2=rep(df[df$age==max(df$age),3],length(91:100)))
  df2<-rbind(df,extra_rows)
  l[[i]]<-merge(l[[i]], df2, by="age", all.x=T)
  }
  ## discount QALY loss for year 2 (no QL from year 3 onward)
  for(i in 1:n.run){
  l[[i]]$QL_y2_d<-l[[i]]$QL_y2/((1+discount_rate_effects)^(1))
  l[[i]]$QL_HZ_d<-l[[i]]$QL_y1+l[[i]]$QL_y2
  }
  
  ### Additional QALY loss PHN/BoI (Zostavax)
  # ## AJ's QL 6m
  # QL_HZ_6m<-read.csv(file=paste(Data,"QL_6m_pre_vac_AJ.csv", sep=""))
  # # Add NAs for years we dont't have
  # QL_HZ_6m_y<-data.frame(age=0:69,
  #                        QL_6m_pre_vac=NA)
  # QL_HZ_6m<-rbind(QL_HZ_6m_y,QL_HZ_6m)
  # QL_HZ_6m$QL_6m_post_vac<-QL_HZ_6m$QL_6m_pre_vac*0.646914809
  # # add to list
  # l<-lapply(l, merge, y=QL_HZ_6m, by="age")
  # QALY loss >3m after rash onset
  QL_y1_o3m<-read.csv(file=paste(Data, "13-09-2018 QL with LE ph simps 1000 bootstrap runs y1 over 3m.csv", sep=""), row.names = 1)
  # add QL >3m to list and extrapolate to age 100
  for(i in 1:n.run){
  df<-data.frame(age=50:90,
               QL_y1_o3m=as.numeric(as.character(QL_y1_o3m[i,])))
  extra_rows<-data.frame(age=91:100,
                       QL_y1_o3m=rep(df[df$age==max(df$age),2],length(91:100)))
  df2<-rbind(df,extra_rows)
  l[[i]]<-merge(l[[i]], df2, by="age", all.x=T)
  }
  # Add QL over 3 months to the discounted QALY loss for year 2 (no QL from year 3 onward)
  for(i in 1:n.run){
  l[[i]]$QL_o3m_pre_vac_d<-l[[i]]$QL_y1_o3m+l[[i]]$QL_y2_d
  }
  # Add VE_PHN_w_HZ
  VE_PHN_w_HZ<-read.csv(file=paste(Data, "03-05-2018 VE for PHN w HZ for each year post vac 100 000 draws.csv", sep=""), row.names=1)
  # add VE PHN w HZ to list and extrapolate to age 100
  for(i in 1:n.run){
  df<-data.frame(age=(vaccination_age):(vaccination_age+10),
               VE_PHN_w_HZ=as.numeric(as.character(VE_PHN_w_HZ[i,])))
  extra_rows<-data.frame(age=(vaccination_age+11):100,
                       VE_PHN_w_HZ=0)
  df2<-rbind(df,extra_rows)
  l[[i]]<-merge(l[[i]], df2, by="age", all.x=T)
  }
  
  
  ### VE HZ
  ## Zostavax nonIC
  if(vaccine=="zostavax" & IC_status=="non IC"){
  # January 2019 zostavax VE with Marc
  MCMC_chain<-read.csv(paste(Data,"01-01-2019 MCMC_chain_zostavax_50000.csv", sep=""), row.names = 1)
  names(MCMC_chain)<-c("delta", "sigma_intercept", "sigma_slope", "lambda_intercept", "lambda_slope", "iteration")
  MCMC_chain$iteration<-1:nrow(MCMC_chain)
  MCMC_chain<-MCMC_chain[sample(nrow(MCMC_chain),n.run),]
  # take
  lambda.age<-function(lambda_intercept,lambda_slope,age){
  lambda_intercept+lambda_slope*age
  }
  take<-list()
  for(i in 1:nrow(MCMC_chain)){
  take[[i]]<-data.frame(age=vaccination_age,
                        lambda=lambda.age(age = vaccination_age,lambda_intercept = MCMC_chain[i,4],lambda_slope = MCMC_chain[i,5]))
  }
  # VE
  years_projected<-100-vaccination_age
  for(i in 1:n.run){
  VE<-data.frame(age=vaccination_age:(vaccination_age+years_projected),
                 VE=take[[i]][,"lambda"]*exp(-MCMC_chain[i,1]*seq(from=365,to=365*(years_projected+1), by=365)))
  l[[i]]<-merge(l[[i]], VE, by="age", all.x=T)
  }
  ## Shingrix nonIC  
  } else if(vaccine=="shingrix" & IC_status=="non IC"){
  # July 2018 shingrix VE with Marc
  MCMC_chain<-read.csv(paste(Data,"31-07-2018 MCMC_chain_shingrix_50000.csv", sep=""), row.names = 1)
  names(MCMC_chain)<-c("delta", "sigma_intercept", "sigma_slope", "lambda_intercept", "lambda_slope", "iteration")
  MCMC_chain$iteration<-1:nrow(MCMC_chain)
  MCMC_chain<-MCMC_chain[sample(nrow(MCMC_chain),n.run),]
  # take
  lambda.age<-function(lambda_intercept,lambda_slope,age){
  lambda_intercept+lambda_slope*age
  }
  take<-list()
  for(i in 1:nrow(MCMC_chain)){
  take[[i]]<-data.frame(age=vaccination_age,
                        lambda=lambda.age(age = vaccination_age,lambda_intercept = MCMC_chain[i,4],lambda_slope = MCMC_chain[i,5]))
  }
  # VE
  years_projected<-100-vaccination_age
  for(i in 1:n.run){
  VE<-data.frame(age=vaccination_age:(vaccination_age+years_projected),
                 VE=take[[i]][,"lambda"]*exp(-MCMC_chain[i,1]*seq(from=365,to=365*(years_projected+1), by=365)))
  l[[i]]<-merge(l[[i]], VE, by="age", all.x=T)
  }
  ## Shingrix IC 
  } else if (vaccine=="shingrix" & IC_status=="IC"){
  # July 2018 shingrix VE with Marc
  MCMC_chain<-read.csv(paste(Data,"31-07-2018 MCMC_chain_shingrix_50000.csv", sep=""), row.names = 1)
  names(MCMC_chain)<-c("delta", "sigma_intercept", "sigma_slope", "lambda_intercept", "lambda_slope", "iteration")
  MCMC_chain$iteration<-1:nrow(MCMC_chain)
  MCMC_chain<-MCMC_chain[sample(nrow(MCMC_chain),n.run),]
  lambda.age<-function(lambda_intercept,lambda_slope,age){
  lambda_intercept+lambda_slope*age
  }
  take<-list()
  for(i in 1:nrow(MCMC_chain)){
  take[[i]]<-data.frame(age=vaccination_age,
                        lambda=lambda.age(age = vaccination_age,lambda_intercept = MCMC_chain[i,4],lambda_slope = MCMC_chain[i,5]))
  }
  years_projected<-100-vaccination_age
  for(i in 1:n.run){
  VE<-data.frame(age=vaccination_age:(vaccination_age+years_projected),
                 VE_nonIC=take[[i]][,"lambda"]*exp(-MCMC_chain[i,1]*seq(from=365,to=365*(years_projected+1), by=365)))
  l[[i]]<-merge(l[[i]], VE, by="age", all.x=T)
  }
  # find VE in non-IC that matches IC point
  take_age54<-vector()
  for(i in 1:nrow(MCMC_chain)){
  take_age54[i]<-lambda.age(age = 54,lambda_intercept = MCMC_chain[i,4],lambda_slope = MCMC_chain[i,5])
  }
  years_projected<-2.35
  VE_nonIC_match_trial_IC<-vector()
  for(i in 1:n.run){
  VE_nonIC_match_trial_IC[i]<-(take_age54[i])*exp(-MCMC_chain[i,1]*(round(365*years_projected)))
  }
  # VE in the IC
  VE_IC_df<-read.csv(paste(Data,"03-01-2019 VE Shingrix IC.csv", sep=""), row.names = 1)
  names(VE_IC_df)<-"VE_IC"
  VE_IC_df$VE_nonIC<-VE_nonIC_match_trial_IC
  VE_IC_df$ratio<-VE_IC_df$VE_IC/VE_IC_df$VE_nonIC
  for(i in 1:n.run){
  l[[i]]$VE<-l[[i]]$VE_nonIC*VE_IC_df$ratio[i]
  }
  } else {print("Error")}
  ## AJ zostavax
  # VE<-read.csv(file=paste(Data,"VE_old_AJ_model.csv", sep=""))
  ## Sept 2017 shingrix VE
  # optim.results<-read.csv(file=paste(Data,"optim.results.shingrix.csv", sep=""), row.names = 1)
  # lambda.age=function(age,mv,bv) {
  #   if(mv<0) bv else (age-50)*(-mv)+bv
  # }
  # lambda.opt.70<-lambda.age(age = 70,mv=optim.results[1,2],bv = optim.results[2,2])
  # delta.opt<-optim.results[5,2]
  # tp<-(70:100)-70
  # tstep=10 #time steps per year
  # VE_by_age_from_70<-lambda.opt.70*((exp(-delta.opt*tstep*tp)))
  # VE<-data.frame(age=70:100,
  #                VE=VE_by_age_from_70)
  # ## Sept 2017 zostavax VE
  # optim.results<-read.csv(file=paste(Data,"optim.results.zostavax.csv", sep=""), row.names = 1)
  # lambda.age=function(age,mv,bv) {
  #   if(mv<0) bv else (age-50)*(-mv)+bv
  # }
  # lambda.opt.70<-lambda.age(age = 70,mv=optim.results[1,2],bv = optim.results[2,2])
  # delta.opt<-optim.results[5,2]
  # tp<-(70:100)-70
  # tstep=10 #time steps per year
  # VE_by_age_from_70<-lambda.opt.70*((exp(-delta.opt*tstep*tp)))
  # VE<-data.frame(age=70:100,
  #                VE=VE_by_age_from_70)
  # # Add ages 0 to 70
  # VE_0_69<-data.frame(age=0:69,
  #                     VE=rep(NA,length(0:69)))
  # VE<-rbind(VE_0_69,VE)
  # # add to list
  # l<-lapply(l, merge, y=VE, by="age")
  
  
  ### HZ mortality
  ## AJ's old mortality
  # HZ_mortality<-read.csv(file=paste(Data,"CFR_HZ_old_AJ_model.csv", sep="")) # this is the mortality rate(?) divided by incidence*100,000
  # names(HZ_mortality)<-c("age", "HZ_mortality_rate")
  # # expand unknown ages
  # HZ_mortality_y<-data.frame(age=0:49,HZ_mortality_rate=NA)
  # HZ_mortality<-rbind(HZ_mortality_y,HZ_mortality)
  # # add to list
  # l<-lapply(l, merge, y=HZ_mortality, by="age")
  ## Peter Hobbelen's analysis 2004-05 to 2012-13 mortality (HES, primary diagnosis)
  # nonIC read data & function to get incidence of HZ mortality from coefficients
  if(IC_status=="non IC"){
  HZ_mortality<-read.csv(file=paste(Data,"11-06-2018 HZ HES mortality incidence primary cause sim_coef_age_1000.csv", sep=""), row.names = 1)
  get_incidence_HZ_mortality<-function(age){as.numeric(as.character(exp(sampled_coef[1])*(exp(sampled_coef[2])^age)))}
  # IC read data & function to get incidence of HZ mortality from coefficients
  } else if (IC_status=="IC"){
  HZ_mortality<-read.csv(file=paste(Data,"01-01-2019 HZ HES mortality incidence primary cause IC sim_coef_age_1000.csv", sep=""), row.names = 1)
  get_incidence_HZ_mortality<-function(age){as.numeric(as.character(exp(sampled_coef[1])*(exp(sampled_coef[2])^age)*(exp(sampled_coef[3])^(age^2))))}
  } else {print("Error")}
  # put data in list 
  for(i in 1:n.run){
  sampled_coef<-HZ_mortality[sample(1:1000,1),] #sample randomly from the 1000 simulated coefficient rows
  df<-data.frame(age=50:89,
               Incidence_HZ_mortality=as.numeric(as.character(sapply(50:89,FUN=get_incidence_HZ_mortality))))
  extra_rows<-data.frame(age=90:100,
                       Incidence_HZ_mortality=rep(df[df$age==max(df$age),2],length(90:100)))
  extra_rows2<-data.frame(age=0:49,
                        Incidence_HZ_mortality=NA)
  df2<-rbind(extra_rows2, df,extra_rows)
  l[[i]]<-merge(l[[i]], df2, by="age", all.x=T)
  }
  # proportion of HZ cases that die
  for(i in 1:n.run){
  l[[i]]$Proportion_deaths<-l[[i]]$Incidence_HZ_mortality/l[[i]]$Incidence_HZ
  }
  
  
  ### EQ5D population norms
  # #Szende A, Janssen B, Cabases J. Self-reported population health: an international perspective based on EQ-5D. Dordrecht: Springer; 2014. TTO value set England. p30
  popest<-data.frame(age=18:100,
                 pe=c(rep(0.929, length(18:24)),
                      rep(0.919, length(25:34)),
                      rep(0.893, length(35:44)),
                      rep(0.855, length(45:54)),
                      rep(0.810,  length(55:64)),
                      rep(0.773, length(65:74)),
                      rep(0.703, length(75:100))))
  # from AJs model
  # popest<-data.frame(age=0:100,
  #                     pe=c(rep(0.9, length(0:14)),
  #                          rep(0.88, length(15:19)),
  #                          rep(0.87, length(20:39)),
  #                          rep(0.85, length(40:49)),
  #                          rep(0.8,  length(50:69)),
  #                          rep(0.75, length(70:79)),
  #                          rep(0.725, length(80:100))))
  
  
  ### hospitalisation rate for HZ
  ## from AJs model (based on first diagnoses per age group), from AJ's model (THIS IS THE PROPORTION OF HZ CASES HOSPITALISED AT FIRST DIAGNOSIS)
  # Hospitalisation_rate_HZ<-read.csv(file=paste(Data,"hospitalisation_rates_old_AJ_model.csv", sep=""))
  # names(Hospitalisation_rate_HZ)<-c("age", "Hospitalisation_rate_HZ")
  ## Hospitalisation rate Peter Hobbelen analysis 2004-05 to 2012-13 
  if(IC_status=="non IC"){
  # nonIC read data & function to get incidence of HZ hospitalisation from coefficients
  Hospitalisation_rate_HZ_coef<-read.csv(file=paste(Data, "HZ hospitalisation incidence sim_coef_age3degreepoly_1000.csv", sep=""), row.names=1)
  get_incidence_hosp<-function(age){as.numeric(as.character(exp(sampled_coef[1])*(exp(sampled_coef[2])^age)*(exp(sampled_coef[3])^(age^2))*(exp(sampled_coef[4])^(age^3))))}
  } else if (IC_status=="IC"){
  # IC read data & function to get incidence of HZ hospitalisation from coefficients
  Hospitalisation_rate_HZ_coef<-read.csv(file=paste(Data, "HZ hospitalisation incidence IC sim_coef_age4degreepoly_1000.csv", sep=""), row.names=1)
  get_incidence_hosp<-function(age){
  as.numeric(as.character((exp(sampled_coef[1]+(sampled_coef[2]*age)+(sampled_coef[3]*(age^2))+(sampled_coef[4]*(age^3))+(sampled_coef[5]*(age^4))))))
  }
  } else {print("Error")}
  # put data in list
  for(i in 1:n.run){
  sampled_coef<-Hospitalisation_rate_HZ_coef[sample(1:1000,1),] #sample randomly from the 1000 simulated coefficient rows
  df<-data.frame(age=50:89,
               Hospitalisation_rate_HZ=as.numeric(as.character(sapply(50:89,FUN=get_incidence_hosp))))
  extra_rows<-data.frame(age=90:100,
                       Hospitalisation_rate_HZ=rep(df[df$age==max(df$age),2],length(90:100)))
  extra_rows2<-data.frame(age=0:49,
                        Hospitalisation_rate_HZ=NA)
  df2<-rbind(extra_rows2, df,extra_rows)
  l[[i]]<-merge(l[[i]], df2, by="age", all.x=T)
  }
  # proportion of HZ cases hospitalised
  for(i in 1:n.run){
  l[[i]]$Proportion_hospitalised<-l[[i]]$Hospitalisation_rate_HZ/l[[i]]$Incidence_HZ
  }
  
  
  ### costs of hospitalisation per HZ case
  ## from AJ's model (based on average length of stay per age group)
  # Hospitalisation_costs_HZ<-read.csv(file=paste(Data,"hospital_costs_HZ_old_AJ_model.csv", sep=""))
  # names(Hospitalisation_costs_HZ)<-c("age", "Cost_per_hospitalisation_HZ")
  ## from Peter Hobbelen's work 2004-05 to 2012-13 
  if(IC_status=="non IC"){
  # nonIC read data & function to get hospitalisation costs from coefficients
  Hospitalisation_costs_HZ_coef<-read.csv(file=paste(Data, "Cost HZ hospitalisation sim_coef_agelm_rr_100000.csv", sep=""), row.names=1)
  get_cost_hosp<-function(age){as.numeric(as.character(sampled_coef[1]+(sampled_coef[2]*age)))}
  } else if (IC_status=="IC"){
  # IC read data & function to get hospitalisation costs from coefficients
  Hospitalisation_costs_HZ_coef<-read.csv(file=paste(Data, "Cost HZ hospitalisation IC sim_coef_agelm_rr_100000.csv", sep=""), row.names=1)
  get_cost_hosp<-function(age){as.numeric(as.character(sampled_coef[1]+(sampled_coef[2]*age)))}
  } else {print("Error")}
  # put data in list
  for(i in 1:n.run){
  sampled_coef<-Hospitalisation_costs_HZ_coef[sample(1:100000,1),] #sample randomly from the 100000 simulated coefficient rows
  df<-data.frame(age=50:89,
               Hospitalisation_costs_pp_HZ=as.numeric(as.character(sapply(50:89,FUN=get_cost_hosp))))
  extra_rows<-data.frame(age=90:100,
                       Hospitalisation_costs_pp_HZ=rep(df[df$age==max(df$age),2],length(90:100)))
  extra_rows2<-data.frame(age=0:49,
                        Hospitalisation_costs_pp_HZ=NA)
  df2<-rbind(extra_rows2, df,extra_rows)
  l[[i]]<-merge(l[[i]], df2, by="age", all.x=T)
  }
  # inflating the hospital costs in the model
  index_2016_17<-302.3
  index_2012_13<-287.3
  cost_inf<-function(x){
  l[[x]]$Hospitalisation_costs_pp_HZ_inf<-l[[x]]$Hospitalisation_costs_pp_HZ*(index_2016_17/index_2012_13)
  return(l[[x]])
  }
  l<-lapply(1:n.run, cost_inf)
  
  
  ### costs GP (outpatient direct cost per HZ episode)- excludes hospitalisations. Gauthier et al. 2009 Epidemiology and cost of herpes zoster and post-herpetic neuralgia in the United Kingdom. As used by AJ in his last model.
  ## AJ costs GP
  # Cost_GP_per_HZ<-data.frame(age=50:100,
  #                            p_PHN_AJ=c(rep(0.09,length(60:64)),
  #                                       rep(0.11,length(65:69)),
  #                                       rep(0.15,length(70:74)),
  #                                       rep(0.20,length(75:79)),
  #                                       rep(0.27,length(80:84)),
  #                                       rep(0.52,length(85:90))),
  #                            Cost_GP_per_non_PHN_HZ=75.63,
  #                            Cost_GP_per_PHN=340.04)
  ## AJ costs
  # AJ_p_PHN<-read.csv(file=paste(Data,"AJ_p_PHN.csv", sep=""))
  # Cost_GP_per_HZ<-data.frame(age=60:99,
  #                            p_PHN=AJ_p_PHN$p_PHN,
  #                            Cost_GP_per_non_PHN_HZ=75.63,
  #                            Cost_GP_per_PHN=340.04)
  # write.csv(Cost_GP_per_HZ, file=paste(Output,"p_PHN_AJ_Nick.csv", sep=""))
  ## Data Jemma CPRD p_PHN
  if(IC_status=="non IC"){
  # nonIC read data & function to get p_PHN from coefficients
  prop_PHN_coef<-read.csv(file=paste(Data, "sim_coef_age2degreepoly_binom_PHN_nIC_1000.csv", sep=""), row.names=1)
  } else if (IC_status=="IC"){
  # IC read data & function to get p_PHN from coefficients
  prop_PHN_coef<-read.csv(file=paste(Data, "sim_coef_age2degreepoly_binom_PHN_IC_1000.csv", sep=""), row.names=1)
  } else {print("Error")}
  # function to get proportion of PHN from coefficients
  get_prop_PHN<-function(age){
  as.numeric(as.character((exp(sampled_coef[1])*(exp(sampled_coef[2])^age)*(exp(sampled_coef[3])^(age^2)))/(1+(exp(sampled_coef[1])*(exp(sampled_coef[2])^age)*(exp(sampled_coef[3])^(age^2))))))
  }
  # add proportion of patients with HZ that get PHN to list
  if(IC_status=="non IC"){
  for(i in 1:n.run){
  sampled_coef<-prop_PHN_coef[sample(1:1000,1),] #sample randomly from the 1000 simulated coefficient rows
  df<-data.frame(age=50:95,
                 p_PHN=as.numeric(as.character(sapply(50:95,FUN=get_prop_PHN))))
  extra_rows<-data.frame(age=0:49,
                         p_PHN=NA)
  extra_rows2<-data.frame(age=96:100,
                          p_PHN=rep(df[df$age==max(df$age),2],length(96:100)))
  df2<-rbind(extra_rows, df, extra_rows2)
  l[[i]]<-merge(l[[i]], df2, by="age", all.x=T)
  }
  } else if (IC_status=="IC"){
  for(i in 1:n.run){
  sampled_coef<-prop_PHN_coef[sample(1:1000,1),] #sample randomly from the 1000 simulated coefficient rows
  df<-data.frame(age=50:90,
                 p_PHN=as.numeric(as.character(sapply(50:90,FUN=get_prop_PHN))))
  extra_rows<-data.frame(age=0:49,
                         p_PHN=NA)
  extra_rows2<-data.frame(age=91:100,
                          p_PHN=rep(df[df$age==max(df$age),2],length(91:100)))
  df2<-rbind(extra_rows, df, extra_rows2)
  l[[i]]<-merge(l[[i]], df2, by="age", all.x=T)
  }
  } else {print("Error")}
  ## outpatient costs Gauthier et al 2009 (in 2006 GBP): 
  # 75.63 per non PHN HZ episode (95% CI 74.68-76.58)
  mean_non_PHN_HZ<-(76.58+74.68)/2 #75.63
  SE_m<-(75.63-74.68)/1.96 # same as (76.58-75.63)/1.96
  SD<-SE_m*(sqrt(21587)) # this is incorrect
  # simulate 100,000 costs
  GP_cost_pp_non_PHN_HZ<-rnorm(mean=75.63,sd=SE_m, n=100000)
  #check
  # quantile(GP_cost_pp_non_PHN_HZ,probs = c(0,5,50,95,100)/100)
  # 340.04 for PHN episode (95% CI 319.23-360.85) defined as 3m
  mean_PHN<-(319.23+360.85)/2 #340.04
  SE_m<-(340.04-319.23)/1.96
  GP_cost_pp_PHN<-rnorm(mean=340.04,sd=SE_m, n=100000)
  #check
  # quantile(GP_cost_pp_PHN,probs = c(0,5,50,95,100)/100)
  # put data in list
  for(i in 1:n.run){
  sampled_GP_cost_pp_non_PHN_HZ<-GP_cost_pp_non_PHN_HZ[sample(1:100000,1)] #sample randomly from all the values
  sampled_GP_cost_pp_PHN<-GP_cost_pp_PHN[sample(1:100000,1)]
  df<-data.frame(age=0:100,
               GP_cost_pp_non_PHN_HZ=as.numeric(as.character(sampled_GP_cost_pp_non_PHN_HZ)),
               GP_cost_pp_PHN=as.numeric(as.character(sampled_GP_cost_pp_PHN)))
  l[[i]]<-merge(l[[i]], df, by="age", all.x=T)
  }
  ## inflate costs
  index_2016_17<-302.3
  index_2005_06<-240.9
  # inflation from 2012/13-2016/17
  # ((index_2016_17/index_2005_06)-1)*100
  cost_inf2<-function(x){
  l[[x]]$GP_cost_pp_non_PHN_HZ_inf<-l[[x]]$GP_cost_pp_non_PHN_HZ*(index_2016_17/index_2005_06)
  l[[x]]$GP_cost_pp_PHN_inf<-l[[x]]$GP_cost_pp_PHN*(index_2016_17/index_2005_06)
  return(l[[x]])
  }
  l<-lapply(1:n.run, cost_inf2)
  
  
  ##########################
  #### cohort size
  # population in England at vaccination age * vaccine_coverage
  if(IC_status=="non IC"){
    cohort_size<-(Pop_2015[Pop_2015$Age%in%vaccination_age,]$Pop) * vaccine_coverage
  } else if (IC_status=="IC"){
    cohort_size<-(Pop_2015[Pop_2015$Age%in%vaccination_age,]$Pop_IC) * vaccine_coverage
  } else {print("Error")}

  
  
  ##########################
  #### Survival_probability_HZ
  survival_calc2<-function(x){
  l[[x]]$p_survival<-1
  l[[x]][l[[x]]$age<vaccination_age,"p_survival"]<-1
  l[[x]][l[[x]]$age==vaccination_age,"p_survival"]<-cohort_size
  for (i in (which(l[[x]]$age==vaccination_age)+1):nrow(l[[x]])){
  l[[x]][i,"p_survival"]<-l[[x]][(i-1),"p_survival"]*(1-l[[x]][(i-1),"Background_mortality_rate"])
  }
  l[[x]]$p_survival<-l[[x]]$p_survival/cohort_size
  return(l[[x]])
  }
  l<-lapply(1:n.run, survival_calc2)
  
  
  ##########################
  #### Model only from vaccination age
  only_vac<-function(x){
  l[[x]]<-l[[x]]%>%filter(age>=vaccination_age)
  return(l[[x]])
  }
  l<-lapply(1:n.run, only_vac)
  
  
  #####################################################################################
  # MODEL
  #####################################################################################
  
  ##########################
  #### Probabilities 
  
  ### function for calculating probabilities
  probs_calc<-function(x){
  
  ## Probability of HZ in the vaccination cohort pre-vaccination
  # Probability * survival
  l[[x]]$p_HZ_alive<-l[[x]]$p_HZ*l[[x]]$p_survival #*cohort_size?
  
  ## Probability of HZ in the vaccination cohort pre-vaccination only GP
  # Probability * survival
  l[[x]]$p_HZ_GP_only_alive<-l[[x]]$p_HZ_GP_only*l[[x]]$p_survival #*cohort_size?
  
  ## Probability of HZ in the vaccination cohort post-vaccination
  # Probability pre-vaccination * (1- VE)
  l[[x]]$p_HZ_post_vac<-l[[x]]$p_HZ_alive*(1-l[[x]]$VE)
  
  
  ## Probability of deaths due to HZ in the vaccination cohort pre-vaccination
  # Probability of HZ in the vaccination cohort pre-vaccination * proportion of deaths due to HZ
  l[[x]]$p_deaths_HZ<-l[[x]]$p_HZ_alive*l[[x]]$Proportion_deaths
  
  
  ## Probability of deaths due to HZ in the vaccination cohort post-vaccination
  # Probability pre-vaccination * (1- VE)
  l[[x]]$p_deaths_HZ_post_vac<-l[[x]]$p_deaths_HZ*(1-l[[x]]$VE)
  
  
  ## Probability of hospitalisation due to HZ in the vaccination cohort pre-vaccination
  # Probability of HZ in the vaccination cohort pre-vaccination * proportion hospitalised due to HZ
  l[[x]]$p_hospitalisation<-l[[x]]$p_HZ_alive*l[[x]]$Proportion_hospitalised
  
  
  ## Probability of hospitalisation due to HZ in the vaccination cohort post-vaccination
  l[[x]]$p_hospitalisation_post_vac<-l[[x]]$p_hospitalisation*(1-l[[x]]$VE)
  
  
  return(l[[x]])
  }
  
  l<-lapply(1:n.run, probs_calc)
  
  
  ##########################
  #### QALY loss HZ
  
  QALY_calc<-function(x){
  
  ### QALY loss HZ in the vaccination cohort pre-vaccination discounted
  
  # QALY loss HZ in the cohort
  l[[x]]$QL_HZ<-l[[x]]$p_HZ_alive*l[[x]]$QL_HZ_d
  
  # discount QALYs
  l[[x]]$QL_HZ_d<-l[[x]]$QL_HZ/((1+discount_rate_effects)^(l[[x]]$age-vaccination_age))
  
  
  ### QALY loss HZ in the vaccination cohort post-vaccination discounted
  
  # additional QL from PHN that is avoided by vaccine
  l[[x]]$QL_o3m_post_vac_d<-l[[x]]$QL_o3m_pre_vac_d*(1-l[[x]]$VE_PHN_w_HZ)
  l[[x]]$QL_o3m_pre_vac_d<-l[[x]]$p_HZ_alive*l[[x]]$QL_o3m_pre_vac_d
  l[[x]]$QL_o3m_post_vac_with_VE_d<-l[[x]]$p_HZ_post_vac*l[[x]]$QL_o3m_post_vac_d
  l[[x]]$QL_o3m_post_vac_no_VE_d<-l[[x]]$p_HZ_post_vac*l[[x]]$QL_o3m_pre_vac_d
  l[[x]]$additional_QL_o3m_d<-l[[x]]$QL_o3m_post_vac_no_VE_d-l[[x]]$QL_o3m_post_vac_with_VE_d
  
  # ensure additional QL only applies to zostavax
  if(vaccine=="shingrix"){
  l[[x]]$additional_QL_o3m_d<-0
  } else if (vaccine=="zostavax"){} else {print("Error")}
  
  # QALY loss HZ in the cohort. Includes additional QL for those vaccinated that developed HZ. In clinical trials, those that developed HZ had a higher QoL if vaccinated than the placebo -> lower QALY loss (for zostavax only)
  l[[x]]$QL_HZ_post_vac<-(l[[x]]$p_HZ_post_vac*l[[x]]$QL_HZ_d)-l[[x]]$additional_QL_o3m_d
  
  # discount QALYs
  l[[x]]$QL_HZ_post_vac_d<-l[[x]]$QL_HZ_post_vac/((1+discount_rate_effects)^(l[[x]]$age-vaccination_age))
  
  return(l[[x]])
  }
  
  l<-lapply(1:n.run, QALY_calc)
  
  
  ##########################
  #### QALY loss death 
  
  ### my calc for QL death
  
  ## discounted survival
  LE<-Background_mortality[,c("age","Background_mortality_rate")]
  # add a fake age row to test life table
  LE[102,]<-c(101,0)
  # survival rate
  LE$Survival<-1
  for (i in 2:nrow(LE)){
  LE[i,3]<-LE[(i-1),3]*(1-LE[(i-1),2])
  }
  # discounted survival at base year ages 0:100
  for (j in 1:101){
  LE[,j+3]<-1
  names(LE)[j+3]<-j-1#paste("Survival_d_base_y", j, sep="")
  for (i in ((j+1):nrow(LE))){
  LE[i,j+3]<-LE[(i-1),j+3]*(1-LE[(i-1),2])/(1+discount_rate_effects)
  }
  }
  # remove fake age row 
  LE<-LE[-102,]
  # fill the survival table with with NAs for ages in which the cohort age is lower than the base year age
  for(i in 4:ncol(LE)){
  LE[as.numeric(as.character(names(LE)))[i]>LE$age,i]<-NA
  }
  #rename variables
  names(LE)[4:104]<-paste("Survival_d_base_y", names(LE)[4:104], sep="")
  
  ## QoL adjusted discounted survival
  # add fake row to popest
  QoL_adjusted_LE<-merge(LE, popest, by="age") #now only for 18+
  # remove colums <18
  QoL_adjusted_LE<-QoL_adjusted_LE[,c(1:3,22:ncol(QoL_adjusted_LE))]
  for(i in 1:nrow(QoL_adjusted_LE)){
  QoL_adjusted_LE[i,4:ncol(QoL_adjusted_LE)]<-QoL_adjusted_LE[i,4:ncol(QoL_adjusted_LE)]*QoL_adjusted_LE[i,"pe"]
  }
  
  ## QALY loss death
  sum_omit<-function(x){sum(na.omit(x))}
  a<-apply(QoL_adjusted_LE,2,sum_omit) #col sums
  b<-a[4:(length(a)-1)] # all except pe column
  QL_death<-data.frame(age=18:100,
                   QL_death=b)
  
  ### Add QL death to list
  # Do we want the one calculated above or QL death or AJs?
  # QL_death<-read.csv(file=paste(Data,"QL death AJ.csv", sep=""))
  l<-lapply(l, merge, y=QL_death, by="age")
  
  ### function to calculate the QL death in the vaccination cohort pre and post vaccination discounted
  QL_death_calc<-function(x){
  
  ## QALY loss death in the vaccination cohort pre-vaccination discounted
  
  # QALY loss death in the vaccination cohort
  l[[x]]$QL_death<-l[[x]]$p_deaths_HZ*l[[x]]$QL_death
  
  # Discounted
  l[[x]]$QL_death_d<-l[[x]]$QL_death/((1+discount_rate_effects)^(l[[x]]$age-vaccination_age))
  
  
  ## QALY loss death in the vaccination cohort post-vaccination discounted
  
  # QALY loss HZ in the cohort
  l[[x]]$QL_death_post_vac<-l[[x]]$p_deaths_HZ_post_vac*l[[x]]$QL_death
  
  # discount QALYs
  l[[x]]$QL_death_post_vac_d<-l[[x]]$QL_death_post_vac/((1+discount_rate_effects)^(l[[x]]$age-vaccination_age))
  
  return(l[[x]])
  }
  
  l<-lapply(1:n.run, QL_death_calc)
  
  
  ##########################
  #### Results data frame to store results
  
  ## results df to store results
  results_df<-data.frame(total_QL_no_vac_d=rep(NA,times=n.run),
                         total_QL_post_vac_d=rep(NA,times=n.run),
                         total_QG_HZ_d=rep(NA,times=n.run),
                         total_QG_death_HZ_d=rep(NA,times=n.run),
                         total_C_no_vac_d=rep(NA,times=n.run),
                         total_C_post_vac_d=rep(NA,times=n.run),
                         total_CS_hospital_d=rep(NA,times=n.run),
                         total_CS_GP_d=rep(NA,times=n.run),
                         cohort_size=rep(cohort_size,times=n.run))
  
  
  ##########################
  #### Total QALYs gained (discounted)
  
  for (i in 1:n.run){
  
  # QALYs lost if no vaccination
  results_df[i,"total_QL_no_vac_d"]<-(sum(l[[i]]$QL_HZ_d+l[[i]]$QL_death_d))*cohort_size
  
  # QALYs lost if vaccination
  results_df[i,"total_QL_post_vac_d"]<-(sum(l[[i]]$QL_HZ_post_vac_d+l[[i]]$QL_death_post_vac_d))*cohort_size  
    
  # QALYs gained for HZ for each age summed
  results_df[i,"total_QG_HZ_d"]<-sum(l[[i]]$QL_HZ_d-l[[i]]$QL_HZ_post_vac_d)
  
  # QALYs gained for death of HZ for each age summed (or zero)
  results_df[i,"total_QG_death_HZ_d"]<-sum(l[[i]]$QL_death_d-l[[i]]$QL_death_post_vac_d)
  
  }
  
  # total QALYs gained
  results_df$total_QALYs_gained_d<-(results_df$total_QG_HZ_d+results_df$total_QG_death_HZ_d)*cohort_size
  
  
  ##########################
  #### Costs
  
  ### function to calculate costs
  costs_calc<-function(x){
  
  ### Costs of hospitalisation due to HZ in the vaccination cohort pre-vaccination discounted
  
  # Costs for hospitalisations due to HZ in the cohort
  l[[x]]$Cost_hospitalisation<-l[[x]]$p_hospitalisation*l[[x]]$Hospitalisation_costs_pp_HZ_inf
  
  # Discounted
  l[[x]]$Cost_hospitalisation_d<-l[[x]]$Cost_hospitalisation/((1+discount_rate_costs)^(l[[x]]$age-vaccination_age))
  
  
  ### Costs of hospitalisation due to HZ in the vaccination cohort post-vaccination discounted
  
  # Costs for hospitalisations due to HZ in the cohort
  l[[x]]$Cost_hospitalisation_post_vac<-l[[x]]$p_hospitalisation_post_vac*l[[x]]$Hospitalisation_costs_pp_HZ_inf
  
  # Discounted
  l[[x]]$Cost_hospitalisation_post_vac_d<-l[[x]]$Cost_hospitalisation_post_vac/((1+discount_rate_costs)^(l[[x]]$age-vaccination_age))
  
  
  ### Costs GP HZ in the vaccination cohort pre-vaccination discounted
  
  # p only PHN and only HZ (no PHN)
  l[[x]]$p_PHN_GP<-l[[x]]$p_HZ_GP_only_alive*l[[x]]$p_PHN #different to p_PHN_AJ
  l[[x]]$p_non_PHN_HZ_GP<-l[[x]]$p_HZ_GP_only_alive*(1-l[[x]]$p_PHN)
  
  # Costs for GP due to HZ in the cohort
  l[[x]]$Cost_GP<-(l[[x]]$p_PHN_GP*l[[x]]$GP_cost_pp_PHN_inf)+(l[[x]]$p_non_PHN_HZ_GP*l[[x]]$GP_cost_pp_non_PHN_HZ_inf)
  
  # Discounted
  l[[x]]$Cost_GP_d<-l[[x]]$Cost_GP/((1+discount_rate_costs)^(l[[x]]$age-vaccination_age))
  
  
  ### Costs GP HZ in the vaccination cohort post-vaccination discounted
  
  # ensure VE_PHN_w_HZ only applies to zostavax
  if(vaccine=="shingrix"){
  l[[x]]$VE_PHN_w_HZ<-0
  } else if (vaccine=="zostavax"){} else {print("Error")}
  
  # p only PHN and only HZ (no PHN)
  l[[x]]$p_PHN_post_vac<-l[[x]]$p_HZ_post_vac*(1-l[[x]]$VE_PHN_w_HZ)*l[[x]]$p_PHN #different to p_PHN_AJ
  l[[x]]$p_non_PHN_HZ_post_vac<-l[[x]]$p_HZ_post_vac*(1-((1-l[[x]]$VE_PHN_w_HZ)*l[[x]]$p_PHN))
  
  # Costs for GP due to HZ in the cohort
  l[[x]]$Cost_GP_post_vac<-(l[[x]]$p_PHN_post_vac*l[[x]]$GP_cost_pp_PHN_inf)+(l[[x]]$p_non_PHN_HZ_post_vac*l[[x]]$GP_cost_pp_non_PHN_HZ_inf)
  
  # Discounted
  l[[x]]$Cost_GP_post_vac_d<-l[[x]]$Cost_GP_post_vac/((1+discount_rate_costs)^(l[[x]]$age-vaccination_age))
  
  return(l[[x]])
  }
  
  l<-lapply(1:n.run, costs_calc)
  
  
  ##########################
  #### Total direct costs saved
  
  for (i in 1:n.run){
    
  # Costs no vaccination
  results_df[i,"total_C_no_vac_d"]<-(sum(l[[i]]$Cost_hospitalisation_d+l[[i]]$Cost_GP_d))*cohort_size
  
  # Costs if vaccination
  results_df[i,"total_C_post_vac_d"]<-(sum(l[[i]]$Cost_hospitalisation_post_vac_d+l[[i]]$Cost_GP_post_vac_d))*cohort_size
    
  # Costs saved for hospitalisation for each age summed
  results_df[i,"total_CS_hospital_d"]<-sum(l[[i]]$Cost_hospitalisation_d-l[[i]]$Cost_hospitalisation_post_vac_d)
  
  # Costs saved for GP for each age summed
  results_df[i,"total_CS_GP_d"]<-sum(l[[i]]$Cost_GP_d-l[[i]]$Cost_GP_post_vac_d)
  }
  
  #total direct costs saved
  results_df$total_CS_d<-results_df$total_CS_hospital_d+results_df$total_CS_GP_d
  
  
  ##########################
  #### Saved direct costs (discounted)
  
  results_df$total_saved_costs_d<-cohort_size*results_df$total_CS_d
  
  
  
  ##########################
  #### Cost intervention
  
  results_df$total_costs_intervention<-cohort_size*((vaccine_cost_per_dose+admin_cost_per_dose)*number_courses)
  
  
  
  ##########################
  #### Total costs of vaccination
  
  results_df$total_net_cost<-results_df$total_costs_intervention-results_df$total_saved_costs_d
  
  
  
  ##########################
  #### ICER, cost per QALY
  
  results_df$ICER<-results_df$total_net_cost/results_df$total_QALYs_gained_d #Shingrix 2363.7, zostavax 10650.6
  
  
  
  ##########################
  #### save output
  
  write.csv(results_df, file=paste(Output,"14-01-2019_CEA_", vaccine, "_", IC_status, "_", "age", vaccination_age, ".csv", sep=""))
  
  
  finished_run<-paste(Output,"14-01-2019_CEA_", vaccine, "_", IC_status, "_", "age", vaccination_age, ".csv", sep="")
  print(finished_run)
  
  
  ## remove variables from enviroment
  rm(l)
  rm(results_df)
}

## end timer
end_time <- Sys.time()
end_time - start_time