# changed calculation of NMB
# changed plots JCVI according to Mark's advice
# change cohort size IC

##########################
#### Prep

##########################
#### Prep

## set wd directory
wdir<-"/Users/eoccaros/Documents/GitHub/HZ_CEA/Code"
# wdir<-"H:\\HZ work\\Projects\\Model"
# wdir<-"/Users/eoccaros/Desktop/HZ work/Projects/Model/"
setwd(wdir)

### paths
Data<-"/Users/eoccaros/Documents/HZ work/Projects/Model/Data/"
Plots<-"/Users/eoccaros/Documents/HZ work/Projects/Model/R/Plots/"
Output<-"/Users/eoccaros/Documents/HZ work/Projects/Model/R/Output/"
# Plots<-paste(wdir,"\\R\\Plots\\", sep="")
# Data<-paste(wdir,"\\Data\\", sep="")
# Output<-paste(wdir, "\\R\\Output\\", sep="")


### libraries
library(dplyr); library(ggplot2);library(betareg); #library(lhs); library(triangle); library(meta)


### read in all files in list and add costs of intervention to each df
all_dfs_shingrix_IC<-list()
for(i in 1:41)
{
  all_dfs_shingrix_IC[[i]]<- read.csv(paste(Output,"02-07-2019_CEA_shingrix_IC_age", i+49, ".csv", sep=""), row.names = 1)
}

all_dfs_shingrix_nonIC<-list()
for(i in 1:41)
{
  all_dfs_shingrix_nonIC[[i]]<- read.csv(paste(Output,"02-07-2019_CEA_shingrix_non IC_age", i+49, ".csv", sep=""), row.names = 1)
}

all_dfs_zostavax_nonIC<-list()
for(i in 1:41)
{
  all_dfs_zostavax_nonIC[[i]]<- read.csv(paste(Output,"02-07-2019_CEA_zostavax_non IC_age", i+49, ".csv", sep=""), row.names = 1)
}



########################################
#### NMB all options using mean
########################################
# NMB is calculated as (incremental benefit x threshold) - incremental cost. Incremental NMB measures the difference in NMB between alternative interventions, a positive incremental NMB indicating that the intervention is cost-effective compared with the alterative at the given willingness-to-pay threshold.
# Net monetary benefit = (E * WTP) - C
# E = effectiveness; WTP = willingness-to-pay threshold; C = cost 


# shingrix IC
mean_NMB_shingrix_IC_WTP20000<-list()
cohort_size_IC<-vector()
for(i in 1:41){
  mean_NMB_shingrix_IC_WTP20000[[i]]<-(mean(all_dfs_shingrix_IC[[i]]$total_QALYs_gained_d)*20000)-(mean(all_dfs_shingrix_IC[[i]]$total_costs_intervention)-mean(all_dfs_shingrix_IC[[i]]$total_saved_costs_d)) 
  cohort_size_IC[i]<-mean(all_dfs_shingrix_IC[[i]]$cohort_size)
}

# shingrix non-IC
mean_NMB_shingrix_nonIC_WTP20000<-list()
cohort_size_nonIC<-vector()
for(i in 1:41){
  mean_NMB_shingrix_nonIC_WTP20000[[i]]<-(mean(all_dfs_shingrix_nonIC[[i]]$total_QALYs_gained_d)*20000)-(mean(all_dfs_shingrix_nonIC[[i]]$total_costs_intervention)-mean(all_dfs_shingrix_nonIC[[i]]$total_saved_costs_d)) 
  cohort_size_nonIC[i]<-mean(all_dfs_shingrix_nonIC[[i]]$cohort_size)
}

# zostavax non-IC
mean_NMB_zostavax_nonIC_WTP20000<-list()
for(i in 1:41){
  mean_NMB_zostavax_nonIC_WTP20000[[i]]<-(mean(all_dfs_zostavax_nonIC[[i]]$total_QALYs_gained_d)*20000)-(mean(all_dfs_zostavax_nonIC[[i]]$total_costs_intervention)-mean(all_dfs_zostavax_nonIC[[i]]$total_saved_costs_d)) 
}

# add to a df
mean_NMB_df<-data.frame(age=50:90,
                   # mean_NMB_shingrix_IC_WTP20000=unlist(mean_NMB_shingrix_IC_WTP20000),
                   mean_NMB_shingrix_nonIC_WTP20000=unlist(mean_NMB_shingrix_nonIC_WTP20000),
                   mean_NMB_zostavax_nonIC_WTP20000=unlist(mean_NMB_zostavax_nonIC_WTP20000))

# add no vaccination scenario
mean_NMB_df$mean_NMB_no_vac_WTP20000<-0


# write
write.csv(mean_NMB_df, file=paste(Output,"03-06-2019 net monetary benefit table.csv", sep=""))

## plots
# IC
library(reshape2)
mean_NMB_df2 = melt(mean_NMB_df, id=c("age"))
mean_NMB_df2<-mean_NMB_df2%>%filter(variable=="mean_NMB_shingrix_IC_WTP20000"|variable=="mean_NMB_no_vac_WTP20000")
plot_mean_NMB_IC<-ggplot(data=mean_NMB_df2, aes(x=age, y=value, col=variable))+
  geom_line()+theme_bw()+#geom_hline(yintercept=0, lty=2)+
  ylab("mean net monetary benefit for WTP 20,000")+
  scale_color_manual(values=c("firebrick2","black"),
                     name="Vaccination strategy",
                     labels=c("HZ/su IC","no vac IC"))
cohort_IC<-data.frame(age=50:90,
                      cohort_size_IC=cohort_size_IC)
mean_NMB_df2b<-merge(mean_NMB_df2,cohort_IC, by="age")
mean_NMB_df2b$value_pp<-mean_NMB_df2b$value/mean_NMB_df2b$cohort_size_IC
plot_mean_NMB_IC_pp<-ggplot(data=mean_NMB_df2b, aes(x=age, y=value_pp, col=variable))+
  geom_line()+theme_bw()+#geom_hline(yintercept=0, lty=2)+
  ylab("mean net monetary benefit for WTP 20,000 per person")+
  scale_color_manual(values=c("firebrick2","black"),
                     name="Vaccination strategy",
                     labels=c("HZ/su IC","no vac IC"))
# non-IC
library(reshape2)
mean_NMB_df3 = melt(mean_NMB_df, id=c("age"))
mean_NMB_df3<-mean_NMB_df3%>%filter(variable!="mean_NMB_shingrix_IC_WTP20000")
plot_mean_NMB_nonIC<-ggplot(data=mean_NMB_df3, aes(x=age, y=value, col=variable))+
  geom_line()+theme_bw()+#geom_hline(yintercept=0, lty=2)+
  ylab("mean net monetary benefit for WTP 20,000")+
  scale_color_manual(values=c("firebrick2", "#56B4E9", "black"),
                     name="Vaccination strategy",
                     labels=c("HZ/su non IC", "ZVL non IC", "no vac nonIC"))
cohort_nonIC<-data.frame(age=50:90,
                   cohort_size_nonIC=cohort_size_nonIC)
mean_NMB_df3b<-merge(mean_NMB_df3,cohort_nonIC, by="age")
mean_NMB_df3b$value_pp<-mean_NMB_df3b$value/mean_NMB_df3b$cohort_size_nonIC
plot_mean_NMB_nonIC_pp<-ggplot(data=mean_NMB_df3b, aes(x=age, y=value_pp, col=variable))+
  geom_line()+theme_bw()+#geom_hline(yintercept=0, lty=2)+
  ylab("mean net monetary benefit for WTP 20,000 per person")+
  scale_color_manual(values=c("firebrick2", "#56B4E9", "black"),
                     name="Vaccination strategy",
                    labels=c("HZ/su non IC", "ZVL non IC", "no vac nonIC"))




########################################
#### CEAF all options
########################################
# mark: for the option with the highest expected NMB calculate the % of samples that have the highest NMB (including no vaccination, i.e. both vaccine options have NMB<0).
# briggs: the frontier indicates the probability that the alternative with the highest NMB will be cost-effective

## IC

# IC -> shingrix 
NMB_shingrix_IC_WTP20000<-list()
p_NMB_shingrix_IC_WTP20000<-list()
for(i in 1:41){
  NMB_shingrix_IC_WTP20000[[i]]<-(all_dfs_shingrix_IC[[i]]$total_QALYs_gained_d*20000)-(all_dfs_shingrix_IC[[i]]$total_net_cost)
  p_NMB_shingrix_IC_WTP20000[[i]]<-(length(NMB_shingrix_IC_WTP20000[[i]][NMB_shingrix_IC_WTP20000[[i]]>0]))/(length(NMB_shingrix_IC_WTP20000[[i]]))
}

# IC -> no vac
p_NMB_no_vac_IC_WTP20000<-list()
for(i in 1:41){
  p_NMB_no_vac_IC_WTP20000[[i]]<-(length(NMB_shingrix_IC_WTP20000[[i]][NMB_shingrix_IC_WTP20000[[i]]<0]))/(length(NMB_shingrix_IC_WTP20000[[i]]))
}

## nonIC
NMB_shingrix_nonIC_WTP20000<-list()
NMB_zostavax_nonIC_WTP20000<-list()
for(i in 1:41){
  NMB_shingrix_nonIC_WTP20000[[i]]<-(all_dfs_shingrix_nonIC[[i]]$total_QALYs_gained_d*20000)-(all_dfs_shingrix_nonIC[[i]]$total_net_cost)
  NMB_zostavax_nonIC_WTP20000[[i]]<-(all_dfs_zostavax_nonIC[[i]]$total_QALYs_gained_d*20000)-(all_dfs_zostavax_nonIC[[i]]$total_net_cost)
}

# proportion of simulations for which no vac is best option ages 50-56
p_NMB_nonIC_WTP20000<-list()
for(i in 1:7){
  p_NMB_nonIC_WTP20000[[i]]<-(length(NMB_shingrix_nonIC_WTP20000[[i]][NMB_shingrix_nonIC_WTP20000[[i]]<0])/(length(NMB_shingrix_nonIC_WTP20000[[i]])))*(length(NMB_zostavax_nonIC_WTP20000[[i]][NMB_zostavax_nonIC_WTP20000[[i]]<0])/(length(NMB_zostavax_nonIC_WTP20000[[i]])))
}

# proportion of simulations for which shingrix is best option ages 57-71
for(i in 8:22){
  p_NMB_nonIC_WTP20000[[i]]<-(length(NMB_shingrix_nonIC_WTP20000[[i]][NMB_shingrix_nonIC_WTP20000[[i]]>NMB_zostavax_nonIC_WTP20000[[i]]])/(length(NMB_shingrix_nonIC_WTP20000[[i]])))*(length(NMB_shingrix_nonIC_WTP20000[[i]][NMB_shingrix_nonIC_WTP20000[[i]]>0])/(length(NMB_shingrix_nonIC_WTP20000[[i]])))
}  

# proportion of simulations for which zostavax is best option ages 72-74
for(i in 23:25){
  p_NMB_nonIC_WTP20000[[i]]<-(length(NMB_zostavax_nonIC_WTP20000[[i]][NMB_zostavax_nonIC_WTP20000[[i]]>NMB_shingrix_nonIC_WTP20000[[i]]])/(length(NMB_zostavax_nonIC_WTP20000[[i]])))*(length(NMB_zostavax_nonIC_WTP20000[[i]][NMB_zostavax_nonIC_WTP20000[[i]]>0])/(length(NMB_zostavax_nonIC_WTP20000[[i]])))
}

# proportion of simulations for which no vac is best option ages 75+
for(i in 26:41){
  p_NMB_nonIC_WTP20000[[i]]<-(length(NMB_shingrix_nonIC_WTP20000[[i]][NMB_shingrix_nonIC_WTP20000[[i]]<0])/(length(NMB_shingrix_nonIC_WTP20000[[i]])))*(length(NMB_zostavax_nonIC_WTP20000[[i]][NMB_zostavax_nonIC_WTP20000[[i]]<0])/(length(NMB_zostavax_nonIC_WTP20000[[i]])))
} 

## list proportion of simulations for which best option all ages nonIC

# proportion of simulations for which no vac is best option any age
p_NMB_no_vac_nonIC_WTP20000<-list()
for(i in 1:41){
  p_NMB_no_vac_nonIC_WTP20000[[i]]<-(length(NMB_shingrix_nonIC_WTP20000[[i]][NMB_shingrix_nonIC_WTP20000[[i]]<0])/(length(NMB_shingrix_nonIC_WTP20000[[i]])))*(length(NMB_zostavax_nonIC_WTP20000[[i]][NMB_zostavax_nonIC_WTP20000[[i]]<0])/(length(NMB_zostavax_nonIC_WTP20000[[i]])))
}

# proportion of simulations for which shingrix is best option any age
p_NMB_shingrix_nonIC_WTP20000<-list()
for(i in 1:41){
  p_NMB_shingrix_nonIC_WTP20000[[i]]<-(length(NMB_shingrix_nonIC_WTP20000[[i]][NMB_shingrix_nonIC_WTP20000[[i]]>NMB_zostavax_nonIC_WTP20000[[i]]])/(length(NMB_shingrix_nonIC_WTP20000[[i]])))*(length(NMB_shingrix_nonIC_WTP20000[[i]][NMB_shingrix_nonIC_WTP20000[[i]]>0])/(length(NMB_shingrix_nonIC_WTP20000[[i]])))
}  

# proportion of simulations for which zostavax is best option any age
p_NMB_zostavax_nonIC_WTP20000<-list()
for(i in 1:41){
  p_NMB_zostavax_nonIC_WTP20000[[i]]<-(length(NMB_zostavax_nonIC_WTP20000[[i]][NMB_zostavax_nonIC_WTP20000[[i]]>NMB_shingrix_nonIC_WTP20000[[i]]])/(length(NMB_zostavax_nonIC_WTP20000[[i]])))*(length(NMB_zostavax_nonIC_WTP20000[[i]][NMB_zostavax_nonIC_WTP20000[[i]]>0])/(length(NMB_zostavax_nonIC_WTP20000[[i]])))
}



## add to a df
p_NMB_df<-data.frame(age=50:90,
                     p_best_option_IC=unlist(p_NMB_shingrix_IC_WTP20000),
                     p_best_option_nonIC=unlist(p_NMB_nonIC_WTP20000),
                     vaccination_stategy_nonIC=c(rep("no vaccination",length(50:55)), 
                                                 rep("shingrix",length(56:71)),
                                                 rep("zostavax",length(72:74)), 
                                                 rep("no vaccination", length(75:90))),
                     vaccination_stategy_IC=c(rep("shingrix",length(50:90))))

## add to a second df nonIC including these proabilities when no longer optimal
p_NMB_df_nonIC<-data.frame(age=rep(50:90,3),
                     p_best_option_nonIC=c(unlist(p_NMB_no_vac_nonIC_WTP20000),
                                           unlist(p_NMB_shingrix_nonIC_WTP20000),
                                           unlist(p_NMB_zostavax_nonIC_WTP20000)),
                     vaccination_stategy_nonIC=c(rep("no vaccination",length(50:90)), 
                                                 rep("shingrix",length(50:90)),
                                                 rep("zostavax",length(50:90))))


## plots
# nonIC plot1
plot_p_best_option_age_nonIC<-ggplot(data=p_NMB_df, aes(x=age, y=p_best_option_nonIC,col=vaccination_stategy_nonIC))+
  geom_point()+theme_bw()+
  ylab("probability of being optimal strategy")+
  labs(col="optimal vaccination strategy")+
  scale_color_manual(values=c("black", "firebrick2", "#56B4E9"),
                     labels=c("no vaccination","HZ/su", "ZVL"))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1.02))

# nonIC plot2
plot_p_option_age_nonIC<-ggplot(data=p_NMB_df_nonIC, aes(x=age, y=p_best_option_nonIC,col=vaccination_stategy_nonIC))+
  geom_point()+theme_bw()+
  ylab("probability of being optimal strategy")+
  labs(col="optimal vaccination strategy")+
  scale_color_manual(values=c("black", "firebrick2", "#56B4E9"),
                     labels=c("no vaccination","HZ/su", "ZVL"))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1.02))

# IC
plot_p_best_option_age_IC<-ggplot(data=p_NMB_df, aes(x=age, y=p_best_option_IC,col=vaccination_stategy_IC))+
  geom_point()+theme_bw()+
  ylab("probability of being optimal strategy")+
  labs(col="optimal vaccination strategy")+
  scale_color_manual(values=c("firebrick2"),
                     labels=c("HZ/su"))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1.02))




########################################
#### JCVI thresholds analysis 
########################################

## most plausible ICER should be below 20,000

# IC -> shingrix 
JCVI_50_shingrix_IC<-vector()
for(i in 1:41){
  JCVI_50_shingrix_IC[[i]]<-(quantile(all_dfs_shingrix_IC[[i]]$total_QALYs_gained_d, prob=0.5)*20000)-(quantile(all_dfs_shingrix_IC[[i]]$total_net_cost, prob=0.5)) 
}

# nonIC -> shingrix 
JCVI_50_shingrix_nonIC<-vector()
for(i in 1:41){
  JCVI_50_shingrix_nonIC[[i]]<-(quantile(all_dfs_shingrix_nonIC[[i]]$total_QALYs_gained_d, prob=0.5)*20000)-(quantile(all_dfs_shingrix_nonIC[[i]]$total_net_cost, prob=0.5))
}

# nonIC -> zostavax 
JCVI_50_zostavax_nonIC<-vector()
for(i in 1:41){
  JCVI_50_zostavax_nonIC[[i]]<-(quantile(all_dfs_zostavax_nonIC[[i]]$total_QALYs_gained_d, prob=0.5)*20000)-(quantile(all_dfs_zostavax_nonIC[[i]]$total_net_cost, prob=0.5))
}


## 90% ICERs should be below 30,000

# IC -> shingrix 
JCVI_90_shingrix_IC<-vector()
for(i in 1:41){
  JCVI_90_shingrix_IC[[i]]<-(quantile(all_dfs_shingrix_IC[[i]]$total_QALYs_gained_d, prob=0.9)*30000)-(quantile(all_dfs_shingrix_IC[[i]]$total_net_cost, prob=0.9)) 
}

# nonIC -> shingrix 
JCVI_90_shingrix_nonIC<-vector()
for(i in 1:41){
  JCVI_90_shingrix_nonIC[[i]]<-(quantile(all_dfs_shingrix_nonIC[[i]]$total_QALYs_gained_d, prob=0.9)*30000)-(quantile(all_dfs_shingrix_nonIC[[i]]$total_net_cost, prob=0.9))
}

# nonIC -> zostavax 
JCVI_90_zostavax_nonIC<-vector()
for(i in 1:41){
  JCVI_90_zostavax_nonIC[[i]]<-(quantile(all_dfs_zostavax_nonIC[[i]]$total_QALYs_gained_d, prob=0.9)*30000)-(quantile(all_dfs_zostavax_nonIC[[i]]$total_net_cost, prob=0.9))
}

## data plots
# Median
JCVI_50<-data.frame(age=rep(50:90,4),
                    median_NMB=c(JCVI_50_shingrix_IC,
                                 JCVI_50_shingrix_nonIC,
                                 JCVI_50_zostavax_nonIC,
                                 rep(0,length(50:90))),
                    vac_strategy= rep(c("HZ/su IC", "HZ/su non-IC", "ZVL non-IC", "no vac"), each=length(50:90)))
JCVI_50_IC<-JCVI_50%>%filter(vac_strategy=="HZ/su IC" | vac_strategy=="no vac")
JCVI_50_IC<-merge(JCVI_50_IC,cohort_IC, by="age")

JCVI_50_nonIC<-JCVI_50%>%filter(vac_strategy!="HZ/su IC")
JCVI_50_nonIC<-merge(JCVI_50_nonIC,cohort_nonIC, by="age")
# 90th quantile
JCVI_90<-data.frame(age=rep(50:90,4),
                    quant_90_NMB=c(JCVI_90_shingrix_IC,
                                   JCVI_90_shingrix_nonIC, 
                                   JCVI_90_zostavax_nonIC,
                                   rep(0,length(50:90))),
                    vac_strategy= rep(c("HZ/su IC", "HZ/su non-IC", "ZVL non-IC", "no vac"), each=length(50:90)))
JCVI_90_IC<-JCVI_90%>%filter(vac_strategy=="HZ/su IC" | vac_strategy=="no vac")
JCVI_90_IC<-merge(JCVI_90_IC,cohort_IC, by="age")

JCVI_90_nonIC<-JCVI_90%>%filter(vac_strategy!="HZ/su IC")
JCVI_90_nonIC<-merge(JCVI_90_nonIC,cohort_nonIC, by="age")

# plots
plot_JCVI_50_IC<-ggplot(data=JCVI_50_IC, aes(x=age, y=median_NMB, col=vac_strategy))+
  geom_line()+theme_bw()+
  ylab("Median NMB ?20,000 WTP threshold")+
  scale_color_manual(values=c("firebrick2", "black"))+
  labs(col="vaccination strategy")

plot_JCVI_50_IC_pp<-ggplot(data=JCVI_50_IC, aes(x=age, y=median_NMB/cohort_size_IC, col=vac_strategy))+
  geom_line()+theme_bw()+
  ylab("Median NMB ?20,000 WTP threshold per person")+
  scale_color_manual(values=c("firebrick2", "black"))+
  labs(col="vaccination strategy")

plot_JCVI_50_nonIC<-ggplot(data=JCVI_50_nonIC, aes(x=age, y=median_NMB, col=vac_strategy))+
  geom_line()+theme_bw()+
  ylab("Median NMB ?20,000 WTP threshold")+
  scale_color_manual(values=c("firebrick2", "black", "#56B4E9"))+
  labs(col="vaccination strategy")

plot_JCVI_50_nonIC_pp<-ggplot(data=JCVI_50_nonIC, aes(x=age, y=median_NMB/cohort_size_nonIC, col=vac_strategy))+
  geom_line()+theme_bw()+
  ylab("Median NMB ?20,000 WTP threshold per person")+
  scale_color_manual(values=c("firebrick2", "black", "#56B4E9"))+
  labs(col="vaccination strategy")

plot_JCVI_90_IC<-ggplot(data=JCVI_90_IC, aes(x=age, y=quant_90_NMB, col=vac_strategy))+
  geom_line()+theme_bw()+
  ylab("90% NMB ?30,000 WTP threshold")+
  scale_color_manual(values=c("firebrick2", "black"))+
  labs(col="vaccination strategy")

plot_JCVI_90_IC_pp<-ggplot(data=JCVI_90_IC, aes(x=age, y=quant_90_NMB/cohort_size_IC, col=vac_strategy))+
  geom_line()+theme_bw()+
  ylab("90% NMB ?30,000 WTP threshold per person")+
  scale_color_manual(values=c("firebrick2", "black"))+
  labs(col="vaccination strategy")

plot_JCVI_90_nonIC<-ggplot(data=JCVI_90_nonIC, aes(x=age, y=quant_90_NMB, col=vac_strategy))+
  geom_line()+theme_bw()+
  ylab("90% NMB ?30,000 WTP threshold")+
  scale_color_manual(values=c("firebrick2", "black", "#56B4E9"))+
  labs(col="vaccination strategy")

plot_JCVI_90_nonIC_pp<-ggplot(data=JCVI_90_nonIC, aes(x=age, y=quant_90_NMB/cohort_size_nonIC, col=vac_strategy))+
  geom_line()+theme_bw()+
  ylab("90% NMB ?30,000 WTP threshold per person")+
  scale_color_manual(values=c("firebrick2", "black", "#56B4E9"))+
  labs(col="vaccination strategy")


########################################
#### Threshold price for Shingrix, main JCVI
########################################

### at what price does shingrix become CE with WTP threshold of 20,000?

threshold_price_shingrix_nonIC_WTP20000<-list()
for(i in 1:20){
  threshold_price_shingrix_nonIC_WTP20000[[i]]<-((all_dfs_shingrix_nonIC[[i]]$total_QALYs_gained_d*20000)/all_dfs_shingrix_nonIC[[i]]$cohort_size)+all_dfs_zostavax_nonIC[[i]]$total_CS_d
}

for(i in 21:30){
  threshold_price_shingrix_nonIC_WTP20000[[i]]<-(((20000*(all_dfs_zostavax_nonIC[[i]]$total_QL_post_vac_d-all_dfs_shingrix_nonIC[[i]]$total_QL_post_vac_d))+(all_dfs_zostavax_nonIC[[i]]$total_C_post_vac_d-all_dfs_shingrix_nonIC[[i]]$total_C_post_vac_d))/all_dfs_shingrix_nonIC[[i]]$cohort_size)
}

for(i in 31:41){
  threshold_price_shingrix_nonIC_WTP20000[[i]]<-((all_dfs_shingrix_nonIC[[i]]$total_QALYs_gained_d*20000)/all_dfs_shingrix_nonIC[[i]]$cohort_size)+all_dfs_zostavax_nonIC[[i]]$total_CS_d
}

plot_df_hist_20000<-data.frame(age=c(rep(50,length(threshold_price_shingrix_nonIC_WTP20000[[1]])),
                               rep(55,length(threshold_price_shingrix_nonIC_WTP20000[[1]])),
                               rep(60,length(threshold_price_shingrix_nonIC_WTP20000[[1]])),
                               rep(65,length(threshold_price_shingrix_nonIC_WTP20000[[1]])),
                               rep(70,length(threshold_price_shingrix_nonIC_WTP20000[[1]])),
                               rep(75,length(threshold_price_shingrix_nonIC_WTP20000[[1]])),
                               rep(80,length(threshold_price_shingrix_nonIC_WTP20000[[1]])),
                               rep(85,length(threshold_price_shingrix_nonIC_WTP20000[[1]])),
                               rep(90,length(threshold_price_shingrix_nonIC_WTP20000[[1]]))),
                         threshold_price_20000=c(unlist(threshold_price_shingrix_nonIC_WTP20000[[1]]),
                                                 unlist(threshold_price_shingrix_nonIC_WTP20000[[6]]),
                                                 unlist(threshold_price_shingrix_nonIC_WTP20000[[11]]),
                                                 unlist(threshold_price_shingrix_nonIC_WTP20000[[16]]),
                                                 unlist(threshold_price_shingrix_nonIC_WTP20000[[21]]),
                                                 unlist(threshold_price_shingrix_nonIC_WTP20000[[26]]),
                                                 unlist(threshold_price_shingrix_nonIC_WTP20000[[31]]),
                                                 unlist(threshold_price_shingrix_nonIC_WTP20000[[36]]),
                                                 unlist(threshold_price_shingrix_nonIC_WTP20000[[41]])))

mean_threshold<-plot_df_hist_20000%>%group_by(age)%>%summarise(m.threshold=mean(threshold_price_20000))
median_threshold<-plot_df_hist_20000%>%group_by(age)%>%summarise(m.threshold=quantile(threshold_price_20000, prob=0.5))

comparison_strategy<-data.frame(age=50:90,
                                comp_stategy= c(rep("no vaccination", length(50:69)),
                                                      rep("ZVL", length(70:79)),
                                                      rep("no vaccination", length(80:90))))

median_threshold<-merge(median_threshold, comparison_strategy, by="age")

plot_df_hist_20000<-merge(plot_df_hist_20000, median_threshold, by="age", all.x=T)

                                
plot_hist_threshold_price_20000<-ggplot(data=plot_df_hist_20000, aes(x=threshold_price_20000,label=paste("?",as.character(round(m.threshold)), sep="")))+
  geom_histogram(bins=50)+theme_bw()+
  # geom_vline(data=mean_threshold, aes(xintercept = m.threshold), col="red")+
  geom_vline(data=median_threshold, aes(xintercept = m.threshold,col=comp_stategy), lty=2)+
  facet_wrap(~age)+ xlab("threshold cost (?) for HZ/su at ?20,000 WTP")+
  ylab("number of PSA runs")+
  geom_text(data=median_threshold, aes(x=m.threshold+20, y=200, col=comp_stategy), size=3.5)+
  scale_color_discrete(name = "Comparison strategy")

# figure out median cost at ages 69
median(unlist(threshold_price_shingrix_nonIC_WTP20000[[20]]))
median(unlist(threshold_price_shingrix_nonIC_WTP20000[[30]]))
  
### at what price does shingrix become CE with WTP threshold of 30,000?

threshold_price_shingrix_nonIC_WTP30000<-list()
for(i in 1:20){
  threshold_price_shingrix_nonIC_WTP30000[[i]]<-(30000*(all_dfs_shingrix_nonIC[[i]]$total_QALYs_gained_d/all_dfs_shingrix_nonIC[[i]]$cohort_size))+all_dfs_zostavax_nonIC[[i]]$total_CS_d
}

for(i in 21:30){
  threshold_price_shingrix_nonIC_WTP30000[[i]]<-(((30000*(all_dfs_zostavax_nonIC[[i]]$total_QL_post_vac_d-all_dfs_shingrix_nonIC[[i]]$total_QL_post_vac_d))+(all_dfs_zostavax_nonIC[[i]]$total_C_post_vac_d-all_dfs_shingrix_nonIC[[i]]$total_C_post_vac_d))/all_dfs_shingrix_nonIC[[i]]$cohort_size)
}

for(i in 31:41){
  threshold_price_shingrix_nonIC_WTP30000[[i]]<-(30000*(all_dfs_shingrix_nonIC[[i]]$total_QALYs_gained_d/all_dfs_shingrix_nonIC[[i]]$cohort_size))+all_dfs_zostavax_nonIC[[i]]$total_CS_d
}

plot_df_hist_30000<-data.frame(age=c(rep(50,length(threshold_price_shingrix_nonIC_WTP30000[[1]])),
                               rep(55,length(threshold_price_shingrix_nonIC_WTP30000[[1]])),
                               rep(60,length(threshold_price_shingrix_nonIC_WTP30000[[1]])),
                               rep(65,length(threshold_price_shingrix_nonIC_WTP30000[[1]])),
                               rep(70,length(threshold_price_shingrix_nonIC_WTP30000[[1]])),
                               rep(75,length(threshold_price_shingrix_nonIC_WTP30000[[1]])),
                               rep(80,length(threshold_price_shingrix_nonIC_WTP30000[[1]])),
                               rep(85,length(threshold_price_shingrix_nonIC_WTP30000[[1]])),
                               rep(90,length(threshold_price_shingrix_nonIC_WTP30000[[1]]))),
                         threshold_price_30000=c(unlist(threshold_price_shingrix_nonIC_WTP30000[[1]]),
                                                 unlist(threshold_price_shingrix_nonIC_WTP30000[[6]]),
                                                 unlist(threshold_price_shingrix_nonIC_WTP30000[[11]]),
                                                 unlist(threshold_price_shingrix_nonIC_WTP30000[[16]]),
                                                 unlist(threshold_price_shingrix_nonIC_WTP30000[[21]]),
                                                 unlist(threshold_price_shingrix_nonIC_WTP30000[[26]]),
                                                 unlist(threshold_price_shingrix_nonIC_WTP30000[[31]]),
                                                 unlist(threshold_price_shingrix_nonIC_WTP30000[[36]]),
                                                 unlist(threshold_price_shingrix_nonIC_WTP30000[[41]])))

mean_threshold<-plot_df_hist_30000%>%group_by(age)%>%summarise(m.threshold=mean(threshold_price_30000))
median_threshold<-plot_df_hist_30000%>%group_by(age)%>%summarise(m.threshold=median(threshold_price_30000))
p90_threshold<-plot_df_hist_30000%>%group_by(age)%>%summarise(m.threshold=quantile(threshold_price_30000, prob=0.9))

comparison_strategy<-data.frame(age=50:90,
                                comp_stategy= c(rep("no vaccination", length(50:69)),
                                                rep("ZVL", length(70:79)),
                                                rep("no vaccination", length(80:90))))

p90_threshold<-merge(p90_threshold, comparison_strategy, by="age")

plot_df_hist_30000<-merge(plot_df_hist_30000, p90_threshold, by="age", all.x=T)

plot_hist_threshold_price_30000<-ggplot(data=plot_df_hist_30000, aes(x=threshold_price_30000,label=paste("?",as.character(round(m.threshold)), sep="")))+
  geom_histogram(bins=50)+theme_bw()+
  geom_vline(data=p90_threshold, aes(xintercept = m.threshold, col=comp_stategy), lty=2)+
  # geom_vline(data=median_threshold, aes(xintercept = m.threshold), col="red", lty=2)+
  facet_wrap(~age)+ xlab("threshold cost (?) for HZ/su at ?30,000 WTP")+
  ylab("number of PSA runs")+
  geom_text(data=p90_threshold, aes(x=m.threshold+30, y=200,col=comp_stategy), size=3.5)+
  scale_color_discrete(name = "Comparison strategy")



########################################
#### save plots
########################################

ggsave(plot_mean_NMB_IC, file=paste(Plots,"14-01-2019 Net monetary benefit IC shingrix.png",sep=""), width=15, height=10, unit="cm")
ggsave(plot_mean_NMB_nonIC, file=paste(Plots,"14-01-2019 Net monetary benefit nonIC shingrix zostavax.png",sep=""), width=15, height=10, unit="cm")
ggsave(plot_mean_NMB_IC_pp, file=paste(Plots,"14-01-2019 Net monetary benefit IC shingrix pp.png",sep=""), width=15, height=12, unit="cm")
ggsave(plot_mean_NMB_nonIC_pp, file=paste(Plots,"14-01-2019 Net monetary benefit nonIC shingrix zostavax pp.png",sep=""), width=15, height=12, unit="cm")
ggsave(plot_p_best_option_age_nonIC, file=paste(Plots,"14-01-2019 p optimal choice nonIC.png",sep=""), width=15, height=10, unit="cm")
ggsave(plot_p_best_option_age_IC, file=paste(Plots,"14-01-2019 p optimal choice IC.png",sep=""), width=15, height=10, unit="cm")
ggsave(plot_p_option_age_nonIC, file=paste(Plots,"14-01-2019 p choice nonIC.png",sep=""), width=15, height=10, unit="cm")
ggsave(plot_JCVI_50_IC, file=paste(Plots,"14-01-2019 JCVI median IC.png",sep=""), width=15, height=10, unit="cm")
ggsave(plot_JCVI_50_nonIC, file=paste(Plots,"14-01-2019 JCVI median nonIC.png",sep=""), width=15, height=10, unit="cm")
ggsave(plot_JCVI_90_IC, file=paste(Plots,"14-01-2019 JCVI 90p quantile IC.png",sep=""), width=15, height=10, unit="cm")
ggsave(plot_JCVI_90_nonIC, file=paste(Plots,"14-01-2019 JCVI 90p quantile nonIC.png",sep=""), width=15, height=10, unit="cm")
ggsave(plot_JCVI_50_IC_pp, file=paste(Plots,"14-01-2019 JCVI median IC pp.png",sep=""), width=15, height=10, unit="cm")
ggsave(plot_JCVI_50_nonIC_pp, file=paste(Plots,"14-01-2019 JCVI median nonIC pp.png",sep=""), width=15, height=10, unit="cm")
ggsave(plot_JCVI_90_IC_pp, file=paste(Plots,"14-01-2019 JCVI 90p quantile IC pp.png",sep=""), width=15, height=10, unit="cm")
ggsave(plot_JCVI_90_nonIC_pp, file=paste(Plots,"14-01-2019 JCVI 90p quantile nonIC pp.png",sep=""), width=15, height=10, unit="cm")
ggsave(plot_hist_threshold_price_20000, file=paste(Plots,"28-01-2019 Threshold price 20000.png",sep=""), width=25, height=15, unit="cm")
ggsave(plot_hist_threshold_price_30000, file=paste(Plots,"28-01-2019 Threshold price 30000.png",sep=""), width=25, height=15, unit="cm")




# ########################################
# #### why is there a peak at age 68 in ICs
# ########################################
# 
# (mean(all_dfs_shingrix_IC[[19]]$total_QALYs_gained_d)*20000)-(mean(all_dfs_shingrix_IC[[19]]$total_costs_intervention)-mean(all_dfs_shingrix_IC[[19]]$total_saved_costs_d))
# 
# mean(all_dfs_shingrix_IC[[18]]$total_QALYs_gained_d) 
# mean(all_dfs_shingrix_IC[[19]]$total_QALYs_gained_d) 
# mean(all_dfs_shingrix_IC[[20]]$total_QALYs_gained_d) #big drop
# 
# mean(all_dfs_shingrix_IC[[18]]$total_QL_post_vac_d-all_dfs_shingrix_IC[[18]]$total_QL_no_vac_d) 
# 
# 
# mean(all_dfs_shingrix_IC[[18]]$total_costs_intervention)
# mean(all_dfs_shingrix_IC[[19]]$total_costs_intervention)
# mean(all_dfs_shingrix_IC[[20]]$total_costs_intervention) # big drop
# 
# mean(all_dfs_shingrix_IC[[18]]$total_saved_costs_d)
# mean(all_dfs_shingrix_IC[[19]]$total_saved_costs_d)
# mean(all_dfs_shingrix_IC[[20]]$total_saved_costs_d) # big drop
# 
# mean(all_dfs_shingrix_IC[[18]]$total_QALYs_gained_d) *20000
# mean(all_dfs_shingrix_IC[[19]]$total_QALYs_gained_d) *20000
# mean(all_dfs_shingrix_IC[[20]]$total_QALYs_gained_d) *20000
# 
# 
# 
# (mean(all_dfs_shingrix_IC[[18]]$total_costs_intervention)-mean(all_dfs_shingrix_IC[[18]]$total_saved_costs_d))
# (mean(all_dfs_shingrix_IC[[19]]$total_costs_intervention)-mean(all_dfs_shingrix_IC[[19]]$total_saved_costs_d))
# (mean(all_dfs_shingrix_IC[[20]]$total_costs_intervention)-mean(all_dfs_shingrix_IC[[20]]$total_saved_costs_d))
# 
# # total_costs_intervention<-cohort_size*((vaccine_cost_per_dose+admin_cost_per_dose)*number_courses)
# # cohort_size<-(Pop_2015[Pop_2015$Age%in%vaccination_age,]$Pop) * vaccine_coverage
# #(Pop_2015[Pop_2015$Age%in%vaccination_age,]$Pop) only thing that is not a constant by age
# # the peak is due to the 
# 
# # England population in the model, ONS mid-year estimate population England 2015
# Pop_2015<-read.csv(file=paste(Data,"midyearestimates2015 ONS population England.csv", sep=""))
# Pop_2015$Pop <- gsub(",","",Pop_2015$Pop)
# Pop_2015$Pop<-as.numeric(as.character(Pop_2015$Pop))
# Pop_2015<-Pop_2015[2:nrow(Pop_2015),] # remove age All
# Pop_2015$Age<-as.numeric(as.character(Pop_2015$Age))
# ggplot(data=Pop_2015, aes(x=Age, y=Pop))+geom_line()# peak is in the england population
# 
# 
# # AJ's old model
# Pop_2015<-read.csv(file=paste(Data,"Background population AJ model.csv", sep=""))
# names(Pop_2015)<-c("Age", "Pop")
# ggplot(data=Pop_2015, aes(x=Age, y=Pop))+geom_line() # peak also in AJ's data
# 
# # what is peak due in England population
# 2015-1947 # baby boom peak in 1947 coindides with our peak age 68 (2015 data)
# 
# 
# # is there something similar in the other vaccine/IC combos
# mean(all_dfs_shingrix_nonIC[[18]]$total_QALYs_gained_d) 
# mean(all_dfs_shingrix_nonIC[[19]]$total_QALYs_gained_d) 
# mean(all_dfs_shingrix_nonIC[[20]]$total_QALYs_gained_d) #big drop, yes
# 
# mean(all_dfs_shingrix_nonIC[[18]]$total_costs_intervention)
# mean(all_dfs_shingrix_nonIC[[19]]$total_costs_intervention)
# mean(all_dfs_shingrix_nonIC[[20]]$total_costs_intervention) # big drop, yes
# 
# mean(all_dfs_shingrix_nonIC[[18]]$total_saved_costs_d)
# mean(all_dfs_shingrix_nonIC[[19]]$total_saved_costs_d)
# mean(all_dfs_shingrix_nonIC[[20]]$total_saved_costs_d) # big drop, yes

# 
# ########################################
# #### why is there a switch from shingrix to zostavax for ages 72-74
# ########################################
# 
# (mean(all_dfs_shingrix_nonIC[[21]]$total_QALYs_gained_d)*20000)-(mean(all_dfs_shingrix_nonIC[[21]]$total_costs_intervention)-mean(all_dfs_shingrix_nonIC[[21]]$total_saved_costs_d))
# (mean(all_dfs_zostavax_nonIC[[21]]$total_QALYs_gained_d)*20000)-(mean(all_dfs_zostavax_nonIC[[21]]$total_costs_intervention)-mean(all_dfs_zostavax_nonIC[[21]]$total_saved_costs_d))
# 
# (mean(all_dfs_shingrix_nonIC[[22]]$total_QALYs_gained_d)*20000)-(mean(all_dfs_shingrix_nonIC[[22]]$total_costs_intervention)-mean(all_dfs_shingrix_nonIC[[22]]$total_saved_costs_d))
# (mean(all_dfs_zostavax_nonIC[[22]]$total_QALYs_gained_d)*20000)-(mean(all_dfs_zostavax_nonIC[[22]]$total_costs_intervention)-mean(all_dfs_zostavax_nonIC[[22]]$total_saved_costs_d))
# 
# (mean(all_dfs_shingrix_nonIC[[23]]$total_QALYs_gained_d)*20000)-(mean(all_dfs_shingrix_nonIC[[23]]$total_costs_intervention)-mean(all_dfs_shingrix_nonIC[[23]]$total_saved_costs_d))
# (mean(all_dfs_zostavax_nonIC[[23]]$total_QALYs_gained_d)*20000)-(mean(all_dfs_zostavax_nonIC[[23]]$total_costs_intervention)-mean(all_dfs_zostavax_nonIC[[23]]$total_saved_costs_d))
# 
# (mean(all_dfs_shingrix_nonIC[[24]]$total_QALYs_gained_d)*20000)-(mean(all_dfs_shingrix_nonIC[[24]]$total_costs_intervention)-mean(all_dfs_shingrix_nonIC[[24]]$total_saved_costs_d))
# (mean(all_dfs_zostavax_nonIC[[24]]$total_QALYs_gained_d)*20000)-(mean(all_dfs_zostavax_nonIC[[24]]$total_costs_intervention)-mean(all_dfs_zostavax_nonIC[[24]]$total_saved_costs_d))