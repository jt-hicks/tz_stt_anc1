orderly::orderly_dependency("02_data_quality", quote(latest()),
                             c('dqa_hf_remove_duplicates_2017.rds',
                               'dqa_hf_remove_duplicates_2022.rds',
                               'dqa_council_summ4map.rds'))
orderly::orderly_shared_resource('theme_base.R')
orderly::orderly_shared_resource('addCIs.R')
orderly::orderly_shared_resource('data/u5yo_prev_dhs_tanz_2017-2022.xlsx')
orderly::orderly_shared_resource('data/dhs_dates.xlsx')
orderly::orderly_shared_resource('data/paper_data_strat_G.xlsx')
orderly::orderly_artefact(files=c('fig1_map_correlation_composite.png',
                                   'fig1_detailed_maps_correlation.png',
                                   'suppfig_full_correlation_range.png',
                                   'suppfig1_council_maps_allyears.png',
                                   'suppfig2_u5_vs_agegroups_correlation.png'),
                           description = 'Manuscript figures: council prevalence maps and correlations')

source('theme_base.R')
source('addCIs.R')
dqa_hf_2017 <- readRDS('dqa_hf_remove_duplicates_2017.rds')
dqa_hf_2022 <- readRDS('dqa_hf_remove_duplicates_2022.rds')

van_eijk <- read_excel('data/paper_data_strat_G.xlsx')

u5yo_prev <- read_xlsx('data/u5yo_prev_dhs_tanz_2017-2022.xlsx',sheet = 'Regions')%>%
  mutate(Region = ifelse(Regions=='Dar es Salaam','Dar Es Salaam',Regions),
         positive = round(tested * prev/100))
u5yo_prev <- addCIs(df=u5yo_prev,Ys=u5yo_prev$positive,Ns=u5yo_prev$tested)

dhs_dates <- read_xlsx('data/dhs_dates.xlsx')

dqa_hf_2017_sum <- dqa_hf_2017 %>%
  group_by(Region)%>%
  summarise(ANC_test = sum(ANC_test,na.rm = TRUE),
            ANC_pos = sum(ANC_pos,na.rm = TRUE),
            ANC_test_lt20 = sum(ANC_test_lt20,na.rm = TRUE),
            ANC_pos_lt20 = sum(ANC_pos_lt20,na.rm = TRUE),
            ANC_test_ge20 = sum(ANC_test_ge20,na.rm = TRUE),
            ANC_pos_ge20 = sum(ANC_pos_ge20,na.rm = TRUE))
dqa_hf_2017_sum <- addCIs(df=dqa_hf_2017_sum,Ys=dqa_hf_2017_sum$ANC_pos,Ns=dqa_hf_2017_sum$ANC_test)%>%
  rename(mean_total = mean,
         lower_total = lower,
         upper_total = upper)
dqa_hf_2017_sum <- addCIs(df=dqa_hf_2017_sum,Ys=dqa_hf_2017_sum$ANC_pos_lt20,Ns=dqa_hf_2017_sum$ANC_test_lt20)%>%
  rename(mean_lt20 = mean,
         lower_lt20 = lower,
         upper_lt20 = upper)
dqa_hf_2017_sum <- addCIs(df=dqa_hf_2017_sum,Ys=dqa_hf_2017_sum$ANC_pos_ge20,Ns=dqa_hf_2017_sum$ANC_test_ge20)%>%
  rename(mean_ge20 = mean,
         lower_ge20 = lower,
         upper_ge20 = upper)
dqa_hf_2017_sum$year <- 2017
dqa_hf_2017_sum$Region <- gsub(' Region','',dqa_hf_2017_sum$Region)

dqa_hf_2022_sum <- dqa_hf_2022 %>%
  group_by(Region)%>%
  summarise(ANC_test = sum(ANC_test,na.rm = TRUE),
            ANC_pos = sum(ANC_pos,na.rm = TRUE),
            ANC_test_10_14 = sum(ANC_test_10_14,na.rm = TRUE),
            ANC_pos_10_14 = sum(ANC_pos_10_14,na.rm = TRUE),
            ANC_test_15_19 = sum(ANC_test_15_19,na.rm = TRUE),
            ANC_pos_15_19 = sum(ANC_pos_15_19,na.rm = TRUE),
            ANC_test_20_24 = sum(ANC_test_20_24,na.rm = TRUE),
            ANC_pos_20_24 = sum(ANC_pos_20_24,na.rm = TRUE),
            ANC_test_25_29 = sum(ANC_test_25_29,na.rm = TRUE),
            ANC_pos_25_29 = sum(ANC_pos_25_29,na.rm = TRUE),
            ANC_test_30_34 = sum(ANC_test_30_34,na.rm = TRUE),
            ANC_pos_30_34 = sum(ANC_pos_30_34,na.rm = TRUE),
            ANC_test_ge35 = sum(ANC_test_ge35,na.rm = TRUE),
            ANC_pos_ge35 = sum(ANC_pos_ge35,na.rm = TRUE),
  )%>%
  mutate(ANC_test_lt20 = ANC_test_10_14+ANC_test_15_19,
         ANC_pos_lt20 = ANC_pos_10_14+ANC_pos_15_19,
         ANC_test_ge20 = ANC_test_20_24 + ANC_test_25_29 + ANC_test_30_34 + ANC_test_ge35,
         ANC_pos_ge20 = ANC_pos_20_24 + ANC_pos_25_29 + ANC_pos_30_34 + ANC_pos_ge35)
dqa_hf_2022_sum <- addCIs(df=dqa_hf_2022_sum,Ys=dqa_hf_2022_sum$ANC_pos,Ns=dqa_hf_2022_sum$ANC_test)%>%
  rename(mean_total = mean,
         lower_total = lower,
         upper_total = upper)
dqa_hf_2022_sum <- addCIs(df=dqa_hf_2022_sum,Ys=dqa_hf_2022_sum$ANC_pos_lt20,Ns=dqa_hf_2022_sum$ANC_test_lt20)%>%
  rename(mean_lt20 = mean,
         lower_lt20 = lower,
         upper_lt20 = upper)
dqa_hf_2022_sum <- addCIs(df=dqa_hf_2022_sum,Ys=dqa_hf_2022_sum$ANC_pos_ge20,Ns=dqa_hf_2022_sum$ANC_test_ge20)%>%
  rename(mean_ge20 = mean,
         lower_ge20 = lower,
         upper_ge20 = upper)
dqa_hf_2022_sum$year <- 2022
dqa_hf_2022_sum$Region <- gsub(' Region','',dqa_hf_2022_sum$Region)


dhs_anc_merged <- bind_rows(dqa_hf_2022_sum,dqa_hf_2017_sum)%>%
  left_join(u5yo_prev,by=c('Region','year'))%>%
  mutate(year=factor(year,levels=c('2017','2022')))
dhs_anc_merged$abs_diff <- dhs_anc_merged$mean_lt20 - dhs_anc_merged$mean_ge20
dhs_anc_merged$prev_ratio <- dhs_anc_merged$mean_lt20/dhs_anc_merged$mean_ge20

dhs_anc_merged$odds_ratio <- (dhs_anc_merged$mean_lt20/(1-dhs_anc_merged$mean_lt20))/(dhs_anc_merged$mean_ge20/(1-dhs_anc_merged$mean_ge20))
dhs_anc_merged$odds_u5 <- dhs_anc_merged$mean/(1-dhs_anc_merged$mean)

##Add Van Eijk et al data
all_ve <- van_eijk %>%
  subset(subG %in% c('P','M')) %>%
  group_by(Site)%>%
  dplyr::summarise(inf_prev_n=mean(child_Y),
                   inf_N=mean(child_N),
                   ANC_prev_n=sum(preg_Y),
                   ANC_N=sum(preg_N))%>%
  dplyr::select(-Site) %>%
  dplyr::rename(positive=inf_prev_n,
                tested=inf_N,
                ANC_pos=ANC_prev_n,
                ANC_test=ANC_N)
all_ve <- addCIs(df=all_ve,Ys=all_ve$ANC_pos,Ns=all_ve$ANC_test)%>%
  rename(mean_total = mean,
         lower_total = lower,
         upper_total = upper)
all_ve <- addCIs(df=all_ve,Ys=all_ve$positive,Ns=all_ve$tested)
all_ve$Region <- 'Van Eijk, et al.'
all_ve$year <- as.factor('Van Eijk, et al.')

dhs_anc_merged <- bind_rows(dhs_anc_merged,all_ve)
# dhs_anc_merged%>%
#   #mutate(year_char <- as.character(year))%>%
#   filter(as.character(year)!='Van Eijk, et al.')
# unique(dhs_anc_merged$year)
all_ages_correlation <- ggplot(dhs_anc_merged%>%
                                 filter(as.character(year)!='Van Eijk, et al.'))+
  geom_abline(color='darkgrey',linetype='dashed',linewidth=1)+
  geom_point(aes(x=mean,y=mean_total,color=year),size=2)+
  geom_errorbar(aes(x=mean,ymin=lower_total,ymax=upper_total,color=year),linewidth=1,width=0)+
  geom_errorbarh(aes(xmin=lower,xmax=upper,y=mean_total,color=year),linewidth=1,height=0)+
  scale_color_brewer(palette='Dark2')+
  # scale_x_continuous(limits = c(0,.35),expand=c(0,0))+
  # scale_y_continuous(limits = c(0,.35),expand=c(0,0))+
  labs(x='Malaria Prevalence\nUnder 5 years old',
       y='Malaria Prevalence\nANC1',
       title='All ages at ANC1')
under20_correlation <- ggplot(dhs_anc_merged)+
  geom_abline(color='darkgrey',linetype='dashed',linewidth=1)+
  geom_point(aes(x=mean,y=mean_lt20,color=year),size=2)+
  geom_errorbar(aes(x=mean,ymin=lower_lt20,ymax=upper_lt20,color=year),linewidth=1,width=0)+
  geom_errorbarh(aes(xmin=lower,xmax=upper,y=mean_lt20,color=year),linewidth=1,height=0)+
  scale_color_brewer(palette='Dark2')+
  scale_x_continuous(limits = c(0,.35),expand=c(0,0))+
  scale_y_continuous(limits = c(0,.35),expand=c(0,0))+
  labs(x='Malaria Prevalence\nUnder 5 years old',
       y='Malaria Prevalence\nANC1',
       title='Less than 20 years old at ANC1')
over20_correlation <- ggplot(dhs_anc_merged)+
  geom_abline(color='darkgrey',linetype='dashed',linewidth=1)+
  geom_point(aes(x=mean,y=mean_ge20,color=year),size=2)+
  geom_errorbar(aes(x=mean,ymin=lower_ge20,ymax=upper_ge20,color=year),linewidth=1,width=0)+
  geom_errorbarh(aes(xmin=lower,xmax=upper,y=mean_ge20,color=year),linewidth=1,height=0)+
  scale_color_brewer(palette='Dark2')+
  scale_x_continuous(limits = c(0,.35),expand=c(0,0))+
  scale_y_continuous(limits = c(0,.35),expand=c(0,0))+
  labs(x='Malaria Prevalence\nUnder 5 years old',
       y='Malaria Prevalence\nANC1',
       title = '20 years and older at ANC1')
composite_plots <- all_ages_correlation+under20_correlation+over20_correlation+plot_layout(nrow=1,guides='collect')
ggsave(composite_plots,filename='TZ_anc_dhs_corr_underover20.png',width=12,height=4)

dhs_anc_merged_only2022 <- dhs_anc_merged%>%
  filter(year=='2022')%>%
  rename(mean_u5 = mean,
         lower_u5 = lower,
         upper_u5 = upper)
dhs_anc_merged_only2022 <- addCIs(df=dhs_anc_merged_only2022,Ys=dhs_anc_merged_only2022$ANC_pos_15_19,Ns=dhs_anc_merged_only2022$ANC_test_15_19)%>%
  rename(mean_15_19 = mean,
         lower_15_19 = lower,
         upper_15_19 = upper)
dhs_anc_merged_only2022 <- addCIs(df=dhs_anc_merged_only2022,Ys=dhs_anc_merged_only2022$ANC_pos_20_24,Ns=dhs_anc_merged_only2022$ANC_test_20_24)%>%
  rename(mean_20_24 = mean,
         lower_20_24 = lower,
         upper_20_24 = upper)
dhs_anc_merged_only2022 <- addCIs(df=dhs_anc_merged_only2022,Ys=dhs_anc_merged_only2022$ANC_pos_25_29,Ns=dhs_anc_merged_only2022$ANC_test_25_29)%>%
  rename(mean_25_29 = mean,
         lower_25_29 = lower,
         upper_25_29 = upper)
dhs_anc_merged_only2022 <- addCIs(df=dhs_anc_merged_only2022,Ys=dhs_anc_merged_only2022$ANC_pos_30_34,Ns=dhs_anc_merged_only2022$ANC_test_30_34)%>%
  rename(mean_30_34 = mean,
         lower_30_34 = lower,
         upper_30_34 = upper)
dhs_anc_merged_only2022 <- addCIs(df=dhs_anc_merged_only2022,Ys=dhs_anc_merged_only2022$ANC_pos_ge35,Ns=dhs_anc_merged_only2022$ANC_test_ge35)%>%
  rename(mean_ge35 = mean,
         lower_ge35 = lower,
         upper_ge35 = upper)

color_pallete_ages <- RColorBrewer::brewer.pal(6,'RdYlBu')[2:6]
names(color_pallete_ages) <- c('15 to 19 years','20 to 24 years','25 to 29 years','30 to 34 years','35 years and older')
all_ages_correlation_2022 <- ggplot(dhs_anc_merged_only2022)+
  geom_abline(color='darkgrey',linetype='dashed',linewidth=1)+
  geom_point(aes(x=mean_u5,y=mean_15_19,color='15 to 19 years'),size=2)+
  geom_point(aes(x=mean_u5,y=mean_20_24,color='20 to 24 years'),size=2)+
  geom_point(aes(x=mean_u5,y=mean_25_29,color='25 to 29 years'),size=2)+
  geom_point(aes(x=mean_u5,y=mean_30_34,color='30 to 34 years'),size=2)+
  geom_point(aes(x=mean_u5,y=mean_ge35,color='35 years and older'),size=2)+
  geom_errorbar(aes(x=mean_u5,ymin=lower_15_19,ymax=upper_15_19,color='15 to 19 years'),linewidth=1,width=0)+
  geom_errorbar(aes(x=mean_u5,ymin=lower_20_24,ymax=upper_20_24,color='20 to 24 years'),linewidth=1,width=0)+
  geom_errorbar(aes(x=mean_u5,ymin=lower_25_29,ymax=upper_25_29,color='25 to 29 years'),linewidth=1,width=0)+
  geom_errorbar(aes(x=mean_u5,ymin=lower_30_34,ymax=upper_30_34,color='30 to 34 years'),linewidth=1,width=0)+
  geom_errorbar(aes(x=mean_u5,ymin=lower_ge35,ymax=upper_ge35,color='35 years and older'),linewidth=1,width=0)+
  # geom_errorbarh(aes(xmin=lower_u5,xmax=upper_u5,y=mean_15_19,color='15 to 19 years'),linewidth=1,height=0)+
  # geom_errorbarh(aes(xmin=lower_u5,xmax=upper_u5,y=mean_20_24,color='20 to 24 years'),linewidth=1,height=0)+
  # geom_errorbarh(aes(xmin=lower_u5,xmax=upper_u5,y=mean_25_29,color='25 to 29 years'),linewidth=1,height=0)+
  # geom_errorbarh(aes(xmin=lower_u5,xmax=upper_u5,y=mean_30_34,color='30 to 34 years'),linewidth=1,height=0)+
  # geom_errorbarh(aes(xmin=lower_u5,xmax=upper_u5,y=mean_ge35,color='35 years and older'),linewidth=1,height=0)+
  scale_color_manual(values=color_pallete_ages,name='Age at ANC1')+
  scale_x_continuous(limits = c(0,.35),expand=c(0,0))+
  scale_y_continuous(limits = c(0,.35),expand=c(0,0))+
  labs(x='Malaria Prevalence - Under 5 years old',
       y='Malaria Prevalence - ANC1')+
  guides(color=guide_legend(nrow=2,byrow=TRUE))
ggsave(all_ages_correlation_2022,filename='TZ_anc_dhs_corr_2022_5agegroups.png',width=5,height=5)


#####Running it in stan with nuts algorithm
# Prepare data list
stan_data <- list(
  N             = nrow(dhs_anc_merged),
  pos_child     = as.integer(dhs_anc_merged$positive),
  total_child   = as.integer(dhs_anc_merged$tested),
  pos_preg_pg   = as.integer(dhs_anc_merged$ANC_pos),
  total_preg_pg = as.integer(dhs_anc_merged$ANC_test)
)

# Compile
sm <- stan_model("anc_allages.stan")

# Sample with NUTS
fit_stan <- sampling(
  sm,
  data       = stan_data,
  chains     = 4,
  iter       = 2000,
  warmup     = 500,
  control    = list(adapt_delta = 0.95, max_treedepth = 12),
  seed       = 123
)

print(fit_stan, pars = c(
  "av_lo_child","intercept_pg","gradient_pg",
  "sigma_c","sigma_int"
))

traceplot(fit_stan,pars=c(
  "av_lo_child","intercept_pg","gradient_pg",
  "sigma_c","sigma_int"
))
pairs(fit_stan,pars=c(
  "av_lo_child","intercept_pg","gradient_pg",
  "sigma_c","sigma_int"),
  diag = TRUE, off_diag=TRUE)
stan_ac(fit_stan,pars=c(
  "av_lo_child","intercept_pg","gradient_pg",
  "sigma_c","sigma_int"))

df_draws <- as.data.frame(as.matrix(fit_stan))
attach(df_draws)

prev_child <- seq(0.01,max(dhs_anc_merged$upper),by=0.01)
logodds_child <-logit(prev_child)

## fn to return prevalence from log_odds
get_prev_from_log_odds<-function(log_odds){
  return(exp(log_odds)/(1+exp(log_odds)))
}

## fn to return odds from prevalence
get_odds_from_prev<-function(prev){
  return(prev/(1-prev))
}

mcmc_sim_summary <- dplyr::bind_rows(lapply(1:length(logodds_child),function(i){
  prev_preg_pg <- get_prev_from_log_odds(logodds_child[i]+intercept_pg+gradient_pg*(logodds_child[i]-av_lo_child))
  # prev_preg_sg <- get_prev_from_log_odds(logit(prev_preg_pg)+intercept_sg+gradient_sg*(logodds_child[i]-av_lo_child))
  # prev_preg_mg <- get_prev_from_log_odds(logit(prev_preg_pg)+intercept_mg+gradient_mg*(logodds_child[i]-av_lo_child))
  #Primigrav-specific gradient
  log_OR_pp_v_c <-intercept_pg+gradient_pg*(logodds_child[i]-av_lo_child)
  # #Secundigrav-specific gradient
  # log_OR_ps_v_pp<-intercept_sg+gradient_sg*(logodds_child[i]-av_lo_child)
  # #Multigrav-specific gradient
  # log_OR_pm_v_pp <-intercept_mg+gradient_mg*(logodds_child[i]-av_lo_child)

  prev_preg_pg_quant <- quantile(prev_preg_pg,c(0.025,0.5,0.975))
  # prev_preg_sg_quant <- quantile(prev_preg_sg,c(0.025,0.5,0.975))
  # prev_preg_mg_quant <- quantile(prev_preg_mg,c(0.025,0.5,0.975))
  log_OR_pp_v_c_quant <- quantile(log_OR_pp_v_c,c(0.025,0.5,0.975))
  # log_OR_ps_v_pp_quant <- quantile(log_OR_ps_v_pp,c(0.025,0.5,0.975))
  # log_OR_pm_v_pp_quant <- quantile(log_OR_pm_v_pp,c(0.025,0.5,0.975))

  data.frame(prev_child = get_prev_from_log_odds(logodds_child[i]),
             prev_preg_pg_median = prev_preg_pg_quant[[2]],
             prev_preg_pg_lower = prev_preg_pg_quant[[1]],
             prev_preg_pg_upper = prev_preg_pg_quant[[3]],
             # prev_preg_sg_median = prev_preg_sg_quant[[2]],
             # prev_preg_sg_lower = prev_preg_sg_quant[[1]],
             # prev_preg_sg_upper = prev_preg_sg_quant[[3]],
             # prev_preg_mg_median = prev_preg_mg_quant[[2]],
             # prev_preg_mg_lower = prev_preg_mg_quant[[1]],
             # prev_preg_mg_upper = prev_preg_mg_quant[[3]],
             log_OR_pp_v_c_median = log_OR_pp_v_c_quant[[2]],
             log_OR_pp_v_c_lower = log_OR_pp_v_c_quant[[1]],
             log_OR_pp_v_c_upper = log_OR_pp_v_c_quant[[3]])
             # log_OR_ps_v_pp_median = log_OR_ps_v_pp_quant[[2]],
             # log_OR_ps_v_pp_lower = log_OR_ps_v_pp_quant[[1]],
             # log_OR_ps_v_pp_upper = log_OR_ps_v_pp_quant[[3]],
             # log_OR_pm_v_pp_median = log_OR_pm_v_pp_quant[[2]],
             # log_OR_pm_v_pp_lower = log_OR_pm_v_pp_quant[[1]],
             # log_OR_pm_v_pp_upper = log_OR_pm_v_pp_quant[[3]])
}))

saveRDS(mcmc_sim_summary,'mcmc_sim_summary_stan.rds')

mcmc_sim_summary <- readRDS('mcmc_sim_summary_stan.rds')

colors_corr <- c(c(viridis(2,begin=0.2,end=0.8)),'#999999')

grav <- ggplot(dhs_anc_merged)+
  geom_point(aes(x=mean*100,y=mean_total*100,col=year),size=3)+
  geom_errorbar(aes(x=mean*100,ymin=lower_total*100,ymax=upper_total*100,col=year),width=0)+
  geom_errorbarh(aes(y=mean_total*100,xmin=lower*100,xmax=upper*100,col=year),height=0)+
  scale_color_manual(values=colors_corr)+
  scale_y_continuous(limits = c(0,85),expand = c(0,0))+
  scale_x_continuous(limits = c(0,85),expand = c(0,0))+
  geom_abline(size=0.8,linetype='dashed')+
  geom_ribbon(data=mcmc_sim_summary,aes(x=prev_child*100,ymin=prev_preg_pg_lower*100,ymax=prev_preg_pg_upper*100),alpha=0.2)+
  geom_line(data=mcmc_sim_summary,aes(x=prev_child*100,y=prev_preg_pg_median*100),size=1)+
  theme(legend.position = 'bottom',
      legend.title = element_blank())+
  labs(x='Cross-section Prevalence (<5 yo)',y='ANC Prevalence (all ages)')

grav_tz_zoom <- ggplot(dhs_anc_merged%>%filter(Region!='Van Eijk, et al.'))+
  geom_point(aes(x=mean,y=mean_total,col=year),size=3)+
  geom_errorbar(aes(x=mean,ymin=lower_total,ymax=upper_total,col=year),width=0)+
  geom_errorbarh(aes(y=mean_total,xmin=lower,xmax=upper,col=year),height=0)+
  scale_color_manual(values=colors_corr)+
  scale_y_continuous(limits = c(0,.85),expand = c(0,0))+
  scale_x_continuous(limits = c(0,.85),expand = c(0,0))+
  coord_cartesian(xlim=c(0,.30),ylim=c(0,.30))+
  geom_abline(size=0.8,linetype='dashed')+
  geom_ribbon(data=mcmc_sim_summary,aes(x=prev_child,ymin=prev_preg_pg_lower,ymax=prev_preg_pg_upper),alpha=0.2)+
  geom_line(data=mcmc_sim_summary,aes(x=prev_child,y=prev_preg_pg_median),size=1)+
  theme(legend.position = 'bottom',
        legend.title = element_blank())+
  labs(x='Cross-section Prevalence (<5 yo)',y='ANC Prevalence (all ages)')
corr_composite <- grav+grav_tz_zoom+plot_layout(nrow=1,guides='collect')
ggsave('corr_withVE_composite_stan.png',plot=corr_composite,units='in',height=4,width=8)
ggsave('corr_withoutVE_stan.png',plot=grav_tz_zoom,units='in',height=4,width=4)

#----------------------------------------------------------
# 7. Compute LOOIC
#----------------------------------------------------------
# 1) Pull out all the saved p_child[1: N_sites] from all chains:
p_child_draws <- run_full$sims.list$p_child[30000:120000,]
p_child_draws <- run_full$sims.list$p_child
#   – this is an array (n_draws_total × N_sites).
#   n_draws_total = (#chains)*(#iterations saved per chain).

# 2) Create a matrix (n_draws_total × N_sites) of log‐likelihoods:
y_obs <- data_list$pos_child
n_obs <- data_list$total_child

# A little helper: for each site i, we take all draws of p_child_draws[ , i ],
# then compute dbinom( y_obs[i], size = n_obs[i], prob = that_draw, log=TRUE ).

log_lik_mat <- matrix(NA,
                      nrow = nrow(p_child_draws),
                      ncol = N_sites)

for (i in seq_len(N_sites)) {
  log_lik_mat[, i] <- dbinom(
    x    = y_obs[i],
    size = n_obs[i],
    prob = p_child_draws[, i],
    log  = TRUE
  )
}

# 3) Now feed that to loo::loo():
loo_result <- loo(log_lik_mat)
saveRDS(loo_result,file='loo_result_fixedANCprev.rds')
print(loo_result)


#----------------------------------------------------------
# 8. Leave-One-Out Cross Validation for Plotting
#----------------------------------------------------------
# 1) Preallocate vectors to hold each site’s held‐out posterior summaries:
predicted_means  <- numeric(N_sites)
predicted_lowers <- numeric(N_sites)
predicted_uppers <- numeric(N_sites)
OR_pregvc_means <- numeric(N_sites)
OR_pregvc_lowers <- numeric(N_sites)
OR_pregvc_uppers <- numeric(N_sites)
observed_props   <- y_obs / n_obs    # “observed” child prevalence at each site

# 2) Loop over sites:
for (i in seq_len(N_sites)) {
  cat("Leaving out site", i, "of", N_sites, "...\n")

  # 2a) Which rows remain?
  loo_idx <- setdiff(seq_len(N_sites), i)
  n_loo   <- length(loo_idx)

  # 2b) Build the LOO data list:
  loo_data <- list(
    pos_child     = as.integer(dhs_anc_merged$positive[loo_idx]),
    total_child   = as.integer(dhs_anc_merged$tested[loo_idx]),
    pos_preg_pg   = as.integer(dhs_anc_merged$ANC_pos[loo_idx]),
    total_preg_pg = as.integer(dhs_anc_merged$ANC_test[loo_idx]),
    # pos_preg_mg   = as.integer(dhs_anc_merged$ANC_pos_ge20[loo_idx]),
    # total_preg_mg = as.integer(dhs_anc_merged$ANC_test_ge20[loo_idx]),
    N             = n_loo
  )

  # 2c) Run BUGS on the “leave‐i‐out” data:
  loo_inits <- function() {
    list(
      av_lo_child=rnorm(1),
      intercept_pg=rnorm(1),
      gradient_pg=rnorm(1),
      sigma_c_inv=runif(1),
      sigma_int_inv=runif(1)
    )
  }

  loo_fit <- bugs(
    data               = loo_data,
    inits              = loo_inits,
    parameters.to.save = c("av_lo_child",
                           "gradient_pg"),
    model.file         = "model_predictive.txt",
    n.chains           = 2,
    n.iter             = 10000,
    n.burnin           = 2000,
    n.thin             = 5,
    DIC                = FALSE,
    debug              = FALSE,
    working.directory  = getwd()
  )

  # 2d) Extract posterior draws for coefficients:
  av_lo_child_samps  <- loo_fit$sims.list$av_lo_child
  gradient_pg_samps <- loo_fit$sims.list$gradient_pg

  # 2e) Compute the “observed” pregnant‐woman proportions at site i (stabilized):
  preg_obs <- (dhs_anc_merged$ANC_pos[i] + 0.5) / (dhs_anc_merged$ANC_test[i] + 1)
  logit_preg_i <- qlogis(preg_obs)

  # 2f) For each posterior draw, predict log‐odds → p_child at held‐out site:
  #     logit(p_pred) = alpha + beta_pg*logit_pg_i + beta_mg*logit_mg_i.
  n_draws <- length(alpha_samps)
  log_odds_pred_draws <- (logit_preg_i + gradient_pg_samps*av_lo_child_samps)/(gradient_pg_samps-1)
  log_OR_pp_v_c <-gradient_pg_samps*(log_odds_pred_draws-av_lo_child_samps)
  OR_pp_v_c <- exp(log_OR_pp_v_c)
  p_pred_draws <- plogis(log_odds_pred_draws)

  # 2g) Summarize:
  predicted_means[i]  <- mean(p_pred_draws)
  predicted_lowers[i] <- quantile(p_pred_draws, 0.025)
  predicted_uppers[i] <- quantile(p_pred_draws, 0.975)
  OR_pregvc_means[i]  <- mean(OR_pp_v_c)
  OR_pregvc_lowers[i] <- quantile(OR_pp_v_c, 0.025)
  OR_pregvc_uppers[i] <- quantile(OR_pp_v_c, 0.975)

}

# 3) Build a data‐frame for plotting:
plot_df <- data.frame(
  site      = seq_len(N_sites),
  observed  = observed_props,
  predicted = predicted_means,
  lower     = predicted_lowers,
  upper     = predicted_uppers,
  or = OR_pregvc_means,
  or_lower     = OR_pregvc_lowers,
  or_upper =   OR_pregvc_uppers
)
saveRDS(plot_df,file='df_4_looplot_gradientallagesfixedANCprev.rds')
plot_df <- readRDS('df_4_looplot_gradientallagesfixedANCprev.rds')
# 4) Finally, plot observed vs. predicted with 95% CIs:
obs_v_pred_gradient <- ggplot(plot_df, aes(x = observed, y = predicted)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", colour = "grey50") +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0) +
  # scale_x_continuous(limits=c(0,0.3),expand=c(0,0))+
  # scale_y_continuous(limits=c(0,0.3),expand=c(0,0))+
  labs(
    x = "Observed under 5 malaria prevalence",
    y = "Predicted under 5 malaria prevalence",
    title = "Less than 20 years ANC1 model performance"
  )
ggsave(plot=obs_v_pred_lt20,filename = 'obs_v_pred_plot_lt20fixedANCprev.png',width=5,height=5,units = 'in')
loo_composit <- obs_v_pred_lt20 + obs_v_pred + obs_v_pred_allages + obs_v_pred_interaction + plot_layout(ncol=2)
ggsave(plot=loo_composit,filename = 'obs_v_pred_plot_lt20fixedANCprevcomp.png',width=10,height=10,units = 'in')

##Model Comparison:
run_full_lt20fixedANCprev <- readRDS('run_full_lt20fixedANCprev.rds')
run_full_interactionfixedANCprev <- readRDS('run_full_interactionfixedANCprev.rds')
run_full_allagesfixedANCprev <- readRDS('run_full_allagesfixedANCprev.rds')
run_full_fixedANCprev <- readRDS('run_full_fixedANCprev.rds')

run_full_fixedANCprev$DIC
run_full_allagesfixedANCprev$DIC
run_full_lt20fixedANCprev$DIC
run_full_interactionfixedANCprev$DIC

##========================================================================
## NEW FIGURES FOR MANUSCRIPT
##========================================================================

##------------------------------------------------------------------------
## Figure 1: Composite - 2024 Council Map + Stan/VE Correlation
##------------------------------------------------------------------------
library(sf)
library(scales)

# Load council map data
dqa_map <- readRDS('dqa_council_summ4map.rds')

# Filter for 2024 data only
dqa_map_2024 <- dqa_map %>% filter(Year == 2024)

# Create 2024 council prevalence map
# Use YlOrRd palette to match manuscript style
map_2024 <- ggplot(dqa_map_2024) +
  geom_sf(aes(fill = prevalence*100), color = 'white', size = 0.1) +
  scale_fill_distiller(palette = 'YlOrRd', direction = 1,
                       name = 'Prevalence (%)',
                       limits = c(0, 20),
                       oob = squish,
                       breaks = seq(0, 20, 5)) +
  labs(title = 'ANC1 Malaria Prevalence by Council (2024)') +
  theme_void() +
  theme(legend.position = 'bottom',
        legend.key.width = unit(1.5, 'cm'),
        plot.title = element_text(hjust = 0.5, face = 'bold', size = 12))

# Use existing Stan/VE correlation composite (already created above as corr_composite)
# Recreate grav_tz_zoom with better formatting for publication
# Use OkabeIto palette to match manuscript style from 03_attendance_rate
oi_palette <- colorblindr::palette_OkabeIto
col_2017 <- oi_palette[1]  # First color
col_2022 <- oi_palette[2]  # Second color
col_VE <- oi_palette[8]    # Grey color for Van Eijk

# Create main scatterplot (0-0.85) with zoom box annotation
grav_tz_main <- ggplot(dhs_anc_merged) +
  # Add zoom box rectangle
  annotate('rect', xmin = 0, xmax = 0.35, ymin = 0, ymax = 0.35, 
           fill = NA, color = 'black', linewidth = 1.2) +
  geom_point(aes(x = mean, y = mean_total, col = year), size = 3) +
  geom_errorbar(aes(x = mean, ymin = lower_total, ymax = upper_total, col = year), width = 0) +
  geom_errorbarh(aes(y = mean_total, xmin = lower, xmax = upper, col = year), height = 0) +
  scale_color_manual(values = c('2017' = col_2017, '2022' = col_2022, 'Van Eijk, et al.' = col_VE), name = 'DHS Survey Year') +
  scale_y_continuous(limits = c(0, 0.85), expand = c(0, 0)) +
  scale_x_continuous(limits = c(0, 0.85), expand = c(0, 0)) +
  coord_fixed(xlim = c(0, 0.85), ylim = c(0, 0.85), ratio = 1) +
  geom_abline(size = 0.8, linetype = 'dashed', color = 'grey30') +
  geom_ribbon(data = mcmc_sim_summary, 
              aes(x = prev_child, ymin = prev_preg_pg_lower, ymax = prev_preg_pg_upper), 
              alpha = 0.2, fill = 'grey50') +
  geom_line(data = mcmc_sim_summary, 
            aes(x = prev_child, y = prev_preg_pg_median), 
            size = 1, color = 'black') +
  labs(x = 'Under 5 Malaria Prevalence',
       y = 'ANC1 Malaria Prevalence (All Ages)') +
  guides(color = guide_legend(override.aes = list(alpha = 0))) +
  theme(plot.margin = margin(15, 5, 5, 5),
        legend.position = 'bottom',
        legend.text = element_blank(),
        legend.title = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 11),
        plot.background = element_rect(fill = 'white', color = NA),
        panel.background = element_rect(fill = 'white', color = NA))

# Create zoomed inset scatterplot (0-0.35)
grav_tz_inset <- ggplot(dhs_anc_merged) +
  geom_point(aes(x = mean, y = mean_total, col = year), size = 3) +
  geom_errorbar(aes(x = mean, ymin = lower_total, ymax = upper_total, col = year), width = 0) +
  geom_errorbarh(aes(y = mean_total, xmin = lower, xmax = upper, col = year), height = 0) +
  scale_color_manual(values = c('2017' = col_2017, '2022' = col_2022, 'Van Eijk, et al.' = col_VE), name = 'DHS Survey Year') +
  scale_y_continuous(limits = c(0, 0.35), expand = c(0, 0)) +
  scale_x_continuous(limits = c(0, 0.35), expand = c(0, 0)) +
  coord_fixed(xlim = c(0, 0.35), ylim = c(0, 0.35), ratio = 1) +
  geom_abline(size = 0.8, linetype = 'dashed', color = 'grey30') +
  geom_ribbon(data = mcmc_sim_summary, 
              aes(x = prev_child, ymin = prev_preg_pg_lower, ymax = prev_preg_pg_upper), 
              alpha = 0.2, fill = 'grey50') +
  geom_line(data = mcmc_sim_summary, 
            aes(x = prev_child, y = prev_preg_pg_median), 
            size = 1, color = 'black') +
  labs(x = 'Under 5 Malaria Prevalence',
       y = 'ANC1 Malaria Prevalence (All Ages)') +
  theme(plot.margin = margin(5, 5, 5, 5),
        legend.position = 'bottom',
        legend.box = 'horizontal',
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 11),
        plot.background = element_rect(fill = 'white', color = NA),
        panel.background = element_rect(fill = 'white', color = NA))

# Combine main and inset scatterplots vertically
grav_tz_zoom_pub <- grav_tz_main / grav_tz_inset + 
  plot_layout(heights = c(1, 1))

##------------------------------------------------------------------------
## Figure 1A: Detailed - Tanzania map with zoom boxes + regional maps + correlation
##------------------------------------------------------------------------

# Create full Tanzania map with zoom region boxes
tz_full_map <- ggplot(dqa_map_2024) +
  geom_sf(aes(fill = prevalence*100), color = 'white', size = 0.1) +
  scale_fill_distiller(palette = 'YlOrRd', direction = 1,
                       name = 'Prevalence (%)',
                       limits = c(0, 20),
                       oob = squish,
                       breaks = seq(0, 20, 5)) +
  labs(title = 'ANC1 Malaria Prevalence by Council (2024)') +
  theme_void() +
  theme(legend.position = 'bottom',
        legend.key.width = unit(1.5, 'cm'),
        plot.title = element_text(hjust = 0.5, face = 'bold', size = 12),
        plot.background = element_rect(fill = 'white', color = NA),
        panel.background = element_rect(fill = 'white', color = NA))

# Define bounding boxes for zoom regions
# Lake Victoria region: expanded to include more land to the south
# Includes Kagera, Mwanza, Geita regions with buffer north of Tanzania border
lake_victoria_bbox <- st_bbox(c(xmin = 30.3, ymin = -4, xmax = 35.5, ymax = -0.5))
lake_victoria_box <- st_as_sfc(lake_victoria_bbox) %>% st_set_crs(st_crs(dqa_map_2024))

# Southern zone: Mtwara focus with modest buffer
southern_zone_bbox <- st_bbox(c(xmin = 37.2, ymin = -11.8, xmax = 40.7, ymax = -9.2))
southern_zone_box <- st_as_sfc(southern_zone_bbox) %>% st_set_crs(st_crs(dqa_map_2024))

# Add boxes to full Tanzania map
tz_full_map_with_boxes <- tz_full_map +
  geom_sf(data = lake_victoria_box, fill = NA, color = 'black', linewidth = 1.2) +
  geom_sf(data = southern_zone_box, fill = NA, color = 'black', linewidth = 1.2) +
  theme(plot.margin = margin(0, 0, 0, 0))

# Create Lake Victoria zoomed map
dqa_map_2024_lv <- dqa_map_2024 %>% 
  st_intersection(lake_victoria_box)

map_lv <- ggplot(dqa_map_2024_lv) +
  geom_sf(aes(fill = prevalence*100), color = 'white', size = 0.1) +
  scale_fill_distiller(palette = 'YlOrRd', direction = 1,
                       name = 'Prevalence (%)',
                       limits = c(0, 20),
                       oob = squish,
                       breaks = seq(0, 20, 5)) +
  labs(title = 'Lake Zone') +
  theme_void() +
  theme(legend.position = 'none',
        plot.title = element_text(hjust = 0.5, face = 'bold', size = 11),
        plot.margin = margin(2, 1, 2, 2),
        plot.background = element_rect(fill = 'white', color = NA),
        panel.background = element_rect(fill = 'white', color = NA))
        plot.background = element_rect(fill = 'white', color = NA),
        panel.background = element_rect(fill = 'white', color = NA))

# Create Southern Zone zoomed map
dqa_map_2024_sz <- dqa_map_2024 %>% 
  st_intersection(southern_zone_box)

map_sz <- ggplot(dqa_map_2024_sz) +
  geom_sf(aes(fill = prevalence*100), color = 'white', size = 0.1) +
  scale_fill_distiller(palette = 'YlOrRd', direction = 1,
                       name = 'Prevalence (%)',
                       limits = c(0, 20),
                       oob = squish,
                       breaks = seq(0, 20, 5)) +
  labs(title = 'Southern Zone') +
  theme_void() +
  theme(legend.position = 'none',
        plot.title = element_text(hjust = 0.5, face = 'bold', size = 11),
        plot.margin = margin(2, 2, 2, 1),
        plot.background = element_rect(fill = 'white', color = NA),
        panel.background = element_rect(fill = 'white', color = NA))

# Arrange maps and correlation plot: Full map + zooms on left, scatterplots on right
# Left: (A) Full TZ map / (B + C) Lake & Southern Zone maps
# Right: (D) Zoomed out scatter / (E) Zoomed in scatter
# Save individual panels and stitch with magick for precise alignment

# Save individual panels
ggsave('fig1_panel_A_tzmap.png', 
       plot = tz_full_map_with_boxes, 
       width = 5, height = 6, units = 'in', dpi = 300, bg = 'white')

ggsave('fig1_panel_B_lakezone.png', 
       plot = map_lv, 
       width = 4, height = 3, units = 'in', dpi = 300, bg = 'white')

ggsave('fig1_panel_C_southernzone.png', 
       plot = map_sz, 
       width = 4, height = 3, units = 'in', dpi = 300, bg = 'white')

ggsave('fig1_panel_D_scatter_main.png', 
       plot = grav_tz_main, 
       width = 3.5, height = 3.5, units = 'in', dpi = 300, bg = 'white')

ggsave('fig1_panel_E_scatter_inset.png', 
       plot = grav_tz_inset, 
       width = 3.5, height = 3.5, units = 'in', dpi = 300, bg = 'white')

# Use magick to composite panels with precise alignment
library(magick)

# Read panels
img_A <- magick::image_read('fig1_panel_A_tzmap.png')
img_B <- magick::image_read('fig1_panel_B_lakezone.png')
img_C <- magick::image_read('fig1_panel_C_southernzone.png')
img_D <- magick::image_read('fig1_panel_D_scatter_main.png')
img_E <- magick::image_read('fig1_panel_E_scatter_inset.png')

# Add letter annotations
img_A_labeled <- magick::image_annotate(
  img_A, "A)", size = 40, color = "black", 
  location = "+20+20", font = "sans", weight = 700
)

img_B_labeled <- magick::image_annotate(
  img_B, "B)", size = 35, color = "black",
  location = "+15+15", font = "sans", weight = 700
)

img_C_labeled <- magick::image_annotate(
  img_C, "C)", size = 35, color = "black",
  location = "+15+15", font = "sans", weight = 700
)

img_D_labeled <- magick::image_annotate(
  img_D, "D)", size = 40, color = "black",
  location = "+20+20", font = "sans", weight = 700
)

img_E_labeled <- magick::image_annotate(
  img_E, "E)", size = 40, color = "black",
  location = "+20+20", font = "sans", weight = 700
)

# Combine B and C horizontally
# Combine B and C vertically (column 2)
bc_column <- magick::image_append(c(img_B_labeled, img_C_labeled), stack = TRUE)

# Combine D and E vertically (column 3)
de_column <- magick::image_append(c(img_D_labeled, img_E_labeled), stack = TRUE)

# Scale columns to match 1.5:1:1 width ratio
# Get dimensions for scaling
info_a <- magick::image_info(img_A_labeled)
info_bc <- magick::image_info(bc_column)
info_de <- magick::image_info(de_column)

# Scale BC and DE to match the ratio (keeping A as base 1.5)
bc_width <- as.integer(info_a$width / 1.5)
de_width <- as.integer(info_a$width / 1.5)

bc_column_scaled <- magick::image_scale(bc_column, paste0(bc_width, "x!"))
de_column_scaled <- magick::image_scale(de_column, paste0(de_width, "x!"))

# Combine all three columns horizontally (A | BC | DE)
fig1_composite_final <- magick::image_append(c(img_A_labeled, bc_column_scaled, de_column_scaled), stack = FALSE)

# Flatten with white background to remove any transparency
fig1_composite_final <- magick::image_flatten(fig1_composite_final)

# Save final composite
magick::image_write(fig1_composite_final, path = 'fig1_map_correlation_composite.png')

cat('Figure 1 (composite map + correlation) saved\n')

##------------------------------------------------------------------------
## Supplemental Figure 1: Non-zoomed Correlation Plot
##------------------------------------------------------------------------

# Create full prevalence range correlation plot for supplemental figure
grav_tz_full <- ggplot(dhs_anc_merged) +
  geom_point(aes(x = mean*100, y = mean_total*100, col = year), size = 3) +
  geom_errorbar(aes(x = mean*100, ymin = lower_total*100, ymax = upper_total*100, col = year), width = 0) +
  geom_errorbarh(aes(y = mean_total*100, xmin = lower*100, xmax = upper*100, col = year), height = 0) +
  scale_color_manual(values = c('2017' = col_2017, '2022' = col_2022, 'Van Eijk, et al.' = col_VE), 
                     name = 'Survey') +
  scale_y_continuous(limits = c(0, 85), expand = c(0, 0)) +
  scale_x_continuous(limits = c(0, 85), expand = c(0, 0)) +
  geom_abline(size = 0.8, linetype = 'dashed', color = 'grey30') +
  geom_ribbon(data = mcmc_sim_summary, 
              aes(x = prev_child*100, ymin = prev_preg_pg_lower*100, ymax = prev_preg_pg_upper*100), 
              alpha = 0.2, fill = 'grey50') +
  geom_line(data = mcmc_sim_summary, 
            aes(x = prev_child*100, y = prev_preg_pg_median*100), 
            size = 1, color = 'black') +
  theme_minimal() +
  theme(legend.position = 'bottom',
        legend.title = element_blank()) +
  labs(x = 'U5 Malaria Prevalence (%)',
       y = 'ANC1 Malaria Prevalence (%)',
       title = 'Correlation: U5 vs ANC1 Prevalence (Full Range)')

ggsave('suppfig_full_correlation_range.png', 
       plot = grav_tz_full, 
       width = 5, height = 5, units = 'in', dpi = 300)

cat('Supplemental Figure - Full correlation plot saved\n')

##------------------------------------------------------------------------
## Supplemental Figure 1: Council Maps for All Years
##------------------------------------------------------------------------

# Create faceted map showing all years
# Use YlOrRd palette to match manuscript style
supp_fig1_allyears <- ggplot(dqa_map) +
  geom_sf(aes(fill = prevalence*100), color = 'white', size = 0.05) +
  scale_fill_distiller(palette = 'YlOrRd', direction = 1,
                       name = 'Prevalence (%)',
                       limits = c(0, 20),
                       oob = squish,
                       breaks = seq(0, 20, 5)) +
  facet_wrap(~Year, ncol = 3) +
  labs(title = 'ANC1 Malaria Prevalence by Council (All Years)') +
  theme_void() +
  theme(legend.position = 'bottom',
        legend.key.width = unit(2, 'cm'),
        plot.title = element_text(hjust = 0.5, face = 'bold', size = 14),
        strip.text = element_text(face = 'bold', size = 11))

ggsave('suppfig1_council_maps_allyears.png',
       plot = supp_fig1_allyears,
       width = 12, height = 8, units = 'in', dpi = 300)

cat('Supplemental Figure 1 (all years council maps) saved\n')

##------------------------------------------------------------------------
## Supplemental Figure 2: U5 vs All ANC Age Groups Correlation
##------------------------------------------------------------------------

# Prepare data in long format for easier plotting
dhs_anc_2022_long <- dhs_anc_merged_only2022 %>%
  select(Region, mean_u5, lower_u5, upper_u5,
         mean_15_19, lower_15_19, upper_15_19,
         mean_20_24, lower_20_24, upper_20_24,
         mean_25_29, lower_25_29, upper_25_29,
         mean_30_34, lower_30_34, upper_30_34,
         mean_ge35, lower_ge35, upper_ge35) %>%
  tidyr::pivot_longer(cols = starts_with('mean_') & !starts_with('mean_u5'),
                      names_to = 'age_group',
                      values_to = 'prev_anc',
                      names_prefix = 'mean_') %>%
  left_join(
    dhs_anc_merged_only2022 %>%
      select(Region, starts_with('lower_')) %>%
      tidyr::pivot_longer(cols = starts_with('lower_') & !starts_with('lower_u5'),
                          names_to = 'age_group',
                          values_to = 'lower_anc',
                          names_prefix = 'lower_'),
    by = c('Region', 'age_group')
  ) %>%
  left_join(
    dhs_anc_merged_only2022 %>%
      select(Region, starts_with('upper_')) %>%
      tidyr::pivot_longer(cols = starts_with('upper_') & !starts_with('upper_u5'),
                          names_to = 'age_group',
                          values_to = 'upper_anc',
                          names_prefix = 'upper_'),
    by = c('Region', 'age_group')
  ) %>%
  mutate(age_group_label = case_when(
    age_group == '15_19' ~ '15-19 years',
    age_group == '20_24' ~ '20-24 years',
    age_group == '25_29' ~ '25-29 years',
    age_group == '30_34' ~ '30-34 years',
    age_group == 'ge35' ~ '35+ years',
    TRUE ~ age_group
  ))

# Create faceted correlation plot
# Use RdYlBu palette to match manuscript style from 03_attendance_rate
color_pallete_age <- RColorBrewer::brewer.pal(6,'RdYlBu')[2:6]
names(color_pallete_age) <- c('15-19 years','20-24 years','25-29 years','30-34 years','35+ years')

supp_fig2_age_corr <- ggplot(dhs_anc_2022_long, 
                             aes(x = mean_u5, y = prev_anc, color = age_group_label)) +
  geom_abline(color = 'grey50', linetype = 'dashed', linewidth = 0.5) +
  geom_point(size = 2.5, alpha = 0.8) +
  geom_errorbar(aes(ymin = lower_anc, ymax = upper_anc), 
                linewidth = 0.5, width = 0, alpha = 0.7) +
  geom_errorbarh(aes(xmin = lower_u5, xmax = upper_u5), 
                 linewidth = 0.5, height = 0, alpha = 0.7) +
  facet_wrap(~age_group_label, ncol = 3) +
  scale_color_manual(values = color_pallete_age, guide = 'none') +
  scale_x_continuous(limits = c(0, 0.35), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 0.35), expand = c(0, 0)) +
  labs(x = 'Under 5 Malaria Prevalence (DHS 2022)',
       y = 'ANC1 Malaria Prevalence by Age Group',
       title = 'Correlation between U5 and ANC1 Prevalence by Age Group (2022)') +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = 'bold', size = 13),
        strip.text = element_text(face = 'bold', size = 10),
        axis.text = element_text(size = 9))

ggsave('suppfig2_u5_vs_agegroups_correlation.png',
       plot = supp_fig2_age_corr,
       width = 10, height = 7, units = 'in', dpi = 300)

cat('Supplemental Figure 2 (U5 vs age groups correlation) saved\n')

cat('\n====================\n')
cat('All manuscript figures completed!\n')
cat('====================\n')

