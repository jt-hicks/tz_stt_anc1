orderly2::orderly_dependency("01_read_data", quote(latest()),
                             c('Full_data.csv'))
orderly2::orderly_shared_resource('theme_base.R')
orderly2::orderly_shared_resource('District edited Jan 2021')
orderly2::orderly_artefact(files=c('alltested_council_map.png',
                                   'council_prevalence_map.png',
                                   'hf_alltested.png',
                                   'hf_attendance.png',
                                   'hf_count.png',
                                   'hf_intervention.png'),description = 'figures')
orderly2::orderly_artefact(files='dqa_hf_remove_duplicates.rds',description = 'cleaned HF level data')
orderly2::orderly_artefact(files='dqa_council_summ4map.rds',description = 'Data summarised at council level, with shapes for maps')

source('theme_base.R')
Full_data <- read.csv('Full_data.csv')

##############################
##Assess DQ at HF level###
##############################
##For each region, % of months with missing or 0 values##
##For each region, % of months with illogical values (#attended<#tested<#positive)
##% months with outliers for number tested, number positive, number attended
##% months with >33% difference from national ratio of current month to preceding year

##Get all possible months in data
yearmon_sequence <- as.yearmon(seq(as.Date("2014-01-01"), as.Date("2024-12-01"), by = "month"))

##Get names of specific variables of interest
results_cols <- c('ANC_test','ANC_pos','llin_provided','ipt2_provided','ipt3_provided','ipt4_provided','Hb_test','Anaemia')
all_results_cols <- c('Total_re_ad','Before_12wk','After_12wk','ANC_re_ad','ANC_test','ANC_pos','llin_provided','ipt2_provided','ipt3_provided','ipt4_provided','Hb_test','Anaemia')
main_results_cols <- c('Total_re_ad','Before_12wk','After_12wk','ANC_re_ad','ANC_test','ANC_pos','Hb_test','Anaemia')

cleaned_dqametrics <- Full_data %>%
  mutate(
    yearmon=as.yearmon(paste0(Year,"-",Month),"%Y-%m"))%>%
  tidyr::complete(yearmon,tidyr::nesting(Region,Council,HF))%>% #Filling in missing dates for each health facility
  mutate( #Replace NAs with 0s where appropriate, apply logic checks
    Year = lubridate::year(yearmon),
    Month = lubridate::month(yearmon),
    pre_2016 = Year < 2016,
    missing_total = Total_re_ad==0|is.na(Total_re_ad),
    ANC_re_ad = ifelse(is.na(ANC_re_ad),0,ANC_re_ad),
    Before_12wk = ifelse(is.na(Before_12wk),0,Before_12wk),
    After_12wk = ifelse(is.na(After_12wk),0,After_12wk),
    ANC_test = ifelse(is.na(ANC_test)&(is.na(ANC_pos)|ANC_pos==0),0,ANC_test),
    Hb_test = ifelse(is.na(Hb_test)&(is.na(Anaemia)|Anaemia==0),0,Hb_test),
    ANC_pos = ifelse(!is.na(ANC_test)&is.na(ANC_pos),0,ANC_pos),
    llin_provided = ifelse(is.na(llin_provided),0,llin_provided),
    ipt2_provided = ifelse(is.na(ipt2_provided),0,ipt2_provided),
    ipt3_provided = ifelse(is.na(ipt3_provided),0,ipt3_provided),
    ipt4_provided = ifelse(is.na(ipt4_provided),0,ipt4_provided),
    Anaemia = ifelse(!is.na(Hb_test)&is.na(Anaemia),0,Anaemia),
    region = ifelse(Region=='Dar Es Salaam Region','DAR',gsub(' Region','',Region)),
    Total_re_ad = ifelse(missing_total,Before_12wk+After_12wk+ANC_re_ad,Total_re_ad),
    total_first_att = Before_12wk+After_12wk,
    total_first_att_bysub = Total_re_ad-ANC_re_ad,
    not_all_tested = ANC_test<total_first_att,
    illogical_posGEtest = ANC_pos>ANC_test,
    illogical_testGEattend = ANC_test>total_first_att,
    illogical_llinGEattend = llin_provided>total_first_att,
    illogical_anemiaGEtesthb = ifelse(pre_2016,
                                      NA,
                                      Anaemia>Hb_test),
    illogical_iptGEattend = ifelse(pre_2016,
                                   NA,
                                   (ipt2_provided+ipt3_provided+ipt4_provided)>ANC_re_ad),
    illogical_totalattend = ifelse(pre_2016,
                                   NA,
                                   total_first_att!=total_first_att_bysub),
    any_non_missing_nonzero = rowSums(!is.na(pick(all_of(results_cols))) &
                                        pick(all_of(results_cols)) != 0) > 0,
    illogical_noattresults = ifelse(pre_2016,
                                    (total_first_att==0|is.na(total_first_att))&any_non_missing_nonzero,
                                    missing_total&any_non_missing_nonzero),
    any_data_available = rowSums(!is.na(pick(all_of(main_results_cols))) &
                                   pick(all_of(main_results_cols)) != 0) > 0,
    data_available = ifelse(pre_2016,
                            total_first_att!=0,
                            Total_re_ad!=0),
    first_att_available = !(total_first_att==0|is.na(total_first_att)),
    anc_test_available = !(ANC_test==0|is.na(ANC_test)),
    hb_test_available = !(Hb_test==0|is.na(Hb_test)),
    error_counts = ifelse(pre_2016,
                          not_all_tested+illogical_posGEtest+illogical_testGEattend,
                          not_all_tested+illogical_posGEtest+illogical_testGEattend+illogical_totalattend),
    sum_counts = ifelse(pre_2016,
                        Before_12wk+After_12wk+ANC_test+ANC_pos+Hb_test+Anaemia+llin_provided+ipt2_provided,
                        Before_12wk+After_12wk+ANC_re_ad+ANC_test+ANC_pos+Hb_test+Anaemia+llin_provided+ipt2_provided),
    date_NA =  as.yearmon(ifelse(!any_data_available,NA,yearmon)),
    month_prevalence = ifelse(ANC_test>0,ANC_pos/ANC_test,NA))%>%
  arrange(Region,Council,HF,yearmon)
##Summarise by HF to find outliers
##Get monthly number of HF reporting
##Get monthly number of new HF reporting
##Get proportion of health facilities with inconsistent reporting

dqa_hf_remove_missing <- cleaned_dqametrics%>%
  group_by(Region,Council,HF)%>%
  mutate(
    all_missed = all(!any_data_available))%>% #Identify HFs with no data reported across study period
  filter(!all_missed)%>% #Remove said HFs
  ungroup()
dqa_hf_remove_duplicates <- dqa_hf_remove_missing%>%
  group_by(Region,Council,HF,yearmon)%>%
  mutate(order = row_number(),
         min_error_counts = min(error_counts),
         keep_less_error = error_counts==min_error_counts)%>%
  filter(keep_less_error)%>% #Keep entry with least errors
  ungroup()%>%
  group_by(Region,Council,HF,yearmon)%>%
  mutate(order = row_number(),
         max_counts = max(sum_counts),
         keep=sum_counts==max_counts)%>%
  filter(keep)%>% #Keep HF rows with most data
  select(-c(keep,order,sum_counts,max_counts,error_counts,min_error_counts,keep_less_error))%>%
  ungroup()
saveRDS(dqa_hf_remove_duplicates,'dqa_hf_remove_duplicates.rds')

dqa_hf <- dqa_hf_remove_duplicates %>%
  group_by(Region,Council,HF)%>%
  summarise(
    start_data_collection = as.yearmon(suppressWarnings(min(date_NA,na.rm=TRUE))),
    end_data_collection = as.yearmon(suppressWarnings(max(date_NA,na.rm=TRUE))),
    expected_total_months = (end_data_collection-start_data_collection)*12+1,
    num_months_with_data = sum(any_data_available),
    num_months_with_firstatt = sum(first_att_available),
    num_months_with_test = sum(anc_test_available),
    num_months_with_hbtest = sum(hb_test_available),
    ANC_pos_sum = sum(ANC_pos,na.rm=TRUE),
    ANC_test_sum = sum(ANC_test,na.rm=TRUE),
    ANC_test_sum_nonmissing = sum(ANC_test[!is.na(ANC_pos)], na.rm = TRUE),
    ANC_red_ad_sum = sum(ANC_re_ad,na.rm=TRUE),
    total_first_att_sum = sum(total_first_att,na.rm=TRUE),
    Before_12wk_sum = sum(Before_12wk,na.rm = TRUE),
    After_12wk_sum = sum(After_12wk,na.rm = TRUE),
    sum_llin_provided = sum(llin_provided,na.rm=TRUE),
    sum_ipt2_provided = sum(ipt2_provided,na.rm=TRUE),
    sum_ipt3_provided = sum(ipt3_provided,na.rm=TRUE),
    sum_ipt4_provided = sum(ipt4_provided,na.rm=TRUE),
    sum_Hb_test = sum(Hb_test,na.rm=TRUE),
    sum_Anaemia = sum(Anaemia,na.rm=TRUE),
    mean_positive = mean(ANC_pos[yearmon >=start_data_collection &yearmon <= end_data_collection], na.rm = TRUE),
    mean_tested = mean(ANC_test[any_data_available], na.rm = TRUE),
    mean_tested_nonmissing = mean(ANC_test[!is.na(ANC_pos)&any_data_available], na.rm = TRUE),
    mean_total = mean(total_first_att[first_att_available==1], na.rm = TRUE),
    mean_prev = ifelse(ANC_test_sum_nonmissing>0,ANC_pos_sum/ANC_test_sum_nonmissing,NA),
    mean_re_ad = mean(ANC_re_ad[any_data_available],na.rm = TRUE),
    mean_Before_12wk = mean(Before_12wk[any_data_available],na.rm=TRUE),
    mean_After_12wk = mean(After_12wk[any_data_available],na.rm=TRUE),
    mean_llin_provided = mean(llin_provided[any_data_available],na.rm=TRUE),
    mean_ipt2_provided = mean(ipt2_provided[any_data_available],na.rm=TRUE),
    mean_ipt3_provided = mean(ipt3_provided[any_data_available],na.rm=TRUE),
    mean_ipt4_provided = mean(ipt4_provided[any_data_available],na.rm=TRUE),
    mean_Hb_test = mean(Hb_test[any_data_available],na.rm=TRUE),
    mean_Anaemia = mean(Anaemia[any_data_available],na.rm=TRUE),
    sd_total = sd(total_first_att[any_data_available],na.rm=TRUE),
    sd_tested = sd(ANC_test[any_data_available],na.rm=TRUE),
    sd_tested_nonmissing = sd(ANC_test[!is.na(ANC_pos)&any_data_available],na.rm=TRUE),
    sd_positive = sd(ANC_pos[any_data_available],na.rm=TRUE),
    sd_prev = sd(month_prevalence[any_data_available],na.rm=TRUE),
    sd_re_ad = sd(ANC_re_ad[any_data_available],na.rm=TRUE),
    sd_Before_12wk = sd(Before_12wk[any_data_available],na.rm=TRUE),
    sd_After_12wk = sd(After_12wk[any_data_available],na.rm=TRUE),
    sd_llin_provided = sd(llin_provided[any_data_available],na.rm=TRUE),
    sd_ipt2_provided = sd(ipt2_provided[any_data_available],na.rm=TRUE),
    sd_ipt3_provided = sd(ipt3_provided[any_data_available],na.rm=TRUE),
    sd_ipt4_provided = sd(ipt4_provided[any_data_available],na.rm=TRUE),
    sd_Hb_test = sd(Hb_test[any_data_available],na.rm=TRUE),
    sd_Anaemia = sd(Anaemia[any_data_available],na.rm=TRUE),
    # total_outlier = ifelse(any_data_available,total_first_att >= mean_total+3*sd_total | total_first_att <= mean_total-3*sd_total,NA),
    # tested_outlier = ifelse(any_data_available,ANC_test >= mean_tested + 3*sd_tested | ANC_test <= mean_tested - 3*sd_tested,NA),
    # positive_outlier = ifelse(any_data_available,ANC_pos >= mean_positive + 3*sd_positive | ANC_pos <= mean_positive - 3*sd_positive,NA),
    # prev_outlier = ifelse(any_data_available,month_prevalence >= mean_prev + 3*sd_prev | month_prevalence <= mean_prev - 3*sd_prev,NA),
    # re_ad_outlier = ifelse(any_data_available,ANC_re_ad >= mean_re_ad + 3*sd_re_ad | ANC_re_ad <= mean_re_ad - 3*sd_re_ad,NA),
    # Before_12wk_outlier = ifelse(any_data_available,Before_12wk >= mean_Before_12wk + 3*sd_Before_12wk | Before_12wk <= mean_Before_12wk - 3*sd_Before_12wk,NA),
    # After_12wk_outlier = ifelse(any_data_available,After_12wk >= mean_After_12wk + 3*sd_After_12wk | After_12wk <= mean_After_12wk - 3*sd_After_12wk,NA),
    # llin_provided_outlier = ifelse(any_data_available,llin_provided >= mean_llin_provided + 3*sd_llin_provided | llin_provided <= mean_llin_provided - 3*sd_llin_provided,NA),
    # ipt2_provided_outlier = ifelse(any_data_available,ipt2_provided >= mean_ipt2_provided + 3*sd_ipt2_provided | ipt2_provided <= mean_ipt2_provided - 3*sd_ipt2_provided,NA),
    # ipt3_provided_outlier = ifelse(any_data_available,ipt3_provided >= mean_ipt3_provided + 3*sd_ipt3_provided | ipt3_provided <= mean_ipt3_provided - 3*sd_ipt3_provided,NA),
    # ipt4_provided_outlier = ifelse(any_data_available,ipt4_provided >= mean_ipt4_provided + 3*sd_ipt4_provided | ipt4_provided <= mean_ipt4_provided - 3*sd_ipt4_provided,NA),
    # Hb_test_outlier = ifelse(any_data_available,Hb_test >= mean_Hb_test + 3*sd_Hb_test | Hb_test <= mean_Hb_test - 3*sd_Hb_test,NA),
    # Anaemia_outlier = ifelse(any_data_available,Anaemia >= mean_Anaemia + 3*sd_Anaemia | Anaemia <= mean_Anaemia - 3*sd_Anaemia,NA),
    # sum_total_outlier = sum(total_outlier,na.rm = TRUE),
    # sum_tested_outlier = sum(tested_outlier,na.rm = TRUE),
    # sum_positive_outlier = sum(positive_outlier,na.rm = TRUE),
    # sum_prev_outlier = sum(prev_outlier,na.rm = TRUE),
    # sum_re_ad_outlier = sum(re_ad_outlier,na.rm = TRUE),
    # sum_Before_12wk_outlier = sum(Before_12wk_outlier,na.rm = TRUE),
    # sum_After_12wk_outlier= sum(After_12wk_outlier,na.rm = TRUE),
    # sum_llin_provided_outlier= sum(llin_provided_outlier,na.rm = TRUE),
    # sum_ipt2_provided_outlier= sum(ipt2_provided_outlier,na.rm = TRUE),
    # sum_ipt3_provided_outlier= sum(ipt3_provided_outlier,na.rm = TRUE),
    # sum_ipt4_provided_outlier= sum(ipt4_provided_outlier,na.rm = TRUE),
    # sum_Hb_test_outlier= sum(Hb_test_outlier,na.rm = TRUE),
    # sum_Anaemia_outlier= sum(Anaemia_outlier,na.rm = TRUE),
    sum_not_all_tested = sum(not_all_tested,na.rm = TRUE),
    sum_illogical_posGEtest = sum(illogical_posGEtest,na.rm = TRUE),
    sum_illogical_testGEattend = sum(illogical_testGEattend,na.rm = TRUE),
    sum_illogical_llinGEattend = sum(illogical_llinGEattend,na.rm = TRUE),
    sum_illogical_anemiaGEtesthb = sum(illogical_anemiaGEtesthb,na.rm = TRUE),
    sum_illogical_iptGEattend = sum(illogical_iptGEattend,na.rm = TRUE),
    sum_illogical_totalattend = sum(illogical_totalattend,na.rm = TRUE),
    sum_illogical_noattresults = sum(illogical_noattresults,na.rm = TRUE),
    sum_illogical_posGEtest = sum(illogical_posGEtest,na.rm = TRUE)
  )%>%
  mutate(proportion_months_with_data = num_months_with_data/expected_total_months,
         proportion_months_flag = proportion_months_with_data>1.0)%>%
  mutate(across(c('num_months_with_data','num_months_with_test','num_months_with_hbtest'), ~ ./expected_total_months, .names = '{.col}_prop'))%>%
  mutate(across(sum_not_all_tested:sum_illogical_posGEtest, ~ ./num_months_with_data, .names = '{.col}_prop'))%>%
  ungroup()

# skimr::skim(dqa_hf)

# dqa_hf_start_date <- dqa_hf%>%
#   group_by(start_data_collection)%>%
#   summarise(count_start=n())%>%
#   ungroup()
#
# dqa_hf_end_date <- dqa_hf%>%
#   group_by(end_data_collection)%>%
#   summarise(count_end=n())
#
# dqa_hf_end_date_expected_reporting <- left_join(dqa_hf_start_date,dqa_hf_end_date,by=join_by(start_data_collection==end_data_collection))%>%
#   arrange(start_data_collection)%>%
#   mutate(count_end = lag(count_end))%>%
#   mutate(count_end = ifelse(is.na(count_end),0,count_end))%>%
#   mutate(count_diff = count_start-count_end,
#          cumulative = cumsum(count_diff))

dqa_df_total_monthly <- dqa_hf_remove_duplicates%>%
  group_by(yearmon)%>%
  summarise(num_hf_with_data = sum(any_data_available,na.rm = TRUE),
            num_hf_with_firstatt = sum(first_att_available,na.rm = TRUE),
            num_hf_with_test = sum(anc_test_available,na.rm = TRUE),
            num_hf_with_hbtest = sum(hb_test_available,na.rm = TRUE),
            ANC_pos_sum=sum(ANC_pos,na.rm = TRUE),
            ANC_test_sum=sum(ANC_test,na.rm = TRUE),
            ANC_test_sum_nonmissing = sum(ANC_test[!is.na(ANC_pos)], na.rm = TRUE),
            prevalence = ANC_pos_sum/ANC_test_sum_nonmissing,
            Before_12wk_sum=sum(Before_12wk,na.rm = TRUE),
            After_12wk_sum=sum(After_12wk,na.rm = TRUE),
            total_first_att_sum=sum(total_first_att,na.rm = TRUE),
            ANC_re_ad_sum = sum(ANC_re_ad,na.rm=TRUE),
            sum_llin_provided = sum(llin_provided,na.rm=TRUE),
            sum_ipt2_provided = sum(ipt2_provided,na.rm=TRUE),
            sum_ipt3_provided = sum(ipt3_provided,na.rm=TRUE),
            sum_ipt4_provided = sum(ipt4_provided,na.rm=TRUE),
            sum_Hb_test = sum(Hb_test,na.rm=TRUE),
            sum_Anaemia = sum(Anaemia,na.rm=TRUE),
            # sum_total_outlier = sum(total_outlier,na.rm = TRUE),
            # sum_tested_outlier = sum(tested_outlier,na.rm = TRUE),
            # sum_positive_outlier = sum(positive_outlier,na.rm = TRUE),
            # sum_prev_outlier = sum(prev_outlier,na.rm = TRUE),
            # sum_re_ad_outlier = sum(re_ad_outlier,na.rm = TRUE),
            # sum_Before_12wk_outlier = sum(Before_12wk_outlier,na.rm = TRUE),
            # sum_After_12wk_outlier= sum(After_12wk_outlier,na.rm = TRUE),
            # sum_llin_provided_outlier= sum(llin_provided_outlier,na.rm = TRUE),
            # sum_ipt2_provided_outlier= sum(ipt2_provided_outlier,na.rm = TRUE),
            # sum_ipt3_provided_outlier= sum(ipt3_provided_outlier,na.rm = TRUE),
            # sum_ipt4_provided_outlier= sum(ipt4_provided_outlier,na.rm = TRUE),
            # sum_Hb_test_outlier= sum(Hb_test_outlier,na.rm = TRUE),
            # sum_Anaemia_outlier= sum(Anaemia_outlier,na.rm = TRUE),
            sum_not_all_tested = sum(not_all_tested,na.rm = TRUE),
            sum_illogical_posGEtest = sum(illogical_posGEtest,na.rm = TRUE),
            sum_illogical_testGEattend = sum(illogical_testGEattend,na.rm = TRUE),
            sum_illogical_llinGEattend = sum(illogical_llinGEattend,na.rm = TRUE),
            sum_illogical_anemiaGEtesthb = sum(illogical_anemiaGEtesthb,na.rm = TRUE),
            sum_illogical_iptGEattend = sum(illogical_iptGEattend,na.rm = TRUE),
            sum_illogical_totalattend = sum(illogical_totalattend,na.rm = TRUE),
            sum_illogical_noattresults = sum(illogical_noattresults,na.rm = TRUE),
            sum_illogical_posGEtest = sum(illogical_posGEtest,na.rm = TRUE)
  )%>%
  mutate(pre_2016 = year(yearmon)<2016,
         ANC_re_ad_sum=ifelse(pre_2016,NA,ANC_re_ad_sum),
         sum_ipt4_provided=ifelse(pre_2016,NA,sum_ipt4_provided),
         sum_Hb_test=ifelse(pre_2016,NA,sum_Hb_test),
         sum_Anaemia=ifelse(pre_2016,NA,sum_Anaemia),
         # sum_re_ad_outlier=ifelse(pre_2016,NA,sum_re_ad_outlier),
         # sum_ipt4_provided_outlier=ifelse(pre_2016,NA,sum_ipt4_provided_outlier),
         # sum_Hb_test_outlier=ifelse(pre_2016,NA,sum_Hb_test_outlier),
         # sum_Anaemia_outlier=ifelse(pre_2016,NA,sum_Anaemia_outlier),
         sum_illogical_anemiaGEtesthb=ifelse(pre_2016,NA,sum_illogical_anemiaGEtesthb),
         sum_illogical_iptGEattend=ifelse(pre_2016,NA,sum_illogical_iptGEattend),
         sum_illogical_totalattend=ifelse(pre_2016,NA,sum_illogical_totalattend)
  )%>%
  mutate(across(c('num_hf_with_firstatt','num_hf_with_test','num_hf_with_hbtest'), ~ ./num_hf_with_data, .names = '{.col}_prop'))%>%
  mutate(across(sum_not_all_tested:sum_illogical_noattresults, ~ ./num_hf_with_data, .names = '{.col}_prop'))%>%
  ungroup()

##Subset for reporting rates for plotting by month
subset_reporting_long <- dqa_df_total_monthly%>%
  select(yearmon,num_hf_with_data:num_hf_with_test)%>%
  tidyr::pivot_longer(-c(yearmon))%>%
  mutate(name=factor(name,levels=c('num_hf_with_data','num_hf_with_firstatt','num_hf_with_test'),labels=c('Any data','Any ANC1 attendance','Any malaria tests')))
hf_data_count <- ggplot(subset_reporting_long)+
  geom_line(aes(x=as.Date(yearmon),y=value,color=name))+
  scale_color_brewer(palette='Dark2')+
  scale_x_date(date_breaks='year',date_labels="'%y")+
  scale_y_continuous(limits=c(0,(max(subset_reporting_long$value)+500)),expand = c(0,0))+
  labs(y='Number of health facilities reporting')+
  theme(axis.title.x = element_blank(),
        legend.title = element_blank())
ggsave(hf_data_count,file='hf_count.png',units='in',height=4,width = 5)

##Subset for attendance rates for plotting by month
subset_attendance_long <- dqa_df_total_monthly%>%
  select(yearmon,total_first_att_sum,ANC_re_ad_sum)%>%
  tidyr::pivot_longer(-c(yearmon))%>%
  mutate(name=factor(name,levels=c('total_first_att_sum','ANC_re_ad_sum'),labels=c('ANC1','ANC2+')))

hf_att_count <- ggplot(subset_attendance_long)+
  geom_line(aes(x=as.Date(yearmon),y=value/10000,color=name))+
  scale_color_brewer(palette='Dark2')+
  scale_x_date(date_breaks='year',date_labels="'%y")+
  scale_y_continuous(limits=c(0,(max(subset_attendance_long$value,na.rm=TRUE)/10000+5)),expand = c(0,0))+
  labs(y='Number of Women (x10,000)')+
  theme(axis.title.x = element_blank(),
        legend.title = element_blank())
ggsave(hf_att_count,file='hf_attendance.png',units='in',height=4,width = 5)

##Subset for intervention distribution rates for plotting by month
subset_int_long <- dqa_df_total_monthly%>%
  select(yearmon,sum_llin_provided:sum_ipt4_provided)%>%
  tidyr::pivot_longer(-c(yearmon))%>%
  mutate(name=factor(name,levels=c('sum_llin_provided','sum_ipt2_provided','sum_ipt3_provided','sum_ipt4_provided'),labels=c('LLIN','IPTp2','IPTp3','IPTp4')))
hf_int_count <- ggplot(subset_int_long)+
  geom_line(aes(x=as.Date(yearmon),y=value/10000,color=name))+
  scale_color_brewer(palette='Dark2')+
  scale_x_date(date_breaks='year',date_labels="'%y")+
  scale_y_continuous(limits=c(0,(max(subset_int_long$value,na.rm=TRUE)/10000+5)),expand = c(0,0))+
  labs(y='Number Nets/Doses Provided (x10,000)')+
  theme(axis.title.x = element_blank(),
        legend.title = element_blank())
ggsave(hf_int_count,file='hf_intervention.png',units='in',height=4,width = 5)

##Subset for proportion of health facilities testing all women for plotting by month
subset_illogical_prop_long <- dqa_df_total_monthly%>%
  select(yearmon,sum_not_all_tested_prop)%>%
  mutate(all_tested_prop = 1-sum_not_all_tested_prop)

hf_alltested_count <- ggplot(subset_illogical_prop_long)+
  geom_line(aes(x=as.Date(yearmon),y=all_tested_prop),color=RColorBrewer::brewer.pal(n=1,name='Dark2')[1])+
  geom_hline(yintercept = 0.9,linetype='dashed',color='darkgrey')+
  scale_x_date(date_breaks='year',date_labels="'%y")+
  scale_y_continuous(breaks=c(0,0.25,0.5,0.75,0.9,1.0),limits=c(0,1),expand = c(0,0))+
  labs(y='Proportion of Health Facilities\nTesting All Women at ANC1')+
  theme(axis.title.x = element_blank(),
        legend.title = element_blank())
ggsave(hf_alltested_count,file='hf_alltested.png',units='in',height=4,width = 5)


##Sumarise by year and council
dqa_council_summ <- dqa_hf_remove_duplicates%>%
  group_by(Region,Council,Year)%>%
  summarise(num_hf_with_data = sum(any_data_available,na.rm = TRUE),
            num_hf_with_firstatt = sum(first_att_available,na.rm = TRUE),
            num_hf_with_test = sum(anc_test_available,na.rm = TRUE),
            num_hf_with_hbtest = sum(hb_test_available,na.rm = TRUE),
            ANC_pos_sum=sum(ANC_pos,na.rm = TRUE),
            ANC_test_sum=sum(ANC_test,na.rm = TRUE),
            ANC_test_sum_nonmissing = sum(ANC_test[!is.na(ANC_pos)], na.rm = TRUE),
            prevalence = ANC_pos_sum/ANC_test_sum_nonmissing,
            Before_12wk_sum=sum(Before_12wk,na.rm = TRUE),
            After_12wk_sum=sum(After_12wk,na.rm = TRUE),
            total_first_att_sum=sum(total_first_att,na.rm = TRUE),
            ANC_re_ad_sum = sum(ANC_re_ad,na.rm=TRUE),
            sum_llin_provided = sum(llin_provided,na.rm=TRUE),
            sum_ipt2_provided = sum(ipt2_provided,na.rm=TRUE),
            sum_ipt3_provided = sum(ipt3_provided,na.rm=TRUE),
            sum_ipt4_provided = sum(ipt4_provided,na.rm=TRUE),
            sum_Hb_test = sum(Hb_test,na.rm=TRUE),
            sum_Anaemia = sum(Anaemia,na.rm=TRUE),
            # sum_total_outlier = sum(total_outlier,na.rm = TRUE),
            # sum_tested_outlier = sum(tested_outlier,na.rm = TRUE),
            # sum_positive_outlier = sum(positive_outlier,na.rm = TRUE),
            # sum_prev_outlier = sum(prev_outlier,na.rm = TRUE),
            # sum_re_ad_outlier = sum(re_ad_outlier,na.rm = TRUE),
            # sum_Before_12wk_outlier = sum(Before_12wk_outlier,na.rm = TRUE),
            # sum_After_12wk_outlier= sum(After_12wk_outlier,na.rm = TRUE),
            # sum_llin_provided_outlier= sum(llin_provided_outlier,na.rm = TRUE),
            # sum_ipt2_provided_outlier= sum(ipt2_provided_outlier,na.rm = TRUE),
            # sum_ipt3_provided_outlier= sum(ipt3_provided_outlier,na.rm = TRUE),
            # sum_ipt4_provided_outlier= sum(ipt4_provided_outlier,na.rm = TRUE),
            # sum_Hb_test_outlier= sum(Hb_test_outlier,na.rm = TRUE),
            # sum_Anaemia_outlier= sum(Anaemia_outlier,na.rm = TRUE),
            sum_not_all_tested = sum(not_all_tested,na.rm = TRUE),
            sum_illogical_posGEtest = sum(illogical_posGEtest,na.rm = TRUE),
            sum_illogical_testGEattend = sum(illogical_testGEattend,na.rm = TRUE),
            sum_illogical_llinGEattend = sum(illogical_llinGEattend,na.rm = TRUE),
            sum_illogical_anemiaGEtesthb = sum(illogical_anemiaGEtesthb,na.rm = TRUE),
            sum_illogical_iptGEattend = sum(illogical_iptGEattend,na.rm = TRUE),
            sum_illogical_totalattend = sum(illogical_totalattend,na.rm = TRUE),
            sum_illogical_noattresults = sum(illogical_noattresults,na.rm = TRUE)
  )%>%
  mutate(across(c('num_hf_with_firstatt','num_hf_with_test','num_hf_with_hbtest'), ~ ./num_hf_with_data, .names = '{.col}_prop'))%>%
  mutate(across(sum_not_all_tested:sum_illogical_noattresults, ~ ./num_hf_with_data, .names = '{.col}_prop'))%>%
  mutate(all_tested_prop = 1-sum_not_all_tested_prop)%>%
  ungroup()

##May by council
# Shape files -----
councils <- st_read("District edited Jan 2021/District edited Jan 2021.shp") %>%
  rename(Council='DHIS2_Dist')
dqa_council_summ4map <- merge(councils,dqa_council_summ,by='Council')%>%
  filter(Year %% 2 == 0)

prevalence_council <- ggplot()+
  geom_sf(data = dqa_council_summ4map, aes(fill=prevalence))+ # Regions layer
  # geom_sf(data = water_bodies_sf, color = "lightblue",fill="lightblue")+ # Overlay colored water bodies
  #north(data = tz1 ,location = "topleft", scale = .1,symbol = 5)+ # North direction
  # scalebar(data = tz1,dist = 150,transform = TRUE,model = "WGS84", # Scale bar
  #          height = .03,dist_unit = "km",location = "bottomleft", nudge_x = 0.5)+# Remove coordinates and boundaries
  scale_fill_distiller(direction=1,palette = 'YlOrRd')+
  facet_wrap(.~Year)+
  labs(title='ANC1 Malaria Prevalence',
       fill='Prevalence')+
  theme(legend.position = "bottom",
        legend.key.width= unit(1, "cm"),
        legend.key.height = unit(0.5,'cm'),
        legend.text = element_text(size=10),
        legend.title = element_text(size=12),
        axis.text.x = element_blank(),
        axis.text.y = element_blank())
ggsave(prevalence_council,file='council_prevalence_map.png',units='in',height=4,width = 10)
saveRDS(dqa_council_summ4map,'dqa_council_summ4map.rds')

alltested_council <- ggplot()+
  geom_sf(data = dqa_council_summ4map, aes(fill=all_tested_prop))+ # Regions layer
  # geom_sf(data = water_bodies_sf, color = "lightblue",fill="lightblue")+ # Overlay colored water bodies
  #north(data = tz1 ,location = "topleft", scale = .1,symbol = 5)+ # North direction
  # scalebar(data = tz1,dist = 150,transform = TRUE,model = "WGS84", # Scale bar
  #          height = .03,dist_unit = "km",location = "bottomleft", nudge_x = 0.5)+# Remove coordinates and boundaries
  scale_fill_distiller(direction=-1,palette = 'YlOrRd')+
  facet_wrap(.~Year)+
  labs(fill='Proportion Health Facilities that\ntest all women at ANC1')+
  theme(legend.position = "bottom",
        legend.key.width= unit(1, "cm"),
        legend.key.height = unit(0.5,'cm'),
        legend.text = element_text(size=10),
        legend.title = element_text(size=12),
        axis.text.x = element_blank(),
        axis.text.y = element_blank())
ggsave(alltested_council,file='alltested_council_map.png',units='in',height=4,width = 10)
#
# # Custom shape files ----
# water_bodies_sf <- tz1 %>% filter(!type_1=="Region") # Filter water bodies
# centr_point <- cbind(st_point_on_surface(tz1), st_coordinates(st_point_on_surface(tz1)))
# wb_centr_point <- cbind(st_point_on_surface(water_bodies_sf),st_coordinates(st_point_on_surface(water_bodies_sf)))
#
# dqa_region_summ$name_1<- as.character(dqa_region_summ$region)
# dqa_region_summ$name_1[dqa_region_summ$name_1=="Dar Es Salaam"]<-"DAR"
# dqa_region_summ4map<- left_join(tz1,dqa_region_summ, by='name_1',multiple='all')
#
# ##Get number of HF that start then stop collecting data
#
# table(dqa_region_filter$start_data_collection)
# skimr::skim(dqa_region)
# dqa_region%>%
#   group_by(HF)
# mean(dqa_region$no_data)
# # %>%
#   # filter((ANC_pos<=ANC_test))%>%
#   group_by(yearmon,Region,region)%>%
#   dplyr::summarise(count=n(),
#                    positive=sum(ANC_pos, na.rm = TRUE),
#                    tested=sum(ANC_test, na.rm=TRUE),
#                    total=sum(total_first_att, na.rm = TRUE),
#                    not_all_tested = )%>%
#   ungroup()%>%
#   right_join(combinations,by=join_by(Region,yearmon))%>%
#   group_by(year(yearmon))%>%
#   mutate(nat_positive = sum(positive, na.rm = TRUE),
#          nat_tested = sum(tested, na.rm = TRUE),
#          nat_total = sum(total, na.rm = TRUE))%>%
#   ungroup()%>%
#   group_by(yearmon)%>%
#   mutate(nat_ratio_positive = sum(positive, na.rm = TRUE)/nat_positive,
#          nat_ratio_tested = sum(tested, na.rm = TRUE)/nat_tested,
#          nat_ratio_total = sum(total, na.rm = TRUE)/nat_total)%>%
#   ungroup()%>%
#   group_by(region)%>%
#   mutate(positive_0 = ifelse(positive==0,1,0),
#          tested_0 = ifelse(tested==0,1,0),
#          total_0 = ifelse(total==0,1,0),
#          positive_na = ifelse(is.na(positive),1,0),
#          tested_na = ifelse(is.na(tested),1,0),
#          total_na = ifelse(is.na(total)==0,1,0),
#          pos_gt_tested = ifelse(positive>tested,1,0),
#          test_gt_total = ifelse(tested>total,1,0),
#          median_total = median(total, na.rm = TRUE),
#          M_total = 0.6745*(total-median(total, na.rm = TRUE))/median(abs(total-median(total, na.rm = TRUE)), na.rm = TRUE),
#          M_tested = 0.6745*(tested-median(tested, na.rm = TRUE))/median(abs(tested-median(tested, na.rm = TRUE)), na.rm = TRUE),
#          M_positive = 0.6745*(positive-median(positive, na.rm = TRUE))/median(abs(positive-median(positive, na.rm = TRUE)), na.rm = TRUE),
#          total_outlier = ifelse(abs(M_total)>3.5,1,0),
#          tested_outlier = ifelse(abs(M_tested)>3.5,1,0),
#          positive_outlier = ifelse(abs(M_positive)>3.5,1,0))%>%
#   ungroup()%>%
#   group_by(year(yearmon),region)%>%
#   mutate(reg_ratio_positive = positive/sum(positive, na.rm = TRUE),
#          reg_ratio_tested = tested/sum(tested, na.rm = TRUE),
#          reg_ratio_total = total/sum(total, na.rm = TRUE),
#          dif_ratio_positive = (reg_ratio_positive-nat_ratio_positive)/nat_ratio_positive,
#          dif_ratio_tested = (reg_ratio_tested-nat_ratio_tested)/nat_ratio_tested,
#          dif_ratio_total = (reg_ratio_total-nat_ratio_total)/nat_ratio_total,
#          is_dif_positive = ifelse(abs(dif_ratio_positive)>=1/3,1,0),
#          is_dif_tested = ifelse(abs(dif_ratio_tested)>=1/3,1,0),
#          is_dif_total = ifelse(abs(dif_ratio_total)>=1/3,1,0))
#
# ggplot(dqa_region)+
#   geom_line(aes(x=as.Date(yearmon),y=M_total),color='#1F78B4')+
#   geom_hline(yintercept=c(-3.5,3.5),color='#666666')+
#   facet_geo(~ region, grid = province_grid%>%
#               select(row,col,code,name))+
#   scale_x_date(date_labels = "'%y")+
#   labs(x='Month',y='Modified Z-Score - Total Attendance')
#
#
# ggplot(dqa_region)+
#   geom_line(aes(x=as.Date(yearmon),y=M_tested),color='#1F78B4')+
#   geom_hline(yintercept=c(-3.5,3.5),color='#666666')+
#   facet_geo(~ region, grid = province_grid%>%
#               select(row,col,code,name))+
#   scale_x_date(date_labels = "'%y")+
#   labs(x='Month',y='Modified Z-Score - Total Tested')
#
# ggplot(dqa_region)+
#   geom_line(aes(x=as.Date(yearmon),y=M_positive),color='#1F78B4')+
#   geom_hline(yintercept=c(-3.5,3.5),color='#666666')+
#   facet_geo(~ region, grid = province_grid%>%
#               select(row,col,code,name))+
#   scale_x_date(date_labels = "'%y")+
#   labs(x='Month',y='Modified Z-Score - Total Positive')+
#   coord_cartesian(ylim = c(-6,6))
#
# ggplot(dqa_region)+
#   geom_line(aes(x=as.Date(yearmon),y=reg_ratio_total),color='#1F78B4')+
#   facet_wrap(.~Region)+
#   labs(x='Month',y='Ratio this month to this year - Total Attendance')
# ggplot(dqa_region)+
#   geom_line(aes(x=yearmon,y=reg_ratio_tested),color='#1F78B4')+
#   facet_wrap(.~Region)+
#   labs(x='Month',y='Ratio this month to this year - Total Tested')
# ggplot(dqa_region)+
#   geom_line(aes(x=yearmon,y=reg_ratio_positive),color='#1F78B4')+
#   facet_wrap(.~Region)+
#   labs(x='Month',y='Ratio this month to this year - Total Positive')
#
# ggplot(dqa_region)+
#   geom_line(aes(x=yearmon,y=dif_ratio_total),color='#1F78B4')+
#   geom_hline(yintercept=c(-1/3,1/3),color='#666666')+
#   facet_wrap(.~Region)+
#   labs(x='Month',y='Ratio difference with national - Total Attendance')
# ggplot(dqa_region)+
#   geom_line(aes(x=yearmon,y=dif_ratio_tested),color='#1F78B4')+
#   geom_hline(yintercept=c(-1/3,1/3),color='#666666')+
#   facet_wrap(.~Region)+
#   labs(x='Month',y='Ratio difference with national - Total Tested')
# ggplot(dqa_region)+
#   geom_line(aes(x=yearmon,y=dif_ratio_positive),color='#1F78B4')+
#   geom_hline(yintercept=c(-1/3,1/3),color='#666666')+
#   facet_wrap(.~Region)+
#   labs(x='Month',y='Ratio difference with national - Total Positive')
#
#
# dqa_region_summ <- dqa_region%>%
#   group_by(region)%>%
#   summarise(count = n(),
#             positive_0 = mean(positive_0),
#             positive_na = mean(positive_na),
#             positive_outlier = mean(positive_outlier),
#             is_dif_positive = mean(is_dif_positive),
#             tested_0 = mean(tested_0),
#             tested_na = mean(tested_na),
#             tested_outlier = mean(tested_outlier),
#             is_dif_tested = mean(is_dif_tested),
#             total_0 = mean(total_0),
#             total_na = mean(total_na),
#             total_outlier = mean(total_outlier),
#             is_dif_total = mean(is_dif_total),
#             pos_gt_tested = mean(pos_gt_tested),
#             test_gt_total = mean(test_gt_total))
#
# # Shape files -----
# councils <- st_read("C:/Users/jthicks/OneDrive - Imperial College London/Imperial_ResearchAssociate/Frank/Trend analysis Frank/shapefiles/District edited Jan 2021/District edited Jan 2021.shp") # Region sf
# tz_regions <- read_sf("./tanz/tza_admbnda_adm1_20181019/tza_admbnda_adm1_20181019.shp")%>%
#   mutate(region=ifelse(ADM1_EN=='Dar-es-salaam','DAR',ADM1_EN))
# dqa_region_summ4map <- merge(tz_regions,dqa_region_summ,by='region')
# pos_outlier_map <- tm_shape(dqa_region_summ4map) + tm_borders() + tm_fill(col='positive_outlier')+ tmap_options(check.and.fix = TRUE)#,palette = viridisLite::viridis(6, begin = 0.05, end = 0.95),title='Maximum Pearson\nCorrelation Coefficient') + tmap_options(check.and.fix = TRUE)
#
#
# # Custom shape files ----
# water_bodies_sf <- tz1 %>% filter(!type_1=="Region") # Filter water bodies
# centr_point <- cbind(st_point_on_surface(tz1), st_coordinates(st_point_on_surface(tz1)))
# wb_centr_point <- cbind(st_point_on_surface(water_bodies_sf),st_coordinates(st_point_on_surface(water_bodies_sf)))
#
# dqa_region_summ$name_1<- as.character(dqa_region_summ$region)
# dqa_region_summ$name_1[dqa_region_summ$name_1=="Dar Es Salaam"]<-"DAR"
# dqa_region_summ4map<- left_join(tz1,dqa_region_summ, by='name_1',multiple='all')
#
# windows(15,15)
# positive_outlier <- ggplot()+
#   geom_sf(data = dqa_region_summ4map, aes(fill=positive_outlier))+ # Regions layer
#   geom_sf(data = water_bodies_sf, color = "lightblue",fill="lightblue")+ # Overlay colored water bodies
#   #north(data = tz1 ,location = "topleft", scale = .1,symbol = 5)+ # North direction
#   # scalebar(data = tz1,dist = 150,transform = TRUE,model = "WGS84", # Scale bar
#   #          height = .03,dist_unit = "km",location = "bottomleft", nudge_x = 0.5)+# Remove coordinates and boundaries
#   scale_fill_distiller(direction=1,palette = 'YlOrRd')+
#   labs(title='Proportion of months with an outlier for number of positive tests',
#        fill='Proportion')+
#   theme(legend.position = "bottom",
#         legend.key.width= unit(1, "cm"),
#         legend.key.height = unit(0.5,'cm'),
#         legend.text = element_text(size=10),
#         legend.title = element_text(size=12),
#         axis.text.x = element_blank(),
#         axis.text.y = element_blank())
#
# tested_outlier <- ggplot()+
#   geom_sf(data = dqa_region_summ4map, aes(fill=tested_outlier))+ # Regions layer
#   geom_sf(data = water_bodies_sf, color = "lightblue",fill="lightblue")+ # Overlay colored water bodies
#   #north(data = tz1 ,location = "topleft", scale = .1,symbol = 5)+ # North direction
#   # scalebar(data = tz1,dist = 150,transform = TRUE,model = "WGS84", # Scale bar
#   #          height = .03,dist_unit = "km",location = "bottomleft", nudge_x = 0.5)+# Remove coordinates and boundaries
#   scale_fill_distiller(direction=1,palette = 'YlOrRd')+
#   labs(title='Proportion of months with an outlier for number of tests',
#        fill='Proportion')+
#   theme(legend.position = "bottom",
#         legend.key.width= unit(1, "cm"),
#         legend.key.height = unit(0.5,'cm'),
#         legend.text = element_text(size=10),
#         legend.title = element_text(size=12),
#         axis.text.x = element_blank(),
#         axis.text.y = element_blank())
#
# ggplot(dqa_council[dqa_council$Region=='Mbeya Region',])+
#   geom_line(aes(x=yearmon,y=dif_ratio_positive),color='#1F78B4')+
#   geom_hline(yintercept=c(-1/3,1/3),color='#666666')+
#   facet_grid(.~Council)+
#   labs(x='Month',y='Ratio difference with Region - Total Positive')
#
# total_outlier <- ggplot()+
#   geom_sf(data = dqa_region_summ4map, aes(fill=total_outlier))+ # Regions layer
#   geom_sf(data = water_bodies_sf, color = "lightblue",fill="lightblue")+ # Overlay colored water bodies
#   #north(data = tz1 ,location = "topleft", scale = .1,symbol = 5)+ # North direction
#   # scalebar(data = tz1,dist = 150,transform = TRUE,model = "WGS84", # Scale bar
#   #          height = .03,dist_unit = "km",location = "bottomleft", nudge_x = 0.5)+# Remove coordinates and boundaries
#   scale_fill_distiller(direction=1,palette = 'YlOrRd')+
#   labs(title='Proportion of months with an outlier for number of attendance',
#        fill='Proportion')+
#   theme(legend.position = "bottom",
#         legend.key.width= unit(1, "cm"),
#         legend.key.height = unit(0.5,'cm'),
#         legend.text = element_text(size=10),
#         legend.title = element_text(size=12),
#         axis.text.x = element_blank(),
#         axis.text.y = element_blank())
#
# positive_outlier <- ggplot()+
#   geom_sf(data = dqa_region_summ4map, aes(fill=positive_outlier))+ # Regions layer
#   geom_sf(data = water_bodies_sf, color = "lightblue",fill="lightblue")+ # Overlay colored water bodies
#   #north(data = tz1 ,location = "topleft", scale = .1,symbol = 5)+ # North direction
#   # scalebar(data = tz1,dist = 150,transform = TRUE,model = "WGS84", # Scale bar
#   #          height = .03,dist_unit = "km",location = "bottomleft", nudge_x = 0.5)+# Remove coordinates and boundaries
#   scale_fill_distiller(direction=1,palette = 'YlOrRd')+
#   labs(title='Proportion of months with an outlier for number of positive tests',
#        fill='Proportion')+
#   theme(legend.position = "bottom",
#         legend.key.width= unit(1, "cm"),
#         legend.key.height = unit(0.5,'cm'),
#         legend.text = element_text(size=10),
#         legend.title = element_text(size=12),
#         axis.text.x = element_blank(),
#         axis.text.y = element_blank())
#
# tested_outlier <- ggplot()+
#   geom_sf(data = dqa_region_summ4map, aes(fill=tested_outlier))+ # Regions layer
#   geom_sf(data = water_bodies_sf, color = "lightblue",fill="lightblue")+ # Overlay colored water bodies
#   #north(data = tz1 ,location = "topleft", scale = .1,symbol = 5)+ # North direction
#   # scalebar(data = tz1,dist = 150,transform = TRUE,model = "WGS84", # Scale bar
#   #          height = .03,dist_unit = "km",location = "bottomleft", nudge_x = 0.5)+# Remove coordinates and boundaries
#   scale_fill_distiller(direction=1,palette = 'YlOrRd')+
#   labs(title='Proportion of months with an outlier for number of tests',
#        fill='Proportion')+
#   theme(legend.position = "bottom",
#         legend.key.width= unit(1, "cm"),
#         legend.key.height = unit(0.5,'cm'),
#         legend.text = element_text(size=10),
#         legend.title = element_text(size=12),
#         axis.text.x = element_blank(),
#         axis.text.y = element_blank())
#
# total_outlier <- ggplot()+
#   geom_sf(data = dqa_region_summ4map, aes(fill=total_outlier))+ # Regions layer
#   geom_sf(data = water_bodies_sf, color = "lightblue",fill="lightblue")+ # Overlay colored water bodies
#   #north(data = tz1 ,location = "topleft", scale = .1,symbol = 5)+ # North direction
#   # scalebar(data = tz1,dist = 150,transform = TRUE,model = "WGS84", # Scale bar
#   #          height = .03,dist_unit = "km",location = "bottomleft", nudge_x = 0.5)+# Remove coordinates and boundaries
#   scale_fill_distiller(direction=1,palette = 'YlOrRd')+
#   labs(title='Proportion of months with an outlier for number of attendance',
#        fill='Proportion')+
#   theme(legend.position = "bottom",
#         legend.key.width= unit(1, "cm"),
#         legend.key.height = unit(0.5,'cm'),
#         legend.text = element_text(size=10),
#         legend.title = element_text(size=12),
#         axis.text.x = element_blank(),
#         axis.text.y = element_blank())
#
# table(is.na(dqa_region$tested))
# ##############################
# ##Assess DQ at Council level###
# ##############################
# ##For each district, % of month with missing or 0 values##
# get_combo <- Full_data %>%
#   mutate(yearmon=as.yearmon(paste0(Full_data$Year,"-",Full_data$Month),"%Y-%m"))
# combinations_council <- expand.grid(yearmon=sort(unique(get_combo$yearmon)),Council=sort(unique(get_combo$Council)))
#
# dqa_council <- Full_data %>%
#   mutate(yearmon=as.yearmon(paste0(Full_data$Year,"-",Full_data$Month),"%Y-%m"))%>%
#   mutate(ANC_pos = ifelse(ANC_test!=0&is.na(ANC_pos),0,ANC_pos))%>%
#   # filter((ANC_pos<=ANC_test))%>%
#   group_by(yearmon,Council,Region)%>%
#   dplyr::summarise(count=n(),positive=sum(ANC_pos, na.rm = TRUE),tested=sum(ANC_test, na.rm=TRUE),total=sum(Total_re_ad, na.rm = TRUE))%>%
#   ungroup()%>%
#   right_join(combinations_council,by=join_by(Council,yearmon))%>%
#   group_by(year(yearmon),Region)%>% #Calculate total annual counts per region
#   mutate(reg_positive = sum(positive, na.rm = TRUE),
#          reg_tested = sum(tested, na.rm = TRUE),
#          reg_total = sum(total, na.rm = TRUE))%>%
#   ungroup()%>%
#   group_by(yearmon,Region)%>% #Calculate ratio of 1 month to 1 year in region
#   mutate(reg_ratio_positive = sum(positive, na.rm = TRUE)/reg_positive,
#          reg_ratio_tested = sum(tested, na.rm = TRUE)/reg_tested,
#          reg_ratio_total = sum(total, na.rm = TRUE)/reg_total)%>%
#   ungroup()%>%
#   group_by(Council)%>%
#   mutate(positive_0 = ifelse(positive==0,1,0),
#          tested_0 = ifelse(tested==0,1,0),
#          total_0 = ifelse(total==0,1,0),
#          positive_na = ifelse(is.na(positive),1,0),
#          tested_na = ifelse(is.na(tested),1,0),
#          total_na = ifelse(is.na(total)==0,1,0),
#          pos_gt_tested = ifelse(positive>tested,1,0),
#          test_gt_total = ifelse(tested>total,1,0),
#          median_positive = median(positive, na.rm = TRUE),
#          median_tested = median(tested, na.rm = TRUE),
#          median_total = median(total, na.rm = TRUE),
#          M_total = 0.6745*(total-median(total, na.rm = TRUE))/median(abs(total-median(total, na.rm = TRUE)), na.rm = TRUE),
#          M_tested = 0.6745*(tested-median(tested, na.rm = TRUE))/median(abs(tested-median(tested, na.rm = TRUE)), na.rm = TRUE),
#          M_positive = 0.6745*(positive-median(positive, na.rm = TRUE))/median(abs(positive-median(positive, na.rm = TRUE)), na.rm = TRUE),
#          total_outlier = ifelse(abs(M_total)>3.5,1,0),
#          tested_outlier = ifelse(abs(M_tested)>3.5,1,0),
#          positive_outlier = ifelse(abs(M_positive)>3.5,1,0))%>%
#   ungroup()%>%
#   group_by(year(yearmon),Council)%>%
#   #Calculate ratio of 1 month to 1 year in Council
#   mutate(counc_ratio_positive = positive/sum(positive, na.rm = TRUE),
#          counc_ratio_tested = tested/sum(tested, na.rm = TRUE),
#          counc_ratio_total = total/sum(total, na.rm = TRUE),
#          #Calculate % difference between council and regional ratios
#          dif_ratio_positive = (counc_ratio_positive-reg_ratio_positive)/reg_ratio_positive,
#          dif_ratio_tested = (counc_ratio_tested-reg_ratio_tested)/reg_ratio_tested,
#          dif_ratio_total = (counc_ratio_total-reg_ratio_total)/reg_ratio_total,
#          #Is the difference between council and regional ratios greater than 33%?
#          is_dif_positive = ifelse(abs(dif_ratio_positive)>=1/3,1,0),
#          is_dif_tested = ifelse(abs(dif_ratio_tested)>=1/3,1,0),
#          is_dif_total = ifelse(abs(dif_ratio_total)>=1/3,1,0))
#
# dqa_council_summ <- dqa_council%>%
#   group_by(Council)%>%
#   summarise(count = n(),
#             positive_0 = mean(positive_0),
#             positive_na = mean(positive_na),
#             positive_outlier = mean(positive_outlier),
#             is_dif_positive = mean(is_dif_positive),
#             tested_0 = mean(tested_0),
#             tested_na = mean(tested_na),
#             tested_outlier = mean(tested_outlier),
#             is_dif_tested = mean(is_dif_tested),
#             total_0 = mean(total_0),
#             total_na = mean(total_na),
#             total_outlier = mean(total_outlier),
#             is_dif_total = mean(is_dif_total),
#             pos_gt_tested = mean(pos_gt_tested),
#             test_gt_total = mean(test_gt_total))
#
# councils <- st_read("C:/Users/jthicks/OneDrive - Imperial College London/Imperial_ResearchAssociate/Frank/Trend analysis Frank/shapefiles/District edited Jan 2021/District edited Jan 2021.shp") %>%
#   rename(Council='DHIS2_Dist')
# dqa_council_summ4map <- merge(councils,dqa_council_summ,by='Council')
#
#
# arusha <- dqa_council%>%
#   filter(Region=='Arusha Region')
# ggplot(data=arusha)+
#   geom_point(aes(x=yearmon,y=positive/tested))+
#   facet_wrap(.~Council)
# windows(15,15)
# positive_outlier_council <- ggplot()+
#   geom_sf(data = dqa_council_summ4map, aes(fill=positive_outlier))+ # Regions layer
#   geom_sf(data = water_bodies_sf, color = "lightblue",fill="lightblue")+ # Overlay colored water bodies
#   #north(data = tz1 ,location = "topleft", scale = .1,symbol = 5)+ # North direction
#   # scalebar(data = tz1,dist = 150,transform = TRUE,model = "WGS84", # Scale bar
#   #          height = .03,dist_unit = "km",location = "bottomleft", nudge_x = 0.5)+# Remove coordinates and boundaries
#   scale_fill_distiller(direction=1,palette = 'YlOrRd',limits = c(0,0.2))+
#   labs(title='Proportion of months with an outlier for number of positive tests',
#        fill='Proportion')+
#   theme(legend.position = "bottom",
#         legend.key.width= unit(1, "cm"),
#         legend.key.height = unit(0.5,'cm'),
#         legend.text = element_text(size=10),
#         legend.title = element_text(size=12),
#         axis.text.x = element_blank(),
#         axis.text.y = element_blank())
#
# tested_outlier_council <- ggplot()+
#   geom_sf(data = dqa_council_summ4map, aes(fill=tested_outlier))+ # Regions layer
#   geom_sf(data = water_bodies_sf, color = "lightblue",fill="lightblue")+ # Overlay colored water bodies
#   #north(data = tz1 ,location = "topleft", scale = .1,symbol = 5)+ # North direction
#   # scalebar(data = tz1,dist = 150,transform = TRUE,model = "WGS84", # Scale bar
#   #          height = .03,dist_unit = "km",location = "bottomleft", nudge_x = 0.5)+# Remove coordinates and boundaries
#   scale_fill_distiller(direction=1,palette = 'YlOrRd',limits = c(0,0.2))+
#   labs(title='Proportion of months with an outlier for number of tests',
#        fill='Proportion')+
#   theme(legend.position = "bottom",
#         legend.key.width= unit(1, "cm"),
#         legend.key.height = unit(0.5,'cm'),
#         legend.text = element_text(size=10),
#         legend.title = element_text(size=12),
#         axis.text.x = element_blank(),
#         axis.text.y = element_blank())
#
# total_outlier_council <- ggplot()+
#   geom_sf(data = dqa_council_summ4map, aes(fill=total_outlier))+ # Regions layer
#   geom_sf(data = water_bodies_sf, color = "lightblue",fill="lightblue")+ # Overlay colored water bodies
#   #north(data = tz1 ,location = "topleft", scale = .1,symbol = 5)+ # North direction
#   # scalebar(data = tz1,dist = 150,transform = TRUE,model = "WGS84", # Scale bar
#   #          height = .03,dist_unit = "km",location = "bottomleft", nudge_x = 0.5)+# Remove coordinates and boundaries
#   scale_fill_distiller(direction=1,palette = 'YlOrRd',limits = c(0,0.2))+
#   labs(title='Proportion of months with an outlier for number of attendance',
#        fill='Proportion')+
#   theme(legend.position = "bottom",
#         legend.key.width= unit(1, "cm"),
#         legend.key.height = unit(0.5,'cm'),
#         legend.text = element_text(size=10),
#         legend.title = element_text(size=12),
#         axis.text.x = element_blank(),
#         axis.text.y = element_blank())
#
# pos_gt_tested_council <- ggplot()+
#   geom_sf(data = dqa_council_summ4map, aes(fill=pos_gt_tested))+ # Regions layer
#   geom_sf(data = water_bodies_sf, color = "lightblue",fill="lightblue")+ # Overlay colored water bodies
#   #north(data = tz1 ,location = "topleft", scale = .1,symbol = 5)+ # North direction
#   # scalebar(data = tz1,dist = 150,transform = TRUE,model = "WGS84", # Scale bar
#   #          height = .03,dist_unit = "km",location = "bottomleft", nudge_x = 0.5)+# Remove coordinates and boundaries
#   scale_fill_distiller(direction=1,palette = 'YlOrRd',limits = c(0,0.2))+
#   labs(title='Proportion of months where # positive > # tested',
#        fill='Proportion')+
#   theme(legend.position = "bottom",
#         legend.key.width= unit(1, "cm"),
#         legend.key.height = unit(0.5,'cm'),
#         legend.text = element_text(size=10),
#         legend.title = element_text(size=12),
#         axis.text.x = element_blank(),
#         axis.text.y = element_blank())
#
# test_gt_total_council <- ggplot()+
#   geom_sf(data = dqa_council_summ4map, aes(fill=test_gt_total))+ # Regions layer
#   geom_sf(data = water_bodies_sf, color = "lightblue",fill="lightblue")+ # Overlay colored water bodies
#   #north(data = tz1 ,location = "topleft", scale = .1,symbol = 5)+ # North direction
#   # scalebar(data = tz1,dist = 150,transform = TRUE,model = "WGS84", # Scale bar
#   #          height = .03,dist_unit = "km",location = "bottomleft", nudge_x = 0.5)+# Remove coordinates and boundaries
#   scale_fill_distiller(direction=1,palette = 'YlOrRd',limits = c(0,0.2))+
#   labs(title='Proportion of months where # tested > # attended',
#        fill='Proportion')+
#   theme(legend.position = "bottom",
#         legend.key.width= unit(1, "cm"),
#         legend.key.height = unit(0.5,'cm'),
#         legend.text = element_text(size=10),
#         legend.title = element_text(size=12),
#         axis.text.x = element_blank(),
#         axis.text.y = element_blank())
#
# is_dif_pos_council <- ggplot()+
#   geom_sf(data = dqa_council_summ4map, aes(fill=is_dif_positive))+ # Regions layer
#   geom_sf(data = water_bodies_sf, color = "lightblue",fill="lightblue")+ # Overlay colored water bodies
#   #north(data = tz1 ,location = "topleft", scale = .1,symbol = 5)+ # North direction
#   # scalebar(data = tz1,dist = 150,transform = TRUE,model = "WGS84", # Scale bar
#   #          height = .03,dist_unit = "km",location = "bottomleft", nudge_x = 0.5)+# Remove coordinates and boundaries
#   scale_fill_distiller(direction=1,palette = 'YlOrRd')+
#   labs(title='Proportion of months where positive tests differ from regional',
#        fill='Proportion')+
#   theme(legend.position = "bottom",
#         legend.key.width= unit(1, "cm"),
#         legend.key.height = unit(0.5,'cm'),
#         legend.text = element_text(size=10),
#         legend.title = element_text(size=12),
#         axis.text.x = element_blank(),
#         axis.text.y = element_blank())
#
# tested_is_dif_council <- ggplot()+
#   geom_sf(data = dqa_council_summ4map, aes(fill=tested_is_dif))+ # Regions layer
#   geom_sf(data = water_bodies_sf, color = "lightblue",fill="lightblue")+ # Overlay colored water bodies
#   #north(data = tz1 ,location = "topleft", scale = .1,symbol = 5)+ # North direction
#   # scalebar(data = tz1,dist = 150,transform = TRUE,model = "WGS84", # Scale bar
#   #          height = .03,dist_unit = "km",location = "bottomleft", nudge_x = 0.5)+# Remove coordinates and boundaries
#   scale_fill_distiller(direction=1,palette = 'YlOrRd')+
#   labs(title='Proportion of months with an is_dif for number of tests',
#        fill='Proportion')+
#   theme(legend.position = "bottom",
#         legend.key.width= unit(1, "cm"),
#         legend.key.height = unit(0.5,'cm'),
#         legend.text = element_text(size=10),
#         legend.title = element_text(size=12),
#         axis.text.x = element_blank(),
#         axis.text.y = element_blank())
#
# total_is_dif_council <- ggplot()+
#   geom_sf(data = dqa_council_summ4map, aes(fill=total_is_dif))+ # Regions layer
#   geom_sf(data = water_bodies_sf, color = "lightblue",fill="lightblue")+ # Overlay colored water bodies
#   #north(data = tz1 ,location = "topleft", scale = .1,symbol = 5)+ # North direction
#   # scalebar(data = tz1,dist = 150,transform = TRUE,model = "WGS84", # Scale bar
#   #          height = .03,dist_unit = "km",location = "bottomleft", nudge_x = 0.5)+# Remove coordinates and boundaries
#   scale_fill_distiller(direction=1,palette = 'YlOrRd')+
#   labs(title='Proportion of months with an is_dif for number of attendance',
#        fill='Proportion')+
#   theme(legend.position = "bottom",
#         legend.key.width= unit(1, "cm"),
#         legend.key.height = unit(0.5,'cm'),
#         legend.text = element_text(size=10),
#         legend.title = element_text(size=12),
#         axis.text.x = element_blank(),
#         axis.text.y = element_blank())
#
# council_dqa_maps <- positive_outlier_council + tested_outlier_council + pos_gt_tested_council + test_gt_total_council +  plot_layout(guides = 'collect',ncol = 2)
#
