orderly::orderly_dependency("01_read_data", quote(latest()),
                             c('Full_data.csv','age_disaggregated_4corr_2017.csv','age_disaggregated_4corr_2022.csv'))
orderly::orderly_shared_resource('Province_grid.csv')
orderly::orderly_shared_resource('theme_base.R')
orderly::orderly_shared_resource('District edited Jan 2021')
orderly::orderly_artefact(files=c('council_prevalence_map.png'),description = 'council-level maps')
orderly::orderly_artefact(files=c('dqa_hf_remove_duplicates.rds',
                                   'dqa_hf_remove_duplicates_2017.rds',
                                   'dqa_hf_remove_duplicates_2022.rds'),
                           description = 'cleaned HF level data')
orderly::orderly_artefact(files='dqa_council_summ4map.rds',description = 'Data summarised at council level, with shapes for maps')

source('theme_base.R')
Full_data <- read.csv('Full_data.csv')
age_disaggregated_4corr_2017 <- read.csv('age_disaggregated_4corr_2017.csv')
age_disaggregated_4corr_2022 <- read.csv('age_disaggregated_4corr_2022.csv')

##########################################
# 1. Load data and define QA variables
##########################################

##############################
# Assess DQ at HF level
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

##rows where data is available, but not first attendance
# cleaned_dqametrics_data_noANC1 removed - dataframe created but never used downstream

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
    ANC_pos_sum = sum(ANC_pos[first_att_available],na.rm=TRUE),
    ANC_test_sum = sum(ANC_test[first_att_available],na.rm=TRUE),
    ANC_test_sum_nonmissing = sum(ANC_test[!is.na(ANC_pos)&first_att_available], na.rm = TRUE),
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
    mean_tested_nonmissing = mean(ANC_test[!is.na(ANC_pos)&first_att_available], na.rm = TRUE),
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
  mutate(proportion_months_with_data = num_months_with_firstatt/expected_total_months,
         proportion_months_flag = proportion_months_with_data>1.0)%>%
  mutate(across(c('num_months_with_data','num_months_with_test','num_months_with_hbtest'), ~ ./expected_total_months, .names = '{.col}_prop'))%>%
  mutate(across(sum_not_all_tested:sum_illogical_posGEtest, ~ ./num_months_with_firstatt, .names = '{.col}_prop'))%>%
  ungroup()



##Calculate by region the monthly proportion of health facilities with data
##(number of health facilities with first attendance data over expected number of health facilities)




# skimr::skim(dqa_hf)

dqa_hf_start_date <- dqa_hf%>%
  group_by(start_data_collection)%>%
  summarise(count_start=n())%>%
  ungroup()

dqa_hf_end_date <- dqa_hf%>%
  group_by(end_data_collection)%>%
  summarise(count_end=n())

dqa_hf_end_date_expected_reporting <- left_join(dqa_hf_start_date,dqa_hf_end_date,by=join_by(start_data_collection==end_data_collection))%>%
  arrange(start_data_collection)%>%
  mutate(count_end = lag(count_end))%>%
  mutate(count_end = ifelse(is.na(count_end),0,count_end))%>%
  mutate(count_diff = count_start-count_end,
         cumulative = cumsum(count_diff))

# Removed unused dataframe: dqa_region_expected_reporting

dqa_df_total_monthly <- dqa_hf_remove_duplicates%>%
  group_by(yearmon)%>%
  summarise(num_hf_with_data = sum(any_data_available,na.rm = TRUE),
            num_hf_with_firstatt = sum(first_att_available,na.rm = TRUE),
            num_hf_with_test = sum(anc_test_available,na.rm = TRUE),
            num_hf_with_hbtest = sum(hb_test_available,na.rm = TRUE),
            ANC_pos_sum=sum(ANC_pos[first_att_available],na.rm = TRUE),
            ANC_test_sum=sum(ANC_test[first_att_available],na.rm = TRUE),
            ANC_test_sum_nonmissing = sum(ANC_test[!is.na(ANC_pos)&first_att_available], na.rm = TRUE),
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
  mutate(across(c('num_hf_with_test','num_hf_with_hbtest'), ~ ./num_hf_with_firstatt, .names = '{.col}_prop'))%>%
  mutate(across(sum_not_all_tested:sum_illogical_noattresults, ~ ./num_hf_with_firstatt, .names = '{.col}_prop'))%>%
  mutate(prop_anymalariatests = num_hf_with_test/num_hf_with_firstatt)%>%
  mutate(sum_all_tested_prop = 1-sum_not_all_tested_prop)%>%
  ungroup()

#Regional summaries
dqa_df_region_monthly <- dqa_hf_remove_duplicates%>%
  group_by(HF)%>%
  mutate(start_data_collection = as.yearmon(suppressWarnings(min(date_NA,na.rm=TRUE))),
         end_data_collection = as.yearmon(suppressWarnings(max(date_NA,na.rm=TRUE))),
         expected_reporting = yearmon >= start_data_collection & yearmon <= end_data_collection
  )%>%
  ungroup()%>%
  group_by(Region,yearmon)%>%
  summarise(expected_reporting_hf = sum(expected_reporting,na.rm=TRUE),
            num_hf_with_data = sum(any_data_available,na.rm = TRUE),
            num_hf_with_firstatt = sum(first_att_available,na.rm = TRUE),
            num_hf_with_test = sum(anc_test_available,na.rm = TRUE),
            num_hf_with_hbtest = sum(hb_test_available,na.rm = TRUE),
            ANC_pos_sum=sum(ANC_pos[first_att_available],na.rm = TRUE),
            ANC_test_sum=sum(ANC_test[first_att_available],na.rm = TRUE),
            ANC_test_sum_nonmissing = sum(ANC_test[!is.na(ANC_pos)&first_att_available], na.rm = TRUE),
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
  mutate(across(c('num_hf_with_test','num_hf_with_hbtest'), ~ ./num_hf_with_firstatt, .names = '{.col}_prop'))%>%
  mutate(across(sum_not_all_tested:sum_illogical_noattresults, ~ ./num_hf_with_firstatt, .names = '{.col}_prop'))%>%
  mutate(prop_expected_with_firstatt = num_hf_with_firstatt/expected_reporting_hf,
         prop_anymalariatests = num_hf_with_test/num_hf_with_firstatt)%>%
  ungroup()

##Subset for reporting rates for plotting by month
subset_reporting_long <- dqa_df_total_monthly%>%
  select(yearmon,num_hf_with_firstatt,num_hf_with_test)%>%
  tidyr::pivot_longer(-c(yearmon))%>%
  mutate(name=factor(name,levels=c('num_hf_with_firstatt','num_hf_with_test'),labels=c('Any ANC1 attendance','Any malaria tests')))

subset_reporting_long_region <- dqa_df_region_monthly%>%
  select(yearmon,Region,num_hf_with_firstatt,num_hf_with_test,prop_anymalariatests,prop_expected_with_firstatt)%>%
  tidyr::pivot_longer(-c(Region,yearmon))%>%
  mutate(name=factor(name,levels=c('num_hf_with_firstatt','num_hf_with_test','prop_anymalariatests','prop_expected_with_firstatt'),labels=c('Any ANC1 attendance','Any malaria tests','Proportion of ANC1 facilities testing for malaria','Proportion of ANC1 facilities reporting attendance')),
         region = gsub(' Region','',Region))


hf_data_count <- ggplot(subset_reporting_long)+
  geom_line(aes(x=as.Date(yearmon),y=value,color=name),linewidth=1.4)+
  #scale_color_
  scale_color_paletteer_d("colorblindr::OkabeIto")+
  # scale_color_brewer(palette='Dark2')+
  scale_x_date(date_breaks='2 years',date_labels="'%y",expand = c(0,0))+
  scale_y_continuous(limits=c(0,(max(subset_reporting_long$value)+500)),expand = c(0,0))+
  guides(colour = guide_legend(nrow = 2)) +
  labs(y='Number of\nhealth facilities')+
  theme(axis.title.x = element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.5,0.1))

ggsave(hf_data_count,file='hf_count.png',units='in',height=4,width = 5)

province_grid<-read.csv("Province_grid.csv") %>%
  mutate(Region=NAME_1,
         code=NAME_1,
         name=NAME_1)

# Removed unused plots: hf_firstatt_data_prop, hf_data_prop_region, hf_firstatt_data_prop_byregion
##Subset for proportion of health facilities testing all women for plotting by month
subset_illogical_prop_long <- dqa_df_total_monthly%>%
  select(yearmon,sum_not_all_tested_prop,sum_not_all_tested,num_hf_with_firstatt)%>%
  mutate(all_tested_prop = 1-sum_not_all_tested_prop,
         sum_all_tested = num_hf_with_firstatt - sum_not_all_tested)

## Find date at which all_tested_prop reaches 0.9 and 0.8
dqa_alltested_90 <- subset_illogical_prop_long%>%
  filter(all_tested_prop >= 0.9) %>%
  slice(1) %>%
  pull(yearmon)
dqa_alltested_80 <- subset_illogical_prop_long%>%
  filter(all_tested_prop >= 0.8) %>%
  slice(1) %>%
  pull(yearmon)
dqa_alltested_95 <- subset_illogical_prop_long%>%
  filter(all_tested_prop >= 0.95) %>%
  slice(1) %>%
  pull(yearmon)

hf_alltested_count <- ggplot(subset_illogical_prop_long)+
  geom_line(aes(x=as.Date(yearmon),y=all_tested_prop),color=colorblindr::palette_OkabeIto[5],linewidth=1.3)+
  geom_segment(aes(x = as.Date(-Inf), xend = as.Date(dqa_alltested_80), y = 0.8, yend = 0.8), linetype = "dashed")+
  geom_segment(aes(x = as.Date(dqa_alltested_80), xend = as.Date(dqa_alltested_80), y = 0.8, yend = -Inf), linetype = "dashed")+
  geom_segment(aes(x = as.Date(-Inf), xend = as.Date(dqa_alltested_90), y = 0.9, yend = 0.9), linetype = "dashed")+
  geom_segment(aes(x = as.Date(dqa_alltested_90), xend = as.Date(dqa_alltested_90), y = 0.9, yend = -Inf), linetype = "dashed")+
  annotate("text", x = as.Date(dqa_alltested_80), y = 0, label = as.yearmon(dqa_alltested_80), vjust = -0.5, hjust = 1.1, size = 3) +
  annotate("text", x = as.Date(dqa_alltested_90), y = 0, label = as.yearmon(dqa_alltested_90), vjust = -0.5, hjust = -0.1, size = 3) +
  # annotate("text", x = as.Date(dqa_alltested_95), y = 0.95, label = as.yearmon(dqa_alltested_95), vjust = -1, hjust = 1, size = 3) +
  geom_hline(yintercept = 0.95,linetype='dashed',color='black')+
  # geom_hline(yintercept = 0.8,linetype='dashed',color='black')+
  scale_x_date(date_breaks='2 years',date_labels="'%y",expand = c(0,0))+
  scale_y_continuous(breaks=c(0,0.25,0.5,0.8,0.9,0.95,1.0),limits=c(0,1),expand = c(0,0))+
  labs(y='Proportion of\nhealth facilities')+
  theme(axis.title.x = element_blank(),
        legend.title = element_blank())
ggsave(hf_alltested_count,file='hf_alltested.png',units='in',height=4,width = 5)

subset_illogical_prop_region_long <- dqa_df_region_monthly%>%
  select(Region,yearmon,sum_not_all_tested_prop)%>%
  mutate(all_tested_prop = 1-sum_not_all_tested_prop,
         region = gsub(' Region','',Region))
# hf_alltested_count_region <- ggplot(subset_illogical_prop_region_long)+
#   geom_line(aes(x=as.Date(yearmon),y=all_tested_prop,group=region),color='darkgrey',linewidth=0.8,alpha=0.3)+
#   geom_line(data=subset_illogical_prop_long,aes(x=as.Date(yearmon),y=all_tested_prop),color=colorblindr::palette_OkabeIto[5],linewidth=1.2)+
#   geom_hline(yintercept = 0.9,linetype='dashed',color='black',linewidth=1)+
#   scale_x_date(date_breaks='2 years',date_labels="'%y")+
#   scale_y_continuous(breaks=c(0,0.25,0.5,0.75,0.9,1.0),limits=c(0,1),expand = c(0,0))+
#   labs(y='Proportion of\nhealth facilities')+
#   theme(axis.title.x = element_blank(),
#         legend.title = element_blank())
# ggsave(hf_alltested_count_region,file='hf_alltested_region.png',units='in',height=4,width = 5)

# Removed unused plot: hf_alltested_count_byregion

# colorblindr::palette_OkabeIto
#"#E69F00" "#56B4E9" "#009E73" "#F0E442" "#0072B2" "#D55E00" "#CC79A7" "#999999"

##For each region, identify the date at which 90% of facilities were testing
##all women
##Calculate date at which 90% of facilities and calculate length of time since
##January 2014
dqa_region_alltested_90 <- dqa_df_region_monthly%>%
  select(Region,yearmon,sum_not_all_tested_prop)%>%
  mutate(all_tested_prop = 1-sum_not_all_tested_prop)%>%
  filter(all_tested_prop>=0.9)%>%
  group_by(Region)%>%
  summarise(date_90alltested = min(yearmon))%>%
  ungroup()%>%
  mutate(months_since_jan2014_90alltested = interval(ymd('2014-01-01'),date_90alltested)%/%months(1))

##Heatmap where x-axis is month and y-axis is region showing proportion of facilities
##testing all women, with regions ordered by date at which 90% of facilities were testing
##all women with a diverging color scale where breakpoint is at 90% (using scale_fill_distiller(direction=1,palette = 'YlGnBu'))
dqa_region_alltested_90_order <- dqa_region_alltested_90%>%
  arrange(months_since_jan2014_90alltested)%>%
  mutate(Region=factor(Region,levels=Region))
# Removed unused plot: hf_alltested_heatmap

##Convert yearmon to an index so that raster is plotted at regular intervals
##but x-axis labels are still yearmon
dqa_df_region_monthly_index <- dqa_df_region_monthly%>%
  group_by(Region)%>%
  arrange(yearmon)%>%
  mutate(month_index = row_number())%>%
  ungroup()%>%
  mutate(Region=factor(gsub(' Region','',Region),levels = gsub(' Region','',dqa_region_alltested_90_order$Region)))

##convert date_90alltested in dqa_region_alltested_90 to month_index for plotting
dqa_region_alltested_90_order_index <- dqa_region_alltested_90_order%>%
  mutate(Region = gsub(' Region','',Region))%>%
  left_join(dqa_df_region_monthly_index%>%select(Region,yearmon,month_index),by=c('Region','date_90alltested'='yearmon'))%>%
  select(Region,date_90alltested,months_since_jan2014_90alltested,month_index)%>%
  mutate(Region=factor(gsub(' Region','',Region),levels= gsub(' Region','',dqa_region_alltested_90_order$Region)))

hf_alltested_heatmap_index <- ggplot(dqa_df_region_monthly_index)+
  geom_raster(aes(x=month_index,y=Region,
                  fill=1-sum_not_all_tested_prop),interpolate=FALSE)+
  geom_raster(data=dqa_region_alltested_90_order_index, aes(x=month_index,y=Region),
              fill='black',interpolate=FALSE)+
  scale_fill_gradient2(
    limits   = c(0.7, 1),
    midpoint = 0.9,
    low      = "#ef8a62",  # < 0.9
    mid      = "white",    # = 0.9
    high     = "#2166ac",  # > 0.9
    breaks   = seq(0, 1, by = 0.1),
    oob      = scales::squish
  )+
  geom_segment(data = dqa_region_alltested_90_order_index,
               aes(x = interval(ymd('2014-01-01'),date_90alltested)%/%months(1)+1,
                   xend = interval(ymd('2014-01-01'),date_90alltested)%/%months(1)+1,
                   y = Region,
                   yend = Region),
               color = "black") +
  geom_segment(data = data.frame(yeardate = seq(13, max(dqa_df_region_monthly_index$month_index), by = 12)),
               aes(x = yeardate,
                   xend = yeardate,
                   y = -Inf,
                   yend = Inf),
               color = "black") +
  ##X-axis labels for beginning of each year - only show year in the middle of the year interval
  scale_x_continuous(breaks = seq(7, max(dqa_df_region_monthly_index$month_index), by = 12),
                     labels = function(x) year(as.Date('2014-01-01') + months(x - 1)),
                     expand =c(0,0)) +
  scale_y_discrete(limits=rev,
                   expand = c(0,0))+
  labs(y='Region',fill='Proportion of\nhealth facilities')+
  theme(axis.title.x = element_blank())

##Do the same calculation and kaplan meier plot but at the council level
## Summarize by month and council for mapping
dqa_council_summ_monthly <- dqa_hf_remove_duplicates%>%
  group_by(Region,Council,yearmon)%>%
  summarise(num_hf_with_data = sum(any_data_available,na.rm = TRUE),
            num_hf_with_firstatt = sum(first_att_available,na.rm = TRUE),
            num_hf_with_test = sum(anc_test_available,na.rm = TRUE),
            num_hf_with_hbtest = sum(hb_test_available,na.rm = TRUE),
            ANC_pos_sum=sum(ANC_pos[first_att_available],na.rm = TRUE),
            ANC_test_sum=sum(ANC_test[first_att_available],na.rm = TRUE),
            ANC_test_sum_nonmissing = sum(ANC_test[!is.na(ANC_pos)&first_att_available], na.rm = TRUE),
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

dqa_council_alltested_90 <- dqa_council_summ_monthly%>%
  select(Council,yearmon,sum_not_all_tested_prop)%>%
  mutate(all_tested_prop = 1-sum_not_all_tested_prop)%>%
  filter(all_tested_prop>=0.9)%>%
  group_by(Council)%>%
  summarise(date_90alltested = min(yearmon))%>%
  ungroup()%>%
  mutate(months_since_jan2014_90alltested = interval(ymd('2014-01-01'),date_90alltested)%/%months(1))

# Removed unused plots: hf_council_alltested_kaplanmeier, hf_council_alltested_kaplanmeier_region
##Sumarise by year and council
dqa_council_summ <- dqa_hf_remove_duplicates%>%
  group_by(Region,Council,Year)%>%
  summarise(num_hf_with_data = sum(any_data_available,na.rm = TRUE),
            num_hf_with_firstatt = sum(first_att_available,na.rm = TRUE),
            num_hf_with_test = sum(anc_test_available,na.rm = TRUE),
            num_hf_with_hbtest = sum(hb_test_available,na.rm = TRUE),
            ANC_pos_sum=sum(ANC_pos[first_att_available],na.rm = TRUE),
            ANC_test_sum=sum(ANC_test[first_att_available],na.rm = TRUE),
            ANC_test_sum_nonmissing = sum(ANC_test[!is.na(ANC_pos)&first_att_available], na.rm = TRUE),
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
  ##https://kilomberodc.go.tz/storage/app/uploads/public/666/e74/217/666e74217547f998847056.pdf
  # mutate(Council = ifelse(Council=='Kilombero District Council' & (Year == 2014 | Year == 2015),'Mlimba District Council',Council))%>%
  # filter(!(Council=='Mlimba District Council' & (Year == 2014 | Year == 2015) & num_hf_with_data == 0))%>%
  # ##https://en.wikipedia.org/wiki/Lindi_District,_Lindi
  # mutate(Council = ifelse(Council=='Lindi District Council' & (Year == 2014 | Year == 2015),'Mtama District Council',Council))%>%
  # filter(!(Council=='Mtama District Council' & (Year == 2014 | Year == 2015) & num_hf_with_data == 0))%>%
  # ##https://katavi.go.tz/history
  # mutate(Council = ifelse(Council=='Mpanda District Council' & (Year == 2014 | Year == 2015),'Tanganyika District Council',Council))%>%
  # filter(!(Council=='Tanganyika District Council' & (Year == 2014 | Year == 2015) & num_hf_with_data == 0))%>%
  ungroup()
##May by council
# Shape files -----
councils <- st_read("District edited Jan 2021/District edited Jan 2021.shp") %>%
  rename(Council='DHIS2_Dist')
sf::sf_use_s2(FALSE)
regions <- councils %>% group_by(Region_Nam)%>%summarise()
dqa_council_summ4map <- merge(councils,dqa_council_summ,by='Council')%>%
  filter(Year %% 2 == 0)
time_of_90testing4map <- left_join(councils,dqa_council_alltested_90,by='Council')

prevalence_council <- ggplot()+
  geom_sf(data = dqa_council_summ4map, aes(fill=prevalence))+ # Regions layer
  geom_sf(data=regions, fill=NA,linewidth=0.3)+
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

# alltested_council <- ggplot()+
#   geom_sf(data = dqa_council_summ4map, aes(fill=all_tested_prop))+ # Regions layer
#   geom_sf(data=regions, fill=NA,linewidth=0.3)+
#   # geom_sf(data = water_bodies_sf, color = "lightblue",fill="lightblue")+ # Overlay colored water bodies
#   #north(data = tz1 ,location = "topleft", scale = .1,symbol = 5)+ # North direction
#   # scalebar(data = tz1,dist = 150,transform = TRUE,model = "WGS84", # Scale bar
#   #          height = .03,dist_unit = "km",location = "bottomleft", nudge_x = 0.5)+# Remove coordinates and boundaries
#   scale_fill_distiller(direction=1,palette = 'YlGnBu')+
#   facet_wrap(.~Year)+
#   labs(fill='Proportion of health facilities')+
#   theme(legend.position = "bottom",
#         legend.key.width= unit(1, "cm"),
#         legend.key.height = unit(0.5,'cm'),
#         legend.text = element_text(size=10,margin = margin(t=3)),
#         legend.title = element_text(size=12),
#         axis.text.x = element_blank(),
#         axis.text.y = element_blank())
# ggsave(alltested_council,file='alltested_council_map.png',units='in',height=4,width = 10)

# Removed unused plot: timing90tested_council (created but never saved)

design_layout <- "
AABB
CCCC
CCCC"
testing_composite <- free(hf_data_count | hf_alltested_count) / hf_alltested_heatmap_index +
 plot_layout(heights=c(1,2))+plot_annotation(tag_levels = 'A')
ggsave(testing_composite,file='anc_testing_composite_update.png',units='in',height=8,width = 7)

##Clean age disaggregated data
##Get names of specific variables of interest
results_cols <- c('ANC_test','ANC_pos','llin_provided','ipt2_provided','ipt3_provided','ipt4_provided','Hb_test','Anaemia')
all_results_cols <- c('Total_re_ad','Before_12wk','After_12wk','ANC_re_ad','ANC_test','ANC_pos','llin_provided','ipt2_provided','ipt3_provided','ipt4_provided','Hb_test','Anaemia')
main_results_cols <- c('Total_re_ad','Before_12wk','After_12wk','ANC_re_ad','ANC_test','ANC_pos','Hb_test','Anaemia')

cleaned_dqametrics_2017 <- age_disaggregated_4corr_2017 %>%
  mutate(
    yearmon=as.yearmon(paste0(Year,"-",Month),"%Y-%m"))%>%
  tidyr::complete(yearmon,tidyr::nesting(Region,Council,HF))%>% #Filling in missing dates for each health facility
  mutate( #Replace NAs with 0s where appropriate, apply logic checks
    Year = lubridate::year(yearmon),
    Month = lubridate::month(yearmon),
    pre_2016 = TRUE,
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

dqa_hf_remove_missing_2017 <- cleaned_dqametrics_2017%>%
  group_by(Region,Council,HF)%>%
  mutate(
    all_missed = all(!any_data_available))%>% #Identify HFs with no data reported across study period
  filter(!all_missed)%>% #Remove said HFs
  ungroup()
dqa_hf_remove_duplicates_2017 <- dqa_hf_remove_missing_2017%>%
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

# nrow(age_disaggregated_4corr_2017)
# nrow(cleaned_dqametrics_2017)
# nrow(dqa_hf_remove_missing_2017)
# nrow(dqa_hf_remove_duplicates_2017)

cleaned_dqametrics_2022 <- age_disaggregated_4corr_2022 %>%
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

dqa_hf_remove_missing_2022 <- cleaned_dqametrics_2022%>%
  group_by(Region,Council,HF)%>%
  mutate(
    all_missed = all(!any_data_available))%>% #Identify HFs with no data reported across study period
  filter(!all_missed)%>% #Remove said HFs
  ungroup()
dqa_hf_remove_duplicates_2022 <- dqa_hf_remove_missing_2022%>%
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

saveRDS(dqa_hf_remove_duplicates_2017,'dqa_hf_remove_duplicates_2017.rds')
saveRDS(dqa_hf_remove_duplicates_2022,'dqa_hf_remove_duplicates_2022.rds')

##########################################
# 2. Process age-disaggregated data (2017 & 2022)
##########################################
# Note: Exploratory code for alternative analyses removed (see git history)
# Current approach uses main Full_data with duplicates removed

##########################################
# 3. Create council-level aggregations and maps
##########################################
# Note: Alternative analyses and exploratory code removed (see git history)
# Current implementation uses core council-level aggregations for mapping

###By council and month
dqa_df_council_monthly <- dqa_hf_remove_duplicates%>%
  mutate(ANC_pos = ifelse(illogical_posGEtest,NA,ANC_pos),
         ANC_test = ifelse(illogical_posGEtest,NA,ANC_test))%>%
  group_by(Region,Council,yearmon)%>%
  summarise(num_hf_with_data = sum(any_data_available,na.rm = TRUE),
            num_hf_with_firstatt = sum(first_att_available,na.rm = TRUE),
            num_hf_with_test = sum(anc_test_available,na.rm = TRUE),
            num_hf_with_hbtest = sum(hb_test_available,na.rm = TRUE),
            ANC_pos_sum=sum(ANC_pos[first_att_available],na.rm = TRUE),
            ANC_test_sum=sum(ANC_test[first_att_available],na.rm = TRUE),
            ANC_test_sum_nonmissing = sum(ANC_test[!is.na(ANC_pos)&first_att_available], na.rm = TRUE),
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

dqa_df_region_monthly <- dqa_hf_remove_duplicates%>%
  mutate(ANC_pos = ifelse(illogical_posGEtest,NA,ANC_pos),
         ANC_test = ifelse(illogical_posGEtest,NA,ANC_test))%>%
  group_by(Region,yearmon)%>%
  summarise(num_hf_with_data = sum(any_data_available,na.rm = TRUE),
            num_hf_with_firstatt = sum(first_att_available,na.rm = TRUE),
            num_hf_with_test = sum(anc_test_available,na.rm = TRUE),
            num_hf_with_hbtest = sum(hb_test_available,na.rm = TRUE),
            ANC_pos_sum=sum(ANC_pos[first_att_available],na.rm = TRUE),
            ANC_test_sum=sum(ANC_test[first_att_available],na.rm = TRUE),
            ANC_test_sum_nonmissing = sum(ANC_test[!is.na(ANC_pos)&first_att_available], na.rm = TRUE),
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
dqa_monthly_region_average <- dqa_df_region_monthly %>%
  filter(yearmon>=as.yearmon('Jan 2024'))%>%
  group_by(Region)%>%
  summarise(average_tested = median(ANC_test_sum_nonmissing))

saveRDS(dqa_df_council_monthly,'dqa_df_council_monthly.rds')

##########################################
# 4. Create zone-stratified datasets
##########################################
# Defines geographic zones and creates nested data structure
# Output: dqa_council_monthly_nested.rds for use in run_pmcmc task

Western_zone<- c('Tabora Region', 'Kigoma Region')
Northern_zone<- c('Kilimanjaro Region', 'Tanga Region', 'Arusha Region')
Central_zone<- c('Dodoma Region', 'Singida Region', 'Manyara Region')
SouthernHighlands_zone<- c('Iringa Region', 'Njombe Region', 'Ruvuma Region')
Southern_zone<- c('Lindi Region', 'Mtwara Region')
SouthwestHighlands_zone<- c('Mbeya Region', 'Rukwa Region', 'Katavi Region', 'Songwe Region')
Lake_zone<- c('Kagera Region', 'Mwanza Region', 'Geita Region', 'Mara Region', 'Simiyu Region', 'Shinyanga Region')
Eastern_zone<- c('Dar Es Salaam Region', 'Pwani Region', 'Morogoro Region')

dqa_monthly_brief <- dqa_council_summ_monthly%>%
  select(Region, Council, yearmon, ANC_pos_sum, ANC_test_sum_nonmissing)%>%
  rename(positive = ANC_pos_sum,
         tested = ANC_test_sum_nonmissing,
         month = yearmon)%>%
  mutate(Zone = case_when(Region %in% Western_zone ~ 'Western',
                          Region %in% Northern_zone ~ 'Northern',
                          Region %in% Central_zone ~ 'Central',
                          Region %in% SouthernHighlands_zone ~ 'Southern Highlands',
                          Region %in% Southern_zone ~ 'Southern',
                          Region %in% SouthwestHighlands_zone ~ 'Southwest Highlands',
                          Region %in% Lake_zone ~ 'Lake',
                          Region %in% Eastern_zone ~ 'Eastern',
                          .default = NA))

dqa_monthly_brief_average <- dqa_monthly_brief %>%
  filter(month>=as.yearmon('Jan 2024'))%>%
  group_by(Region, Council)%>%
  summarise(average_tested = median(tested))

split_nested <- function(df, cols) {
  stopifnot(all(cols %in% names(df)))
  f <- function(d, i) {
    if (i > length(cols)) return(d)
    sp <- split(d, d[[cols[i]]], drop = TRUE)
    lapply(sp, f, i + 1)
  }
  f(df, 1)
}

dqa_monthly_nested <- split_nested(dqa_monthly_brief, c("Zone", "Region", "Council"))
saveRDS(dqa_monthly_nested,'dqa_council_monthly_nested.rds')
