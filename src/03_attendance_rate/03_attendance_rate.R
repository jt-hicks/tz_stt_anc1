orderly2::orderly_dependency("02_data_quality", quote(latest()),
                             c('dqa_hf_remove_duplicates.rds'))
orderly2::orderly_shared_resource('theme_base.R')
orderly2::orderly_shared_resource('District edited Jan 2021')
orderly2::orderly_shared_resource('data/tz_population_estimates_2022.csv')
orderly2::orderly_shared_resource('data/dhs_fertility_by_region.csv')

source('theme_base.R')
dqa_hf <- readRDS('dqa_hf_remove_duplicates.rds')

councils <- st_read("District edited Jan 2021/District edited Jan 2021.shp") # Region sf

##Aggregate polygons to region level
sf_use_s2(FALSE)
regions <- councils %>%
  group_by(Region_Nam) %>%
  summarise()%>%
  ungroup()%>%
  mutate(Region = ifelse(Region_Nam=='Dar-es-salaam','Dar Es Salaam',Region_Nam))

##Read in census data to get number of women of reproductive age
census_2022 <- read.csv('data/tz_population_estimates_2022.csv')
names(census_2022) <- c('Region','total_population','female_population','percent_under1yr','percent_0to4',
                        'percent_0to8','percent_0to14','percent_0to17','percent_13to19',
                        'percent_15to54','percent_60plus','percent_65plus','percent_70plus',
                        'age_dependency_ratio','percent_women_reproage','percent_urban','percent_rural')
census_2022$Region <- ifelse(census_2022$Region=='Dar es Salaam','Dar Es Salaam',census_2022$Region)
#Read in DHS 2022 fertility rates
fertility_2022 <- read.csv('data/dhs_fertility_by_region.csv')
fertility_2022$Region <- ifelse(fertility_2022$Region=='Dar es Salaam','Dar Es Salaam',fertility_2022$Region)
fertility_2022$monthly_fertility_rate <- fertility_2022$Total.fertility.rate/3/12 #Total fertility rate was the reported fertility in the 3 years preceding the survey

census_with_fert <- inner_join(census_2022,fertility_2022,by='Region')
nrow(census_with_fert)
#https://pmc.ncbi.nlm.nih.gov/articles/PMC6534308/
census_with_fert$mean_fertility_rate <- census_with_fert$Total.fertility.rate / (5*7)
census_with_fert$monthly_fertility_rate <- census_with_fert$mean_fertility_rate / 12
census_with_fert$expected_monthly_pregnancies <- census_with_fert$female_population * census_with_fert$percent_women_reproage/100 * census_with_fert$monthly_fertility_rate
mean(census_with_fert$expected_monthly_pregnancies)

ggplot(census_with_fert)+
  geom_col(aes(x=fct_rev(fct_reorder(Region, expected_monthly_pregnancies)),y=expected_monthly_pregnancies/1000))+
  scale_y_continuous(limits=c(0,max(census_with_fert$expected_monthly_pregnancies)/1000+3),expand=c(0,0))+
  labs(x='Region',y='Expected Monthly Pregnancies (x10,000)')+
  coord_flip()

##Summarise anc data by region
##Sumarise by year and council
dqa_region_monthly <- dqa_hf%>%
  group_by(Region,yearmon)%>%
  summarise(ANC_pos_sum=sum(ANC_pos,na.rm = TRUE),
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
            sum_Anaemia = sum(Anaemia,na.rm=TRUE)
  )%>%
  ungroup()%>%
  mutate(Region = gsub(' Region','',Region))

att_with_expected_births <- ggplot(data=dqa_region_monthly)+
  geom_line(aes(x=yearmon,y=total_first_att_sum))+
  geom_hline(data = census_with_fert,aes(yintercept = expected_monthly_pregnancies),color='darkgrey',linetype='dashed')+
  facet_wrap(.~Region)
dqa_region_yearly <- dqa_hf%>%
  group_by(Region,Year)%>%
  summarise(ANC_pos_sum=sum(ANC_pos,na.rm = TRUE),
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
            sum_Anaemia = sum(Anaemia,na.rm=TRUE)
  )%>%
  ungroup()%>%
  mutate(Region = gsub(' Region','',Region))
