orderly::orderly_artefact(description='Raw data',files='Full_data.csv')
orderly::orderly_artefact(description='Subsets data',files=c("age_disaggregated_4corr_2017.csv",
                                                             "age_disaggregated_4corr_2022.csv",
                                                             "age_disaggregated_22to24.csv"))

orderly::orderly_shared_resource('data')
orderly::orderly_shared_resource('data/dhs_dates.xlsx')

dhs_dates <- read_xlsx('data/dhs_dates.xlsx')

###Updated data March 2023 - Not disaggregated by age###
#### read in ANC data ##
Full_data_2016<-read_excel("data/ANC_Data_Compiled.xlsx",sheet="2016_2019")%>%
  mutate(HF=toupper(HF))%>%
  mutate(HF=gsub('  +',' ',HF))
Full_data_2020<-read_excel("data/ANC_Data_Compiled.xlsx",sheet="2020_2021")%>%
  mutate(HF=toupper(HF))%>%
  mutate(HF=gsub('  +',' ',HF))

names(Full_data_2016)[5]<-"Month"
names(Full_data_2016)[6]<-"Before_12wk"
names(Full_data_2016)[7]<-"After_12wk"
names(Full_data_2016)[8]<-"ANC_re_ad"
names(Full_data_2016)[9]<-"Total_re_ad"
names(Full_data_2016)[10]<-"ANC_test"
names(Full_data_2016)[11]<-"ANC_pos"
names(Full_data_2016)[12]<-"llin_provided"
names(Full_data_2016)[13]<-"ipt2_provided"
names(Full_data_2016)[14]<-"ipt3_provided"
names(Full_data_2016)[15]<-"ipt4_provided"
names(Full_data_2016)[16]<-"Hb_test"
names(Full_data_2016)[17]<-"Anaemia"

names(Full_data_2020)[5]<-"Month"
names(Full_data_2020)[6]<-"Before_12wk"
names(Full_data_2020)[7]<-"After_12wk"
names(Full_data_2020)[8]<-"ANC_re_ad"
names(Full_data_2020)[9]<-"Total_re_ad"
names(Full_data_2020)[10]<-"ANC_test"
names(Full_data_2020)[11]<-"ANC_pos"
names(Full_data_2020)[12]<-"llin_provided"
names(Full_data_2020)[13]<-"ipt2_provided"
names(Full_data_2020)[14]<-"ipt3_provided"
names(Full_data_2020)[15]<-"ipt4_provided"
names(Full_data_2020)[16]<-"Hb_test"
names(Full_data_2020)[17]<-"Anaemia"

Full_data_2016 <- Full_data_2016%>%
  group_by_all()%>%
  filter(row_number()==1)%>%
  ungroup()

Full_data_2020 <- Full_data_2020%>%
  group_by_all()%>%
  filter(row_number()==1)%>%
  ungroup()
month_count_2016 <- Full_data_2016 %>%
  group_by(Region,Council,HF,Year,Month)%>%
  summarise(count_repeat=n())%>%
  arrange(Region,Council,HF,Year,Month)


Full_data_2016_count <- left_join(Full_data_2016,month_count_2016, by=c('Region','Council','HF','Year','Month'))

month_count_2020 <- Full_data_2020 %>%
  group_by(Region,Council,HF,Year,Month)%>%
  summarise(count_repeat=n())%>%
  arrange(Region,Council,HF,Year,Month)
Full_data_2020_count <- left_join(Full_data_2020,month_count_2020, by=c('Region','Council','HF','Year','Month'))

##Age disaggregated data 2014 to 2017
age_split_14to15 <- read.csv("data/anc2014_17.csv") %>%
  mutate(Health_facility=toupper(Health_facility))%>%
  mutate(Health_facility=gsub('  +',' ',Health_facility))%>%
  rename(HF='Health_facility',
         Before_12wk_lt20='ANCLT12LT20',
         After_12wk_lt20='ANCGE12LT20',
         Before_12wk_ge20='ANCLT12GE20',
         After_12wk_ge20='ANCGE12GE20',
         ANC_test_lt20='RDTLT20',
         ANC_test_ge20='RDTGE20',
         ANC_pos_lt20='MALLT20',
         ANC_pos_ge20='MALGE20',
         llin_provided_lt20='LLINLT20',
         llin_provided_ge20='LLINGE20',
         ipt2_provided_lt20='IPTp2LT20',
         ipt2_provided_ge20='IPTp2GE20',
         ipt3_provided_lt20='IPTp3LT20',
         ipt3_provided_ge20='IPTp3GE20',
         ANC_attendance_total = 'ANC',
         ANC_pos = 'Mal',
         ANC_test = 'RDT'
  )%>%
  filter(Year < 2016)%>%
  mutate(
    Before_12wk = Before_12wk_lt20 + Before_12wk_ge20,
    After_12wk = After_12wk_lt20 + After_12wk_ge20,
    ANC_re_ad = NA,
    llin_provided = llin_provided_lt20 + llin_provided_ge20,
    ipt2_provided = ipt2_provided_lt20 + ipt2_provided_ge20,
    ipt3_provided = ipt3_provided_lt20 + ipt3_provided_ge20,
    Total_re_ad = NA,
    ipt4_provided = NA,
    Hb_test = NA,
    Anaemia = NA,
    Month = match(Month, month.name)
  )%>%
  mutate(Council = case_when(Council == 'Kilombero District Council' ~ 'Mlimba District Council',
                             Council == 'Lindi District Council' ~ 'Mtama District Council',
                             Council == 'Mpanda District Council' ~ 'Tanganyika District Council',
                             .default = Council))%>%
  group_by_all()%>%
  filter(row_number()==1)%>%
  ungroup()



month_count_14to15 <- age_split_14to15 %>%
  group_by(Region,Council,HF,Year,Month)%>%
  summarise(count_repeat=n())%>%
  arrange(Region,Council,HF,Year,Month)
Full_data_14to15_count <- left_join(age_split_14to15,month_count_14to15, by=c('Region','Council','HF','Year','Month'))
str(dhs_dates)

dhs_dates_2017 <- c(as.yearmon(as.Date(dhs_dates$Start)[1]),as.yearmon(as.Date(dhs_dates$End)[1]))

age_disaggregated_4corr_2017 <- read.csv("data/anc2014_17.csv") %>%
  mutate(Health_facility=toupper(Health_facility))%>%
  mutate(Health_facility=gsub('  +',' ',Health_facility))%>%
  rename(HF='Health_facility',
         Before_12wk_lt20='ANCLT12LT20',
         After_12wk_lt20='ANCGE12LT20',
         Before_12wk_ge20='ANCLT12GE20',
         After_12wk_ge20='ANCGE12GE20',
         ANC_test_lt20='RDTLT20',
         ANC_test_ge20='RDTGE20',
         ANC_pos_lt20='MALLT20',
         ANC_pos_ge20='MALGE20',
         llin_provided_lt20='LLINLT20',
         llin_provided_ge20='LLINGE20',
         ipt2_provided_lt20='IPTp2LT20',
         ipt2_provided_ge20='IPTp2GE20',
         ipt3_provided_lt20='IPTp3LT20',
         ipt3_provided_ge20='IPTp3GE20',
         ANC_attendance_total = 'ANC',
         ANC_pos = 'Mal',
         ANC_test = 'RDT'
         )%>%
  mutate(yearmon=as.yearmon(paste0(Year,"-",Month),"%Y-%B"))%>%
  filter(yearmon >= dhs_dates_2017[1] & yearmon <= dhs_dates_2017[2])%>%
  mutate(
    Before_12wk = Before_12wk_lt20 + Before_12wk_ge20,
    After_12wk = After_12wk_lt20 + After_12wk_ge20,
    ANC_re_ad = NA,
    llin_provided = llin_provided_lt20 + llin_provided_ge20,
    ipt2_provided = ipt2_provided_lt20 + ipt2_provided_ge20,
    ipt3_provided = ipt3_provided_lt20 + ipt3_provided_ge20,
    Total_re_ad = NA,
    ipt4_provided = NA,
    Hb_test = NA,
    Anaemia = NA,
    Month = match(Month, month.name)
  )%>%
  group_by_all()%>%
  filter(row_number()==1)%>%
  ungroup()

write.csv(age_disaggregated_4corr_2017,"age_disaggregated_4corr_2017.csv")


#Age disaggregated 2022-2024
Full_data_2022<-read_excel("data/ANC data 080425.xlsx")%>%
  mutate(`Health Facility`=toupper(`Health Facility`))%>%
  mutate(`Health Facility`=gsub('  +',' ',`Health Facility`))
Full_data_2022_mod <- Full_data_2022%>%
  rename(HF='Health Facility',
         Council = 'District',
         Month = 'periodname')%>%
  mutate(ANC_re_ad = rowSums(pick(matches("ANC Wateja wa marudio .+$")),na.rm = TRUE),
         ANC_test = rowSums(pick(matches("ANC Waliopimwa Malaria kutumia mRDT/BS .+$")),na.rm = TRUE),
         ANC_pos = rowSums(pick(matches("ANC Waliogundulika Malaria positive .+$")),na.rm = TRUE),
         llin_provided = rowSums(pick(matches("ANC Waliopewa LLIN .+$")),na.rm = TRUE),
         ipt2_provided = rowSums(pick(matches("ANC Waliopewa IPT2 .+$")),na.rm = TRUE),
         ipt3_provided = rowSums(pick(matches("ANC Waliopewa IPT3 .+$")),na.rm = TRUE),
         ipt4_provided = rowSums(pick(matches("ANC Waliopewa IPT4 .+$")),na.rm = TRUE),
         Hb_test = rowSums(pick(matches("ANC Idadi ya Wajawazito Waliopima wingi wa damu .+$")),na.rm = TRUE),
         Anaemia = rowSums(pick(matches("ANC Upungufu mkubwa wa damu &amp;lt;8.5g/dl – Anaemia hudhurio la kwanza .+$")),na.rm = TRUE),
         Before_12wk = rowSums(pick(matches("ANC Umri wa mimba chini ya wiki 12 .+$")),na.rm = TRUE),
         After_12wk = rowSums(pick(matches("ANC Umri wa mimba wiki 12 au zaidi .+$")),na.rm = TRUE),
         Total_re_ad = rowSums(pick(Before_12wk,After_12wk,ANC_re_ad),na.rm=TRUE),
         Month = ifelse(Month %in% month.name,match(Month, month.name),Month)
  )%>%
  group_by_all()%>%
  filter(row_number()==1)%>%
  ungroup()

month_count_2022 <- Full_data_2022_mod %>%
  group_by(Region,Council,HF,Year,Month)%>%
  summarise(count_repeat=n())%>%
  arrange(Region,Council,HF,Year,Month)
Full_data_2022_count <- left_join(Full_data_2022_mod,month_count_2022, by=c('Region','Council','HF','Year','Month'))

Full_data<-rbind(
  Full_data_14to15_count%>%select(Region,Council,HF,Year,Month,count_repeat,Before_12wk,After_12wk,ANC_re_ad,Total_re_ad,ANC_test,ANC_pos,llin_provided,ipt2_provided,ipt3_provided,ipt4_provided,Hb_test,Anaemia),
  Full_data_2016_count%>%select(Region,Council,HF,Year,Month,count_repeat,Before_12wk,After_12wk,ANC_re_ad,Total_re_ad,ANC_test,ANC_pos,llin_provided,ipt2_provided,ipt3_provided,ipt4_provided,Hb_test,Anaemia),
  Full_data_2020_count%>%select(Region,Council,HF,Year,Month,count_repeat,Before_12wk,After_12wk,ANC_re_ad,Total_re_ad,ANC_test,ANC_pos,llin_provided,ipt2_provided,ipt3_provided,ipt4_provided,Hb_test,Anaemia),
  Full_data_2022_count%>%select(Region,Council,HF,Year,Month,count_repeat,Before_12wk,After_12wk,ANC_re_ad,Total_re_ad,ANC_test,ANC_pos,llin_provided,ipt2_provided,ipt3_provided,ipt4_provided,Hb_test,Anaemia)
)
write.csv(Full_data,"Full_data.csv")

dhs_dates_2022 <- c(as.yearmon(as.Date(dhs_dates$Start)[2]),as.yearmon(as.Date(dhs_dates$End)[2]))

age_disaggregated_4corr_2022 <- Full_data_2022%>%
  rename(HF='Health Facility',
         Council = 'District',
         Month = 'periodname')%>%
  mutate(ANC_re_ad = rowSums(pick(matches("ANC Wateja wa marudio .+$")),na.rm = TRUE),
         ANC_test = rowSums(pick(matches("ANC Waliopimwa Malaria kutumia mRDT/BS .+$")),na.rm = TRUE),
         ANC_pos = rowSums(pick(matches("ANC Waliogundulika Malaria positive .+$")),na.rm = TRUE),
         llin_provided = rowSums(pick(matches("ANC Waliopewa LLIN .+$")),na.rm = TRUE),
         ipt2_provided = rowSums(pick(matches("ANC Waliopewa IPT2 .+$")),na.rm = TRUE),
         ipt3_provided = rowSums(pick(matches("ANC Waliopewa IPT3 .+$")),na.rm = TRUE),
         ipt4_provided = rowSums(pick(matches("ANC Waliopewa IPT4 .+$")),na.rm = TRUE),
         Hb_test = rowSums(pick(matches("ANC Idadi ya Wajawazito Waliopima wingi wa damu .+$")),na.rm = TRUE),
         Anaemia = rowSums(pick(matches("ANC Upungufu mkubwa wa damu &amp;lt;8.5g/dl – Anaemia hudhurio la kwanza .+$")),na.rm = TRUE),
         Before_12wk = rowSums(pick(matches("ANC Umri wa mimba chini ya wiki 12 .+$")),na.rm = TRUE),
         After_12wk = rowSums(pick(matches("ANC Umri wa mimba wiki 12 au zaidi .+$")),na.rm = TRUE),
         Total_re_ad = rowSums(pick(Before_12wk,After_12wk,ANC_re_ad),na.rm=TRUE),
         Month = ifelse(Month %in% month.name,match(Month, month.name),Month)
  )%>%
  mutate(yearmon=as.yearmon(paste0(Year,"-",Month),"%Y-%m"))%>%
  filter(yearmon >= dhs_dates_2022[1] & yearmon <= dhs_dates_2022[2])%>%
  group_by_all()%>%
  filter(row_number()==1)%>%
  ungroup()

names(age_disaggregated_4corr_2022) <- gsub(x = names(age_disaggregated_4corr_2022), pattern = "ANC Wateja wa marudio ", replacement = "ANC_re_ad_")
names(age_disaggregated_4corr_2022) <- gsub(x = names(age_disaggregated_4corr_2022), pattern = "ANC Waliopimwa Malaria kutumia mRDT/BS ", replacement = "ANC_test_")
names(age_disaggregated_4corr_2022) <- gsub(x = names(age_disaggregated_4corr_2022), pattern = "ANC Waliogundulika Malaria positive ", replacement = "ANC_pos_")
names(age_disaggregated_4corr_2022) <- gsub(x = names(age_disaggregated_4corr_2022), pattern = "ANC Waliopewa LLIN ", replacement = "llin_provided_")
names(age_disaggregated_4corr_2022) <- gsub(x = names(age_disaggregated_4corr_2022), pattern = "ANC Waliopewa IPT2 ", replacement = "ipt2_provided_")
names(age_disaggregated_4corr_2022) <- gsub(x = names(age_disaggregated_4corr_2022), pattern = "ANC Waliopewa IPT3 ", replacement = "ipt3_provided_")
names(age_disaggregated_4corr_2022) <- gsub(x = names(age_disaggregated_4corr_2022), pattern = "ANC Waliopewa IPT4 ", replacement = "ipt4_provided_")
names(age_disaggregated_4corr_2022) <- gsub(x = names(age_disaggregated_4corr_2022), pattern = "ANC Idadi ya Wajawazito Waliopima wingi wa damu ", replacement = "Hb_test_")
names(age_disaggregated_4corr_2022) <- gsub(x = names(age_disaggregated_4corr_2022), pattern = "ANC Upungufu mkubwa wa damu &amp;lt;8.5g/dl – Anaemia hudhurio la kwanza ", replacement = "Anaemia_")
names(age_disaggregated_4corr_2022) <- gsub(x = names(age_disaggregated_4corr_2022), pattern = "ANC Umri wa mimba chini ya wiki 12 ", replacement = "Before_12wk_")
names(age_disaggregated_4corr_2022) <- gsub(x = names(age_disaggregated_4corr_2022), pattern = "ANC Umri wa mimba wiki 12 au zaidi ", replacement = "After_12wk")
names(age_disaggregated_4corr_2022) <- gsub(x = names(age_disaggregated_4corr_2022), pattern = " - ", replacement = "_")
names(age_disaggregated_4corr_2022) <- gsub(x = names(age_disaggregated_4corr_2022), pattern = " -", replacement = "_")
names(age_disaggregated_4corr_2022) <- gsub(x = names(age_disaggregated_4corr_2022), pattern = ">=35 yrs", replacement = "ge35")

write.csv(age_disaggregated_4corr_2022,"age_disaggregated_4corr_2022.csv")

age_disaggregated_22to24 <- Full_data_2022%>%
  rename(HF='Health Facility',
         Council = 'District',
         Month = 'periodname')%>%
  mutate(ANC_re_ad = rowSums(pick(matches("ANC Wateja wa marudio .+$")),na.rm = TRUE),
         ANC_test = rowSums(pick(matches("ANC Waliopimwa Malaria kutumia mRDT/BS .+$")),na.rm = TRUE),
         ANC_pos = rowSums(pick(matches("ANC Waliogundulika Malaria positive .+$")),na.rm = TRUE),
         llin_provided = rowSums(pick(matches("ANC Waliopewa LLIN .+$")),na.rm = TRUE),
         ipt2_provided = rowSums(pick(matches("ANC Waliopewa IPT2 .+$")),na.rm = TRUE),
         ipt3_provided = rowSums(pick(matches("ANC Waliopewa IPT3 .+$")),na.rm = TRUE),
         ipt4_provided = rowSums(pick(matches("ANC Waliopewa IPT4 .+$")),na.rm = TRUE),
         Hb_test = rowSums(pick(matches("ANC Idadi ya Wajawazito Waliopima wingi wa damu .+$")),na.rm = TRUE),
         Anaemia = rowSums(pick(matches("ANC Upungufu mkubwa wa damu &amp;lt;8.5g/dl – Anaemia hudhurio la kwanza .+$")),na.rm = TRUE),
         Before_12wk = rowSums(pick(matches("ANC Umri wa mimba chini ya wiki 12 .+$")),na.rm = TRUE),
         After_12wk = rowSums(pick(matches("ANC Umri wa mimba wiki 12 au zaidi .+$")),na.rm = TRUE),
         Total_re_ad = rowSums(pick(Before_12wk,After_12wk,ANC_re_ad),na.rm=TRUE),
         Month = ifelse(Month %in% month.name,match(Month, month.name),Month)
  )%>%
  mutate(yearmon=as.yearmon(paste0(Year,"-",Month),"%Y-%m"))%>%
  filter(yearmon >= dhs_dates_2022[1] & yearmon <= dhs_dates_2022[2])%>%
  group_by_all()%>%
  filter(row_number()==1)%>%
  ungroup()

names(age_disaggregated_22to24) <- gsub(x = names(age_disaggregated_22to24), pattern = "ANC Wateja wa marudio ", replacement = "ANC_re_ad_")
names(age_disaggregated_22to24) <- gsub(x = names(age_disaggregated_22to24), pattern = "ANC Waliopimwa Malaria kutumia mRDT/BS ", replacement = "ANC_test_")
names(age_disaggregated_22to24) <- gsub(x = names(age_disaggregated_22to24), pattern = "ANC Waliogundulika Malaria positive ", replacement = "ANC_pos_")
names(age_disaggregated_22to24) <- gsub(x = names(age_disaggregated_22to24), pattern = "ANC Waliopewa LLIN ", replacement = "llin_provided_")
names(age_disaggregated_22to24) <- gsub(x = names(age_disaggregated_22to24), pattern = "ANC Waliopewa IPT2 ", replacement = "ipt2_provided_")
names(age_disaggregated_22to24) <- gsub(x = names(age_disaggregated_22to24), pattern = "ANC Waliopewa IPT3 ", replacement = "ipt3_provided_")
names(age_disaggregated_22to24) <- gsub(x = names(age_disaggregated_22to24), pattern = "ANC Waliopewa IPT4 ", replacement = "ipt4_provided_")
names(age_disaggregated_22to24) <- gsub(x = names(age_disaggregated_22to24), pattern = "ANC Idadi ya Wajawazito Waliopima wingi wa damu ", replacement = "Hb_test_")
names(age_disaggregated_22to24) <- gsub(x = names(age_disaggregated_22to24), pattern = "ANC Upungufu mkubwa wa damu &amp;lt;8.5g/dl – Anaemia hudhurio la kwanza ", replacement = "Anaemia_")
names(age_disaggregated_22to24) <- gsub(x = names(age_disaggregated_22to24), pattern = "ANC Umri wa mimba chini ya wiki 12 ", replacement = "Before_12wk_")
names(age_disaggregated_22to24) <- gsub(x = names(age_disaggregated_22to24), pattern = "ANC Umri wa mimba wiki 12 au zaidi ", replacement = "After_12wk")
names(age_disaggregated_22to24) <- gsub(x = names(age_disaggregated_22to24), pattern = " - ", replacement = "_")
names(age_disaggregated_22to24) <- gsub(x = names(age_disaggregated_22to24), pattern = " -", replacement = "_")
names(age_disaggregated_22to24) <- gsub(x = names(age_disaggregated_22to24), pattern = ">=35 yrs", replacement = "ge35")

write.csv(age_disaggregated_22to24,"age_disaggregated_22to24.csv")

#names(Full_data)
#
# #Full_data$Region_mb=Full_data$Region
# # Full_data_new$region[Full_data_new$region=="Songwe Region"]="Mbeya Region"
# dqa_fail <- data.frame()
# #### summarise at district level ##
# tanz_data_all_16to22_dist <-Full_data%>%
#   mutate(yearmon=as.yearmon(paste0(Full_data$Year,"-",Full_data$Month),"%Y-%m"))%>%
#   mutate(ANC_pos = ifelse(ANC_test!=0&is.na(ANC_pos),0,ANC_pos))%>%
#   filter((ANC_pos<=ANC_test))%>%
#   group_by(yearmon,Region,Council)%>%
#   dplyr::summarise(count=n(),positive=sum(ANC_pos, na.rm = TRUE),tested=sum(ANC_test, na.rm=TRUE),total=sum(Total_re_ad, na.rm = TRUE))
# write_csv(tanz_data_all_16to22_dist,"TZ_ANC_data_district_2016_2022.csv")
#
# tanz_data_all_16to22_region <-Full_data%>%
#   mutate(yearmon=as.yearmon(paste0(Full_data$Year,"-",Full_data$Month),"%Y-%m"))%>%
#   mutate(ANC_pos = ifelse(ANC_test!=0&is.na(ANC_pos),0,ANC_pos))%>%
#   filter((ANC_pos<=ANC_test))%>%
#   group_by(yearmon,Region)%>%
#   dplyr::summarise(count=n(),positive=sum(ANC_pos, na.rm = TRUE),tested=sum(ANC_test, na.rm=TRUE),total=sum(Total_re_ad, na.rm = TRUE))
# write_csv(tanz_data_all_16to22_region,"TZ_ANC_data_region_2016_2022.csv")
#
#
#
# tanz_data_all_16to22_region <- read_csv("./tanz/Patrick/processed_inputs/TZ_ANC_data_region_2016_2022.csv")
# tanz_data_list_16to22 <- tanz_data_process_allonly(tanz_data_all_16to22_region,remove_before = as.yearmon('Jan 2015'),level='Region')
# saveRDS(tanz_data_list_16to22,'./tanz/tanz_data_list_16to22.rds')
# tanz_data_list_16to22 <- readRDS('./tanz/tanz_data_list_16to22.rds')
#
# tanz_data_irs_16to22_dist_list <- tanz_data_process_allonly(tanz_data_irs_16to22_dist,level='District')
# saveRDS(tanz_data_irs_16to22_dist_list,'./tanz/tanz_data_irs_16to22_dist_list.rds')
# tanz_data_irs_16to22_dist_list <- readRDS('./tanz/tanz_data_irs_16to22_dist_list.rds')
#
# names(tanz_data_irs_16to22_dist_list)
# tanz_hist_prev_2015_list
#
# tanga_only <- tanz_data_all_14to22_district%>%
#   filter(Region=='Tanga Region')%>%
#   rename(region = Region,
#          council = Council)
# tanga_data_list_15to22 <- tanz_data_process_allonly(tanga_only,remove_before = as.yearmon('Jan 2015'),level='District')
#
# ##SMC candidate districts
# unique(tanz_data_all_16to22_dist[tanz_data_all_16to22_dist$Region=='Lindi Region',]$Council)
# lindi_smc_councils <- c("Kilwa District Council", "Liwale District Council","Ruangwa District Council")
# tanz_data_smc_lindi_16to22_dist <- tanz_data_all_16to22_dist%>%
#   filter(Council%in%lindi_smc_councils)
# tanz_data_lindi_16to22_dist_list <- tanz_data_process_allonly(tanz_data_smc_lindi_16to22_dist,level='District')
# saveRDS(tanz_data_lindi_16to22_dist_list,'./tanz/tanz_data_lindi_16to22_dist_list.rds')
# tanz_data_lindi_16to22_dist_list <- readRDS('./tanz/tanz_data_lindi_16to22_dist_list.rds')
#
# names(mc_smc_options)
# mc_smc_options <- readxl::read_xls('./tanz/MALARIA CONSORTIUM - TZ data.xls')%>%
#   mutate(Region=gsub(' Region','',Region))%>%
#   rename(smc_option=`SMC (O=Option)`,
#          strata_2022 = `Strata 2022`,
#          strata_urb_2022 = `Strata 2022 + Urban`,
#          inc_2022 = `2022 confirmed Incidence per 1000`,
#          pop_2022 = `Census 2022`,
#          cases_2022 = `2022 Confirmed Cases`)%>%
#   filter(smc_option%in%c('O1','O2'))
#
# o1_list <- mc_smc_options[mc_smc_options$smc_option=='O1','District']%>%
#   mutate(council = gsub(' DC',' District Council',District))%>%
#   mutate(council = gsub(' TC',' Town Council',council))%>%
#   mutate(council = gsub(' MC',' Municipal Council',council))
# o2_list <- mc_smc_options[mc_smc_options$smc_option=='O2','District']%>%
#   mutate(council = gsub(' DC',' District Council',District))%>%
#   mutate(council = gsub(' TC',' Town Council',council))%>%
#   mutate(council = gsub(' MC',' Municipal Council',council))
#
# tanz_data_smc_o1_16to22_dist <- tanz_data_all_16to22_dist%>%
#   mutate(council = gsub(' DC',' District Council',Council))%>%
#   mutate(council = gsub(' TC',' Town Council',council))%>%
#   mutate(council = gsub(' MC',' Municipal Council',council))%>%
#   right_join(o1_list,by='council')
# unique(tanz_data_smc_o1_16to22_dist$council)
# o1_regions <- tanz_data_smc_o1_16to22_dist[!duplicated(tanz_data_smc_o1_16to22_dist$Council),c('Council','Region')]
# o1_region_key <- gsub(' Region','',o1_regions$Region)
# names(o1_region_key) <- o1_regions$Council
#
# o1_region_key <-
#   tanz_data_smc_o1_16to22_dist <- addCIs(df=tanz_data_smc_o1_16to22_dist,Ys=tanz_data_smc_o1_16to22_dist$positive,Ns=tanz_data_smc_o1_16to22_dist$tested)
# tanz_data_smc_o2_16to22_dist <- tanz_data_all_16to22_dist%>%
#   mutate(council = gsub(' DC',' District Council',Council))%>%
#   mutate(council = gsub(' TC',' Town Council',council))%>%
#   mutate(council = gsub(' MC',' Municipal Council',council))%>%
#   right_join(o2_list,by='council')
# unique(tanz_data_smc_o2_16to22_dist$council)
# tanz_data_smc_o2_16to22_dist <- addCIs(df=tanz_data_smc_o2_16to22_dist,Ys=tanz_data_smc_o2_16to22_dist$positive,Ns=tanz_data_smc_o2_16to22_dist$tested)
# o2_regions <- tanz_data_smc_o2_16to22_dist[!duplicated(tanz_data_smc_o2_16to22_dist$Council),c('Council','Region')]
# o2_region_key <- gsub(' Region','',o2_regions$Region)
# names(o2_region_key) <- o2_regions$Council
# names(tanz_data_smc_o1_16to22_dist)
# o1_trend_plot <- ggplot(tanz_data_smc_o1_16to22_dist)+
#   geom_vline(xintercept = as.yearmon(c('Jan 2016','Jan 2017','Jan 2018','Jan 2019','Jan 2020','Jan 2021','Jan 2022')),linetype='dashed',color='#999999')+
#   geom_line(aes(x=as.yearmon(yearmon),y=mean))+
#   geom_errorbar(aes(x=as.yearmon(yearmon),ymin=lower,ymax=upper))+
#   facet_wrap(Region~District)+
#   labs(x='Month',y='ANC prevalence',title='SMC Option 1')
# ggsave(filename='./tanz/figures/tz_smc_o1_trend.tiff',plot=o1_trend_plot,units='cm',width=21,height=14,dpi=300)
# o2_trend_plot <- ggplot(tanz_data_smc_o2_16to22_dist)+
#   geom_vline(xintercept = as.yearmon(c('Jan 2016','Jan 2017','Jan 2018','Jan 2019','Jan 2020','Jan 2021','Jan 2022')),linetype='dashed',color='#999999')+
#   geom_line(aes(x=as.yearmon(yearmon),y=mean))+
#   geom_errorbar(aes(x=as.yearmon(yearmon),ymin=lower,ymax=upper))+
#   facet_wrap(Region~District)+
#   labs(x='Month',y='ANC prevalence',title='SMC Option 2')
# ggsave(filename='./tanz/figures/tz_smc_o2_trend.tiff',plot=o2_trend_plot,units='cm',width=21,height=14,dpi=300)
# tanz_data_smc_o1_16to22_dist_outliers <- tanz_data_smc_o1_16to22_dist %>%
#   group_by(Region,Council,District,year(as.yearmon(yearmon)))%>%
#   mutate(special_mean_positive = (sum(positive) - positive)/(n()-1),
#          special_mean_tested = (sum(tested) - tested)/(n()-1),
#          special_mean_prev_unwt = (sum(mean) - mean)/(n()-1),
#          special_mean_prev_wt = special_mean_positive/special_mean_tested,
#          prev_gt3xwtmean = (mean>=special_mean_prev_wt*3),
#          prev_gt3xunwtmean = (mean>=special_mean_prev_unwt*3),
#          prev_gt2xmax = (mean>=max(mean)*2),
#          prev_gtIQR = (mean>=(quantile(mean,probs=0.75)+1.5*IQR(mean))),
#          prev_gtMAD = (mean>=median(mean) + 3 * mad(mean)))
# o1_trend_plot <- ggplot(tanz_data_smc_o1_16to22_dist_outliers)+
#   geom_vline(xintercept = as.yearmon(c('Jan 2016','Jan 2017','Jan 2018','Jan 2019','Jan 2020','Jan 2021','Jan 2022')),linetype='dashed',color='#999999')+
#   geom_line(aes(x=as.yearmon(yearmon),y=mean))+
#   geom_point(aes(x=as.yearmon(yearmon),y=mean,color=prev_gt3xwtmean))+
#   geom_errorbar(aes(x=as.yearmon(yearmon),ymin=lower,ymax=upper,color=prev_gt3xwtmean))+
#   facet_wrap(Region~District)+
#   labs(x='Month',y='ANC prevalence',title='SMC Option 1')
# o1_dist_outliers <- tanz_data_smc_o1_16to22_dist_outliers%>%
#   filter(prev_gt3xunwtmean)
#
# tanz_data_smc_o2_16to22_dist_outliers <- tanz_data_smc_o2_16to22_dist %>%
#   group_by(Region,Council,District,year(as.yearmon(yearmon)))%>%
#   mutate(special_mean_positive = (sum(positive) - positive)/(n()-1),
#          special_mean_tested = (sum(tested) - tested)/(n()-1),
#          special_mean_prev_unwt = (sum(mean) - mean)/(n()-1),
#          special_mean_prev_wt = special_mean_positive/special_mean_tested,
#          prev_gt3xwtmean = (mean>=special_mean_prev_wt*3),
#          prev_gt3xunwtmean = (mean>=special_mean_prev_unwt*3),
#          prev_gt2xmax = (mean>=max(mean)*2),
#          prev_gtIQR = (mean>=(quantile(mean,probs=0.75)+1.5*IQR(mean))),
#          prev_gtMAD = (mean>=median(mean) + 3 * mad(mean)))
# o2_trend_plot <- ggplot(tanz_data_smc_o2_16to22_dist_outliers)+
#   geom_vline(xintercept = as.yearmon(c('Jan 2016','Jan 2017','Jan 2018','Jan 2019','Jan 2020','Jan 2021','Jan 2022')),linetype='dashed',color='#999999')+
#   geom_line(aes(x=as.yearmon(yearmon),y=mean))+
#   geom_point(aes(x=as.yearmon(yearmon),y=mean,color=prev_gt3xwtmean))+
#   geom_errorbar(aes(x=as.yearmon(yearmon),ymin=lower,ymax=upper,color=prev_gt3xwtmean))+
#   facet_wrap(Region~District)+
#   labs(x='Month',y='ANC prevalence',title='SMC Option 2')
# o2_dist_outliers <- tanz_data_smc_o2_16to22_dist_outliers%>%
#   filter(prev_gt3xunwtmean)
#
# o1_dist_outliers_hf <- Full_data %>%
#   mutate(yearmon=as.yearmon(paste0(Full_data$Year,"-",Full_data$Month),"%Y-%m"))%>%
#   filter(Council %in% o1_dist_outliers$Council)%>%
#   filter(!is.na(ANC_test))%>%
#   filter(ANC_test != 0)%>%
#   mutate(ANC_pos = ifelse(ANC_test!=0&is.na(ANC_pos),0,ANC_pos))%>%
#   filter((ANC_pos<=ANC_test))%>%
#   mutate(prev=ANC_pos/ANC_test)
#
# tanga <- o1_dist_outliers_hf[o1_dist_outliers_hf$Region=='Tanga Region',]
# newala <-o1_dist_outliers_hf[o1_dist_outliers_hf$Council=='Newala Town Council',]
# litembo <- o1_dist_outliers_hf[o1_dist_outliers_hf$HF=='LITEMBO Other Hospital',]
# o1_trend_plot_hf <- ggplot(o1_dist_outliers_hf[o1_dist_outliers_hf$Council=='Newala Town Council',])+
#   geom_vline(xintercept = as.yearmon(c('Jan 2016','Jan 2017','Jan 2018','Jan 2019','Jan 2020','Jan 2021','Jan 2022')),linetype='dashed',color='#999999')+
#   geom_point(aes(x=as.yearmon(yearmon),y=Total_re_ad))+
#   facet_wrap(.~HF)+
#   labs(x='Month',y='ANC prevalence',title='SMC Option 1')
#
# tanz_data_smc_o1_16to22_dist_list <- tanz_data_process_allonly(tanz_data_smc_o1_16to22_dist,level='District')
# saveRDS(tanz_data_smc_o1_16to22_dist_list,'./tanz/tanz_data_smc_o1_16to22_dist_list.rds')
# tanz_data_smc_o1_16to22_dist_list <- readRDS('./tanz/tanz_data_smc_o1_16to22_dist_list.rds')
# tanz_data_smc_o2_16to22_dist_list <- tanz_data_process_allonly(tanz_data_smc_o2_16to22_dist,level='District')
# saveRDS(tanz_data_smc_o2_16to22_dist_list,'./tanz/tanz_data_smc_o2_16to22_dist_list.rds')
