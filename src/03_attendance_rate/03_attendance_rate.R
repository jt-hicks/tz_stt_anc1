orderly::orderly_dependency("02_data_quality", quote(latest()),
                             c('dqa_hf_remove_duplicates.rds'))
orderly::orderly_shared_resource('theme_base.R')
# orderly::orderly_shared_resource('District edited Jan 2021')
orderly::orderly_shared_resource('data/tz_population_estimates_2022.csv')
orderly::orderly_shared_resource('data/dhs_fertility_by_region.csv')
orderly::orderly_shared_resource('data/dhs_table3_1_women15to49_updated.csv')
orderly::orderly_shared_resource('data/dhs_table5_14_pregnancyoutcome.csv')
orderly::orderly_shared_resource('data/dhs_table5_14_pregnancyoutcome.csv')
orderly::orderly_shared_resource('data/tz_census_pop_growth.csv')
orderly::orderly_shared_resource('Province_grid.csv')
orderly::orderly_artefact(files=c('att_coverage_composite_final.png'),
                           description = 'figures')

source('theme_base.R')
library(colorblindr)

# Define colors from OkabeIto palette for consistent coloring
oi_colors <- colorblindr::palette_OkabeIto
color_attendance <- oi_colors[1]  # First color for attendance
color_coverage <- oi_colors[2]     # Second color for coverage

dqa_hf <- readRDS('dqa_hf_remove_duplicates.rds')

# councils <- st_read("District edited Jan 2021/District edited Jan 2021.shp") # Region sf

##Aggregate polygons to region level
# sf_use_s2(FALSE)
# regions <- councils %>%
#   group_by(Region_Nam) %>%
#   summarise()%>%
#   ungroup()%>%
#   mutate(Region = ifelse(Region_Nam=='Dar-es-salaam','Dar Es Salaam',Region_Nam))

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
fertility_numpregs <- read.csv('data/dhs_table5_14_pregnancyoutcome.csv')%>%
  select(Region,num_preg_last3years)%>%
  mutate(Region = ifelse(Region=='Dar es Salaam','Dar Es Salaam',Region))
fertility_numwomen <- read.csv('data/dhs_table3_1_women15to49_updated.csv')
fertility_numwomen$Region <- ifelse(fertility_numwomen$Region=='Dar es Salaam','Dar Es Salaam',fertility_numwomen$Region)
fert_raw <- inner_join(fertility_numpregs,fertility_numwomen,by='Region')
all_fert_2022 <- inner_join(fertility_2022,fert_raw,by='Region')

##Calculate monthly population size using growth rate estimates
##Calculate change in fertility rates using change from 2016 to 2022 total fertility rate
##DHS survey data to calculate fertility
##Table 12.9 (pg 401) DHS report 2022 = number of women with still and livebirths in the preceding 2 years (also table 2.12)
##Need number of women by region in reproductive age Table 3.1?? (#women 15-49)
##Number of women age 15+ in table 2.17.1

census_with_fert <- inner_join(census_2022,all_fert_2022,by='Region')
# nrow(census_with_fert)
#https://pmc.ncbi.nlm.nih.gov/articles/PMC6534308/
census_with_fert$mean_fertility_rate <- census_with_fert$Total.fertility.rate / (5*7)
census_with_fert$monthly_fertility_rate <- census_with_fert$mean_fertility_rate / 12
census_with_fert$expected_monthly_pregnancies_1 <- census_with_fert$female_population * census_with_fert$percent_women_reproage/100 * census_with_fert$monthly_fertility_rate
census_with_fert$preg_per_woman_3yrs_wt <- census_with_fert$num_preg_last3years/census_with_fert$women_weightednumber
census_with_fert$preg_per_woman_monthly <- census_with_fert$preg_per_woman_3yrs_wt/36
census_with_fert$expected_monthly_pregnancies_2 <- census_with_fert$female_population * census_with_fert$percent_women_reproage/100 * census_with_fert$preg_per_woman_monthly

# mean(census_with_fert$expected_monthly_pregnancies)

growth_rates <- read.csv("data/tz_census_pop_growth.csv", stringsAsFactors = FALSE)%>%
  mutate(Region = gsub('Dar es Salaam','Dar Es Salaam',region)) %>%
  select(Region, growthrate_2022)

# join growth rates into your fertility + census table
census_with_fert <- census_with_fert %>%
  left_join(
    growth_rates %>%
      select(Region, growthrate_2022),
    by = "Region"
  ) %>%
  mutate(
    annual_growth  = growthrate_2022 / 100,                    # fraction per year
    monthly_growth = (1 + annual_growth)^(1/12) - 1           # fraction per month
  )

ref_date <- as.Date("2022-07-01")

loss_factor <- 1.1 ##accounts for missed pregnancies (fetal loss and induced abortions)


# ggplot(census_with_fert)+
#   geom_col(aes(x=fct_rev(fct_reorder(Region, expected_monthly_pregnancies_2)),y=expected_monthly_pregnancies_2/1000))+
#   scale_y_continuous(limits=c(0,max(census_with_fert$expected_monthly_pregnancies_2)/1000+3),expand=c(0,0))+
#   labs(x='Region',y='Expected Monthly Pregnancies (x10,000)')+
#   coord_flip()

##Summarise anc data by region
##Sumarise by month and council
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

dqa_region_monthly_dyn <- dqa_region_monthly %>%
  # make sure Region naming is aligned
  left_join(
    census_with_fert %>%
      select(
        Region,
        expected_monthly_pregnancies_2,
        monthly_growth
      ),
    by = "Region"
  ) %>%
  mutate(
    date      = as.Date(yearmon),  # if yearmon is a yearmon object
    # approx number of months from the reference date (can use lubridate::interval if you prefer)
    months_from_ref = as.integer(round(
      as.numeric(date - ref_date) / 30.4375    # average days per month
    )),

    # dynamic expected pregnancies:
    # value at 2022 * (1 + r_month)^(months_from_ref)
    expected_monthly_pregnancies_dyn =
      expected_monthly_pregnancies_2 * (1 + monthly_growth)^(months_from_ref),

    # ANC1 coverage using time-varying denominator
    anc1_coverage = total_first_att_sum / expected_monthly_pregnancies_dyn
  )%>%
  mutate(
    anc1_att_10k     = total_first_att_sum / 1e4,
    expected_preg_10k = (expected_monthly_pregnancies_dyn * loss_factor) / 1e4
  )

province_grid<-read.csv("Province_grid.csv") %>%
  mutate(Region=NAME_1,
         code=NAME_1,
         name=NAME_1)

# att_with_expected_births <- ggplot(data=dqa_region_monthly)+
#   geom_line(aes(x=as.Date(yearmon),y=total_first_att_sum/10000), colour = color_attendance)+
#   geom_hline(data = census_with_fert,aes(yintercept = expected_monthly_pregnancies_1/10000),color='darkgrey',linetype='dashed',linewidth=1)+
#   scale_x_date(date_breaks='3 years',date_labels="'%y")+
#   scale_y_continuous(limits=c(0,2.0),expand=c(0,0))+
#   labs(y='Number of Women Attending ANC1 (x10,000)')+
#   facet_geo(~ Region, grid = province_grid%>%
#               select(row,col,code,name))+
#   theme(axis.title.x = element_blank())
# ggsave(att_with_expected_births,file='att_with_expected_births_1.png',units='in',height=7,width = 7)
# att_with_expected_births_2 <- ggplot(data=dqa_region_monthly)+
#   geom_line(aes(x=as.Date(yearmon),y=total_first_att_sum/10000), colour = color_attendance)+
#   geom_hline(data = census_with_fert,aes(yintercept = expected_monthly_pregnancies_2/10000),color='darkgrey',linetype='dashed',linewidth=1)+
#   scale_x_date(date_breaks='3 years',date_labels="'%y")+
#   scale_y_continuous(limits=c(0,2.0),expand=c(0,0))+
#   labs(y='Number of Women Attending ANC1 (x10,000)')+
#   facet_geo(~ Region, grid = province_grid%>%
#               select(row,col,code,name))+
#   theme(axis.title.x = element_blank())
# ggsave(att_with_expected_births_2,file='att_with_expected_births_2.png',units='in',height=7,width = 7)

##Average number of malaria tests nationwide in 2024
avg_tests_2024 <- dqa_region_monthly_dyn%>%
  filter(year(date)==2024)%>%
  group_by(yearmon)%>%
  summarise(ANC_test_sum=sum(ANC_test_sum,na.rm=TRUE))%>%
  ungroup()%>%
  summarise(avg_tests = mean(ANC_test_sum,na.rm=TRUE))%>%
  pull(avg_tests)
cat(paste("Average number of malaria tests nationwide in 2024:", round(avg_tests_2024), "\n"))

## Summarise expected pregnancies annually at region level
dqa_region_annual_dyn <- dqa_region_monthly_dyn %>%
  mutate(year = year(date)) %>%
  group_by(Region, year) %>%
  summarise(
    # observed ANC1 over the year
    anc1_att_year = sum(total_first_att_sum, na.rm = TRUE),

    # expected pregnancies over the year (sum over 12 months)
    expected_preg_year = sum(expected_monthly_pregnancies_dyn, na.rm = TRUE) * loss_factor,

    # annual ANC1 coverage
    anc1_coverage_year = anc1_att_year / expected_preg_year,
    .groups = "drop"
  )

## National-level monthly dynamic data
dqa_national_monthly_dyn <- dqa_region_monthly_dyn %>%
  group_by(yearmon, date, months_from_ref) %>%
  summarise(
    ANC_pos_sum = sum(ANC_pos_sum, na.rm = TRUE),
    ANC_test_sum = sum(ANC_test_sum, na.rm = TRUE),
    ANC_test_sum_nonmissing = sum(ANC_test_sum_nonmissing, na.rm = TRUE),
    Before_12wk_sum = sum(Before_12wk_sum, na.rm = TRUE),
    After_12wk_sum = sum(After_12wk_sum, na.rm = TRUE),
    total_first_att_sum = sum(total_first_att_sum, na.rm = TRUE),
    ANC_re_ad_sum = sum(ANC_re_ad_sum, na.rm = TRUE),
    sum_llin_provided = sum(sum_llin_provided, na.rm = TRUE),
    sum_ipt2_provided = sum(sum_ipt2_provided, na.rm = TRUE),
    sum_ipt3_provided = sum(sum_ipt3_provided, na.rm = TRUE),
    sum_ipt4_provided = sum(sum_ipt4_provided, na.rm = TRUE),
    sum_Hb_test = sum(sum_Hb_test, na.rm = TRUE),
    sum_Anaemia = sum(sum_Anaemia, na.rm = TRUE),
    expected_monthly_pregnancies_dyn = sum(expected_monthly_pregnancies_dyn, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    prevalence = ANC_pos_sum / ANC_test_sum_nonmissing,
    anc1_coverage = total_first_att_sum / expected_monthly_pregnancies_dyn,
    anc1_att_10k = total_first_att_sum / 1e4,
    expected_preg_10k = (expected_monthly_pregnancies_dyn * loss_factor) / 1e4
  )

## National-level annual dynamic data
dqa_national_annual_dyn <- dqa_national_monthly_dyn %>%
  mutate(year = year(date)) %>%
  group_by(year) %>%
  summarise(
    anc1_att_year = sum(total_first_att_sum, na.rm = TRUE),
    expected_preg_year = sum(expected_monthly_pregnancies_dyn, na.rm = TRUE) * loss_factor,
    anc1_coverage_year = anc1_att_year / expected_preg_year,
    .groups = "drop"
  )

dqa_national_annual <- dqa_region_annual_dyn %>%
  group_by(year) %>%
  summarise(
    anc1_att_year       = sum(anc1_att_year, na.rm = TRUE),
    expected_preg_year  = sum(expected_preg_year, na.rm = TRUE),
    anc1_coverage_year  = anc1_att_year / expected_preg_year,
    .groups = "drop"
  )

dqa_national_monthly <- dqa_hf%>%
  group_by(yearmon)%>%
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
  ungroup()

# Create a composite figure with regional and national plots
# Save individually with matching aesthetics, then combine as images
library(magick)

# Define consistent theme for both regional and national plots
consistent_theme <- theme(
  axis.title.x = element_blank(),
  plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
  legend.position = "bottom"
)

# Regional monthly plot with consistent theme
att_with_expected_births_dyn <- ggplot(data = dqa_region_monthly_dyn) +
  geom_line(aes(x = date, y = anc1_att_10k), colour = color_attendance, linewidth = 1) +
  geom_line(aes(x = date, y = expected_preg_10k),
            colour = "grey20", linetype = "dashed", linewidth = 1) +
  scale_x_date(date_breaks = "3 years", date_labels = "'%y") +
  scale_y_continuous(limits = c(0, 2.0), expand = c(0, 0)) +
  labs(y = "Number of Women (x10,000)") +
  facet_geo(
    ~ Region,
    grid = province_grid %>% select(row, col, code, name)
  ) +
  theme(axis.title.x = element_blank())

regional_monthly_final <- att_with_expected_births_dyn + 
  labs(title = "Regional") +
  consistent_theme

# Save regional plot
ggsave(regional_monthly_final, file = 'att_with_expected_births_dyn_titled.png', 
       units = 'in', height = 7, width = 7, dpi = 300)

# National monthly plot with matching aesthetics
national_monthly_final <- ggplot(data = dqa_national_monthly_dyn) +
  geom_line(aes(x = date, y = anc1_att_10k), colour = color_attendance, linewidth = 1) +
  geom_line(aes(x = date, y = expected_preg_10k),
            colour = "grey20", linetype = "dashed", linewidth = 1) +
  scale_x_date(date_breaks = "1 year", date_labels = "'%y") +
  scale_y_continuous(limits = c(0, 25), expand = c(0, 0)) +
  labs(y = "Number of Women (x10,000)", title = "National") +
  consistent_theme

# Save national plot with same dimensions
ggsave(national_monthly_final, file = 'att_with_expected_births_national_monthly_final.png', 
       units = 'in', height = 3.5, width = 7, dpi = 300)

# Annual plots with same approach
annual_anc_coverage <- ggplot(data=dqa_region_annual_dyn)+
  geom_point(aes(x=year,y=anc1_coverage_year), colour = color_coverage, size = 1.5)+
  geom_hline(yintercept=1,color='darkgrey',linetype='dashed',linewidth=1)+
  scale_x_continuous(breaks=seq(2015,2025,by=3), labels = function(x) sprintf("'%02d", x %% 100))+
  scale_y_continuous(limits=c(.5,1.5),expand=c(0,0))+
  labs(y='Annual ANC1 Coverage')+
  facet_geo(~ Region, grid = province_grid%>%
              select(row,col,code,name))+
  theme(axis.title.x = element_blank())

regional_annual_final <- annual_anc_coverage + 
  labs(title = "Regional") +
  consistent_theme

ggsave(regional_annual_final, file = 'annual_anc_coverage_titled.png', 
       units = 'in', height = 7, width = 7, dpi = 300)

national_annual_final <- ggplot(dqa_national_annual_dyn) +
  geom_point(aes(x = year, y = anc1_coverage_year), colour = color_coverage, size = 3) +
  geom_hline(yintercept = 1, color = 'darkgrey', linetype = 'dashed', linewidth = 1) +
  scale_x_continuous(breaks = seq(2014, 2025, by = 1), labels = function(x) sprintf("'%02d", x %% 100)) +
  scale_y_continuous(limits = c(.5, 1.5), expand = c(0, 0)) +
  labs(y = 'Annual ANC1 Coverage', title = "National") +
  consistent_theme

ggsave(national_annual_final, file = 'annual_anc_coverage_national_final.png', 
       units = 'in', height = 3.5, width = 7, dpi = 300)

# Create a single 2x2 composite with attendance on left, coverage on right
# Load all four individual plot images
regional_att_img <- magick::image_read('att_with_expected_births_dyn_titled.png')
national_att_img <- magick::image_read('att_with_expected_births_national_monthly_final.png')
regional_cov_img <- magick::image_read('annual_anc_coverage_titled.png')
national_cov_img <- magick::image_read('annual_anc_coverage_national_final.png')

# Add letter annotations to each plot
regional_att_labeled <- magick::image_annotate(
  regional_att_img, "A", size = 60, color = "black", 
  location = "+30+30", font = "sans", weight = 700
)

national_att_labeled <- magick::image_annotate(
  national_att_img, "B", size = 60, color = "black",
  location = "+30+30", font = "sans", weight = 700
)

regional_cov_labeled <- magick::image_annotate(
  regional_cov_img, "C", size = 60, color = "black",
  location = "+30+30", font = "sans", weight = 700
)

national_cov_labeled <- magick::image_annotate(
  national_cov_img, "D", size = 60, color = "black",
  location = "+30+30", font = "sans", weight = 700
)

# Combine attendance plots (top row, left column)
attendance_column <- magick::image_append(
  c(regional_att_labeled, national_att_labeled),
  stack = TRUE
)

# Combine coverage plots (top row, right column)
coverage_column <- magick::image_append(
  c(regional_cov_labeled, national_cov_labeled),
  stack = TRUE
)

# Combine columns side by side
composite_final <- magick::image_append(
  c(attendance_column, coverage_column),
  stack = FALSE
)

# Save the final composite
magick::image_write(composite_final, path = 'att_coverage_composite_final.png')

# Clean up interim individual plot files
file.remove('att_with_expected_births_dyn_titled.png',
            'att_with_expected_births_national_monthly_final.png',
            'annual_anc_coverage_titled.png',
            'annual_anc_coverage_national_final.png')
