##################################WITH UTKARSH
##for mm analysis
##interquartimes quantiles
#  summarize(median_age= median(diag_age),q1_age=quantile(diag_age, probs=c(0.25)),
#q2_age=quantile(diag_age, probs=c(0.5)),q3_age=quantile(diag_age, probs=c(0.75)))

mm_cohort_data2 <- arrange(mm_cohort_data2, PROCHI, ADMISSION_DATE)
temp <- mm_cohort_data2 %>%
  group_by(PROCHI) %>%
  mutate(num_of_cond = 1:n()) %>%
  ungroup()

temp2 <-filter(temp, num_of_cond==2)  ## select only those with 2 conditions and use their age at diag to determine age at onset of mm
length(unique(temp4$PROCHI)) ## unique individuals with mm 

temp_total <- temp2 %>%
  group_by(adm_year) %>%
  mutate(cond_total_year = n()) %>% ## to determine yearly totals for the incidence of mm
  ungroup()

temp_total <-select(temp_total,PROCHI,cond_total_year)
temp_joined<-left_join(temp,temp_total)


##alternatively
temp <- mm_cohort_data2 %>%
  group_by(PROCHI) %>%
  mutate(num_of_cond = 1:n()) %>%
  ungroup()
temp2 <-filter(temp, num_of_cond==2)## select only those with 2 conditions and use their age at diag to determine age at onset of mm
temp_total <- temp2 %>%
  group_by(adm_year) %>%
  mutate(cond_total_year = n()) %>%
  ungroup()
temp_total_b<-select(temp_total,PROCHI,num_of_cond,cond_total_year)
temp_joined<-left_join(temp,temp_total_b)

temp_mm_contributors <-filter(temp_joined, num_of_cond<=2)# filter out those that contributed to mm for clustering
temp_mm_contributors <- filter(temp_mm_contributors, mm=="2")

table(temp_joined$diag_age3,temp_joined$adm_year)##year by age at onset of MM
table(temp2$diag_age3,temp2$adm_year)##year by age at onset of mm
table(temp_joined$diag_age3,temp_joined$adm_year)##year by number of occurrence
table(temp_joined$adm_year,temp_joined$cond_total_year)##yearly incidence

##############################
#####count conditions and age at onset of MM

###diag_age is more correct than diag_age3
ttt<-filter(temp_total)
result_mm_onset_all<-ttt %>% ##count conditions and age at onset of MM
  group_by() %>% 
  summarize(number= n(),median_age3= median(diag_age3),
            min_age= min(diag_age3), max_age= max(diag_age3))

result_mm_onset_sex<-ttt %>% ##count conditions and age at onset of MM
  group_by(sex) %>% 
  summarize(number= n(),median_age3= median(diag_age3),
            min_age= min(diag_age3), max_age= max(diag_age3))

result_mm_onset_age<-ttt %>% ##count conditions and age at onset of MM
  group_by(ageg) %>% 
  summarize(number= n(),median_age3= median(diag_age3),
            min_age= min(diag_age3), max_age= max(diag_age3))

result_mm_onset_age_sex<-ttt %>% ##count conditions and age at onset of MM
  group_by(ageg,sex) %>% 
  summarize(number= n(),median_age3= median(diag_age3),
            min_age= min(diag_age3), max_age= max(diag_age3))



result_mm_onset_sex<-ttt %>% ##count conditions and age at onset of MM
  group_by(sex) %>% 
  summarize(number= n(),median_age3= median(diag_age3),
            min_age= min(diag_age3), max_age= max(diag_age3))

result_mm_onset_dep<-ttt %>% ##count conditions and age at onset of MM
  group_by(dep_sc) %>% 
  summarize(number= n(),median_age3= median(diag_age3),
            min_age= min(diag_age3), max_age= max(diag_age3))

result_mm_onset_dep_sex<-ttt %>% ##count conditions and age at onset of MM
  group_by(dep_sc,sex) %>% 
  summarize(number= n(),median_age3= median(diag_age3),
            min_age= min(diag_age3), max_age= max(diag_age3))


###########################
##new conditions per year     yearly incidence of mm
result_mm_inc_all<-temp_total %>% ##new conditions per year
  group_by(adm_year) %>% 
  summarize(number= n())

ttt<-filter(temp_total, sex=="F")
result_mm_inc_sex_f<-ttt %>% ##new conditions per year
  group_by(adm_year,sex) %>% 
  summarize(number= n())

ttt<-filter(temp_total, sex=="M")
result_mm_inc_sex_m<-ttt %>% ##new conditions per year
  group_by(adm_year,sex) %>% 
  summarize(number= n())

ttt<-filter(temp_total, ageg=="_50")
result_mm_inc_age1<-ttt %>% ##new conditions per year
  group_by(adm_year,ageg) %>% 
  summarize(number= n())

ttt<-filter(temp_total, ageg=="50_59")
result_mm_inc_age2<-ttt %>% ##new conditions per year
  group_by(adm_year,ageg) %>% 
  summarize(number= n())

ttt<-filter(temp_total, ageg=="60_69")
result_mm_inc_age3<-ttt %>% ##new conditions per year
  group_by(adm_year,ageg) %>% 
  summarize(number= n())

ttt<-filter(temp_total, ageg=="70_79")
result_mm_inc_age4<-ttt %>% ##new conditions per year
  group_by(adm_year,ageg) %>% 
  summarize(number= n())

ttt<-filter(temp_total, ageg=="_80")
result_mm_inc_age5<-ttt %>% ##new conditions per year
  group_by(adm_year,ageg) %>% 
  summarize(number= n())

ttt<-filter(temp_total, dep_sc=="1")
result_mm_inc_dep1<-ttt %>% ##new conditions per year
  group_by(adm_year,dep_sc) %>% 
  summarize(number= n())

ttt<-filter(temp_total, dep_sc=="5")
result_mm_inc_dep5<-ttt %>% ##new conditions per year
  group_by(adm_year,dep_sc) %>% 
  summarize(number= n())

###
#sex and age

ttt<-filter(temp_total, sex=="F" & ageg=="_50")
result_mm_inc_agef1<-ttt %>% ##new conditions per year
  group_by(adm_year,ageg,sex) %>% 
  summarize(number= n())

ttt<-filter(temp_total, sex=="F" & ageg=="50_59")
result_mm_inc_agef2<-ttt %>% ##new conditions per year
  group_by(adm_year,ageg,sex) %>% 
  summarize(number= n())

ttt<-filter(temp_total, sex=="F" & ageg=="60_69")
result_mm_inc_agef3<-ttt %>% ##new conditions per year
  group_by(adm_year,ageg,sex) %>% 
  summarize(number= n())

ttt<-filter(temp_total, sex=="F" & ageg=="70_79")
result_mm_inc_agef4<-ttt %>% ##new conditions per year
  group_by(adm_year,ageg,sex) %>% 
  summarize(number= n())

ttt<-filter(temp_total, sex=="F" & ageg=="_80")
result_mm_inc_agef5<-ttt %>% ##new conditions per year
  group_by(adm_year,ageg,sex) %>% 
  summarize(number= n())


##sex=m by age dep


ttt<-filter(temp_total, sex=="M" & ageg=="_50")
result_mm_inc_agem1<-ttt %>% ##new conditions per year
  group_by(adm_year,ageg,sex) %>% 
  summarize(number= n())

ttt<-filter(temp_total, sex=="M" & ageg=="50_59")
result_mm_inc_agem2<-ttt %>% ##new conditions per year
  group_by(adm_year,ageg,sex) %>% 
  summarize(number= n())

ttt<-filter(temp_total, sex=="M" & ageg=="60_69")
result_mm_inc_agem3<-ttt %>% ##new conditions per year
  group_by(adm_year,ageg,sex) %>% 
  summarize(number= n())

ttt<-filter(temp_total, sex=="M" & ageg=="70_79")
result_mm_inc_agem4<-ttt %>% ##new conditions per year
  group_by(adm_year,ageg,sex) %>% 
  summarize(number= n())

ttt<-filter(temp_total, sex=="M" & ageg=="_80")
result_mm_inc_agem5<-ttt %>% ##new conditions per year
  group_by(adm_year,ageg,sex) %>% 
  summarize(number= n())

##sex and dep

ttt<-filter(temp_total, sex=="F" & dep_sc=="1")
result_mm_inc_depf1<-ttt %>% ##new conditions per year
  group_by(adm_year,dep_sc,sex) %>% 
  summarize(number= n())

ttt<-filter(temp_total, sex=="F" & dep_sc=="5")
result_mm_inc_depf5<-ttt %>% ##new conditions per year
  group_by(adm_year,dep_sc,sex) %>% 
  summarize(number= n())


ttt<-filter(temp_total, sex=="M" & dep_sc=="1")
result_mm_inc_depm1<-ttt %>% ##new conditions per year
  group_by(adm_year,dep_sc,sex) %>% 
  summarize(number= n())

ttt<-filter(temp_total, sex=="M" & dep_sc=="5")
result_mm_inc_depm5<-ttt %>% ##new conditions per year
  group_by(adm_year,dep_sc,sex) %>% 
  summarize(number= n())


ttt<-filter(temp_total, ageg=="_50" & dep_sc=="1")
result_mm_inc_age1_dep1<-ttt %>% ##new conditions per year
  group_by(adm_year,dep_sc,ageg) %>% 
  summarize(number= n())

ttt<-filter(temp_total, ageg=="_50" & dep_sc=="5")
result_mm_inc_age1_dep5<-ttt %>% ##new conditions per year
  group_by(adm_year,dep_sc,ageg) %>% 
  summarize(number= n())

ttt<-filter(temp_total, ageg=="_80" & dep_sc=="1")
result_mm_inc_age5_dep1<-ttt %>% ##new conditions per year
  group_by(adm_year,dep_sc,ageg) %>% 
  summarize(number= n())

ttt<-filter(temp_total, ageg=="_80" & dep_sc=="5")
result_mm_inc_age5_dep5<-ttt %>% ##new conditions per year
  group_by(adm_year,dep_sc,ageg) %>% 
  summarize(number= n())


mm_inc_yearly<-cbind.data.frame(result_mm_inc_all,result_mm_inc_sex_f,result_mm_inc_sex_m,result_mm_inc_age1,
                                 result_mm_inc_age2,result_mm_inc_age3,result_mm_inc_age4,result_mm_inc_age5,
                                 result_mm_inc_dep1,result_mm_inc_dep5,result_mm_inc_agef1,result_mm_inc_agef2,
                                 result_mm_inc_agef3,result_mm_inc_agef4,result_mm_inc_agef5,
                                 result_mm_inc_depf1,result_mm_inc_depf5,result_mm_inc_agem1,
                                 result_mm_inc_agem2,result_mm_inc_agem3,result_mm_inc_agem4,result_mm_inc_agem5,
                                 result_mm_inc_depm1,result_mm_inc_depm5,result_mm_inc_age1_dep1,
                                 result_mm_inc_age5_dep1,result_mm_inc_age5_dep5)
mm_inc_yearly_dep1_age5<-cbind.data.frame(result_mm_inc_age1_dep5)

rm(result_mm_inc_all,result_mm_inc_sex_f,result_mm_inc_sex_m,result_mm_inc_age1,
   result_mm_inc_age2,result_mm_inc_age3,result_mm_inc_age4,result_mm_inc_age5,
   result_mm_inc_dep1,result_mm_inc_dep5,result_mm_inc_agef1,result_mm_inc_agef2,
   result_mm_inc_agef3,result_mm_inc_agef4,result_mm_inc_agef5,
   result_mm_inc_depf1,result_mm_inc_depf5,result_mm_inc_agem1,
   result_mm_inc_agem2,result_mm_inc_agem3,result_mm_inc_agem4,result_mm_inc_agem5,
   result_mm_inc_depm1,result_mm_inc_depm5,result_mm_inc_age1_dep1,
   result_mm_inc_age1_dep5,result_mm_inc_age5_dep1,result_mm_inc_age5_dep5)
fwrite(mm_inc_yearly, file="mm_inc_yearly.csv",sep=",")
fwrite(mm_inc_yearly_dep1_age5, file="mm_inc_yearly_dep1_age5.csv",sep=",")