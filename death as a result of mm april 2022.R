### This is to compute fatality (people that had mm and died in preceding years so as 
###to correctly estimate the numerator of the prevalence)
###follow up from the age at diagnosis incidence for mm april 2022
########################################################################################
##new conditions per year     yearly incidence of mm
result_mm_death_all<-temp_total %>% ##new conditions per year
  group_by(death_year) %>% 
  summarize(number= n())

ttt<-filter(temp_total, sex=="F")
result_mm_death_sex_f<-ttt %>% ##new conditions per year
  group_by(death_year,sex) %>% 
  summarize(number= n())

ttt<-filter(temp_total, sex=="M")
result_mm_death_sex_m<-ttt %>% ##new conditions per year
  group_by(death_year,sex) %>% 
  summarize(number= n())

ttt<-filter(temp_total, ageg=="_50")
result_mm_death_age1<-ttt %>% ##new conditions per year
  group_by(death_year,ageg) %>% 
  summarize(number= n())

ttt<-filter(temp_total, ageg=="50_59")
result_mm_death_age2<-ttt %>% ##new conditions per year
  group_by(death_year,ageg) %>% 
  summarize(number= n())

ttt<-filter(temp_total, ageg=="60_69")
result_mm_death_age3<-ttt %>% ##new conditions per year
  group_by(death_year,ageg) %>% 
  summarize(number= n())

ttt<-filter(temp_total, ageg=="70_79")
result_mm_death_age4<-ttt %>% ##new conditions per year
  group_by(death_year,ageg) %>% 
  summarize(number= n())

ttt<-filter(temp_total, ageg=="_80")
result_mm_death_age5<-ttt %>% ##new conditions per year
  group_by(death_year,ageg) %>% 
  summarize(number= n())

ttt<-filter(temp_total, dep_sc=="1")
result_mm_death_dep1<-ttt %>% ##new conditions per year
  group_by(death_year,dep_sc) %>% 
  summarize(number= n())

ttt<-filter(temp_total, dep_sc=="5")
result_mm_death_dep5<-ttt %>% ##new conditions per year
  group_by(death_year,dep_sc) %>% 
  summarize(number= n())

###
#sex and age

ttt<-filter(temp_total, sex=="F" & ageg=="_50")
result_mm_death_agef1<-ttt %>% ##new conditions per year
  group_by(death_year,ageg,sex) %>% 
  summarize(number= n())

ttt<-filter(temp_total, sex=="F" & ageg=="50_59")
result_mm_death_agef2<-ttt %>% ##new conditions per year
  group_by(death_year,ageg,sex) %>% 
  summarize(number= n())

ttt<-filter(temp_total, sex=="F" & ageg=="60_69")
result_mm_death_agef3<-ttt %>% ##new conditions per year
  group_by(death_year,ageg,sex) %>% 
  summarize(number= n())

ttt<-filter(temp_total, sex=="F" & ageg=="70_79")
result_mm_death_agef4<-ttt %>% ##new conditions per year
  group_by(death_year,ageg,sex) %>% 
  summarize(number= n())

ttt<-filter(temp_total, sex=="F" & ageg=="_80")
result_mm_death_agef5<-ttt %>% ##new conditions per year
  group_by(death_year,ageg,sex) %>% 
  summarize(number= n())


##sex=m by age dep


ttt<-filter(temp_total, sex=="M" & ageg=="_50")
result_mm_death_agem1<-ttt %>% ##new conditions per year
  group_by(death_year,ageg,sex) %>% 
  summarize(number= n())

ttt<-filter(temp_total, sex=="M" & ageg=="50_59")
result_mm_death_agem2<-ttt %>% ##new conditions per year
  group_by(death_year,ageg,sex) %>% 
  summarize(number= n())

ttt<-filter(temp_total, sex=="M" & ageg=="60_69")
result_mm_death_agem3<-ttt %>% ##new conditions per year
  group_by(death_year,ageg,sex) %>% 
  summarize(number= n())

ttt<-filter(temp_total, sex=="M" & ageg=="70_79")
result_mm_death_agem4<-ttt %>% ##new conditions per year
  group_by(death_year,ageg,sex) %>% 
  summarize(number= n())

ttt<-filter(temp_total, sex=="M" & ageg=="_80")
result_mm_death_agem5<-ttt %>% ##new conditions per year
  group_by(death_year,ageg,sex) %>% 
  summarize(number= n())

##sex and dep

ttt<-filter(temp_total, sex=="F" & dep_sc=="1")
result_mm_death_depf1<-ttt %>% ##new conditions per year
  group_by(death_year,dep_sc,sex) %>% 
  summarize(number= n())

ttt<-filter(temp_total, sex=="F" & dep_sc=="5")
result_mm_death_depf5<-ttt %>% ##new conditions per year
  group_by(death_year,dep_sc,sex) %>% 
  summarize(number= n())


ttt<-filter(temp_total, sex=="M" & dep_sc=="1")
result_mm_death_depm1<-ttt %>% ##new conditions per year
  group_by(death_year,dep_sc,sex) %>% 
  summarize(number= n())

ttt<-filter(temp_total, sex=="M" & dep_sc=="5")
result_mm_death_depm5<-ttt %>% ##new conditions per year
  group_by(death_year,dep_sc,sex) %>% 
  summarize(number= n())


ttt<-filter(temp_total, ageg=="_50" & dep_sc=="1")
result_mm_death_age1_dep1<-ttt %>% ##new conditions per year
  group_by(death_year,dep_sc,ageg) %>% 
  summarize(number= n())

ttt<-filter(temp_total, ageg=="_50" & dep_sc=="5")
result_mm_death_age1_dep5<-ttt %>% ##new conditions per year
  group_by(death_year,dep_sc,ageg) %>% 
  summarize(number= n())



ttt<-filter(temp_total, ageg=="50_59" & dep_sc=="1")
result_mm_death_age2_dep1<-ttt %>% ##new conditions per year
  group_by(death_year,dep_sc,ageg) %>% 
  summarize(number= n())

ttt<-filter(temp_total, ageg=="50_59" & dep_sc=="5")
result_mm_death_age2_dep5<-ttt %>% ##new conditions per year
  group_by(death_year,dep_sc,ageg) %>% 
  summarize(number= n())


ttt<-filter(temp_total, ageg=="60_69" & dep_sc=="1")
result_mm_death_age3_dep1<-ttt %>% ##new conditions per year
  group_by(death_year,dep_sc,ageg) %>% 
  summarize(number= n())

ttt<-filter(temp_total, ageg=="60_69" & dep_sc=="5")
result_mm_death_age3_dep5<-ttt %>% ##new conditions per year
  group_by(death_year,dep_sc,ageg) %>% 
  summarize(number= n())


ttt<-filter(temp_total, ageg=="70_79" & dep_sc=="1")
result_mm_death_age4_dep1<-ttt %>% ##new conditions per year
  group_by(death_year,dep_sc,ageg) %>% 
  summarize(number= n())

ttt<-filter(temp_total, ageg=="70_79" & dep_sc=="5")
result_mm_death_age4_dep5<-ttt %>% ##new conditions per year
  group_by(death_year,dep_sc,ageg) %>% 
  summarize(number= n())


ttt<-filter(temp_total, ageg=="_80" & dep_sc=="1")
result_mm_death_age5_dep1<-ttt %>% ##new conditions per year
  group_by(death_year,dep_sc,ageg) %>% 
  summarize(number= n())

ttt<-filter(temp_total, ageg=="_80" & dep_sc=="5")
result_mm_death_age5_dep5<-ttt %>% ##new conditions per year
  group_by(death_year,dep_sc,ageg) %>% 
  summarize(number= n())


mm_death_yearly<-cbind.data.frame(result_mm_death_all,result_mm_death_sex_f,result_mm_death_sex_m,result_mm_death_age1,
                                 result_mm_death_age2,result_mm_death_age3,result_mm_death_age4,result_mm_death_age5,
                                 result_mm_death_dep1,result_mm_death_dep5,result_mm_death_agef1,result_mm_death_agef2,
                                 result_mm_death_agef3,result_mm_death_agef4,result_mm_death_agef5,
                                 result_mm_death_depf1,result_mm_death_depf5,result_mm_death_agem1,
                                 result_mm_death_agem2,result_mm_death_agem3,result_mm_death_agem4,result_mm_death_agem5,
                                 result_mm_death_depm1,result_mm_death_depm5,result_mm_death_age1_dep1,result_mm_death_age2_dep1, result_mm_death_age3_dep1,
                                 result_mm_death_age4_dep1, result_mm_death_age2_dep5,
                                 result_mm_death_age3_dep5,   result_mm_death_age4_dep5,
                                 result_mm_death_age5_dep1,result_mm_death_age5_dep5)
mm_death_yearly_age1_dep5<-cbind.data.frame(result_mm_death_age1_dep5)

rm(result_mm_death_all,result_mm_death_sex_f,result_mm_death_sex_m,result_mm_death_age1,
   result_mm_death_age2,result_mm_death_age3,result_mm_death_age4,result_mm_death_age5,
   result_mm_death_dep1,result_mm_death_dep5,result_mm_death_agef1,result_mm_death_agef2,
   result_mm_death_agef3,result_mm_death_agef4,result_mm_death_agef5,
   result_mm_death_depf1,result_mm_death_depf5,result_mm_death_agem1,
   result_mm_death_agem2,result_mm_death_agem3,result_mm_death_agem4,result_mm_death_agem5,
   result_mm_death_depm1,result_mm_death_depm5,result_mm_death_age1_dep1,
   result_mm_death_age1_dep5,result_mm_death_age2_dep1, result_mm_death_age3_dep1,
   result_mm_death_age4_dep1, result_mm_death_age2_dep5,
   result_mm_death_age3_dep5,   result_mm_death_age4_dep5,
   result_mm_death_age5_dep1,result_mm_death_age5_dep5)
fwrite(mm_death_yearly, file="mm_death_yearly_april_2022.csv",sep=",")
fwrite(mm_death_yearly_age1_dep5, file="mm_death_yearly_age1_dep5_april_2022.csv",sep=",")
