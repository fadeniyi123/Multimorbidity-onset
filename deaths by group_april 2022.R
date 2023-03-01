mm_cohort2[,3]<-NULL
mm_cohort2<-left_join(mm_cohort2, demo, by="PROCHI")



##new conditions per year     yearly incidence of mm
result_death_all<-mm_cohort2 %>% ##new conditions per year
  group_by(death_year) %>% 
  summarize(number= n())

ttt<-filter(mm_cohort2, sex=="F")
result_death_sex_f<-ttt %>% ##new conditions per year
  group_by(death_year,sex) %>% 
  summarize(number= n())

ttt<-filter(mm_cohort2, sex=="M")
result_death_sex_m<-ttt %>% ##new conditions per year
  group_by(death_year,sex) %>% 
  summarize(number= n())

ttt<-filter(mm_cohort2, ageg=="_50")
result_death_age1<-ttt %>% ##new conditions per year
  group_by(death_year,ageg) %>% 
  summarize(number= n())

ttt<-filter(mm_cohort2, ageg=="50_59")
result_death_age2<-ttt %>% ##new conditions per year
  group_by(death_year,ageg) %>% 
  summarize(number= n())

ttt<-filter(mm_cohort2, ageg=="60_69")
result_death_age3<-ttt %>% ##new conditions per year
  group_by(death_year,ageg) %>% 
  summarize(number= n())

ttt<-filter(mm_cohort2, ageg=="70_79")
result_death_age4<-ttt %>% ##new conditions per year
  group_by(death_year,ageg) %>% 
  summarize(number= n())

ttt<-filter(mm_cohort2, ageg=="_80")
result_death_age5<-ttt %>% ##new conditions per year
  group_by(death_year,ageg) %>% 
  summarize(number= n())

ttt<-filter(mm_cohort2, dep_sc=="1")
result_death_dep1<-ttt %>% ##new conditions per year
  group_by(death_year,dep_sc) %>% 
  summarize(number= n())

ttt<-filter(mm_cohort2, dep_sc=="5")
result_death_dep5<-ttt %>% ##new conditions per year
  group_by(death_year,dep_sc) %>% 
  summarize(number= n())

###
#sex and age

ttt<-filter(mm_cohort2, sex=="F" & ageg=="_50")
result_death_agef1<-ttt %>% ##new conditions per year
  group_by(death_year,ageg,sex) %>% 
  summarize(number= n())

ttt<-filter(mm_cohort2, sex=="F" & ageg=="50_59")
result_death_agef2<-ttt %>% ##new conditions per year
  group_by(death_year,ageg,sex) %>% 
  summarize(number= n())

ttt<-filter(mm_cohort2, sex=="F" & ageg=="60_69")
result_death_agef3<-ttt %>% ##new conditions per year
  group_by(death_year,ageg,sex) %>% 
  summarize(number= n())

ttt<-filter(mm_cohort2, sex=="F" & ageg=="70_79")
result_death_agef4<-ttt %>% ##new conditions per year
  group_by(death_year,ageg,sex) %>% 
  summarize(number= n())

ttt<-filter(mm_cohort2, sex=="F" & ageg=="_80")
result_death_agef5<-ttt %>% ##new conditions per year
  group_by(death_year,ageg,sex) %>% 
  summarize(number= n())


##sex=m by age dep


ttt<-filter(mm_cohort2, sex=="M" & ageg=="_50")
result_death_agem1<-ttt %>% ##new conditions per year
  group_by(death_year,ageg,sex) %>% 
  summarize(number= n())

ttt<-filter(mm_cohort2, sex=="M" & ageg=="50_59")
result_death_agem2<-ttt %>% ##new conditions per year
  group_by(death_year,ageg,sex) %>% 
  summarize(number= n())

ttt<-filter(mm_cohort2, sex=="M" & ageg=="60_69")
result_death_agem3<-ttt %>% ##new conditions per year
  group_by(death_year,ageg,sex) %>% 
  summarize(number= n())

ttt<-filter(mm_cohort2, sex=="M" & ageg=="70_79")
result_death_agem4<-ttt %>% ##new conditions per year
  group_by(death_year,ageg,sex) %>% 
  summarize(number= n())

ttt<-filter(mm_cohort2, sex=="M" & ageg=="_80")
result_death_agem5<-ttt %>% ##new conditions per year
  group_by(death_year,ageg,sex) %>% 
  summarize(number= n())

##sex and dep

ttt<-filter(mm_cohort2, sex=="F" & dep_sc=="1")
result_death_depf1<-ttt %>% ##new conditions per year
  group_by(death_year,dep_sc,sex) %>% 
  summarize(number= n())

ttt<-filter(mm_cohort2, sex=="F" & dep_sc=="5")
result_death_depf5<-ttt %>% ##new conditions per year
  group_by(death_year,dep_sc,sex) %>% 
  summarize(number= n())


ttt<-filter(mm_cohort2, sex=="M" & dep_sc=="1")
result_death_depm1<-ttt %>% ##new conditions per year
  group_by(death_year,dep_sc,sex) %>% 
  summarize(number= n())

ttt<-filter(mm_cohort2, sex=="M" & dep_sc=="5")
result_death_depm5<-ttt %>% ##new conditions per year
  group_by(death_year,dep_sc,sex) %>% 
  summarize(number= n())


ttt<-filter(mm_cohort2, ageg=="_50" & dep_sc=="1")
result_death_age1_dep1<-ttt %>% ##new conditions per year
  group_by(death_year,dep_sc,ageg) %>% 
  summarize(number= n())

ttt<-filter(mm_cohort2, ageg=="_50" & dep_sc=="5")
result_death_age1_dep5<-ttt %>% ##new conditions per year
  group_by(death_year,dep_sc,ageg) %>% 
  summarize(number= n())

ttt<-filter(mm_cohort2, ageg=="50_59" & dep_sc=="1")
result_death_age2_dep1<-ttt %>% ##new conditions per year
  group_by(death_year,dep_sc,ageg) %>% 
  summarize(number= n())

ttt<-filter(mm_cohort2, ageg=="50_59" & dep_sc=="5")
result_death_age2_dep5<-ttt %>% ##new conditions per year
  group_by(death_year,dep_sc,ageg) %>% 
  summarize(number= n())


ttt<-filter(mm_cohort2, ageg=="60_69" & dep_sc=="1")
result_death_age3_dep1<-ttt %>% ##new conditions per year
  group_by(death_year,dep_sc,ageg) %>% 
  summarize(number= n())

ttt<-filter(mm_cohort2, ageg=="60_69" & dep_sc=="5")
result_death_age3_dep5<-ttt %>% ##new conditions per year
  group_by(death_year,dep_sc,ageg) %>% 
  summarize(number= n())


ttt<-filter(mm_cohort2, ageg=="70_79" & dep_sc=="1")
result_death_age4_dep1<-ttt %>% ##new conditions per year
  group_by(death_year,dep_sc,ageg) %>% 
  summarize(number= n())

ttt<-filter(mm_cohort2, ageg=="70_79" & dep_sc=="5")
result_death_age4_dep5<-ttt %>% ##new conditions per year
  group_by(death_year,dep_sc,ageg) %>% 
  summarize(number= n())


ttt<-filter(mm_cohort2, ageg=="_80" & dep_sc=="1")
result_death_age5_dep1<-ttt %>% ##new conditions per year
  group_by(death_year,dep_sc,ageg) %>% 
  summarize(number= n())

ttt<-filter(mm_cohort2, ageg=="_80" & dep_sc=="5")
result_death_age5_dep5<-ttt %>% ##new conditions per year
  group_by(death_year,dep_sc,ageg) %>% 
  summarize(number= n())


death_yearly<-cbind.data.frame(result_death_all,result_death_sex_f,result_death_sex_m,result_death_age1,
                                result_death_age2,result_death_age3,result_death_age4,result_death_age5,
                                result_death_dep1,result_death_dep5,result_death_agef1,result_death_agef2,
                                result_death_agef3,result_death_agef4,result_death_agef5,
                                result_death_depf1,result_death_depf5,result_death_agem1,
                                result_death_agem2,result_death_agem3,result_death_agem4,result_death_agem5,
                                result_death_depm1,result_death_depm5,result_death_age1_dep1,
                               result_death_age1_dep5,result_death_age2_dep1,result_death_age2_dep5,
                               result_death_age3_dep1,result_death_age3_dep5,
                               result_death_age4_dep1,result_death_age4_dep5,
                               result_death_age5_dep1,result_death_age5_dep5)
death_yearly_age1_dep5<-cbind.data.frame(result_death_age1_dep5)

rm(result_death_all,result_death_sex_f,result_death_sex_m,result_death_age1,
   result_death_age2,result_death_age3,result_death_age4,result_death_age5,
   result_death_dep1,result_death_dep5,result_death_agef1,result_death_agef2,
   result_death_agef3,result_death_agef4,result_death_agef5,
   result_death_depf1,result_death_depf5,result_death_agem1,
   result_death_agem2,result_death_agem3,result_death_agem4,result_death_agem5,
   result_death_depm1,result_death_depm5,result_death_age1_dep1,
   result_death_age1_dep5,result_death_age2_dep1,result_death_age2_dep5,
   result_death_age3_dep1,result_death_age3_dep5,result_death_age4_dep1,
   result_death_age4_dep5,result_death_age5_dep1,result_death_age5_dep5)
fwrite(death_yearly, file="death_yearly_april_2022.csv",sep=",")
fwrite(death_yearly_age1_dep5, file="death_yearly_age1_dep5_april_2022.csv",sep=",")

