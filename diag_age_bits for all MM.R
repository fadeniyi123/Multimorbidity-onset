

result_cond_diag_age_all<-mm_cohort_data2 %>% ##new conditions per year
  group_by(Elixhauser_Index_Phenotypes,) %>% 
  summarize(median_age= median(diag_age3))

ttt<-filter(mm_cohort_data2, sex=="F")
result_cond_diag_age_sex_f<-ttt %>% ##new conditions per year
  group_by(Elixhauser_Index_Phenotypes,sex) %>% 
  summarize(median_age= median(diag_age3))

ttt<-filter(mm_cohort_data2, sex=="M")
result_cond_diag_age_sex_m<-ttt %>% ##new conditions per year
  group_by(Elixhauser_Index_Phenotypes,sex) %>% 
  summarize(median_age= median(diag_age3))

ttt<-filter(mm_cohort_data2, ageg=="_50")
result_cond_diag_age_age1<-ttt %>% ##new conditions per year
  group_by(Elixhauser_Index_Phenotypes,ageg) %>% 
  summarize(median_age= median(diag_age3))

ttt<-filter(mm_cohort_data2, ageg=="50_59")
result_cond_diag_age_age2<-ttt %>% ##new conditions per year
  group_by(Elixhauser_Index_Phenotypes,ageg) %>% 
  summarize(median_age= median(diag_age3))

ttt<-filter(mm_cohort_data2, ageg=="60_69")
result_cond_diag_age_age3<-ttt %>% ##new conditions per year
  group_by(Elixhauser_Index_Phenotypes,ageg) %>% 
  summarize(median_age= median(diag_age3))

ttt<-filter(mm_cohort_data2, ageg=="70_79")
result_cond_diag_age_age4<-ttt %>% ##new conditions per year
  group_by(Elixhauser_Index_Phenotypes,ageg) %>% 
  summarize(median_age= median(diag_age3))

ttt<-filter(mm_cohort_data2, ageg=="_80")
result_cond_diag_age_age5<-ttt %>% ##new conditions per year
  group_by(Elixhauser_Index_Phenotypes,ageg) %>% 
  summarize(median_age= median(diag_age3))

ttt<-filter(mm_cohort_data2, dep_sc=="1")
result_cond_diag_age_dep1<-ttt %>% ##new conditions per year
  group_by(Elixhauser_Index_Phenotypes,dep_sc) %>% 
  summarize(median_age= median(diag_age3))

ttt<-filter(mm_cohort_data2, dep_sc=="5")
result_cond_diag_age_dep5<-ttt %>% ##new conditions per year
  group_by(Elixhauser_Index_Phenotypes,dep_sc) %>% 
  summarize(median_age= median(diag_age3))
#sex and age

ttt<-filter(mm_cohort_data2, sex=="F" & ageg=="_50")
result_cond_diag_age_agef1<-ttt %>% ##new conditions per year
  group_by(Elixhauser_Index_Phenotypes,ageg,sex) %>% 
  summarize(median_age= median(diag_age3))

ttt<-filter(mm_cohort_data2, sex=="F" & ageg=="50_59")
result_cond_diag_age_agef2<-ttt %>% ##new conditions per year
  group_by(Elixhauser_Index_Phenotypes,ageg,sex) %>% 
  summarize(median_age= median(diag_age3))

ttt<-filter(mm_cohort_data2, sex=="F" & ageg=="60_69")
result_cond_diag_age_agef3<-ttt %>% ##new conditions per year
  group_by(Elixhauser_Index_Phenotypes,ageg,sex) %>% 
  summarize(median_age= median(diag_age3))

ttt<-filter(mm_cohort_data2, sex=="F" & ageg=="70_79")
result_cond_diag_age_agef4<-ttt %>% ##new conditions per year
  group_by(Elixhauser_Index_Phenotypes,ageg,sex) %>% 
  summarize(median_age= median(diag_age3))

ttt<-filter(mm_cohort_data2, sex=="F" & ageg=="_80")
result_cond_diag_age_agef5<-ttt %>% ##new conditions per year
  group_by(Elixhauser_Index_Phenotypes,ageg,sex) %>% 
  summarize(median_age= median(diag_age3))


##sex=m by age dep


ttt<-filter(mm_cohort_data2, sex=="M" & ageg=="_50")
result_cond_diag_age_agem1<-ttt %>% ##new conditions per year
  group_by(Elixhauser_Index_Phenotypes,ageg,sex) %>% 
  summarize(median_age= median(diag_age3))

ttt<-filter(mm_cohort_data2, sex=="M" & ageg=="50_59")
result_cond_diag_age_agem2<-ttt %>% ##new conditions per year
  group_by(Elixhauser_Index_Phenotypes,ageg,sex) %>% 
  summarize(median_age= median(diag_age3))

ttt<-filter(mm_cohort_data2, sex=="M" & ageg=="60_69")
result_cond_diag_age_agem3<-ttt %>% ##new conditions per year
  group_by(Elixhauser_Index_Phenotypes,ageg,sex) %>% 
  summarize(median_age= median(diag_age3))

ttt<-filter(mm_cohort_data2, sex=="M" & ageg=="70_79")
result_cond_diag_age_agem4<-ttt %>% ##new conditions per year
  group_by(Elixhauser_Index_Phenotypes,ageg,sex) %>% 
  summarize(median_age= median(diag_age3))

ttt<-filter(mm_cohort_data2, sex=="M" & ageg=="_80")
result_cond_diag_age_agem5<-ttt %>% ##new conditions per year
  group_by(Elixhauser_Index_Phenotypes,ageg,sex) %>% 
  summarize(median_age= median(diag_age3))

##sex deprivation

ttt<-filter(mm_cohort_data2, sex=="F" & dep_sc=="1")
result_cond_diag_age_depf1<-ttt %>% ##new conditions per year
  group_by(Elixhauser_Index_Phenotypes,dep_sc,sex) %>% 
  summarize(median_age= median(diag_age3))

ttt<-filter(mm_cohort_data2, sex=="F" & dep_sc=="5")
result_cond_diag_age_depf5<-ttt %>% ##new conditions per year
  group_by(Elixhauser_Index_Phenotypes,dep_sc,sex) %>% 
  summarize(median_age= median(diag_age3))

ttt<-filter(mm_cohort_data2, sex=="M" & dep_sc=="1")
result_cond_diag_age_depm1<-ttt %>% ##new conditions per year
  group_by(Elixhauser_Index_Phenotypes,dep_sc,sex) %>% 
  summarize(median_age= median(diag_age3))

ttt<-filter(mm_cohort_data2, sex=="M" & dep_sc=="5")
result_cond_diag_age_depm5<-ttt %>% ##new conditions per year
  group_by(Elixhauser_Index_Phenotypes,dep_sc,sex) %>% 
  summarize(median_age= median(diag_age3))

##age by deprivation
ttt<-filter(mm_cohort_data2, ageg=="_50" & dep_sc=="1")
result_cond_diag_age_age1_dep1<-ttt %>% ##new conditions per year
  group_by(Elixhauser_Index_Phenotypes,dep_sc,ageg) %>% 
  summarize(median_age= median(diag_age3))

ttt<-filter(mm_cohort_data2, ageg=="_50" & dep_sc=="5")
result_cond_diag_age_age1_dep5<-ttt %>% ##new conditions per year
  group_by(Elixhauser_Index_Phenotypes,dep_sc,ageg) %>% 
  summarize(median_age= median(diag_age3))



ttt<-filter(mm_cohort2, ageg=="50_59" & dep_sc=="1")
result_cond_diag_age_age2_dep1<-ttt %>% ##new conditions per year
  group_by(Elixhauser_Index_Phenotypes,dep_sc,ageg) %>% 
  summarize(median_age= median(diag_age3))

ttt<-filter(mm_cohort2, ageg=="50_59" & dep_sc=="5")
result_cond_diag_age_age2_dep5<-ttt %>% ##new conditions per year
  group_by(Elixhauser_Index_Phenotypes,dep_sc,ageg) %>% 
  summarize(median_age= median(diag_age3))


ttt<-filter(mm_cohort2, ageg=="60_69" & dep_sc=="1")
result_cond_diag_age_age3_dep1<-ttt %>% ##new conditions per year
  group_by(Elixhauser_Index_Phenotypes,dep_sc,ageg) %>% 
  summarize(median_age= median(diag_age3))

ttt<-filter(mm_cohort2, ageg=="60_69" & dep_sc=="5")
result_cond_diag_age_age3_dep5<-ttt %>% ##new conditions per year
  group_by(Elixhauser_Index_Phenotypes,dep_sc,ageg) %>% 
  summarize(median_age= median(diag_age3))


ttt<-filter(mm_cohort2, ageg=="70_79" & dep_sc=="1")
result_cond_diag_age_age4_dep1<-ttt %>% ##new conditions per year
  group_by(Elixhauser_Index_Phenotypes,dep_sc,ageg) %>% 
  summarize(median_age= median(diag_age3))

ttt<-filter(mm_cohort2, ageg=="70_79" & dep_sc=="5")
result_cond_diag_age_age4_dep5<-ttt %>% ##new conditions per year
  group_by(Elixhauser_Index_Phenotypes,dep_sc,ageg) %>% 
  summarize(median_age= median(diag_age3))


ttt<-filter(mm_cohort_data2, ageg=="_80" & dep_sc=="1")
result_cond_diag_age_age5_dep1<-ttt %>% ##new conditions per year
  group_by(Elixhauser_Index_Phenotypes,dep_sc,ageg) %>% 
  summarize(median_age= median(diag_age3))

ttt<-filter(mm_cohort_data2, ageg=="_80" & dep_sc=="5")
result_cond_diag_age_age5_dep5<-ttt %>% ##new conditions per year
  group_by(Elixhauser_Index_Phenotypes,dep_sc,ageg) %>% 
  summarize(median_age= median(diag_age3))

all_cond_diag_age<-cbind.data.frame(result_cond_diag_age_all,result_cond_diag_age_sex_f,result_cond_diag_age_sex_m,result_cond_diag_age_age1,
                                 result_cond_diag_age_age2,result_cond_diag_age_age3,result_cond_diag_age_age4,
                                 result_cond_diag_age_dep1,result_cond_diag_age_dep5,result_cond_diag_age_agef1,result_cond_diag_age_agef2,
                                 result_cond_diag_age_agef3,result_cond_diag_age_agef4,
                                 result_cond_diag_age_depf1,result_cond_diag_age_depf5,result_cond_diag_age_agem1,
                                 result_cond_diag_age_agem2,result_cond_diag_age_agem3,result_cond_diag_age_agem4,
                                 result_cond_diag_age_depm1,result_cond_diag_age_depm5,result_cond_diag_age_age1_dep5,result_cond_diag_age_age2_dep1,
                                 result_cond_diag_age_age3_dep1, result_cond_diag_age_age4_dep1, result_cond_diag_age_age2_dep5,
                                 result_cond_diag_age_age3_dep5, result_cond_diag_age_age4_dep5,)
all_cond_diag_age_80<-cbind.data.frame(result_cond_diag_age_age5,result_cond_diag_age_agef5,result_cond_diag_age_agem5,
                                       result_cond_diag_age_age1_dep1,result_cond_diag_age_age5_dep1,result_cond_diag_age_age5_dep5)
fwrite(all_cond_diag_age, file="all_cond_diag_age.csv",sep=",")
fwrite(all_cond_diag_age_80, file="all_cond_diag_age_80.csv",sep=",")

rm(result_cond_diag_age_all,result_cond_diag_age_sex_f,result_cond_diag_age_sex_m,result_cond_diag_age_age1,
   result_cond_diag_age_age2,result_cond_diag_age_age3,result_cond_diag_age_age4,
   result_cond_diag_age_dep1,result_cond_diag_age_dep5,result_cond_diag_age_agef1,result_cond_diag_age_agef2,
   result_cond_diag_age_agef3,result_cond_diag_age_agef4,
   result_cond_diag_age_depf1,result_cond_diag_age_depf5,result_cond_diag_age_agem1,
   result_cond_diag_age_agem2,result_cond_diag_age_agem3,result_cond_diag_age_agem4,
   result_cond_diag_age_depm1,result_cond_diag_age_depm5,result_cond_diag_age_age1_dep5,
   result_cond_diag_age_age5,result_cond_diag_age_agef5,result_cond_diag_age_agem5,
   result_cond_diag_age_age1_dep1, result_cond_diag_age_age1_dep5,result_cond_diag_age_age2_dep1,
   result_cond_diag_age_age3_dep1, result_cond_diag_age_age4_dep1, result_cond_diag_age_age2_dep5,
   result_cond_diag_age_age3_dep5, result_cond_diag_age_age4_dep5,result_cond_diag_age_age5_dep1,result_cond_diag_age_age5_dep5)

