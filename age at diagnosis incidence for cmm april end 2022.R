########################################################################################
# DESC:  This code will merge different datasets with ICD10 codes
# Input:
# 
# Outputs:
# 
########################################################################################
#devtools::install_github("mhahsler/arules")
#detach("package:arules", unload = TRUE)
#fwrite(data1, file="data1.csv", sep=",")

# # to install libraries use the following command
# install.packages("data.table",repos=NULL,contriburl="file:V:/R/3.5.1")
# install.packages("dplyr",repos=NULL,contriburl="file:V:/R/3.5.1")
# install.packages("lubridate",repos=NULL,contriburl="file:V:/R/3.5.1")
# install.packages("stringr",repos=NULL,contriburl="file:V:/R/3.5.1")
# install.packages("comorbidity",repos=NULL,contriburl="file:V:/R/3.5.1")
# install.packages("tidygraph",repos=NULL,contriburl="file:V:/R/3.6.2/")
# install.packages("tidyr",repos=NULL,contriburl="file:V:/R/3.6.2/")
# install.packages("tidytext",repos=NULL,contriburl="file:V:/R/3.6.2/")
# install.packages("tidyverse",repos=NULL,contriburl="file:V:/R/3.6.2/")


# to install libraries use the following command
install.packages("data.table",repos=NULL,contriburl="file:V:/R/4.1.2")
install.packages("dplyr",repos=NULL,contriburl="file:V:/R/4.1.2")
install.packages("lubridate",repos=NULL,contriburl="file:V:/R/4.1.2")
install.packages("stringr",repos=NULL,contriburl="file:V:/R/4.1.2")
install.packages("comorbidity",repos=NULL,contriburl="file:V:/R/4.1.2")
install.packages("tidygraph",repos=NULL,contriburl="file:V:/R/4.1.2/")
install.packages("tidyr",repos=NULL,contriburl="file:V:/R/4.1.2/")
install.packages("tidytext",repos=NULL,contriburl="file:V:/R/4.1.2/")
install.packages("tidyverse",repos=NULL,contriburl="file:V:/R/4.1.2/")

# load required libraries
library(data.table)
library(dplyr)
library(lubridate)
library(stringr)
library(comorbidity)
library(tidyr)
library(tidytext)
library(tidyverse)
library("knitr")
library("ggtext")

#install.packages("tidyr",repos=NULL,contriburl="file:V:/R/4.0.4")
#install.packages("tidyverse",repos=NULL,contriburl="file:V:/R/4.0.4")
install.packages("factoextra",repos=NULL,contriburl="file:V:/R/3.5.1")
install.packages("rpart",repos=NULL,contriburl="file:V:/R/3.5.1")
install.packages("caret",repos=NULL,contriburl="file:V:/R/3.5.1")
install.packages("arules",repos=NULL,contriburl="file:V:/R/3.6.2")
install.packages("arulesViz",repos=NULL,contriburl="file:V:/R/3.6.2")
install.packages("rpart.plot",repos=NULL,contriburl="file:V:/R/3.5.1")
install.packages("e1071",repos=NULL,contriburl="file:V:/R/3.5.1")
install.packages("klaR",repos=NULL,contriburl="file:V:/R/3.5.1")
install.packages("plotly",repos=NULL,contriburl="file:V:/R/3.5.1")
install.packages("RColorBrewer",repos=NULL,contriburl="file:V:/R/3.5.1")
install.packages("rpart",repos=NULL,contriburl="file:V:/R/3.5.1")
install.packages("rpart.plot",repos=NULL,contriburl="file:V:/R/3.5.1")
install.packages("arulesSequences",repos=NULL,contriburl="file:V:/R/3.5.1")
install.packages("ggtext",repos=NULL,contriburl="file:V:/R/3.5.1")
install.packages("knitr",repos=NULL,contriburl="file:V:/R/3.5.1")
install.packages("remotes",repos=NULL,contriburl="file:V:/R/3.5.1")
#remotes::install_github("hadley/dplyr")
#install.packages("arules")



library("arulesSequences")
library("rpart.plot")
library("rpart")
library(RColorBrewer)
library(plotly)
library(klaR)
library(rpart.plot)
library(arules)
library("arulesViz")
library(caret)
library(rpart)
library(factoextra)


#start with data cleaning
####this next line has been generated from all tho code in this file.
###so you can read it directly
setwd('P:/Project 3581/codes and data/adeniyi/') 
mm_cohort_data2<- fread("inc_death_diag_age_data.csv", na.strings = c("","NA"))
#mm_cohort_data2$diag_age3<-coalesce(mm_cohort_data2$diag_age2,mm_cohort_data2$diag_age)
# length(which(!is.na(mm_cohort_data2$diag_age)))
# length(which(!is.na(mm_cohort_data2$diag_age2)))
# length(which(!is.na(mm_cohort_data2$diag_age3)))
# tt<-filter(mm_cohort_data2,is.na(diag_age2))

#####else continue here

#start with data cleaning that was processed in stata
####this next line has been generated from all the code in this file.
###so you can read it directly
library(haven)
incidence_death_diag_data <- read_dta("codes/incidence death diag data.dta")

incidence_death_diag_data$sex<-haven::as_factor(incidence_death_diag_data$sex)
incidence_death_diag_data$ageg<-haven::as_factor(incidence_death_diag_data$ageg)
incidence_death_diag_data$dep_sc<-haven::as_factor(incidence_death_diag_data$dep_sc)
mean(incidence_death_diag_data$diag_age)
mean(incidence_death_diag_data$diag_age2)## reformat to integer
mean(incidence_death_diag_data$diag_age2,na.rm = TRUE)
mean(incidence_death_diag_data$diag_age2)
##mm_cohort_data2<-incidence_death_diag_data

#####else start afresh and continue from here
setwd('P:/Project 3581/Demography/') 
demography_data <- fread("Demography_Current.csv", na.strings = c("","NA"))

setwd('P:/Project 3581/codes and data/adeniyi/data created/') 
all_elix_pheno <- fread("all_elixhauser_phenotype.csv", na.strings = c("","NA"))
elix_data_10 <- fread("Elixhauser_data_10.csv", na.strings = c("","NA"))
mm_cohort2 <- fread("MM_cohort_u2.csv", na.strings = c("","NA"))

# setwd('P:/Project 3581/codes and data/utkarsh/data created/')
# mm_cohort_ua <- fread("MM_cohort_u2.csv", na.strings = c("","NA"))

setwd('P:/Project 3581/codes and data/adeniyi/') 
#data1 <- fread("data1.csv", na.strings = c("","NA"))

unique(all_elix_pheno$Elixhauser_Index_Phenotypes)
##clean up the phenotypes
tt<-filter(all_elix_pheno,Elixhauser_Index_Phenotypes=="Pulmonary Circulation Disorders + Chronic Pulmonary Disorders") 
length(unique(tt$MAIN_CONDITION))
unique(tt$MAIN_CONDITION)
tt<-filter(all_elix_pheno,Elixhauser_Index_Phenotypes=="Liver Disease + Alcohol Abuse")
unique(tt$MAIN_CONDITION)
all_elix_pheno$Elixhauser_Index_Phenotypes[all_elix_pheno$Elixhauser_Index_Phenotypes=="Liver Disease + Alcohol Abuse"]<-"Alcohol Abuse"
all_elix_pheno$Elixhauser_Index_Phenotypes[all_elix_pheno$Elixhauser_Index_Phenotypes=="Psychoses + Depression"]<-"Psychoses"
all_elix_pheno$Elixhauser_Index_Phenotypes[all_elix_pheno$Elixhauser_Index_Phenotypes=="c(\"\"Psychoses\"\", \"\"Depression\"\")"]<-"Psychoses"
all_elix_pheno$Elixhauser_Index_Phenotypes[all_elix_pheno$Elixhauser_Index_Phenotypes=="c(\"\"Congestive Heart Failure\"\", \"\"Alcohol Abuse\"\")"]<-"Congestive Heart Failure"
unique(all_elix_pheno$Elixhauser_Index_Phenotypes)
all_elix_pheno$Elixhauser_Index_Phenotypes[all_elix_pheno$Elixhauser_Index_Phenotypes=="Pulmonary Circulation Disorders + Chronic Pulmonary Disorders"]<-"Chronic Pulmonary Disorders"
tt<-filter(all_elix_pheno,Elixhauser_Index_Phenotypes=="Other Neurological Disorders + Paralysis")
unique(tt$MAIN_CONDITION)
all_elix_pheno$Elixhauser_Index_Phenotypes[all_elix_pheno$Elixhauser_Index_Phenotypes=="Other Neurological Disorders + Paralysis"]<-"Paralysis"
tt<-filter(all_elix_pheno,Elixhauser_Index_Phenotypes=="Hypertension, complicated + Congestive Heart Failure")
unique(tt$MAIN_CONDITION)
tt<-filter(all_elix_pheno,Elixhauser_Index_Phenotypes=="Hypertension, complicated + Renal Failure")
unique(tt$MAIN_CONDITION)
all_elix_pheno$Elixhauser_Index_Phenotypes[all_elix_pheno$Elixhauser_Index_Phenotypes=="Hypertension, complicated + Renal Failure"]<-"Renal Failure"
all_elix_pheno$Elixhauser_Index_Phenotypes[all_elix_pheno$Elixhauser_Index_Phenotypes=="Hypertension, complicated + Congestive Heart Failure"]<-"Congestive Heart Failure"
unique(all_elix_pheno$Elixhauser_Index_Phenotypes)

####then remove thosewith no conditions
all_elix_pheno_b<-na.omit(all_elix_pheno)
length(unique(all_elix_pheno_b$PROCHI))## all with at least a condition 223662  
table(all_elix_pheno_b$Elixhauser_Index_Phenotypes, useNA = "ifany")
#all_elix_pheno_c<-all_elix_pheno_b %>% group_by(PROCHI,Elixhauser_Index_Phenotypes)
#head(all_elix_pheno_c)

#prepare to select distinct individuals with distinct conditions ordered by date of diagnoses
all_elix_pheno_c<-all_elix_pheno_b 
#head(all_elix_pheno_c)
all_elix_pheno_c<-arrange(all_elix_pheno_c, PROCHI,ADMISSION_DATE,  .by_group = TRUE)
all_elix_pheno_c<-distinct(all_elix_pheno_c, PROCHI,Elixhauser_Index_Phenotypes, .keep_all=TRUE)
length(unique(all_elix_pheno_c$PROCHI))## all with at least a condition 223662 

##calculate age at end of study

mm_cohort2$age2019a<-as.integer(round((as.difftime(as.Date("2019-01-01")-as.Date(mm_cohort2$anon_date_of_birth) ,unit="year")/365),0))
mm_cohort2$age2019<-mm_cohort2$age_on_1stjan2000 + 19
mm_cohort2$anon_date_of_birth<-as.Date(mm_cohort2$anon_date_of_birth)
mm_cohort2$dob_year<-format(mm_cohort2$anon_date_of_birth,format="%Y")

#mm_cohort2$ageg[mm_cohort2$age2019a<45]=0
#mm_cohort2$age2019a[mm_cohort2$age2019a<50]=1
mm_cohort2$ageg[mm_cohort2$age2019a>=44&mm_cohort2$age2019a<=49]=1
mm_cohort2$ageg[mm_cohort2$age2019a>=50&mm_cohort2$age2019a<=59]=2
mm_cohort2$ageg[mm_cohort2$age2019a>=60&mm_cohort2$age2019a<=69]=3
mm_cohort2$ageg[mm_cohort2$age2019a>=70&mm_cohort2$age2019a<=79]=4
mm_cohort2$ageg[mm_cohort2$age2019a>=80]=5
mm_cohort2$ageg2<-mm_cohort2$ageg
mm_cohort2$ageg<-factor(mm_cohort2$ageg,levels=c(0,1,2,3,4,5),label=c("_45","45_49","50_59","60_69","70_79","80+"))
addmargins(table(mm_cohort2$ageg))

# #add the demographic data from the MM cohort mm_cohort2
# demo<-select(demography_data, PROCHI,hb_extract, sex,Calculated_Age, SCSIMD5)
# colnames(demo)<-c("PROCHI","hb", "sex","age","dep_sc")
# demo$ageg[demo$age<45]=0
# #demo$ageg[demo$age<50]=1
# demo$ageg[demo$age>=45&demo$age<=49]=1
# demo$ageg[demo$age>=50&demo$age<=59]=2
# demo$ageg[demo$age>=60&demo$age<=69]=3
# demo$ageg[demo$age>=70&demo$age<=79]=4
# demo$ageg[demo$age>=80]=5
# demo$ageg<-factor(demo$ageg,levels=c(0,1,2,3,4,5),label=c("_45","45_49","50_59","60_69","70_79","_80"))
# #demo<-as_tibble(demo) %>% add_column(age_10=demo$age-10)
# table(demo$ageg)
# 
mm_cohort2$dep_sc<- mm_cohort2$HBSIMD5
mm_cohort2$dep_sc[is.na(mm_cohort2$dep_sc)]=6

mm_cohort2$dead[!is.na(mm_cohort2$date_of_death)]=1
mm_cohort2$dead[is.na(mm_cohort2$date_of_death)]=0

fwrite(mm_cohort2, file = "mm_cohort2.csv", sep=",") 

demo<-select(mm_cohort2, PROCHI,hb_extract, sex,age2019a,ageg,dep_sc,anon_date_of_birth,cohort_member,dead)
all_elix_pheno_d<-left_join(all_elix_pheno_c,demo, by="PROCHI")

#create data capture date and obtain age at diagnosis
all_elix_pheno_d$capture_date<-"2019-01-01"
all_elix_pheno_d$capture_date<-ymd(all_elix_pheno_d$capture_date)
all_elix_pheno_d$ADMISSION_DATE<-ymd(all_elix_pheno_d$ADMISSION_DATE)

#all_elix_pheno_d$diag_age<-as.difftime(all_elix_pheno_d$capture_date-all_elix_pheno_d$ADMISSION_DATE,unit="years")
#all_elix_pheno_d$diag_age<-all_elix_pheno_d$age-round((as.difftime(all_elix_pheno_d$capture_date-all_elix_pheno_d$ADMISSION_DATE,unit="year")/366),0)
all_elix_pheno_d$diag_age<-round((as.difftime(all_elix_pheno_d$ADMISSION_DATE - all_elix_pheno_d$anon_date_of_birth ,unit="year")/365),0)
all_elix_pheno_d$diag_age<-as.integer(all_elix_pheno_d$diag_age)

all_elix_pheno_d$adm_year<-as.Date(all_elix_pheno_d$ADMISSION_DATE)
all_elix_pheno_d$adm_year<-format(all_elix_pheno_d$adm_year, "%Y")


####drop off the non multi morbids 
all_elix_pheno_e<-all_elix_pheno_d
## if you want only the multimorbid patients
#all_elix_pheno_e<-filter(all_elix_pheno_d,Elixhauser_Score>1)

##change condition names to smaller names
all_elix_pheno_e$cond[all_elix_pheno_e$Elixhauser_Index_Phenotypes=="Solid Tumour without Metastasis"]<-"solidtum"
all_elix_pheno_e$cond[all_elix_pheno_e$Elixhauser_Index_Phenotypes=="Metastatic Cancer"]<-"metacanc"                              
all_elix_pheno_e$cond[all_elix_pheno_e$Elixhauser_Index_Phenotypes=="Hypertension, uncomplicated" ]<-"hypunc"                    
all_elix_pheno_e$cond[all_elix_pheno_e$Elixhauser_Index_Phenotypes=="Congestive Heart Failure"]<-"chf"                        
all_elix_pheno_e$cond[all_elix_pheno_e$Elixhauser_Index_Phenotypes=="Diabetes, uncomplicated"]<-"diabunc"                         
all_elix_pheno_e$cond[all_elix_pheno_e$Elixhauser_Index_Phenotypes=="Renal Failure"]<-"rf"                                   
all_elix_pheno_e$cond[all_elix_pheno_e$Elixhauser_Index_Phenotypes=="Diabetes, complicated"]<-"diabc"                           
all_elix_pheno_e$cond[all_elix_pheno_e$Elixhauser_Index_Phenotypes=="Peripheral Vascular Disease"]<-"pvd"                     
all_elix_pheno_e$cond[all_elix_pheno_e$Elixhauser_Index_Phenotypes=="Other Neurological Disorders"]<-"ond"                    
all_elix_pheno_e$cond[all_elix_pheno_e$Elixhauser_Index_Phenotypes=="Weight Loss"]<-"wloss"                                     
all_elix_pheno_e$cond[all_elix_pheno_e$Elixhauser_Index_Phenotypes=="Rheumatoid Arthritis/ Collagen Vascular Disease"]<-"rheumd" 
all_elix_pheno_e$cond[all_elix_pheno_e$Elixhauser_Index_Phenotypes=="Peptic Ulcer Disease, exluding bleeding"]<-"pud"         
all_elix_pheno_e$cond[all_elix_pheno_e$Elixhauser_Index_Phenotypes=="Deficiency Anaemia"]<-"dane"                              
all_elix_pheno_e$cond[all_elix_pheno_e$Elixhauser_Index_Phenotypes=="Alcohol Abuse"]<-"alcohol"                                   
all_elix_pheno_e$cond[all_elix_pheno_e$Elixhauser_Index_Phenotypes=="Depression"]<-"depre"                                      
all_elix_pheno_e$cond[all_elix_pheno_e$Elixhauser_Index_Phenotypes=="Fluid and Electrolyte Disorders"]<-"fed" 
all_elix_pheno_e$cond[all_elix_pheno_e$Elixhauser_Index_Phenotypes=="Chronic Pulmonary Disorders"]<-"cpd"                     
all_elix_pheno_e$cond[all_elix_pheno_e$Elixhauser_Index_Phenotypes=="Paralysis"]<-"para"                                       
all_elix_pheno_e$cond[all_elix_pheno_e$Elixhauser_Index_Phenotypes=="Cardic Arrhythmias"]<-"carit"                              
all_elix_pheno_e$cond[all_elix_pheno_e$Elixhauser_Index_Phenotypes=="Valvular Disease"]<-"valv"                                
all_elix_pheno_e$cond[all_elix_pheno_e$Elixhauser_Index_Phenotypes=="Hypothyroidism"]<-"hypothy"                                  
all_elix_pheno_e$cond[all_elix_pheno_e$Elixhauser_Index_Phenotypes=="Lymphoma"]<-"lymph"                                        
all_elix_pheno_e$cond[all_elix_pheno_e$Elixhauser_Index_Phenotypes=="Pulmonary Circulation Disorders"]<-"pcd"                 
all_elix_pheno_e$cond[all_elix_pheno_e$Elixhauser_Index_Phenotypes=="Blood Loss Anaemia"]<-"blane"                              
all_elix_pheno_e$cond[all_elix_pheno_e$Elixhauser_Index_Phenotypes=="Coagulopathy"]<-"coag"                                    
all_elix_pheno_e$cond[all_elix_pheno_e$Elixhauser_Index_Phenotypes=="Hypertension, complicated"]<-"hypc"                       
all_elix_pheno_e$cond[all_elix_pheno_e$Elixhauser_Index_Phenotypes=="Drug Abuse"]<-"drug"                                      
all_elix_pheno_e$cond[all_elix_pheno_e$Elixhauser_Index_Phenotypes=="Liver Disease"]<-"ld"                                   
all_elix_pheno_e$cond[all_elix_pheno_e$Elixhauser_Index_Phenotypes=="Obesity"]<-"obese"                                         
all_elix_pheno_e$cond[all_elix_pheno_e$Elixhauser_Index_Phenotypes=="Psychoses"]<-"psycho"                                       
all_elix_pheno_e$cond[all_elix_pheno_e$Elixhauser_Index_Phenotypes=="AIDS/HIV"]<-"AIDS/HIV"

##save this and change the name if necessary
fwrite(all_elix_pheno_e, file="all incidence death diag phenotype.csv", sep=",")


##generate number of conditions
all_elix_pheno_e<-all_elix_pheno_e    %>%
  group_by(PROCHI) %>%
  mutate(num_of_cond = 1:n()) %>% ##numbering of conditions
  ungroup()

##GENERATING A NEATER DATABASE having singlr phenotype and full cohort
mm_merged<-filter(all_elix_pheno_e,num_of_cond==1)
mm_merged<-select(mm_merged, -hb_extract, -sex,-age2019a,-ageg,-dep_sc,-anon_date_of_birth,-cohort_member,-dead)
mm_merged<-mutate(mm_merged,cond_mm1=1)

mm_merged<-full_join(mm_cohort2,mm_merged, by="PROCHI")
addmargins(table(mm_merged$cohort_member,mm_merged$cond_mm1 ,useNA = "always"))

cond_mm2<-filter(all_elix_pheno_e,num_of_cond==2)
cond_mm2<-mutate(cond_mm2,cond_mm2=2)
cond_mm2<-select(cond_mm2,PROCHI,cond_mm2)
cond_mm4<-filter(all_elix_pheno_e,num_of_cond==4)
cond_mm4<-mutate(cond_mm4,cond_mm4=4)
cond_mm4<-select(cond_mm4,PROCHI,cond_mm4)
mm_merged<-left_join(mm_merged,cond_mm2, by="PROCHI")
mm_merged<-left_join(mm_merged,cond_mm4, by="PROCHI")
mm_merged$num_of_cond[is.na(mm_merged$num_of_cond)]=0
mm_merged$cohort_member[is.na(mm_merged$cohort_member)]=0
mm_merged$cond_mm1[is.na(mm_merged$cond_mm1)]=0
mm_merged$cond_mm2[is.na(mm_merged$cond_mm2)]=0
mm_merged$cond_mm4[is.na(mm_merged$cond_mm4)]=0

fwrite(mm_merged, file = "mm_merged.csv", sep=",") 


##continue  
##note the correct age is diag_age


# all_elix_pheno_c<-all_elix_pheno_c %>% 
#      group_by(PROCHI,Elixhauser_Index_Phenotypes) %>% 
#      summarise(n())
##selct those with valid diag_age
all_elix_pheno_e<-filter(all_elix_pheno_e, !is.na(diag_age))
##select only cohort members
all_elix_pheno_e<-filter(all_elix_pheno_e, cohort_member==1)

#select only the alive
all_elix_pheno_k<-filter(all_elix_pheno_e, dead==0)

result_all<-all_elix_pheno_e %>% 
  group_by(Elixhauser_Index_Phenotypes) %>% 
  summarize(median_age= median(diag_age),mean_age= round(mean(diag_age),1),
            min_age= min(diag_age), max_age= max(diag_age))

result_sex<-all_elix_pheno_e %>% 
  group_by(Elixhauser_Index_Phenotypes,sex) %>% 
  summarize(median_age= median(diag_age),mean_age= round(mean(diag_age),1),
            min_age= min(diag_age), max_age= max(diag_age))

result_agegroups<-all_elix_pheno_e %>% 
  group_by(Elixhauser_Index_Phenotypes,ageg) %>% 
  summarize(median_age= median(diag_age),mean_age= round(mean(diag_age),1),
            min_age= min(diag_age), max_age= max(diag_age))

result_sex_age<-all_elix_pheno_e %>% 
  group_by(Elixhauser_Index_Phenotypes,sex,ageg) %>% 
  summarize(median_age= median(diag_age),mean_age= round(mean(diag_age),1),
            min_age= min(diag_age), max_age= max(diag_age))

result_depriv<-all_elix_pheno_e %>% 
  group_by(Elixhauser_Index_Phenotypes, dep_sc) %>% 
  summarize(median_age= median(diag_age),mean_age= round(mean(diag_age),1),
            min_age= min(diag_age), max_age= max(diag_age))

result_sex_depriv<-all_elix_pheno_e %>% 
  group_by(Elixhauser_Index_Phenotypes,sex,dep_sc) %>% 
  summarize(median_age= median(diag_age),mean_age= round(mean(diag_age),1),
            min_age= min(diag_age), max_age= max(diag_age))
##median age at diagnosis
# result_median<-all_elix_pheno_d %>%
#   group_by(PROCHI,Elixhauser_Index_Phenotypes) %>%
#   summarize(median_age= median(diag_age))
# ##mean age at diagnosis
# result_mean<-all_elix_pheno_d %>% 
#   group_by(Elixhauser_Index_Phenotypes) %>% 
#   summarize(mean_age= round(mean(diag_age),1))
# #summarize(mean_age= mean(diag_age))
# ##minimum age at diagnosis
# result_min<-all_elix_pheno_d %>% 
#   group_by(Elixhauser_Index_Phenotypes) %>% 
#   summarize(min_age= min(diag_age))
# ##maximum age at diagnosis
# result_max<-all_elix_pheno_d %>% 
#   group_by(Elixhauser_Index_Phenotypes) %>% 
#   summarize(max_age= max(diag_age))

#age_at_diagnosis=as.data.frame(cbind(result_mean, result_median[,2], result_max[,2],result_min[,2]))
fwrite(result_all, file = "age_at_diagnosis_all.csv", sep=",")
fwrite(result_sex, file = "age_at_diagnosis_sex.csv", sep=",")
fwrite(result_agegroups, file = "age_at_diagnosis_age.csv", sep=",")
fwrite(result_sex_age, file = "age_at_diagnosis_sex_age.csv", sep=",")
fwrite(result_depriv, file = "age_at_diagnosis_depriv.csv", sep=",")
fwrite(result_sex_depriv, file = "age_at_diagnosis_sex_depriv.csv", sep=",")
#rm(result_mean, result_median, result_max,result_min)
rm(result_agegroups,result_all, result_depriv, result_sex, result_sex_age,result_sex_depriv)


#this section is no longer necessary as we have already obtained thecorrect age at diagnosis from mm_cohort2
# ###here we have used diag_age3
# ###alternatively use the mmcohort data
# mm_cohort_data<-select(mm_cohort2,PROCHI,anon_date_of_birth,date_of_death)
# mm_cohort_data$dob<-as.Date(mm_cohort_data$anon_date_of_birth)
# mm_cohort_data<-left_join(all_elix_pheno_e,mm_cohort_data,by="PROCHI")
# mm_cohort_data$diag_age2<-as.integer(round((as.difftime(mm_cohort_data$ADMISSION_DATE-mm_cohort_data$dob,unit="year")/366),0))
# mm_cohort_data$diag_age3<-coalesce(mm_cohort_data$diag_age2,mm_cohort_data$diag_age)##replace mising values in diag_age2 with diag_age
# #mm_cohort_data2<-select(mm_cohort_data,PROCHI,dob,cond,ADMISSION_DATE, date_of_death,Elixhauser_Score,Elixhauser_Index_Phenotypes, diag_age, diag_age2,diag_age3, sex, ageg, dep_sc)
# mm_cohort_data2<-mm_cohort_data
# #mm_cohort_data2<-na.omit(mm_cohort_data2)
# 
# mm_cohort_data2$dob<-as.Date(mm_cohort_data2$dob)
# mm_cohort_data2$dob_year<-format(mm_cohort_data2$dob,format="%Y")
# 
# mm_cohort_data2$ADMISSION_DATE<-as.Date(mm_cohort_data2$ADMISSION_DATE)
# mm_cohort_data2$adm_year<-format(mm_cohort_data2$ADMISSION_DATE,format="%Y")
# 
# 
# ###add year of death
# mm_cohort_data2$death_date<-as.Date(mm_cohort_data2$date_of_death)
# mm_cohort_data2$death_year<-format(mm_cohort_data2$death_date, "%Y")
# #mm_cohort_data2<-distinct(mm_cohort_data2, PROCHI, .keep_all=TRUE)
# 
# mm_cohort_data2 <- mm_cohort_data2 %>%
#   group_by(PROCHI) %>%
#   mutate(num_of_cond = 1:n()) %>% ##numbering of conditions
#   ungroup()
# 
# fwrite(mm_cohort_data2, file = "inc_death_diag_age_data_april_25_2022.csv", sep=",")     


##GENERATING A NEATER DATABASE
# mm_merged<-filter(mm_cohort_data2,num_of_cond==1)
# mm_merged<-select(mm_merged, -anon_date_of_birth,-date_of_death,-sex,-dob)
# mm_merged<-mutate(mm_merged,cond_mm1=1)
# mm_cohort2<-mutate(mm_cohort2,cohort_member=1)
# mm_merged<-full_join(mm_merged,mm_cohort2, by="PROCHI")
# addmargins(table(mm_merged$cohort_member,mm_merged$cond_mm1 ,useNA = "always"))
# 
# cond_mm2<-filter(mm_cohort_data2,num_of_cond==2)
# cond_mm2<-mutate(cond_mm2,cond_mm2=2)
# cond_mm2<-select(cond_mm2,PROCHI,cond_mm2)
# cond_mm4<-filter(mm_cohort_data2,num_of_cond==4)
# cond_mm4<-mutate(cond_mm4,cond_mm4=4)
# cond_mm4<-select(cond_mm4,PROCHI,cond_mm4)
# mm_merged<-left_join(mm_merged,cond_mm2, by="PROCHI")
# mm_merged<-left_join(mm_merged,cond_mm4, by="PROCHI")
# 
# mm_merged$cohort_member[is.na(mm_merged$cohort_member)]=0
# mm_merged$cond_mm1[is.na(mm_merged$cond_mm1)]=0
# mm_merged$cond_mm2[is.na(mm_merged$cond_mm2)]=0
# mm_merged$cond_mm4[is.na(mm_merged$cond_mm4)]=0
# 
# fwrite(mm_merged, file = "mm_merged.csv", sep=",")   

    
##yearly deaths

per_year_death<-mm_cohort_data2 %>% 
  group_by(death_year,) %>% 
  summarise(n())

result_mm_dob<-mm_cohort_data2 %>% 
  group_by(dob_year) %>% 
  summarize(count=n())
fwrite(result_mm_dob, file = "age_at_birth_mm.csv", sep=",")            
##yearly count by phenos and years
result_mm_adm_year<-mm_cohort_data2 %>% 
  group_by(adm_year, Elixhauser_Index_Phenotypes) %>% 
  summarize(count=n())
#fwrite(result_mm_dob, file = "age_at_birth_mm.csv", sep=",")            

##yearly count by phenos and years

result_mm_adm_year<-all_elix_pheno_e %>% 
  group_by(sex,adm_year, Elixhauser_Index_Phenotypes) %>% 
  summarize(count=n())
fwrite(result_mm_adm_year, file = "first_pheno_yearly new sex.csv", sep=",") 


result_all_mm_count<-mm_cohort_data %>% 
  group_by(Elixhauser_Index_Phenotypes) %>% 
  summarise(count=n())
#yyy=cbind(result_all,result_all_mm[,c(2:5)])



##incidence
##report only years rather than full date
all_elix_pheno_b<-na.omit(all_elix_pheno)
#table(all_elix_pheno_b$Elixhauser_Index_Phenotypes, useNA = "ifany")
#all_elix_pheno_c<-all_elix_pheno_b %>% group_by(PROCHI,Elixhauser_Index_Phenotypes)
#head(all_elix_pheno_c)
all_elix_pheno_c<-arrange(all_elix_pheno_c, PROCHI,ADMISSION_DATE,  .by_group = TRUE)

demo<-select(demography_data, PROCHI,hb_extract, sex,Calculated_Age, hb_extract,SCSIMD5)
colnames(demo)<-c("PROCHI","hb", "sex","age","dep_sc")
#demo$ageg[demo$age<45]=0
demo$ageg[demo$age<50]=1
#demo$ageg[demo$age>=45&demo$age<=49]=1
demo$ageg[demo$age>=50&demo$age<=59]=2
demo$ageg[demo$age>=60&demo$age<=69]=3
demo$ageg[demo$age>=70&demo$age<=79]=4
demo$ageg[demo$age>=80]=5
demo$ageg<-factor(demo$ageg,levels=c(0,1,2,3,4,5),label=c("_45","_50","50_59","60_69","70_79","_80"))
#demo<-as_tibble(demo) %>% add_column(age_10=demo$age-10)

#demo$dep_hb[is.na(demo$dep_hb)]=6
demo$dep_sc[is.na(demo$dep_sc)]=6
all_elix_pheno_d<-left_join(all_elix_pheno_c,demo, by="PROCHI")
all_elix_pheno_d$year<-as.Date(all_elix_pheno_d$ADMISSION_DATE)
all_elix_pheno_d$year<-format(all_elix_pheno_d$adm_year, "%Y")
all_elix_pheno_d<-distinct(all_elix_pheno_d, PROCHI,Elixhauser_Index_Phenotypes, .keep_all=TRUE)

##change years below to adm_year
per_year_inc_all<-all_elix_pheno_d %>% 
  group_by(adm_year, Elixhauser_Index_Phenotypes) %>% 
  summarise(n())

per_year_inc_sex<-all_elix_pheno_d %>% 
  group_by(year, Elixhauser_Index_Phenotypes,sex) %>% 
  summarise(n())

per_year_inc_agegroups<-all_elix_pheno_d %>% 
  group_by(year, Elixhauser_Index_Phenotypes,ageg) %>% 
  summarise(n())

per_year_inc_sex_age<-all_elix_pheno_d %>% 
  group_by(year, Elixhauser_Index_Phenotypes,sex,ageg) %>% 
  summarise(n())

per_year_inc_depriv<-all_elix_pheno_d %>% 
  group_by(year, Elixhauser_Index_Phenotypes, dep_sc) %>% 
  summarise(n())

per_year_inc_sex_depriv<-all_elix_pheno_d %>% 
  group_by(year, Elixhauser_Index_Phenotypes,sex,dep_sc) %>% 
  summarise(n())


fwrite(per_year_inc_all, file = "per_year_inc_all_mm.csv", sep=",")
fwrite(per_year_inc_sex, file = "per_year_inc_sex_mm.csv", sep=",")
fwrite(per_year_inc_agegroups, file = "per_year_inc_age_mm.csv", sep=",")
fwrite(per_year_inc_sex_age, file = "per_year_inc_sex_age_mm.csv", sep=",")
fwrite(per_year_inc_depriv, file = "per_year_inc_depriv_mm.csv", sep=",")
fwrite(per_year_inc_sex_depriv, file = "per_year_inc_sex_depriv_mm.csv", sep=",")
#rm(per_year_inc_mean, per_year_inc_median, per_year_inc_max,per_year_inc_min)
rm(per_year_inc_agegroups,per_year_inc_all, per_year_inc_depriv, per_year_inc_sex, per_year_inc_sex_age,per_year_inc_sex_depriv)



##################################WITH UTKARSH
##for cmm analysis
##this was executed earlier into       mm_cohort_data2....check
mm_cohort_data2 <- arrange(mm_cohort_data2, PROCHI, ADMISSION_DATE)
temp <- mm_cohort_data2 %>%
  group_by(PROCHI) %>%
  mutate(num_of_cond = 1:n()) %>% ##numbering of conditions
  ungroup()

temp4 <-filter(temp, num_of_cond==4)  ## select only the 4th conditions of people with CMM and use their age at diag to determine age at onset of cmm
length(unique(temp4$PROCHI)) ## unique individuals with cMM  57024

temp_total <- temp4 %>%
  group_by(adm_year) %>%
  mutate(cond_total_year = n()) %>% ## to determine yearly totals for the incidence of CMM
  ungroup()

temp_total <-select(temp_total,PROCHI,cond_total_year)
temp_joined<-left_join(temp,temp_total)


##alternatively
temp <- mm_cohort_data2 %>%
  group_by(PROCHI) %>%
  mutate(num_of_cond = 1:n()) %>% ##numbering of conditions
  ungroup()
temp4 <-filter(temp, num_of_cond==4)## select only the 4th conditions of people with CMM and use their age at diag to determine age at onset of cmm
temp_total <- temp4 %>%
  group_by(adm_year) %>%
  mutate(cond_total_year = n()) %>%
  ungroup()
temp_total_b<-select(temp_total,PROCHI,num_of_cond,cond_total_year)
temp_joined<-left_join(temp,temp_total_b)

temp_cmm_contributors <-filter(temp_joined, num_of_cond<=4)# filter out those that contributed to cmm for clustering
temp_cmm_contributors <- filter(temp_cmm_contributors, cmm=="2")

table(temp_joined$diag_age3,temp_joined$adm_year)##year by age at onset of MM
table(temp4$diag_age3,temp4$adm_year)##year by age at onset of cMM
table(temp_joined$diag_age3,temp_joined$adm_year)##year by number of occurrence
table(temp_joined$adm_year,temp_joined$cond_total_year)##yearly incidence

##############################
#####count conditions and age at onset of cMM

##interquartimes quantiles
#  summarize(median_age= median(diag_age),q1_age=quantile(diag_age, probs=c(0.25)),
#q2_age=quantile(diag_age, probs=c(0.5)),q3_age=quantile(diag_age, probs=c(0.75)))
###diag_age is more correct than diag_age3
ttt<-filter(temp_total)
result_cmm_onset_all<-ttt %>% ##count conditions and age at onset of cMM
  group_by() %>% 
  summarize(number= n(),median_age3= median(diag_age3),
             min_age= min(diag_age3), max_age= max(diag_age3))

result_cmm_onset_sex<-ttt %>% ##count conditions and age at onset of cMM
  group_by(sex) %>% 
  summarize(number= n(),median_age3= median(diag_age3),
             min_age= min(diag_age3), max_age= max(diag_age3))

result_cmm_onset_age<-ttt %>% ##count conditions and age at onset of cMM
  group_by(ageg) %>% 
  summarize(number= n(),median_age3= median(diag_age3),
             min_age= min(diag_age3), max_age= max(diag_age3))

result_cmm_onset_age_sex<-ttt %>% ##count conditions and age at onset of cMM
  group_by(ageg,sex) %>% 
  summarize(number= n(),median_age3= median(diag_age3),
             min_age= min(diag_age3), max_age= max(diag_age3))


result_cmm_onset_dep<-ttt %>% ##count conditions and age at onset of cMM
  group_by(dep_sc) %>% 
  summarize(number= n(),median_age3= median(diag_age3),
             min_age= min(diag_age3), max_age= max(diag_age3))

result_cmm_onset_dep_sex<-ttt %>% ##count conditions and age at onset of cMM
  group_by(dep_sc,sex) %>% 
  summarize(number= n(),median_age3= median(diag_age3),
             min_age= min(diag_age3), max_age= max(diag_age3))

########################################################################################
##new conditions per year     yearly incidence of cMM
result_cmm_inc_all<-temp_total %>% ##new conditions per year
  group_by(adm_year) %>% 
  summarize(number= n())

ttt<-filter(temp_total, sex=="F")
result_cmm_inc_sex_f<-ttt %>% ##new conditions per year
  group_by(adm_year,sex) %>% 
  summarize(number= n())

ttt<-filter(temp_total, sex=="M")
result_cmm_inc_sex_m<-ttt %>% ##new conditions per year
  group_by(adm_year,sex) %>% 
  summarize(number= n())

ttt<-filter(temp_total, ageg=="_50")
result_cmm_inc_age1<-ttt %>% ##new conditions per year
  group_by(adm_year,ageg) %>% 
  summarize(number= n())

ttt<-filter(temp_total, ageg=="50_59")
result_cmm_inc_age2<-ttt %>% ##new conditions per year
  group_by(adm_year,ageg) %>% 
  summarize(number= n())

ttt<-filter(temp_total, ageg=="60_69")
result_cmm_inc_age3<-ttt %>% ##new conditions per year
  group_by(adm_year,ageg) %>% 
  summarize(number= n())

ttt<-filter(temp_total, ageg=="70_79")
result_cmm_inc_age4<-ttt %>% ##new conditions per year
  group_by(adm_year,ageg) %>% 
  summarize(number= n())

ttt<-filter(temp_total, ageg=="_80")
result_cmm_inc_age5<-ttt %>% ##new conditions per year
  group_by(adm_year,ageg) %>% 
  summarize(number= n())

ttt<-filter(temp_total, dep_sc=="1")
result_cmm_inc_dep1<-ttt %>% ##new conditions per year
  group_by(adm_year,dep_sc) %>% 
  summarize(number= n())

ttt<-filter(temp_total, dep_sc=="5")
result_cmm_inc_dep5<-ttt %>% ##new conditions per year
  group_by(adm_year,dep_sc) %>% 
  summarize(number= n())

###
#sex and age

ttt<-filter(temp_total, sex=="F" & ageg=="_50")
result_cmm_inc_agef1<-ttt %>% ##new conditions per year
  group_by(adm_year,ageg,sex) %>% 
  summarize(number= n())

ttt<-filter(temp_total, sex=="F" & ageg=="50_59")
result_cmm_inc_agef2<-ttt %>% ##new conditions per year
  group_by(adm_year,ageg,sex) %>% 
  summarize(number= n())

ttt<-filter(temp_total, sex=="F" & ageg=="60_69")
result_cmm_inc_agef3<-ttt %>% ##new conditions per year
  group_by(adm_year,ageg,sex) %>% 
  summarize(number= n())

ttt<-filter(temp_total, sex=="F" & ageg=="70_79")
result_cmm_inc_agef4<-ttt %>% ##new conditions per year
  group_by(adm_year,ageg,sex) %>% 
  summarize(number= n())

ttt<-filter(temp_total, sex=="F" & ageg=="_80")
result_cmm_inc_agef5<-ttt %>% ##new conditions per year
  group_by(adm_year,ageg,sex) %>% 
  summarize(number= n())


##sex=m by age dep


ttt<-filter(temp_total, sex=="M" & ageg=="_50")
result_cmm_inc_agem1<-ttt %>% ##new conditions per year
  group_by(adm_year,ageg,sex) %>% 
  summarize(number= n())

ttt<-filter(temp_total, sex=="M" & ageg=="50_59")
result_cmm_inc_agem2<-ttt %>% ##new conditions per year
  group_by(adm_year,ageg,sex) %>% 
  summarize(number= n())

ttt<-filter(temp_total, sex=="M" & ageg=="60_69")
result_cmm_inc_agem3<-ttt %>% ##new conditions per year
  group_by(adm_year,ageg,sex) %>% 
  summarize(number= n())

ttt<-filter(temp_total, sex=="M" & ageg=="70_79")
result_cmm_inc_agem4<-ttt %>% ##new conditions per year
  group_by(adm_year,ageg,sex) %>% 
  summarize(number= n())

ttt<-filter(temp_total, sex=="M" & ageg=="_80")
result_cmm_inc_agem5<-ttt %>% ##new conditions per year
  group_by(adm_year,ageg,sex) %>% 
  summarize(number= n())

##sex and dep

ttt<-filter(temp_total, sex=="F" & dep_sc=="1")
result_cmm_inc_depf1<-ttt %>% ##new conditions per year
  group_by(adm_year,dep_sc,sex) %>% 
  summarize(number= n())

ttt<-filter(temp_total, sex=="F" & dep_sc=="5")
result_cmm_inc_depf5<-ttt %>% ##new conditions per year
  group_by(adm_year,dep_sc,sex) %>% 
  summarize(number= n())


ttt<-filter(temp_total, sex=="M" & dep_sc=="1")
result_cmm_inc_depm1<-ttt %>% ##new conditions per year
  group_by(adm_year,dep_sc,sex) %>% 
  summarize(number= n())

ttt<-filter(temp_total, sex=="M" & dep_sc=="5")
result_cmm_inc_depm5<-ttt %>% ##new conditions per year
  group_by(adm_year,dep_sc,sex) %>% 
  summarize(number= n())


ttt<-filter(temp_total, ageg=="_50" & dep_sc=="1")
result_cmm_inc_age1_dep1<-ttt %>% ##new conditions per year
  group_by(adm_year,dep_sc,ageg) %>% 
  summarize(number= n())

ttt<-filter(temp_total, ageg=="_50" & dep_sc=="5")
result_cmm_inc_age1_dep5<-ttt %>% ##new conditions per year
  group_by(adm_year,dep_sc,ageg) %>% 
  summarize(number= n())



ttt<-filter(temp_total, ageg=="50_59" & dep_sc=="1")
result_cmm_inc_age2_dep1<-ttt %>% ##new conditions per year
  group_by(adm_year,dep_sc,ageg) %>% 
  summarize(number= n())

ttt<-filter(temp_total, ageg=="50_59" & dep_sc=="5")
result_cmm_inc_age2_dep5<-ttt %>% ##new conditions per year
  group_by(adm_year,dep_sc,ageg) %>% 
  summarize(number= n())


ttt<-filter(temp_total, ageg=="60_69" & dep_sc=="1")
result_cmm_inc_age3_dep1<-ttt %>% ##new conditions per year
  group_by(adm_year,dep_sc,ageg) %>% 
  summarize(number= n())

ttt<-filter(temp_total, ageg=="60_69" & dep_sc=="5")
result_cmm_inc_age3_dep5<-ttt %>% ##new conditions per year
  group_by(adm_year,dep_sc,ageg) %>% 
  summarize(number= n())


ttt<-filter(temp_total, ageg=="70_79" & dep_sc=="1")
result_cmm_inc_age4_dep1<-ttt %>% ##new conditions per year
  group_by(adm_year,dep_sc,ageg) %>% 
  summarize(number= n())

ttt<-filter(temp_total, ageg=="70_79" & dep_sc=="5")
result_cmm_inc_age4_dep5<-ttt %>% ##new conditions per year
  group_by(adm_year,dep_sc,ageg) %>% 
  summarize(number= n())


ttt<-filter(temp_total, ageg=="_80" & dep_sc=="1")
result_cmm_inc_age5_dep1<-ttt %>% ##new conditions per year
  group_by(adm_year,dep_sc,ageg) %>% 
  summarize(number= n())

ttt<-filter(temp_total, ageg=="_80" & dep_sc=="5")
result_cmm_inc_age5_dep5<-ttt %>% ##new conditions per year
  group_by(adm_year,dep_sc,ageg) %>% 
  summarize(number= n())


cmm_inc_yearly<-cbind.data.frame(result_cmm_inc_all,result_cmm_inc_sex_f,result_cmm_inc_sex_m,result_cmm_inc_age1,
                 result_cmm_inc_age2,result_cmm_inc_age3,result_cmm_inc_age4,result_cmm_inc_age5,
                 result_cmm_inc_dep1,result_cmm_inc_dep5,result_cmm_inc_agef1,result_cmm_inc_agef2,
                 result_cmm_inc_agef3,result_cmm_inc_agef4,result_cmm_inc_agef5,
                 result_cmm_inc_depf1,result_cmm_inc_depf5,result_cmm_inc_agem1,
                 result_cmm_inc_agem2,result_cmm_inc_agem3,result_cmm_inc_agem4,result_cmm_inc_agem5,
                 result_cmm_inc_depm1,result_cmm_inc_depm5,result_cmm_inc_age1_dep1,result_cmm_inc_age2_dep1, result_cmm_inc_age3_dep1,
                 result_cmm_inc_age4_dep1, result_cmm_inc_age2_dep5,
                 result_cmm_inc_age3_dep5,   result_cmm_inc_age4_dep5,
                 result_cmm_inc_age5_dep1,result_cmm_inc_age5_dep5)
cmm_inc_yearly_age1_dep5<-cbind.data.frame(result_cmm_inc_age1_dep5)

rm(result_cmm_inc_all,result_cmm_inc_sex_f,result_cmm_inc_sex_m,result_cmm_inc_age1,
   result_cmm_inc_age2,result_cmm_inc_age3,result_cmm_inc_age4,result_cmm_inc_age5,
   result_cmm_inc_dep1,result_cmm_inc_dep5,result_cmm_inc_agef1,result_cmm_inc_agef2,
   result_cmm_inc_agef3,result_cmm_inc_agef4,result_cmm_inc_agef5,
   result_cmm_inc_depf1,result_cmm_inc_depf5,result_cmm_inc_agem1,
   result_cmm_inc_agem2,result_cmm_inc_agem3,result_cmm_inc_agem4,result_cmm_inc_agem5,
   result_cmm_inc_depm1,result_cmm_inc_depm5,result_cmm_inc_age1_dep1,
   result_cmm_inc_age1_dep5,result_cmm_inc_age2_dep1, result_cmm_inc_age3_dep1,
   result_cmm_inc_age4_dep1, result_cmm_inc_age2_dep5,
   result_cmm_inc_age3_dep5,   result_cmm_inc_age4_dep5,
   result_cmm_inc_age5_dep1,result_cmm_inc_age5_dep5)
fwrite(cmm_inc_yearly, file="cmm_inc_yearly_april_2022.csv",sep=",")
fwrite(cmm_inc_yearly_age1_dep5, file="cmm_inc_yearly_age1_dep5_april_2022.csv",sep=",")
