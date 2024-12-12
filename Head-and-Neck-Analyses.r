##################################################################################################################################################################################
#################################################################################Head & Neck use case#############################################################################
##################################################################################################################################################################################
library(haven)
library(skimr)
library(dplyr)
library(ggplot2)
library(scales)  
library(tidyverse)
library(lubridate)
library(survival)
library(survminer)
library(dplyr)
library(gtsummary)
library(gt)
library(webshot2) #Package to save tables in pdf
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)

#DB is the database including all patients (cases and controls)

#Calculation of age at diagnosis
DB$Nb_years_btw_recr_diag <- time_length(difftime(as.Date(DB$D_Dgrare), as.Date(DB$D_Recrui)), "years")
DB$Age_diag <- DB$Age_Recr + DB$Nb_years_btw_recr_diag

#Formatting of database
DB_OK <- DB %>% mutate(Sexname = as_factor(Sex), Sitename = as_factor(Siterare), Morpname = as_factor(Morprare))
DB_summary <- DB_OK %>% mutate(Country = as_factor(Country),Center=as_factor(Center), Vit_Stat=as_factor(Vit_Stat), L_School=as_factor(L_School), Smoke_Stat=as_factor(Smoke_Stat), Sex=as_factor(Sex),Mar_Stat=as_factor(Mar_Stat),Hypert=as_factor(Hypert),Diabet=as_factor(Diabet),Dur_Smok_C=as_factor(Dur_Smok_C), Smoke_Intensity=as_factor(Smoke_Intensity), Alc_Drinker=as_factor(Alc_Drinker), Alc_Re_C=as_factor(Alc_Re_C), Alc_Lifetime_C=as_factor(Alc_Lifetime_C), Alc_Pattern=as_factor(Alc_Pattern), Pa_Total=as_factor(Pa_Total))
DB_summary <- DB_summary[,c("Sex","Age_Recr","Age_diag","Agexit","Nb_years_btw_recr_diag","D_Recrui","D_Check","D_Csr_Cncr","D_Dthlst","D_Endfup","D_Dgrare","Country","Center","Vit_Stat","Typ_Tumo_Family","Typ_Tumo_Tier1","Typ_Tumo_Tier2","Siterare","Sitename","Morprare","Morpname","Mar_Stat","L_School","Hypert","Diabet","Bmi_C","Smoke_Stat","Smoke_Intensity","Pack_Years","Alc_Drinker","Alc_Pattern","Alc_Drinktime","Alc_Lifetime_C","Asbestos_Cum","Pa_Total","QE_ENERGY")]

#Set new columns for case_hn and subtypes (1=H&N rare cancers, 0=controls=all others)
DB_summary <- DB_summary %>%
  mutate(case_rare=
           case_when(
             Typ_Tumo_Family == "" ~ 0,
             TRUE ~ 1))
DB_summary <- DB_summary %>%
  mutate(case_hn=
           case_when(
             Typ_Tumo_Family == "Head and neck" ~ 1,
             TRUE ~ 0))

DB_summary <- DB_summary %>%
  mutate(case_hn_larynx=
           case_when(
             Typ_Tumo_Tier2 == "Squamous cell carcinoma with variants of larynx" ~ 1,
             TRUE ~ 0))

DB_summary <- DB_summary %>%
  mutate(case_hn_oral_cavity=
           case_when(
             Typ_Tumo_Tier2 == "Squamous cell carcinoma with variants of oral cavity" ~ 1,
             TRUE ~ 0))

DB_summary <- DB_summary %>%
  mutate(case_hn_oropharynx=
           case_when(
             Typ_Tumo_Tier2 == "Squamous cell carcinoma with variants of oropharynx" ~ 1,
             TRUE ~ 0))

DB_summary <- DB_summary %>%
  mutate(case_hn_hypopharynx=
           case_when(
             Typ_Tumo_Tier2 == "Squamous cell carcinoma with variants of hypopharynx" ~ 1,
             TRUE ~ 0))

DB_summary <- DB_summary %>%
  mutate(case_hn_lip=
           case_when(
             Typ_Tumo_Tier2 == "Squamous cell carcinoma with variants of lip" ~ 1,
             TRUE ~ 0))

table(DB_summary$case_rare) #8851
table(DB_summary$case_hn) #881
table(DB_summary$case_hn_larynx) #268
table(DB_summary$case_hn_oral_cavity) #189
table(DB_summary$case_hn_oropharynx) #177
table(DB_summary$case_hn_hypopharynx) #53
table(DB_summary$case_hn_lip) #54

#Calculate follow-up =fup (in months or years)
##case=1: recr to diag
##case = 0 and death=1: recr to date of death
##case = 0 and death=0 : recr to date of end of fup (D_Csr_Cncr)

DB_summary <- DB_summary %>%
  mutate(time_followup =
           case_when(
             case_hn == 1 ~ time_length(interval(D_Recrui,D_Dgrare),"years"),
             case_hn == "0" & Vit_Stat =="Dead" ~ time_length(interval(D_Recrui,D_Dthlst),"years"),
             case_hn == "0" & Vit_Stat =="Alive" ~ time_length(interval(D_Recrui, D_Endfup),"years"),
             T~NA_real_))

DB_summary <- DB_summary %>%
  mutate(time_followup_larynx =
           case_when(
             case_hn_larynx == 1 ~ time_length(interval(D_Recrui,D_Dgrare),"years"),
             case_hn_larynx == "0" & Vit_Stat =="Dead" ~ time_length(interval(D_Recrui,D_Dthlst),"years"),
             case_hn_larynx == "0" & Vit_Stat =="Alive" ~ time_length(interval(D_Recrui, D_Endfup),"years"),
             T~NA_real_))


DB_summary <- DB_summary %>%
  mutate(time_followup_oral_cavity =
           case_when(
             case_hn_oral_cavity == 1 ~ time_length(interval(D_Recrui,D_Dgrare),"years"),
             case_hn_oral_cavity == "0" & Vit_Stat =="Dead" ~ time_length(interval(D_Recrui,D_Dthlst),"years"),
             case_hn_oral_cavity == "0" & Vit_Stat =="Alive" ~ time_length(interval(D_Recrui, D_Endfup),"years"),
             T~NA_real_))

DB_summary <- DB_summary %>%
  mutate(time_followup_oropharynx =
           case_when(
             case_hn_oropharynx == 1 ~ time_length(interval(D_Recrui,D_Dgrare),"years"),
             case_hn_oropharynx == "0" & Vit_Stat =="Dead" ~ time_length(interval(D_Recrui,D_Dthlst),"years"),
             case_hn_oropharynx == "0" & Vit_Stat =="Alive" ~ time_length(interval(D_Recrui, D_Endfup),"years"),
             T~NA_real_))

DB_summary <- DB_summary %>%
  mutate(time_followup_hypopharynx =
           case_when(
             case_hn_hypopharynx == 1 ~ time_length(interval(D_Recrui,D_Dgrare),"years"),
             case_hn_hypopharynx == "0" & Vit_Stat =="Dead" ~ time_length(interval(D_Recrui,D_Dthlst),"years"),
             case_hn_hypopharynx == "0" & Vit_Stat =="Alive" ~ time_length(interval(D_Recrui, D_Endfup),"years"),
             T~NA_real_))

DB_summary <- DB_summary %>%
  mutate(time_followup_lip =
           case_when(
             case_hn_lip == 1 ~ time_length(interval(D_Recrui,D_Dgrare),"years"),
             case_hn_lip == "0" & Vit_Stat =="Dead" ~ time_length(interval(D_Recrui,D_Dthlst),"years"),
             case_hn_lip == "0" & Vit_Stat =="Alive" ~ time_length(interval(D_Recrui, D_Endfup),"years"),
             T~NA_real_))

#Remove factor with no data (country=Greece, Sweden and Norway + center )
DB_summary$Country = droplevels(DB_summary$Country)
DB_summary$Center = droplevels(DB_summary$Center)
DB_summary$Hypert = droplevels(DB_summary$Hypert)
DB_summary$Diabet = droplevels(DB_summary$Diabet)

###################################gtsummary package to generate summary table

#1. Small table
	#Whole cohort vs rare cancers
DB_summary_tbl1 <- DB_summary %>% select(all_of(Sex,Age_Recr,Vit_Stat,Bmi_C, Smoke_Stat,Alc_Pattern,case_rare))
DB_summary_tbl1 <- DB_summary_tbl1 %>% mutate(case_rare=recode(case_rare,`0` = "Controls", `1` = "Cases"))
gt_table1 <- DB_summary_tbl1 %>% tbl_summary(by=case_rare) %>% add_overall() %>% modify_caption("Table1. Baseline characteristics of rare cancer cases vs controls") %>% bold_labels() %>% as_gt()
gtsave(gt_table1,"table1.docx")

	#Whole cohort vs rare H&N
DB_summary_tbl2 <- DB_summary %>% select(all_of(Sex,Age_Recr,Vit_Stat,Bmi_C,Smoke_Stat,Alc_Pattern,case_hn))
DB_summary_tbl2 <- DB_summary_tbl2 %>% mutate(case_hn=recode(case_hn,`0` = "Controls", `1` = "Cases"))
gt_table2 <- DB_summary_tbl2 %>% tbl_summary(by=case_hn) %>% add_overall() %>% modify_caption("Table2. Baseline characteristics of head and neck rare cancer cases vs controls") %>% bold_labels() %>% as_gt()
gtsave(gt_table2,"table2.docx")

#2. Extended tables with additional variables
	#Whole cohort vs rare cancers
DB_summary_tbl3 <- DB_summary %>% select(all_of(Sex,Age_Recr,Vit_Stat,Country,L_School,Bmi_C,Smoke_Intensity,Alc_Drinker,Pa_Total,QE_ENERGY,case_rare))
DB_summary_tbl3 <- DB_summary_tbl3 %>% mutate(case_rare=recode(case_rare,`0` = "Controls", `1` = "Cases"))
gt_table3 <- DB_summary_tbl3 %>% tbl_summary(by=case_rare) %>% add_overall() %>% modify_caption("Table3. Baseline characteristics of rare cancer cases vs controls") %>% bold_labels() %>% as_gt()
gtsave(gt_table3,"table3.docx")

	#Whole cohort vs rare H&N
DB_summary_tbl4 <- DB_summary %>% select(all_of(Sex,Age_Recr,Vit_Stat,Country,L_School,Bmi_C,Smoke_Intensity,Alc_Drinker,Pa_Total,QE_ENERGY,case_hn))
DB_summary_tbl4 <- DB_summary_tbl4 %>% mutate(case_hn=recode(case_hn,`0` = "Controls", `1` = "Cases"))
gt_table4 <- DB_summary_tbl4 %>% tbl_summary(by=case_hn) %>% add_overall() %>% modify_caption("Table4. Baseline characteristics of head and neck rare cancer cases vs controls") %>% bold_labels() %>% as_gt()
gtsave(gt_table4,"table4.docx")

########################################################################################
################# ANALYSES #############################################################

################### Cox model - adjusted model which include standard risk factors

#Reorder factors to have reference with enough cases
DB_summary$Country <- factor(DB_summary$Country, levels=c("Denmark","Spain","United Kingdom","Germany","Italy","The Netherlands","France"))
levels(DB_summary$L_School)[levels(DB_summary$L_School)=='Longer education (incl. University deg.)'] <- 'Longer education' #Shortened the longer education level to better display in plots
DB_summary$L_School <- factor(DB_summary$L_School, levels=c("Primary school completed","Technical/professional school","Longer education","Secondary school","None","Not specified"))
DB_summary$Alc_Pattern <- factor(DB_summary$Alc_Pattern, levels=c("Never drinkers","Former light drinkers","Light drinkers","Never heavy drinkers","Periodically heavy drinkers","Always heavy drinkers","Former heavy drinkers","Unknown"))

#Rename variables
DB_summary_OK <- DB_summary %>% 
  rename("School_level" = "L_School",
         "Age_at_recruitment" = "Age_Recr",
         "Age_at_diagnosis" = "Age_diag",
         "Hypertension" = "Hypert",
         "Diabetes" = "Diabet",
         "BMI"= "Bmi_C",
         "Smoking_status"="Smoke_Stat",
         "Smoking_status_and_intensity"="Smoke_Intensity",
         "Alcohol_pattern"="Alc_Pattern",
         "Asbestos"="Asbestos_Cum",
         "Physical_activity"="Pa_Total",
         "Vital_status"="Vit_Stat",
         "Energy_intake"="QE_ENERGY")

################### ALL HN
#remove France (not enough cases)
DB_summary_HN_OK <- DB_summary_OK %>% filter(Country != "France")
DB_summary_HN_OK$Country = droplevels(DB_summary_HN_OK$Country)
levels(DB_summary_HN_OK$Country)

###############################################################################################################
#All effective factors using Smoking intensity

HN_Cox_adj_all_v2 <-coxph(Surv(time_followup, case_hn)~Smoking_status_and_intensity+Alcohol_pattern+Hypertension+Diabetes+Sex+Age_at_recruitment+Country+School_level,data=DB_summary_HN_OK)
summary(HN_Cox_adj_all_v2)
ggforest(HN_Cox_adj_all_v2, data=as.data.frame(DB_summary_HN_OK)) #Supp-Fig2

#Test of cox model - Proportional hazard assumption -> Schoenfeld residuals
test_v2.ph <- cox.zph(HN_Cox_adj_all_v2)
ggcoxzph(test_v2.ph) #Supp-Fig1

#Other plot for v2 => Figure 3 of manuscript
HN_datarecord_adj_all_v2<-as.data.frame(summary(HN_Cox_adj_all_v2)$conf.int[,c(1,3,4)]) #just need the HR, lowerCI and UpperCI
HN_datarecord_adj_all_v2$Variables<-rownames(HN_datarecord_adj_all_v2)
HN_datarecord_adj_all_v2<-HN_datarecord_adj_all_v2 %>% rename(HR=`exp(coef)`, LowerCI=`lower .95`,UpperCI=`upper .95`)
HN_datarecord_adj_all_v2$Model<-"Adjusted for sex, age, country, smoking, alcohol, hypertension, diabetes and highest school level"

HN_datarecord_adj_all_v2 <- mutate(HN_datarecord_adj_all_v2, Variables = case_when( 
  Variables == "Smoking_status_and_intensityCurrent, 1-15 cig/day" ~ "Current smoker, 1-15 cig/day",
  Variables == "Smoking_status_and_intensityCurrent, 16-25 cig/day" ~ "Current smoker, 16-25 cig/day",
  Variables == "Smoking_status_and_intensityCurrent, 26+ cig/day" ~ "Current smoker, 26+ cig/day",
  Variables == "Smoking_status_and_intensityFormer, quit <= 10 years" ~ "Former smoker, quit <= 10 years",
  Variables == "Smoking_status_and_intensityFormer, quit 11-20 years" ~ "Former smoker, quit 11-20 years",
  Variables == "Smoking_status_and_intensityFormer, quit 20+ years" ~ "Former smoker, quit 20+ years",
  Variables == "Smoking_status_and_intensityCurrent, pipe/cigar/occas" ~ "Current smoker, pipe/cigar/occas",
  Variables == "Alcohol_patternFormer light drinkers" ~ "Former light drinkers",
  Variables == "Alcohol_patternLight drinkers" ~ "Light drinkers",
  Variables == "Alcohol_patternNever heavy drinkers" ~ "Never heavy drinkers",
  Variables == "Alcohol_patternPeriodically heavy drinkers" ~ "Periodically heavy drinkers",
  Variables == "Alcohol_patternAlways heavy drinkers" ~ "Always heavy drinkers",
  Variables == "Alcohol_patternFormer heavy drinkers" ~ "Former heavy drinkers",
  Variables == "HypertensionYes" ~ "Hypertension: Yes",
  Variables == "DiabetesYes" ~ "Diabetes: Yes",
  Variables == "SexFemale" ~ "Female",
  Variables == "Age_at_recruitment" ~ "Age at recruitment",
  TRUE ~ Variables))

HN_datarecord_adj_all_v2$Variables <- factor (HN_datarecord_adj_all_v2$Variables, levels=c("Age at recruitment","Female","Diabetes: Yes","Hypertension: Yes","Former heavy drinkers","Always heavy drinkers","Periodically heavy drinkers","Former light drinkers","Light drinkers","Never heavy drinkers","Former smoker, quit 20+ years","Former smoker, quit 11-20 years","Former smoker, quit <= 10 years","Current smoker, pipe/cigar/occas","Current smoker, 26+ cig/day","Current smoker, 16-25 cig/day","Current smoker, 1-15 cig/day"))
HN_datarecord_adj_all_v2$Group <- "NA"
  
  HN_datarecord_adj_all_v2 <- mutate(HN_datarecord_adj_all_v2, Group = case_when( 
    Variables == "Current smoker, 1-15 cig/day" ~ "Smoking status & intensity",
    Variables == "Current smoker, 16-25 cig/day" ~ "Smoking status & intensity",
    Variables == "Current smoker, 26+ cig/day" ~ "Smoking status & intensity",
    Variables == "Former smoker, quit <= 10 years" ~ "Smoking status & intensity",
    Variables == "Former smoker, quit 11-20 years" ~ "Smoking status & intensity",
    Variables == "Former smoker, quit 20+ years" ~ "Smoking status & intensity",
    Variables == "Current smoker, pipe/cigar/occas" ~ "Smoking status & intensity",
    Variables == "Former light drinkers" ~ "Alcohol pattern",
    Variables == "Light drinkers" ~ "Alcohol pattern",
    Variables == "Never heavy drinkers" ~ "Alcohol pattern",
    Variables == "Periodically heavy drinkers" ~ "Alcohol pattern",
    Variables == "Always heavy drinkers" ~ "Alcohol pattern",
    Variables == "Former heavy drinkers" ~ "Alcohol pattern",
    Variables == "Hypertension: Yes" ~ "H",
    Variables == "Diabetes: Yes" ~ "D",
    Variables == "Female" ~ "Sex",
    Variables == "Age at recruitment" ~ "Age",
    TRUE ~ Group))  
  
  HN_datarecord_adj_all_v2_filtered <- HN_datarecord_adj_all_v2 %>% filter(Variables %in% c("Former smoker, quit 11-20 years","Former smoker, quit <= 10 years","Current smoker, pipe/cigar/occas","Current smoker, 26+ cig/day","Current smoker, 16-25 cig/day","Current smoker, 1-15 cig/day","Former smoker, quit 20+ years","Former light drinkers","Light drinkers","Never heavy drinkers","Periodically heavy drinkers","Always heavy drinkers","Former heavy drinkers","Hypertension: Yes","Diabetes: Yes","Female","Age at recruitment"))
  HN_datarecord_adj_all_v2_filtered$Group_ok = factor(HN_datarecord_adj_all_v2_filtered$Group, levels=c("Smoking status & intensity","Alcohol pattern","H", "D", "Sex", "Age"))
  
  plot_All_adj_v2 <-ggplot(HN_datarecord_adj_all_v2_filtered,aes(x=HR,y=Variables))+
  geom_point(position = position_dodge(width=1), size=4)+ #to avoid overlapping + 
  geom_text(aes(HR, label=paste(format(round(HR,digits=2),nsmall=2)," (",format(round(LowerCI,digits=2),nsmall=2),"-",format(round(UpperCI,digits=2),nsmall=2),")", sep="")), size=4, position = position_dodge(width=1), hjust=0.08, vjust=-1.2) +
  geom_errorbarh(aes(xmin=LowerCI,xmax=UpperCI),position = position_dodge(width=1), height=0.35, size=1)+
  geom_vline(xintercept = 1,lty=2, color="purple", size=1)+ #add HR=1 as significance threshold
  theme_bw(base_size=18) +
  scale_y_discrete(breaks=c("Current smoker, 1-15 cig/day", "Current smoker, 16-25 cig/day","Current smoker, 26+ cig/day","Former smoker, quit <= 10 years","Former smoker, quit 11-20 years","Former smoker, quit 20+ years",
                            "Current smoker, pipe/cigar/occas","Former light drinkers","Light drinkers","Never heavy drinkers","Periodically heavy drinkers","Always heavy drinkers","Former heavy drinkers","Hypertension: Yes","Diabetes: Yes","Female","Age at recruitment"),
                   labels=c("Current smoker, 1-15 cig/day", "Current smoker, 16-25 cig/day","Current smoker, 26+ cig/day","Former smoker, quit <= 10 years","Former smoker, quit 11-20 years","Former smoker, quit 20+ years",
                            "Current smoker, pipe/cigar/occas","Former light drinkers","Light drinkers","Never heavy drinkers","Periodically heavy drinkers","Always heavy drinkers","Former heavy drinkers","Hypertension: Yes","Diabetes: Yes","Female","Age at recruitment")) +
  theme(axis.text=element_text(size=14), axis.title.y=element_blank(), axis.title.x=element_text(size=20,face="bold"),plot.title = element_text(size = 20, face = "bold")) + xlab ("HR (95%CI)")

p.grid <- plot_All_adj_v2 + facet_grid(Group_ok ~ ., scales = "free_y", space = "free_y", switch="y")
p.grid #=> Figure 3

#Generate additional table with number per category

event_table_for_figure3_part1 <- as.data.frame(t(table(DB_summary_HN_OK$case_hn,DB_summary_HN_OK$Smoking_status_and_intensity)))
colnames(event_table_for_figure3_part1) <- c("Variable", "Case","Count")
event_table_for_figure3_part1 <- reshape(event_table_for_figure3_part1, direction='wide', idvar=c("Variable"), timevar="Case")
event_table_for_figure3_part1 <- event_table_for_figure3_part1 %>% filter (!Variable %in% c("Never","Current/Former, missing","Unknown"))
event_table_for_figure3_part1_OK <- event_table_for_figure3_part1[c(1:3,7,4:6), ]

event_table_for_figure3_part2 <- as.data.frame(t(table(DB_summary_HN_OK$case_hn,DB_summary_HN_OK$Alcohol_pattern)))
colnames(event_table_for_figure3_part2) <- c("Variable", "Case","Count")
event_table_for_figure3_part2 <- reshape(event_table_for_figure3_part2, direction='wide', idvar=c("Variable"), timevar="Case")
event_table_for_figure3_part2 <- event_table_for_figure3_part2 %>% filter (!Variable %in% c("Never drinkers","Unknown"))
event_table_for_figure3_part2_OK <- event_table_for_figure3_part2[c(3,2,1,4:6), ]

event_table_for_figure3_part3 <- as.data.frame(t(table(DB_summary_HN_OK$case_hn,DB_summary_HN_OK$Hypertension)))
colnames(event_table_for_figure3_part3) <- c("Variable", "Case","Count")
event_table_for_figure3_part3 <- reshape(event_table_for_figure3_part3, direction='wide', idvar=c("Variable"), timevar="Case")
event_table_for_figure3_part3_OK <- event_table_for_figure3_part3 %>% filter (!Variable %in% c("No","Do not know"))
levels(event_table_for_figure3_part3_OK$Variable)[match("Yes",levels(event_table_for_figure3_part3_OK$Variable))] <- "Hypertension: Yes"


event_table_for_figure3_part4 <- as.data.frame(t(table(DB_summary_HN_OK$case_hn,DB_summary_HN_OK$Diabetes)))
colnames(event_table_for_figure3_part4) <- c("Variable", "Case","Count")
event_table_for_figure3_part4 <- reshape(event_table_for_figure3_part4, direction='wide', idvar=c("Variable"), timevar="Case")
event_table_for_figure3_part4_OK <- event_table_for_figure3_part4 %>% filter (!Variable %in% c("No","Do not know"))
levels(event_table_for_figure3_part4_OK$Variable)[match("Yes",levels(event_table_for_figure3_part4_OK$Variable))] <- "Diabet: Yes"

event_table_for_figure3_part5 <- as.data.frame(t(table(DB_summary_HN_OK$case_hn,DB_summary_HN_OK$Sex)))
colnames(event_table_for_figure3_part5) <- c("Variable", "Case","Count")
event_table_for_figure3_part5 <- reshape(event_table_for_figure3_part5, direction='wide', idvar=c("Variable"), timevar="Case")
event_table_for_figure3_part5_OK <- event_table_for_figure3_part5 %>% filter (!Variable %in% c("Male"))

table(DB_summary_HN_OK$case_hn) #=> cases=879 + non-cases=310189 - Variable, Count.0, Count.1
event_table_for_figure3_part6 <- c("Age at recruitment",310189,879)

#Merge tables
event_table_for_figure3 <- rbind(event_table_for_figure3_part1_OK,event_table_for_figure3_part2_OK,event_table_for_figure3_part3_OK,event_table_for_figure3_part4_OK,event_table_for_figure3_part5_OK) 
colnames(event_table_for_figure3)
event_table_for_figure3$Variable <- as.character(event_table_for_figure3$Variable)
event_table_for_figure3_OK <- rbind(event_table_for_figure3,event_table_for_figure3_part6)
colnames(event_table_for_figure3_OK) =c("Variable","Non-cases\nNb of events","Cases\nNb of events")
event_table_for_figure3_OK <- event_table_for_figure3_OK[,c(1,3,2)]

library(gridExtra)
pdf(file = "Table_for_figure3.pdf")
grid.table(event_table_for_figure3_OK, rows=NULL)
dev.off()


######################## OTHER FACTORS with Smoke_Stat instead of Smoke_Intensity for adjustment - all H&N rare cancer cases
#Bmi_C
HN_Cox_adj6<-coxph(Surv(time_followup, case_hn)~BMI+Smoking_status+Alcohol_pattern+Hypertension+Diabetes+Sex+Age_at_recruitment+Country+School_level,data=DB_summary_HN_OK)
summary(HN_Cox_adj6)
suppfig3 <- ggforest(HN_Cox_adj6, data=as.data.frame(DB_summary_HN_OK))
	#Analysis with only smokers
HN_Cox_adj6b<-coxph(Surv(time_followup, case_hn)~BMI+Alcohol_pattern+Hypertension+Diabetes+Sex+Age_at_recruitment+Country+School_level,subset=(Smoking_status=="Smoker"), data=DB_summary_HN_OK)
summary(HN_Cox_adj6b)
ggforest(HN_Cox_adj6b, data=as.data.frame(DB_summary_HN_OK)) #=> HR=0.95 (CI: 0.92-0.98)
	#Analysis with only periodically heavy drinkers (remove low drinkers) - without Smoking_status=Unknown
DB_summary_HN_OK_without_smoking_unknown <- DB_summary_HN_OK %>% filter(Smoking_status != "Unknown")
DB_summary_HN_OK_without_smoking_unknown$Smoking_status = droplevels(DB_summary_HN_OK_without_smoking_unknown$Smoking_status)
HN_Cox_adj6c<-coxph(Surv(time_followup, case_hn)~BMI+Smoking_status+Hypertension+Diabetes+Sex+Age_at_recruitment+Country+School_level,subset=(Alcohol_pattern=="Periodically heavy drinkers"), data=DB_summary_HN_OK_without_smoking_unknown)
summary(HN_Cox_adj6c)
ggforest(HN_Cox_adj6c, data=as.data.frame(DB_summary_HN_OK_without_smoking_unknown)) #=> HR=0.95 (CI: 0.91-0.99)

#Pa_Total
HN_Cox_adj7<-coxph(Surv(time_followup, case_hn)~Physical_activity+Smoking_status+Alcohol_pattern+Hypertension+Diabetes+Sex+Age_at_recruitment+Country+School_level,data=DB_summary_HN_OK)
summary(HN_Cox_adj7)
suppfig4 <- ggforest(HN_Cox_adj7, data=as.data.frame(DB_summary_HN_OK))

#QE_ENERGY
HN_Cox_adj8<-coxph(Surv(time_followup, case_hn)~Energy_intake+Smoking_status+Alcohol_pattern+Hypertension+Diabetes+Sex+Age_at_recruitment+Country+School_level,data=DB_summary_HN_OK)
summary(HN_Cox_adj8)
suppfig5 <- ggforest(HN_Cox_adj8, data=as.data.frame(DB_summary_HN_OK))

#Asbestos_Cum

#Removal of cases with country="The Netherlands" because no data + UK because upper .95 =Inf for France (only 2 cases and no males) so analysis does not work
#379825 obs => 191997
DB_summary_asbestos <- DB_summary_HN_OK %>% filter(Country != "The Netherlands" & Country != "United Kingdom")
#Remove factor with no data
DB_summary_asbestos$Country = droplevels(DB_summary_asbestos$Country)

#Removal of cases with School_level="Missing" because upper .95 =Inf so analysis does not work
#191997 obs => 191520
DB_summary_asbestos <- DB_summary_asbestos %>% filter(School_level != "Missing")
#Remove factor with no data
DB_summary_asbestos$School_level = droplevels(DB_summary_asbestos$School_level)

HN_Cox_adj5<-coxph(Surv(time_followup, case_hn)~Asbestos+Smoking_status+Alcohol_pattern+Hypertension+Diabetes+Sex+Age_at_recruitment+Country+School_level,data=DB_summary_asbestos)
summary(HN_Cox_adj5)
suppfig6 <- ggforest(HN_Cox_adj5, data=as.data.frame(DB_summary_asbestos))

###################### LARYNX
DB_summary_larynx <- DB_summary_HN_OK

#All effective factors
HN_larynx_Cox_adj_all<-coxph(Surv(time_followup_larynx, case_hn_larynx)~Smoking_status+Alcohol_pattern+Hypertension+Diabetes+Sex+Age_at_recruitment+Country+School_level,data=DB_summary_larynx)
summary(HN_larynx_Cox_adj_all)
suppfig7 <- ggforest(HN_larynx_Cox_adj_all, data=as.data.frame(DB_summary_larynx))
table(DB_summary_larynx$case_hn_larynx) #268

#Proportional hazard assumption -> Schoenfeld residuals
cox.zph(HN_larynx_Cox_adj_all)

###################### ORAL CAVITY

#Removal of cases with Smoke_Stat="Unknown" because upper .95 =Inf so analysis does not work
#311068 obs => 308111
DB_summary_oral_cavity <- DB_summary_HN_OK %>% filter(Smoking_status != "Unknown")
#Remove factor with no data
DB_summary_oral_cavity$Smoking_status = droplevels(DB_summary_oral_cavity$Smoking_status)

HN_oral_cavity_Cox_adj_all<-coxph(Surv(time_followup_oral_cavity, case_hn_oral_cavity)~Smoking_status+Alcohol_pattern+Hypertension+Diabetes+Sex+Age_at_recruitment+Country+School_level,data=DB_summary_oral_cavity)  
summary(HN_oral_cavity_Cox_adj_all)
suppfig9 <- ggforest(HN_oral_cavity_Cox_adj_all, data=as.data.frame(DB_summary_oral_cavity))
table(DB_summary_oral_cavity$case_hn_oral_cavity) #186
cox.zph(HN_oral_cavity_Cox_adj_all)

###################### OROPHARYNX
DB_summary_oropharynx <- DB_summary_HN_OK %>% filter(Smoking_status != "Unknown")
#Remove factor with no data
DB_summary_oropharynx$Smoking_status = droplevels(DB_summary_oropharynx$Smoking_status)

HN_oropharynx_Cox_adj_all<-coxph(Surv(time_followup_oropharynx, case_hn_oropharynx)~Smoking_status+Alcohol_pattern+Hypertension+Diabetes+Sex+Age_at_recruitment+Country+School_level,data=DB_summary_oropharynx)
summary(HN_oropharynx_Cox_adj_all)
suppfig8 <- ggforest(HN_oropharynx_Cox_adj_all, data=as.data.frame(DB_summary_oropharynx))
table(DB_summary$case_hn_oropharynx) #177
cox.zph(HN_oropharynx_Cox_adj_all)

###################### HYPOPHARYNX
#Removal of cases with Smoke_Stat="Unknown" because upper .95 =Inf so analysis does not work
#311068 obs => 308111
DB_summary_hypopharynx <- DB_summary_HN_OK %>% filter(Smoking_status != "Unknown")
#Remove factor with no data
DB_summary_hypopharynx$Smoking_status = droplevels(DB_summary_hypopharynx$Smoking_status)

#Removal of cases with AlC_Pattern="Light drinkers" because upper .95 =Inf for AlC_Pattern=Light drinkers so analysis does not work
#308111 obs => 268459
DB_summary_hypopharynx <- DB_summary_hypopharynx %>% filter(Alcohol_pattern != "Light drinkers")
#Remove factor with no data
DB_summary_hypopharynx$Alcohol_pattern = droplevels(DB_summary_hypopharynx$Alcohol_pattern)

HN_hypopharynx_Cox_adj_all<-coxph(Surv(time_followup_hypopharynx, case_hn_hypopharynx)~Smoking_status+Alcohol_pattern+Hypertension+Diabetes+Sex+Age_at_recruitment+Country+School_level,data=DB_summary_hypopharynx)
summary(HN_hypopharynx_Cox_adj_all)
suppfig10 <- ggforest(HN_hypopharynx_Cox_adj_all, data=as.data.frame(DB_summary_hypopharynx))
table(DB_summary_hypopharynx$case_hn_hypopharynx) #53
cox.zph(HN_hypopharynx_Cox_adj_all)

###################### LIP
#Removal of cases with Smoke_Stat="Unknown" because upper .95 =Inf for Smoke_Stat=Unknown so analysis does not work
#311068 obs => 308111
DB_summary_lip <- DB_summary_HN_OK %>% filter(Smoking_status != "Unknown")
#Remove factor with no data
DB_summary_lip$Smoking_status = droplevels(DB_summary_lip$Smoking_status)

#Removal of cases with L_School="Longer education (incl. University deg.)" because upper .95 =Inf so analysis does not work
#308111 obs => 235247
DB_summary_lip <- DB_summary_lip %>% filter(School_level != "Longer")
#Remove factor with no data
DB_summary_lip$School_level = droplevels(DB_summary_lip$School_level)

#Removal of cases with Alc_Pattern="Former heavy drinkers" because upper .95 =Inf so analysis does not work
#235247 obs => 233801
DB_summary_lip <- DB_summary_lip %>% filter(Alcohol_pattern != "Former heavy drinkers")
#Remove factor with no data
DB_summary_lip$Alcohol_pattern = droplevels(DB_summary_lip$Alcohol_pattern)

#Removal of cases with Diabet="Do not know" because upper .95 =Inf so analysis does not work
#233801 obs => 205939
DB_summary_lip <- DB_summary_lip %>% filter(Diabetes != "Do not know")
#Remove factor with no data
DB_summary_lip$Diabetes = droplevels(DB_summary_lip$Diabetes)

HN_lip_Cox_adj_all<-coxph(Surv(time_followup_lip, case_hn_lip)~Smoking_status+Alcohol_pattern+Hypertension+Diabetes+Sex+Age_at_recruitment+Country+School_level,data=DB_summary_lip)
summary(HN_lip_Cox_adj_all)
suppfig11 <- ggforest(HN_lip_Cox_adj_all, data=as.data.frame(DB_summary_lip))
table(DB_summary_lip$case_hn_lip) #51
cox.zph(HN_lip_Cox_adj_all)
