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

################### Calculation of 5-year survival
model <- survfit(Surv(time_followup, case_hn)~Smoke_Stat+Alc_Pattern+Hypert+Diabet+Sex+Age_Recr+Country+L_School,data=as.data.frame(DB_summary))
summary(model,5)

################### Cox model - adjusted model which include standard risk factors

#Reorder factors to have reference with enough cases
DB_summary$Country <- factor(DB_summary$Country, levels=c("Denmark","Spain","United Kingdom","Germany","Italy","The Netherlands","France"))
levels(DB_summary$L_School)[levels(DB_summary$L_School)=='Longer education (incl. University deg.)'] <- 'Longer education' #Shortened the longer education level to better display in plots
DB_summary$L_School <- factor(DB_summary$L_School, levels=c("Primary school completed","Technical/professional school","Longer education","Secondary school","None","Not specified"))
DB_summary$Alc_Pattern <- factor(DB_summary$Alc_Pattern, levels=c("Never drinkers","Former light drinkers","Light drinkers","Never heavy drinkers","Periodically heavy drinkers","Always heavy drinkers","Former heavy drinkers","Unknown"))

################### ALL HN

###############################################################################################################
#All effective factors using Smoking intensity

HN_Cox_adj_all_v2 <-coxph(Surv(time_followup, case_hn)~Smoke_Intensity+Alc_Pattern+Hypert+Diabet+Sex+Age_Recr+Country+L_School,data=DB_summary)
summary(HN_Cox_adj_all_v2)
ggforest(HN_Cox_adj_all_v2, data=as.data.frame(DB_summary))

#Test of cox model - Proportional hazard assumption -> Schoenfeld residuals
test_v2.ph <- cox.zph(HN_Cox_adj_all_v2)
ggcoxzph(test_v2.ph, caption="Supplementary Figure 1: Stratification and Schoenfeld residual tests for head and neck rare cancers, based on a model incorporating all factors: \nsmoking intensity, alcohol consumption patterns, hypertension, diabetes, sex, age at recruitment, country, and education level")

#Other plot for v2 => Figure 3 of manuscript
HN_datarecord_adj_all_v2<-as.data.frame(summary(HN_Cox_adj_all_v2)$conf.int[,c(1,3,4)]) #just need the HR, lowerCI and UpperCI
HN_datarecord_adj_all_v2$Variables<-rownames(HN_datarecord_adj_all_v2)
HN_datarecord_adj_all_v2<-HN_datarecord_adj_all_v2 %>% rename(HR=`exp(coef)`, LowerCI=`lower .95`,UpperCI=`upper .95`)
HN_datarecord_adj_all_v2$Model<-"Adjusted for sex, age, country, smoking, alcohol, hypertension, diabet and highest school level"

HN_datarecord_adj_all_v2 <- mutate(HN_datarecord_adj_all_v2, Variables = case_when( 
  Variables == "Smoke_IntensityCurrent, 1-15 cig/day" ~ "Current smoker, 1-15 cig/day",
  Variables == "Smoke_IntensityCurrent, 16-25 cig/day" ~ "Current smoker, 16-25 cig/day",
  Variables == "Smoke_IntensityCurrent, 26+ cig/day" ~ "Current smoker, 26+ cig/day",
  Variables == "Smoke_IntensityFormer, quit <= 10 years" ~ "Former smoker, quit <= 10 years",
  Variables == "Smoke_IntensityFormer, quit 11-20 years" ~ "Former smoker, quit 11-20 years",
  Variables == "Smoke_IntensityFormer, quit 20+ years" ~ "Former smoker, quit 20+ years",
  Variables == "Smoke_IntensityCurrent, pipe/cigar/occas" ~ "Current smoker, pipe/cigar/occas",
  Variables == "Alc_PatternFormer light drinkers" ~ "Former light drinkers",
  Variables == "Alc_PatternLight drinkers" ~ "Light drinkers",
  Variables == "Alc_PatternNever heavy drinkers" ~ "Never heavy drinkers",
  Variables == "Alc_PatternPeriodically heavy drinkers" ~ "Periodically heavy drinkers",
  Variables == "Alc_PatternAlways heavy drinkers" ~ "Always heavy drinkers",
  Variables == "Alc_PatternFormer heavy drinkers" ~ "Former heavy drinkers",
  Variables == "HypertYes" ~ "Hypertension: Yes",
  Variables == "DiabetYes" ~ "Diabetes: Yes",
  Variables == "SexFemale" ~ "Female",
  Variables == "Age_Recr" ~ "Age at recruitment",
  TRUE ~ Variables))

HN_datarecord_adj_all_v2$Variables <- factor (HN_datarecord_adj_all_v2$Variables, levels=c("Age at recruitment","Female","Diabetes: Yes","Hypertension: Yes","Former heavy drinkers","Always heavy drinkers","Periodically heavy drinkers","Former light drinkers","Light drinkers","Never heavy drinkers","Former smoker, quit 20+ years","Former smoker, quit 11-20 years","Former smoker, quit <= 10 years","Current smoker, pipe/cigar/occas","Current smoker, 26+ cig/day","Current smoker, 16-25 cig/day","Current smoker, 1-15 cig/day"))
HN_datarecord_adj_all_v2$Group <- "NA"
  
  HN_datarecord_adj_all_v2 <- mutate(HN_datarecord_adj_all_v2, Group = case_when( 
    Variables == "Current smoker, 1-15 cig/day" ~ "Smoke intensity",
    Variables == "Current smoker, 16-25 cig/day" ~ "Smoke intensity",
    Variables == "Current smoker, 26+ cig/day" ~ "Smoke intensity",
    Variables == "Former smoker, quit <= 10 years" ~ "Smoke intensity",
    Variables == "Former smoker, quit 11-20 years" ~ "Smoke intensity",
    Variables == "Former smoker, quit 20+ years" ~ "Smoke intensity",
    Variables == "Current smoker, pipe/cigar/occas" ~ "Smoke intensity",
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
  HN_datarecord_adj_all_v2_filtered$Group_ok = factor(HN_datarecord_adj_all_v2_filtered$Group, levels=c("Smoke intensity","Alcohol pattern","H", "D", "Sex", "Age"))
  
  plot_All_adj_v2 <-ggplot(HN_datarecord_adj_all_v2_filtered,aes(x=HR,y=Variables))+
  geom_point(position = position_dodge(width=1), size=4)+ #to avoid overlapping + 
  geom_text(aes(HR, label=paste(round(HR,2)," (95% CI:",round(LowerCI,2),"-",round(UpperCI,2),")", sep="")), size=4, position = position_dodge(width=1), hjust=0.08, vjust=-1.2) +
  geom_errorbarh(aes(xmin=LowerCI,xmax=UpperCI),position = position_dodge(width=1), height=0.35, size=1)+
  geom_vline(xintercept = 1,lty=2, color="purple", size=1)+ #add HR=1 as significance threshold
  theme_bw(base_size=18) +
  scale_y_discrete(breaks=c("Current smoker, 1-15 cig/day", "Current smoker, 16-25 cig/day","Current smoker, 26+ cig/day","Former smoker, quit <= 10 years","Former smoker, quit 11-20 years","Former smoker, quit 20+ years",
                            "Current smoker, pipe/cigar/occas","Former light drinkers","Light drinkers","Never heavy drinkers","Periodically heavy drinkers","Always heavy drinkers","Former heavy drinkers","Hypertension: Yes","Diabetes: Yes","Female","Age at recruitment"),
                   labels=c("Current smoker, 1-15 cig/day", "Current smoker, 16-25 cig/day","Current smoker, 26+ cig/day","Former smoker, quit <= 10 years","Former smoker, quit 11-20 years","Former smoker, quit 20+ years",
                            "Current smoker, pipe/cigar/occas","Former light drinkers","Light drinkers","Never heavy drinkers","Periodically heavy drinkers","Always heavy drinkers","Former heavy drinkers","Hypertension: Yes","Diabetes: Yes","Female","Age at recruitment")) +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=20,face="bold"),plot.title = element_text(size = 20, face = "bold")) 

p.grid <- plot_All_adj_v2 + facet_grid(Group_ok ~ ., scales = "free_y", space = "free_y", switch="y")
p.grid #=> Figure 3

#Generate additional table with number per category

event_table_for_figure3_part1 <- as.data.frame(t(table(DB_summary$case_hn,DB_summary$Smoke_Intensity)))
colnames(event_table_for_figure3_part1) <- c("Variable", "Case","Count")
event_table_for_figure3_part1 <- reshape(event_table_for_figure3_part1, direction='wide', idvar=c("Variable"), timevar="Case")
event_table_for_figure3_part1 <- event_table_for_figure3_part1 %>% filter (!Variable %in% c("Never","Current/Former, missing","Unknown"))
event_table_for_figure3_part1_OK <- event_table_for_figure3_part1[c(1:3,7,4:6), ]

event_table_for_figure3_part2 <- as.data.frame(t(table(DB_summary$case_hn,DB_summary$Alc_Pattern)))
colnames(event_table_for_figure3_part2) <- c("Variable", "Case","Count")
event_table_for_figure3_part2 <- reshape(event_table_for_figure3_part2, direction='wide', idvar=c("Variable"), timevar="Case")
event_table_for_figure3_part2 <- event_table_for_figure3_part2 %>% filter (!Variable %in% c("Never drinkers","Unknown"))
event_table_for_figure3_part2_OK <- event_table_for_figure3_part2[c(3,2,1,4:6), ]

event_table_for_figure3_part3 <- as.data.frame(t(table(DB_summary$case_hn,DB_summary$Hypert)))
colnames(event_table_for_figure3_part3) <- c("Variable", "Case","Count")
event_table_for_figure3_part3 <- reshape(event_table_for_figure3_part3, direction='wide', idvar=c("Variable"), timevar="Case")
event_table_for_figure3_part3_OK <- event_table_for_figure3_part3 %>% filter (!Variable %in% c("No","Do not know"))
levels(event_table_for_figure3_part3_OK$Variable)[match("Yes",levels(event_table_for_figure3_part3_OK$Variable))] <- "Hypertension: Yes"

event_table_for_figure3_part4 <- as.data.frame(t(table(DB_summary$case_hn,DB_summary$Diabet)))
colnames(event_table_for_figure3_part4) <- c("Variable", "Case","Count")
event_table_for_figure3_part4 <- reshape(event_table_for_figure3_part4, direction='wide', idvar=c("Variable"), timevar="Case")
event_table_for_figure3_part4_OK <- event_table_for_figure3_part4 %>% filter (!Variable %in% c("No","Do not know"))
levels(event_table_for_figure3_part4_OK$Variable)[match("Yes",levels(event_table_for_figure3_part4_OK$Variable))] <- "Diabet: Yes"

event_table_for_figure3_part5 <- as.data.frame(t(table(DB_summary$case_hn,DB_summary$Sex)))
colnames(event_table_for_figure3_part5) <- c("Variable", "Case","Count")
event_table_for_figure3_part5 <- reshape(event_table_for_figure3_part5, direction='wide', idvar=c("Variable"), timevar="Case")
event_table_for_figure3_part5_OK <- event_table_for_figure3_part5 %>% filter (!Variable %in% c("Male"))

#Merge tables
event_table_for_figure3 <- rbind(event_table_for_figure3_part1_OK,event_table_for_figure3_part2_OK,event_table_for_figure3_part3_OK,event_table_for_figure3_part4_OK,event_table_for_figure3_part5_OK) 
colnames(event_table_for_figure3) =c("Variable","Controls - nb of events","Cases - nb of events")
event_table_for_figure3_OK <- event_table_for_figure3[,c(1,3,2)]

library(gridExtra)
pdf(file = "Table_for_figure3.pdf")
grid.table(event_table_for_figure3_OK, rows=NULL)
dev.off()


######################## OTHER FACTORS with Smoke_Stat instead of Smoke_Intensity for adjustment - all H&N rare cancer cases
#Bmi_C
HN_Cox_adj6<-coxph(Surv(time_followup, case_hn)~Bmi_C+Smoke_Stat+Alc_Pattern+Hypert+Diabet+Sex+Age_Recr+Country+L_School,data=DB_summary)
summary(HN_Cox_adj6)
ggforest(HN_Cox_adj6, data=as.data.frame(DB_summary), main = "Forest plot for coxph model with BMI\n (adjusted by smoking status, alcohol pattern, hypertension, diabetes, sex, age at recruitment, country and school level)\n \n Hazard Ratio")
#Pa_Total
HN_Cox_adj7<-coxph(Surv(time_followup, case_hn)~Pa_Total+Smoke_Stat+Alc_Pattern+Hypert+Diabet+Sex+Age_Recr+Country+L_School,data=DB_summary)
summary(HN_Cox_adj7)
ggforest(HN_Cox_adj7, data=as.data.frame(DB_summary), main = "Forest plot for coxph model with Physical activity\n (adjusted by smoking status, alcohol pattern, hypertension, diabetes, sex, age at recruitment, country and school level)\n \n Hazard Ratio")
#QE_ENERGY
HN_Cox_adj8<-coxph(Surv(time_followup, case_hn)~QE_ENERGY+Smoke_Stat+Alc_Pattern+Hypert+Diabet+Sex+Age_Recr+Country+L_School,data=DB_summary)
summary(HN_Cox_adj8)
ggforest(HN_Cox_adj8, data=as.data.frame(DB_summary), main = "Forest plot for coxph model with Energy intake\n (adjusted by smoking status, alcohol pattern, hypertension, diabetes, sex, age at recruitment, country and school level)\n \n Hazard Ratio")

#Asbestos_Cum

#Removal of cases with country="France" & "The Netherlands" because no data + UK because upper .95 =Inf for France (only 2 cases and no males) so analysis does not work
#379825 obs => 191997
DB_summary_asbestos <- DB_summary %>% filter(Country != "France" & Country != "The Netherlands" & Country != "United Kingdom")
#Remove factor with no data
DB_summary_asbestos$Country = droplevels(DB_summary_asbestos$Country)

#Removal of cases with L_School="Not specified" because upper .95 =Inf so analysis does not work
#191997 obs => 191520
DB_summary_asbestos <- DB_summary_asbestos %>% filter(L_School != "Not specified")
#Remove factor with no data
DB_summary_asbestos$L_School = droplevels(DB_summary_asbestos$L_School)

HN_Cox_adj5<-coxph(Surv(time_followup, case_hn)~Asbestos_Cum+Smoke_Stat+Alc_Pattern+Hypert+Diabet+Sex+Age_Recr+Country+L_School,data=DB_summary_asbestos)
summary(HN_Cox_adj5)
ggforest(HN_Cox_adj5, data=as.data.frame(DB_summary), main = "Forest plot for coxph model with Asbestos exposure\n (adjusted by smoking status, alcohol pattern, hypertension, diabetes, sex, age at recruitment, country and school level)\n \n Hazard Ratio")

###################### LARYNX
#Removal of cases with country="France" because upper .95 =Inf for France (only 2 cases and no males) so analysis does not work
#379825 obs => 311068

DB_summary_larynx <- DB_summary %>% filter(Country != "France")
#Remove factor with no data
DB_summary_larynx$Country = droplevels(DB_summary_larynx$Country)

#All effective factors
HN_larynx_Cox_adj_all<-coxph(Surv(time_followup_larynx, case_hn_larynx)~Smoke_Stat+Alc_Pattern+Hypert+Diabet+Sex+Age_Recr+Country+L_School,data=DB_summary_larynx)
summary(HN_larynx_Cox_adj_all)
ggforest(HN_larynx_Cox_adj_all, data=as.data.frame(DB_summary_larynx), main = "Forest plot for coxph model with Smoking status and Alcohol pattern for Larynx subtype (n=268)\n (without Country=France and adjusted by hypertension, diabetes, sex, age at recruitment, country and school level)\n \n Hazard Ratio")
table(DB_summary_larynx$case_hn_larynx) #268

#Proportional hazard assumption -> Schoenfeld residuals
cox.zph(HN_larynx_Cox_adj_all)

###################### ORAL CAVITY
#Removal of cases with country="France" because upper .95 =Inf for France (only 2 cases and no males) so analysis does not work
#379825 obs => 311068
DB_summary_oral_cavity <- DB_summary %>% filter(Country != "France")
#Remove factor with no data
DB_summary_oral_cavity$Country = droplevels(DB_summary_oral_cavity$Country)

#Removal of cases with Smoke_Stat="Unknown" because upper .95 =Inf so analysis does not work
#311068 obs => 308111
DB_summary_oral_cavity <- DB_summary_oral_cavity %>% filter(Smoke_Stat != "Unknown")
#Remove factor with no data
DB_summary_oral_cavity$Smoke_Stat = droplevels(DB_summary_oral_cavity$Smoke_Stat)

HN_oral_cavity_Cox_adj_all<-coxph(Surv(time_followup_oral_cavity, case_hn_oral_cavity)~Smoke_Stat+Alc_Pattern+Hypert+Diabet+Sex+Age_Recr+Country+L_School,data=DB_summary_oral_cavity)  
summary(HN_oral_cavity_Cox_adj_all)
ggforest(HN_oral_cavity_Cox_adj_all, data=as.data.frame(DB_summary_oral_cavity), main = "Forest plot for coxph model with Smoking status and Alcohol pattern for Oral Cavity subtype (n=186)\n (without Country=France and Smoke_Stat=Unknown and adjusted by hypertension, diabetes, sex, age at recruitment, country and school level)\n \n Hazard Ratio")
table(DB_summary_oral_cavity$case_hn_oral_cavity) #186

cox.zph(HN_oral_cavity_Cox_adj_all)

###################### OROPHARYNX
HN_oropharynx_Cox_adj_all<-coxph(Surv(time_followup_oropharynx, case_hn_oropharynx)~Smoke_Stat+Alc_Pattern+Hypert+Diabet+Sex+Age_Recr+Country+L_School,data=DB_summary)
summary(HN_oropharynx_Cox_adj_all)
ggforest(HN_oropharynx_Cox_adj_all, data=as.data.frame(DB_summary), main = "Forest plot for coxph model with Smoking status and Alcohol pattern for Oropharynx subtype (n=177)\n (adjusted by hypertension, diabetes, sex, age at recruitment, country and school level)\n \n Hazard Ratio")
table(DB_summary$case_hn_oropharynx) #177

cox.zph(HN_oropharynx_Cox_adj_all)

###################### HYPOPHARYNX
#Removal of cases with country="France" because upper .95 =Inf for France (only 2 cases and no males) so analysis does not work
#379825 obs => 311068
DB_summary_hypopharynx <- DB_summary %>% filter(Country != "France")
#Remove factor with no data
DB_summary_hypopharynx$Country = droplevels(DB_summary_hypopharynx$Country)

#Removal of cases with Smoke_Stat="Unknown" because upper .95 =Inf so analysis does not work
#311068 obs => 308111
DB_summary_hypopharynx <- DB_summary_hypopharynx %>% filter(Smoke_Stat != "Unknown")
#Remove factor with no data
DB_summary_hypopharynx$Smoke_Stat = droplevels(DB_summary_hypopharynx$Smoke_Stat)

#Removal of cases with AlC_Pattern="Light drinkers" because upper .95 =Inf for AlC_Pattern=Light drinkers so analysis does not work
#308111 obs => 268459
DB_summary_hypopharynx <- DB_summary_hypopharynx %>% filter(Alc_Pattern != "Light drinkers")
#Remove factor with no data
DB_summary_hypopharynx$Alc_Pattern = droplevels(DB_summary_hypopharynx$Alc_Pattern)

HN_hypopharynx_Cox_adj_all<-coxph(Surv(time_followup_hypopharynx, case_hn_hypopharynx)~Smoke_Stat+Alc_Pattern+Hypert+Diabet+Sex+Age_Recr+Country+L_School,data=DB_summary_hypopharynx)
summary(HN_hypopharynx_Cox_adj_all)
ggforest(HN_hypopharynx_Cox_adj_all, data=as.data.frame(DB_summary_hypopharynx), main = "Forest plot for coxph model with Smoking status and Alcohol pattern for Hypopharynx subtype (n=53)\n (without Country=France, Smoke_Stat=Unknown and Alc_Pattern=Light drinkers \n & adjusted by hypertension, diabetes, sex, age at recruitment, country and school level)\n \n Hazard Ratio")
table(DB_summary_hypopharynx$case_hn_hypopharynx) #53

cox.zph(HN_hypopharynx_Cox_adj_all)

###################### LIP
#Removal of cases with country="France" because upper .95 =Inf for France (only 2 cases and no males) so analysis does not work
#379825 obs => 311068
DB_summary_lip <- DB_summary %>% filter(Country != "France")
#Remove factor with no data
DB_summary_lip$Country = droplevels(DB_summary_lip$Country)

#Removal of cases with Smoke_Stat="Unknown" because upper .95 =Inf for Smoke_Stat=Unknown so analysis does not work
#311068 obs => 308111
DB_summary_lip <- DB_summary_lip %>% filter(Smoke_Stat != "Unknown")
#Remove factor with no data
DB_summary_lip$Smoke_Stat = droplevels(DB_summary_lip$Smoke_Stat)

#Removal of cases with L_School="Longer education (incl. University deg.)" because upper .95 =Inf so analysis does not work
#308111 obs => 235247
DB_summary_lip <- DB_summary_lip %>% filter(L_School != "Longer education")
#Remove factor with no data
DB_summary_lip$L_School = droplevels(DB_summary_lip$L_School)

#Removal of cases with Alc_Pattern="Former heavy drinkers" because upper .95 =Inf so analysis does not work
#235247 obs => 233801
DB_summary_lip <- DB_summary_lip %>% filter(Alc_Pattern != "Former heavy drinkers")
#Remove factor with no data
DB_summary_lip$Alc_Pattern = droplevels(DB_summary_lip$Alc_Pattern)

#Removal of cases with Diabet="Do not know" because upper .95 =Inf so analysis does not work
#233801 obs => 205939
DB_summary_lip <- DB_summary_lip %>% filter(Diabet != "Do not know")
#Remove factor with no data
DB_summary_lip$Diabet = droplevels(DB_summary_lip$Diabet)

HN_lip_Cox_adj_all<-coxph(Surv(time_followup_lip, case_hn_lip)~Smoke_Stat+Alc_Pattern+Hypert+Diabet+Sex+Age_Recr+Country+L_School,data=DB_summary_lip)
summary(HN_lip_Cox_adj_all)
ggforest(HN_lip_Cox_adj_all, data=as.data.frame(DB_summary_lip), main = "Forest plot for coxph model with Smoking status and Alcohol pattern for Lip subtype (n=51)\n (without Country=France, Smoke_Stat=Unknown, L_School=Longer education, Alc_Pattern=Former heavy drinkers and Diabet=Do not know \n & adjusted by hypertension, diabetes, sex, age at recruitment, country and school level)\n \n Hazard Ratio")
table(DB_summary_lip$case_hn_lip) #51
