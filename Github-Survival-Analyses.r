library(survival)
library(survminer)

#Keep only those who are alive or dead
cohort_with_vit_stat <- cohort %>% filter(Vit_Stat == 1 | Vit_Stat == 2)
cohort_with_vit_stat <- cohort_with_vit_stat %>% mutate(Vital_Status = as_factor(Vit_Stat))
cohort_with_vit_stat <- cohort_with_vit_stat %>% mutate(Vital_Status=recode(Vital_Status, `Alive` = 0, `Dead` = 1))

rare_cancer_cases_with_vit_stat <- cohort_with_vit_stat %>% filter(Cncr_Evt_Rare == 1) # => 8824 cases
common_cancer_cases_with_vit_stat <- cohort_with_vit_stat %>% filter(Any_Other_Cancer == 1) # => 39355 cases

#Rare cancers
fit_rare <- survfit(Surv(Fu_Time_Rare, Vital_Status) ~ 1 , data=rare_cancer_cases_with_vit_stat, type="kaplan-meier")
plot(fit_rare)
str(fit_rare)
summary(fit_rare, times=5)

#Common cancers
fit_common <- survfit(Surv(Fu_Time_Other_Cancer, Vital_Status) ~ 1 , data=common_cancer_cases_with_vit_stat, type="kaplan-meier")
plot(fit_common)
summary(fit_common, times=5)