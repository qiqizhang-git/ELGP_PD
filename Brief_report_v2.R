# 1. Use cleaned drug data
# 2. EXCLUDE 2 subject with unknown Epilepsy from df_10_1
# 3. Updated the dummy date algorithm by using matched case diagnosis date
# 4. Add a category of the AED_over10yr to show people have AED prescribed more than 10 years before PD diagnosis

# rm(list=ls())
####### Define functions ######
lu = function(x){length(unique(x))}

####### Read Matched Data ######
date.orig = as.Date("2023-01-01") - as.numeric(as.Date("2023-01-01"))
df_10_1 = read.csv("G:/CCP/PNU Data/East London GP Project/ELGP 10 to 1 matched.csv") %>% 
  mutate(imd_quintile = factor(imd_quintile, 
                               levels = c("01-Feb","03-Apr","05-Jun","07-Aug","09-Oct","Unknown"),
                               labels = c("1-2","3-4","5-6","7-8","9-10","Unknown"))) %>% # correcting the format issue
  mutate(ethnic = factor(ethnic, levels = c("White","Black","S.Asian","Other","Unknown"))) %>%
  filter(epilepsy!="Unknown") %>% 
  mutate(date_pd  = as.Date(date_pd,  origin = date.orig))

nrow(df_10_1)
table(df_10_1$epilepsy, exclude = NULL)

## Format df_10_1
# find the origin date:
# date.orig = as.Date("2023-01-01") - as.numeric(as.Date("2023-01-01")) # "1970-01-01"
# as.Date(as.numeric(as.Date("2023-01-01")), origin = date.orig) # "2023-01-01"
# format
# df_10_1 = df_10_1 %>% 
  # mutate(date_pd  = as.Date(date_pd,  origin = date.orig),
  #        date_pd2 = as.Date(date_pd2, origin = date.orig),
  #        date_pd3 = as.Date(date_pd3, origin = date.orig)) 
  
####### Create 5:1 Matched Data ######
## Updated: 2023/02/20 use dataset 1:5 instead, to remove the duplicated records in dataset 1:10
# Find duplicate ID
n_occur = data.frame(table(df_10_1$id))
n_occur[1:10,]
table(n_occur$Freq)
dup.id = n_occur[n_occur$Freq > 1,]
uni.id = n_occur[n_occur$Freq == 1,]
dim(dup.id); sum(dup.id$Freq)
dim(uni.id)
id.dup = dup.id$Var1
length(id.dup)
# Drop duplicate ID
df_10_1 %>% filter(id %in% id.dup) %>% count(pd2) # all duplicated id are controls
# For each matchset, keep the case, and randomly select 5 controls
df_5_1_case = df_10_1 %>% filter(!id %in% id.dup & pd2==1)
nrow(df_5_1_case)
set.seed(12345)
df_5_1_control = df_10_1 %>% 
  filter(!id %in% id.dup & pd2==0) %>% 
  group_by(matchset) %>%  
  mutate(rand_n = runif(length(matchset)) ) %>%
  top_n(n=5, wt=rand_n) %>% 
  select(-rand_n)
nrow(df_5_1_control)
table(data.frame(table(df_5_1_control$id))$Freq) # all id only appeared once
# Combine case set and control set
df_5_1 = rbind(df_5_1_case, df_5_1_control) %>% 
  mutate(epilepsy_numeric = case_when(epilepsy=="Yes" ~ 1, epilepsy=="Normal" ~ 0))
names(df_5_1)
rm(df_10_1)
####### Read Drug data #######
datapath = "G:/CCP/East_London_GP_Project/"
epildrug = readRDS(paste0(datapath,"Data/Drugs_clean/df_epildrug.rds"))
head(epildrug)
names(epildrug)
no.early = length(which(!is.na(epildrug$epildrugdate_e_new)))  # 56088
length(which(!is.na(epildrug$epildrugdate_e_new_new))) # 56060
no.late  = length(which(!is.na(epildrug$epildrugdate6m_l_new)))# 24914
no.early.and.late = length(which(!is.na(epildrug$epildrugdate_e_new) & !is.na(epildrug$epildrugdate6m_l_new))) # 24914 having both early and late
no.yesearly.nolate = length(which(!is.na(epildrug$epildrugdate_e_new) &is.na(epildrug$epildrugdate6m_l_new) )) 
no.noearly.yeslate = length(which(is.na(epildrug$epildrugdate_e_new) &!is.na(epildrug$epildrugdate6m_l_new) )) 

#### Create aed_pd ####
aed_pd = 
  df_5_1 %>% select(id, gender, age, age2, ethnic, year_birth, pd2, date_pd, imd_quintile,
                    contains("epilepsy"),matchset) %>% 
  merge(epildrug,by=c("id", "year_birth"),all.x = T)

names(aed_pd)
table(is.na(aed_pd$year_birth))
class(aed_pd$date_pd) 
summary(aed_pd$date_pd)
AED_freq = sort(table(epildrug$epildrug_e),   exclude = NULL, decreasing = T)

##### Create Var to show AED category #####
aed_pd_df_0 = aed_pd %>% 
  mutate(anyAED = epildrug_Drug ) %>% 
  mutate(pd2_f  = factor(pd2, levels = c(1,0), labels = c("Case", "Control"))) %>% 
  ## dummy diagnosis date for controls
  group_by(matchset) %>% 
  mutate(date_PD_dummy_2 = if_else(pd2==0, date_pd[which(pd2==1)], date_pd)) %>% 
  ungroup() 

aed_pd_df_1 = aed_pd_df_0%>% 
  ## compare with PD
  mutate(
    AED_Term_PD_e = case_when(epildrugdate_e_new<date_PD_dummy_2 ~ epildrug_e, TRUE ~ as.character(NA)),
    AED_Term_PD_l = case_when(epildrugdate6m_l_new<date_PD_dummy_2 ~ epildrug6m_l, TRUE ~ as.character(NA))
  ) %>% 
  # Compare Drug date vs PD date, based on date_PD_dummy_2. [Set Drug Date after PD as NA (if term is NA then Date is NA)]
  mutate(
    AED_Date_PD_e   = if_else(!is.na(AED_Term_PD_e), epildrugdate_e_new, as.Date(NA)),
    AED_Date_PD_l   = if_else(!is.na(AED_Term_PD_l), epildrugdate6m_l_new, as.Date(NA))
  ) %>% 
  # Update "binary Date" after treating AED AFTER PD as nonAED
  mutate(AED_Date_PD_binary = if_else(epildrugdate_e_new<date_PD_dummy_2, epildrug_Date_binary, 0)
  ) %>% 
  # Update "binary Term" after treating AED AFTER PD as nonAED
  mutate(AED_Term_PD_binary = if_else(epildrugdate_e_new<date_PD_dummy_2, epildrug_Term_binary, 0)
  ) %>% 
  # Update "Drug" exposure after treating AED AFTER PD as nonAED
  mutate(AED_Drug_PD = if_else(epildrugdate_e_new>=date_PD_dummy_2 & epildrug_Drug=="Exposed",as.factor("Non_Exposed"), epildrug_Drug))  %>% 
  droplevels()

table(aed_pd_df_1$anyAED, aed_pd_df_1$pd2_f)
table(is.na(aed_pd_df_1$epildrug_e))
table(is.na(aed_pd_df_1$AED_Term_PD_e))

table((aed_pd_df_1$epildrug_Drug))
table((aed_pd_df_1$AED_Drug_PD))
class(aed_pd_df_1$epildrug_Drug)
class((aed_pd_df_1$AED_Drug_PD))
##############################################################################

########################### Updated on 2023.03.22 ###########################
# The aim of our time interval code is to only KEEP those with in intervals of <2. 2-5. 5-10; rather than to Exclude them
# Previous code from "pd_risk_scores_analysis_v6_JB4" is not straightforward - 
# It treated patients with AED>=2yr as "Normal", therefore only <2 were treated as "risk factor presented".
# e.g. " # B) PRE-DIAGNOSTIC RISK FACTORS BY <2 YEAR:" vs "(df$x<=0 | df$x>730)". 
# Code was updated as below, to literally ONLY KEEP those within intervals
#############################################################################
aed_pd_df_2 = aed_pd_df_1 %>% 
  mutate(x  = as.numeric(date_PD_dummy_2 - epildrugdate_e_new)) %>%  
  # A: PRE-DIAGNOSTIC RISK FACTORS BY <2 YEAR: (0, 2yr)
  mutate(AED_0to2yr = if_else(AED_Drug_PD=="Exposed" & x <365*2, as.factor("lessthan2yr"),anyAED)) %>% 
  
  # B: PRE-DIAGNOSTIC RISK FACTORS BY  >=2 & <5 YEARS:
  mutate(AED_2to5yr = if_else(AED_Drug_PD=="Exposed" & (x>=365*2 & x <(365*5+1)), as.factor("2to5yr"),anyAED)) %>% 
  
  # C: PRE-DIAGNOSTIC RISK FACTORS BY  >=5 & <10 YEARS:
  mutate(AED_5to10yr = if_else(AED_Drug_PD=="Exposed" & (x>=(365*5+1) & x <(365*10+3)), as.factor("5to10yr"),anyAED)) %>% 
  
  # D: PRE-DIAGNOSTIC RISK FACTORS BY  <5 YEARS:
  mutate(AED_0to5yr = if_else(AED_Drug_PD=="Exposed" & x<(365*5+1), as.factor("lessthan5yr"),anyAED)) %>% 
  
  # E: PRE-DIAGNOSTIC RISK FACTORS BY <10 YEARS: (0, 10yr)
  mutate(AED_0to10yr = if_else(AED_Drug_PD=="Exposed" & x <(365*10+3), as.factor("lessthan10yr"),anyAED)) %>% 
  
  # E.2: PRE-DIAGNOSTIC RISK FACTORS BY >=10 YEARS: (10yr, +...)
  mutate(AED_over10yr = if_else(AED_Drug_PD=="Exposed" & x >=(365*10+3), as.factor("over10yr"),anyAED)) %>%
  
  # F: INTERVAL variable
  mutate(AED_interval = case_when(AED_Drug_PD=="Exposed" & x<365*2 ~ "AED_0to2yr",
                                  AED_Drug_PD=="Exposed" & (x>=365*2 & x <(365*5+1)) ~ "AED_2to5yr",
                                  AED_Drug_PD=="Exposed" & (x>=(365*5+1) & x <(365*10+3)) ~ "AED_5to10yr",
                                  AED_Drug_PD=="Exposed" & (x>=(365*10+3)) ~ "AED_over10yr",
                                  AED_Drug_PD=="Non_Exposed" ~ "Non_Exposed") ) %>% 
  mutate(AED_interval = relevel(factor(AED_interval, 
                                       levels = c("AED_0to2yr","AED_2to5yr","AED_5to10yr", "AED_over10yr","Non_Exposed")), 
                                ref="Non_Exposed")) 

table(aed_pd_df_2$AED_0to2yr,   aed_pd_df_2$pd2_f)
table(aed_pd_df_2$AED_2to5yr,   aed_pd_df_2$pd2_f)
table(aed_pd_df_2$AED_5to10yr,  aed_pd_df_2$pd2_f)
table(aed_pd_df_2$AED_interval, aed_pd_df_2$pd2_f)
addmargins(table(aed_pd_df_2$AED_interval, aed_pd_df_2$pd2_f))
# table(aed_pd_df_2$AED_0to5yr,  aed_pd_df_2$pd2_f)
# table(aed_pd_df_2$AED_0to10yr,  aed_pd_df_2$pd2_f)

# aed_pd_df_2$x <- NULL
# aed_pd_df_3 = aed_pd_df_2 %>% filter(AED_0to2yr!="lessthan2yr") %>% droplevels() # this is for reports before 2023.03.22

aed_pd_df_0to2yr  = aed_pd_df_2 %>% filter(AED_interval=="AED_0to2yr"  | AED_interval=="Non_Exposed")
aed_pd_df_2to5yr  = aed_pd_df_2 %>% filter(AED_interval=="AED_2to5yr"  | AED_interval=="Non_Exposed")
aed_pd_df_5to10yr = aed_pd_df_2 %>% filter(AED_interval=="AED_5to10yr" | AED_interval=="Non_Exposed")
aed_pd_df_over10yr = aed_pd_df_2 %>% filter(AED_interval=="AED_over10yr" | AED_interval=="Non_Exposed")

#### Other_freq: Output breakdown of Other AED #### 
# Other_freq = aed_pd_df_2 %>% filter(AED_cat=="Other") %>% count(epildrug_e,epilepsy) %>% arrange(-n)
# write.csv(Other_freq,"./Output/Misc/Other_freq.csv")

# merge PD term with data to check what terms are excluded
# aed_pd_df_3 = aed_pd_df_2 %>% merge(neuro %>% select(id, pdterm_e, pdcode_e), by = "id", all.x=T)
# setdiff(unique(neuro$pdterm_e), unique(aed_pd_df_3$pdterm_e))
# setdiff(unique(aed_pd_df_3$pdterm_e), unique(neuro$pdterm_e))
# 
# setdiff(unique(neuro$pdcode_e), unique(aed_pd_df_3$pdcode_e))
# setdiff(unique(aed_pd_df_3$pdcode_e), unique(neuro$pdcode_e))

################## Analysis ###########


