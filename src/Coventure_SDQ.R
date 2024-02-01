### Install the packages below
library(haven)
library(dplyr)
library(tidyr)
library(data.table)
library(brms)
#library(formattable)
library(broom)
#library(psych)


### Put the working directory to where data file is ####
setwd("/data")
cov <- read_sav("coventurefinaldataset.sav")

##### filter out those with NO_SURPS_NO_DEPADO ##### 
cov= cov[cov$NO_SURPS_NO_DEPADO==0,]

##### Creating different riskhilo variables, PC agreed on the relevance of riskhilo3 ##### 
cov$riskhilo2=1*((cov$AssignGroup>0&cov$Exp_Group==1)|(cov$riskhilo==1&cov$Exp_Group==0))
cov$riskhilo3=1*(cov$RiskStatus>0)

##### selecting relevant variables, recoding the flagged variable , and filter out high risks #####
vars=c("schoolnum","Exp_Group","AgeAtTesting_Y5","Language","DEM_01_Y1","SES_Y1","riskhilo","NO_SURPS","DEPAPO_ECHELLE_LUMIERE_Y1",
       "DEPAPO_ECHELLE_LUMIERE_Y2","DEPAPO_ECHELLE_LUMIERE_Y3","DEPAPO_ECHELLE_LUMIERE_Y4","DEPAPO_ECHELLE_LUMIERE_Y5",
       "SDQ_echelle_total_difficulties_Y1","SDQ_echelle_total_difficulties_Y2","SDQ_echelle_total_difficulties_Y3","SDQ_echelle_total_difficulties_Y4",  
       "SDQ_echelle_total_difficulties_Y5")


cov_2=cov%>%dplyr::select(starts_with(vars))%>%mutate(id=1:n(),year1=1,year2=2,year3=3,year4=4,year5=5,Flagged_ECHELLE_LUMIERE_Y1=DEPAPO_ECHELLE_LUMIERE_Y1, DIFF_echelle_total_difficulties_Y1=SDQ_echelle_total_difficulties_Y1)
cov_2= cov_2[cov_2$NO_SURPS_NO_DEPADO==0,]
cov_2= cov_2[cov_2$riskhilo3==1,]


##### Renaming variables for conformity, if this step is ignored the wide to long transformation will be faulty #####
##### This step is added for dashed and undashed variables to be separated in the long format #####

colnames(cov_2)
cov_3=cov_2%>% rename("Flagged_ECHELLE_LUMIERE_Y2"=        "DEPAPO_ECHELLE_LUMIERE_Y2_1"  ,
                      "Flagged_ECHELLE_LUMIERE_Y3"=        "DEPAPO_ECHELLE_LUMIERE_Y3_1"  ,
                      "Flagged_ECHELLE_LUMIERE_Y4"=        "DEPAPO_ECHELLE_LUMIERE_Y4_1"  ,
                      "Flagged_ECHELLE_LUMIERE_Y5"=        "DEPAPO_ECHELLE_LUMIERE_Y5_1"  ,
                      "DIFF_echelle_total_difficulties_Y2"="SDQ_echelle_total_difficulties_Y2_1"  ,
                      "DIFF_echelle_total_difficulties_Y3"="SDQ_echelle_total_difficulties_Y3_1"  ,
                      "DIFF_echelle_total_difficulties_Y4"="SDQ_echelle_total_difficulties_Y4_1"  ,
                      "DIFF_echelle_total_difficulties_Y5"="SDQ_echelle_total_difficulties_Y5_1"   
)
colnames(cov_3)        
cov_4=cov_3%>% rename("Age"="AgeAtTesting_Y5"  ,
                      "DEM_01"=        "DEM_01_Y1"  ,
                      "SES"=        "SES_Y1"   )


##### Creating The long format dataset without IPW #####

cov_4=cov_4 %>% dplyr::select(order(colnames(cov_4)))
colnames(cov_4)    
cov_5=melt(setDT(cov_4), 
           measure = patterns('DEPAPO_', 'Flagged_','DIFF_', 'SDQ_', "year"),
           variable.name = 'var', value.name = c('DEPADO', 'Flagged','DIFF', 'SDQ', "year"))
colnames(cov_5)  

# in the long dataset DIFF is dashed
#psych::describeBy(cov_5$DIFF, cov_5$year, mat = TRUE) 
# in the long dataset SDQ is undashed
#psych::describeBy(cov_5$SDQ, cov_5$year, mat = TRUE) 
################# SDQ model with continuous time variable ################

model_Undashed_SDQ <- brm(SDQ ~ 1 + DEM_01 + Exp_Group + Language + year + year:Exp_Group + (1|id+schoolnum),  
              data = cov_5, 
              warmup = 1000, iter = 3000, 
              cores = 4, chains = 4, 
              seed = 123)      
mod_sdq_a=summary(model_Undashed_SDQ)

saveRDS(model_Undashed_SDQ, "/models/sdq/models/model-undashed-sdq.rds")
saveRDS(mod_sdq_a, "/models/sdq/mod_subs-undashed-sdq-a.rds")
#formattable(mod_sdq_a$fixed)

model_Dashed_SDQ <- brm(DIFF ~ 1 + DEM_01 + Exp_Group + Language + year + year:Exp_Group + (1|id+schoolnum),  
              data = cov_5, 
              warmup = 1000, iter = 3000, 
              cores = 4, chains = 4, 
              seed = 123)      
mod_sdq_b=summary(model_Dashed_SDQ)

saveRDS(model_Dashed_SDQ, "/models/sdq/models/model-dashed-sdq.rds")
saveRDS(mod_sdq_b, "/models/sdq/mod_subs-dashed-sdq-b.rds")
#formattable(mod_sdq_b$fixed)
################# SDQ model with categorical time variable ################
cov_5$year=as.factor(cov_5$year)

model_Undashed_SDQ_C <- brm(SDQ ~ 1 + DEM_01 + Exp_Group + Language + year + year:Exp_Group + (1|id+schoolnum),  
                        data = cov_5, 
                        warmup = 1000, iter = 3000, 
                        cores = 4, chains = 4, 
                        seed = 123)      
mod_sdq_a_C=summary(model_Undashed_SDQ_C)

saveRDS(model_Undashed_SDQ_C, "/models/sdq/models/model-undashed-sdq-c.rds")
saveRDS(mod_sdq_a_C, "/models/sdq/mod_subs-undashed-sdq-a-c.rds")
#formattable(mod_sdq_a_C$fixed)

model_Dashed_SDQ_C <- brm(DIFF ~ 1 + DEM_01 + Exp_Group + Language + year + year:Exp_Group + (1|id+schoolnum),  
                          data = cov_5, 
                          warmup = 1000, iter = 3000, 
                          cores = 4, chains = 4, 
                          seed = 123)      
mod_sdq_b_C=summary(model_Dashed_SDQ_C)
saveRDS(model_Dashed_SDQ_C, "/models/sdq/models/model-dashed-sdq-c.rds")
saveRDS(mod_sdq_b_C, "/models/sdq/mod_subs-dashed-sdq-b-c.rds")
#formattable(mod_sdq_b_C$fixed)
