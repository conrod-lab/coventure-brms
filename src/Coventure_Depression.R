### Install the packages below
library(haven)
library(dplyr)
library(tidyr)
library(data.table)
library(brms)
library(formattable)
library(broom)
library(psych)

### Put the working directory to where data file is ####
setwd("~/....")
cov <- read_sav("coventurefinaldataset.sav")

##### filter out those with NO_SURPS_NO_DEPADO ##### 
cov= cov[cov$NO_SURPS_NO_DEPADO==0,]

##### Creating different riskhilo variables, PC agreed on the relevance of riskhilo3 ##### 
cov$riskhilo2=1*((cov$AssignGroup>0&cov$Exp_Group==1)|(cov$riskhilo==1&cov$Exp_Group==0))
cov$riskhilo3=1*(cov$RiskStatus>0)

##### selecting relevant variables, recoding the flagged variable , and filter out high risks #####
vars=c("schoolnum","Exp_Group","AgeAtTesting_Y5","Language","DEM_01_Y1","SES_Y1","riskhilo","NO_SURPS","DEPAPO_ECHELLE_LUMIERE_Y1",
       "DEPAPO_ECHELLE_LUMIERE_Y2","DEPAPO_ECHELLE_LUMIERE_Y3","DEPAPO_ECHELLE_LUMIERE_Y4","DEPAPO_ECHELLE_LUMIERE_Y5",
       "BSI_echelle_TotalDepression_Y1","BSI_echelle_TotalDepression_Y2","BSI_echelle_TotalDepression_Y3","BSI_echelle_TotalDepression_Y4",  
       "BSI_echelle_TotalDepression_Y5")


cov_2=cov%>%dplyr::select(starts_with(vars))%>%mutate(id=1:n(),year1=1,year2=2,year3=3,year4=4,year5=5,Flagged_ECHELLE_LUMIERE_Y1=DEPAPO_ECHELLE_LUMIERE_Y1, DEP_TotalDepression_Y1=BSI_echelle_TotalDepression_Y1)
cov_2= cov_2[cov_2$NO_SURPS_NO_DEPADO==0,]
cov_2= cov_2[cov_2$riskhilo3==1,]
cov_2[,11:19]=apply(cov_2[,11:19],2,function(x){1*(x>1)})
cov_2$Flagged_ECHELLE_LUMIERE_Y1=1*(cov_2$Flagged_ECHELLE_LUMIERE_Y1>1)

##### Renaming variables for conformity, if this step is ignored the wide to long transformation will be faulty  #####
##### This step is added for dashed and undashed variables to be separated in the long format #####


cov_3=cov_2%>% rename("Flagged_ECHELLE_LUMIERE_Y2"="DEPAPO_ECHELLE_LUMIERE_Y2_1"  ,
                      "Flagged_ECHELLE_LUMIERE_Y3"=        "DEPAPO_ECHELLE_LUMIERE_Y3_1"  ,
                      "Flagged_ECHELLE_LUMIERE_Y4"=        "DEPAPO_ECHELLE_LUMIERE_Y4_1"  ,
                      "Flagged_ECHELLE_LUMIERE_Y5"=        "DEPAPO_ECHELLE_LUMIERE_Y5_1"  ,
                      "DEP_TotalDepression_Y2"="BSI_echelle_TotalDepression_Y2_1"  ,
                      "DEP_TotalDepression_Y3"="BSI_echelle_TotalDepression_Y3_1"  ,
                      "DEP_TotalDepression_Y4"="BSI_echelle_TotalDepression_Y4_1"  ,
                      "DEP_TotalDepression_Y5"="BSI_echelle_TotalDepression_Y5_1"   
)

cov_4=cov_3%>% rename("Age"="AgeAtTesting_Y5"  ,
                      "DEM_01"=        "DEM_01_Y1"  ,
                      "SES"=        "SES_Y1"   )


##### Creating The long format dataset without IPW #####

cov_4=cov_4 %>% dplyr::select(order(colnames(cov_4)))
cov_5=melt(setDT(cov_4), 
           measure = patterns('DEPAPO_', 'Flagged_','DEP_', 'BSI_', "year"),
           variable.name = 'var', value.name = c('DEPADO', 'Flagged','DEP', 'BSI', "year"))

# in the long dataset BSI is undashed
psych::describeBy(cov_5$BSI, cov_5$year, mat = TRUE) 
# in the long dataset DEP is dashed
psych::describeBy(cov_5$DEP, cov_5$year, mat = TRUE) 

################# Depression model with continuous time variable ################

model_Undashed_BSI <- brm(BSI ~ 1 + DEM_01 + Exp_Group + Language + year + year:Exp_Group + (1|id+schoolnum),  
              data = cov_5, 
              warmup = 1000, iter = 3000, 
              cores = 4, chains = 4, 
              seed = 123)      
mod_dep_a=summary(model_Undashed_BSI)

# Saving the model and its summary using saveRDS
saveRDS(model_Undashed_BSI, "/models/depression/model_undashed_bsi.rds")
saveRDS(mod_dep_a, "/models/depression/mod_dep_a.rds")
#formattable(mod_dep_a$fixed)

model_Dashed_BSI <- brm(DEP ~ 1 + DEM_01 + Exp_Group + Language + year + year:Exp_Group + (1|id+schoolnum),  
              data = cov_5, 
              warmup = 1000, iter = 3000, 
              cores = 4, chains = 4, 
              seed = 123)      
mod_dep_b=summary(model_Dashed_BSI)
saveRDS(model_Dashed_BSI, "/models/depression/model_dashed_bsi.rds")
saveRDS(mod_dep_b, "/models/depression/mod_dep_b.rds")

#formattable(mod_dep_b$fixed)
################# Depression model with categorical time variable ################
cov_5$year=as.factor(cov_5$year)

model_Undashed_BSI_C <- brm(BSI ~ 1 + DEM_01 + Exp_Group + Language + year + year:Exp_Group + (1|id+schoolnum),  
                        data = cov_5, 
                        warmup = 1000, iter = 3000, 
                        cores = 4, chains = 4, 
                        seed = 123)      
mod_dep_a_C=summary(model_Undashed_BSI_C)

# Saving the model and its summary using saveRDS for categorical time variable
saveRDS(model_Undashed_BSI_C, "/models/depression/model_undashed_bsi_c.rds")
saveRDS(mod_dep_a_C, "/models/depression/mod_dep_a_c.rds")
#formattable(mod_dep_a_C$fixed)

model_Dashed_BSI_C <- brm(DEP ~ 1 + DEM_01 + Exp_Group + Language + year + year:Exp_Group + (1|id+schoolnum),  
                          data = cov_5, 
                          warmup = 1000, iter = 3000, 
                          cores = 4, chains = 4, 
                          seed = 123)      
mod_dep_b_C=summary(model_Dashed_BSI_C)

saveRDS(model_Dashed_BSI_C, "/models/depression/model_dashed_bsi_c.rds")
saveRDS(mod_dep_b_C, "/models/depression/mod_dep_b_c.rds")
#formattable(mod_dep_b_C$fixed)
