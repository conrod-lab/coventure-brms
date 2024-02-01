### Install the packages below
library(haven)
library(dplyr)
library(tidyr)
library(data.table)
library(brms)
library(broom)

### Put the working directory to where data file is ####
setwd("/data")
cov <- read_sav("coventurefinaldataset.sav")

##### filter out those with NO_SURPS_NO_DEPADO ##### 
cov= cov[cov$NO_SURPS_NO_DEPADO==0,]

##### Creating different riskhilo variables, PC agreed on the relevance of riskhilo3 ##### 
cov$riskhilo2=1*((cov$AssignGroup>0&cov$Exp_Group==1)|(cov$riskhilo==1&cov$Exp_Group==0))
cov$riskhilo3=1*(cov$RiskStatus>0)

##### selecting relevant variables, recoding the flagged variable, and filter out high risks #####
vars=c("schoolnum","Exp_Group","AgeAtTesting_Y5","Language","DEM_01_Y1","SES_Y1","riskhilo","NO_SURPS","DEPAPO_ECHELLE_LUMIERE_Y1",
       "DEPAPO_ECHELLE_LUMIERE_Y2","DEPAPO_ECHELLE_LUMIERE_Y3","DEPAPO_ECHELLE_LUMIERE_Y4","DEPAPO_ECHELLE_LUMIERE_Y5",
       "BSI_echelle_TotalDepression_Y1","BSI_echelle_TotalDepression_Y2","BSI_echelle_TotalDepression_Y3","BSI_echelle_TotalDepression_Y4",  
       "BSI_echelle_TotalDepression_Y5")

cov_2=cov%>%dplyr::select(starts_with(vars))%>%mutate(id=1:n(),year1=1,year2=2,year3=3,year4=4,year5=5,Flagged_ECHELLE_LUMIERE_Y1=DEPAPO_ECHELLE_LUMIERE_Y1, DEP_TotalDepression_Y1=BSI_echelle_TotalDepression_Y1)
cov_2= cov_2[cov_2$NO_SURPS_NO_DEPADO==0,]
cov_2= cov_2[cov_2$riskhilo3==1,]
cov_2[,11:19]=apply(cov_2[,11:19],2,function(x){1*(x>1)})
cov_2$Flagged_ECHELLE_LUMIERE_Y1=1*(cov_2$Flagged_ECHELLE_LUMIERE_Y1>1)

##### Renaming variables for conformity #####

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

##### Creating IPW weights #####

cov_4$depado_missingness_sum=c(is.na(cov_4$DEPAPO_ECHELLE_LUMIERE_Y1)*1+
                                 is.na(cov_4$DEPAPO_ECHELLE_LUMIERE_Y2)*1+
                                 is.na(cov_4$DEPAPO_ECHELLE_LUMIERE_Y3)*1+
                                 is.na(cov_4$DEPAPO_ECHELLE_LUMIERE_Y4)*1+
                                 is.na(cov_4$DEPAPO_ECHELLE_LUMIERE_Y5)*1)

model_predict_net <- glm(cov_4$Exp_Group ~ cov_4$depado_missingness_sum,
                         family = binomial(link = "logit"))

# Generate propensity scores and IPWs
cov_4_b <- augment_columns(model_predict_net, cov_4,
                           type.predict = "response") %>% 
  rename(propensity = .fitted) %>% 
  mutate(ipw = (cov_4$Exp_Group / propensity) + ((1 - cov_4$Exp_Group) / (1 - propensity)))

cov_4_b$id_sub=c(1:nrow(cov_4_b))
cov_4_b=cov_4_b %>% dplyr::select(order(colnames(cov_4_b)))

##### Creating The long format dataset without IPW #####

cov_4=cov_4 %>% dplyr::select(order(colnames(cov_4)))
cov_5=melt(setDT(cov_4), 
           measure = patterns('DEPAPO_', 'Flagged_','DEP_', 'BSI_', "year"),
           variable.name = 'var', value.name = c('DEPADO', 'Flagged','DEP', 'BSI', "year"))

# THIS TABLE PROVIDES FREQUENCIES TO MAKE SURE THAT THE VARIABLE IS UNDASHED
table(cov_5$DEPADO,cov_5$year,cov_5$Exp_Group)

##### model with continuous time variable without IPW #####
cov_5$DEPADO=as.factor(cov_5$DEPADO)
model3a <- brm(DEPADO ~ 1 + DEM_01 + Exp_Group + Language + year + year:Exp_Group + (1|id+schoolnum),  
               data = cov_5, 
               family = bernoulli,
               warmup = 1000, iter = 3000, 
               cores = 4, chains = 4, 
               seed = 123)      
mod_subs_a=summary(model3a)

# Saving the model and its summary using saveRDS
saveRDS(model3a, "/models/depado_undashed/model3a.rds")
saveRDS(mod_subs_a, "/models/depado_undashed/mod_subs_a.rds")

##### model with categorical time variable without IPW #####
cov_5$year=as.factor(cov_5$year)

model3d <- brm(DEPADO ~ 1 + DEM_01 + Exp_Group + Language + year + year:Exp_Group + (1|id+schoolnum),  
               data = cov_5, 
               family = bernoulli,
               warmup = 1000, iter = 3000, 
               cores = 4, chains = 4, 
               seed = 123)      
mod_subs_d=summary(model3d)

# Saving the model and its summary using saveRDS
saveRDS(model3d, "/models/depado_undashed/model3d.rds")
saveRDS(mod_subs_d, "/models/depado_undashed/mod_subs_d.rds")

##### model with continuous time variable without IPW for visualization #####
cov_5$DEPADO=as.factor(cov_5$DEPADO)
model3v <- brm(DEPADO ~ 1 + Exp_Group +  year + year:Exp_Group + (1|id+schoolnum),  
               data = cov_5, 
               family = bernoulli,
               warmup = 1000, iter = 3000, 
               cores = 4, chains = 4, 
               seed = 123)      
mod_subs_v=summary(model3v)

# Saving the model and its summary using saveRDS
saveRDS(model3v, "/models/depado_undashed/model3v.rds")
saveRDS(mod_subs_v, "/models/depado_undashed/mod_subs_v.rds")

##### Creating The long format dataset with IPW #####
cov_5_b=melt(setDT(cov_4_b), 
             measure = patterns('DEPAPO_', 'Flagged_','DEP_', 'BSI_', "year"),
             variable.name = 'var', value.name = c('DEPADO', 'Flagged','DEP', 'BSI', "year"))

# THIS TABLE PROVIDES FREQUENCIES TO MAKE SURE THAT THE VARIABLE IS UNDASHED
table(cov_5_b$DEPADO,cov_5_b$year,cov_5_b$Exp_Group)

##### model with continuous time variable with IPW #####
cov_5_b$DEPADO=as.factor(cov_5_b$DEPADO)
model3b <-  brm(DEPADO|weights(ipw) ~ 1 + DEM_01 + Exp_Group + Language + year + year:Exp_Group + (1|id+schoolnum),  
                data = cov_5_b, 
                family = bernoulli,
                warmup = 1000, iter = 3000, 
                cores = 4, chains = 4, 
                seed = 123) 
mod_subs_b=summary(model3b)

# Saving the model and its summary using saveRDS
saveRDS(model3b, "/models/depado_undashed/model3b.rds")
saveRDS(mod_subs_b, "/models/depado_undashed/mod_subs_b.rds")

##### model with categorical time variable with IPW #####
table(cov_5_b$year)
cov_5_b$year=as.factor(cov_5_b$year)
model3c <-  brm(DEPADO|weights(ipw) ~ 1 + DEM_01 + Exp_Group + Language + year + year:Exp_Group + (1|id+schoolnum),  
                data = cov_5_b, 
                family = bernoulli,
                warmup = 1000, iter = 3000, 
                cores = 4, chains = 4, 
                seed = 123)   
mod_subs_c=summary(model3c)

# Saving the model and its summary using saveRDS
saveRDS(model3c, "/models/depado_undashed/model3c.rds")
saveRDS(mod_subs_c, "/models/depado_undashed/mod_subs_c.rds")
