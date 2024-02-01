### Install the packages below
library(haven)
library(dplyr)
library(tidyr)
library(data.table)
library(brms)
library(broom)
# library(psych)

### Put the working directory to where data file is ####
setwd("/home/spinney/scratch/coventure/src")
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
# psych::describeBy(cov_5$BSI, cov_5$year, mat = TRUE) 
# # in the long dataset DEP is dashed
# psych::describeBy(cov_5$DEP, cov_5$year, mat = TRUE) 

# Assuming cov_5 is your dataset
library(kableExtra)

# Create a table for BSI variable
table_BSI <- psych::describeBy(cov_5$BSI, cov_5$year, mat = TRUE)

# Convert the table to a data frame for better formatting
table_BSI_df <- as.data.frame(table_BSI)

# Create a kableExtra table for BSI
kable_BSI <- kable(table_BSI_df, format = "html", caption = "BSI Variable Descriptive Statistics") %>%
  kable_styling(full_width = FALSE)

# Save the kableExtra table to an HTML file
html_file_path_BSI <- file.path("/home/spinney/scratch/coventure/models/depression/html_output", "BSI_descriptive_statistics.html")
save_kable(kable_BSI, html_file_path_BSI)
cat("HTML file saved:", html_file_path_BSI, "\n")

# Create a table for DEP variable
table_DEP <- psych::describeBy(cov_5$DEP, cov_5$year, mat = TRUE)

# Convert the table to a data frame for better formatting
table_DEP_df <- as.data.frame(table_DEP)

# Create a kableExtra table for DEP
kable_DEP <- kable(table_DEP_df, format = "html", caption = "DEP Variable Descriptive Statistics") %>%
  kable_styling(full_width = FALSE)

# Save the kableExtra table to an HTML file
html_file_path_DEP <- file.path("/home/spinney/scratch/coventure/models/depression/html_output", "DEP_descriptive_statistics.html")
save_kable(kable_DEP, html_file_path_DEP)
cat("HTML file saved:", html_file_path_DEP, "\n")
