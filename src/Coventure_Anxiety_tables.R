### Install the packages below
library(haven)
library(dplyr)
library(tidyr)
library(data.table)
library(brms)
library(broom)
library(psych)


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
       "BSI_echelle_TotalAnxiete_Y1","BSI_echelle_TotalAnxiete_Y2","BSI_echelle_TotalAnxiete_Y3","BSI_echelle_TotalAnxiete_Y4",  
       "BSI_echelle_TotalAnxiete_Y5")


cov_2=cov%>%dplyr::select(starts_with(vars))%>%mutate(id=1:n(),year1=1,year2=2,year3=3,year4=4,year5=5,Flagged_ECHELLE_LUMIERE_Y1=DEPAPO_ECHELLE_LUMIERE_Y1, ANX_TotalAnxiete_Y1=BSI_echelle_TotalAnxiete_Y1)
cov_2= cov_2[cov_2$NO_SURPS_NO_DEPADO==0,]
cov_2= cov_2[cov_2$riskhilo3==1,]


##### Renaming variables for conformity, if this step is ignored the wide to long transformation will be faulty #####
##### This step is added for dashed and undashed variables to be separated in the long format #####

cov_3=cov_2%>% rename("Flagged_ECHELLE_LUMIERE_Y2"="DEPAPO_ECHELLE_LUMIERE_Y2_1"  ,
                      "Flagged_ECHELLE_LUMIERE_Y3"=        "DEPAPO_ECHELLE_LUMIERE_Y3_1"  ,
                      "Flagged_ECHELLE_LUMIERE_Y4"=        "DEPAPO_ECHELLE_LUMIERE_Y4_1"  ,
                      "Flagged_ECHELLE_LUMIERE_Y5"=        "DEPAPO_ECHELLE_LUMIERE_Y5_1"  ,
                      "ANX_TotalAnxiete_Y2"="BSI_echelle_TotalAnxiete_Y2_1"  ,
                      "ANX_TotalAnxiete_Y3"="BSI_echelle_TotalAnxiete_Y3_1"  ,
                      "ANX_TotalAnxiete_Y4"="BSI_echelle_TotalAnxiete_Y4_1"  ,
                      "ANX_TotalAnxiete_Y5"="BSI_echelle_TotalAnxiete_Y5_1"   
)
cov_4=cov_3%>% rename("Age"="AgeAtTesting_Y5"  ,
                      "DEM_01"=        "DEM_01_Y1"  ,
                      "SES"=        "SES_Y1"   )


##### Creating The long format dataset without IPW #####

cov_4=cov_4 %>% dplyr::select(order(colnames(cov_4)))
cov_5=melt(setDT(cov_4), 
           measure = patterns('DEPAPO_', 'Flagged_','ANX_', 'BSI_', "year"),
           variable.name = 'var', value.name = c('DEPADO', 'Flagged','ANX', 'BSI_A', "year"))


# # in the long dataset BSI_A is undashed
# psych::describeBy(cov_5$BSI_A, cov_5$year, mat = TRUE) 
# # # in the long dataset ANX is dashed
# psych::describeBy(cov_5$ANX, cov_5$year, mat = TRUE) 

# Assuming cov_5 is your dataset
library(kableExtra)

# Save describeBy output for BSI_A
result_BSI_A <- psych::describeBy(cov_5$BSI_A, cov_5$year, mat = TRUE)

# Create a kableExtra table for BSI_A
kable_BSI_A <- kable(result_BSI_A, format = "html", caption = "BSI_A Summary") %>%
  kable_styling(full_width = FALSE)

# Save the kableExtra table to an HTML file if it doesn't exist
html_file_path_BSI_A <- file.path("/home/spinney/scratch/coventure/models/anxiety/html_output", "BSI_A_summary.html")
if (!file.exists(html_file_path_BSI_A)) {
  save_kable(kable_BSI_A, html_file_path_BSI_A)
  cat("HTML file saved:", html_file_path_BSI_A, "\n")
} else {
  cat("HTML file already exists, skipping:", html_file_path_BSI_A, "\n")
}

# Save describeBy output for ANX
result_ANX <- psych::describeBy(cov_5$ANX, cov_5$year, mat = TRUE)

# Create a kableExtra table for ANX
kable_ANX <- kable(result_ANX, format = "html", caption = "ANX Summary") %>%
  kable_styling(full_width = FALSE)

# Save the kableExtra table to an HTML file if it doesn't exist
html_file_path_ANX <- file.path("/home/spinney/scratch/coventure/models/anxiety/html_output", "ANX_summary.html")
if (!file.exists(html_file_path_ANX)) {
  save_kable(kable_ANX, html_file_path_ANX)
  cat("HTML file saved:", html_file_path_ANX, "\n")
} else {
  cat("HTML file already exists, skipping:", html_file_path_ANX, "\n")
}

