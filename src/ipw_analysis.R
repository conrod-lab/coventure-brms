### Install the packages below
library(haven)
library(dplyr)
library(tidyr)
library(data.table)
library(brms)
#library(formattable)
library(broom)
library(rstan)
# Load the ggplot2 package
library(ggplot2)

# set number of cores to use
options(mc.cores = 4)

# add a comment here

# save models
rstan_options(auto_write = TRUE)

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
weights = cov_4_b$ipw


eps <- predict(model_predict_net, type = "response")
n.treated <- sum(cov_4$Exp_Group == 1)
n.control <- sum(cov_4$Exp_Group == 0)
weights <- ifelse(cov_4$Exp_Group, 1/eps, 1/(1 - eps))
### plots

temp.data <- data.frame(weights = weights, treated = as.factor(cov_4$Exp_Group))
ggplot(temp.data, aes(x = weights, fill = treated, color = treated)) + 
  geom_histogram(alpha = 0.5, position = "identity") + 
  xlab("Weights") 
ggsave("weights.png", width = 8, height = 6, units = "in", dpi = 300)

## Histogram of estimated propensity score
temp.data <- data.frame(eps = eps, treated = as.factor(cov_4$Exp_Group))
ggplot(temp.data, aes(x = eps, fill = treated, color = treated)) + 
  geom_histogram(alpha = 0.5, position = "identity") + xlim(c(0, 1)) +
  ggtitle("Histogram of eps before trimming")
ggsave("histpropensity.png", width = 8, height = 6, units = "in", dpi = 300)

# Your data
missingness_counts <- table(cov_4$depado_missingness_sum)

# Convert data to a data frame for ggplot
missingness_data <- data.frame(
  Number_of_Missing_Values = as.factor(names(missingness_counts)),
  Frequency = as.numeric(missingness_counts)
)

# Create a ggplot bar plot
ggplot(missingness_data, aes(x = Number_of_Missing_Values, y = Frequency)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Distribution of Missing Values",
       x = "Number of Missing Values",
       y = "Frequency")

# Save the ggplot as a PNG file
ggsave("bar_plot.png", width = 8, height = 6, units = "in", dpi = 300)

# Kernel density plots
ggplot(cov_4_b, aes(x = propensity, fill = as.factor(cov_4$Exp_Group))) +
  geom_density(alpha = 0.5) +
  labs(title = "Kernel Density Plot of Propensity Scores",
       x = "Propensity Score",
       y = "Density",
       fill = "Group") +
   theme_light()
ggsave("kerneldensityipw.png", width = 8, height = 6, units = "in", dpi = 300)

# Histogram of weights
ggplot(cov_4_b, aes(x = ipw, fill = as.factor(cov_4$Exp_Group))) +
  geom_histogram(binwidth = 0.1, alpha = 0.5, position = "identity") +
  labs(title = "Histogram of Inverse Probability Weights",
       x = "Inverse Probability Weight",
       y = "Frequency",
       fill = "Group") +
   theme_light()
ggsave("histipw.png", width = 8, height = 6, units = "in", dpi = 300)


glm.IPW = glm(cov_4$Exp_Group ~ cov_4$depado_missingness_sum, family = binomial(link="logit"), weights=weights)

# covariate adjusted means


# Distribution of the outcome variable
cov_4_b$id_sub=c(1:nrow(cov_4_b))
cov_4_b=cov_4_b %>% dplyr::select(order(colnames(cov_4_b)))


##### Creating The long format dataset without IPW #####

cov_4=cov_4 %>% dplyr::select(order(colnames(cov_4)))
cov_5=melt(setDT(cov_4), 
           measure = patterns('DEPAPO_', 'Flagged_','DEP_', 'BSI_', "year"),
           variable.name = 'var', value.name = c('DEPADO', 'Flagged','DEP', 'BSI', "year"))


##### model with continuous time variable without IPW #####
cov_5$Flagged=as.factor(cov_5$Flagged)
# Assuming cov_5 is your data frame
cov_5 <- na.omit(cov_5)

p<- ggplot(cov_5, aes(x = Flagged, fill = as.factor(cov_5$Exp_Group))) +
  geom_density(alpha = 0.5) +
  labs(title = "Outcome Distribution Before and After IPW",
       x = "Outcome Variable",
       y = "Density",
       fill = "Group") +
   theme_light()
ggsave("distoutcome.png", plot=p,width = 8, height = 6, units = "in", dpi = 300)

ipw <- cov_5$ipw
# Create separate density plots for treated and control groups
density_before_ipw <- ggplot(cov_5, aes(x = Flagged, fill = as.factor(cov_5$Exp_Group))) +
  geom_density(alpha = 0.5) +
  labs(title = "Outcome Distribution Before IPW",
       x = "Outcome Variable",
       y = "Density",
       fill = "Group") +
  theme_minimal()

density_after_ipw <- ggplot(cov_5, aes(x = Flagged, fill = as.factor(cov_5$Exp_Group))) +
  geom_density(aes(weight = ipw), alpha = 0.5) +
  labs(title = "Outcome Distribution After IPW",
       x = "Outcome Variable",
       y = "Density",
       fill = "Group") +
  theme_minimal()

# Arrange the plots side by side
#library(gridExtra)
p2<-grid.arrange(density_before_ipw, density_after_ipw, ncol = 2)
ggsave("before-and-after-ipw.png",plot=p2,width = 8, height = 6, units = "in", dpi = 300)
